/*
 * skyfly.c	$Revision: 1.16 $
*/
#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <sys/types.h>
#include <sys/sysmp.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <malloc.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>
#include <ulocks.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <math.h>
#include "skyfly.h"

#define ERR_WARNING			0x1
#define ERR_FATAL			0x2
#define ERR_SYSERR			0x4

#define AMALLOC(a, type, num, func) 	{				    \
    if((int)(a = (type*)amalloc(sizeof(type)*(num), shmem_arena)) <= 0)	    \
	err_msg(ERR_FATAL, func, "amalloc failed");		    \
}

int MS;			    /* do I have hardware multisampling */

float 	ScaleZ	 = 2.3;	    /* Terrain height scale factor */
int 	CellDim = 4;	    /* Terrain cell is CellDim X Celldim quads */
int	NumCells = 36;	    /* Terrain grid is NumCells X NumCells cells */
int 	GridDim;	    /* Terrain grid is GridDim X GridDim quads */
float	XYScale;	    /* Conversion from world-space to grid index */
float	CellSize;	    /* World-space size of cell */

int	Init_pos;	    /* if true, set initial position and kbd mode */
float	Init_x, Init_y, Init_z, Init_azimuth;

/*
 * Data that changes from frame to frame needs to be double-buffered because
 * two processes may be working on two different frames at the same time.
*/
typedef struct buffered_data_struct {

	/* objects */
	perfobj_t       paper_plane_pos_obj[NUM_PLANES];
	perfobj_t       viewer_pos_obj;

	/* flags */
	unsigned long   paper_plane_pos_flags[2];
	unsigned long   viewer_pos_flags[2];

	/* data */
	float  paper_plane_position[NUM_PLANES][6];
	float  viewer_position[4];

} buffered_data;

/*
 * This is the per-pipe data structure which holds pipe id, semaphores,
 * and variable data buffers. Both gfxpipe and buffer structures live in
 * shared memory so the sim can communicate with its forked children.
*/
typedef struct gfxpipe_data_struct {
	int		gfxpipenum;

	usema_t		*sim_done;
	usema_t		*cull_done;
	usema_t		*draw_done;

	buffered_data	**buffers;

} gfxpipe_data;

static gfxpipe_data	*gfxpipe;	/* A processes' gfxpipe struct */
static gfxpipe_data	*gfxpipes[2];	/* Maximum of 2 graphics pipes */
static int		num_pipes;
static void		*shmem_arena;
static usptr_t		*sema_arena;
static usema_t		*gfxinit_done;
static char		sky_shmem_file[256];
static char		sky_sema_arena[256];
static int		sky_uid;
static int		num_procs;	/* Number of physical processors */
static int		cpus_per_channel;
static float		fog_params[4];	/* Fog and clear color */
static float		fog_density = .025;
static float		far_cull  = 31.;    /* Far clip distance from eye */
static char		OptionStr[] = "cdtz:n:f:y:w:o:vp:";

extern int errno;

int	SkyType = SKY_CYCLOPS;	    /* See skyfly.h */
int	Wxsize = 640, Wysize = 512; /* Default is 1/4 screen */
int	Wxorg = 320,  Wyorg = 256;  /* Default centers viewport on screen */
int	VGAmode = 0;		    /* 640x480 window with 0,0 origin */

/*
 * All non-variable data like geometry is stored in shared memory. This way
 * forked processes avoid duplicating data unnecessarily.
*/
shared_data	*SharedData;

/* //////////////////////////////////////////////////////////////////////// */

void sim_proc(void);
void sim_cyclops(void);
void sim_dualchannel(void);
void sim_singlechannel(void);
void cull_proc(void);
void draw_proc(void);
void sim_exit(void);
void init_misc(int argc, char *argv[]); 
void init_shmem(void); 
void init_terrain(void);
void init_clouds(void);
void init_paper_planes(void);
void init_positions(void);
void init_gfxpipes(void);
void init_gl(int gfxpipenum);
void err_msg(int type, char* func, char* error);
void dialog(char *msg);
void fly(perfobj_t *viewer_pos);
void fly_paper_planes(perfobj_t *paper_plane_pos);
float terrain_height(void);

void
main(int argc, char *argv[])
{
    int             i;
    pid_t           ppid;

    init_misc(argc, argv);
    init_shmem();
    init_gfxpipes();
    init_terrain();
    init_clouds();
    init_paper_planes();
    init_positions();

    /*
     * Fork off cull/draw process pairs for each pipe. The configuration
     * for cyclops and dual-channel look like:
     *
     *			shared group(sproc)
     *		----------------------------------------	
     *	 fork->	|    Cull --> ring buffer -->	Draw   |    --> Pipe 0
     *	 /	----------------------------------------
     *  /		
     * -------	     ----------------------------------
     * | Sim |-----> |  pipe structures, geometry     | <-- shared memory
     * -------	     ----------------------------------
     *  \
     *	 \	----------------------------------------	
     *	fork->	|    Cull --> ring buffer -->	Draw   |    --> Pipe 1
     *		----------------------------------------
     *			shared group(sproc)
     *
    */
    for (i = 0; i < num_pipes; i++) {

	/*
	 * Set global gfxpipe so forked process knows which gfxpipe it owns.
	*/
	gfxpipe = gfxpipes[i];

	if((ppid = fork()) < 0)
	    err_msg(ERR_FATAL | ERR_SYSERR, "main()", "fork() failed");

	else if (ppid == 0) {		    /* If child process */

	    init_gl(gfxpipe->gfxpipenum);   

	    if(sproc((void (*)(void *))cull_proc, PR_SALL) < 0)
		err_msg(ERR_FATAL | ERR_SYSERR, "main()", "sproc() failed");

	    draw_proc();
	}
	uspsema(gfxinit_done);	/* Wait until child has opened window */
    }

    sim_proc();
}

/*-------------------------------------- Sim ------------------------------*/

void
sim_proc(void)
{
    /*
     * Set up sim to exit if any child exits
    */	
    signal(SIGCLD, SIG_DFL);
 
    /*
     * Open up a connection to the Xserver to receive mouse and keyboard 
     * events without using a GL window. This is preferable over noport()
     * for performance reasons. See xinput.c.
    */
    openXinput();

    switch (SkyType) {
    case SKY_CYCLOPS:
	sim_cyclops();
	break;
    case SKY_DUALCHANNEL:
	sim_dualchannel();
	break;
    case SKY_SINGLECHANNEL:
	sim_singlechannel();
	break;
    }
}

/* _________________________________________________________________________

  Cyclops synchronization - The video output from 2 hardware graphics pipes
running at 30hz are multiplexed to a single screen providing a 60hz update
rate(for 1/4 screen windows).  Latency in this case is a maximum of 67ms 
between sampling inputs and final pixel scanned out on screen.

S = sim, C = cull, D = draw

Processor #
---------

0 	    |--S2--|---C2--|  |--S4--|---C4---|

1   |-------D0-------|-------D2-------|------D4--------|	--> Pipe 0

2	    	     |--S3--|---C3---||--S5--|---C5---|

3   --------|-------D1--------|-------D3-------|-------D5-------|--> Pipe 1

	    <1/60sec >
    end of D0 triggers S3, end of D1 triggers S4, etc

 _________________________________________________________________________
 
    This cylcops programming model has 5 processes: 1 sim, 2 cull/draw pairs. 
The sim samples inputs at a regular 60hz by semaphoring on draw_done.
Cyclops hardware synchronizes the 2 pipes via mswapbuffers() to ensure that 
the rendering pipes are staggered as drawn above. Note that the above 
diagram is the steady-state situation; the initial start-up is different.

*/

void
sim_cyclops(void)
{
    int             gfxpipeid = 0, buffer = 0;
    perfobj_t      *viewer_pos;
    buffered_data  *buffered;

    while (!Xgetbutton(ESCKEY)) {

	/*
	 * Pipe 0
	*/
	uspsema(gfxpipes[0]->cull_done);
	uspsema(gfxpipes[1]->draw_done);

	    buffered = gfxpipes[0]->buffers[buffer];

	    fly( &(buffered->viewer_pos_obj) );
	    fly_paper_planes( buffered->paper_plane_pos_obj );

	usvsema( gfxpipes[0]->sim_done );

	/*
	 * Pipe 1
	*/
	uspsema(gfxpipes[1]->cull_done);
	uspsema(gfxpipes[0]->draw_done);

	    buffered = gfxpipes[1]->buffers[buffer];

	    fly( &(buffered->viewer_pos_obj) );
	    fly_paper_planes( buffered->paper_plane_pos_obj );

	usvsema( gfxpipes[1]->sim_done );

	buffer = !buffer;
    }
    sim_exit();
}

/* _________________________________________________________________________

Dual-channel synchronization - The 2 graphics hardware pipes render 2 
different views of the scene on 2 screens at 30hz. Latency in this case is 
maximum 83ms between sampling inputs and the final pixel scanned out on 
the screen.

S = sim, C = cull, D = draw

Processor #
---------

0   |-S1-|--C1-|      |--S2--|---C2-----|

1   |-------D0--------|-------D1------|------D2--------|	--> Pipe 0

2	  |--C1---|	     |---C2---|

3   |-------D0--------|-------D1------|-------D2-------|	--> Pipe 1

    <    1/30sec      >
    end of S1 triggers C1, end of C1 triggers S2, etc

 _________________________________________________________________________

    Like the cyclops mode above, this configuration uses 5 processes.
However, the hardware is not in cyclops mode so the pipes are essentially
independent. User-level synchronization is needed to keep the two pipes
rendering the same time frame. Note that the sim process runs half as much
as in cyclops mode, i.e.- at 30 hz.

*/

void
sim_dualchannel(void)
{
    int             buffer = 0;
    perfobj_t      *viewer_pos;
    buffered_data  **buffered = gfxpipes[0]->buffers;

    while (!Xgetbutton(ESCKEY)) {

	uspsema(gfxpipes[0]->cull_done);
	uspsema(gfxpipes[1]->cull_done);
	uspsema(gfxpipes[0]->draw_done);
	uspsema(gfxpipes[1]->draw_done);

	    fly(&(buffered[buffer]->viewer_pos_obj));
	    fly_paper_planes(buffered[buffer]->paper_plane_pos_obj);

	usvsema(gfxpipes[0]->sim_done);
	usvsema(gfxpipes[1]->sim_done);

	buffer = !buffer;
    }
    sim_exit();
}

/*
 * This is a single-channel version of the dual-channel simulation
 * described above.
*/
void
sim_singlechannel(void)
{
    int             buffer = 0;
    perfobj_t      *viewer_pos;
    buffered_data  **buffered = gfxpipes[0]->buffers;

    while (!Xgetbutton(ESCKEY)) {

	uspsema(gfxpipes[0]->cull_done);
	uspsema(gfxpipes[0]->draw_done);

	    fly(&(buffered[buffer]->viewer_pos_obj));
	    fly_paper_planes(buffered[buffer]->paper_plane_pos_obj);

	usvsema(gfxpipes[0]->sim_done);

	buffer = !buffer;
    }
    sim_exit();
}

void
sim_exit(void)
{
    closeXinput();
    exit(1);
}

/*-------------------------------------- Cull ------------------------------*/

/*
 *   The cull and draw processes operate in a classic producer/consumer, 
 * write/read configuration using a ring buffer. The ring consists of pointers
 * to perfobj's instead of actual geometric data. This is important because
 * you want to minimize the amount of data 'shared' between two processes that
 * run on different processors in order to reduce cache invalidations.
 *   enter_in_ring and get_from_ring spin on ring full and ring empty 
 * conditions respectively.
 *   Since cull/draw are shared group processes(sproc), the ring buffer is
 * in the virtual address space of both processes and shared memory is not
 * necessary.
*/

#define RING_SIZE   1000    /* Size of ring */

typedef struct render_ring_struct {
    volatile unsigned long	head, tail;
    perfobj_t			**ring;
} render_ring;

render_ring ringbuffer;

void	    enter_in_ring(perfobj_t *perfobj);
perfobj_t*  get_from_ring(void);

void
cull_proc(void)
{
    int             i, pntr, buffer = 0;
    int             x, y, x0, y0, x1, y1, xx, yy;
    float           vX, vY, vazimuth, px, py;
    float           ax, ay, bx, by, cx, cy;
    float           minx, maxx, miny, maxy;
    float           left_area, right_area, far_area;
    float           left_dx, left_dy, right_dx, right_dy, far_dx, far_dy;
    perfobj_t      *viewer_pos, *paper_plane_pos;
    buffered_data  *buffered;
    perfobj_t     **cells;
    perfobj_t      *terrain_texture = &(SharedData->terrain_texture_obj);
    perfobj_t      *paper_plane = &(SharedData->paper_plane_obj);
    perfobj_t      *paper_plane_start = &(SharedData->paper_plane_start_obj);
    perfobj_t      *paper_plane_2ndpass = &(SharedData->paper_plane_2ndpass_obj);
    perfobj_t      *paper_plane_end = &(SharedData->paper_plane_end_obj);
    perfobj_t      *clouds_texture = &(SharedData->clouds_texture_obj);
    perfobj_t      *clouds = &(SharedData->clouds_obj);

    perfobj_t       viewer_pos_obj[2];
    unsigned long   viewer_pos_flags[4];
    float           *viewer, viewer_position[2][4];

    float           side, far, epsilon, plane_epsilon;
    float           cellscale;
    float           fovx = FOV *(float) Wxsize /(float) Wysize;

    cellscale = 1./ (XYScale * (float) CellDim);
    side = far_cull / cosf(fovx / 2.);
    far = 2.* side * sinf(fovx / 2.);
    epsilon = sqrtf(2.) * CellSize / 2.;
    plane_epsilon = .5;

    /* 
     * Set up cull process to exit when its parent, sim, exits
    */
    signal(SIGHUP, SIG_DFL);
    if (prctl(PR_TERMCHILD) < 0)
	err_msg(ERR_WARNING | ERR_SYSERR, "cull_proc()", "prctl() failed");

    /*
     * Pin cull process to a single processor if there are enough processors
    */
    if (!sky_uid && cpus_per_channel >= 2 && 
	sysmp(MP_MUSTRUN, gfxpipe->gfxpipenum * 2) < 0)
	err_msg(ERR_WARNING | ERR_SYSERR, "cull_proc()", "sysmp() failed");

    cells = (perfobj_t **) malloc(NumCells * NumCells * sizeof(perfobj_t *));
    for (x = 0; x < NumCells; x++)
	for (y = 0; y < NumCells; y++)
	    cells[x * NumCells + y] =
		&(SharedData->terrain_cells[x * NumCells + y]);

    ringbuffer.ring = malloc(RING_SIZE * sizeof(perfobj_t *));
    ringbuffer.head = ringbuffer.tail = 0;

    viewer_pos_obj[0].flags = viewer_pos_flags;
    viewer_pos_obj[0].vdata = viewer_position[0];
    viewer_pos_obj[1].flags = viewer_pos_flags;
    viewer_pos_obj[1].vdata = viewer_position[1];

    *(viewer_pos_flags) = PD_VIEWER_POS;
    *(viewer_pos_flags + 1) = PD_END;

    while (1) {

	int             visible_planes[NUM_PLANES], nplanes;

	uspsema(gfxpipe->sim_done);

	buffered = gfxpipe->buffers[buffer];

	viewer_pos = &(buffered->viewer_pos_obj);
	paper_plane_pos = buffered->paper_plane_pos_obj;

	vX = *((float *) viewer_pos->vdata + 0);
	vY = *((float *) viewer_pos->vdata + 1);
	vazimuth = *((float *) viewer_pos->vdata + 3);

	/*
	 * Compute different views if we are in dual-channel mode
	*/
	if (SkyType == SKY_DUALCHANNEL)
	    if (gfxpipe->gfxpipenum == 0) {
		vazimuth -= fovx / 2.;
	    } else {
		vazimuth += fovx / 2.;
	    }

	viewer = viewer_position[buffer];

	*(viewer + 0) = vX;
	*(viewer + 1) = vY;
	*(viewer + 2) = *((float *) viewer_pos->vdata + 2);
	*(viewer + 3) = vazimuth;

	/*
	 * Begin cull to viewing frustrum 
	 */
	ax = (vX - sinf(-vazimuth + fovx *.5) * side);
	ay = (vY + cosf(-vazimuth + fovx *.5) * side);
	bx = vX;
	by = vY;
	cx = (vX + sinf(vazimuth + fovx *.5) * side);
	cy = (vY + cosf(vazimuth + fovx *.5) * side);

	minx = MIN(MIN(ax, bx), cx);
	miny = MIN(MIN(ay, by), cy);
	maxx = MAX(MAX(ax, bx), cx);
	maxy = MAX(MAX(ay, by), cy);

	x0 = MAX((int) (minx / CellSize), 0);
	x1 = MIN((int) (maxx / CellSize) + 1, NumCells);
	y0 = MAX((int) (miny / CellSize), 0);
	y1 = MIN((int) (maxy / CellSize) + 1, NumCells);

	left_dx = ax - bx;
	left_dy = ay - by;
	right_dx = cx - bx;
	right_dy = cy - by;

	enter_in_ring(&viewer_pos_obj[buffer]);
	enter_in_ring(terrain_texture);
	/*
	 * Add visible cells to ring buffer 
	 */
	for (x = x0; x < x1; x++) {
	    for (y = y0; y < y1; y++) {
		float           cntrx =(x +.5) * CellSize;
		float           cntry =(y +.5) * CellSize;
		float           dist;

		left_area = left_dx * (cntry - by) - left_dy * (cntrx - bx);
		right_area = right_dx * (cntry - by) - right_dy * (cntrx - bx);
		far_area = far_dx * (cntry - ay) - far_dy * (cntrx - ax);

		if (left_area < epsilon * side && right_area > -epsilon * side
		    && far_area < epsilon * far) {
			enter_in_ring(cells[x * NumCells + y]);
		}
	    }
	}
	enter_in_ring(clouds_texture);
	enter_in_ring(clouds);

	nplanes = 0;
	enter_in_ring(paper_plane_start);
	/*
	 * Add visible planes to ring buffer
	*/
	for (i = 0; i < NUM_PLANES; i++) {

	    px = *((float *) paper_plane_pos[i].vdata + 0);
	    py = *((float *) paper_plane_pos[i].vdata + 1);
	    left_area = left_dx * (py - by) - left_dy * (px - bx);
	    right_area = right_dx * (py - by) - right_dy * (px - bx);
	    far_area = far_dx * (py - ay) - far_dy * (px - ax);

	    if (left_area < plane_epsilon * side && right_area > -plane_epsilon * side
		&& far_area < plane_epsilon * far) {
		enter_in_ring(&paper_plane_pos[i]);
		enter_in_ring(paper_plane);
		visible_planes[nplanes++] = i;
	    }
	}
	/*
	 * Draw planes a second time to anti-alias them(see perfdraw.c)
	 * using  VGXT style PYM_LINE_FAST
	*/

	if (!MS) {
	enter_in_ring(paper_plane_2ndpass);
	for (i = 0; i < nplanes; i++) {
	    enter_in_ring(&paper_plane_pos[visible_planes[i]]);
	    enter_in_ring(paper_plane);
	}
	}

	enter_in_ring(paper_plane_end);

	enter_in_ring((perfobj_t *) 0);	    /* 0 indicates end of frame */
	
	usvsema(gfxpipe->cull_done);
	buffer = !buffer;
    }
}

void
enter_in_ring(perfobj_t *perfobj)
{
    while (ringbuffer.head == RING_SIZE+ringbuffer.tail-1) {}
    ringbuffer.ring[ringbuffer.head % RING_SIZE] = perfobj;
    ringbuffer.head++;
}

perfobj_t*
get_from_ring(void)
{
    static perfobj_t *pobj;

    while(ringbuffer.tail == ringbuffer.head) {}
    pobj = ringbuffer.ring[ringbuffer.tail % RING_SIZE];
    ringbuffer.tail++;
    return pobj;	
}

/*-------------------------------------- Draw ------------------------------*/

void draw_exit(void);

void
draw_proc(void)
{
    perfobj_t      *too_draw;

    /* 
     * Set up draw process to call draw_exit() when its parent, sim, exits
    */
    signal(SIGHUP, draw_exit);
    if (prctl(PR_TERMCHILD) < 0)
	err_msg(ERR_WARNING | ERR_SYSERR, "draw_proc()", "prctl() failed");

    /* 
     * Restrict a processor to run ONLY draw_proc() if we have enough
     * processors.
    */
    if (!sky_uid && cpus_per_channel >= 2) {
	if (sysmp(MP_RESTRICT, gfxpipe->gfxpipenum * 2 + 1) < 0)
	    err_msg(ERR_WARNING | ERR_SYSERR, "draw_proc()", "sysmp() failed");
	if (sysmp(MP_MUSTRUN, gfxpipe->gfxpipenum * 2 + 1) < 0)
	    err_msg(ERR_WARNING | ERR_SYSERR, "draw_proc()", "sysmp() failed");
    }

    c3f(fog_params+1);
    clear();
    zclear();
    while(ringbuffer.tail == ringbuffer.head) {}
    while (1) {
	while (too_draw = get_from_ring()) {
	    drawperfobj(too_draw);
	}

	if (SkyType == SKY_CYCLOPS)
	    mswapbuffers(NORMALDRAW | DUALDRAW);
	else
	    swapbuffers();

	/*
	 * This c3f() is very important. After (m)swapbuffers, the c3f()
	 * attempts to write to the pipe and blocks until the next vertical
	 * retrace. This causes usvsema() to occur on vertical retrace. Since
	 * sim is triggered by draw_done, we can guarantee that sim samples
	 * inputs at regular intervals. If however, you want to trigger sim
	 * earlier, switch the ordering of c3f() and usvsema() but you
	 * may need other synchronization/buffering depending on frame times.
	*/
        c3f(fog_params+1);
        clear();
        zclear();
	usvsema(gfxpipe->draw_done);
    }
}

void 
draw_exit(void)
{
    /*
     * We MUST explicitly unisolate a restricted processor.
    */
    if (!sky_uid && cpus_per_channel >= 2) {
	if (sysmp(MP_UNISOLATE, gfxpipe->gfxpipenum * 2 + 1) < 0)
	    err_msg(ERR_WARNING | ERR_SYSERR, "draw_proc()", "sysmp() failed");
    }

    gexit();
    exit(1);
}

/*------------------------------- Init -----------------------------------*/

void init_texture_and_lighting(void);
void init_buffered_data(buffered_data *buffered);

void
init_misc(int argc, char *argv[])
{
    extern int      getopt(int, char *const *, const char *);
    extern char *optarg;

    int	    org = 0, c;
    float   density;

    if(getgdesc(GD_TEXTURE_PERSP) != 1)
    	err_msg(ERR_FATAL, "init_misc()", 
		    "skyfly only runs on SkyWriter systems");

    while ((c = getopt(argc, argv, OptionStr)) != -1)  
	    switch(c) {
	    case 'c': 
		SkyType = SKY_CYCLOPS; 
		break;
	    case 'd':
		SkyType = SKY_DUALCHANNEL;
                break;
	    case 't':
		SkyType = SKY_SINGLECHANNEL;
                break;
	    case 'z':
		ScaleZ = atof(optarg);
                break;
	    case 'n':
		NumCells = atoi(optarg);
                break;
	    case 'y':
		far_cull = atof(optarg);
                break;
	    case 'f':
		fog_density = atof(optarg);
                break;
	    case 'v':
		VGAmode = 1;
                break;
	    case 'w':
		sscanf(optarg, "%d,%d", &Wxsize, &Wysize);
                break;
	    case 'o':
		org = 1;
		sscanf(optarg, "%d,%d", &Wxorg, &Wyorg);
                break;
	    case 'p':
		sscanf(optarg, "%f,%f,%f,%f", &Init_x, &Init_y, &Init_z,
			&Init_azimuth);
		Init_pos = 1;
            }

    if(getgdesc(GD_MUXPIPES) != 2) {
	err_msg(ERR_WARNING, "init_misc()", 
	    "No cyclops capability. Defaulting to single-pipe mode.");
	SkyType = SKY_SINGLECHANNEL;
    }

    if(VGAmode) {
    	Wxorg = Wyorg = 0;
    	Wxsize = 640;
    	Wysize = 480;
    }
    else if(!org) {	/* Center window if origin not specified */
    	Wxorg = 640 - Wxsize/2;
    	Wyorg = 512 - Wysize/2;
    }

    if ((sky_uid = getuid()) != 0) {
	fprintf(stderr, 
	    "Warning: Not running as root. Can't lock processes to processors\n");
    }

    num_procs = sysmp(MP_NAPROCS);
    if(SkyType == SKY_SINGLECHANNEL)
	cpus_per_channel = num_procs;
    else
	cpus_per_channel = num_procs >> 1;

    /*
     * Compute fog and clear color to be linear interpolation between blue
     * and white.
    */
    density = 1.- expf(-5.5 * fog_density * fog_density *
			      far_cull * far_cull);
    density = MAX(MIN(density, 1.), 0.);

    fog_params[0] = fog_density;
    fog_params[1] = .23 + density *.57;
    fog_params[2] = .35 + density *.45;
    fog_params[3] = .78 + density *.22;

    dialog("Press 'Esc' key to quit...");
    dialog("Please wait while terrain is generated...");
}
				    
#define	SHMEMSIZE   (10*(1<<20)) /* Size of shared memory == 10Meg */

typedef struct sigcontext sigcontext;
void segv(int sig, int code, sigcontext*);

void
init_shmem(void)
{
    int             i;
    int             shmem_fd;
    void           *shmem_addr;
    unsigned long  *flagsptr;
    perfobj_vert_t *vertsptr;
    int             nflags, nverts;
    char           *tmpdir;

    signal(SIGSEGV, segv);

    if(tmpdir = getenv("TMPDIR")) {
	sprintf(sky_shmem_file, "%s/sky_shmem_file.XXXXXX", tmpdir);
	sprintf(sky_sema_arena, "%s/sky_sema_arena.XXXXXX", tmpdir);
    }
    else {
	sprintf(sky_shmem_file, "/usr/tmp/sky_shmem_file.XXXXXX");
	sprintf(sky_sema_arena, "/usr/tmp/sky_sema_arena.XXXXXX");
    }

    /*
     * Get unique file names for shared memory file and semaphore arena file 
     */
    if (mktemp(sky_shmem_file) == NULL || mktemp(sky_sema_arena) == NULL)
        err_msg(ERR_FATAL | ERR_SYSERR, "init_shmem()", "mktemp() failed");

    if (!(sema_arena = usinit(sky_sema_arena))) {
	err_msg(ERR_FATAL | ERR_SYSERR, "init_shmem()", "usinit() failed");
	unlink(sky_sema_arena);
    }
    if ((shmem_fd = open(sky_shmem_file, O_RDWR | O_CREAT)) < 0)
	err_msg(ERR_FATAL | ERR_SYSERR, "init_shmem()", "open() failed");

    if ((int) (shmem_addr = mmap(NULL, SHMEMSIZE, PROT_READ | PROT_WRITE,
			    MAP_SHARED | MAP_AUTOGROW, shmem_fd, 0)) < 0) {
	err_msg(ERR_FATAL | ERR_SYSERR, "init_shmem()", "mmap() failed");
	unlink(sky_shmem_file);
    }
    /*
     * Unlink shared memory file and semaphore arena file so they are
     * removed from file system when all skyfly processes exit 
     */
    if (unlink(sky_shmem_file) < 0 || unlink(sky_sema_arena) < 0)
	err_msg(ERR_WARNING | ERR_SYSERR, "main()", "unlink() failed");

    close(shmem_fd);

    if (!(shmem_arena = acreate(shmem_addr, SHMEMSIZE, MEM_SHARED, sema_arena,
				NULL)))
	err_msg(ERR_FATAL | ERR_SYSERR, "init_shmem()", "acreate() failed");

    AMALLOC(SharedData, shared_data, 1, "init_shmem");
    AMALLOC(SharedData->terrain_cells, perfobj_t,
	    NumCells * NumCells, "init_shmem");
    AMALLOC(SharedData->terrain_cell_flags, unsigned long *,
	    NumCells * NumCells, "init_shmem");
    AMALLOC(SharedData->terrain_cell_verts, perfobj_vert_t *,
	    NumCells * NumCells, "init_shmem");

    /*
     * Allocate the flags and vertices of all terrain cells in 2 big chunks
     * to improve data locality and consequently, cache hits 
     */
    nflags = 2 * CellDim + 1;
    AMALLOC(flagsptr, unsigned long, nflags * NumCells * NumCells, "init_shmem");
    nverts = (CellDim + 1) * 2 * CellDim;
    AMALLOC(vertsptr, perfobj_vert_t, nverts * NumCells * NumCells, "init_shmem");

    if ((int) vertsptr & 0xf)
	err_msg(ERR_WARNING, "init_shmem()", "verts not quad-word aligned");

    for (i = 0; i < NumCells * NumCells; i++) {
	SharedData->terrain_cell_flags[i] = flagsptr;
	flagsptr += nflags;
	SharedData->terrain_cell_verts[i] = vertsptr;
	vertsptr += nverts;
    }
    gfxinit_done = usnewsema(sema_arena, 0);
}

/*
 * Signal handler to catch SEGV from amallocs
*/
void 
segv(int sig, int code, sigcontext *sc)
{
    if(code == ENXIO)
    	err_msg(ERR_FATAL|ERR_SYSERR, "init_shmem()", 
			"skyfly requires 10MB in /usr/tmp or in $TMPDIR");
}

/*
 * Initialize gfxpipe data structures. There is one set of semaphores 
 * per pipe.
*/
void
init_gfxpipes(void)
{
    int             i, j, npipes;

    if(SkyType == SKY_SINGLECHANNEL)
	num_pipes = 1;
    else
	num_pipes = 2;

    for (i = 0; i < num_pipes; i++) {

	AMALLOC(gfxpipes[i], gfxpipe_data, 1, "initgfxpipes");
	AMALLOC(gfxpipes[i]->buffers, buffered_data *, NBUFFERS,
		"init_gfxpipes");
	gfxpipes[i]->gfxpipenum = i;
    }

    switch (SkyType) {
    case SKY_CYCLOPS:
	for (i = 0; i < num_pipes; i++) {
	    gfxpipes[i]->sim_done = usnewsema(sema_arena, 0);
	    gfxpipes[i]->cull_done = usnewsema(sema_arena, 1);
	    gfxpipes[i]->draw_done = usnewsema(sema_arena, 2);

	    for (j = 0; j < NBUFFERS; j++) {
		AMALLOC(gfxpipes[i]->buffers[j], buffered_data, 1,
			"init_gfxpipes");

		init_buffered_data(gfxpipes[i]->buffers[j]);
	    }
	}
	break;
    case SKY_DUALCHANNEL:
	{
	    buffered_data  *buffers;

	    AMALLOC(buffers, buffered_data, NBUFFERS, "init_gfxpipes");
	    for (j = 0; j < NBUFFERS; j++)
		init_buffered_data(&buffers[j]);

	    for (i = 0; i < num_pipes; i++) {
	    	gfxpipes[i]->sim_done = usnewsema(sema_arena, 0);
	    	gfxpipes[i]->cull_done = usnewsema(sema_arena, 1);
	    	gfxpipes[i]->draw_done = usnewsema(sema_arena, 2);

		for (j = 0; j < NBUFFERS; j++)
		    gfxpipes[i]->buffers[j] = &buffers[j];
	    }
	}
	break;
    case SKY_SINGLECHANNEL:
	gfxpipes[0]->sim_done = usnewsema(sema_arena, 0);
	gfxpipes[0]->cull_done = usnewsema(sema_arena, 1);
	gfxpipes[0]->draw_done = usnewsema(sema_arena, 2);

	for (j = 0; j < NBUFFERS; j++) {
	    AMALLOC(gfxpipes[0]->buffers[j], buffered_data, 1,
		    "init_gfxpipes");

	    init_buffered_data(gfxpipes[0]->buffers[j]);
	}
	break;
    }
}

void
init_buffered_data(buffered_data *buffered)
{
    int             i;
    perfobj_t      *pobj;

    pobj = &(buffered->viewer_pos_obj);
    pobj->flags = buffered->viewer_pos_flags;
    pobj->vdata = buffered->viewer_position;

    *(buffered->viewer_pos_flags) = PD_VIEWER_POS;
    *(buffered->viewer_pos_flags + 1) = PD_END;

    for (i = 0; i < NUM_PLANES; i++) {
	pobj = &(buffered->paper_plane_pos_obj[i]);
	pobj->flags = buffered->paper_plane_pos_flags;
	pobj->vdata = buffered->paper_plane_position[i];
    }
    *(buffered->paper_plane_pos_flags) = PD_PAPER_PLANE_POS;
    *(buffered->paper_plane_pos_flags + 1) = PD_END;
}

void
init_gl(int gfxpipenum)
{
    if(SkyType != SKY_SINGLECHANNEL)
	scrnselect(gfxpipenum);

	MS=1;
    noborder();
    foreground();
    prefposition(0, getgdesc(GD_XPMAX)-1, 0, getgdesc(GD_YPMAX)-1);
    winopen("skysim");
    prefposition(0, getgdesc(GD_XPMAX)-1, 0, getgdesc(GD_YPMAX)-1);
    winconstraints();
    RGBmode();
    doublebuffer();
    MS=getgdesc(GD_MULTISAMPLE);
    if (MS) mssize(4,32,0);
    gconfig();

    frontbuffer(1);
    cpack(0x0);
    clear();
    frontbuffer(0);

    /*
     * It is not wise to use the full range of Z because of limited
     * precision in the hardware. 
     */
    lsetdepth(getgdesc(GD_ZMIN)+0xff, getgdesc(GD_ZMAX)-0xff);
    zfunction(ZF_LEQUAL);

    backface(TRUE);
    zbuffer(TRUE);
    subpixel(TRUE);		/* For maximum performance, this MUST be
				 * specified */

    init_texture_and_lighting();

    mmode(MVIEWING);
    perspective((int) (FOV * RAD_TO_DEG * 10.), (float)Wxsize/(float)Wysize,
							 .1, far_cull *.95);

    switch (SkyType) {
    case SKY_CYCLOPS:
    case SKY_SINGLECHANNEL:
	viewport(Wxorg, Wxorg + Wxsize - 1, Wyorg, Wyorg + Wysize - 1);
	break;
    case SKY_DUALCHANNEL:
	if (!VGAmode) {
	    if (gfxpipenum == 0)
		viewport(1279 - Wxsize, 1279, 
			 512 - Wysize / 2, 512 + Wysize / 2 - 1);
	    else
		viewport(0, Wxsize - 1, 512 - Wysize / 2, 
			 512 + Wysize / 2 - 1);
	} else {
	    viewport(0, 0, 639, 479);
	}
	break;
    }

    lmbind(MATERIAL, 1);

    if(fog_params[0] > 0.) {
	fogvertex(FG_PIX_EXP2, fog_params);
	fogvertex(FG_ON, fog_params);
    }

    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(RIGHTMOUSE);

    usvsema(gfxinit_done);
}

unsigned char* read_bwimage(char *name, int *w, int *h);

void
init_texture_and_lighting(void) 
{

    static float    texps[] = {
	TX_MINFILTER, TX_MIPMAP_TRILINEAR,
	TX_MAGFILTER, TX_BILINEAR,
	TX_NULL
    };

    static float    tevps1[] = {	/* For terrain */
	TV_BLEND, TV_COLOR, .1, .1, .1, 0., TV_NULL
    };

    static float    tevps2[] = {	/* For sky */
	TV_BLEND, TV_COLOR, 1., 1., 1., 0., TV_NULL
    };

    static float           def_light[] = {
	AMBIENT, 1.0, 1.0, 1.0,
	POSITION, LX, LY, LZ, 0.,
	LCOLOR, 1.0, 1.0, 1.0,
	LMNULL
    };

    static float           def_mat[] = {
	AMBIENT, 0.3, 0.3, 0.1,
	DIFFUSE, 0.7, 0.7, 0.1,
	LMNULL
    };

    static float def_model[] = {
	LMNULL
    };

    unsigned char  *bwimage256, *bwimage128;

    int             i, j, w, h;

    if(!(bwimage256 = (unsigned char*) read_bwimage("terrain.bw", &w, &h)))
       if(!(bwimage256 = (unsigned char *) 
		read_bwimage("/usr/demos/data/textures/terrain.bw", &w, &h)))
	err_msg(ERR_FATAL, "init_texture_and_lighting()", 
					"Can't open terrain.bw");

    if(w != 256 || h != 256)
	err_msg(ERR_FATAL, "init_texture_and_lighting()", 
					"terrain.bw must be 256x256");

    if (!(bwimage128 = (unsigned char *) read_bwimage("clouds.bw", &w, &h)))
	if (!(bwimage128 = (unsigned char *)
	      read_bwimage("/usr/demos/data/textures/clouds.bw", &w, &h)))
	    err_msg(ERR_FATAL, "init_misc()", "Can't open clouds.bw");

    if (w != 128 || h != 128)
	err_msg(ERR_FATAL, "init_misc()", "clouds.bw must be 128x128");

    /*
     * 1 and 2-component textures offer the highest performance on SkyWriter
     * so they are the most recommended.
    */
    texdef2d(1, 1, 256, 256, (unsigned long *) bwimage256, 0, texps);
    texdef2d(2, 1, 128, 128, (unsigned long *) bwimage128, 0, texps);
    tevdef(1, 0, tevps1);
    tevdef(2, 0, tevps2);

    lmdef(DEFMATERIAL, 1, 0, def_mat);
    lmdef(DEFLIGHT, 1, 0, def_light);
    lmdef(DEFLMODEL, 1, 0, def_model);
}

/*-------------------------------- Utility ---------------------------------*/

void 
err_msg(int type, char* func, char* error)
{
    char    msg[512];
    int	    i;

    if (type & ERR_WARNING) {
	fprintf(stderr, "Warning:  ");
	sprintf(msg, "Warning:  %s", error);
    }
    else if (type & ERR_FATAL) {
	fprintf(stderr, "FATAL:  ");
	sprintf(msg, "FATAL:  %s", error);
    }

    fprintf(stderr, "%s: %s\n", func, error);
    dialog(msg);
    if (type & ERR_SYSERR) {
	perror("perror() = ");
	fprintf(stderr, "errno = %d\n", errno);
    }
    fflush(stderr);

    if (type & ERR_FATAL) {
	unlink(sky_shmem_file);
	unlink(sky_sema_arena);
    	if (!sky_uid) {
	    for(i=0; i<4; i++)
	    	if (sysmp(MP_UNISOLATE, i) < 0)
		    err_msg(ERR_WARNING | ERR_SYSERR, "err_msg()",
		        "sysmp() failed to unisolate processor");
	}
	exit(-1);
    }
}

void dialogue(char *msg);
int num_dialogs=0;

void
dialog(char *msg)
{
    int ppid;

    num_dialogs++;
    ppid = fork();
    if(ppid == 0)
	dialogue(msg);
}

void
dialogue(char *msg)
{
    foreground();
    prefposition(280, 1000, 496-num_dialogs*64, 528-num_dialogs*64);
    winopen("Skyfly Dialogue");
    RGBmode();
    gconfig();

    cpack(0x40);
    clear();
    cpack(0xffffff);

    ortho2(-0.5, 720.5, -0.5, 32.5);

    cmov2i(32, 8);
    charstr(msg);

    sleep(6);

    gexit();
    exit(1);
}
