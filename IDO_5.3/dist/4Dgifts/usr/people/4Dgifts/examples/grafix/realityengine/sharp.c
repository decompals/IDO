#include <gl/gl.h>
#include <gl/device.h>

static char *MagFilters[] = {"TX_MAGFILTER", "TX_MAGFILTER_COLOR",
 				"TX_MAGFILTER_ALPHA", "TX_BILINEAR" };

static float vert_data[8][4]= {
	-1.0, -3.0, -1.0, 0.0,
	 1.0, -3.0, -1.0, 0.0,
	 1.0, -3.0,  1.0, 0.0,
	-1.0, -3.0,  1.0, 0.0,
	};

static float idmat[4][4]= {
	1.0, 0.0, 0.0, 0.0,
	0.0, 1.0, 0.0, 0.0,
	0.0, 0.0, 1.0, 0.0,
	0.0, 0.0, 0.0, 1.0
	};

static float tex_data[4][2]= {
	{0.01, 0.01},
	{0.99, 0.01},
	{0.99, 0.99},
	{0.01, 0.99},
	};

static long win_xorigin=20, win_yorigin=600, xsize, ysize; 

static unsigned long *tree_texture;
static unsigned long grid_texture[128*128];

static int index = 0;
static int whichtex = 0;  /* 0 = tree, 1 = grid */

float texps_trilinear[] = { TX_MAGFILTER, TX_BILINEAR, 
			TX_INTERNAL_FORMAT, TX_RGBA_8, TX_NULL};
float texps_magfilter_both[] = { TX_MAGFILTER, TX_SHARPEN,
			TX_INTERNAL_FORMAT, TX_RGBA_8,  TX_NULL};
float texps_magfilter_color[] = { TX_MAGFILTER_COLOR, TX_SHARPEN,
			TX_INTERNAL_FORMAT, TX_RGBA_8,  TX_NULL};
float texps_magfilter_alpha[] = { TX_MAGFILTER_ALPHA, TX_SHARPEN,
			TX_INTERNAL_FORMAT, TX_RGBA_8,  TX_NULL};

static float tevps[] = {TV_NULL};

static void init_queue(void)
{
    qdevice(RIGHTMOUSE);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(REDRAW);
    qdevice(ESCKEY);
}

static void init_window(void)
{

    prefsize(500,500);
    foreground();
    winopen("SHARPEN TEXTURES");
    keepaspect(1, 1);
    winconstraints();
    RGBmode();
    RGBsize(8);
    doublebuffer();
    gconfig();
    frontbuffer(TRUE);
    cpack(0);
    clear();
    cpack(0x00ffff);
    cmov2i(175,250);
    charstr("HOLD ON PLEASE");
    frontbuffer(FALSE);
    zbuffer(TRUE);
    subpixel(TRUE);
    afunction(0, AF_NOTEQUAL);

    getorigin(&win_xorigin, &win_yorigin);
    getsize(&xsize, &ysize);
    init_queue();

    mmode(MPROJECTION);
    perspective(600,1.,0.1,20.);
    mmode(MVIEWING);
    translate(0.0, 0.0, -6.0);
    rot(-90, 'x');
    pushmatrix();
}


static void init_texture(void) 
{
extern unsigned long *longimagedata(char *name);
    int i, j;

    tree_texture = longimagedata("tree1.rgba");
/* initialize to all white */
    for(i=0; i<128*128; i++ ) grid_texture [i] = 0xffffffff;
/* add black stripes */
    for(i=0; i<128; i++ )
        for(j=0; j<128; j++)
           if(!(j%32)) grid_texture [i*128 + j] = 0;

    for(i=0; i<128; i++ )
        for(j=0; j<128; j++)
           if(!(j%32)) grid_texture [j*128 + i] = 0;

    texdef2d(1, 4, 128, 128,tree_texture,0,texps_magfilter_both);
    texdef2d(2, 4, 128, 128,tree_texture,0,texps_magfilter_color);
    texdef2d(3, 4, 128, 128,tree_texture,0,texps_magfilter_alpha);
    texdef2d(4, 4, 128, 128,tree_texture,0,texps_trilinear);
    texdef2d(5, 4, 128, 128,grid_texture,0,texps_magfilter_both);
    texdef2d(6, 4, 128, 128,grid_texture,0,texps_magfilter_color);
    texdef2d(7, 4, 128, 128,grid_texture,0,texps_magfilter_alpha);
    texdef2d(8, 4, 128, 128,grid_texture,0,texps_trilinear);

    tevdef(1, 0, tevps);
    tevbind(0, 1);
    texbind(TX_TEXTURE_0, 1);
    wintitle(MagFilters[index]);
}


static void draw_poly(void)
{    
	cpack(0xffffffff);
	bgnpolygon();
	    t2f(tex_data[0]); v3f(vert_data[0]);
	    t2f(tex_data[1]); v3f(vert_data[1]);
	    t2f(tex_data[2]); v3f(vert_data[2]);
	    t2f(tex_data[3]); v3f(vert_data[3]);
	endpolygon();
}

static void bind_tex(void)
{
       texbind(TX_TEXTURE_0, 1+index%4+4*whichtex);
}

static void draw_scene(void) 
{
    cpack(0xff101010); 
    clear(); zclear();
    draw_poly();    
    swapbuffers();
}


static void do_scale(void)
{
    long oxpos,  oypos;
    long xpos,  ypos;
    long xdist,  ydist;
    float scale_factor;
    
    oxpos = getvaluator(MOUSEX) - win_xorigin;
    oypos = getvaluator(MOUSEY) - win_yorigin;
    while (getbutton(MIDDLEMOUSE)) {
	xpos = getvaluator(MOUSEX) - win_xorigin;
	ypos = getvaluator(MOUSEY) - win_yorigin;
	xdist = xpos - oxpos;
	ydist = ypos - oypos;
	
	if (xdist || ydist) {
	    scale_factor = 1.0 + ((float) (xdist)) / ((float) ysize);
	    scale(scale_factor, scale_factor, scale_factor);
	    draw_scene();

	}
	oxpos = xpos;
	oypos = ypos;
    }
}


static void do_events(void)
{
    int dev;
    short val;
    long x,y;
    
    while (1) {
	if(qtest()) switch (dev = qread(&val)) {
	    case REDRAW:
		keepaspect(1, 1);
		reshapeviewport();
		getorigin(&win_xorigin, &win_yorigin);
    		getsize(&xsize, &ysize);
		break;
	    case ESCKEY: 
		gexit();
		exit(0);
		break;
	    case LEFTMOUSE:
		if (val)  whichtex = 1 - whichtex;
		break;
	    case MIDDLEMOUSE:
		if (val)  do_scale();
		break;
	    case RIGHTMOUSE:
		if (val)  wintitle(MagFilters[(++index)%4]);
		break;
	    default:
		break;
	} /* switch */
	bind_tex();
	draw_scene();
    } /* while 1 */
}

void
main(int argc, char **argv)
{
    int i;

    init_window();
    init_texture();
    draw_scene();
    do_events();
}

