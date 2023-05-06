/* Detail - Detailed textures are a new addition to Reality Engines.
** Large textures are very expensive in terms of texture memory allocation.
** The use of detail is a method to preserve image quality while reducing
** the amount of memory required. The detail method is a way of extracting
** high frequency information to reduce the size of the texture. 
** Please refer to the Graphics Library Programming Guide chapter 18 
** for more details on using and creating the base and detail textures.
** On page 18-28, there is a cook book outline on creating the detail
** texture. Note that there is a typo. The utility "subimage" should
** be "subimg".
**
** Run-time interface:
** ESCKEY	-   exit program
** MIDDLEMOUSE	-   scale world bassed on MOUSEX position.
** RIGHTMOUSE   -   popup menu to change the detail parameters.
*/

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define SUCCESS 100
#define SPLINE1 1
#define SPLINE2 2
#define SPLINE3 3
#define SPLINE4 4
#define EXIT 9

void update_spline(int, int);
void gr_charstr(float);

int update_mag(int);
int update_onoff(void);
int update_mn(int);
int update_mag(), update_mn(), update_onoff();

float filters[3] = { TX_BILINEAR, TX_MODULATE_DETAIL, TX_ADD_DETAIL };
static char *MagFilters[] = {"TX_BILINEAR", "TX_MODULATE_DETAIL",
 							"TX_ADD_DETAIL" };

static char *Detail[] = { "OFF", "ON" };
static float DetailMN[] = { 4., 8., 16., 32., 64. };

static float Spline[] = { 0.0, 0.0,
			  3.0, 3.0,
			  4.0, 4.0,
			  6.0, 6.0
			  };

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
	{0., 0.},
	{1., 0.},
	{1., 1.},
	{0., 1.},
	};

float texps_lowres[] = { 
        TX_MAGFILTER, TX_MODULATE_DETAIL,
	TX_CONTROL_POINT, 0., 0.,
	TX_CONTROL_POINT, 3., 3.,
	TX_CONTROL_POINT, 4., 4.,
	TX_CONTROL_POINT, 6.0, 6.0,
        TX_CONTROL_CLAMP, 7.0,
        TX_NULL};

static float texps_detail[] = { 
        TX_DETAIL, 4., 4., 4., 4., 0.,
	TX_NULL};

static float tevps[] = {TV_NULL};

static long win_xorigin=20, win_yorigin=600, 
    	    win_xmax=420, win_ymax=1000,
    	    win_xsize=600, win_ysize=600;

static unsigned long *lowres_texture;
static unsigned long *detail_texture;

static int index = 1;
static int onoff = 1;
static int mn = 0;

int pupval,lodval, mainmenu, spline_lod, spline_weight1,
    spline_weight2, spline_weight3, spline_weight4,  magfilters, mn_vals;

static void init_queue(void)
{
    qdevice(RIGHTMOUSE);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(REDRAW);
    qdevice(ESCKEY);
}

/* Initialize the window. See the comment in the do_events()
** subroutine on the reasoning for the spline_weight return values.
*/

static void init_window(void)
{
    float xsize, ysize, osize;

    keepaspect(1, 1);
    prefsize(win_xsize, win_ysize);
    foreground();
    winopen("Detail Textures");
    minsize(win_xsize, win_ysize);
    winconstraints();
    RGBmode();
    RGBsize(8);
    doublebuffer();
    gconfig();
    zbuffer(TRUE);
    subpixel(TRUE);

    magfilters = defpup("Mag Filters%t%F|TX_BILINEAR%x0|\
			 TX_MODULATE_DETAIL%x1|TX_ADD_DETAIL%x2", update_mag);
    mn_vals = defpup("M,N VALS%t%F|4x4|8x8|16x16|32x32|64x64",update_mn);
    spline_weight1 = defpup("Spline WEIGHT%t|0%x10|1%x11|2%x12|3%x13|\
					     4%x14|5%x15|6%x16|7%x17");
    spline_weight2 = defpup("Spline WEIGHT%t|0%x20|1%x21|2%x22|3%x23|\
					     4%x24|5%x25|6%x26|7%x27");
    spline_weight3 = defpup("Spline WEIGHT%t|0%x30|1%x31|2%x32|3%x33|\
					     4%x34|5%x35|6%x36|7%x37");
    spline_weight4 = defpup("Spline WEIGHT%t|0%x40|1%x41|2%x42|3%x43|\
					     4%x44|5%x45|6%x46|7%x47");
    spline_lod = defpup("Spline LOD%t|1%m|3%m|4%m|6%m",spline_weight1,
			spline_weight2,spline_weight3,spline_weight4);
    mainmenu = defpup("Paramters%t|Mag Filter%m|M,N%m|On Off%f|Splines%m|\
		     Exit%x9", magfilters,mn_vals,update_onoff,spline_lod);

    getorigin(&win_xorigin, &win_yorigin);
    getsize(&win_xsize, &win_ysize);
    init_queue();

    mmode(MPROJECTION);
    perspective(600,1.*win_xsize/win_ysize,0.1,20.);
    mmode(MVIEWING);
    translate(0.0, 0.0, -6.0);
    rot(-90, 'x');
    pushmatrix();
}


/* Detail textures are new to Reality Engine. Note that we still
** use the texdef call to define a detail. We also use texbind to 
** activate the detail, however the target is new : TX_TEXTURE_DETAIL.
*/

static void init_texture(void) 
{
extern unsigned long *longimagedata(char *name);
    int i;

    lowres_texture = longimagedata("lowres.rgb");
    texdef2d(1, 4, 256, 256,lowres_texture,0,texps_lowres);
    detail_texture = longimagedata("detail.rgb");
    texdef2d(2, 4, 256,256,detail_texture,0,texps_detail);

    tevdef(1, 0, tevps);
    tevbind(0, 1);
    texbind(TX_TEXTURE_0, 1);
    texbind(TX_TEXTURE_DETAIL, 2);
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

/* Use charstr to show what all the current parameters are.
** It is necessary to unbind the current texture so we can 
** specify the color of the characters ourselves.
*/

static void draw_buttons(void)
{
    texbind(TX_TEXTURE_0, 0);
    ortho2(-.5, (float)win_xsize -.5, -.5, (float)win_ysize/8.f -.5);
    pushmatrix();
    loadmatrix(idmat);
    cpack(0x00ffffff);
    cmov2i(10, (int)(win_ysize/11.f));
    charstr(MagFilters[(index)%3]);
    cmov2i(10, (int)(win_ysize/17.f));
    charstr("Detail M,N : ");
    gr_charstr(DetailMN[(mn)%5]);
    cmov2i(10, (int)(win_ysize/35.f));
    charstr(Detail[onoff%2]);
    cmov2i(200, (int)(win_ysize/35.f));
    gr_charstr(Spline[0]);
    charstr("    ");
    gr_charstr(Spline[2]);
    charstr("    ");
    gr_charstr(Spline[4]);
    charstr("    ");
    gr_charstr(Spline[6]);
    cmov2i(200, (int)(win_ysize/16.f));
    gr_charstr(Spline[1]);
    charstr("    ");
    gr_charstr(Spline[3]);
    charstr("    ");
    gr_charstr(Spline[5]);
    charstr("    ");
    gr_charstr(Spline[7]);
    popmatrix();
    perspective(600,1.*win_xsize/win_ysize,0.1,20.);
    texbind(TX_TEXTURE_0, 1);
}

static void draw_scene(void) 
{
    viewport(0,win_xsize,(int)(win_ysize/8.f), win_ysize);
    cpack(0xff101010); 
    clear(); zclear();
    draw_poly();    
    viewport(0,win_xsize,0, (int)(win_ysize/8.f));
    cpack(0xfff0a000);
    clear(); zclear();
    draw_buttons();
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
	    scale_factor = 1.0 + ((float) (xdist)) / ((float) win_ysize);
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
    int do_draw = 0;
    long x,y;
    
    while (1) {
	if(qtest()) switch (dev = qread(&val)) {
	    case REDRAW:
		keepaspect(1, 1);
		reshapeviewport();
		getorigin(&win_xorigin, &win_yorigin);
		getsize(&win_xsize, &win_ysize);
		draw_scene();
		break;
	    case ESCKEY: 
		gexit();
		exit(0);
		break;
	    case MIDDLEMOUSE:
		if (val)  {
		    do_scale();
		    do_draw = 1;
		}
		break;
	    case RIGHTMOUSE:
		if (val) {
		   pupval = dopup(spline_lod);

/* The menu return values for the spline LOD's and the
** corresponding weights were chosen such that each sequence
** corresponded to a particualr LOD. So dividing the return
** value by 10 gave the LOD number and the MOD value gave
** the actual weight value.
*/
		   switch(pupval/10){
			case SPLINE1:
			case SPLINE2:
			case SPLINE3:
			case SPLINE4:
			     update_spline(pupval/10, pupval%10);
			     break;
			case EXIT:
			     gexit();
			     exit(0);
			     break;
	    		default:
	         	     break;
		   }
		}
		break;
	    default:
	         break;
	} /* switch */
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

/* When selecting various magnifacation filters, it's possible that
** bilinear may be chosen. If so, we want to unbind the detail texture.
*/

int
update_mag(int filt)
{
	index = filt;
    	wintitle(MagFilters[index]);
	texps_lowres[1] = filters[index];
        if (!index) {
	     onoff = 1;
	     update_onoff();
	}
	else {
	     onoff = 0;
	     update_onoff();
	}
	return SUCCESS;
}

/* This subroutine is designed to allow the user to turn on and
** off the detail texture. When detail is turned off, the base
** magnification filter is changed to bilinear. Turning the detail
** back on with just the "on" selection only rebinds the detail texture.
** the base texture magnification needs to be changed as well.
*/

int
update_onoff(void)
{
	onoff++;
	if(onoff%2){
	    texbind(TX_TEXTURE_DETAIL, 2);
	    texps_lowres[1] = filters[index%3];
	    texdef2d(1, 4, 256, 256,lowres_texture,1,texps_lowres);
    	    texbind(TX_TEXTURE_0, 1);
	}
	else 
	{ 
    	    texbind(TX_TEXTURE_DETAIL, 0);
	    texps_lowres[1] = TX_BILINEAR;
	    index = 0;
	    texdef2d(1, 4, 256, 256,lowres_texture,1,texps_lowres);
    	    texbind(TX_TEXTURE_0, 1);
	}
	return SUCCESS;
}

/* The M,N values in the detail array correspond to pixel
** coverage for the detail. When it's changed it's necessary
** to redefine and bind the detail texture. The base texture
** isn't modified.
*/

int
update_mn(int val)
{
	mn = val -1;
	texps_detail[3] = DetailMN[mn];
	texps_detail[4] = DetailMN[mn];

	texps_detail[index+2] = Spline[index];
	texdef2d(1, 4, 256, 256,detail_texture,1,texps_detail);
        texbind(TX_TEXTURE_DETAIL, 2);

	return SUCCESS;
}

/* When the spline weights are altered, it is necessary 
** to redefine and bind the textrure again. 
*/

void
update_spline(int lod, int weight)
{
    int i;
    Spline[2*lod-1] = (float) weight;
 
    texps_lowres[3*lod+1] = Spline[2*lod-1];
    texdef2d(1, 4, 256, 256,lowres_texture,0,texps_lowres);
    texbind(TX_TEXTURE_0, 1);
}

/* Since charstr won't convert floats directly
** we need to do it here via sprintf.
*/
void
gr_charstr(float num)
{
	char str[32];

	sprintf(str, "%1.2f", num);
	charstr(str);
}
