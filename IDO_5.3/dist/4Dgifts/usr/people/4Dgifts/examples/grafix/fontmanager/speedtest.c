/*
 *  speedtest.c:
 *
 *    Benchmark test to see how long it takes to write out a certain font 
 *  using.  Anything which generates a REDRAW event will cause the test to
 *  be run again.  
 *	
 *				Glen Williams - 1987
 */

#include <sys/time.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <fmclient.h>

double atof();

struct timeval family_begin, family_end;;
struct timeval time_begin, time_end;
struct timezone tz;

int height;


main(argc,argv)
int argc;
char **argv;
{
    fmfonthandle f;
    fmfonthandle fsized;
    fmfontinfo finfo;
    char winname[100];
    char *str;
    float size;
    short val;


    if(argc<3) {
	printf("usage: speedtest fontname size [str]\n");
	printf("Benchmark time it takes to write out a string 2000 times\n");
	exit(1);
    }
    size = atof(argv[2]);
    if(argc>3)
	str = argv[3];
    else
	str = "A string of sample text.";

    fminit();
    f = fmfindfont(argv[1]);
    if (!f) {
	printf("can't find font \"%s\"\n", argv[1]);
	exit (-1);
    }
    fsized = fmscalefont(f, size);
    fmsetfont(fsized);
    fmgetfontinfo(fsized, &finfo);
    height = finfo.height;

    sprintf(winname, "%s %s %.1f", argv[0], argv[1], size);
    winopen(" ");
    wintitle(winname);
    qdevice(KEYBD);

    drawit(str);
    report();
    while(1) {
	switch(qread(&val)) {
	case KEYBD:
            gexit();
	    exit(0);
            break;
	case REDRAW:
     	    qreset();
	    drawit(str);
	    report();
	    break;
	}
    }
}


drawit(str)
char *str;
{
    Icoord x, y;
    long win_width, win_height;
    int i;

    printf("\nDrawing\n\n");
    reshapeviewport();
    color(7);
    clear();
    color(0);
    getsize(&win_width, &win_height);
    gettimeofday(&time_begin, &tz);
    x = 20;
    y = win_height-height-5;

    for(i=0; i<2000; i++) {
	cmov2i(x,y);
	fmprstr(str);
    }
    gettimeofday(&time_end, &tz);
    printf("%d iterations of writing string\n", i);
}
    

report()
{
    long t, ut;
    static double sum_t;
    static rept_iteration;

    rept_iteration++;
    ut = time_end.tv_usec-time_begin.tv_usec;
    t = time_end.tv_sec-time_begin.tv_sec;
    if(t && time_begin.tv_usec) {
	ut = (1000000 - time_begin.tv_usec) + time_end.tv_usec;
	if(ut<1000000)
	    t--;
	else
	    ut = ut - 1000000;
    }

    sum_t = sum_t + t*1000000;
    sum_t = sum_t + ut;

    printf("Interval: %d.%d\n", t, ut);
    printf("Accumulated time: %4.3f seconds\n", sum_t/1000000);
    printf("Average: %4.3f seconds\n", (double)(sum_t/1000000)/rept_iteration);
}
