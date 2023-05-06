/*
 *   swirl.c:
 *
 *    a standby vector swirl program.  Window created in / managed by X,
 *  drawing done by the GL.  swirl randomly picks 2-D vertices, and uses
 *  GL line drawing to create a series of lines with 8 different RGB
 *  colors.  These lines are continuously rotated while their lengths
 *  are altered within the X window the GL is drawing them within.  
 *
 *          swirl provides the following 3 options:
 *
 *          swirl -h            print the usage message; then exit
 *          swirl -sleep num    suspend drawing a line for number of 
 *                              milliseconds.  The default is 50ms.
 *          swirl -batch num    set number of lines per batch.  The
 *                              default is 64.   The total number of
 *                              lines is equal to ((batch num + 1) * 2).
 *
 *                                          Judy Hsu - March 1991
 */

#include "swirl.h"
#include <gl/glws.h>
#include <stdio.h>
#include <stdlib.h>
#include "glxhelper.h"

extern void storeRGB();
extern int  msleep();
extern void initline();
extern void drawline();

void usage(progname)
char *progname;
{
     int i = 0;

     printf("Usage:   %s [-options ...]\n\n", progname);
     printf("where options include:\n");

     while(i < OPTIONS)
	 printf(option[i++]);
	
     exit(0);
}

void CheckArg(argc,argv)
int  argc;
char **argv;
{
     char *progname;
     Bool batchflag = FALSE;
     Bool sleepflag = FALSE;

     if(argc != 1) {
	progname = argv[0];
	for(argc--,argv++; argc > 0; argc--,argv++) {

	    if(**argv != '-')
	       usage(progname);

	    switch(argv[0][1]) {
		case 'h':
		     usage(progname);
                case 's':
		     if(!strcmp(argv[0], "-sleep")) {
			sleeptime = atoi(argv[1]);
			sleepflag = TRUE;
			argv++;
			argc--;

			printf("sleeptime = %d\n", sleeptime);

		     } else usage(progname);
		     break;
		case 'b':
		     if(!strcmp(argv[0], "-batch")) {
			batchcount = atoi(argv[1]);
			batchflag = TRUE;
			argv++;
                        argc--;

			printf("batch num = %d\n",batchcount);

		     } else usage(progname);
		     break;
		default:
		     usage(progname);
	    }
	}
     }

     if(!sleepflag)
	sleeptime = SLEEP;
     if(!batchflag)
	batchcount = BATCH;
}
     


void main(argc,argv)
int  argc;
char **argv;
{
     Display  *Dpy;
     Window   root,glW;
     int      screen_num;
     XEvent event;


     CheckArg(argc,argv);

     if((Dpy=XOpenDisplay(0)) == NULL) {
         fprintf(stderr,"\nSwirl can not connect to X server\n");
         exit(-1);
     }

     screen_num = DefaultScreen(Dpy);
     root = RootWindow(Dpy,screen_num);

     glW = GLXCreateWindow(Dpy, root, 0, 0, 700, 700, 0, GLXrgbSingleBuffer);

     XStoreName(Dpy, glW, "swirl:  standby vector drawing program");
     XMapWindow(Dpy,glW);
     XSelectInput(Dpy,glW,OwnerGrabButtonMask|ButtonPressMask|ExposureMask|KeyPressMask);
     XSync(Dpy,False);

     if (GLXwinset(Dpy,glW) < 0) {
        fprintf(stderr, "\nGLXwinset is < 0\n");
        exit(-1);
     }

     storeRGB();

     do {
         initline(Dpy,glW);

         do {
	     while(!XPending(Dpy)) {
	           drawline(Dpy,glW);
	           msleep(sleeptime);
	     }
	     XNextEvent(Dpy,&event);
         } while (event.type != KeyPress && event.type != Expose);
     } while (event.type != KeyPress);

     GLXunlink(Dpy,glW);
     exit(0);
}
