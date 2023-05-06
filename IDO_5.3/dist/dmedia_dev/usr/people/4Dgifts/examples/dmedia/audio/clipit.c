#include <fcntl.h>
#include <stdio.h>
#include <gl.h>
#include <gl/device.h>
#include <dmedia/audio.h>

/*
 * clipit -- a simple multi-channel audio example. We admit that
 * the drawing and window-handling in this application is not
 * exemplary; please focus on the audio code...
 * 
 * If you invoke clipit with the '-4' option,  it will record
 * and play 4 channels instead of 2.
 */

typedef signed short SAMPLE;
#define MAXBUFSIZE 1000000
#define BUFCHUNKSIZE 2000
#define SIGLIMIT 32768
#define MAXTRACKS 4
#define POSMAX 32767
#define NEGMAX -32768

#define MIN(A,B)  ( (A) < (B) ? (A) : (B)  )
#define MAX(A,B)  ( (A) > (B) ? (A) : (B)  )

SAMPLE inbuf[MAXTRACKS*MAXBUFSIZE];

long nsamps,screenx0,screeny0,screenwidth,screenheight;
int ntracks=2;
/*
 * ntracks is (2^(trackshift-1))
 */
int trackshift=2;
int firstvisible,nvisible;
int nselected, firstselected, lastselected;
int BytesPerSample = 2;
float leftedge,rightedge;
int wid;    /* our window ID */
int copysize;
ALconfig conf;

main(argc,argv)
int argc;
char *argv[];
{
	
	int temp,i;
	long dev;
	short data;
	int mainmenu, puprequest;

	copysize = 0;
	
	if(argc == 2) {
	    long min, max;
	    if (strcmp(argv[1], "-4")) {
		fprintf(stderr, "usage: %s [-4]\n", argv[0]);
		exit(-1);
	    }
	    ntracks = 4;
	    trackshift = 3;
	    /*
	     * query to see if we are on a machine with 4-channel
	     * HW capability. If so,  switch into 4-channel mode.
	     * If AL_CHANNEL_MODE both exists (ALgetminmax doesn't
	     * fail) AND has a maximum of 4,  then we're OK.
	     * 
	     * If we wanted to be really nice,  we could check, 
	     * by querying AL_INPUT_COUNT and AL_OUTPUT_COUNT, to
	     * see if any other apps were doing audio. If so,  we
	     * might not want to switch to 4-channel mode,  lest
	     * we introduce artifacts into their audio streams.
	     */
	    if (ALgetminmax(AL_DEFAULT_DEVICE, AL_CHANNEL_MODE, 
		&min, &max) >= 0 && max == 4) {
		long buf[2];
		buf[0] = AL_CHANNEL_MODE;
		buf[1] = 4;
		ALsetparams(AL_DEFAULT_DEVICE, buf, 2);
	    }
	    /*
	     * Even if we don't have 4-channel HW capability, 
	     * the AL will let us use a 4-channel buffer,  so 
	     * we can continue at this point without regard to
	     * HW type.
	     */
	}

	ALseterrorhandler(0);
	conf = ALnewconfig();
	ALsetwidth(conf,AL_SAMPLE_16);
	ALsetchannels(conf,ntracks);

	bzero(inbuf, sizeof(inbuf));
	nsamps = 1024;
	nvisible = nsamps;
	firstvisible = 0;
	leftedge=0;
	rightedge=nsamps-1;
	firstselected = 0;
	nselected = nsamps;

	prefposition(200,1200, 300,700);
	wid = winopen("Sound Snippet");
	winconstraints();
	qdevice(LEFTMOUSE);
	qdevice(MIDDLEMOUSE);
	qdevice(RIGHTMOUSE);
	qdevice(SPACEKEY);
	
	refresh();
	
	mainmenu = 
  defpup("Clipit %t| play | record  | zoom in | zoom out |quit");
	while (TRUE) {
		dev=qread(&data);
		if(dev == LEFTMOUSE && data == 1) {
			data = make_new_space(LEFTMOUSE,TRUE);
			refresh();
			speak();
		} else if(dev == MIDDLEMOUSE && data == 1) {
			qread(&data);
			rightedge = ((nsamps-1) * (data-screenx0)+0.0)/screenwidth;
			refresh();
			speak();
		} else if(dev == RIGHTMOUSE && data == 1) {
			puprequest = dopup(mainmenu);
			switch ( puprequest ) {
			case 1:
				speak();
				break;
			case 2:

				nsamps = recordbuffer(inbuf,MAXBUFSIZE*ntracks);
				firstvisible = 0;
				nvisible = nsamps;
				leftedge=0;
				rightedge=nsamps-1;
    				firstselected = leftedge;
    				lastselected = rightedge;
    				nselected = lastselected-firstselected; 
  				refresh();
				
				break;

			case 3:
				if( nselected < screenwidth) 
					{
					/*
					firstvisible = firstvisible +
					  (screenwidth-nselected)/2);
					*/
					break;
					}
				firstvisible = firstvisible+firstselected;
				firstselected = 0;
				nvisible = nselected;
				refresh();
				break;
			case 4: 
				firstselected += firstvisible;
				firstvisible = 0;
				nvisible = nsamps;
				refresh();
				break;
			case 5:
				exit(0);
			default:
				qreset();
				break;
			}
			
		} else if(dev == REDRAW) {
			refresh();
		}
		
	}
}

speak() 
{
	int l,r,w,firstsamp;
	int ntoplay,nplayed,nleft,duration;
	short val,dev;
	int audio;
	int err;
	register SAMPLE *p;
	ALport port;
	port = ALopenport("clipit play", "w", conf);
	if ( port ==  (ALport) 0) {
		err = oserror();
		if (err == AL_BAD_NO_PORTS) {
			fprintf(stderr, " System is out of audio ports\n");
		} else if (err == AL_BAD_DEVICE_ACCESS) {
			fprintf(stderr, " Couldn't access audio device\n");
		} else if (err == AL_BAD_OUT_OF_MEM) {
			fprintf(stderr, " Out of memory: port open failed\n");
		}
		
 		exit(1);
	}

	p = inbuf+(ntracks*(firstselected+firstvisible));
        nleft = nselected*ntracks;
	while (nleft > 0)
		{
		ntoplay = MIN(BUFCHUNKSIZE, nleft);

		ALwritesamps(port, (void *)p, ntoplay);
		nplayed = ntoplay;
		p += nplayed;
		nleft -= nplayed;
		if (qtest()) 
			{
			dev = qread(&val);
			if (dev == SPACEKEY || dev == RIGHTMOUSE || dev == LEFTMOUSE || dev == MIDDLEMOUSE)
			  {  qread(&val); break;}
			}
		}

	/* 
	 * wait for the port to drain before we close. Otherwise, 
	 * when we close, samples still left in the port will be
	 * cut off.
	 */
	while(ALgetfilled(port))
		sginap(1);
	ALcloseport(port);
	return(1);
}

#define BASE(x) (((SIGLIMIT<<trackshift)-SIGLIMIT)-(2*SIGLIMIT)*x)

refresh()
{
	int i,j,k,l;
	int sampsperpixel;
	int min,max;
	long x[2];
	long dev;
	register SAMPLE *p;
	reshapeviewport();
	
	getorigin(&screenx0,&screeny0);
	getsize(&screenwidth,&screenheight);
	ortho2(0,nvisible-1,0,(SIGLIMIT<<trackshift));
	color(8);
	clear();
	color(43);
	rectf(firstselected,(SIGLIMIT>>3),firstselected+nselected,(SIGLIMIT<<trackshift)-(SIGLIMIT>>3));
	color(3);
	sampsperpixel = nvisible/screenwidth;
	if (sampsperpixel < 1 ) sampsperpixel = 1;
	ortho2(0,screenwidth-1,0,(SIGLIMIT<<trackshift));


	for (i = 0; i < ntracks; i++) {
	    p = inbuf+(firstvisible*ntracks)+i;
	    l = MIN(screenwidth, nvisible);
	    color(i);
	    bgnline();
	    x[0] = 0;
	    x[1] = BASE(i);
	    v2i(x);
	    for (j=0;j<l;j++) {
		max = NEGMAX;
		min = POSMAX;
		for(k=0;k<sampsperpixel;k++,p+=ntracks) {
		    if (*p > max) {
			max = *p;
		    }
		    if (*p < min) {
			min = *p;
		    }
		}
		x[0] = j;
		x[1] = BASE(i)+min;
		v2i(x);
		x[0] = j;
		x[1] = BASE(i)+max;
		v2i(x);
	    }
	    endline();
	}
	ortho2(0,nvisible-1,-(SIGLIMIT),(SIGLIMIT));
}


int recordbuffer(buf,size) SAMPLE *buf; int size;
	{
	int  ntorecord, ntotal, nleft, nbufs =0, nwaiting;
	short val;
	int audio;
	int err;
	ALport port;
	char *bp;
	long dev;
	nleft = size;
	ntotal =0;
	qreset();
	port = ALopenport("clipit rrecord", "r", conf);
	if ( port == (ALport) 0) {
	    err = oserror();
	    if (err == AL_BAD_NO_PORTS) {
		    fprintf(stderr, " System is out of audio ports\n");
	    } else if (err == AL_BAD_DEVICE_ACCESS) {
		    fprintf(stderr, " Couldn't access audio device\n");
	    } else if (err == AL_BAD_OUT_OF_MEM) {
		    fprintf(stderr, " Out of memory: port open failed\n");
	    }
	    exit(-1);
	}
	ortho2(0,MAXBUFSIZE*ntracks,-(SIGLIMIT<<1),(SIGLIMIT<<1));
	color(8);
	clear();
	color(43);
	while (nleft > 0)
		{
		nwaiting = ALgetfilled(port);
		ntorecord = MIN(nleft, MAX(BUFCHUNKSIZE,nwaiting));
		ALreadsamps(port, (void *) buf, ntorecord);
		
		nbufs++;
		buf += ntorecord;
		nleft -= ntorecord;
		ntotal += ntorecord;
		if (qtest () ) 
			{
			dev = qread(&val);
			if (dev == SPACEKEY || dev == RIGHTMOUSE || dev == LEFTMOUSE || dev == MIDDLEMOUSE)
				 {qread(&val); break;}
			}
	 	if( nleft < BUFCHUNKSIZE ) break;
		rectf(0,-(SIGLIMIT>>3),ntotal,(SIGLIMIT>>3));
		}
	nwaiting = ALgetfilled(port);
	ntorecord = MIN(nleft, MAX(BUFCHUNKSIZE,nwaiting));
	ALreadsamps(port, (void *) buf, ntorecord);
	ntotal += ntorecord;
	ALcloseport(port);
	return(ntotal/ntracks);
	}

make_new_space(button,maintainaspect) short button; int maintainaspect;
{

    long            mx, omx,  cx,  bx,  obx;
    int fgrndcolor;
    float tmp;
    short dev,val;
    
    drawmode(OVERDRAW);

    fgrndcolor = 1;
    qreset();

    omx = mx = getvaluator(MOUSEX) - screenx0;
 
    obx = bx = cx = mx;

    color(0);
    clear();


    do {
	while (!qtest()) {

		mx = getvaluator(MOUSEX) - screenx0;

		if (mx != omx ) {

		bx = mx;
		color(0);

		leftedge = ((nvisible-1) * (cx+0.0))/ screenwidth; 
		rightedge = ((nvisible-1) * (obx+0.0))/ screenwidth; ;
		rect(leftedge, -SIGLIMIT, rightedge, SIGLIMIT);

		obx = bx;
		omx = mx;

		leftedge = ((nvisible-1) * (cx+0.0))/ screenwidth; 
		rightedge = ((nvisible-1) * (bx+0.0))/ screenwidth; ;

		color(fgrndcolor);
		rect (leftedge, -SIGLIMIT, rightedge, SIGLIMIT);
		}
    	}
    } while ((dev = qread(&val)) != button );
    color(0);
    clear();

    drawmode(NORMALDRAW);
    if ( leftedge > rightedge)
		{
		tmp =leftedge;
		leftedge =rightedge;
		rightedge = tmp;
		}
  
    firstselected = leftedge;
    lastselected = rightedge;
    nselected = lastselected-firstselected+1;   
    return(val);
}


