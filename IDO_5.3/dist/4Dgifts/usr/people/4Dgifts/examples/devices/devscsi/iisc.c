/*
|| iisc.c - send image(s) to Apple LaserWriter IISC
||
|| dslib version - uses dsopen, etc.
||
|| Several features are dummied or simply ignored at present, as:
||
||   Command line parsing
||   Environment variable parsing
||
||   buffered mode
||   manual paper feeding
||   multiple copies of pages
||   RAM-less (direct) mode
||   variable page size
||
||
||	Accepts an image from standard input, which may be either
||  in the format described by rasterfile.h (such as
||	/usr/NeWS/smi/globes/*.im1) (only 1bit per pixel images)
||	or standard SGI image file format (8 or 24 bit, b/w or color).
||  depending on which of SGI_IMAGE or RASTERIMAGE is defined.
||	(One of the two must be define on compile line, see the
||	Makefile.)
||
|| Usage:	iisc	[ -d <path> ]
||		-d	path to device
||			Otherwise, defaults to /dev/scsi/iisc.
||
|| Copyright 1988, 1989, by
||   Gene Dronek (Vulcan Laboratory) and
||   Rich Morin  (Canta Forda Computer Laboratory).
|| Freely redistributable as long as this notice is preserved.
*/
#ident "iisc.c: $Revision: 1.1 $"

/* must have one and only one of the image file formats defined */
#if !defined(SGI_IMAGE) && !defined(RASTERIMAGE)
OOPS;
one of SGI_IMAGE / RASTERIMAGE must be defined...
#endif
#if defined(SGI_IMAGE) && defined(RASTERIMAGE)
OOPS;
only one of SGI_IMAGE / RASTERIMAGE may be defined...
#endif


#ifdef SGI_IMAGE
double zoom = 1.0, atof(char *);
#endif

int iheight;	/* number of lines in image */
int iwidth;	/* number of pixels across */
int devflags;	/* flags supported by driver */
#define DSDBG(x) {if (dsdebug) {x;}}

#define MAXLIN (310)
#define MAXPKT (20)

#include <fcntl.h>
#include <stdio.h>

/*	devscsi include files 	AFU!!! */
#ifdef aux
#ifdef hack
# include "dslib.h"
#else
# include <ds/dslib.h>
#endif hack
# include <sys/vio.h>		/* for SDC bits */
# include <sys/scsireq.h>
#else
# include "dslib.h"
#endif aux

/*	iisc include files	*/
#include "iisc.h"

extern uchar_t *malloc();

char *devpath = "/dev/scsi/iisc";	/* name of device to open */

int
bytes,	/* bytes to read per line of image */
line;	/* which line of image are we on */

struct dsreq *dsp;

main(argc, argv, arge)
int argc;
uchar_t **argv, **arge;
{
	int indx;

	indx = get_args(argc, argv);

	init_dev();				      /* initialize the printer	*/

	if(indx == argc)
		printimg();
	else for(; indx < argc; indx++) {
		if(freopen(argv[indx], "r", stdin) == NULL) {
			fprintf(stderr, "can't open image file ");
			perror(argv[indx]);
			continue;
		}
		printimg();
	}
	exit(0);
	/*NOTREACHED*/
}

printimg()
{
	if (get_hdr()) {
		if (iheight  > IISC_HMAX)
			ds_zot("iisc: image too tall");
		if (iheight  < IISC_HMIN)
			ds_zot("iisc: image too short");
		if (iwidth   > IISC_WMAX)
			ds_zot("iisc: image too wide");
		if (iwidth   < IISC_WMIN)
			ds_zot("iisc: image too narrow");
		init_page();			         /* initialize the page	*/
		copy_page();			        /* copy page to printer	*/
		if (print0a(dsp, 0x80) != 0)	 	/* actually print page	*/
			ds_zot("iisc: print0a failure");
	}
	endimg();
}

/*
|| copy_page - copy page
*/

copy_page()
{
	int brx, bry, lin_cnt, lin_sav, n_read, out_cnt;

	DSDBG( fprintf(stderr,">>> copy_page\n"); )

	brx = bytes*8;				      /* bottom right X	*/
	bry = iheight -1;			      /* bottom right Y	*/

	if (clearbits06(dsp, 0, 0, brx, bry) != 0)	  /* clear image buffer	*/
		ds_zot("iisc: clearbits06 failure");

	out_cnt = 10;					 /* pre-allocate header	*/

	lin_sav = -1;
	for (lin_cnt = line = 0; line < iheight; line++) {
#ifdef RASTERIMAGE
		n_read = get_ber(&dsr_data[out_cnt], bytes, stdin);
#endif RASTERIMAGE
#ifdef SGI_IMAGE
		n_read = get_line(&dsr_data[out_cnt], bytes);
#endif SGI_IMAGE

		if (n_read <  0) {
			perror("iisc");
			ds_zot("iisc: error reading data line");
		}
		if (n_read == 0)		ds_zot("iisc: EOF reading data line");
		if (n_read != bytes)	ds_zot("iisc: data line too short");

		lin_cnt++;
		out_cnt += bytes;

#ifdef DEBUG3
		int j;
		fprintf(stderr, "out_cnt:		%d\n", out_cnt);
		fprintf(stderr, "lin_sav:		%d\n", lin_sav);
		fprintf(stderr, "line:		%d\n", line);
		fprintf(stderr, "brx:		%d\n", brx);

		fprintf(stderr, "\n");
		for (j=0; j<out_cnt; j++)
			fprintf(stderr, "%02x", dsr_data[j]);
		fprintf(stderr, "\n");
#endif DEBUG3

		if (drawbits05(dsp, dsr_data, out_cnt,
		    0, lin_sav+1, brx, line+1, 0) != 0) {
			/*OLSON*/printf("at line %d, lin_sav %d, brx %d, out_cnt %d, bytes\n",
				line+1, lin_sav+1, brx, out_cnt, bytes);
			ds_zot("iisc: drawbits05 failure, call 1");
		}

		lin_sav = line;
		lin_cnt = 0;
		out_cnt = 10;				 /* pre-allocate header	*/
	}
	if (lin_cnt != 0)
		/* not line+1 because already incremented by for loop */
		if (drawbits05(dsp, dsr_data, out_cnt,
		    0, lin_sav+1, brx, line, 0) != 0)
			ds_zot("iisc: drawbits05 failure, call 2");
}


/*
|| get_args - get command line arguments, etc.  { dummied }
*/

get_args(argc,argv)
int argc;
char **argv;
{
	int c;
	extern int optind, opterr;
	extern char *optarg;

	opterr = 0;	/* handle errors ourselves */

#ifdef RASTERIMAGE
	while((c=getopt(argc, argv, "d:")) != -1) {
		switch(c) {
		case 'd':
			devpath = optarg;
			break;
		default:
			fprintf(stderr, "Usage: %s: [-d <path>] ", *argv);
			exit(1);
		}
	}
#endif

#ifdef SGI_IMAGE
	while((c=getopt(argc, argv, "d:z:")) != -1) {
		switch(c) {
		case 'd':
			devpath = optarg;
			break;
		case 'z':
			zoom = atof(optarg);
			if(zoom <= 0.) {
				fprintf(stderr, "zoom factor must be > 0.\n");
				zoom = 1.0;
			}
			break;
		default:
			fprintf(stderr, "Usage: %s [-d <path>] [-z zoomfactor] ", *argv);
			exit(1);
		}
	}
#endif
	return optind;
}



/*
|| init_dev - initialize the printer
*/

init_dev()
{
	int i;

	uchar_t
	    str1[25],
	    str2[5];
	struct dsconf conf;

	static uchar_t apl[] = "APPLE   PERSONAL LASER  ";

	DSDBG( fprintf(stderr,">>> init_dev\n") )
	    /* Open specified device	*/
	if ((dsp = dsopen(devpath,O_RDONLY)) == NULL)
		ds_zot("iisc: unable to open device");

	if(ioctl(getfd(dsp), DS_CONF, &conf) == -1) {
		perror("couldn't determine flags supported by driver");
		devflags = 0;
	}
	else
		devflags = conf.dsc_flags;
	dsreqflags = devflags & DSRQ_SENSE;	/* always do a sense if supported */

#if defined(SDC_RDPOLL)	/* aux */
	{
	int flags;
	flags = SDC_RDPOLL|SDC_WRPOLL;
	if((devflags & flags) == flags &&
		ioctl(getfd(dsp), DS_SET, &flags) == -1)
		ds_zot("iisc: cannot set SDC flags");
	}
#endif

	/* clear check cond.	*/
	if (testunitready00(dsp) != 0)
		if (testunitready00(dsp) != 0)
			ds_zot("iisc: printer not ready");
	/* Check device type	*/
	if (inquiry12(dsp, dsr_data, sizeof dsr_data, 0) != 0)
		ds_zot("iisc: inquiry12 failure");

	DSDBG( fprintf(stderr,"inqu_retp=0x%x %x %x %x\n", inqu_retp,
	    dsr_data[0],dsr_data[1],dsr_data[2]) )

	    for (i=0; i<24; i++)
		str1[i] = inqu_retp->id_str[i];
	str1[i] = '\0';

	for (i=0; i<4; i++)
		str2[i] = inqu_retp->id_str[i+24];
	str2[i] = '\0';

	DSDBG( fprintf(stderr,   "PDT:		%02x\n",	inqu_retp->pdt);
	    fprintf(stderr,   "TAP:		%02x\n",	inqu_retp->tap);
	    fprintf(stderr,   "T/R/P:\t	%02x\n",	inqu_retp->t_r_p);
	    fprintf(stderr,   "ID:		<%s>\n",	str1);
	    fprintf(stderr,   "VER:		<%s>\n",	str2); )

	    if (inqu_retp->pdt            != 0x02)  ds_zot("iisc: device not printer");
	if ((inqu_retp->t_r_p & 0x70) == 0x00)  ds_zot("iisc: no RAM in printer");
	if (strcmp(str1, apl)         != 0)	  ds_zot("iisc: ID mismatch");
}


/*
|| init_page - initialize the page
*/

init_page()
{
	int lmargin, tmargin;

	DSDBG( fprintf(stderr,">>> init_page\n") )
	    /* Send parameters	*/

	tmargin = (3300 - iheight) / 2;		/* Assume 11.0"	*/
	tmargin = (3450 - iheight) / 2;	    /* (?) Assume 11.5"	*/
	if (tmargin < 0x76) tmargin = 0x76;

	lmargin = (2550 - iwidth) / 16;		 /* Assume 8.5"	*/
	lmargin = (2700 - iwidth) / 16;	     /* (?) Assume 9.0"	*/
	if (lmargin < 0x12) lmargin = 0x12;

	DSDBG( fprintf(stderr,"top  margin: %d %x \n", tmargin, tmargin);
	    fprintf(stderr,"left margin: %d %x \n", lmargin, lmargin); )

	    if (format04(dsp, 0, tmargin, lmargin) != 0)
		ds_zot("iisc: format04 failure - call 1");

	DSDBG( fprintf(stderr,"bytes:  %d %x \n", bytes, bytes);
	    fprintf(stderr,"height: %d %x \n", iheight, iheight); )

	    if (format04(dsp, 2, bytes, iheight) != 0)
		ds_zot("iisc: format04 failure - call 2");
}
