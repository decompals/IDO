/*
 *	svtomovie.c -- make a movie from IndigoVideo board input
 *
 * svtomovie captures captures video frames, converts them to RGB data,
 * and then writes them to a file as an SGI movie file playable by
 * movieplayer and editable by moviemaker. Audio can also be added.
 *
 * The default frame rate is 15 frames per second with mono audio.
 *   NOTE: -b will be 30 frames per second only (not changeable)
 * The default picture size is 320 (w) x 240 (h).
 * Use vpanel and apanel to choose the sources, rates, levels, etc.
 *
 * Usage: svtomovie [options] filename
 *
 * [options] include:
 *	   -a		turn audio off (default = audio on)
 *	   -b		use burst mode (default = continuous)
 *			   note: will be short and silent
 *	   -d		turn diagnostic message on (default = off)
 *	   -f framerate	frames per second (default = 15)
 *			   NOTE: -b will be 30 only (not changeable)
 *	   -m 		turn audio monitoring on (default = off)
 *         -n numframes	number of frames to grab (default = 100)
 *         -s		stereo audio (default = mono)
 *	   -w width	video width in pixels (default = 320)
 * 
 */

#ident "$Revision: 1.20 $"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <strings.h>
#include <fcntl.h>
#include <getopt.h>
#include <audio.h>
#include <svideo.h>
#include <gl/gl.h>
#include "Movie.h"

static short audiobuf[96000];		/* maximum audio buffer size */


#define OPT_STR "abdf:mn:sw:"

void 
usage(char *name)
{
    printf("Usage: %s [options] filename\n", name);
    printf("[options] include:\n");
    printf("\t-a\t\tmovie audio off (default = movie audio on)\n");
    printf("\t-b\t\tuse burst mode (default = continuous)\n");
    printf("\t-d\t\tdiagnostic message off (default = on)\n");
    printf("\t-f framerate\tframes per second (default = 15)\n");
    printf("\t-m\t\taudio monitoring on (default = off)\n");
    printf("\t-n numframes\tnumber frames to grab (default = 100)\n");
    printf("\t-s\t\tstereo audio (default = mono)\n");
    printf("\t-w width\tvideo width in pixels (default = 320)\n");
}

int main(int argc, char *argv[])
{
    MovieHeader mh;		/* from Movie.h, movie structure */
    DirectoryEntry *direntry;	/* from Movie.h, movie structure */
    ALconfig aconfig;		/* for configuring audio port */
    ALport ainport;		/* the input audio port */
    svCaptureInfo svci;		/* for video capture initialization */
    SVhandle V;			/* the input Video port handle */
    long win;			/* the gl window id for binding to video */
    long pvi, pvbuf[16];		/* for configuring input ports */
    int asamps_per_vframe;	/* audio samples per video frame */
    int audio = 1;		/* audio flag, 0=no audio, 1= audio */
    int audiomonitor = 0;	/* audio monitoring (1=on, 0=off) */
    int audiosize;		/* size in bytes of audio per frame */
    int burstmode = 0;		/* 1=use burst mode, 0=use continuous mode */
    int diagmess = 0;		/* 1=diagnostic message on, 0=off */
    int errflg;			/* to check for command line errors */
    int frame;			/* frame index for loops */
    int fieldID;		/* returned from capture and used in burst */
    int *fieldIDarray;		/* used to collect fieldID's for movie */
    int i, j, k;		/* temp loop indices */
    int location;		/* file pointer for writing data */
    int outfd;			/* the movie's file handle */
    int videosize;		/* size in bytes of video per frame */
    unsigned char *invertedframe; /* video data inverted */
    unsigned char *videoframe;	/* raw video data input */
    char *bitvec;		/* bit vector for burst mode */
    char buf[256];		/* temp buf for sprintf's */
    
/* Initialize movie header items that may be changed by command line options */
    mh.frameWidth = 320;		/* video width in pixels */
    mh.frameHeight = 240;		/* video height in pixels */
    mh.framesPerSec = 15;		/* frames per second */
    mh.frameCount = 100;		/* number of frames in movie */
    mh.audioNumChannels = AL_MONO; 	/* number of audio channels */

/* Handle command line options */
    errflg = 0;
    optind = 1;
    while ((j = getopt(argc, argv, OPT_STR)) != -1) {
	switch (j)	{    
	    case 'a': audio = 0;
	              break;
	    case 'b': burstmode = 1;
	              break;
	    case 'd': diagmess = 1;
	              break;
	    case 'f': mh.framesPerSec = atoi(optarg);
	              break;
	    case 'm': audiomonitor = 1;
	              break;
	    case 'n': mh.frameCount = atoi(optarg);
	              break;
	    case 's': mh.audioNumChannels = AL_STEREO;
	              break;
	    case 'w': mh.frameWidth = atoi(optarg);
		      mh.frameHeight = mh.frameWidth * 6 / 8;
	              break;
	    default:  errflg++;
	}
    }
    if (errflg || optind >= argc) {
	usage(argv[0]);
	exit(1);
    }

/* Open output movie file */
    if ((outfd = open(argv[optind], O_CREAT|O_RDWR|O_TRUNC, 0644)) < 0) {
	sprintf(buf,"%s: couldn't open file %s",argv[0],argv[optind]);
	perror(buf);
	exit(1);
    }
    if (burstmode) {
	audio = 0;		/* SILENT */
	mh.framesPerSec = 30;		/* full frame rate */
    }

/* Initialize the movie header */
    mh.movie_id = MOVIE_ID;
    mh.version = MOVIE_VERSION_2;
    mh.movieType = (audio) ? SOUND : SILENT;
    mh.showType = MOVIE;
    mh.image_format = IMAGE_FORMAT_8RGB;
    mh.imageXsize = mh.frameWidth;
    mh.imageYsize = mh.frameHeight;
    mh.imageCsize = 4;
    mh.imageDatatype = 2;
    mh.loop_mode = LOOP_CONTINUOUSLY;
    mh.audioSampleRate = AL_RATE_22050;
    mh.audioSampleSize = AL_SAMPLE_16;
    mh.audioSampleFormat = 401;
    mh.audioCompression = 0;
    strcpy(mh.title, "This is the first movie");
    strcpy(mh.comments, "here we put the comments");
    strcpy(mh.field_extensions, "field_extensions is the next area");
    strcpy(&mh.field_extensions[248], "the end");

/* Initialize size constants */
    asamps_per_vframe = mh.audioNumChannels *
	(mh.audioSampleRate / mh.framesPerSec);
    audiosize = (audio) ? asamps_per_vframe * mh.audioSampleSize : 0;
    videosize = mh.frameWidth * mh.frameHeight;

/* Allocate the data areas */
    invertedframe= malloc(videosize);
/* videoframe is allocated later, after videosize is really determined */
    direntry = malloc(mh.frameCount*sizeof(DirectoryEntry));
    fieldIDarray = malloc(mh.frameCount*sizeof(int));
    bitvec = malloc(((mh.frameCount + 7)/8) * 2); /* 2 bits per frame */
    if ((invertedframe == NULL) ||
	(direntry == NULL) || (fieldIDarray == NULL)  || (bitvec == NULL)) {
	sprintf(buf,"%s: Ran out of memory in malloc",argv[0]);
	perror(buf);
	exit(1);
    }

/* Since the sizes are all constant, create the directory entries.
 * NOTE: this may change if user changes rate with apanel, so this
 * is recomputed and rewritten after capture is complete.
 */
    location = sizeof(MovieHeader) + mh.frameCount*sizeof(DirectoryEntry);
    for (frame = 0; frame < mh.frameCount; frame++) {
	direntry[frame].location = location;
	direntry[frame].asize = audiosize;
	direntry[frame].vsize = videosize;
	location += audiosize + videosize;
    }

/* Write out movie header and directory */
    if ((write(outfd, &mh, sizeof(MovieHeader))) != sizeof(MovieHeader)) {
	sprintf(buf,"%s: Can't write file %s",argv[0],argv[optind]);
	perror(buf);
	exit(1);
    }
    if (write(outfd, direntry, mh.frameCount*sizeof(DirectoryEntry))
	    < mh.frameCount*sizeof(DirectoryEntry)) {
	sprintf(buf,"%s: can't write file %s",argv[0],argv[optind]);
	perror(buf);
	exit(1);
    }

/* Open the graphics window for displaying the video */
    prefposition(300,300+mh.frameWidth-1,100,100+mh.frameHeight-1);
    foreground();
    win = winopen("svtomovie");

/* Open and initialize the IndigoVideo board */
    if ((V = svOpenVideo()) == NULL) {
	svPerror("svOpenVideo");
	exit(1);
    }
    svSetSize(V, mh.frameWidth, mh.frameHeight);

/* Associate video with window */
    if (svBindGLWindow(V, win, SV_IN_REPLACE) < 0) {
	svPerror("svBindGLWindow");
	exit(1);
    }

/* Set audio sample rate to default */
    pvi = 0;
    pvbuf[pvi++] = AL_INPUT_RATE;
    pvbuf[pvi++] = mh.audioSampleRate;
    pvbuf[pvi++] = AL_OUTPUT_RATE;
    pvbuf[pvi++] = mh.audioSampleRate;
    if (audiomonitor) {		/* if monitoring, turn it on */
	pvbuf[pvi++]=AL_MONITOR_CTL;
	pvbuf[pvi++]=AL_MONITOR_ON;
    }
    ALsetparams(AL_DEFAULT_DEVICE, pvbuf, pvi);
    fsync(outfd);

/* Prompt user and wait until enter */
    printf("%s: Press <ENTER> when you are ready to collect video:",argv[0]);
    gets(buf);

/* Get final audio sample rate */
    pvbuf[0] = AL_INPUT_RATE;
    pvbuf[1] = 0;
    ALgetparams(AL_DEFAULT_DEVICE, pvbuf, 2);
    mh.audioSampleRate = pvbuf[1] ;

/* Initialize size constants */
    asamps_per_vframe = mh.audioNumChannels *
	(mh.audioSampleRate / mh.framesPerSec);
    audiosize = (audio) ? asamps_per_vframe * mh.audioSampleSize : 0;

/* Open and initialize the audio device */
    aconfig = ALnewconfig();
    ALsetwidth(aconfig, mh.audioSampleSize);
    ALsetchannels(aconfig, mh.audioNumChannels);
    ALsetqueuesize(aconfig, asamps_per_vframe+1);
    if ((ainport = ALopenport("x", "r", aconfig)) == 0) {
	sprintf(buf,"%s: Can't open audio port",argv[0]);
	perror(buf);
	exit(1);
    }

/* Initialize the video capture to either burst or continuous mode */
    if (burstmode) {
	/* Burst mode fills a large buffer with as many frames as fit */
	svci.format = SV_RGB8_FRAMES;
	svci.width = mh.frameWidth;
	svci.height = mh.frameHeight;
	svci.size = mh.frameCount;
	svci.samplingrate = 0;
	if (svQueryCaptureBufferSize(V, &svci, &i) < 0) {
	    svPerror("svQueryCaptureBufferSize");
	    exit(1);
	}
	videoframe = malloc(i);
        if (!videoframe) {
	    sprintf(buf,"%s: Ran out of memory in malloc",argv[0]);
	    perror(buf);
	    exit(1);
	}
	bzero(bitvec,((mh.frameCount + 7)/8) * 2); /* 2 bits per frame */
	if (svCaptureBurst(V, &svci, videoframe, bitvec) < 0) {
	    svPerror("svCaptureBurst");
	    exit(1);
	}
    } else {
	/* Continuous mode simply starts the capture, later the frames will
		be read */
	svci.format = SV_RGB8_FRAMES;
	svci.width = mh.frameWidth;
	svci.height = mh.frameHeight;
	svci.size = 3;
	svci.samplingrate = 30 / mh.framesPerSec;
	if (svQueryCaptureBufferSize(V, &svci, &i) < 0) {
	    svPerror("svQueryCaptureBufferSize");
	    exit(1);
	}
	if (svInitContinuousCapture(V, &svci) < 0) {
	    svPerror("svInitContinuousCapture");
	    exit(1);
	}
    }

/* Check to see if the library changed the width or height */
    if (svci.width != mh.frameWidth) {
	printf("%s: Width changed from %d to %d\n",
		argv[0], mh.frameWidth,svci.width);
	mh.frameWidth = svci.width;
	mh.imageXsize = svci.width;
    }
    if (svci.height != mh.frameHeight) {
	printf("%s: Height changed from %d to %d\n",
		argv[0], mh.frameHeight,svci.height);
	mh.frameHeight = svci.height;
	mh.imageYsize = svci.height;
    }
    videosize = mh.frameWidth * mh.frameHeight;
    if (!burstmode) {
	videoframe = malloc(videosize);
        if (!videoframe) {
	    sprintf(buf,"%s: Ran out of memory in malloc",argv[0]);
	    perror(buf);
	    exit(1);
	}
    }

/* Burst mode 
 * For every frame, get a video frame from the video
 * buffer and write it to the movie file.
 * The tricky part is dealing with the bit vector which
 * indicates what fields have been read (even or odd).
 * In the perfect world, it would always be even then odd.
 * But in real life, the board drops a field every 200 fields.
 * So, to handle that case, the loop will check for the even/odd
 * bits, and duplicate a field that has no matching even/odd.
 * So, if this happens, a print states what frame has the duplicated
 * field.
 */
    fsync(outfd);
    if (burstmode) {
	fieldID = 0;
	for (frame = 0; frame < mh.frameCount; frame++) {
	    /* Write the field of video data out to movie file */
	    if (write(outfd,videoframe+(fieldID*(videosize/2)),
				 videosize/2)<videosize/2) {
		printf("%s: bad write even field %d in movie %s\n",
			argv[0],frame+1,argv[optind]);
		exit(1);
	    }
	    /* bump buffer pointer if just wrote odd field */
	    if (SV_GET_FIELD(bitvec,fieldID) == SV_ODD_FIELD) {
		fieldID++;
	    } else if (diagmess) {
		printf("%s: Duplicating odd field %d in frame %d\n",
				argv[0],fieldID,frame+1);
	    }
	    /* Write the field of video data out to movie file */
	    if (write(outfd,videoframe+(fieldID*(videosize/2)),
				 videosize/2)<videosize/2) {
		printf("%s: bad write even field %d in movie %s\n",
			argv[0],frame+1,argv[optind]);
		exit(1);
	    }
	    /* bump buffer pointer if just wrote even field */
	    if (SV_GET_FIELD(bitvec,fieldID) == SV_EVEN_FIELD) {
		fieldID++;
	    } else if (diagmess) {
		printf("%s: Duplicating even field %d in frame %d\n",
				argv[0],fieldID,frame+1);
	    }
	}
	if (diagmess && (2*mh.frameCount-fieldID) > 0)
	    printf("%s: Skipped %d fields in burst mode\n",
		argv[0],2*mh.frameCount-fieldID);
    } else {
/* Continuous mode 
 * For every frame, read a piece of audio from the audio hardware,
 * and write it to the movie file. Then, using capture,
 * get a video frame from the video board and write it to the
 * movie file.
 */
	for (frame = 0; frame < mh.frameCount; frame++) {
	    /* Get captured video data */
	    do {
		if (svGetCaptureData(V, (void **)&videoframe, &fieldID) < 0) {
		    svPerror("svGetCaptureData");
		    svEndContinuousCapture(V);
		    exit(1);
		}
		if (videoframe == NULL)  /* nothing available yet */
		    sginap(1);
	    } while (videoframe == NULL);
	    fieldIDarray[frame] = fieldID;

	    /* Get and write audio data to movie file */
	    if (audio) {
		ALreadsamps(ainport, audiobuf, asamps_per_vframe);
		if ((write(outfd, audiobuf, audiosize)) < audiosize) {
		    printf("%s: bad audio write at frame %d in file %s\n",
				argv[0],frame+1,argv[optind]);
		    svEndContinuousCapture(V);
		    exit(1);
		}
	    }

	    /* Write video data out to movie file */
	    if (write(outfd, videoframe, videosize) < videosize) {
		printf("%s: bad write frame %d in movie %s\n",
			argv[0],frame+1,argv[optind]);
		svEndContinuousCapture(V);
		exit(1);
	    }
	    /* Release capture area */
	    svUnlockCaptureData(V, videoframe);
	}        
    }

/* Close everything but file */
    if (audiomonitor) {	/* if monitoring, turn it off */
	pvi = 0;
	pvbuf[pvi++]=AL_MONITOR_CTL;
	pvbuf[pvi++]=AL_MONITOR_OFF;
	ALsetparams(AL_DEFAULT_DEVICE, pvbuf, pvi);
    }
    ALcloseport(ainport);
    svEndContinuousCapture(V);
    winclose(win);
    fsync(outfd);

/* In case something was changed, recreate the directory entries */
    location = sizeof(MovieHeader) + mh.frameCount*sizeof(DirectoryEntry);
    for (frame = 0; frame < mh.frameCount; frame++) {
	direntry[frame].location = location;
	direntry[frame].asize = audiosize;
	direntry[frame].vsize = videosize;
	location += audiosize + videosize;
    }

/* Seek back to beginning of file */
    if ((j = lseek(outfd, 0, SEEK_SET)) != 0) {
	printf("%s: lseek failed %s, %d not 0\n",argv[0],argv[optind],j);
	exit(1);
    }

/* Rewrite movie header and directory */
    if ((write(outfd, &mh, sizeof(MovieHeader))) != sizeof(MovieHeader)) {
	sprintf(buf,"%s: Can't write file %s",argv[0],argv[optind]);
	perror(buf);
	exit(1);
    }
    if (write(outfd, direntry, mh.frameCount*sizeof(DirectoryEntry))
	    < mh.frameCount*sizeof(DirectoryEntry)) {
	sprintf(buf,"%s: can't write file %s",argv[0],argv[optind]);
	perror(buf);
	exit(1);
    }

/* Reread video file to convert to RGB style pictures */
    for (frame = 0; frame < mh.frameCount; frame++) {
	location = direntry[frame].location;
	/* Seek to next video frame */
	if ((j = lseek(outfd, location + audiosize, SEEK_SET)) !=
		location + audiosize) {
	    printf("%s: relseek failed frame %d movie %s at %d not %d\n",
		 argv[0],frame+1,argv[optind],j,sizeof(MovieHeader));
	    exit(1);
	}

	/* Read video data from movie file */
	if (read(outfd, videoframe, videosize) < videosize) {
	    printf("%s: bad read frame %d in movie %s\n",
		    argv[0],frame+1,argv[optind]);
	    exit(1);
	}

	/* Invert and interleave data to create RGB picture */
	svInterleaveFields(TRUE, videoframe, invertedframe,
				 mh.frameWidth, mh.frameHeight);

	/* Seek back to video frame */
	if ((j = lseek(outfd, location + audiosize, SEEK_SET)) !=
		location + audiosize) {
	    printf("%s: relseek failed frame %d movie %s at %d not %d\n",
		 argv[0],frame+1,argv[optind],j,sizeof(MovieHeader));
	    exit(1);
	}

	/* Rewrite RGB data out to movie file */
	if (write(outfd, invertedframe, videosize) < videosize) {
	    printf("%s: bad rewrite frame %d in movie %s\n",
		    argv[0],frame+1,argv[optind]);
	    exit(1);
	}

	if (burstmode == 0 && diagmess) {  /* check in continuous mode only */
	    /* Check field ID's for missing frames */
	    if (frame > 0) {
		if (fieldIDarray[frame] != (fieldIDarray[frame-1] +
			    (svci.samplingrate * 2)))
		    printf("%s: Missing frame %d, field %d should be %d\n",
			argv[0],frame+1,fieldIDarray[frame],
			fieldIDarray[frame-1] + (svci.samplingrate * 2));
	    }
	}
    }
    close(outfd);
}
