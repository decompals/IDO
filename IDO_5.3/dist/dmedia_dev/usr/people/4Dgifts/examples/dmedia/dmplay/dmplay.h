#ifndef __INC_DMPLAY_H__
#define __INC_DMPLAY_H__ 1

/*
 *dmplay.h: header file for dmplay.h
 *
 *
 *Original version by Rajal Shah. (5/24/94)
 *
 */

#include <stdio.h>

/*
 *X / gl include files
 */
#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Sgm/GlxMDraw.h>


#include <sys/types.h>
#include <sys/resource.h>
#include <sys/prctl.h>
#include <sys/signal.h>

/*
 * dmedia headers
 */
#include <dmedia/dm_image.h>
#include <dmedia/cl.h>
#include <dmedia/cl_cosmo.h>
#include <vl/vl.h>
#include <audio.h>
#include <movie.h>




#define GRAPHICS               10
#define VIDEO_1_FIELD          11
#define VIDEO_2_FIELD          12

#define PLAY_STEP 0
#define PLAY_CONT 1

#define LOOP_NOTSPEC -1 /* not specified */
#define LOOP_NONE 0     /* no looping */
#define LOOP_REPEAT 1   /* repeat looping */


typedef struct {
    char *filename;
    MVloopmode loopMode;
    MVid mv;           /*Movie*/
    MVid imgTrack;     /*ImgTrack*/
    MVid audTrack;     /*AudTrack*/
} _Movie;

typedef struct {
    int isTrack;  			/*is there a video ('image') track?*/
    int height;
    int width;
    DMinterlacing interlacing;
    DMorientation orientation;
    int display;
    int numberFrames;
    double frameRate;
} _Image;

typedef struct {
    int loopMode; /* LOOP_NONE or LOOP_REPEAT */
    int audioActive;
    int playMode; /* PLAY_STEP or PLAY_CONT */
    int advanceAudio; 
    int advanceVideo;
    int autoStop; /* 0 if wait for user input, 1 if stop automatically */
} _PlayState;

typedef struct {
    CLcompressorHdl Hdl;
    int OriginalFormat;
    int engine;
    int singleFrame;
} _Codec;

typedef struct {
    int        isTrack;                   /* is there an audio track ?*/
    ALport     outPort;                   /* audio output port */
    ALconfig   config;                    /* audio output port configuration */
    int        frameCount;                /* audio track length */
    int        sampleWidth;               /* audio sample width, in bits */
    int	       frameSize;		  /* bytes per audio frame */
    int        blockSize;                 /* ?? */
    int        channelCount;              /* 1=mono, 2=stereo, etc */
    int        queueSize;                 /* audio output port queue */
    double     frameRate;                 /* audio sample rate */
} _Audio;

typedef struct {
    VLServer svr;
    VLPath path;		/* video data path from Cosmo */
    VLNode src;			/* from Cosmo */
    VLNode drn;			/* screen out */
    VLNode voutdrn;		/* video out */
    VLNode dataTiming;		/* timing portion of data path */
    VLNode devNode;
    int    dataActive;		/* vlBeginTransfer on data path? */
    int    dataFrozen;		/* has VL_FREEZE been called? */

    VLNode timingSrc;		/* Timing path */
    VLNode timingDrn;
    VLPath timingPath;
    int    timingActive;	/* is the timing path currently active? */
} _Video;


/* 
 * Command line options
 */

typedef struct {
    char *myname;                 /* name of this program */
    int use_default_ports;        /* if no -p options specified by user */
    int display_port_specified;   /* if -p graphics or -p video flag seen */
    int audio_port_specified ;    /* if -p audio flag seen */
    int playAudioIfPresent;       /* set when -p audio flag seen */
    int verbose;		  /* -v flag */
    int initialLoopMode;          /* LOOP_NOTSPEC, LOOP_NONE or LOOP_REPEAT */
    int autostop;                 /* 1 if -E auto, 0 if -E key */
    int initialPlayMode;          /* PLAY_CONT if -P cont,PLAY_STEP if -P step*/
    char *ifilename;              /* input file name */
    char *image_engine;	          /* image track decompression engine */
    int image_port_type;          /* image track display: graphics or video*/
    char *vid_device;	          /* video device name: 'ev1' for now */
    int  vid_portnum;             /* video device port num: not used yet */
} _Options;


extern _Codec codec;
extern _PlayState playstate;
extern _Image image;
extern _Movie movie;
extern _Audio audio;
extern _Video video;
extern _Options options;
extern XtAppContext appContext;

extern void setscheduling( void );
extern void stopAllThreads( void );
extern void singleFrameDecompress( void );
extern void streamDecompress( void );
extern void Xinit( int* argcp, char* argv[] );
extern void clInit( int );
extern void mvInit( void );
extern void alInit( void );
extern void vlInit( void );
extern void Xgo( void );
extern void deinterlaceImage( void* from, void* to );

/********
*
* ERROR - Report an error and exit the program.
*
* This example program has a very simple error handling mechanism.
* When something goes wrong in a call that was not expected to fail, 
* an error message is printed, all executing threads are stopped, and
* the program quits.
*
********/

#define ERROR(message)							      \
	{								      \
	    fprintf( stderr, "%s: %s\n", options.myname, message );	      \
	    fprintf( stderr, "%s, line %d\n", __FILE__, __LINE__); 	      \
	    stopAllThreads();						      \
	}

#endif /* __INC_DMPLAY_H__ */
