/*
 * dmplay.c: Plays back an SGI Movie JPEG file.
 *
 * 
 * Silicon Graphics Inc., June 1994
 */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <unistd.h>	/* geteuid(), getuid(), etc. */

/*
 * for schedctl
 */
#include <limits.h>
#include <sys/types.h>
#include <sys/prctl.h>
#include <sys/schedctl.h>


/*
 *X / gl include files
 *
 */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Sgm/GlxMDraw.h>

/*
 *dmdev include files
 */
#include <dmedia/cl.h>
#include <dmedia/cl_cosmo.h>
#include <movie.h>
#include <vl/vl.h>
#include <audio.h>
#include <audiofile.h>

#include "dmplay.h"

/*
 *Global definitions
 */
_Movie movie;
_Image image;
_Codec  codec;
_Audio  audio;
_PlayState playstate;
_Video  video;

/*
 *Prototype definitions
 */
extern void parseargs (int argc, char *argv[], char *myname);
extern void mvInit ();
extern void alInit ();
extern void vlInit ();
extern void clInit ();
extern void XInit (int argc, char *argv[]);

_Options options; /* initialized by parseargs() */

static void
initglobals( void )
{
    audio.isTrack = 0;

    playstate.advanceAudio = 1;
    playstate.advanceVideo = 1;
    playstate.playMode = PLAY_CONT;

    codec.singleFrame = 0;
    codec.OriginalFormat = CL_RGBX;
    codec.engine = CL_JPEG_COSMO;

    image.isTrack   = 0;
    image.interlacing = DM_IMAGE_INTERLACED_ODD;
    image.orientation = DM_TOP_TO_BOTTOM;
    image.width           = 640;
    image.height          = 244;
    image.display = VIDEO_2_FIELD;
    image.numberFrames = -1;

    video.dataActive = 0;
    video.dataFrozen = 0;
    video.timingActive = 0;
}

static void
initPlayState( void )
{
    if ((audio.isTrack) && (options.playAudioIfPresent))
        playstate.audioActive = 1;
    else 
        playstate.audioActive = 0;

    playstate.playMode = options.initialPlayMode;
    playstate.autoStop = options.autostop;

    if (options.initialLoopMode == LOOP_NOTSPEC) 
    {
        if (movie.loopMode == MV_LOOP_CONTINUOUSLY)
            playstate.loopMode = LOOP_REPEAT;
        else  /* XXX MV_LOOP_SWINGING not supported */
            playstate.loopMode = LOOP_NONE;
    }
    else 
    {
        playstate.loopMode = options.initialLoopMode;
    }
}

void
setscheduling()
{
    /*
     * set non-degrading priority
     * use high priority if running as root
     */
    /* 
     * swap permissions so that we become root for a moment. 
     * setreuid() should let us set real uid to effective uid = root
     * if the program is setuid root 
     * 
     */
    setreuid(geteuid(), getuid());
    setregid(getegid(), getgid());

    if (schedctl(NDPRI, 0, NDPHIMIN)< 0)
    {
         fprintf(stderr, "%s: run as root to enable real time scheduling\n",
             options.myname);
    }

    /* swap permissions back, now we're just "joe user" */
    setreuid(geteuid(), getuid());
    setregid(getegid(), getgid());

}


main (int argc, char *argv[]) 
{
    char *myname = argv[0];

    setreuid(geteuid(), getuid());
    setregid(getegid(), getgid());

    initglobals();
    Xinit(&argc, argv);
    parseargs(argc, argv, myname);
    mvInit ();
    alInit ();
    vlInit ();
    /* 
     * set up initial play state variables based on information
     * from command-line options and movie file header
     */
    initPlayState(); 
    Xgo();
    exit(0);
}
