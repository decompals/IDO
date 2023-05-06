/*
 * Files:       moviescreen.c, moviescreenArgs.c, moviescreenWin.c,
 *              moviescreenGl.c, moviescreenEvents.c, glxhelper.c,
 *              moviescreenArgs.h, moviescreenWin.h, moviescreenGl.h,
 *              moviescreenEvents.h, glxhelper.h
 *
 * Usage:       moviescreen [-k] [-f] [-s] [-v vol] [-z zoom] [-l loopmode]
 *                         moviefile [moviefile2...]
 *
 * Description: Moviescreen is a screen saver which plays movies. It will
 *              not save screens by itself.  It is designed to run 
 *              under haven(1), a wrapper for IRIS GL-based screensavers
 *              which is available on SGI systems. 
 * 
 * Functions:   SGI Movie Library functions used in this program. One or more
 *		of these functions are used in the other *.c files listed 
 *		after "Files:" above.
 *              
 *              mvOpenFD() 
 *              mvIsMovieFile() 
 *              mvPlay() 
 *              mvStop() 
 *              mvClose() 
 *              mvFindTrackByMedium() 
 *              mvBindWindow() 
 *              mvSetFrameDisplay() 
 *              mvSetCurrentFrame() 
 *              mvSetViewBackground() 
 *              mvSetViewSize() 
 *              mvSetViewOffset() 
 *              mvSetSelectEvents() 
 *              mvGetImageWidth() 
 *              mvGetImageHeight() 
 *              mvGetLoopMode() 
 *              mvSetPlayLoopMode() 
 *              mvSetEnableAudio() 
 *              mvReleaseIrisGL() 
 *              mvGrabIrisGL() 
 */

#include "moviescreenArgs.h"
#include "moviescreenWin.h"
#include "moviescreenEvents.h"
#include <dmedia/moviefile.h>
#include <dmedia/movieplay.h>
#include <sys/types.h>
#include <unistd.h>

/***************************************************************************
 *
 * main
 *
 ***************************************************************************/

main( int argc, char **argv )
{

    /*
     * Interpret the command line arguments, setting user options
     * as appropriate, and building the movie list.
     */
 
    processCmdArgs( argc, argv );

    /* 
     * Seed the random number generator.
     */

    srandom( getpid() );

    /*
     * Create the main window for the screen saver.
     */

    if( createXWindow( ) == DM_FAILURE ) {
       exit( EXIT_FAILURE );
    }

    /*
     * Initiate/terminate screen saving as appropriate.
     */

    handleXEvent();
}
