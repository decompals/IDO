/*****************************************************************************
 *
 * File:        moviescreenEvents.c
 *
 * Description: Part of moviescreen. Contains code to start, perform, and end
 *              screen saving, depending on reception of XEvents.
 *
 * Functions:   SGI Movie Library functions used:
 *
 *  		mvOpenFile()
 *  		mvSetFrameDisplay()
 *  		mvSetCurrentFrame()
 *  		mvBindWindow()
 *  		mvSetViewBackground()
 *  		mvSetSelectEvents()
 *  		mvSetViewSize()
 *  		mvFindTrackByMedium()
 *  		mvGetImageWidth()
 * 		mvGetImageHeight()
 *  		mvSetViewOffset()
 *  		mvGetLoopMode()
 *  		mvSetPlayLoopMode()
 *  		mvSetEnableAudio()
 *  		mvPlay()
 *              mvStop()
 *              mvClose()
 *
 *****************************************************************************/

#include "moviescreenEvents.h"
#include "moviescreenArgs.h"
#include "moviescreenWin.h"
#include "moviescreenGl.h"
#include <stdio.h>
#include <X11/Xatom.h>
#include <audio.h>

static DMboolean        saving;       /* Indicates if moviescreen is currently
                                         saving the screen. */ 

extern int      sginap(long); /* Declaration of same in unistd.h is broken. */

/*
 * Forward declarations of functions local to this module.
 */

static void processEvent( XEvent *event );
static void beginSaveScreen( void );
static void initSaverMovie( void );
static void setupMovieWindow( MVid theMovie );
static void setupSmallScreenMovie( void );
static void setPlaybackLoopmode( MVid theMovie );
static void setVolumeLevel( MVid theMovie );
static void endSaveScreen( void );

/*********
 *
 * Wait for something to happen. Upon receiving an XEvent, process it.
 *
 *********/

void handleXEvent()
{
    for (; ; ) {
        XEvent event;
        XNextEvent( getXDisplay(), &event );
        processEvent( &event );
    }
}

/*********
 *
 * Interpret an XEvent and either start or terminate screen saving.
 *
 *********/

void processEvent( XEvent* event )
{
    if ( event->type == MapNotify ) {
        beginSaveScreen();
    } else if ( event->type == UnmapNotify ) {
        endSaveScreen();
    }
}

/*********
 *
 * Start saving the screen.
 *
 *********/

static void beginSaveScreen()
{
    Display *dpy = getXDisplay();

    initSaverMovie();

    /*
     * Wait for end request.
     */

    saving = DM_TRUE;

    while( saving ) {

        /*
         * Wait for one-fifth of a second
         */

        sginap( 20 );

        /*
         * Move the picture around.
         */

        undrawSaverPicture();
        moveSaverPicture();

        /*
         * Check for any events.
         */

        /* XXX this loop should also listen for movie events */
        XEventsQueued( dpy, QueuedAfterReading );
        while ( XPending( dpy ) > 0 ) {
            XEvent event;
            XNextEvent( dpy, &event );
            processEvent( &event );
        }
    }
}

/*********
 *
 * Select a movie, open it, set up the window, and start playing it.
 *
 ********/

static void initSaverMovie( )
{
    MVid        newMovie;
    char        *movieName;

    movieName = pickMovieAtRandom( );

    if ( mvOpenFile( movieName, O_RDONLY, &newMovie ) == DM_FAILURE ) {
        printf("%s: Could not open movie %s\n", getProgramName(), movieName );
        exit( EXIT_FAILURE );
    }
    setMovieID( newMovie );

    /*
     * Disable movie display temporarily, to avoid visual glitches.
     */

    mvSetFrameDisplay( DM_FALSE );

    setupMovieWindow( newMovie );

    setPlaybackLoopmode( newMovie );
 
    setVolumeLevel( newMovie );

    /*
     * play the movie
     */

    mvSetCurrentFrame( newMovie, 0 );
    mvSetFrameDisplay( DM_TRUE );
    mvPlay( newMovie );
}

/*********
 *
 * Bind the movie window and set the display parameters.
 *
 *********/

static void setupMovieWindow( MVid theMovie )
{
    Display     *dpy = getXDisplay();
    Window      xWin = getXWindow();

    /*
     * Bind movie window and set display parameters
     */

    if ( mvBindWindow( theMovie, dpy, xWin ) == DM_FAILURE ) {
        fprintf( stderr, "%s: Could not bind movie to window\n",
                getProgramName() );
        exit( EXIT_FAILURE );
    }
    mvSetSelectEvents( MV_EVENT_MASK_ERROR );

    if ( isFullScreen() ) {
        mvSetViewBackground( theMovie, 0, 0, 0 );
        mvSetViewSize( theMovie, DisplayWidth( dpy, DefaultScreen( dpy ) ),
                      DisplayHeight( dpy, DefaultScreen( dpy ) ), DM_TRUE );
    } else {
        setupSmallScreenMovie( );
    }
}

/*********
 *
 * Set up initial location and size for a movie which is to cover less
 * than the full screen.
 *
 *********/

static void setupSmallScreenMovie( )
{
    int         width;
    int         height;
    MVid        imageTrack;
    int         zoom = getZoom();
    MVid        theMovie = getMovieID();

    if ( mvFindTrackByMedium( theMovie, DM_IMAGE,
                             &imageTrack ) == DM_FAILURE) {
        fprintf( stderr, "%s: Could not find image track\n", getProgramName() );
        exit( EXIT_FAILURE );
    }
    width  = mvGetImageWidth( imageTrack );
    height = mvGetImageHeight( imageTrack );
    mvSetViewSize( theMovie, width * zoom, height * zoom, DM_TRUE );

    initPositionAndDirection( zoom, width, height );

    mvSetViewOffset( theMovie, getXOffset(), getYOffset(), DM_TRUE );
}

/*********
 *
 * If the movie doesn't explicitly specify loop
 * or swing mode, set one of them based on the
 * user's stated preferences.
 *
 *********/

static void setPlaybackLoopmode( MVid theMovie )
{
    int fileLoopmode = mvGetLoopMode( theMovie );

    if ( fileLoopmode == MV_LOOP_NONE ) {
        int movieLoopmode = getLoopmode();
        if ( movieLoopmode == MV_LOOP_NONE ) {
            mvSetPlayLoopMode( theMovie, MV_LOOP_CONTINUOUSLY );
        } else {
            mvSetPlayLoopMode( theMovie, movieLoopmode );
        }
    }
}

/*********
 *
 * Enable/disable audio for the movie. If the user specified a volume 
 * level, set the system audio appropriately.
 *
 *********/

static void setVolumeLevel( MVid theMovie )
{
    mvSetEnableAudio( theMovie, playSound() );
    if ( ( playSound() ) && ( isVolumeSet() ) ) {
        long aparam[4];
        aparam[0] = AL_LEFT_SPEAKER_GAIN;
        aparam[1] = getVolume();
        aparam[2] = AL_RIGHT_SPEAKER_GAIN;
        aparam[3] = getVolume();
        ALsetparams( AL_DEFAULT_DEVICE, aparam, 4 );
    }
}

/*********
 *
 * Terminate screen saving; stop and close the current movie.
 *
 *********/

static void endSaveScreen()
{
    MVid theMovie = getMovieID();
    saving = DM_FALSE;
    mvStop( theMovie );
    mvClose( theMovie );
}

