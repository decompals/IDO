/*****************************************************************************
 *
 * File:        moviescreenArgs.c
 *
 * Description: Part of moviescreen. Contains code for helper functions
 *              used by moviescreen for accessing/manipulating data
 *              entered by the user via command line arguments.
 *              Maintains this information as static variables restricted
 *              to this module.
 *
 * Functions:   SGI Movie Library functions used:
 *
 *		mvIsMovieFile()
 *
 *****************************************************************************/

#include "moviescreenArgs.h"
#include <stdio.h>
#include <getopt.h>

/*
 * Tiny structure to hold a linked list of movie names.
 */

typedef struct _namelist {
    char*       name;
    struct _namelist *  next;
} namelist;

/*
 * Variables restricted to this module.
 */

static char 		*programName	= NULL;
static namelist 	*movieNames 	= NULL;
static MVid 		movie 		= 0;
static int 		movieCount 	= 0;
static DMboolean 	fullScreen  	= DM_FALSE;
static DMboolean 	sound 		= DM_FALSE;
static int 		volume 		= 0;
static DMboolean 	volumeSet 	= DM_FALSE;
static int 		zoom 		= 1;
static MVloopmode 	loopmode 	= MV_LOOP_CONTINUOUSLY;

/*
 * Forward declarations for functions local to this module.
 */

static DMstatus makeMovieNameList( char **newMovieNames, int numNames );
static void 	setMovieCount( int numMovies );
static void 	setFullScreen( DMboolean fillScreen );
static void 	setSound( DMboolean soundOn );
static void 	setVolume( int newVolume );
static void 	clearVolume( void );
static void 	setZoom( int newZoom );
static void 	setLoopmode( MVloopmode newLoopmode );

/*********
 *
 * Process the command line arguments, setting user preferences, building
 * and validating the movie list.
 *
 *********/

void processCmdArgs( int argc, char**argv )
{
    int 	ch;

    if ( argc < 2) {
        usage();
    }

    programName = argv[0];

    while ( ( ch = getopt( argc, argv, "fsl:v:z:" ) ) != -1 ) {
        switch ( ch ) {
        case 'f':

            /*
             * Fill the screen with the movie.
             */

            setFullScreen( DM_TRUE );
            break;
        case 'l':

            /*
             * Set to loop or swing.
             */

            if ( optarg == NULL ) {
                usage();
	    }
	    {
    	        MVloopmode newLoopmode;
                sscanf( optarg, "%d", &newLoopmode );
                if ( ( newLoopmode < 0 ) || ( newLoopmode > 1 ) ) {
                    usage();
                }
                setLoopmode( newLoopmode );
	    }
            break;
        case 's':

            /*
             * Play the sound track.
             */

            setSound( DM_TRUE );
            break;
        case 'v':

            /*
             * Set the volume level.
             */

            if ( optarg == NULL ) {
                usage();
	    }
	    {
    	        int volumeLevel;
                sscanf( optarg, "%d", &volumeLevel );
                if ( ( volumeLevel < 0 ) || ( volumeLevel > 255 ) ) {
                    usage();
                }
                setVolume( volumeLevel );
	    }
            break;
        case 'z':

            /*
             * Set the zoom factor.
             */

            if ( optarg == NULL ) {
                usage();
	    }
    	    {
		int newZoom;
            	sscanf( optarg, "%d", &newZoom );
            	setZoom( newZoom );
	    }
            break;
        case '?':
            usage();
        }
    }

    {
        /*
         * Make the list of movie names, verifying that
         * each is a movie file before continuing.
         */

        char **newNameList = &( argv[ optind ] );

        if ( makeMovieNameList ( newNameList, 
				( argc - optind ) ) != DM_SUCCESS ) {
            exit( EXIT_FAILURE );
        }
    }
}

/*********
 *
 * Inform the unenlightened about the proper invocation of the program.
 *
 *********/

void usage()
{
    fprintf( stderr, "usage: %s [-k] [-f] [-s] [-v vol] [-z zoom]\n"
        "       [-l loopmode] moviefile [moviefile2...]\n\n"
        " -f: Fill the screen.  Enlarge the movie frame to fill screen.\n"
        "     The default is normal size.\n\n"
        " -s: Play sound.  The default is silent.\n\n"
        " -v <vol> : Set the volume for audio playback.  This takes\n"
        "     effect only if sound is enabled.  <vol> is an integer in the\n"
        "     range: 0 - 255 (255 full volume).\n\n"
        " -z <zoom> : Zoom factor. Integer values only.  This takes\n"
        "     effect only if fullscreen display is off.\n\n"
        " -l <loopmode> : Loop/swing.  0 means loop continuously,\n"
        "     1 means swing playback back and forth. The default is\n"
        "     0 (loop continuously).  If a movie has been set\n"
        "     to loop or swing in its movie header, that setting\n"
        "     will be respected - this allows you to specify some\n"
        "     movies which loop continuously, and others to swing.\n\n",
        programName );
    exit( EXIT_FAILURE );
}

/*********
 *
 * Make the list of movie names. Verify that each is a movie file before
 * continuing.
 *
 *********/

static DMstatus makeMovieNameList( char **newMovieNames, int numNames )
{
    int		i;
    namelist 	*end 	= NULL;
    namelist 	*current = NULL;

    for ( i = 0; i < numNames; i++ ) {

        if ( !( mvIsMovieFile( newMovieNames[i] ) ) ) {
            fprintf( stderr, "%s: %s is not a movie file.\n",
                    programName, newMovieNames[i] );
            return  DM_FAILURE ;
        }
        current = ( namelist * ) malloc( sizeof( namelist ) );
        if ( current == NULL ) {
            fprintf( stderr, "%s: Malloc failed.\n", programName );
            return DM_FAILURE;
        }
        current->name = newMovieNames[ i ];
        if ( end ) {
            end->next = current;
        } else {
            movieNames = current;
        }
        end = current;
    }

    setMovieCount( numNames );

    return DM_SUCCESS;
}

/*********
 *
 * Choose a movie at random from the list of movies provided
 * on the command line.
 *
 *********/

char *pickMovieAtRandom( void )
{
    int 	i;
    int 	r = random() % getMovieCount();
    namelist 	*current = movieNames;

    for ( i = 0; i < r ; i++ ) {
        current = current->next;
    }
    return current->name;
}

/*********
 *
 * Set and retrieve the MVid of the current movie.
 *
 *********/

void setMovieID( MVid newMovieID )
{
    movie = newMovieID;
}

MVid getMovieID( void )
{
    return movie;
}

/*********
 *
 * Set and retrieve the number of movies entered on the command line.
 * setMovieCount() is local to this module.
 *
 *********/

static void setMovieCount( int numMovies )
{
    movieCount = numMovies;
}

int getMovieCount( void )
{
    return movieCount;
}

/*********
 *
 * Set and retrieve the value of fullScreen - whether or not the movie is
 * to fill the screen. setFullScreen() is local to this module.
 *
 *********/

static void setFullScreen( DMboolean fillScreen )
{
    fullScreen = fillScreen;
}

DMboolean isFullScreen()
{
    return fullScreen;
}

/*********
 *
 * setSound(), local to this module, turns sound on or off for playback.
 * playSound() returns a DMboolean indicating the on or off state.
 *
 *********/

static void setSound( DMboolean soundOn )
{
    sound = soundOn;
    if ( sound == DM_FALSE )
        clearVolume();
}

DMboolean playSound( void )
{
    return sound;
}

/*********
 *
 * Set and retrieve the volume setting for playback, clear the volume
 * setting, and determine if it has been set. setVolume() and clearVolume()
 * are local to this module.
 *
 *********/

static void setVolume( int newVolume )
{
    volume = newVolume;
    volumeSet = DM_TRUE;
}

static void clearVolume( void )
{
    volume = 0;
    volumeSet = DM_FALSE;
}

int getVolume( void )
{
    return volume;
}

DMboolean isVolumeSet( void )
{
    return volumeSet;
}

/*********
 *
 * Set and retrieve a zoom factor, which determines the size of the
 * movie region for playback. setZoom() is local to this module.
 *
 *********/

static void setZoom( int newZoom )
{
    zoom = newZoom;
}

int getZoom( void )
{
    return zoom;
}

/*********
 *
 * Set and retrieve the preferred loopmode for playback.
 *
 *********/

static void setLoopmode( MVloopmode newLoopmode )
{
    loopmode = newLoopmode;
}

int getLoopmode( void )
{
    return loopmode;
}

/*********
 *
 * Retrieve the name of the program.
 *
 *********/

char *getProgramName( void )
{
    return programName;
}
