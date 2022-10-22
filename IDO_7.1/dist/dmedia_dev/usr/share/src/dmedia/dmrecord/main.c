/**********************************************************************
*
* File: main.c
*
* JPEG movie capture program.
*
**********************************************************************/

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <libgen.h>		/* for basename() */
#include <limits.h>		/* for ULONGLONG_MAX */
#include <unistd.h>		/* for setreuid(), etc. */
#include <sys/types.h>
#include <sys/prctl.h>
#include <sys/schedctl.h>
#include <sys/time.h>
#include <dmedia/dmedia.h>
#include <dmedia/vl.h>

#include "dmrecord.h"
#include "handy.h"

extern void setscheduling (int hiPriority);

static char defaultString[] = "Default";

/*
**	global variables
**
*/

char* g_ProgramName;
static Options options;
static volatile Boolean 	doneFlag = FALSE;

/*
**
**	Signal catcher
**
*/
static void
sigint()
{
	doneFlag = 1;
}

/*
**	This following two are callback functions. They are defined here but 
**	called by the modules that do the real work. 
**
**	done(int thisFrame) 
**	will be called once for each iteration of the main loop in the module.
**	It returns 1 if it is time to break out of the main loop.
*/

int
done()
{

	if ( doneFlag ) {
		return doneFlag;
	}

	return 0;
}

/*
**	goAhead() will be called by the modules after they have done the initial
**	setup but before they enter the main loop. goAhead() will block until it
**	time to actually begin recording.
*/

void
goAhead()
{
	if ( !options.batchMode) {
		printf ("\nHit the Enter key to begin recording... ");
		fflush(stdout);
		getchar();
	}

	if ( !options.seconds) {	/* record till user key stroke or <ctrl>-c. */
		printf("Hit <ctrl>-c to stop recording...\n");
	}
	signal(SIGINT, sigint);
} /* goAhead */

/********
*
* main
*
********/

int main
    (
    int argc, 
    char** argv
    )
{
    /*
    ** Save the program name to use in error messages.
    */
    
    g_ProgramName = basename( argv[0] );

    /*
    ** Set high non-degrading priority for realtime
    */
    
    setscheduling( 1 );

    /*
    ** switch to running as joe user (instead of root) if run setuid 
    */
	
    setreuid( getuid(), getuid() );
    setregid( getgid(), getgid() );
    
    /*
    ** Set the default options.
    */

    options.height 		= 0;
    options.videoDevice		= NULL;
    options.videoPort 		= VL_ANY;
    options.critical 		= FALSE;
    options.verbose 		= FALSE;
    options.fileName 		= NULL;
    options.movieTitle 		= NULL;
    options.video 		= FALSE;
    options.audio 		= FALSE;
    options.audioChannels 	= 2;
    options.seconds 		= 0;
    options.batchMode		= FALSE;
    options.compressionScheme	= FALSE;
    options.compressionEngine	= FALSE;
    options.qualityFactor	= 75;
    options.halfx		= 0;
    options.halfy		= 0;

    /*
    ** Use the command-line arguments to override the defaults.
    */

    parseArgs( argc, argv, &options );
    
    /*
    ** check if every thing has been correctly specified.
    */
	
    if ( ! options.video ) {
	fprintf (stderr, "Video path not specified\n" );
	usage(0);
    }
    
    if ( options.videoDevice == NULL ) {
	/* if video device not specified, default to ev1 */
	options.videoDevice = strdup("ev1");
	if ( options.verbose > 1) {
	    printf("Using default video device:  ev1\n");
	}
    }
    
    if ( options.videoDevice && strcmp ( options.videoDevice, "ev1" ) != 0 ) {
	fprintf(stderr,"Unsupported video device '%s'\n",options.videoDevice);
	usage(0);
    }
    
    if ( ! options.compressionScheme ) {
	/* if compression scheme not specified, default to jpeg */
	options.compressionScheme = strdup("jpeg"); 
	if ( options.verbose > 1) {
	    printf("Using default video compression scheme:  jpeg\n");
	}
    }
    
    if ( options.compressionScheme && 
	 strcmp( options.compressionScheme, "jpeg" ) !=0 ) {
	fprintf ( stderr, "Unsupported compression scheme '%s'\n",
			options.compressionScheme);
	usage(0);
    }
    
    if ( ! options.compressionEngine ) {
	/* if compression engine not specified, default to cosmo */
	options.compressionEngine = strdup("cosmo"); 
	if ( options.verbose > 1)
	    printf("Using default video compression engine:  Cosmo\n");
    }
    
    if ( options.compressionEngine && 
	 strcmp( options.compressionEngine, "cosmo" ) != 0 ) {
	fprintf ( stderr, "Unsupported compression engine '%s'\n",
		  options.compressionEngine);
	usage(0);
    }
    
    if ( options.verbose ) {
	printf( "Options:\n");
	if ( options.audio )
	    printf("        Audio channels: %d\n", options.audioChannels);
	if ( options.video ) 
	    printf("          Video device: %s\n"
		   "    Compression scheme: %s\n"
		   "    Compression engine: %s\n",
		   options.videoDevice, 
		   options.compressionScheme, 
		   options.compressionEngine);
	if ( options.avrFrameRate )
	    printf("              Bit Rate: %d bits/sec\n", 
		   options.avrFrameRate);
	else
	    printf("               Quality: %d\n", 
		   options.qualityFactor);

	if ( options.movieTitle)
	    printf("File options:\n"
		   "                 Title: %s\n", options.movieTitle);
    } /* if verbose */
    
    cosmo_capture( &options );
    
    exit( EXIT_SUCCESS );
} /* main */

