#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <libgen.h>		/* for basename() */
#include <limits.h>		/* for ULONGLONG_MAX */
#include <sys/types.h>
#include <sys/prctl.h>
#include <sys/time.h>

#include "dmrecord.h"

/********
*
* usage
*
* if invoked with non-zero verbose parameter (via '-h' option),
* usage displays the whole banana, else just a lean, mean, terse
* ditty in the Great Unix Tradition we all know and love to hate.
*
********/

void usage
    (
    int verbose
    )
{
	fprintf(stderr, "Usage: \n\
%s -h  : print help message\n\n\
%s [-v][-B auto|key ][-C][-t secs] -p video_port [-p audio_port] [-f file_options] FILE\n",
		g_ProgramName,g_ProgramName);

	if (verbose) {
		fprintf(stderr, "\n\
-p video_port   : -p video[,device=ev1][,port=N][,comp=jpeg[,engine=cosmo]\n\
                     [,quality=Q] [,brate=N] ]\n\
        device  : ev1 is the only supported video input device\n\
        port    : video input port number (if Galileo 601 option installed)\n\
        comp    : JPEG compression is the only supported scheme\n\
        engine  : compression engine (cosmo is the only supported JPEG engine)\n\
        quality : JPEG quality factor from 1-100 (default=75)\n\
	brate	: compression bit rate in bits per second (if omitted, record with no rate control\n\
\n\
-p audio_port   : -p audio[,channels=C]\n\
                     enable recording from audio input port\n\
                     (use Audio Control Panel to select\n\
                      source={mic,line,digital} and rate)\n\
        channels: 1 for mono, 2 for stereo\n\
\n\
-v      verbose\n\
-B      begin-recording mode\n\
            auto: recording begins as soon as possible\n\
            key : recording begins when user hits a key (default)\n\
-C      critical: abort recording, delete FILE if any video frames are dropped\n\
-t      recording duration in seconds (if omitted, record until <ctrl>-c)\n\
-f file_options: -f [title=movie_name]\n");

		fprintf(stderr, "\n\
Example: record synchronized audio and JPEG video tracks\n\
%s -p audio -p video,device=ev1,comp=jpeg,engine=cosmo,quality=75 out.mv\n\
\n\
Example: record 30 seconds of high-quality JPEG video\n\
%s -t 30 -p video,device=ev1,comp=jpeg,engine=cosmo,quality=90 out.mv\n\
\n\
Example: record 30 seconds of best-quality JPEG video at 14000000 bits/second\n\
%s -t 30 -p video,device=ev1,comp=jpeg,engine=cosmo,brate=14m out.mv\n",
		g_ProgramName,g_ProgramName,g_ProgramName);
	}
	exit(EXIT_FAILURE+1);
} /* usage */

/********
*
* processPathOptions
*
********/

void processPathOptions 
    (
    char* arg,
    Options* options
    )
{
    char* videoOpts[] = {
	#define DEVICE 0
	"device",
	#define PORT 1
	"port",
	#define TRACK 2		/* not supported yet */
	"track",		
	#define COMPRESSION 3
	"comp",
	#define ENGINE 4
	"engine",
	#define QUALITY 5
	"quality",
	#define HEIGHT 6
	"height",
	#define HALFX 7
	"halfx",
	#define HALFY 8
	"halfy",
	#define BRATE 9
	"brate",
	NULL };
    
    char* audioOpts[] = {
	#define CHANNELS 0
	"channels",
	NULL };
    
    char *value = NULL;
    char *p;
    
    /*
    ** check to see if it is audio or video.
    */
	
    if ( strncmp ( "audio", arg, strlen( "audio" ) ) == 0 ) {
	options->audio = 1;
	arg += strlen("audio");
	if ( *arg != ',' ) {
	    return;
	}
	arg++;	/* skip past the comma */
	while ( *arg != '\0' ) {
	    switch( getsubopt( &arg, audioOpts, &value ) ) {
	    case CHANNELS:
		if ( value == NULL ) {
		    usage(0);
		}
		options->audioChannels = atoi( value );
		if (options->audioChannels != 1 && 
		    options->audioChannels != 2) {
		    fprintf(stderr,
			    "channels = %d is out of range [1,2]", 
			    options->audioChannels);
		    fprintf(stderr, " Resetting to 2\n");
		    options->audioChannels = 2;
		}
		break;
	    default:
		/* process unknown token */
		fprintf(stderr,
			"Unknown -p (input path) suboption '%s'\n",value);
		usage(0);
		break;
	    } /* switch */
	} /* while */
	return;
    } /* if audio */
    
    else if ( strncmp ("video", arg, strlen( "video" ) != 0 ) ) {
	fprintf( stderr, "Unknown input option %s\n", arg);
	usage(0);
    }
    
    /* this is video, process all the fancy suboptions */
    options->video = 1;
    arg += strlen("video");
    if ( *arg != ',' ) {
	fprintf( stderr, "No video options specified\n" );
	usage(0);
    }
    arg++;	/* skip past the comma */
    while ( *arg != '\0' ) {
	switch( getsubopt( &arg, videoOpts, &value ) ) {
	case DEVICE:
	    if ( value == NULL ) {
		fprintf( stderr, 
			"No value specified for device\n" );
		usage(0);
	    }
	    options->videoDevice = strdup( value );
	    break;
	case PORT:
	    if ( value == NULL ) {
		fprintf( stderr, 
			"No value specified for port\n" );
		usage(0);
	    }
	    options->videoPort = atoi( value );
	    if ( (options->videoPort < 0) || (options->videoPort > 2) ) {
		fprintf( stderr, 
			"port %d is out of range [0-2]. ",
			options->videoPort);
		fprintf( stderr, "Resetting value to 0\n");
		options->videoPort = 0;
	    }
	    break;
	case COMPRESSION :
	    if ( value == NULL ) {
		fprintf( stderr, 
			"No value specified for compression\n" );
		usage(0);
	    }
	    options->compressionScheme = strdup( value );
	    break;
	case ENGINE :
	    if ( value == NULL ) {
		fprintf( stderr, 
			"No value specified for engine\n" );
		usage(0);
	    }
	    options->compressionEngine = strdup( value );
	    break;
	case QUALITY :
	    if ( value == NULL ) {
		fprintf( stderr, 
			"No value specified for quality\n" );
		usage(0);
	    }
	    options->qualityFactor = atoi( value );
	    if (options->qualityFactor < 1 || options->qualityFactor > 100) {
		fprintf(stderr,
			"quality of %d is out of range ", 
			options->qualityFactor);
		fprintf(stderr,
			"[1,100]. Resetting quality to 75\n");
		options->qualityFactor = 75;
	    }
	    break;
	case HEIGHT:
	    if (value != NULL) {
		options->height = atoi( value );
	    }
	    break;
	case HALFX:
	    options->halfx = 1;
	    break;
	case HALFY:
	    options->halfy = 1;
	    break;
	case BRATE :
	    if ( value == NULL ) {
		fprintf( stderr, 
			"No value specified for brate\n" );
		usage(0);
	    }
#define MAX_BIT_RATE  100000000
	    {
	    char *cp;
	    double d;

	    d = strtod(value, &cp);
	    switch(*cp) {
		case 'm':
		case 'M':
		    d *= 1000000;
		    break;
		case 'k':
		case 'K':
		    d *= 1000;
		    break;
	    }
	    options->avrFrameRate = (int)d;
	    }
	    if (options->avrFrameRate < 0 || options->avrFrameRate > MAX_BIT_RATE ) {
		fprintf(stderr,
			"Bit rate of %d is out of range. ", 
			options->avrFrameRate);
		fprintf(stderr,
			"Resetting value to zero\n");
		options->avrFrameRate = 0;
	    }
	    break;
	default:
	    /* process unknown token */
	    fprintf(stderr,
		    "Unknown -p (input path) suboption '%s'\n",value);
	    usage(0);
	    break;
	} /* switch */
    } /* while */
} /* processPathOptions */

/********
*
* processFileOptions
*
********/

void processFileOptions
    (
    char* arg,
    Options* options
    )
{
    char* myopts[] = {
	#define TITLE 0
	"title",
	NULL };

    char* value = NULL;
    while ( *arg != '\0' ) {
	switch( getsubopt( &arg, myopts, &value ) ) {
	case TITLE:
	    if ( value == NULL ) {
		fprintf( stderr, 
			"No value specified for title\n" );
		usage(0);
	    }
	    options->movieTitle = strdup( value );
	    break;
	default:
	    /* process unknown token */
	    fprintf(stderr,
		    "Unknown -f (file option) suboption `%s'\n",value);
	    usage(0);
	    break;
	} /* switch */
    } /* while */
} /* processFileOptions */

/********
*
* parseArgs
*
********/

void parseArgs
    (
    int argc, 
    char **argv,
    Options* options
    )
{
    extern char* optarg;
    extern int   optind;
    int	         c;
    int          res;
    
    /*
    ** Parse the command line arguments.
    */
    
    while ((c = getopt(argc, argv, "B:t:vhp:f:C")) != EOF) {
	switch (c) {
	case 'B':
	    if ( strcmp( optarg, "auto" ) == 0 ) {
		options->batchMode = 1;
	    }
	    else if ( strcmp( optarg, "key" ) == 0 ) {
		options->batchMode = 0;
	    }
	    else {
		usage(0);
	    }
	    break;
	    
	case 'C':
	    options->critical = 1;
	    break;
	    
	case 't':
	    options->seconds = atoll(optarg);
	    if ( options->seconds <= 0 ) {
		fprintf(stderr,
			"-t opt: invalid \"seconds\" arg (%d)\n",
			options->seconds);
		usage(0);
	    }
	    break;

	case 'v':
	    options->verbose++;
	    break;
	    
	case 'f':
	    processFileOptions( optarg, options );
	    break;
	    
	case 'p':
	    processPathOptions( optarg, options );
	    break;
	    
	case 'h':	/* display the detailed usage message */
	    usage(1);
	    /*NOTREACHED*/;
	    
	case '?':
	    usage(0);
	    /*NOTREACHED*/;
	    
	} /* switch */
    } /* while */
    
    if (optind == argc) {
	/* missing file name */
	fprintf(stderr, "Missing filename\n");
	usage(0);
    }
    else {
	options->fileName = argv[optind];
    }
    
    if ( options->verbose > 1 ) {
	printf("%s: args ", g_ProgramName);
	for (res = 0; res < argc; res++)
	    printf("%s ", argv[res]);
	printf("\n");
    }
}
