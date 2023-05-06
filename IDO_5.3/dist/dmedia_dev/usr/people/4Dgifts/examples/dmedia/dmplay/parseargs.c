/*
 *parseargs.c: command line parser for dmplay
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <libgen.h> /* for basename() */

#include "dmplay.h"

extern char *optarg;
extern int optind, opterr,optopt;

/*
 *Prototype definitions
 */
void parseargs (int argc, char *argv[], char *myname);
static void usage(int);
static void process_port_opts(char *options,_Options *opts,int type,
                                 char *progname);
static void check_opts(_Options *opts);

/*
 * not documented:
 * 
 * single-frame decompression engines which use the code
 * in singleFrameDecompress.c:
 *     engine=cosmoSF  and  engine=swSF
 *
 * -P [cont | step]  playback modes
 */
void
usage(int verbose)
{
	fprintf(stderr,
	"Usage:\n"
	"%s -h		- Display long help \n"
	"%s -p graphics|video[,device=ev1][,engine=cosmo|sw]  [-p audio]\n"
	"     [-v] [-E auto|key] [-L none|repeat] file\n"
	"	        - Play file \n",
	options.myname, options.myname);

	if (verbose) {
		fprintf(stderr, "\n"
	"-p\n"
	"        graphics  play movie in a graphics window\n"
	"        video     play movie out video and in a window\n"
	"        device    specify video device (IndyVideo/Galileo is ev1)\n"
	"        engine    specify decompression engine (Cosmo or software)\n"
	"\n"
	"-v      verbose\n"
	"-E\n"
	"        auto      Program stops when playback is finished\n"
	"        key       Wait for user input to terminate program (default)\n"
	"-L\n"
	"        none 	   Play the sequence once\n"
	"        repeat    Loop through sequence from beginning to end\n\n\n");
	}

	exit(EXIT_FAILURE);
}


#define A_PORT 1
#define V_PORT 2
#define G_PORT 3

/*
 * Process a port option which looks like this:
 *    -p port_desc,params
 *
 * Port option could eventually look like this:
 *    -p video,device=dev:0,port=0,track=1,params...
 *
 * This would mean: display image track 1 from movie using
 * port 0 on the 0th instance of video device 'dev'.
 * 
 */
void process_port_opts(char *options, _Options *opts, int port_type,
                            char *progname)
{
    char *myopts[] = {
            #define DEVICE       0   /* eg  device=ev1 */
            "device",
            #define PORT         1   /* eg  port=2   not supported yet */
            "port",
            #define TRACK	 2   /* eg  track=2  not supported yet */
            "track",
            #define ENGINE       3   /* decompression engine */
            "engine",
            NULL };
    char *path;
    char *value = NULL;
    int len;

    path = options;

    while ( *options != '\0')
    {
        switch( getsubopt( &options, myopts, &value ) ) 
        {
            case DEVICE:
                if (value == NULL) {
                    fprintf(stderr, "No value specified for device: '%s'\n",
                                          path);                        
                    usage(0);
                }
                if (port_type != V_PORT) {
                    fprintf(stderr, "Invalid device '%s'\n", value);
                    usage(0);
                }
                if ((port_type==V_PORT) && (strcmp(value, "ev1") != 0)) {
                    fprintf(stderr, "Invalid video device '%s'\n", value);
                    usage(0);
                }
                /* only device is 'ev1' for video right now */
                break;
            case PORT:
                if (value == NULL) {
                    fprintf(stderr, "No value specified for port: '%s'\n",
                                          path);                        
                    usage(0);
                }
                if (port_type != V_PORT) {
                    fprintf(stderr, "Invalid port argument '%s'\n", value);
                    usage(0);
                }
                if (port_type==V_PORT) {
                    fprintf(stderr, 
                       "Not yet able to specify port '%s' for device '%s'\n",
                            value, opts->vid_device);
                }
                break;
            case TRACK:  /* not implemented yet */
                if (value == NULL) {
                    fprintf(stderr, "No value specified for track: '%s'\n",
                                          path);                        
                    usage(0);
                }
                fprintf(stderr, 
                    "Not yet able to specify track. Using default %s track.\n",
                      port_type == A_PORT ? "audio" : "image");
                break;
            case ENGINE:
                if (value == NULL) {
                    fprintf(stderr, "No value specified for engine: '%s'\n",
                               path);
                    usage(0);
                }
                if (port_type == A_PORT) {
                    fprintf(stderr, "Invalid engine argument: '%s'\n", path);
                    usage(0);
                }
                opts->image_engine = strdup(value);
                break;

            default:
                /* process unknown token */
                fprintf(stderr, "Unknown display path suboption '%s'\n",
                                      value);
                usage(0);
                break;
        }
    }
}


/*
 *parseargs: parses the command line options from the user.
 */
void parseargs (int argc, char *argv[], char *myname) 
{
    int ch;

    _Options *opts = &options;

    /*
     * defaults
     */
    opts->myname = strdup(basename(myname));
    opts->use_default_ports = 1;
    opts->audio_port_specified = 0;
    opts->playAudioIfPresent = 0;
    opts->display_port_specified = 0;
    opts->ifilename = NULL;
    opts->verbose    = 0;
    opts->initialLoopMode= LOOP_NOTSPEC;
    opts->autostop   = 0;
    opts->initialPlayMode= PLAY_CONT;
    opts->image_engine = strdup("cosmo");
    opts->image_port_type = 0;
    opts->vid_device = strdup("ev1");

    if (argc < 2) {
	usage(0);
    }


    while ((ch = getopt (argc, argv, "hvE:P:L:p:")) != EOF) 
    {
	switch (ch) 
        {
            case 'p':
                opts->use_default_ports = 0;
                if (!strncmp( optarg, "audio", strlen("audio"))) {
                    if (opts->audio_port_specified)
                    {
                        fprintf(stderr, 
                          "Please specify only one audio port option.\n");
                        usage(0);
                    }
                    else 
                    {
                        opts->audio_port_specified = 1;
                    }
                    opts->playAudioIfPresent = 1; 
                    while (*optarg != ',' && *optarg != '\0') optarg++;
                    if (*optarg != '\0') {
                        optarg++; /* skip comma */
                        process_port_opts( optarg ,opts, A_PORT, argv[0]);
                    }
                }
                else if (!strncmp( optarg, "video", strlen("video"))) {
                    if (opts->display_port_specified)
                    {
                        fprintf(stderr, 
                          "Please specify only one display port option.\n");
                        usage(0);
                    }
                    else 
                    {
                        opts->display_port_specified = 1;
                    }
                    opts->image_port_type = V_PORT;
                    while (*optarg != ',' && *optarg != '\0') optarg++;
                    if (*optarg != '\0') {
                        optarg++; /* skip comma */
                        process_port_opts( optarg ,opts, V_PORT, argv[0]);
                    }
                }
                else if (!strncmp( optarg, "graphics", strlen("graphics"))) {
                    if (opts->display_port_specified)
                    {
                        fprintf(stderr, 
                          "Please specify only one display port option.\n");
                        usage(0);
                    }
                    else 
                    {
                        opts->display_port_specified = 1;
                    }
                    opts->image_port_type = G_PORT;
                    while (*optarg != ',' && *optarg != '\0') optarg++;
                    if (*optarg != '\0') {
                        optarg++; /* skip comma */
                        process_port_opts( optarg ,opts, G_PORT, argv[0]);
                    }
                }
                else {
                    fprintf(stderr, "Unrecognized port type '%s'\n", optarg);
                    usage(0);
                }
	        break;

            case 'E':
                if (!strcmp(optarg, "auto") ) {
                    opts->autostop = 1;
                }
                else if (!strcmp(optarg, "key") ) {
                    opts->autostop = 0;
                }
                else {
                    fprintf(stderr, "Unknown stop mode '%s'\n", optarg);
                    usage(0);
                }
                break;
            case 'P':
                if (!strcmp(optarg, "cont") ) {
                    opts->initialPlayMode=PLAY_CONT;
                }
                else if (!strcmp(optarg, "step") ) {
                    opts->initialPlayMode= PLAY_STEP;
                }
                else {
                    fprintf(stderr, "Unknown play mode '%s'\n", optarg);
                    usage(0);
                }
                break;

	    case 'L':
                if (!strcmp(optarg, "repeat") ) 
		    opts->initialLoopMode = LOOP_REPEAT;
		else if (!strcmp(optarg, "none") ) 
		    opts->initialLoopMode = LOOP_NONE;
		else {
                    fprintf(stderr, "Unknown loop mode '%s'\n", optarg);
                    usage(0);
                }
	        break;

	    case 'v':
                opts->verbose=1;
	        break;

	      case 'h':
		usage (1);
		break;

	    case '?':
	        usage(0);
	        break;

	    default:
	        break;
	}
    }

    if (optind == argc) {
        fprintf(stderr, "No input file specified\n");
        usage(0);
    }
    if (optind < argc-1) {
        fprintf(stderr, "Multiple file arguments not supported\n");
        usage(0);
    }
    opts->ifilename = strdup(argv[optind]);
 

    check_opts(opts);

    exit;
}


/*
 * check the command-line options for consistency
 * set the global control variables for the program
 */
void 
check_opts(_Options *opts)
{

    if (opts->audio_port_specified && !opts->display_port_specified)
    {
        fprintf(stderr, "Audio port specified but no display port specified\n");
        fprintf(stderr, "Please specify -p [video | graphics][,....]\n");
        usage(0);
    }

    if (opts->use_default_ports)
    {
        opts->playAudioIfPresent = 1;

        opts->image_port_type = V_PORT;
        opts->image_engine = strdup("cosmo");
        opts->vid_device = strdup("ev1");

        if (opts->verbose) 
        {
            fprintf(stderr, "No port (-p) options specified: using defaults\n");
        }
    }

    if ((opts->initialPlayMode==PLAY_STEP) && (opts->playAudioIfPresent))
	{
        fprintf(stderr,"Audio playback disabled during single-step playback\n");
    }

    if ((opts->audio_port_specified) && 
        ((opts->image_port_type!=V_PORT)
             || strcasecmp("cosmo", opts->image_engine)))
    {
        fprintf(stderr, 
            "%s: -p audio requires -p video,device=ev1,engine=cosmo\n",
            opts->myname);
        usage(0);
    }

    if (!strncmp(opts->image_engine, "sw", strlen("sw")) 
          && (opts->image_port_type == V_PORT)) 
    {
        fprintf(stderr, "Must use 'engine=cosmo' when display is 'video'\n");
        usage(0);
    }

    if (opts->verbose) {
        printf("input file: %s\n", opts->ifilename);
        printf("Initial playback options:\n");
        printf("	looping      = %s\n", 
                         opts->initialLoopMode==LOOP_REPEAT?"repeat":"none");
        printf("	play mode    = %s\n", opts->initialPlayMode==PLAY_STEP
                                         ?"single-step": "continuous");
        printf("Display port:\n");
        printf("	display      = %s\n", opts->image_port_type == V_PORT ?
                                  "video" : "graphics");
        if (opts->image_port_type == V_PORT) 
            printf("	device       = IndyVideo/Galileo\n");
        printf("        engine       = %s JPEG\n", opts->image_engine);
        printf("Audio port:\n");
        printf("	%s if soundtrack present\n", 
                         opts->playAudioIfPresent?"enable playback":"mute");
    }
  
    if (opts->image_engine != NULL) { 
        if ( !strcasecmp ("sw", opts->image_engine)) {
            codec.engine = CL_JPEG_SOFTWARE;
    	    codec.singleFrame = 0;
        } 
        else if ( !strcasecmp ("swSF", opts->image_engine)) {
    	    codec.engine = CL_JPEG_SOFTWARE;
    	    codec.singleFrame = 1;
        } 
        else if ( !strcasecmp ("cosmo", opts->image_engine)) {
            codec.engine = CL_JPEG_COSMO;
            codec.singleFrame = 0;
        } 
        else if ( !strcasecmp ("cosmoSF", opts->image_engine)) {
            codec.engine = CL_JPEG_COSMO;
            codec.singleFrame = 1;
        } 
        else {
            fprintf(stderr, "Unrecognized decompression engine '%s'\n",
                                        opts->image_engine);
            usage(0);
        }
    }

    if ((opts->initialPlayMode==PLAY_STEP) && (!codec.singleFrame)) {
	fprintf (stderr, 
		 "Single-step playback not implemented for engine \"%s\"\n",
		 opts->image_engine);
        usage(0);
    }
    
    if (opts->image_port_type == G_PORT) {
            image.display = GRAPHICS;
    } 
    else if (opts->image_port_type == V_PORT) {
            image.display = VIDEO_2_FIELD;
    } 
    else {
        fprintf(stderr, "Unrecognized display port\n");
        usage(0);
    }

    movie.filename = strdup(opts->ifilename);
}
