/*
 * recordaifc:
 *
 * 4Dgifts recordaiff program now ported to the audio file library
 *             scott porter, august 91
 * 
 * - program now supports AIFF and AIFF-C file formats
 * - program will do real-time G.722 and u-law compression via audio file
 *         library (AIFF-C only)
 *              scott porter, february 92
 * - added support for 4-channel data
 *               june 93
 *
 * - when this program is named "recordaiff" it creates AIFF files by default
 *                                             (for backward compatability)
 * - when this program is named "recordaifc" (or anything else) it 
 *                                             creates AIFF-C files by default
 */
#ident "$Revision: 1.36 $"

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <malloc.h>
#include <math.h>
#include <string.h>
#include <signal.h>
#include <dmedia/audio.h>
#include <dmedia/audiofile.h>
#include <dmedia/audioutil.h>
#include <limits.h>
#include <sys/prctl.h>
#include <sys/schedctl.h>

static char recordaiff[] = "recordaiff"; /* old name for this program */

typedef struct
{
    long frames_per_sec;
    long bits_per_samp;
    long samps_per_frame;
    long time_limit_specified;
    double time_limit;
    long compression;
    long filefmt;
} record_params_t;

/*
 * local subroutines
 */
static void record_params_defaults(record_params_t *);
static void parse_cmd_line(int argc, char **argv, record_params_t *);
static void record_audio(ALport, AFfilehandle, record_params_t *);
static void on_sigint();
static void getpathtail(char *, char **);
static void quietAFerror(long, const char *);
static void quietALerror(long, const char *, ...);
static void defaultAFerror(long, const char *);
static void defaultALerror(long, const char *, ...);
static void SizeInBytes(int totalBytes, char *outputString);
static void Duration(double seconds, char *outputString);

/*
 * globals 
 */
static char  *filename;         /* input file name */
static char  *filenametail;
static int   fd;                /* input file descriptor */
static char  *myname;           /* name of this program */
static int   verbose;           /* global flag */
static int   quiet;             /* global flag */
static int   caught_sigint;     /* global flag */
static int   times_up;          /* stop recording */
static record_params_t record_params;

static char options[] = 
"\n\
  options:\n\
  [-nchannels (1 2 4)]   [-samplefmt (8 16 24)] [-rate (44100 48000 etc)]\n\
  [-verbose]             [-help]                [-time (seconds)]\n\
  [-compress (g722 ulaw alaw awmulti)]          [-filefmt (aiff aifc)]\n\
  [-quiet]\n";



/*
 * determine the current input rate
 */
    static long 
get_input_rate()
{
    long pvbuf[2];
    long input_rate;
    long buflen, *buf;
    long i, n;
    
    pvbuf[0] = AL_INPUT_RATE;
    ALgetparams(AL_DEFAULT_DEVICE, pvbuf, 2);
    input_rate = pvbuf[1];
    
    /*
     * WARNING!!! The rate returned by ALgetparams is not necessarily
     * a value in Hz.  It can be one of the special tokens we test for
     * here.
     *
     * If it is not one of these tokens, then assume rate is in Hz.
     *
     * If it is an AES token, then we get the AL_DIGITAL_INPUT_RATE
     * parameter to see what the actual current AES rate is.
     *
     * note that AL_DIGITAL_INPUT_RATE is not currently supported
     * on all audio platforms, so we check to see if it is supported
     * using ALqueryparams()
     */

    if (input_rate == AL_RATE_AES_1 ||
        input_rate == AL_RATE_AES_2 ||
        input_rate == AL_RATE_AES_3 ||    
        input_rate == AL_RATE_AES_4 ||
        input_rate == AL_RATE_AES_6 ||
        input_rate == AL_RATE_AES_1s)
    {
        /* input rate is digital */
        /* attempt to get digital input rate */
        buflen=ALqueryparams(AL_DEFAULT_DEVICE,0,0); 
        buf=(long *)malloc(buflen*sizeof(long));
        (void)ALqueryparams(AL_DEFAULT_DEVICE,buf,buflen); 
        n = buflen/2;
        /* search buffer to see if digital input rate is valid ... */
        for (i = 0; i < n; i++) {
            if (buf[2*i] == AL_DIGITAL_INPUT_RATE) {
                /* get digital input rate ... */
                ALgetparams(AL_DEFAULT_DEVICE, buf+2*i, 2);
                input_rate = buf[2*i+1];
                break;
            }
        }
        free(buf);
    }
    return input_rate;
}

main(int argc, char **argv)
{
    int i,n,r;
    record_params_t record_params;
    ALport audio_port;
    ALconfig audio_port_config;
    AFfilehandle audio_file;
    AFfilesetup audio_file_setup;
    int fd;
    
    /*myname = argv[0];*/
    getpathtail(argv[0], &myname);

    record_params_defaults(&record_params);
    parse_cmd_line(argc, argv, &record_params);

    /*
     * check record parameters for consistency
     */
    if ((record_params.compression != AF_COMPRESSION_NONE)
        && (record_params.filefmt == AF_FILE_AIFF))
    {
        fprintf(stderr,
                "%s: can't store compressed audio data in an AIFF file: use AIFF-C\n",
                myname);
        exit(1); 
    }
    if ((record_params.bits_per_samp != 16)
        && (record_params.compression != AF_COMPRESSION_NONE))
    {
        fprintf(stderr,
                "%s: compressor requires 16-bit sample data\n",
                myname);
        exit(1);
    }


    if ((record_params.frames_per_sec = get_input_rate()) < 0)
    {
        fprintf(stderr, 
                "%s: ERROR: cannot determine input rate [currently set to Digital]\n",
                myname);
        exit(1);
    }


    /*
     * check license for Aware encoder
     */
    switch (record_params.compression)
    {
     case AF_COMPRESSION_AWARE_DEFAULT_MULTIRATE:
        if (!quiet) {
            fprintf(stderr, "%s: checking license for Aware encoder ...",
                    myname);
        }
        AUchecklicense(AU_LICENSE_AWARE_MULTIRATE_ENCODER, &r, (char **)0);
        if (!quiet) {
            fprintf(stderr, " done\n");
        }
        if (r != AU_LICENSE_OK)
        {
            if (!quiet) {
                fprintf(stderr, 
                        "%s: license for Aware MultiRate encoder not available\n",
                        myname);
            }
            exit(1);
        }
        break; 
     default:
        break;
    }

    if (quiet)
    {
        ALseterrorhandler(quietALerror);
        AFseterrorhandler(quietAFerror);
    }
    else
    {
        ALseterrorhandler(defaultALerror);
        AFseterrorhandler(defaultAFerror);
    }

    /*
     * obtain a unix file descriptor for the input file
     */
    fd = open(filename, O_CREAT|O_TRUNC|O_WRONLY, 0644);
    if (fd < 0) {
        fprintf(stderr, "%s: could not create output file %s\n",
                myname, filename);
        exit(1);
    }

    getpathtail(filename, &filenametail);

    /*
     * initialize the audio port and audio file structures
     */
    audio_port_config = ALnewconfig();
    audio_file_setup  = AFnewfilesetup();


    AFinitfilefmt(audio_file_setup, record_params.filefmt);
    AFinitcompression(audio_file_setup, AF_DEFAULT_TRACK,
                      record_params.compression);

    switch (record_params.samps_per_frame) {
     case 1:
        ALsetchannels(audio_port_config, AL_MONO);
        AFinitchannels(audio_file_setup, AF_DEFAULT_TRACK, 1);
        break;
     default:
     case 2:
        ALsetchannels(audio_port_config, AL_STEREO);
        AFinitchannels(audio_file_setup, AF_DEFAULT_TRACK, 2);
        break;
     case 4:
        ALsetchannels(audio_port_config, AL_4CHANNEL);
        AFinitchannels(audio_file_setup, AF_DEFAULT_TRACK, 4);
        break;
    }
    switch (record_params.bits_per_samp) {
     case 8:
        ALsetsampfmt(audio_port_config, AL_SAMPFMT_TWOSCOMP);
        ALsetwidth(audio_port_config, AL_SAMPLE_8);
        AFinitsampfmt(audio_file_setup, AF_DEFAULT_TRACK, 
                      AF_SAMPFMT_TWOSCOMP, 8);
        break;
     default:
     case 16:
        ALsetsampfmt(audio_port_config, AL_SAMPFMT_TWOSCOMP);
        ALsetwidth(audio_port_config, AL_SAMPLE_16);
        AFinitsampfmt(audio_file_setup, AF_DEFAULT_TRACK, 
                      AF_SAMPFMT_TWOSCOMP, 16);
        break;
     case 24:
        ALsetsampfmt(audio_port_config, AL_SAMPFMT_TWOSCOMP);
        ALsetwidth(audio_port_config, AL_SAMPLE_24);
        AFinitsampfmt(audio_file_setup, AF_DEFAULT_TRACK,
                      AF_SAMPFMT_TWOSCOMP, 24); 
        break;
    }
    AFinitrate(audio_file_setup, AF_DEFAULT_TRACK, 
               (double)record_params.frames_per_sec);

    if ((audio_file = AFopenfd(fd , "w", audio_file_setup)) 
        == AF_NULL_FILEHANDLE) {
        fprintf(stderr, "%s: failed to open audio file %s\n", 
                myname, filename);
        exit(1);
    }

    caught_sigint = 0;
    sigset(SIGINT, on_sigint);
    audio_port      = ALopenport("recordaiff", "r", audio_port_config);
    record_audio(audio_port, audio_file, &record_params);

    /*
     * clean up  
     */
    if ((n =  AFclosefile(audio_file)) < 0)
    {
        fprintf(stderr, "%s: failed to update and close %s\n", 
                myname, filename);
        exit(1);
    }
    AFfreefilesetup(audio_file_setup);
    ALcloseport(audio_port);
    ALfreeconfig(audio_port_config);

    exit(0);
}



/*
 * R E C O R D _ P A R A M S _ D E F A U L T S
 */
    static void 
record_params_defaults(record_params_t *record_params)
{
    long pvbuf[2];
    long pvbuflen;

    if (!strncmp(myname, recordaiff, 10))
    {
        record_params->filefmt = AF_FILE_AIFF;
    }
    else
    {
        record_params->filefmt = AF_FILE_AIFFC;
    }

    record_params->time_limit_specified = 0;
    record_params->time_limit = 0.0;  

    record_params->samps_per_frame = 2;
    record_params->bits_per_samp   = 16;
    record_params->frames_per_sec = get_input_rate();
    record_params->compression = AF_COMPRESSION_NONE;
}

/*
 * P A R S E _ C M D _ L I N E
 */
    static void
parse_cmd_line(int argc, char **argv, record_params_t *record_params)
{
    int i,j,n,l;
    char *s;
    long pvlen;
    long pvbuf[2];
    int filefmt_seen  = 0;
    int rate_seen     = 0; 
    int width_seen    = 0;
    int channels_seen = 0;
    int time_seen     = 0;
    int compression_seen    = 0;

    verbose = 0;
    quiet   = 0;

    if (argc < 2) 
    {
        fprintf(stderr, "Usage: %s [options] filename %s\n", myname, options);
        exit(1);
    }

    for (i=1; i<argc; i++)
    {
        s = argv[i];
        if (!strncmp(s, "-f", 2)) /* file format */
        {
            filefmt_seen++;
            i++;
            s = argv[i];
            l = strlen(s);
            if (l<4) 
            {
                fprintf(stderr, "%s: invalid file format %s\n", myname, s);
                exit(1);
            }
            if  (!strcasecmp(s, "aiff"))
            {
                record_params->filefmt = AF_FILE_AIFF;
            }
            else if (!strcasecmp(s, "aifc") 
                     || !strcasecmp(s, "aiff-c")
                     || !strcasecmp(s, "aiffc"))
            {
                record_params->filefmt = AF_FILE_AIFFC;
            }
        }
        else if (!strncmp(s, "-n", 2)) /* samps_per_frame */
        {
            channels_seen++;
            i++;
            s = argv[i];
            if (!isdigit(s[0]))
            {
                fprintf(stderr, "Usage: %s [options] filename %s\n", myname, 
                        options);
                exit(1);
            }
            record_params->samps_per_frame = atoi(s);
            if ((record_params->samps_per_frame != 1) 
                && (record_params->samps_per_frame != 2)
                && (record_params->samps_per_frame != 4))
            {
                fprintf(stderr, "%s: invalid number of channels %d\n",
                        myname, record_params->samps_per_frame);
                exit(1);
            }
        }
        else if (!strncmp(s, "-r", 2)) /* sample rate */
        {
            rate_seen++;
            i++;
            s = argv[i];
            if (!isdigit(s[0]))
            {
                fprintf(stderr, "Usage: %s [options] filename %s\n", myname, 
                        options);
                exit(1);
            }

            record_params->frames_per_sec = atoi(s);
            if (record_params->frames_per_sec < 0) {
                fprintf(stderr, "%s: invalid sample rate %d\n",
                        myname, record_params->frames_per_sec);
                exit(1);
            }
        }
        else if (!strncmp(s, "-s", 2)) /* sample format */
        {
            width_seen++;
            i++;
            s = argv[i];
            if (!isdigit(s[0]))
            {
                fprintf(stderr, "Usage: %s [options] filename %s\n", myname, 
                        options);
                exit(1);
            }
            record_params->bits_per_samp = atoi(s);
            if ((record_params->bits_per_samp != 8)
                && (record_params->bits_per_samp != 16)
                && (record_params->bits_per_samp != 24))
            {
                fprintf(stderr, "%s: invalid sample width %d\n",
                        myname, record_params->bits_per_samp);
                exit(1);
            }
        }
        else if (!strncmp(s, "-c", 2)) /* compression */
        {
            compression_seen++;
            i++;
            s=argv[i];

            if (!strcmp(s, "g722"))
            {
                record_params->compression = AF_COMPRESSION_G722;
            }
            else if (!strcmp(s, "ulaw"))
            {
                record_params->compression = AF_COMPRESSION_G711_ULAW;
            }
            else if (!strcmp(s, "alaw"))
            {
                record_params->compression = AF_COMPRESSION_G711_ALAW;
            }
#ifdef AWARE
            else if (!strcmp(s, "awmulti"))
            {
                record_params->compression 
                    = AF_COMPRESSION_AWARE_DEFAULT_MULTIRATE;
            }
#endif                          /*AWARE*/
            else
            {
                fprintf(stderr, "%s: invalid compression %s\n",
                        myname, s);
                exit(1);
            }
        }
        else if (!strncmp(s, "-t", 2)) /* length of recording session*/
        {
            time_seen++;
            i++;
            s = argv[i];
            if (!isdigit(s[0]) && (s[0] != '.'))
            {
                fprintf(stderr, "Usage: %s [options] filename %s\n", myname, 
                        options);
                exit(1);
            }
            record_params->time_limit_specified = 1;
            record_params->time_limit = (double)atof(s);

            if (record_params->time_limit < 0)
            { 
                fprintf(stderr, "%s: invalid time limit %f seconds\n",
                        myname, record_params->time_limit);
                exit(1);
            }
        }
        else if (!strncmp(s, "-v", 2)) /* verbose */
        {
            verbose = 1;
        }
        else if (!strncmp(s, "-q", 2)) /* quiet */
        {
            quiet = 1;
        }
        else if (s[0] == '-')   /* help */
        {
            fprintf(stderr, "Usage: %s [options] filename %s\n", myname, 
                    options);
            exit(1);
        }
        else
        {
            filename = argv[i];
            break;
        }
    }                           /* for */

    if (verbose) quiet = 0;

    /*
     * we have to change the hardware input sample rate setting
     */
    if (rate_seen)
    {
        /*
         *  we set the rate to our value in Hz.  ALsetparams will set
	 *  the hardware to the closest rate to audio_rate which the
	 *  hardware supports.
	 *
	 *  we will later call ALgetparams to get the actual rate we
	 *  set, and use that to fill in the file.
         */
        pvbuf[0] = AL_INPUT_RATE;
        pvbuf[1] = record_params->frames_per_sec;
        pvlen = 2;
        ALsetparams(AL_DEFAULT_DEVICE, pvbuf, pvlen);
        /*hardware is set to nearest capable rate ... get it*/
        record_params->frames_per_sec = get_input_rate();               
    }
}                               /* parse_cmd_line */

/*
 * O N _ S I G I N T
 */
    static void
on_sigint()
{
    caught_sigint = 1;
}

/*
 * R E C O R D _ A U D I O 
 */
    static void
record_audio(ALport audio_port, AFfilehandle audio_file,
                 record_params_t *record_params)
{
    int nsamps;
    int nframes;
    char *buf;
    int samps_per_buf;
    int bytes_per_buf;
    int frames_per_buf;
    double frames_per_sec;
    int bytes_per_samp;
    double secs_per_frame;
    double secs_per_buf;
    double sec_count;
    int samps_per_frame;
    int i;
    int frame_limit;
    int num_bufs;
    int leftover_samps;
    int leftover_frames;
    int leftover_bytes;
    int done = 0;
    long bits_per_samp;
    long total_samp_bytes;
    long total_frames;
    long samp_type;
    int tmp;
    char compressionname[20];
    char    byteUnitsString[100], durationString[100];

    samps_per_frame = AFgetchannels(audio_file, AF_DEFAULT_TRACK);
    AFgetsampfmt(audio_file, AF_DEFAULT_TRACK, &samp_type, &bits_per_samp);
    if (bits_per_samp <= 8) {
        bytes_per_samp = 1;
    }
    else if (bits_per_samp <= 16) {
        bytes_per_samp = 2;
    }
    else {
        bytes_per_samp = 4;
    }
    frames_per_sec  = AFgetrate(audio_file, AF_DEFAULT_TRACK);

    secs_per_frame  = 1.0/frames_per_sec;
    frames_per_buf  = frames_per_sec / 2;
    samps_per_buf   = frames_per_buf * samps_per_frame;
    bytes_per_buf   = samps_per_buf * bytes_per_samp;
    secs_per_buf    = secs_per_frame * frames_per_buf;

    buf          = (char *)malloc(bytes_per_buf);


    /*
     * we assume the file pointer now points to the beginning
     * of the audio sample section of the AIFF file
     */ 
    if (!record_params->time_limit_specified)
    {
        total_samp_bytes = 0;
        total_frames     = 0;
        sec_count        = 0.0;
        done             = 0;

        if (!quiet)
        {
            switch (record_params->compression)
            {
             default:
             case AF_COMPRESSION_NONE:
                strcpy(compressionname, "");
                break;
             case AF_COMPRESSION_G722:
                strcpy(compressionname, "--> G.722");
                break;
             case AF_COMPRESSION_G711_ALAW:
                strcpy(compressionname, "--> A-law");
                break;
             case AF_COMPRESSION_G711_ULAW:
                strcpy(compressionname, "--> u-law");
                break;
#ifdef AWARE
             case AF_COMPRESSION_AWARE_DEFAULT_MULTIRATE:
                strcpy(compressionname, "--> Aware MultiRate");
                break;
#endif                          /*AWARE*/
            }
            printf("%s: '%s' %g Hz %6s %d-bit %s %s\n",
                   myname,
                   filenametail,
                   frames_per_sec,
                   samps_per_frame == 1 ? "mono" : 
                   (samps_per_frame == 2 ? "stereo" : "4-channel"),
                   record_params->bits_per_samp,
                   compressionname,
                   record_params->filefmt == AF_FILE_AIFFC ? "AIFF-C" : "AIFF"
                   );
        }

#ifdef THIS_IS_BROKEN_REVISIT_AFTER_5_1
        /* 
         * set non-degrading priority
         * use high priority if running as root
         * would be better to use DEADLINE scheduling, but, hey, it's a start
         */
    
        if (schedctl(NDPRI, 0, NDPNORMMAX)!=-1) 
        {
            /* we are root, use real exploitation */
            schedctl(SLICE, 0, CLK_TCK);  
        }
        else
        {
            if (verbose) fprintf(stderr, 
                                 "%s: Run as root for higher process priority\n", 
                                 myname);
            /* find lowest user non-degrading priority, ...
             * this value is one more than ndpri_hilim which can be set with 
             * systune(3), but cannot be determined by a system call, hence
             * the for() loop.
             */
            for (i = NDPLOMAX+1; schedctl(NDPRI, 0, i) != -1; i--) ;
        }
#else
    if (schedctl(NDPRI, 0, NDPNORMMAX)< 0) {
        if (verbose) fprintf(stderr,
                             "%s: Run as root for higher process priority\n",
                             myname);
    }
#endif
           
        while (!done)
        {
            ALreadsamps(audio_port, buf, samps_per_buf);
            if ((nframes = AFwriteframes(audio_file, AF_DEFAULT_TRACK, 
                                         buf, frames_per_buf)) != frames_per_buf)
            {
                fprintf(stderr, 
                        "%s:  tried to write %d frames to %s, wrote only %d\n",
                        myname, frames_per_buf, filename, nframes);
                total_frames     += nframes;
                sec_count        += total_frames * secs_per_frame;
                total_samp_bytes += nframes * samps_per_frame * bytes_per_samp;
                done++;
            }
            else 
            {
                total_frames     += frames_per_buf;
                total_samp_bytes += bytes_per_buf;
                sec_count += secs_per_buf;
            }

            if (verbose)
            {
	    Duration(sec_count, durationString);
	    SizeInBytes(total_samp_bytes, byteUnitsString);

                fprintf(stderr,"%s: %d sample frames %s %s\n",
                        myname, total_frames, durationString, byteUnitsString);
            }
            if (caught_sigint)
            {
                done++;
            }
        }                       /* while */
        /*
         * drain the audio data that has accumulated in the audio port 
         * since the last time we checked 
         */ 
        nsamps   = ALgetfilled(audio_port);
        nframes  = nsamps / samps_per_frame;
        num_bufs = nframes / frames_per_buf;
        leftover_frames = nframes - (num_bufs * frames_per_buf);
        leftover_samps  = leftover_frames * samps_per_frame;

        if (!quiet && (num_bufs > 0))
        {
            fprintf(stderr, "%s: draining the audio input port ...\n",
                    myname);
        }

        while (num_bufs)
        {
            ALreadsamps(audio_port, buf, samps_per_buf);
            if ((nframes = AFwriteframes(audio_file, AF_DEFAULT_TRACK, 
                                         buf, frames_per_buf)) != frames_per_buf)
            {
                fprintf(stderr, 
                        "%s:  tried to write %d frames to %s, wrote only %d\n",
                        myname, frames_per_buf, filename, nframes);
                total_frames     += nframes;
                sec_count        += total_frames * secs_per_frame;
                total_samp_bytes += nframes * samps_per_frame * bytes_per_samp;
                num_bufs = 0;
            }
            else 
            {
                total_frames     += frames_per_buf;
                total_samp_bytes += bytes_per_buf;
                sec_count += secs_per_buf;
                num_bufs--;
            }
            if (verbose)
            {
	    Duration(sec_count, durationString);
	    SizeInBytes(total_samp_bytes, byteUnitsString);

                fprintf(stderr,"%s: %d sample frames %s %s\n",
                        myname, total_frames, durationString, byteUnitsString);
            }
        }
        ALreadsamps(audio_port, buf, leftover_samps);
        if ((nframes = AFwriteframes(audio_file, AF_DEFAULT_TRACK, 
                                     buf, leftover_frames)) != leftover_frames)
        {
            fprintf(stderr, 
                    "%s:  tried to write %d frames to %s, wrote only %d\n",
                    myname, leftover_frames, filename, nframes);
            total_frames     += nframes;
            sec_count        += total_frames * secs_per_frame;
            total_samp_bytes += nframes * samps_per_frame * bytes_per_samp;
        }
        else 
        {
            total_frames     += leftover_frames;
            total_samp_bytes 
                += (leftover_frames * bytes_per_samp * samps_per_frame);
        }
        if (verbose)
        {
            sec_count += leftover_frames * secs_per_frame;
 	    Duration(sec_count, durationString);
	    SizeInBytes(total_samp_bytes, byteUnitsString);

                fprintf(stderr,"%s: %d sample frames %s %s\n",
                        myname, total_frames, durationString, byteUnitsString);
        }

    }                           /* no time limit specified */

    else                        /* user has specified a time limit */
    {
        total_samp_bytes = 0;
        total_frames     = 0;
        sec_count        = 0.0;
        done             = 0;

        /*
         * translate time (in seconds) to number of sample frames
         */
        frame_limit     = (int)(record_params->time_limit * frames_per_sec);
        num_bufs        = frame_limit / frames_per_buf;
        leftover_frames = frame_limit - (num_bufs * frames_per_buf);
        leftover_samps  = leftover_frames * samps_per_frame;
        leftover_bytes  = leftover_samps * bytes_per_samp;
        sec_count       = 0.0;
        done            = 0;

        if (!quiet)
        {
            switch (record_params->compression)
            {
             default:
             case AF_COMPRESSION_NONE:
                strcpy(compressionname, "");
                break;
             case AF_COMPRESSION_G722:
                strcpy(compressionname, "G.722");
                break;
             case AF_COMPRESSION_G711_ALAW:
                strcpy(compressionname, "A-law");
                break;
             case AF_COMPRESSION_G711_ULAW:
                strcpy(compressionname, "u-law");
                break;
            }
            printf("%s: '%s' %6.3f sec %7.1f Hz %6s %d-bit %s %s\n",
                   myname,
                   filenametail,
                   record_params->time_limit,
                   frames_per_sec,
                   samps_per_frame == 1 ? "mono" : 
                   (samps_per_frame == 2 ? "stereo" : "4-channel"),
                   record_params->bits_per_samp,
                   compressionname,
                   record_params->filefmt == AF_FILE_AIFFC ? "AIFF-C" : "AIFF"
                   );
        }

        /*
         * write blocks of samples
         */
        for (i=0; (i<num_bufs) && (!done); i++)
        {
            ALreadsamps(audio_port, buf, samps_per_buf);
            if ((nframes = AFwriteframes(audio_file, AF_DEFAULT_TRACK, 
                                         buf, frames_per_buf)) != frames_per_buf)
            {
                fprintf(stderr, 
                        "%s:  tried to write %d samps to %s, actually wrote %d\n",
                        myname, samps_per_buf, filename, nsamps);

                total_frames     += nframes;
                sec_count        += total_frames * secs_per_frame;
                done++;
            }
            else 
            {
                total_frames     += frames_per_buf;
                total_samp_bytes += bytes_per_buf;
                sec_count        += secs_per_buf;
            }
            if (verbose)
            {
	    Duration(sec_count, durationString);
	    SizeInBytes(total_samp_bytes, byteUnitsString);

                fprintf(stderr,"%s: %d sample frames %s %s\n",
                        myname, total_frames, durationString, byteUnitsString);
            }
            if (caught_sigint)
            {
                done++;
            }
        }
        /*
         * write the leftover samples
         */
        if (!done)
        {
            ALreadsamps(audio_port, buf, leftover_samps);
            if ((nframes = AFwriteframes(audio_file, AF_DEFAULT_TRACK, 
                                         buf, leftover_frames)) != leftover_frames)
            {
                fprintf(stderr, 
                        "%s:  tried to write %d frames to %s, wrote only %d\n",
                        myname, leftover_frames, filename, nframes);
            }
            total_frames     += nframes;
            total_samp_bytes += nframes * samps_per_frame * bytes_per_samp;
            if (verbose)
            {
                sec_count += leftover_frames * secs_per_frame; 

	    Duration(sec_count, durationString);
	    SizeInBytes(total_samp_bytes, byteUnitsString);

                fprintf(stderr,"%s: %d sample frames %s %s\n",
                        myname, total_frames, durationString, byteUnitsString);
            }
        }
    }
}

/*
 * returns tail part of a file's pathname
 */
    static void
getpathtail(char *path, char **pathtail)
{
    char *p;

    p = strrchr(path, '/');
    if (p) {
        p++;
    }
    else {
        p = path;
    }

    *pathtail = (char *)malloc(strlen(p) + 1);
    strcpy(*pathtail, p);
}

/*
 * application-defined silent error reporting routine
 * for Audio Library
 */
    static void
quietALerror(long code, const char *desc, ...)
{
}

/*
 * application-defined silent error reporting routine
 * for Audio File Library
 */
    static void
quietAFerror(long code, const char *desc)
{
}

/*
 * default application-defined error reporting routine
 * for Audio Library
 */
    static void
defaultALerror(long code, const char *desc, ...)
{
    fprintf(stderr, "%s (Audio Library error %d): %s\n",
            myname, code, desc);
}

/*
 * default application-defined error reporting routine
 * for Audio Library
 */
    static void
defaultAFerror(long code, const char *desc)
{
    fprintf(stderr, "%s (Audio File Library error %d): %s\n",
            myname, code, desc);
}

/* ******************************************************************
 *  Duration	Convert to format hours:minutes:seconds
 * ****************************************************************** */
    static void
Duration(double seconds, char *outputString)
{
int	hours, minutes, wholeSeconds;
double	subSeconds;
char	s[100], s2[50];

if (seconds < 0)
    {
    printf("Duration(): bogus seconds: %g\n", seconds);
    return;
    }

/* compute hours, minutes and seconds */
wholeSeconds = (int) seconds;
minutes = (wholeSeconds/60)%60;
hours = wholeSeconds/3600;
subSeconds = seconds - ((double) wholeSeconds);
wholeSeconds %= 60;

/* print minimal info:  if no hours, no space for hours */
if (hours == 0)
    {
    if (minutes == 0)
	{
	sprintf(outputString, "%.1f second", seconds);
    /* pluralize units */
	if (seconds != 1)
	    strcat(outputString, "s");
	}
    else
	{
	sprintf(outputString, "%d:%.2d minutes", minutes, wholeSeconds);
	}
    }
else
    {
    sprintf(outputString, "%d:%.2d:%.2d", hours, minutes, wholeSeconds);
    if (subSeconds != 0)
	{
	sprintf(s2, ".%g", subSeconds);
	strcat(outputString, s2+2);
	}
    strcat(outputString, " hours");
    }
} /*------------------- end Duration() --------------- */

/* ******************************************************************
 *  SizeInBytes	    Convert to bytes with metric system prefixes
 * ****************************************************************** */
    static void
SizeInBytes(int totalBytes, char *outputString)
{
char	s[50];
double	bytes;

if (totalBytes < 0)
    {
    printf("SizeInBytes(): bogus bytes: %g\n", bytes);
    return;
    }

/* print size with 3 significant digits in range */
bytes = (double) totalBytes;
/* units will be in giga bytes */
if	(totalBytes > 1000000000)
    {
    bytes /= 1024*1024*1024;
    s[0] = 'G'; s[1] = '\0';
    }
/* units will be in mega bytes */
else if (totalBytes > 1000000)
    {
    bytes /= 1204*1024;
    s[0] = 'M'; s[1] = '\0';
    }
/* units will be in kilo bytes */
else if (totalBytes > 1000)
    {
    bytes /= 1024;
    s[0] = 'K'; s[1] = '\0';
    }
/* units will be in bytes */
else 
    s[0] = '\0';

sprintf(outputString, "%.2f %sByte", bytes, s);
if (bytes != 1)
    strcat(outputString, "s");
} /*------------------- end SizeInBytes() --------------- */
