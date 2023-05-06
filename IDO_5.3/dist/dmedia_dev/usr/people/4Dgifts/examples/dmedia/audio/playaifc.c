/*
 * playaifc (playaiff):
 *     4Dgifts 'playaiff' program ported to IRIS Audio File Library
 *     scott porter august 91
 *
 * notes:
 *     this program will now play both AIFF and AIFF-C files
 *     real-time G.722 / G.711 decompression for AIFF-C via audio file 
 *        library
 *     G.722 real-time decompression works best if this program is run
 *         with nondegrading high priority (npri -h), especially for 
 *         stereo data
 *     this program now plays back AIFF-C files which contain audio data 
 *         encoded with Aware MultiRate or MPEG compression algorithms
 *     this program now handles 4-channel AIFF/AIFF-C files 
 *
 * usage:
 *     playaifc [options] filename(s)
 *     playaiff [options] filename(s)
 *
 * command line options:
 *     q = quiet:    suppress file descriptions, warning and error messages
 *     v = verbose:  print additional information
 *     r = rude:     modify global IRIS Audio Processor output rate as 
 *                   necessary in order to play files
 *     p = polite:   don't be rude - see 'nonrude' below
 *
 * defaults:
 *    playaiff assumes 'r' option, in order to maintain backward compatibility
 *       with earlier releases
 *    playaifc defaults to 'nonrude' behavior
 *
 *    'nonrude' behavior is defined as follows: before modifying global
 *        output sample rate in the IRIS Audio Processor, first check to
 *        whether any other output ports are currently active; if any other
 *        processes have open output ports, don't modify the output rate
 *    
 *    both programs emit one line descriptions for each file as it is played
 *    these descriptions are turned off by the 's' option
 */
#ident "$Revision: 1.49 $"

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <malloc.h>
#include <math.h>
#include <string.h>
#include <dmedia/audio.h>
#include <dmedia/audiofile.h>
#include <getopt.h>
#include <limits.h>
#include <sys/prctl.h>
#include <sys/schedctl.h>

/*
 * local subroutines
 */
static int  InitAudio(AFfilehandle, ALport *);
static int  PlaySamples(AFfilehandle , ALport);
static void GetPathTail(char *, char **);
static void DefaultAFerror(long, const char *);
static void QuietAFerror(long, const char *);
static void Duration(double seconds, char *outputString);

/*
 * globals 
 */
static char	*filename;		/* input file name */
static char	*filenametail;		/* input file name tail */
static long	filefmt;		/* input file format */
static int	fd;			/* input file descriptor */
static char	*myname;		/* name of this program */
static int	verbose, rude, quiet;	/* global flags */
static int	bytes_per_samp;		/* sample width */
static long	bits_per_samp;		/* sample resolution */
static int	samps_per_frame;	/* frame size */
static int	frames_per_sec;		/* sample rate */
static int	bytes_per_buf;		/* bytes / sample buffer */
static int	samps_per_buf;		/* samples / sample buffer */
static int	frames_per_buf;		/* frames / sample buffer */
static double	secs_per_frame;		/* time to play one audio frame */
static double	secs_per_buf;		/* time to play one sample buffer */
static char	*sampbuf;		/* sample buffer */
static long	compression;		/* audio data compression type */
static double	file_rate;		/* audio file sample rate */

int GetOutputRate();

/* ******************************************************************
 *  usage:	print usage into shell
 * ****************************************************************** */
    static void 
usage()
{
    static int alreadyhere = 0;

    if (alreadyhere) return;
    alreadyhere++;
    fprintf(stderr, 
"Usage: %s [-hqvrp] file1 [file2 ...]\n\
\t-h\thelp with a usage message\n\
\t-q\tquietly suppress error messages\n\
\t-v\tverbosely print info to stdout, implies !quiet\n\
\t-r\trudely change output rate despite other's audio\n\
\t-p\tpolitely don't change sample rate if other ports are in use\n",
            myname);
} /* --------------------- end usage() --------------- */

/* ******************************************************************
 *  main:	
 * ****************************************************************** */
main(int argc, char **argv)
{
AFfilehandle audio_file;
ALport audio_port;
int errseen;
int result;
int c;
int i;
extern char *optarg;
extern int   optind;

/*
 * swap effective & real userIDs.
 * this allows us to become super-user when we wish,  but run
 * as "joe user" for the rest of the time.
 */
setreuid(geteuid(), getuid());
setregid(getegid(), getgid());


GetPathTail(argv[0], &myname);

rude    = 0;
quiet  = 0;
verbose = 0;

/*
 * maintain backward compatability with playaiff
 */    
if (strcmp(myname, "playaiff") == 0) 
{
    rude   = 1;
    quiet = 1;
}

while ((c = getopt(argc, argv, "qvrph")) != -1)
{
    switch(c)
    {
     case 'q': quiet   = 1; break;
     case 'v': verbose = 1; break;
     case 'r': rude    = 1; break;
     case 'p': rude    = 0; break;
     case 'h': usage();     break;
     default:
	usage();
	exit(1);
    }
}

if (verbose) quiet = 0;
ALseterrorhandler(0);
if (quiet) {
    AFseterrorhandler(QuietAFerror);
}
else {
    AFseterrorhandler(DefaultAFerror);
}
errseen = 0;

if (argc - optind < 1) {
    usage();
    exit(1);
}


while (optind < argc)
{
    filename = argv[optind];
    GetPathTail(filename, &filenametail);
    
    /*
     * obtain a unix file descriptor for the input file
     */
    if ((fd = open(filename, O_RDONLY)) < 0)
    {
	fprintf(stderr, "%s: failed to open file '%s' %s\n",
		myname, filename, strerror(errno));
	errseen = 1;
    }
    /*
     * test the file descriptor to see whether we can attach an audio
     *    file handle to it
     */
    else if (AFidentifyfd(fd) < 0) 
    {
	fprintf(stderr,"%s: '%s' not an AIFF-C or AIFF file\n",
		myname, filename);
	errseen = 1;
    }
    /*
     * attach an audio file handle to the file descriptor
     */
    else if ((audio_file = 
	      AFopenfd(fd, "r", AF_NULL_FILESETUP)) == AF_NULL_FILEHANDLE)
    {
	if (!quiet)
	{
	    fprintf(stderr, "%s: failed to open file '%s'\n", 
		    myname, filename);
	}
	errseen = 1;
    }
    else 
    {
	result = InitAudio(audio_file, &audio_port);
	if (result != -1)
	{
	    PlaySamples(audio_file, audio_port);
	}
	else {
	    errseen++;
	}
	AFclosefile(audio_file);
	if (result != -1) 
	{
	    ALcloseport(audio_port);
	}
	free(sampbuf);
    }
    optind++;
}
if (errseen) exit(1);
exit(0);
} /* --------------------- end main() --------------- */

/* ******************************************************************
 *  InitAudio():	initialize audio port and global state of IRIS Audio 
 *			Hardware
 * ****************************************************************** */
    static int 
InitAudio(AFfilehandle audio_file, ALport *audio_port)
{
    ALconfig audio_port_config;
    long pvbuf[4];
    long audio_rate, output_rate;
    long samp_type;
    long samp_wordsize;    
    long vers;
    int err;
    
    samps_per_frame   = AFgetchannels(audio_file, AF_DEFAULT_TRACK);
    file_rate         = AFgetrate(audio_file, AF_DEFAULT_TRACK);
    compression       = AFgetcompression(audio_file, AF_DEFAULT_TRACK);
    filefmt           = AFgetfilefmt(audio_file, &vers);
    
    AFgetsampfmt(audio_file, AF_DEFAULT_TRACK, &samp_type, &bits_per_samp);
    
    /*
     * need to determine whether audio is in use. if not, then we
     * can just go ahead and be "rude."
     */
    pvbuf[0] = AL_OUTPUT_COUNT;
    pvbuf[2] = AL_MONITOR_CTL;
    if (ALgetparams(AL_DEFAULT_DEVICE, pvbuf, 4) < 0) {
	if (oserror() == AL_BAD_DEVICE_ACCESS) {
	    fprintf(stderr,"%s: Can't play -- could not access audio hardware.\n",myname);
	    return -1;
	}
    }

    
    if ((pvbuf[1] == 0) && (pvbuf[3] == AL_MONITOR_OFF)) {
        rude = 1;
    }
    
    /* 
     * decide on output rate for the audio hardware
     */
    audio_rate = frames_per_sec = (long) file_rate;
    
    
    /*
     * determine current output rate
     */
    output_rate = GetOutputRate();

    /*
     * if the rates are the same, all is well. if not, then we need to proceed
     * in a either a "rude" or "polite" manner.
     */
    
    if (output_rate != audio_rate)
    {
        /*
         *  we set the rate to our value in Hz.  ALsetparams will set
	 *  the hardware to the closest rate to audio_rate which the
	 *  hardware supports.
         */

        if (rude)
        {
            output_rate = audio_rate;
            pvbuf[0] = AL_OUTPUT_RATE;
            pvbuf[1] = output_rate;
            ALsetparams(AL_DEFAULT_DEVICE, pvbuf, 2);
        }
        else                    /*polite*/
        {
            if (!quiet)
            {
                if (output_rate < 0) 
                    fprintf(stderr, 
                            "%s: '%s': Adjust output rate [currently set to an undetermined Digital rate]\n",
                            myname, filenametail);
                else 
                    fprintf(stderr,
                            "%s: '%s': Adjust output rate [currently %d Hz]\n",
                            myname, filenametail, output_rate);
            }
        }
    }
    
    
    /*
     * decide what size blocks of samples we should read from the
     * input file and pass to ALwritesamps
     */
    if (bits_per_samp <= 8)
    {
        bytes_per_samp = 1;
        samp_wordsize  = AL_SAMPLE_8;
    }
    else if (bits_per_samp <= 16)
    {
        bytes_per_samp = 2;
        samp_wordsize  = AL_SAMPLE_16;
    }
    else if (bits_per_samp <= 24)
    {
        bytes_per_samp = 4;
        samp_wordsize  = AL_SAMPLE_24;
    }
    else
    {
        if (!quiet)
        { 
            fprintf(stderr, "%s: %s: error can't play %d bit samples\n",
                    myname, filenametail, bits_per_samp);
        }
        return(-1);
    }
    
    if ((samps_per_frame != 1)&&(samps_per_frame!= 2)&&(samps_per_frame!=4))
    {
        if (!quiet)
        {
            fprintf(stderr, "%s: %s: error can't play %d channel sample data\n",
                    myname, filenametail, samps_per_frame);
        }
        return(-1);
    }
    
    /*
     * make the buffer large enough to hold 1/2 sec of audio frames
     * we add one to frames_per_sec before we divide in order to
     * correctly handle the 11025 case
     */
    secs_per_frame = 1.0 / ((double)frames_per_sec);
    frames_per_buf = (frames_per_sec+1)/2;
    
    samps_per_buf = frames_per_buf * samps_per_frame;
    bytes_per_buf = samps_per_buf * bytes_per_samp;
    secs_per_buf  = secs_per_frame * frames_per_buf;
    
    sampbuf = malloc(bytes_per_buf);
    
    /*
     * configure and open audio port
     */
    audio_port_config = ALnewconfig();
    ALsetwidth(audio_port_config, samp_wordsize);
    ALsetchannels(audio_port_config, samps_per_frame);
    
    /*
     * make the ring buffer large enough to hold 1 sec of audio samples
     */
    ALsetqueuesize(audio_port_config, samps_per_buf*2);
    *audio_port = ALopenport(myname, "w", audio_port_config);
    if (*audio_port == 0) {
	err = oserror();
	if (err == AL_BAD_NO_PORTS) {
	    fprintf(stderr,"%s: Can't play -- no audio ports available at the moment\n",myname);
	}
	else if (err == AL_BAD_OUT_OF_MEM) {
	    fprintf(stderr,"%s: Can't play -- not enough memory to open audio port\n",myname);
	}
	return -1;
    }
} /* --------------------- end InitAudio() --------------- */


/* ******************************************************************
 *  PlaySamples:	play audio sample data through output port
 * ****************************************************************** */
    static int
PlaySamples(AFfilehandle audio_file, ALport audio_port)
{
    int num_bufs;
    int leftover_bytes;
    int leftover_samps;
    int leftover_frames;
    int samp_count;
    int frame_count;
    double sec_count;
    int i;
    int bytes_read;
    int samples_read;
    int frames_read;
    int done;
    int total_frames;   
    int total_samps;
    int total_samp_bytes;
    float file_playingtime;
    char compressionname[10];
    float fileplayingtime;
    char    durationString[100];

    sec_count = 0.0;
    
    /*
     * figure out how many reads we have to do
     */
    total_frames    =  AFgetframecnt(audio_file, AF_DEFAULT_TRACK);
	if (total_frames > 0) {
    total_samps      =  total_frames * samps_per_frame;
    total_samp_bytes =  total_samps * bytes_per_samp;
    num_bufs         = total_samp_bytes / bytes_per_buf;
    leftover_bytes   = total_samp_bytes % bytes_per_buf;
    leftover_samps   = leftover_bytes / bytes_per_samp;
    leftover_frames  = leftover_samps / samps_per_frame;
  } else {
	/* this could be an undertermined length raw file, or mpeg file */
    num_bufs = -1;
  }
    GetPathTail(filename, &filenametail);
    if (!quiet)
    {
        fileplayingtime = (float)total_frames / (float)frames_per_sec;
        switch (compression)
        {
         default:
         case AF_COMPRESSION_NONE: 
            strcpy(compressionname, "");
            break;
         case AF_COMPRESSION_G722:
            strcpy(compressionname, "G.722 -->");
            break;
         case AF_COMPRESSION_G711_ALAW:
            strcpy(compressionname, "A-law -->");
            break;
         case AF_COMPRESSION_G711_ULAW:
            strcpy(compressionname, "u-law -->");
            break;
#ifdef AWARE
         case AF_COMPRESSION_AWARE_MPEG:
            strcpy(compressionname, "Aware MPEG -->");
            break;
         case AF_COMPRESSION_AWARE_MULTIRATE:
            strcpy(compressionname, "Aware MultiRate -->");
            break;
#endif                          /*AWARE*/
        }
        printf("%s: '%s' %6.3f sec %g Hz %6s %s %d-bit %s\n",
               myname,
               filenametail,
               fileplayingtime,
               (float) file_rate,
               samps_per_frame == 1 ? "mono" : 
               (samps_per_frame == 2 ? "stereo" : "4-channel"),
               compressionname,
               bits_per_samp,
               filefmt == AF_FILE_AIFFC ? "AIFF-C" : "AIFF"
               );
    }
    if (verbose) 
    {
        printf( "        total sample frames       = %d (%d bytes)\n",
                total_frames, total_samp_bytes);
        printf( "        play data using blocksize = %d sample frames\n",
                frames_per_buf);
        printf( "        total blocks              = %d (%d extra frames)\n",
                num_bufs, leftover_frames); 
    }
    
    
    /* 
     * set non-degrading priority
     * use high priority if running as root
     * would be better to use DEADLINE scheduling, but, hey, it's a start
     */
    
    /*
     * swap permissions so that we become root for a moment.
     */
    setreuid(geteuid(), getuid());
    setregid(getegid(), getgid());

#ifdef THIS_IS_BROKEN_REVISIT_AFTER_5_1
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

    /*
     * swap permissions back, now we're just "joe user"
     */
    setreuid(geteuid(), getuid());
    setregid(getegid(), getgid());

    
    /*
     * move the fileptr to the beginning of the sample data
     */
    AFseekframe(audio_file, AF_DEFAULT_TRACK, 0);
    
    /*
     * note that there may be some pad bytes following the valid samples -
     * for example, the sample data area may be padded so that the valid 
     * samples begin on a block boundary and the sample area ends on a block
     * boundary (where blocksize is specified by the user)
     */
    done        = 0;
    samp_count  = 0;
    frame_count = 0; 
    sec_count   = 0.0;
    
    for (i=0; num_bufs<0 || i<num_bufs; i++)
    {
        samp_count  += samps_per_buf;
        frame_count += frames_per_buf;
        sec_count   += secs_per_buf;
        
        if (verbose) 
	    {
	    Duration(sec_count, durationString);
            printf("\t%d sample frames %s\n",
                   frame_count, durationString); 
	    }
        if ((frames_read 
             = AFreadframes(audio_file, AF_DEFAULT_TRACK, 
                            sampbuf, frames_per_buf)) < frames_per_buf)
        {
            if (!quiet && num_bufs > 0)
            {
                fprintf(stderr, 
                        "%s: warning short read for %s: expected %d frames, got %d frames\n",
                        myname, filenametail, frames_per_buf, frames_read);
            }
            done++;
        }
        samples_read = frames_read * samps_per_frame;
        ALwritesamps(audio_port, sampbuf, samples_read);
        if (done)
        {
            /*
             * allow the audio buffer to drain
             */
            while(ALgetfilled(audio_port) > 0) {
                sginap(1);
            }
            sginap(10);
            return(0);
        }
    }
    /*
     * play the leftovers
     */
    samp_count  += leftover_samps;
    frame_count += leftover_frames;
    sec_count   += ((double)leftover_frames) * secs_per_frame;
    if (verbose && leftover_samps>0) 
	{
	    Duration(sec_count, durationString);
            printf("\t%d sample frames %s\n",
                   frame_count, durationString); 
	}
    if ((frames_read = 
         AFreadframes(audio_file, AF_DEFAULT_TRACK, sampbuf, 
                      leftover_frames)) < leftover_frames)
    {
        if (!quiet)
        { 
            fprintf(stderr, 
                    "%s: warning short read for %s: expected %d frames, got %d frames\n",
                    myname, filenametail, leftover_frames, frames_read);
        }
    }
    samples_read = frames_read * samps_per_frame;
    ALwritesamps(audio_port, sampbuf, samples_read);
    
    /*
     * allow the audio buffer to drain
     */
    while(ALgetfilled(audio_port) > 0) {
        sginap(1);
    }
    sginap(10);
    return(0);
} /* --------------------- end PlaySamples() --------------- */

/* ******************************************************************
 *  GetPathTail:	get tail of file's path name
 * ****************************************************************** */
    static void
GetPathTail(char *thepath, char **thetail)
{
    char *p;
    
    p = strrchr(thepath, '/');
    if (p) {
        p++;
    }
    else {
        p = thepath;
    }
    
    *thetail = (char *)malloc(strlen(p) + 1);
    strcpy(*thetail, p);
} /* --------------------- end GetPathTail() --------------- */

/* ******************************************************************
 *  QuietAFerror:	silent application-defined error reporting routine
 *			for Audio File Library
 * ****************************************************************** */
    static void
QuietAFerror(long code, const char *desc)
{
} /* --------------------- end QuietAFerror() --------------- */

/* ******************************************************************
 *  DefaultAFerror:	default application-defined error reporting routine
 *			for Audio Library
 * ****************************************************************** */
    static void
DefaultAFerror(long code, const char *desc)
{
    switch (code)
    {
#if 0
     case AF_BAD_CODEC_LICENSE:
        fprintf(stderr, "%s: license unavailable for Aware %s decoder\n",
                myname, 
                compression==AF_COMPRESSION_AWARE_MULTIRATE?"MultiRate":"MPEG");
        break;
#endif
     default:
        fprintf(stderr, "%s (Audio File Library error %d): %s\n",
                myname, code, desc);
        break;
    }
} /* --------------------- end DefaultAFerror() --------------- */

/* ******************************************************************
 *  GetInputRate:	return Audio Hardware input sampling rate
 * ****************************************************************** */
    int
GetInputRate()
{
long buf[6];

buf[0] = AL_INPUT_RATE;
buf[2] = AL_INPUT_SOURCE;
buf[4] = AL_DIGITAL_INPUT_RATE;
ALgetparams(AL_DEFAULT_DEVICE, buf, 6);

/* we are clocked off digital input. find real input rate, if possible. */
if	((buf[1] == AL_RATE_AES_1)||(buf[3] == AL_INPUT_DIGITAL)) 
    {
    if (ALgetdefault(AL_DEFAULT_DEVICE, AL_DIGITAL_INPUT_RATE) >= 0) 
	return (buf[5]);
    }
/* input rate is Hz and we're using an analog input */
else if (buf[1] > 0) 
    return (buf[1]);

return (AL_RATE_UNDEFINED);
} /* --------------------- end GetInputRate() --------------- */

/* ******************************************************************
 *  GetOutputRate:	return Audio Hardware output sampling rate
 * ****************************************************************** */
    int
GetOutputRate()
{
long buf[4];

buf[0] = AL_OUTPUT_RATE;
buf[2] = AL_DIGITAL_INPUT_RATE;
ALgetparams(AL_DEFAULT_DEVICE, buf, 4);

/* output rate is in Hz -- return it */
if (buf[1] > 0) 
    return (buf[1]);

/* output rate is logical -- track down what it means */
else 
    {
    if	    (buf[1] == AL_RATE_AES_1) 
	{
	/* we are clocked off of digital input. find
	 * real input rate, if system supports this ability.
	 * Otherwise, return AL_RATE_UNDEFINED */
	if (ALgetdefault(AL_DEFAULT_DEVICE,AL_DIGITAL_INPUT_RATE) >= 0) 
	    return (buf[3]);
	}
    else if (buf[1] == AL_RATE_INPUTRATE) 
	return (GetInputRate());
    return (AL_RATE_UNDEFINED);
    }
} /* --------------------- end GetOutputRate() --------------- */

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
