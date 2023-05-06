/*****************************************************************************
 *
 *     code to monitor audio input via direct copy to audio output
 *
 *****************************************************************************/
#include <stdio.h>
#include <fcntl.h>
#include <dmedia/audio.h>
#include <signal.h>
#include <limits.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/prctl.h>
#include <sys/schedctl.h>

static char usage[] = 
"\n\
[-help]          [-verbose]   [-rate R]    [-nchannels N]\n\
[-samplefmt S]   [-block B]   [-qsize Q]   [-errors]\n\
\n\
       nchannels  N = mono or stereo (1 2)\n\
       samplefmt  S = sample format (8 16 24)\n\
       rate       R = sample rate (48000 44100 32000 22050 16000 11025 8000)\n\
       block      B = transfer block size (in sample FRAMES)\n\
       qsize      Q = sample queue size for each audio port (in SAMPLES)\n";

static void ParseCommandLineOptions(int argc, char **argv);
static void OnSignalInterrupt();
static void PrintConfiguration();
static long GetInputRate();
static long GetOutputRate();

static ALport		iport;
static ALport		oport;
static ALconfig		theconfig;
static unsigned char	*buf;
static int		caught_sigint;
static int		verbose;
static int		error_report;
static char		*myname;
static long		qsize;
static long		nchannels;
static long		sampfmt;
static long		monitorRate;
static int		change_rate;
static int		samples_per_frame;
static int		bytes_per_sample;
static int		frames_per_transfer;
static long		new_rate;
static long		originalInputRate;
static long		originalOutputRate;

/* ******************************************************************
 *  main:    
 * ****************************************************************** */
main(int argc, char **argv)
{
int	    i;
long	    pvbuf[4];
long	    err_buf[10];
long	    err_buflen = 10;
long	    input_errors = 0; 
long	    output_errors = 0;
pid_t	    pid;
int	    result;
double	    tmpdbl;
int	    audioLibraryFileID;

/* Try to open Indigo style audio IO port.  On failure, machine has 
 * no audio ability: print message regarding absence 
    of audio hardware. Exit application */
audioLibraryFileID = open("/dev/hdsp/hdsp0master", O_RDONLY);
if (audioLibraryFileID < 0)
    {
    fprintf(stderr, "This machine has no audio abilities.\n"); 
    close(0);
    close(1);
    close(2);
    exit(1);
    }
close(audioLibraryFileID);

myname        = argv[0];
verbose       = 0;
change_rate   = 0;
caught_sigint = 0;
error_report  = 0;

/* disable Audio LIbrary error messages */
ALseterrorhandler(0);

/* defaults */
qsize                = 100000;
frames_per_transfer  = 50000;
sampfmt              = AL_SAMPLE_24;
nchannels            = AL_STEREO;

/* use current input sample rate for default */
originalInputRate  = GetInputRate();
originalOutputRate = GetOutputRate();
monitorRate = originalOutputRate;

/* parse command line for special requests */
ParseCommandLineOptions(argc, argv);

/* set interuupt handler */
signal(SIGINT, OnSignalInterrupt);

/* attempt to anchor process memory in main memory */
result = prctl(PR_RESIDENT);
if (result == -1)
    fprintf(stderr, "unable or permission denied to insure that process is memory-resident.\n"); 

/* attempt to set process priority.  Higher priorities reduce
likelyhood of audio dropouts */
pid = getpid();
result = schedctl(NDPRI, pid, NDPHIMIN);
if (result == -1)
    fprintf(stderr, "unable or permission denied to set process priority.\n");

/* set input and output rates */
if (change_rate)   
    {
    monitorRate = new_rate;
    pvbuf[0] = AL_INPUT_RATE;
    pvbuf[1] = new_rate;
    pvbuf[2] = AL_OUTPUT_RATE;
    pvbuf[3] = new_rate;
    ALsetparams(AL_DEFAULT_DEVICE, pvbuf, 4);
    }

/* determine sample format */
samples_per_frame = (nchannels == AL_MONO) ? 1 : 2;
switch (sampfmt)
    {
    case AL_SAMPLE_8:
	bytes_per_sample = 1;
    break;
    case AL_SAMPLE_16:
	bytes_per_sample = 2;
    break;
    case AL_SAMPLE_24:
    default:
	bytes_per_sample = 4;
    break;
    }


if (verbose)
    PrintConfiguration();

theconfig = ALnewconfig();
ALsetqueuesize(theconfig, qsize);
ALsetwidth(theconfig, sampfmt);
ALsetchannels(theconfig, nchannels);

/* open audio ports,  */
oport = ALopenport("the ouput port", "w", theconfig);
if (!oport)
    {
    fprintf(stderr,"%s: failed to open audio write port\n", myname);
    exit(1);
    }
iport = ALopenport("the input port", "r", theconfig);
if (!iport)
    {
    fprintf(stderr,"%s: failed to open audio read port\n", myname, i);
    exit(1);
    }

/* allocate memory */
buf = (void *) malloc(frames_per_transfer*bytes_per_sample*samples_per_frame);
if (buf == 0) 
    {
    fprintf(stderr,"%s: failed to malloc transfer buffer\n", myname);
    exit(1);
    } 

/* prepare error buffer for capturing nasty errors */
err_buf[0] = AL_ERROR_NUMBER;
err_buf[2] = AL_ERROR_TYPE;
err_buf[4] = AL_ERROR_LENGTH;
err_buf[6] = AL_ERROR_LOCATION_LSP;
err_buf[8] = AL_ERROR_LOCATION_MSP;

/*
 * main passthru loop
 */
while (!caught_sigint)
    {
        ALreadsamps(iport, buf, frames_per_transfer*samples_per_frame);
        ALwritesamps(oport, buf, frames_per_transfer*samples_per_frame);
	if (error_report)
	{
	    ALgetstatus(iport, err_buf, err_buflen);
            tmpdbl = (0xffffff & err_buf[9]) * 16777216.0 +
                              (0xffffff & err_buf[7]);

	    if (input_errors != err_buf[1])
	    {
	     input_errors = err_buf[1];
	     printf("\ninput stream discontinuities to date = %d\n\
        length of last discontinuity = %d sample frames\n\
        discontinuity location       = %.0f frames from stream beginning\n",
		    err_buf[1], err_buf[5], tmpdbl);
	    }
	    ALgetstatus(oport, err_buf, err_buflen);
            tmpdbl = (0xffffff & err_buf[9]) * 16777216.0 +
                              (0xffffff & err_buf[7]);
	    if (output_errors != err_buf[1])
	    {
	     output_errors = err_buf[1];
	     printf("\noutput stream discontinuities to date = %d\n\
        length of last discontinuity = %d sample frames\n\
        discontinuity location       = %.0f frames from stream beginning\n",
		err_buf[1], err_buf[5], tmpdbl);
	    }
	}
	
    } 

ALcloseport(iport);
ALcloseport(oport);
ALfreeconfig(theconfig);
exit(0);
} /* --------- end main() ---------- */

/* ******************************************************************
 *  OnSignalInterrupt:    
 * ****************************************************************** */
    static void
OnSignalInterrupt()
{
caught_sigint++;
} /* --------- end OnSignalInterrupt() ---------- */

/* ******************************************************************
 *  ParseCommandLineOptions:	    parse commnad line options  
 * ****************************************************************** */
    static void
ParseCommandLineOptions(int argc, char **argv)
{
int	i, j;
char	*s;
int	n;
int	temp, rate_seen;

rate_seen = 0;

for (i=1; i<argc; i++)
    {
        s = argv[i];

 /* queue size */
        if (strlen(s)>1 && !strncmp(s, "-q", 2))     
        {
            i++;
            s = argv[i];
            if (!isdigit(s[0]))
            {
                fprintf(stderr, "\nUsage: %s %s\n", myname, usage);
                exit(1);
            }
            qsize = atoi(s);
            if (qsize <= 0)
            {
                fprintf(stderr, "%s: invalid sample queue size %d\n", 
                          myname, qsize);
                exit(1);
            }
        }

 /* blocksize */
        else if (strlen(s)>1 && !strncmp(s, "-b", 2))   
        {
            i++;
            s = argv[i];
            if (!isdigit(s[0]))
            {
                fprintf(stderr, "\nUsage: %s %s\n", myname, usage);
                exit(1);
            }
            frames_per_transfer = atoi(s);
            if (frames_per_transfer <= 0)
            {
                fprintf(stderr, "%s: invalid blocksize %d\n",
                          myname, frames_per_transfer);
                exit(1);
            }
        }

/* channels */
        else if (strlen(s)>1 && !strncmp(s, "-n", 2))  
        {
            i++;
            s = argv[i];
            if (!isdigit(s[0]))
            {
                fprintf(stderr, "\nUsage: %s %s\n", myname, usage);
                exit(1);
            }
            temp = atoi(s);
            if ((temp != 1)  && (temp != 2))
            {
                fprintf(stderr, "%s: invalid number of channels %d\n",
                          myname, temp);
                exit(1);
            }
            nchannels = (temp == 1) ? AL_MONO : AL_STEREO;
        }

 /* rate */
        else if (strlen(s)>1 && !strncmp(s, "-r", 2))   
        {
            i++;
            s = argv[i];
            if (!isdigit(s[0]))
            {
                fprintf(stderr, "\nUsage: %s %s\n", myname, usage);
                exit(1);
            }
            temp = atoi(s);
            switch (temp)
            {
                case 48000: new_rate = AL_RATE_48000; break;
                case 44100: new_rate = AL_RATE_44100; break;
                case 32000: new_rate = AL_RATE_32000; break;
                case 22050: new_rate = AL_RATE_22050; break;
                case 16000: new_rate = AL_RATE_16000; break;
                case 11025: new_rate = AL_RATE_11025; break;
                case  8000: new_rate = AL_RATE_8000;  break;
                default:
                    fprintf(stderr, "%s: invalid sample rate %d\n",
                           myname, temp);
                    exit(1);
            }
            rate_seen++;
        }

/* sample format */
        else if (strlen(s)>1 && !strncmp(s, "-s", 2))
        {
            i++;
            s = argv[i];
            if (!strcmp(s, "8"))
            {
                sampfmt = AL_SAMPLE_8;
            }
            else if (!strcmp(s, "16"))
            {
                sampfmt = AL_SAMPLE_16;
            }
            else if (!strcmp(s, "24"))
            {
                sampfmt = AL_SAMPLE_24;
            }
            else
            {
                fprintf(stderr, "%s: invalid sample format %s\n",
                          myname, s);
                exit(1);
            }
        }

/* verbose */
        else if (!strncmp(s, "-v", 2))
        {
            verbose++;
        }

/* help */
        else if (!strncmp(s, "-h", 2))
        {
        printf("\n");
        printf("%s options:\n%s", myname, usage);
        printf("\n");
	exit(0);
        }

/* error report */
	else if (!strncmp(s, "-e", 2))
	{
	    error_report++;
	}
        else
        {
            fprintf(stderr, "\nUsage: %s %s\n", myname, usage);
            exit(1);
        }
    } /* for */

    /* check to see whether we need to change input/output rates */
    if (rate_seen)
	{
        if ((new_rate != originalInputRate)||(new_rate != originalOutputRate))
            change_rate++;
	}
    else if (originalInputRate != originalOutputRate)
        {
	change_rate++;
	new_rate = originalInputRate;
        }
} /* --------- end ParseCommandLineOptions() ---------- */

/* ******************************************************************
 *  PrintConfiguration:  print out monitor configuration parameters  
 * ****************************************************************** */
    static void 
PrintConfiguration()
{
printf("\n");
printf("%s configuration:\n\n", myname);

printf("channels             = %d\n", nchannels);
switch (sampfmt)
    {
    case AL_SAMPLE_8:
	 printf("sample fmt           = 8-bit\n");
    break;
    case AL_SAMPLE_16:
	 printf("sample fmt           = 16-bit\n");
    break;
    default:
    case AL_SAMPLE_24:
	 printf("sampfmt              = 24-bit\n");
    break;
    } 

switch (monitorRate)
    {
    case AL_RATE_48000:
    case AL_RATE_44100:
    case AL_RATE_32000:
    case AL_RATE_22050:
    case AL_RATE_16000:
    case AL_RATE_11025:
    case AL_RATE_8000:
	 printf("sample rate          = %d Hz\n", monitorRate);
    break;
    default:
	 printf("sample rate          = undetermined rate from digital audio input\n");
    break;
    } 

printf("queue size           = %d samples\n", qsize);
printf("bytes per sample     = %d bytes\n", bytes_per_sample);
printf("frames per transfer  = %d frames\n", frames_per_transfer);
printf("samples per frame    = %d samples\n", samples_per_frame); 
printf("\n");
} /* --------- end PrintConfiguration() ---------- */

/* ******************************************************************
 *  GetInputRate:	return Audio Hardware input sampling rate
 * ****************************************************************** */
    long
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
    long
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
