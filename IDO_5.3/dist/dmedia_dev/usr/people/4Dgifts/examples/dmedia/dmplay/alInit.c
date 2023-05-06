/*
 * alInit.c: Does libaudio (audio library) initializations)
 *
 * 
 * Silicon Graphics Inc., June 1994
 */

#include "dmplay.h"

static void setAudioFrameRate (void);
static int AudioOutputHardwareInUse(double rate);
static long GetAudioHardwareOutputRate(void);
static long GetAudioHardwareInputRate(void);


void alInit ()
{
    int sampleWidth;    
    
    if (!audio.isTrack)
	return;

    /* initialize audio hardware output port */

    if ((audio.config = ALnewconfig()) == NULL) {
	fprintf (stderr, "Unable to initialise an ALconfig structure\n");
	exit (-1);
    }

    /* attempt to set audio hardware output sampling rate to file rate */
    setAudioFrameRate ();

    if (ALsetqueuesize(audio.config, audio.queueSize) == -1) {
	fprintf (stderr, "Unable to set Queue size for the audio port.\n");
	exit (-1);
    }
    if (audio.sampleWidth <= 8) {
	sampleWidth = AL_SAMPLE_8;
    } else if (audio.sampleWidth <= 16) {
	sampleWidth = AL_SAMPLE_16;
    } else {
	sampleWidth = AL_SAMPLE_24;
    }
    if (ALsetwidth(audio.config, sampleWidth)) {
	fprintf (stderr, "Unable to set audio sample width.\n");
	exit (-1);
    }
    
    if (ALsetchannels(audio.config, audio.channelCount)) {
	fprintf (stderr, "Unable to set number of audio channels\n.");
	exit (-1);
    }
    if ((audio.outPort = ALopenport("dmplay", "w", audio.config)) == NULL) {
	printf("Sorry, no audio output.\n");
    }    

}


/*** All the routines below are for setting audio frame rate ***/

void setAudioFrameRate ()
{
    int audioHardwareInUse;
#ifdef RUDE_AUDIO
    int doNotRudelyChangeSamplingRate = 0;
#else  
    int doNotRudelyChangeSamplingRate = 1;
#endif /*RUDE_AUDIO*/
    long currentHardwareRate;
    long buffer[2];
    
    /* turn off AL error handler during first half of this funtion */
    ALerrfunc oldErrorHandler = ALseterrorhandler(NULL);


    currentHardwareRate = GetAudioHardwareOutputRate();
    if (currentHardwareRate != audio.frameRate)
	{
	    audioHardwareInUse = AudioOutputHardwareInUse(audio.frameRate);
	    /* if no one else is using audio output hardware, change sampling rate */
	    if ((!audioHardwareInUse)||
		(!doNotRudelyChangeSamplingRate))
		{
		    buffer[0] = AL_OUTPUT_RATE;
		    buffer[1] = (long) audio.frameRate;
		    ALsetparams(AL_DEFAULT_DEVICE, buffer, 2);
		}
	    
	    /* hardware will use nearest supported rate.  */
	    /* warn about unsupported rates (outside the Audio Driver 
	       rate set) */
	    currentHardwareRate = GetAudioHardwareOutputRate();
	    if ((currentHardwareRate != AL_RATE_UNDEFINED)&&
		(currentHardwareRate != (long) audio.frameRate))
		{
		    if (audioHardwareInUse) {
			fprintf (stderr, 
				 "Audio Hardware in Use. Playing at ");
			fprintf (stderr,"%d Hz, not file rate %g Hz\n", 
				 currentHardwareRate, audio.frameRate);
			fprintf (stderr, "Shutting off audio\n");
                        /*
                         * we won't play the audio track because we
                         * can't change the output sample rate
                         * another audio app is already running
                         */
			audio.isTrack = 0; 
		    } else {
			fprintf (stderr, "File rate %g Hz unsupported. ");
			fprintf (stderr, "Playing at %d Hz\n", 
			      audio.frameRate, currentHardwareRate);
		    }
		}
	}
    /* restore AL error handler */
    ALseterrorhandler(oldErrorHandler);
}

/* ******************************************************************
 * GetAudioHardwareOutputRate	acquire audio hardware output sampling rate	
 * ****************************************************************** */
static long 
GetAudioHardwareOutputRate(void)
{
long	buffer[4];

buffer[0] = AL_OUTPUT_RATE;
buffer[2] = AL_DIGITAL_INPUT_RATE;
ALgetparams(AL_DEFAULT_DEVICE, buffer, 4);

/* output rate is in Hertz */
if	(buffer[1] > 0) 
    return (buffer[1]);

/* output rate is input rate */
else if (AL_RATE_INPUTRATE == buffer[1])
    return (GetAudioHardwareInputRate());

/* for input rate AES word clock and machine has ability to read digital input sampling rate, return 
	AL_DIGITAL_INPUT_RATE */
else if (ALgetdefault(AL_DEFAULT_DEVICE, AL_DIGITAL_INPUT_RATE) >= 0) 
    return (buffer[3]);

/* could not determine sampling rate */
else
    return (AL_RATE_UNDEFINED);
} /* ---- end GetAudioHardwareOutputRate() ---- */

/* ******************************************************************
 * AudioOutputHardwareInUse    any output ports open or monitor on?  
 * ****************************************************************** */
int
AudioOutputHardwareInUse(double rate)
{
long	buffer[4];

buffer[0] = AL_OUTPUT_COUNT;
buffer[2] = AL_MONITOR_CTL;
ALgetparams(AL_DEFAULT_DEVICE, buffer, 4);

/* no open audio output ports and monitor off */
if ((0 == buffer[1]) && (AL_MONITOR_OFF == buffer[3]))
    return (FALSE);

return (TRUE);
} /* ---- end AudioOutputHardwareInUse() ---- */

/*
 * GetAudioHardwareInputRate:    acquire audio hardware input sampling rate
 * -------------------------------------------------------------------- */
    long 
GetAudioHardwareInputRate(void)
{
long buffer[6];

/* acquire state variables of audio hardware */
buffer[0] = AL_INPUT_RATE;
buffer[2] = AL_INPUT_SOURCE;
buffer[4] = AL_DIGITAL_INPUT_RATE;
ALgetparams(AL_DEFAULT_DEVICE, buffer, 6);

/* for input sources microphone or line and input rate not AES word clock */
if	((buffer[3] != AL_INPUT_DIGITAL)&&(buffer[1] > 0))
    return (buffer[1]);

/* for input rate AES word clock and machine has ability to read digital input sampling rate, return 
	AL_DIGITAL_INPUT_RATE */
else if (ALgetdefault(AL_DEFAULT_DEVICE, AL_DIGITAL_INPUT_RATE) >= 0) 
    return (buffer[5]);

/* could not determine sampling rate */
else
    return (AL_RATE_UNDEFINED);
}   /* ---- end GetAudioHardwareInputRate() ---- */

