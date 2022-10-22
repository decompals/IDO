/*****************************************************************************
 *
 *   routine to play sound file 
 *	    
 *****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <signal.h>

#include <audio.h>
#include "audiofile.h"
#include "al.h"
#include "sf.h"

/* ******************************************************************
 * PrepareAudioFileAndHardware   	
 * ****************************************************************** */
bool 
PrepareAudioFileAndHardware( AFfilehandle fileHandle,
                             int id, 
                             ALconfig alConfiguration,
                             bool doNotRudelyChangeSamplingRate,
                             int printFormat )
{
    int	sampleFormat, sampleWidth;
    int	channelCount;
    double	fileSamplingRate;
    long	buffer[10];
    long	currentHardwareRate;
    bool	audioHardwareInUse;
    long	minOutChannelCapacity, maxOutChannelCapacity;
    long	audioHardwareChannelMode;

    /* turn off AL error handler during first half of this funtion */
    ALerrfunc oldErrorHandler = ALseterrorhandler(NULL);

    afSetVirtualByteOrder(fileHandle, id, AF_BYTEORDER_BIGENDIAN);

    /* attempt to set audio hardware output sampling rate to file rate */
    fileSamplingRate = afGetRate(fileHandle, id);
    if (fileSamplingRate <= 0)
    {
	SFError("Bogus sampling rate: %g Hertz\n", fileSamplingRate);
	return (FALSE);
    }

    /* if necessary, change audio hardware output sampling rate  */
    currentHardwareRate = GetAudioHardwareOutputRate();
    if (currentHardwareRate != fileSamplingRate)
    {
	audioHardwareInUse = AudioOutputHardwareInUse();
	/* if no one else is using audio output hardware, change sampling rate */
	if ((!audioHardwareInUse)||
	    (!doNotRudelyChangeSamplingRate))
	{
	    buffer[0] = AL_OUTPUT_RATE;
	    buffer[1] = (long) fileSamplingRate;
	    ALsetparams(AL_DEFAULT_DEVICE, buffer, 2);
	}

	/* hardware will use nearest supported rate.  */
	/* warn about unsupported rates (outside the Audio Driver rate set) */
	currentHardwareRate = GetAudioHardwareOutputRate();
	if ((currentHardwareRate != AL_RATE_UNDEFINED)&&
	    (currentHardwareRate != (long) fileSamplingRate))
	{
	    if (audioHardwareInUse)
		SFError("Audio Hardware in Use. Playing at %d Hz, not file rate %g Hz", 
		    currentHardwareRate, fileSamplingRate);
	    else
		SFError("File rate %g Hz unsupported. Playing at %d Hz", 
		    fileSamplingRate, currentHardwareRate);
	}
    }

    /* restore AL error handler */
    ALseterrorhandler(oldErrorHandler);

    /* query audio hardware for maximum channel count */
    /* if parameter does not exist, ALgetminmax() will return -1 */
    if (-1 == ALgetminmax(AL_DEFAULT_DEVICE, AL_CHANNEL_MODE, 
        &minOutChannelCapacity, &maxOutChannelCapacity))
    {
	minOutChannelCapacity = 2;
	maxOutChannelCapacity = 2;
	audioHardwareChannelMode = 2;
    }
    else
    {
	buffer[0] = AL_CHANNEL_MODE;
	ALgetparams(AL_DEFAULT_DEVICE, buffer, 2);
	audioHardwareChannelMode = buffer[1];
    }

    channelCount = afGetChannels(fileHandle, id);
    /* set channel count for 2 channel hardware */
    if (2 == audioHardwareChannelMode)
    {
	switch ( channelCount )
	{
	case 1:
	case 2:
	    ALsetchannels( alConfiguration, channelCount );
	    afSetVirtualChannels( fileHandle, id, channelCount );
	    break;
	default:
	    SFError("warning: playing first 2 of %d channels", channelCount);
	    ALsetchannels( alConfiguration, AL_STEREO );
	    afSetVirtualChannels( fileHandle, id, 2 );
	    break;
	}
    }
    /* set channel count for 4 channel hardware */
    else if (4 == audioHardwareChannelMode)
    {
	switch ( channelCount )
	{
	case 1:
	case 2:
	    ALsetchannels( alConfiguration, channelCount );
	    afSetVirtualChannels( fileHandle, id, channelCount);
	    break;
	case 3:
	    ALsetchannels( alConfiguration, 2);
	    afSetVirtualChannels( fileHandle, id, channelCount );
	    break;
	default:
	    if (channelCount != 4)
		SFError("warning: playing first 4 of %d channels", channelCount);
	    ALsetchannels( alConfiguration, AL_4CHANNEL );
	    afSetVirtualChannels( fileHandle, id, 4 );
	    break;
	}
    }
    else
    {
	SFError("Hey, invalid Audio Hardware channel mode: %d", audioHardwareChannelMode);
	return (FALSE);
    }

    /* set sample format */
    afGetSampleFormat(fileHandle, id, &sampleFormat, &sampleWidth);
    switch ( sampleFormat )
    {
    case AF_SAMPFMT_UNSIGNED:
    case AF_SAMPFMT_TWOSCOMP:
	switch ((sampleWidth-1)/8 + 1)
	{
	case 1:
	    ALsetwidth( alConfiguration, AL_SAMPLE_8 );
	    afSetVirtualSampleFormat( fileHandle, id, AF_SAMPFMT_TWOSCOMP, 8 );
	    break;
	case 2:
	    ALsetwidth( alConfiguration, AL_SAMPLE_16 );
	    afSetVirtualSampleFormat( fileHandle, id, AF_SAMPFMT_TWOSCOMP, 16 );
	    break;
	case 3:
	case 4:
	    ALsetwidth( alConfiguration, AL_SAMPLE_24 );
	    afSetVirtualSampleFormat( fileHandle, id, AF_SAMPFMT_TWOSCOMP, 24 );
	    break;

	default:
	    SFError("program plays 1-32 bit integer sound data only");
	    return(FALSE);
	}
	break;

    case AF_SAMPFMT_FLOAT:
    case AF_SAMPFMT_DOUBLE:
	ALsetwidth( alConfiguration, AL_SAMPLE_24 );
	afSetVirtualSampleFormat( fileHandle, id, AF_SAMPFMT_TWOSCOMP, 24 );
	break;
    }

    return (TRUE);
} /* ---- end PrepareAudioFileAndHardware() ---- */

/* ******************************************************************
 * PlaySoundFile   	
 * ****************************************************************** */
int 
PlaySoundFile(char *audioPortName,  AFfilehandle fileHandle, int id, 
bool background, bool doNotRudelyChangeSamplingRate, 
int printFormat)
/* audioPortName	name of port, cool to use application name
*/
{
    int	    processID;
    ALconfig    alConfiguration;
    ALport	    audioPort;
    int	    frames;		    /* frames per chunk */
    int	    channelCount;	    /* # channels in chunk */
    int	    frame;
    void	    *sampleBuffer;

    alConfiguration = ALnewconfig();
    if (!PrepareAudioFileAndHardware(fileHandle, id, alConfiguration, 
        doNotRudelyChangeSamplingRate, printFormat))
    {
	if (alConfiguration)
	    ALfreeconfig(alConfiguration);
	return (-1);
    }

    /* we're now in child process if this is background play */
    /* our chunk size is 1/2 second of sample data */
    frames = (int) (0.5*afGetRate(fileHandle, id));
    channelCount = afGetVirtualChannels(fileHandle, id);

    /* make ring buffer large enough to hold 1 sec of converted samples */
    ALsetqueuesize( alConfiguration, channelCount*frames*2 );

    /* check to see if audio port open.  fork makes it tought to detect if
    forked process was able to open an audioport.  However,  there is
    a possiblity that the forked process may not be able to open a new
    port due to a race condition which results from competing with
    other applications opening audio ports between the oprt available
    check and its subsequent open in the forked process.  */
    audioPort = ALopenport( audioPortName, "w", alConfiguration );
    if ( !audioPort )
    {
	SFError("failed to open audio port");
	return (-2);
    }
    ALcloseport(audioPort);

    /* make refill happen at 1/4 sec */
    ALsetfillpoint( audioPort, channelCount*frames/2 );

    /* move fileptr to beginning of sample data */
    afSeekFrame(fileHandle, AF_DEFAULT_TRACK, 0);

    /* If this is background play, fork off here.
 Parent returns back immediately, while child allocates buffers, plays, 
 and exits when done.

 Note that in the background, the child can NOT call Error()--its 
 only choice on error is to exit.

 Also we make some nonsense calls to disable SIGINT handler of parent,
 which generally does some parent-specific thing.
*/
    if (background)
    {
#ifdef NOTDEF
	void  (*istat)(int, ...);
#endif /*NOTDEF*/
	void  (*istat)();

	istat = sigset( SIGINT, SIG_DFL );
	if (processID = fork())
	{			/* parent process */
	    sigset( SIGINT, istat );
	    return (processID);
	}
    }

    audioPort = ALopenport( audioPortName, "w", alConfiguration );
    sampleBuffer = malloc( frames * afGetVirtualFrameSize(fileHandle, id, TRUE) );
    /* do it ! */
    while ( 0 != (frame = afReadFrames(fileHandle, id, sampleBuffer, frames)))
    {
	ZAP();
	ALwritesamps(audioPort, sampleBuffer, frame*channelCount);
	if (frame != frames)
	    break;
    }

    free(sampleBuffer);

    /* poll output audio port for empty sample buffer */
    while (ALgetfilled(audioPort) > 0)
	sginap(1);
    ALcloseport( audioPort );
    if (alConfiguration)
	ALfreeconfig(alConfiguration);

    if (background)
	exit(0);

    return (0);
} /* ---- end PlaySoundFile() ---- */
