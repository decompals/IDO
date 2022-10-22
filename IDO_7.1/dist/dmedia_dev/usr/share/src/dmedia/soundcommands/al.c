/*****************************************************************************
 * GetAudioHardwareInputRate:    acquire audio hardware input sampling rate
 * GetAudioHardwareOutputRate	acquire audio hardware output sampling rate	
 * AudioOutputHardwareInUse    any output ports open or monitor on?  
 *
 *
 *		Written by Chris Pirazzi, Scott Porter, Gints Klimanis
 *			    1991-
 *****************************************************************************/
#include "al.h"
#include <audio.h>

/*
 * These routines expect to be run with AL error handler shut off to prevent
 *  spew of error messages to shell.
 * (call ALseterrorhandler(0)).
 */

/* ******************************************************************
 * GetAudioHardwareInputRate:    acquire audio hardware input sampling rate
 * ****************************************************************** */
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

/* for input rate AES word clock and machine has ability to read digital 
    input sampling rate, return AL_DIGITAL_INPUT_RATE */
else if (ALgetdefault(AL_DEFAULT_DEVICE, AL_DIGITAL_INPUT_RATE) >= 0) 
    return (buffer[5]);

/* could not determine sampling rate */
else
    return (AL_RATE_UNDEFINED);
}   /* ---- end GetAudioHardwareInputRate() ---- */

/* ******************************************************************
 * GetAudioHardwareOutputRate	acquire audio hardware output sampling rate	
 * ****************************************************************** */
    long 
GetAudioHardwareOutputRate(void)
{
long	buffer[4];

buffer[0] = AL_OUTPUT_RATE;
buffer[2] = AL_DIGITAL_INPUT_RATE;
ALgetparams(AL_DEFAULT_DEVICE, buffer, 4);

/* return output rate (Hertz) */
if	(buffer[1] > 0) 
    return (buffer[1]);

/* when output rate is input rate, get input rate */
else if (AL_RATE_INPUTRATE == buffer[1])
    return (GetAudioHardwareInputRate());

/* for input rate AES word clock and machine has ability to read digital 
    input sampling rate, return AL_DIGITAL_INPUT_RATE */
else if (ALgetdefault(AL_DEFAULT_DEVICE, AL_DIGITAL_INPUT_RATE) >= 0) 
    return (buffer[3]);

/* could not determine sampling rate */
else
    return (AL_RATE_UNDEFINED);
} /* ---- end GetAudioHardwareOutputRate() ---- */

/* ******************************************************************
 * AudioOutputHardwareInUse    any output ports open or monitor on?  
 * ****************************************************************** */
    char 
AudioOutputHardwareInUse(void)
{
long	buffer[4];

buffer[0] = AL_OUTPUT_COUNT;
buffer[2] = AL_MONITOR_CTL;
ALgetparams(AL_DEFAULT_DEVICE, buffer, 4);

/* no open audio output ports and monitor off */
if ((0 == buffer[1]) && (AL_MONITOR_OFF == buffer[3]))
    return (0);

return (1);
} /* ---- end AudioOutputHardwareInUse() ---- */
