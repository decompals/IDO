/*
 * concert_a.c - Generate a pure 441 Hz tone for 5 seconds.
 *
 * Concert A is usually pitched somewhere between 440 Hz (USA) 
 *    and 442 Hz (Europe).
 *
 *		     Written by Scott Porter, Gints Klimanis 
 *			Silicon Graphics Computer Systems
 *				 1994
 */
#include <dmedia/audio.h>
#include <math.h>
#include <stdio.h>

    long 
GetAudioHardwareInputRate(void);
    long 
GetAudioHardwareOutputRate(void);
    void 
ComputeSinusoid(short *out, int length, double normalFrequency, double phase,
	    double amplitude);

main()
{
int	    i;
short	    buf[60000];
ALconfig    config;
ALport	    audioPort;
double	    samplingRate;
int	    length;
double	    arg, argInc;
double	    amplitude, frequency, phase;

ALseterrorhandler(0);

/* query audio hardware for output sampling */
/* if undefined, default to 44100 Hertz */
samplingRate = GetAudioHardwareOutputRate();
if (samplingRate == AL_RATE_UNDEFINED)
    samplingRate = 44100;
length = samplingRate;

config = ALnewconfig();
ALsetqueuesize(config, samplingRate);
ALsetchannels(config, AL_MONO);

audioPort = ALopenport("outport", "w", config);
if (!audioPort) 
    {
    fprintf(stderr, "couldn't open port\n");
    exit(1);
    }

/* generate a second of sine wave */
amplitude = 32767;	/* for wave in range [-32767 .. 32767] */
frequency = 440;	/* Hertz */
phase = M_PI/2;		/* Radians */
length = samplingRate;
ComputeSinusoid(buf, length, frequency/samplingRate, phase, amplitude);

/* write 5 seconds to audio hardware */
for (i = 0; i < 5; i++) 
    ALwritesamps(audioPort, buf, length);

/* allow audio to drain from port */
while (ALgetfilled(audioPort) > 0) 
      sginap(1);

ALfreeconfig(config);
ALcloseport(audioPort);
}


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
 * ComputeSinusoid:    use waveguide oscillator to compute sinusoid
 * ****************************************************************** */
    void 
ComputeSinusoid(short *out, int length, double normalFrequency, double phase,
	    double amplitude)
/*
    out			ptr to output
    length		length of output
    normalFrequency	frequency/samplingFrequency
    phase		in Radians.  0=cosine wave, PI/2= sine wave
    amplitude		maximum&minimum of sinusoid
*/
{
int	i;
double	sum1, sum2;
double	z1, z2;
double	amplitudeCoefficient, tuningCoefficient;

/* 
 * -------- compute transformer normalized waveguide sinusoid oscillator 
 */
/*
more efficient version of:

argInc = frequency*2*M_PI/((float) samplingRate);
arg = phase;
for (i = 0; i < length; i++, arg += argInc) 
    buf[i] = (short) (amplitude*cos(arg));
*/

/* ring in z2:  z2 output is ring-value amplitude cosine wave  
		z1 output is low amplitude -sine 

   ring in z1:  z1 output is ring-value amplitude cosine wave
		z2 output is huge amplitude sine wave.  	

   to start at arbitrary initial phase:
	z1 = -amplitudeCoefficient*amplitude*sin(phase)
	z2 = amplitude*cos(phase)
*/
tuningCoefficient = cos(2*M_PI*normalFrequency);
amplitudeCoefficient = sqrt(((1-tuningCoefficient)/(1+tuningCoefficient)));
z2 = amplitude*cos(phase);
z1 = -amplitudeCoefficient*amplitude*sin(phase);
for (i = 0; i < length; i++)
    {
    sum2 = (z1 + z2)*tuningCoefficient;
    sum1 = sum2 - z2;
    z2 = z1 + sum2;
    z1 = sum1;

    out[i] = (short) z2;
    }

}   /* ---- end ComputeSinusoid() ---- */

