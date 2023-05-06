#ifndef __AUDIO_H__
#define __AUDIO_H__ 

/*****************************************************************************
 *
 * SGI audio library 
 *
 * audio.h
 *	header file for use with /usr/lib/libaudio.a
 *
 * Copyright 1991, 1992, 1993 Silicon Graphics, Inc.
 * All Rights Reserved.
 *
 * This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
 * the contents of this file may not be disclosed to third parties, copied or
 * duplicated in any form, in whole or in part, without the prior written
 * permission of Silicon Graphics, Inc.
 *
 * RESTRICTED RIGHTS LEGEND:
 * Use, duplication or disclosure by the Government is subject to restrictions
 * as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
 * and Computer Software clause at DFARS 252.227-7013, and/or in similar or
 * successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
 * rights reserved under the Copyright Laws of the United States.
 *
 ****************************************************************************/
#ident "$Revision: 1.49 $"

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

/*
 * audio port sample rates (these are the only ones supported by the library)
 */
#define AL_RATE_48000 	48000
#define AL_RATE_44100	44100
#define AL_RATE_32000	32000
#define AL_RATE_22050	22050
#define AL_RATE_16000	16000
#define AL_RATE_11025	11025
#define AL_RATE_8000	8000

/*
 * Setting the output rate to AL_RATE_INPUTRATE means that the output sample 
 * rate should track the sample rate of the currently selected input source.
 */
#define AL_RATE_INPUTRATE       (-1)

/*
 * The AES_X tokens define rates derived from the AES3 digital input
 * clock. The AES divided clock is divided by X and optionally multiplied
 * by 2/3 (if the "s" suffix is present). So AES_2 is AES/2, AES_1s is 
 * AES*2/3, etc.
 */
#define AL_RATE_AES_1           (-2)
#define AL_RATE_AES_2		(-3)
#define AL_RATE_AES_3		(-4)
#define AL_RATE_AES_4		(-5)
#define AL_RATE_AES_6		(-6)
#define AL_RATE_AES_1s		(-7)

/*
 * The following are possible return values from digital input rate queries.
 * AL_RATE_UNDEFINED means that a clock is present but its rate is
 *	not encoded in the digital audio stream;
 * AL_RATE_NO_DIGITAL_INPUT means that the AES receiver was unable
 *	to lock to the input clock, probably implying that there is no
 *	input signal;
 * AL_RATE_UNACQUIRED means that a clock is present but that the rate encoding
 * 	in the data stream has not yet been encountered. Rates are encoded
 * 	only periodically in an AES3 digital audio stream (every 192 sample
 *	frames).
 */
#define AL_RATE_UNDEFINED	(-8)
#define AL_RATE_NO_DIGITAL_INPUT	(-9)
#define AL_RATE_UNACQUIRED	(-10)

/*
 * audio sample numeric format:
 *  
 * the audio library represents all audio data using linear pulse
 *   code modulation (PCM) format
 * linear PCM values are represented either as signed integers (the
 *   default), or as floating-point numbers
 */
#define AL_SAMPFMT_TWOSCOMP   1
#define AL_SAMPFMT_FLOAT      32
#define AL_SAMPFMT_DOUBLE     64

/*
 * audio sample resolution
 *    8-bit  signed integer samples are stored in signed characters
 *    16-bit signed integer samples are stored in signed shorts
 *    24-bit signed integer samples are sign extended into signed longs
 *
 * single and double precision floating-point numbers can
 *   represent more than 24-bits of dynamic range, however 24-bit resolution
 *   is the maximum supported by the audio I/O system
 */
#define AL_SAMPLE_8	 1
#define AL_SAMPLE_16	 2
#define AL_SAMPLE_24	 4

/*
 * number of channels in audio port configuration
 */
#define AL_MONO		1
#define AL_STEREO	2
#define AL_4CHANNEL	4

/*
 * global input source: the source you select (by calling ALsetparams) is
 * the source that supplies all open input ports in the system
 */
#define AL_INPUT_LINE		0
#define AL_INPUT_MIC		1
#define AL_INPUT_DIGITAL	2

/*
 * monitor control
 */
#define AL_MONITOR_OFF 0
#define AL_MONITOR_ON  1

/*
 * mute control
 */
#define AL_SPEAKER_MUTE_OFF 0
#define AL_SPEAKER_MUTE_ON  1

/*
 * port style
 */
#define AL_PORTSTYLE_SERIAL 0
#define AL_PORTSTYLE_DIRECT 1

/* error information */
#define AL_ERROR_NUMBER       0
#define AL_ERROR_TYPE         1
#define AL_ERROR_LOCATION_LSP 2
#define AL_ERROR_LOCATION_MSP 3
#define AL_ERROR_LENGTH       4

/* error types ... so far */
#define AL_ERROR_INPUT_OVERFLOW    0
#define AL_ERROR_OUTPUT_UNDERFLOW  1

typedef struct _ALconfig  *ALconfig;   /* audio port config handle */
typedef struct _ALport *ALport;        /* audio port handle        */

/*
 * audio port configuration
 * these routines allow you to:
 *   - obtain, release  a port config struct handle
 *   - initialize the configuration of an audio port before you open it
 *   - obtain configuration parameters for an open audio port
 *   - reconfigure certain parameters for an open port (see man page
 *     for ALsetconfig)
 */
extern ALconfig      ALnewconfig(void);			/* 0 for failure */
extern int	     ALfreeconfig(ALconfig);		/* < 0 for failure */
extern int	     ALsetconfig(ALport, ALconfig);	/* < 0 for failure */
extern ALconfig	     ALgetconfig(ALport);		/* 0 for failure */
extern int	     ALsetqueuesize(ALconfig, long);	/* < 0 for failure */
extern long	     ALgetqueuesize(ALconfig);		/* < 0 for failure */
extern int	     ALsetwidth(ALconfig, long);	/* < 0 for failure */
extern long	     ALgetwidth(ALconfig);		/* < 0 for failure */
extern int           ALsetsampfmt(ALconfig, long);	/* < 0 for failure */
extern long          ALgetsampfmt(ALconfig);		/* < 0 for failure */
extern int	     ALsetchannels(ALconfig, long);	/* < 0 for failure */
extern long	     ALgetchannels(ALconfig);		/* < 0 for failure */

extern int	     ALsetfloatmax(ALconfig, double);	/* < 0 for failure */
extern double        ALgetfloatmax(ALconfig);		/* = 0 for failure */


extern int           ALgetstatus(ALport, long*, long);	/* < 0 for failure */

/*
 * audio port manipulation
 * these routines allow you to:
 *    - open, close an audio port
 *    - obtain information about the dynamic state of the sample queue for
 *      an open port
 *    - read samples from an input port / write samples to an output port
 *    - obtain a file descriptor for an audio port (for use with the 
 *      poll and select system calls)
 */
extern ALport	ALopenport(const char* /*name*/, const char* /*direction*/, 
                                                              ALconfig);
						/* 0 for failure */
extern int	ALcloseport(ALport);		/* < 0 for failure */
extern int 	ALgetfd(ALport);		/* < 0 for failure */
extern long	ALgetfilled(ALport);		/* < 0 for failure */
extern long	ALgetfillable(ALport);		/* < 0 for failure */
extern int	ALreadsamps(ALport, void* /*buf*/, long /*count*/);
						/* < 0 for failure */
extern int	ALwritesamps(ALport, void* /*buf*/, long /*count*/);
						/* < 0 for failure */
extern int  	ALflush(ALport, long);		/* < 0 for failure */
extern int  	ALsetfillpoint(ALport, long);	/* < 0 for failure */
extern long	ALgetfillpoint(ALport);		/* < 0 for failure */

extern int  	ALgetframenumber(ALport, unsigned long long *);	
extern int	ALgetframetime(ALport, unsigned long long *, 
			unsigned long long *);		

/*
 * global hardware device and system configuration
 */
#define AL_DEFAULT_DEVICE	1  /* audio I/O device */

extern long	ALqueryparams(long /*device*/, long* /*PVbuffer*/, 
                                 long /*maxlength*/);
						/* < 0 for failure */
extern int	ALgetparams(long /*device*/, long* /*PVbuffer*/,
				long /*bufferlength*/);
						/* < 0 for failure */
extern int	ALsetparams(long /*device*/, long* /*PVbuffer*/,
				long /*bufferlength*/);
						/* < 0 for failure */
extern int	ALgetminmax(long /*device*/, long /*param*/,
				long* /*minparam*/, long* /*maxparam*/);
						/* < 0 for failure */
extern long	ALgetdefault(long /*device*/, long /*parameter*/);
						/* < 0 for failure */
extern char*	ALgetname(long /*device*/, long /*parameter*/);
						/* 0 for failure */


#define AL_MAX_PBUFSIZE		64	/* maximum size for parameter buffer */
/*
 * global hardware parameters
 */
#define AL_INPUT_SOURCE		0
#define AL_LEFT_INPUT_ATTEN	1
#define AL_RIGHT_INPUT_ATTEN	2
#define AL_INPUT_RATE		3
#define AL_OUTPUT_RATE		4
#define AL_LEFT_SPEAKER_GAIN	5
#define AL_RIGHT_SPEAKER_GAIN	6
#define AL_INPUT_COUNT		7
#define AL_OUTPUT_COUNT		8
#define AL_UNUSED_COUNT		9
#define AL_SYNC_INPUT_TO_AES    10      /* Obsolete */
#define AL_SYNC_OUTPUT_TO_AES   11      /* Obsolete */
#define AL_MONITOR_CTL          12
#define AL_LEFT_MONITOR_ATTEN   13
#define AL_RIGHT_MONITOR_ATTEN  14
#define AL_CHANNEL_MODE		15
#define AL_SPEAKER_MUTE_CTL	16
#define	AL_MIC_MODE		17
#define AL_LEFT1_INPUT_ATTEN    1 	/* same as AL_LEFT_INPUT_ATTEN  */
#define AL_RIGHT1_INPUT_ATTEN   2 	/* same as AL_RIGHT_INPUT_ATTEN */
#define AL_LEFT2_INPUT_ATTEN    18
#define AL_RIGHT2_INPUT_ATTEN   19 
#define AL_DIGITAL_INPUT_RATE	20

/*
 * parameter value type
 */
#define AL_ENUM_VALUE	1	/* only certain constant values are valid */
#define AL_RANGE_VALUE	2	/* any value in a gven range is valid */

/*
 * error handler: you can replace the default error handler with your
 *     own routine by calling ALseterrorhandler
 */
typedef void (*ALerrfunc)(long,const char*,...);
extern ALerrfunc ALseterrorhandler(ALerrfunc efunc);

/*
 * error codes
 */
#define AL_BAD_NOT_IMPLEMENTED	0 /* not implemented yet */
#define AL_BAD_PORT		1 /* tried to use an invalid port */
#define AL_BAD_CONFIG		2 /* tried to use an invalid configuration */
#define AL_BAD_DEVICE		3 /* tried to use an invalid device */
#define AL_BAD_DEVICE_ACCESS	4 /* unable to access the device */
#define AL_BAD_DIRECTION	5 /* illegal direction given for port */
#define AL_BAD_OUT_OF_MEM	6 /* operation has run out of memory */
#define AL_BAD_NO_PORTS		7 /* not able to allocate a port */
#define AL_BAD_WIDTH		8 /* invalid sample width given */
#define AL_BAD_ILLEGAL_STATE	9 /* an illegal state has occurred */
#define AL_BAD_QSIZE		10 /* attempt to set an invalid queue size */
#define AL_BAD_FILLPOINT	11 /* attempt to set an invalid fillpoint */
#define AL_BAD_BUFFER_NULL	12 /* null buffer pointer */
#define AL_BAD_COUNT_NEG	13 /* negative count */
#define AL_BAD_PVBUFFER		14 /* param/val buffer doesn't make sense */
#define AL_BAD_BUFFERLENGTH_NEG	15 /* negative buffer length */
#define AL_BAD_BUFFERLENGTH_ODD	16 /* odd length parameter/value buffer */
#define AL_BAD_CHANNELS		17 /* invalid channel specifier */
#define AL_BAD_PARAM		18 /* invalid parameter */
#define AL_BAD_SAMPFMT          19 /* attempt to set invalid sample format */
#define AL_BAD_RATE             20 /* invalid sample rate token */
#define AL_BAD_TRANSFER_SIZE	21 /* invalid size for sample read/write */
#define AL_BAD_FLOATMAX		22 /* invalid size for floatmax */
#define AL_BAD_PORTSTYLE	23 /* invalid port style */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif /* __AUDIO_H__ */
