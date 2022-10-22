#ifndef __INC_DM_AUDIOUTIL_H__
#define __INC_DM_AUDIOUTIL_H__  

/***************************************************************************
 * SGI Digital Media Library: Audio Utility Routines
 *
 * <dmedia/dm_audioutil.h>
 *    header file for use with /usr/lib/libdmedia.so
 *
 ***************************************************************************
 * 
 * Copyright 1995, Silicon Graphics, Inc.
 * ALL RIGHTS RESERVED
 *
 * UNPUBLISHED -- Rights reserved under the copyright laws of the United
 * States.   Use of a copyright notice is precautionary only and does not
 * imply publication or disclosure.
 *
 * U.S. GOVERNMENT RESTRICTED RIGHTS LEGEND:
 * Use, duplication or disclosure by the Government is subject to restrictions
 * as set forth in FAR 52.227.19(c)(2) or subparagraph (c)(1)(ii) of the Rights
 * in Technical Data and Computer Software clause at DFARS 252.227-7013 and/or
 * in similar or successor clauses in the FAR, or the DOD or NASA FAR
 * Supplement.  Contractor/manufacturer is Silicon Graphics, Inc.,
 * 2011 N. Shoreline Blvd. Mountain View, CA 94039-7311.
 *
 * THE CONTENT OF THIS WORK CONTAINS CONFIDENTIAL AND PROPRIETARY
 * INFORMATION OF SILICON GRAPHICS, INC. ANY DUPLICATION, MODIFICATION,
 * DISTRIBUTION, OR DISCLOSURE IN ANY FORM, IN WHOLE, OR IN PART, IS STRICTLY
 * PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN PERMISSION OF SILICON
 * GRAPHICS, INC.
 *
 ****************************************************************************/

#ident "$Revision: 1.29 $"

#ifdef __cplusplus
extern "C" {
#endif

#include <dmedia/dmedia.h>
#include <dmedia/dm_audio.h>
#include <dmedia/dm_params.h>

/********
*
* Audio sampling rate conversion algorithms
*
********/

#define DM_AUDIO_RC_INPUT_RATE		"DM_AUDIO_RC_INPUT_RATE"
#define DM_AUDIO_RC_OUTPUT_RATE		"DM_AUDIO_RC_OUTPUT_RATE"

#define DM_AUDIO_RC_ALGORITHM      	"DM_AUDIO_RC_ALGORITHM"

#define DM_AUDIO_RC_JITTER_FREE    	"DM_AUDIO_RC_JITTER_FREE"
#define DM_AUDIO_RC_POLYNOMIAL_ORDER_1  "DM_AUDIO_RC_POLYNOMIAL_ORDER_1"
#define DM_AUDIO_RC_POLYNOMIAL_ORDER_3  "DM_AUDIO_RC_POLYNOMIAL_ORDER_3"

/********
*
* Algorithm-specific parameters for the jitter-free rate conversion algorithm
*
********/

#define DM_AUDIO_RC_JITTER_FREE_STOPBAND_ATTENUATION \
		"DM_AUDIO_RC_JITTER_FREE_STOPBAND_ATTENUATION"

#define DM_AUDIO_RC_JITTER_FREE_STOPBAND_ATTENUATION_78_DB	78.0
#define DM_AUDIO_RC_JITTER_FREE_STOPBAND_ATTENUATION_96_DB	96.0
#define DM_AUDIO_RC_JITTER_FREE_STOPBAND_ATTENUATION_120_DB	120.0

#define DM_AUDIO_RC_JITTER_FREE_TRANSITION_BANDWIDTH \
		"DM_AUDIO_RC_JITTER_FREE_TRANSITION_BANDWIDTH"

#define	DM_AUDIO_RC_JITTER_FREE_TRANSITION_BANDWIDTH_1_PERCENT	1.0
#define	DM_AUDIO_RC_JITTER_FREE_TRANSITION_BANDWIDTH_10_PERCENT	10.0
#define	DM_AUDIO_RC_JITTER_FREE_TRANSITION_BANDWIDTH_20_PERCENT	20.0


/********
*
* Parameters returned by dmAudioRateConverterGetParams (for all algorithms)
*
********/

#define DM_AUDIO_RC_ATOMIC_IN_LENGTH        \
                        "DM_AUDIO_RC_ATOMIC_IN_LENGTH"
#define DM_AUDIO_RC_ATOMIC_OUT_LENGTH       \
                        "DM_AUDIO_RC_ATOMIC_OUT_LENGTH"
#define DM_AUDIO_RC_GROUP_DELAY             \
                        "DM_AUDIO_RC_GROUP_DELAY"
/********
*
* Audio sampling rate conversion routines 
*
*********/

typedef struct _DMaudiorateconverter *DMaudiorateconverter;

extern DMstatus dmAudioRateConverterCreate ( DMaudiorateconverter *converter);

extern DMstatus dmAudioRateConverterDestroy ( DMaudiorateconverter converter);

extern DMstatus	dmAudioRateConverterSetParams ( DMaudiorateconverter converter, 
                                              	DMparams *params);

extern DMstatus	dmAudioRateConverterGetParams ( DMaudiorateconverter converter, 
                                              	DMparams *params);

extern DMstatus	dmAudioRateConvert ( DMaudiorateconverter converter,
						float *inBuffer,
						float *outBuffer,
						int numInputSamples,
						int *numOutputSamples);

extern DMstatus	dmAudioRateConverterReset ( DMaudiorateconverter converter, 
                                              	float resetValue);

/********
*
* CCITT G.711 mu-law, A-law conversion routines
*
*********/

extern void 	dmG711MulawEncode ( short *inBuffer, 
				  		unsigned char 	*outBuffer, 
				   		int 		numSamples );
extern void 	dmG711MulawDecode ( unsigned char *inBuffer, 
				   	   	short 		*outBuffer, 
                     		   	   	int		numSamples );
extern void 	dmG711MulawZeroTrapEncode ( short *inBuffer, 
						unsigned char 	*outBuffer,
                     				int 		numSamples );
extern void 	dmSunMulawEncode ( short *inBuffer, 
						unsigned char   *outBuffer,
                     				int 		numSamples );

extern void 	dmSunMulawDecode ( unsigned char *inBuffer, 
						short 		*outBuffer, 
                     				int 		numSamples );

extern void 	dmNeXTMulawEncode ( short *inBuffer, 
						unsigned char 	*outBuffer,
                     				int 		numSamples);

extern void 	dmG711AlawEncode (short *inBuffer, 
						unsigned char 	*outBuffer,
                     				int 		numSamples);

extern void 	dmG711AlawDecode (unsigned char *inBuffer, 
						short 		*outBuffer, 
                     				int 		numSamples);

extern void 	dmG711AlawToMulaw (unsigned char *inBuffer, 
						unsigned char 	*outBuffer,
                     				int 		numSamples);

extern void 	dmG711MulawToAlaw (unsigned char *inBuffer, 
						unsigned char 	*outBuffer,
                     				int 		numSamples);

#define 	dmNeXTMulawDecode        	dmG711MulawDecode
#define 	dmG711MulawZeroTrapDecode 	dmG711MulawDecode 

/********
*
* CCITT G.722 conversion routines
*
* Note: The numSamples argument to dmG722Encode and dmG722Decode
*    	must always be a multiple of 2, since G.722 stores two 
*	encoded samples in a single byte
*
*********/

typedef struct _DMG722encoder *DMG722encoder;
typedef struct _DMG722decoder *DMG722decoder;

extern DMstatus dmG722EncoderCreate ( DMG722encoder *encoder,
			                 	int maxSamples);

extern DMstatus dmG722EncoderDestroy( DMG722encoder encoder);

extern DMstatus dmG722Encode ( DMG722encoder encoder,
                   				short *inBuffer, 
						unsigned char *outBuffer, 
						int numSamples);



extern DMstatus dmG722DecoderCreate( DMG722decoder *decoder,
						int maxSamples, 
						int decodeMode);

extern DMstatus dmG722DecoderDestroy( DMG722decoder decoder);

extern DMstatus dmG722Decode( DMG722decoder decoder,
                   				unsigned char *inBuffer, 
						short *outBuffer, 
						int numSamples);


#ifdef __cplusplus 
}
#endif

#endif /* ! __INC_DM_AUDIOUTILH__ */
