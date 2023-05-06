/*************************************************************************\
*                                                                         *
*                  C O P Y R I G H T   N O T I C E                        *
*                                                                         *
\*************************************************************************/


/* 
 * Copyright (c) 1992 Aware, Inc. All rights reserved.
 * 
 * Copyright Unpublished, Aware Inc.  All Rights Reserved.
 * This software contains proprietary and confidential
 * information of Aware, Inc.  Use, disclosure or
 * reproduction is prohibited without the prior express
 * written consent of Aware, Inc.
 * 
 */


#ifndef __AWARE_AUDIO_H__
#define __AWARE_AUDIO_H__

/*
 * awareAudio.h - Aware Compression Library Parameter Header
 *
 *   01/21/92	Original Version by Brian Knittel and Jonathon Devine
 */

/* Aware Audio Specific Parameters - For both MPEG Audio and Multirate */
#define CL_CHANNEL_POLICY	    (CL_NUMBER_OF_PARAMS + 0)
#define CL_NOISE_MARGIN		    (CL_NUMBER_OF_PARAMS + 1)
#define CL_BITRATE_POLICY	    (CL_NUMBER_OF_PARAMS + 2)

/* Additional parameters for MPEG Audio */
#define CL_BITRATE_TARGET	    (CL_NUMBER_OF_PARAMS + 3)
#define CL_LAYER		    (CL_NUMBER_OF_PARAMS + 4)

/* read/write for compression configuration
 * read for state during compression/decompression 
*/
/*
 * Channel Policy
 */
#define   AWCMP_STEREO                  1
#define   AWCMP_JOINT_STEREO            2
#define   AWCMP_INDEPENDENT             3

/* 
 * read/write for compression configuration,
 * read for state during compression
 */
/*
 * Bit-rate Policy
 */
#define   AWCMP_FIXED_RATE              1
#define   AWCMP_CONST_QUAL              2
#define   AWCMP_LOSSLESS                4

/*
 * Layer values
 */
#define   AWCMP_MPEG_LAYER_I            1
#define   AWCMP_MPEG_LAYER_II           2


#endif
