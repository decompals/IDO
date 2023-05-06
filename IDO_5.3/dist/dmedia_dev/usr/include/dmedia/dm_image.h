#ifndef __INC_DM_IMAGE_H__
#define __INC_DM_IMAGE_H__  

/*****************************************************************************
*
*  Copyright 1993, Silicon Graphics, Inc.
*  All Rights Reserved.
*
*  This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
*  the contents of this file may not be disclosed to third parties, copied or
*  duplicated in any form, in whole or in part, without the prior written
*  permission of Silicon Graphics, Inc.
*
*  RESTRICTED RIGHTS LEGEND:
*  Use, duplication or disclosure by the Government is subject to restrictions
*  as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
*  and Computer Software clause at DFARS 252.227-7013, and/or in similar or
*  successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
*  rights reserved under the Copyright Laws of the United States.
* 
*****************************************************************************/

#include <stdlib.h>		/* for size_t */
#include <dmedia/dm_params.h>

#ifdef __cplusplus
extern "C" {
#endif

/**********************************************************************
*
* Image Parameters
* ----------------
*
* The following set of parameters defines how an image is represented:
*
*   * Size (width and height)
*   * Sample Rate (frame rate: number of frames per second)
*   * Compression Scheme
*   * Interlacing 
*   * Pixel Format/Bit Packing (e.g., RGB332)
*   * Orientations (top-to-bottom vs. bottom-to-top)
*
* To set up a complete image format description:
*
*   DMparams* format;
*   dmParamsCreate( &format );
*   dmParamsSetInt   ( format, DM_IMAGE_WIDTH,       320                   );
*   dmParamsSetInt   ( format, DM_IMAGE_HEIGHT,      240                   );
*   dmParamsSetInt   ( format, DM_IMAGE_RATE,        15.0                  );
*   dmParamsSetString( format, DM_IMAGE_COMPRESSION, DM_IMAGE_UNCOMPRESSED );
*   dmParamsSetEnum  ( format, DM_IMAGE_INTERLACING, DM_IMAGE_NONINTERLACED);
*   dmParamsSetEnum  ( format, DM_IMAGE_PACKING,     DM_PACKING_RGBX       );
*   dmParamsSetEnum  ( format, DM_IMAGE_ORIENTATION, DM_BOTTOM_TO_TOP      );
*
* The following is equivalent:
*
*   DMparams* format;
*   dmParamsCreate( &format );
*   dmSetImageDefaults( format, 320, 240, DM_PACKING_RGBX );
*   dmParamsSetInt   ( format, DM_IMAGE_RATE,        15.0                  );
*   dmParamsSetString( format, DM_IMAGE_COMPRESSION, DM_IMAGE_UNCOMPRESSED );
*   dmParamsSetEnum  ( format, DM_IMAGE_ORIENTATION, DM_BOTTOM_TO_TOP      );
*
* Library Compatibility
*
*    Each of the digital media libraries supports a different set of
*    image formats.  There are comments to the right of each constant
*    indicating which of the libraries support it:
*
*            MV = Movie Library
*
**********************************************************************/

/********
*
* Size (integers)
*
********/

#define DM_IMAGE_WIDTH		"DM_IMAGE_WIDTH"
#define DM_IMAGE_HEIGHT		"DM_IMAGE_HEIGHT"

/********
*
* Sample Rate (float)
*
********/

#define DM_IMAGE_RATE		"DM_IMAGE_RATE"

/********
*
* Compression Scheme
*
* Compression names are strings to allow for user extension to
* the suite of compression algorithms.  The suggested naming 
* scheme for new algorithm includes an organization name to 
* help avoid name collisions.  For example, to add a new scheme:
*
*   #define DM_COMP_SCHEME2    "SGI_SCHEME2"
*
********/

#define DM_IMAGE_COMPRESSION	"DM_IMAGE_COMPRESSION"

#define DM_IMAGE_UNCOMPRESSED	"Uncompressed Video"	/* MV */
#define DM_IMAGE_RLE		"RLE"			/* MV */
#define DM_IMAGE_RLE24		"RLE24"			/* MV */
#define DM_IMAGE_JPEG		"JPEG"			/* MV */
#define DM_IMAGE_MPEG1		"MPEG1Video"		/* MV */
#define DM_IMAGE_MVC1		"MVC1"			/* MV */
#define DM_IMAGE_MVC2		"MVC2"			/* MV */
#define DM_IMAGE_RTR		"RTR"
#define DM_IMAGE_HDCC		"HDCC"
#define DM_IMAGE_QT_VIDEO	"Apple Video"		/* MV */
#define DM_IMAGE_QT_ANIM	"Apple Animation"	/* MV */

/********
*
* Interlacing
*
********/

#define DM_IMAGE_INTERLACING "DM_IMAGE_INTERLACING"

typedef enum __DMinterlacing {
    DM_IMAGE_NONINTERLACED,			/* Full frame */
    DM_IMAGE_INTERLACED_EVEN,			/* Two fields, even first */
    DM_IMAGE_INTERLACED_ODD			/* Two fields, odd first */
} DMinterlacing;

#define DM_IMAGE_NONINTERLEAVED DM_IMAGE_NONINTERLACED  /* for compatability */
#define DM_IMAGE_INTERLEAVED    DM_IMAGE_INTERLACED_ODD /* for compatability */

/********
*
* Pixel Bit Packing
*
********/

#define DM_IMAGE_PACKING "DM_IMAGE_PACKING"

typedef enum __DMpacking
{
    DM_PACKING_RGB		= 1000,
    DM_PACKING_RGBX		= 1001,			/* MV */
    DM_PACKING_RGBA		= 1002,
    DM_PACKING_RGB332		= 1003,		
        /* Starter graphics */
    DM_PACKING_RGB8		= 1004,
    DM_PACKING_GRAYSCALE	= 1005,			/* MV */
        /* 8-bit grayscale */
    DM_PACKING_YUV		= 1006,
    DM_PACKING_YUV411		= 1007,
    DM_PACKING_YUV422		= 1008,
    DM_PACKING_YUV422HC		= 1009,
    DM_PACKING_APPLE_32		= 1090,			/* MV */
    DM_PACKING_APPLE_16		= 1091			/* MV */
} DMpacking;
    
#define DM_PACKING_Y		DM_PACKING_GRAYSCALE
#define DM_PACKING_YCbCr	DM_PACKING_YUV
#define DM_PACKING_YCbCr422	DM_PACKING_YUV422
#define DM_PACKING_YCbCr422HC	DM_PACKING_YUV422HC
#define DM_PACKING_YUV422DC	DM_PACKING_YUV422HC
#define DM_PACKING_YCbCr422DC	DM_PACKING_YUV422HC

/********
*
* Orientation
*
********/

#define DM_IMAGE_ORIENTATION	"DM_IMAGE_ORIENTATION"

typedef enum __DMorientation 
{
    DM_TOP_TO_BOTTOM		= 1100,
    DM_BOTTOM_TO_TOP		= 1101			/* MV */
} DMorientation;

/**********************************************************************
*
* Image Functions
*
**********************************************************************/

/********
*
* dmSetImageDefaults
*
* This is a convenience function used when creating image formats.
* It creates a new param list and sets the three most commonly used
* parameters.  The rest are defaulted as follows:
*
*   Sample rate = 15.0
*   Compresson  = DM_IMAGE_COMP_UNCOMPRESSED
*   Interlacing = DM_IMAGE_NONINTERLEAVED
*   Orientation = DM_IMAGE_BOTTOM_TO_TOP
*
********/

DMstatus dmSetImageDefaults( DMparams*  toParam,
			     int        width,
			     int        height,
			     DMpacking  packing );

/********
*
* dmImageFrameSize
*
* Returns the number of bytes required to store an uncompressed image.
*
********/

size_t dmImageFrameSize( const DMparams* params );

#ifdef __cplusplus 
}
#endif

#endif /* ! __INC_DM_IMAGE_H__  */
