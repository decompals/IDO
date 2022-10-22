#ifndef __INC_DM_IMAGE_H__
#define __INC_DM_IMAGE_H__  

/*****************************************************************************
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
* The following is a complete set of parameters which defines how an image
* is represented; not all may be required by a specific library.
*
*   * Size (width and height)
*   * Sample Rate (frame rate: number of frames per second)
*   * Spatial Quality
*   * Temporal Quality
*   * Compression Scheme
*   * Pixel aspect ratio (pixel height / pixel width - e.g. 1.0 is square)
*   * Bitrate		 (bits per second, used by some compressors
*   *                     to determine compression ratio, quality, etc.)
*   * Keyframe distance  (distance between key frames in frames)
*   * Interlacing 
*   * Pixel Packing      (e.g., XBGR, RGBA)
*   * Component datatype
*   * Image order
*   * Orientation        (top-to-bottom vs. bottom-to-top)
*   * Mirror             (left-to-right vs. right-to-left)
*   * Bias/Scale         (per component)
*   * Minimum/Maximum    (per component)
*   * Gamma coefficients (per component)
*
* Here is an example of setting up a complete image format description:
*
*   DMparams* params;
*   dmParamsCreate    (&params );
*   dmParamsSetInt     (params, DM_IMAGE_WIDTH,       320);
*   dmParamsSetInt     (params, DM_IMAGE_HEIGHT,      240);
*   dmParamsSetFloat   (params, DM_IMAGE_RATE,        15.0);
*   dmParamsSetFloat   (params, DM_IMAGE_QUALITY_SPATIAL,
*                               DM_IMAGE_QUALITY_NORMAL );
*   dmParamsSetFloat   (params, DM_IMAGE_QUALITY_TEMPORAL,
*                               DM_IMAGE_QUALITY_NORMAL );
*   dmParamsSetFloat   (params, DM_IMAGE_PIXEL_ASPECT, 1.0);
*   dmParamsSetString  (params, DM_IMAGE_COMPRESSION,  DM_IMAGE_UNCOMPRESSED );
*   dmParamsSetEnum    (params, DM_IMAGE_INTERLACING,  DM_IMAGE_NONINTERLACED);
*   dmParamsSetEnum    (params, DM_IMAGE_PACKING,      DM_IMAGE_PACKING_XBGR);
*   dmParamsSetEnum    (params, DM_IMAGE_ORIENTATION,  DM_IMAGE_BOTTOM_TO_TOP);
*
* The following is equivalent:
*
*   DMparams* format;
*   dmParamsCreate    (&params );
*   dmSetImageDefaults (params, 320, 240, DM_IMAGE_PACKING_XBGR);
*   dmParamsSetFloat   (params, DM_IMAGE_RATE, 15.0);
*
* Library Compatibility
*
*    The Digital Media Libraries support a wide variety of image
*    formats.  Comments described whether a particular library supports
*    that group of format specifier.
*
*            MV  = Movie       Library
*            CSL = Color Space Library
*
*
* DIGITAL MEDIA IMAGE PIXEL FORMATS
*
* The following terms are used to describe pixel formats in digital
* media images below.
*
* Colorspace   - A colorspace is a mathematical representation of a set of colors.
*                The number of components commonly used to define a colorspace is
*                1, 3, or 4.  Examples include (Red, Green, Blue), (Y, Cr, Cb) and
*                (Cyan, Yellow, Magenta, Black).  A single color is described be a
*                color space vector.
* 
* Data Type    - The data type used to describe each component of a color space
*                vector.
* 
* Scale/Bias   - Scale represents the dynamic range of a color component.  The
*                actual range is represented by [bias, bias+scale].
* 
* Min/Max      - Min and max refer to the absolute floor and ceiling of each of
*                the components respectively.
* 
* Sub-sampling - In a sub-sampled image, not all components are measured for
*                each pixel.  Examples of sub-sampling are: 444, 422, and 420.
* 
* Packing      - Describes the relative order of the components.  From the constants
*                defined in this file, the color space and sub-sampling are implied.
*
* Gamma        - Gamma correction is the process which compensates for the
*                non-linearity between voltage and light of the receiver or
*                emitter.  Gamma correction is calculated by: 
*
*                X = X * TS                 if X < TB
*                X = ((X+A)*B)^gamma + C    otherwise
*
*                where X is either R, G, or B. Note that if X = X^gamma is required,
*                simply set TB = TS = A = C = 0, and B = 1.
*
* Order       - The components or component blocks occur in the order of the
*		packing specification.  Examples of order are:
*
*               ABGR packing:
*               -------------
*		Interleaved: ABGRABGRABGR
*               Sequential:  AAABBBGGGRRR		on a per line  basis
*               Separate:    AAABBBGGGRRR		on a per image basis
*
*               444 YCrCb, with a CbYCr packing:
*               --------------------------------
*		Interleaved: CbYCrCbYCrCbYCr
*               Sequential:  CbCbCbYYYCrCrCr		on a per line  basis
*               Separate:    CbCbCbYYYCrCrCr		on a per image basis
*            
*               422 YCrCb, with a CbYCrY packing:
*               ---------------------------------
*		Interleaved: CbYCrYCbYCrYCbYCrY
*               Sequential:  CbCbCbYYYYYYCrCrCr		on a per line  basis
*               Separate:    CbCbCbYYYYYYCrCrCr		on a per image basis
*            
*               420 YCrCb, with a CbYCrYYY packing:
*               -----------------------------------
*		Interleaved: CbYCrYYYCbYCrYYYCbYCrYYY
*               Sequential:  CbCbCbYYYYYYYYYYYYCrCrCr	on a per line  basis
*               Separate:    CbCbCbYYYYYYYYYYYYCrCrCr	on a per image basis
*
*            
*               Note: Sequential 420 YCrCb data (CbYCrYYY packing) stores the
*               Cb for rows N and N+1, followed by the Y for row N followed
*               by the Y for row N+1 followed by the Cr for rows N and N+1.
*
*               Note: Separate 420 YCrCb data (CbYCrYYY packing) stores the Cb
*               for rows 0 thru' N-1, followed by the Y for row 0 thru' N-1,
*               followed by the Cr for rows 0 thru' N-1.
*
**********************************************************************/


/********
*
* Size (integers)
*
* Supported by: CSL
*               MV
*
********/
#define DM_IMAGE_WIDTH		        "DM_IMAGE_WIDTH"
#define DM_IMAGE_HEIGHT		        "DM_IMAGE_HEIGHT"


/********
*
* Sample Rate (floats)
*
* Supported by: MV
*
********/
#define DM_IMAGE_RATE		        "DM_IMAGE_RATE"


/********
*
* Image Quality (floats)
*
* Supported by: MV ( not all compressors honor them )
*                 
********/
#define DM_IMAGE_QUALITY_SPATIAL	"DM_IMAGE_QUALITY_SPATIAL"
#define DM_IMAGE_QUALITY_TEMPORAL	"DM_IMAGE_QUALITY_TEMPORAL"

#define DM_IMAGE_QUALITY_MIN            0
#define DM_IMAGE_QUALITY_LOW            0.25
#define DM_IMAGE_QUALITY_NORMAL         0.5
#define DM_IMAGE_QUALITY_HIGH           0.75
#define DM_IMAGE_QUALITY_MAX            0.99
#define DM_IMAGE_QUALITY_LOSSLESS       1.0


/********
*
* Compression Scheme
*
* The naming mechanism has been replaced by the one below. These names
* match those returned by clGetAlgorithmName()
*
* Supported by: MV (except MVC3, RTR, HDCC)
*
********/
#define DM_IMAGE_COMPRESSION	        "DM_IMAGE_COMPRESSION"

#define DM_IMAGE_UNCOMPRESSED	        "Uncompressed Video"
#define DM_IMAGE_RLE		        "RLE"
#define DM_IMAGE_RLE24		        "RLE24"
#define DM_IMAGE_JPEG		        "JPEG"
#define DM_IMAGE_MPEG1		        "MPEG-1 Video"
#define DM_IMAGE_MVC1		        "MVC1"
#define DM_IMAGE_MVC2		        "MVC2"
#define DM_IMAGE_MVC3		        "MVC3"
#define DM_IMAGE_RTR		        DM_IMAGE_MVC3
#define DM_IMAGE_HDCC		        "HDCC"
#define DM_IMAGE_QT_VIDEO	        "Apple Video"
#define DM_IMAGE_QT_ANIM	        "Apple Animation"
#define DM_IMAGE_QT_CVID	        "Apple Compact Video"
#define DM_IMAGE_QT_SMC		        "Apple Graphics"
#define DM_IMAGE_INDEO	                "Indeo Video"


/********
*
* Pixel aspect ratio (float)
*
* Supported by: MV
*
********/
#define DM_IMAGE_PIXEL_ASPECT   "DM_IMAGE_PIXEL_ASPECT"


/*******
*
* Bitrate (floats)
*
* Supported by: MV
*
********/
#define DM_IMAGE_BITRATE         "DM_IMAGE_BITRATE"


/*******
*
* Keyframe and reference frame distances
*
* Supported by: MV
*
********/
#define DM_IMAGE_KEYFRAME_DISTANCE "DM_IMAGE_KEYFRAME_DISTANCE"
#define DM_IMAGE_REFFRAME_DISTANCE "DM_IMAGE_REFFRAME_DISTANCE"


/********
*
* Interlacing
*
* Non-interlaced: Image is a single non-interlaced frame or is
*                 composed of alternating lines from two fields.
* Interlaced:     Image consists of two fields, one stacked on top of the other.
*
* Supported by: MV (currently only applicable to JPEG compressed movies)
*
********/
#define DM_IMAGE_INTERLACING "DM_IMAGE_INTERLACING"

typedef enum __DMimageinterlacing {
    DM_IMAGE_NONINTERLACED,
    DM_IMAGE_INTERLACED_EVEN,	/* Two fields; even lines first, PAL/625  */
    DM_IMAGE_INTERLACED_ODD	/* Two fields; odd lines first,  NTSC/525 */
} DMimageinterlacing;

/* Backwards compatiblity */
#define DM_IMAGE_NONINTERLEAVED DM_IMAGE_NONINTERLACED
#define DM_IMAGE_INTERLEAVED    DM_IMAGE_INTERLACED_ODD
#define DMinterlacing		DMimageinterlacing


/********
*
* Pixel Packing
*
* Supported by: CSL
*               MV (current release only supports the following:
*                   DM_IMAGE_PACKING_RGBX, DM_IMAGE_PACKING_XBGR, 
*                   DM_IMAGE_PACKING_RGBA, DM_IMAGE_PACKING_ABGR, 
*                   DM_IMAGE_PACKING_XRGB, DM_IMAGE_PACKING_ARGB
*                   DM_IMAGE_PACKING_XRGB1555, DM_IMAGE_LUMINANCE )
*
********/
#define DM_IMAGE_PACKING "DM_IMAGE_PACKING"

typedef enum __DMimagepacking {
    DM_IMAGE_PACKING_RGB       = 2000,
    DM_IMAGE_PACKING_BGR       = 1000,
    DM_IMAGE_PACKING_RGBX      = 2002,
    DM_IMAGE_PACKING_RGBA      = 2001,
    DM_IMAGE_PACKING_XRGB      = 1090, /* Apple 32 */
    DM_IMAGE_PACKING_ARGB      = 1089, /* Apple 32 */
    DM_IMAGE_PACKING_XBGR      = 1001,
    DM_IMAGE_PACKING_ABGR      = 1002,
    DM_IMAGE_PACKING_RBG323    = 1003, /* Indigo Entry / Starter Video           */
    DM_IMAGE_PACKING_BGR233    = 1004, /* XL / Vino 8 bit                        */
    DM_IMAGE_PACKING_XRGB1555  = 1091, /* Apple 16                               */
    DM_IMAGE_PACKING_CbYCr     = 1006, /* YCrCb - 444 sub-sampling               */
    DM_IMAGE_PACKING_CbYCrA    = 3000, /* YCrCb - 444 sub-sampling with alpha    */
    DM_IMAGE_PACKING_CbYCrY    = 1008, /* YCrCb - 422 sub-sampling               */
    DM_IMAGE_PACKING_CbYCrYYY  = 3001, /* YCrCb - 420 sub-sampling (MPEG, H.261) */
    DM_IMAGE_PACKING_LUMINANCE = 1005
} DMimagepacking;

/* Backwards compatiblity */
#define DM_PACKING_RGB		DM_IMAGE_PACKING_BGR
#define DM_PACKING_RGBX		DM_IMAGE_PACKING_XBGR
#define DM_PACKING_RGBA		DM_IMAGE_PACKING_ABGR
#define DM_PACKING_RGB332	DM_IMAGE_PACKING_RBG323
#define DM_PACKING_RGB8		DM_IMAGE_PACKING_BGR233
#define DM_PACKING_YUV		DM_IMAGE_PACKING_CbYCr
#define DM_PACKING_YUV422	DM_IMAGE_PACKING_CbYCrY
#define DM_PACKING_APPLE_32	DM_IMAGE_PACKING_XRGB
#define DM_PACKING_APPLE_16	DM_IMAGE_PACKING_XRGB1555
#define DM_PACKING_GRAYSCALE	DM_IMAGE_PACKING_LUMINANCE
#define DM_PACKING_Y		DM_IMAGE_PACKING_LUMINANCE
#define DM_PACKING_YCbCr	DM_IMAGE_PACKING_CbYCr
#define DM_PACKING_YCbCr422	DM_IMAGE_PACKING_CbYCrY
#define DMpacking		DMimagepacking


/*************
*
* Data Type
*
* Supported by: CSL
*
*************/
#define DM_IMAGE_DATATYPE	"DM_IMAGE_DATATYPE"

typedef enum __DMimagedatatype {
    DM_IMAGE_DATATYPE_BIT,           /* Non-uniform # bits per component       */
    DM_IMAGE_DATATYPE_CHAR,          /*  8 bits per component                  */
    DM_IMAGE_DATATYPE_SHORT10L,      /* 10 bits per component - left  aligned  */
    DM_IMAGE_DATATYPE_SHORT10R,      /* 10 bits per component - right aligned  */
    DM_IMAGE_DATATYPE_SHORT12L,      /* 12 bits per component - left  aligned  */
    DM_IMAGE_DATATYPE_SHORT12R       /* 12 bits per component - right aligned  */
} DMimagedatatype;


/********
*
* Order
*
* Supported by: CSL
*
*********/
#define DM_IMAGE_ORDER		"DM_IMAGE_ORDER"

typedef enum __DMimageorder {
    DM_IMAGE_ORDER_INTERLEAVED,
    DM_IMAGE_ORDER_SEQUENTIAL,
    DM_IMAGE_ORDER_SEPARATE
} DMimageorder;


/********
*
* Orientation
*
* Supported by: CSL
*               MV
*
********/
#define DM_IMAGE_ORIENTATION	"DM_IMAGE_ORIENTATION"

typedef enum __DMimageorientation {
    DM_IMAGE_TOP_TO_BOTTOM     = 1100,
    DM_IMAGE_BOTTOM_TO_TOP     = 1101
} DMimageorientation;

/* Backwards compatiblity */
#define DM_TOP_TO_BOTTOM	DM_IMAGE_TOP_TO_BOTTOM
#define DM_BOTTOM_TO_TOP	DM_IMAGE_BOTTOM_TO_TOP
#define DMorientation		DMimageorientation


/********
*
* Mirroring
*
* Supported by: CSL
*
*********/
#define DM_IMAGE_MIRROR		"DM_IMAGE_MIRROR"

typedef enum __DMimagemirror {
    DM_IMAGE_LEFT_TO_RIGHT,
    DM_IMAGE_RIGHT_TO_LEFT
} DMimagemirror;


/**************
*
* Components (DMparams)
*
* Supported by: CSL
*
**************/
#define DM_IMAGE_COMPONENT_1		"DM_IMAGE_COMPONENT_1"
#define DM_IMAGE_COMPONENT_2		"DM_IMAGE_COMPONENT_2"
#define DM_IMAGE_COMPONENT_3		"DM_IMAGE_COMPONENT_3"
#define DM_IMAGE_COMPONENT_ALPHA	"DM_IMAGE_COMPONENT_ALPHA"
#define DM_IMAGE_COMPONENT_ALL		"DM_IMAGE_COMPONENT_ALL"


/**************
*
* Bias/Scale (floats)
*
* Supported by: CSL
*
**************/
#define DM_IMAGE_BIAS		"DM_IMAGE_BIAS"
#define DM_IMAGE_SCALE		"DM_IMAGE_SCALE"


/**************
*
* Min/Max (floats)
*
* Supported by: CSL
*
**************/
#define DM_IMAGE_MIN		"DM_IMAGE_MIN"
#define DM_IMAGE_MAX		"DM_IMAGE_MAX"


/**************
*
* Gamma (floats)
*
* Supported by: CSL
*
**************/
#define DM_IMAGE_GAMMA		"DM_IMAGE_GAMMA"
#define DM_IMAGE_GAMMA_TB	"DM_IMAGE_GAMMA_TB"
#define DM_IMAGE_GAMMA_TS	"DM_IMAGE_GAMMA_TS"
#define DM_IMAGE_GAMMA_A	"DM_IMAGE_GAMMA_A"
#define DM_IMAGE_GAMMA_B	"DM_IMAGE_GAMMA_B"
#define DM_IMAGE_GAMMA_C	"DM_IMAGE_GAMMA_C"


/*******
*
*  Gamma defaults
*
*******/
#define DM_IMAGE_DEFAULT_GAMMA       1.0
#define DM_IMAGE_DEFAULT_GAMMA_TB    0.0
#define DM_IMAGE_DEFAULT_GAMMA_TS    0.0
#define DM_IMAGE_DEFAULT_GAMMA_A     0.0
#define DM_IMAGE_DEFAULT_GAMMA_B     1.0
#define DM_IMAGE_DEFAULT_GAMMA_C     0.0


/*******
*
*  Bit format defaults
*
*******/
#define DM_IMAGE_DEFAULT_RBG323_R_BIAS                 0
#define DM_IMAGE_DEFAULT_RBG323_G_BIAS                 0
#define DM_IMAGE_DEFAULT_RBG323_B_BIAS                 0
#define DM_IMAGE_DEFAULT_RBG323_R_SCALE                7
#define DM_IMAGE_DEFAULT_RBG323_G_SCALE                7
#define DM_IMAGE_DEFAULT_RBG323_B_SCALE                3
#define DM_IMAGE_DEFAULT_RBG323_R_MIN                  0
#define DM_IMAGE_DEFAULT_RBG323_G_MIN                  0
#define DM_IMAGE_DEFAULT_RBG323_B_MIN                  0
#define DM_IMAGE_DEFAULT_RBG323_R_MAX                  7
#define DM_IMAGE_DEFAULT_RBG323_G_MAX                  7
#define DM_IMAGE_DEFAULT_RBG323_B_MAX                  3

#define DM_IMAGE_DEFAULT_BGR233_R_BIAS                 0
#define DM_IMAGE_DEFAULT_BGR233_G_BIAS                 0
#define DM_IMAGE_DEFAULT_BGR233_B_BIAS                 0
#define DM_IMAGE_DEFAULT_BGR233_R_SCALE                7
#define DM_IMAGE_DEFAULT_BGR233_G_SCALE                7
#define DM_IMAGE_DEFAULT_BGR233_B_SCALE                3
#define DM_IMAGE_DEFAULT_BGR233_R_MIN                  0
#define DM_IMAGE_DEFAULT_BGR233_G_MIN                  0
#define DM_IMAGE_DEFAULT_BGR233_B_MIN                  0
#define DM_IMAGE_DEFAULT_BGR233_R_MAX                  7
#define DM_IMAGE_DEFAULT_BGR233_G_MAX                  7
#define DM_IMAGE_DEFAULT_BGR233_B_MAX                  3

#define DM_IMAGE_DEFAULT_ARGB1555_RGB_BIAS             0
#define DM_IMAGE_DEFAULT_ARGB1555_RGB_SCALE           31
#define DM_IMAGE_DEFAULT_ARGB1555_RGB_MIN              0
#define DM_IMAGE_DEFAULT_ARGB1555_RGB_MAX             31

#define DM_IMAGE_DEFAULT_ARGB1555_ALPHA_BIAS           0
#define DM_IMAGE_DEFAULT_ARGB1555_ALPHA_SCALE          1
#define DM_IMAGE_DEFAULT_ARGB1555_ALPHA_MIN            0
#define DM_IMAGE_DEFAULT_ARGB1555_ALPHA_MAX            1


/*******
*
*  8-bit defaults
*
*******/
#define DM_IMAGE_DEFAULT08_RGB_BIAS                    0
#define DM_IMAGE_DEFAULT08_RGB_SCALE                 255
#define DM_IMAGE_DEFAULT08_RGB_MIN                     0
#define DM_IMAGE_DEFAULT08_RGB_MAX                   255

#define DM_IMAGE_DEFAULT08_YCrCb_Y_BIAS               16
#define DM_IMAGE_DEFAULT08_YCrCb_CrCb_BIAS            16
#define DM_IMAGE_DEFAULT08_YCrCb_Y_SCALE             219
#define DM_IMAGE_DEFAULT08_YCrCb_CrCb_SCALE          224
#define DM_IMAGE_DEFAULT08_YCrCb_Y_MIN                 1
#define DM_IMAGE_DEFAULT08_YCrCb_CrCb_MIN              1
#define DM_IMAGE_DEFAULT08_YCrCb_Y_MAX               254
#define DM_IMAGE_DEFAULT08_YCrCb_CrCb_MAX            254

#define DM_IMAGE_DEFAULT08_LUMINANCE_BIAS              0
#define DM_IMAGE_DEFAULT08_LUMINANCE_SCALE           255
#define DM_IMAGE_DEFAULT08_LUMINANCE_MIN               0
#define DM_IMAGE_DEFAULT08_LUMINANCE_MAX             255

#define DM_IMAGE_DEFAULT08_ALPHA                     255

#define DM_IMAGE_DEFAULT08_RGB_ALPHA_BIAS              0
#define DM_IMAGE_DEFAULT08_RGB_ALPHA_SCALE           255
#define DM_IMAGE_DEFAULT08_RGB_ALPHA_MIN               0
#define DM_IMAGE_DEFAULT08_RGB_ALPHA_MAX             255

#define DM_IMAGE_DEFAULT08_YCrCb_ALPHA_BIAS            0
#define DM_IMAGE_DEFAULT08_YCrCb_ALPHA_SCALE         255
#define DM_IMAGE_DEFAULT08_YCrCb_ALPHA_MIN             1
#define DM_IMAGE_DEFAULT08_YCrCb_ALPHA_MAX           254

#define DM_IMAGE_DEFAULT08_LUMINANCE_ALPHA_BIAS        0
#define DM_IMAGE_DEFAULT08_LUMINANCE_ALPHA_SCALE     255
#define DM_IMAGE_DEFAULT08_LUMINANCE_ALPHA_MIN         0
#define DM_IMAGE_DEFAULT08_LUMINANCE_ALPHA_MAX       255

#define DM_IMAGE_DEFAULT08_CrCb_GRAY                 128


/*******
*
* 10-bit defaults
*
*******/
#define DM_IMAGE_DEFAULT10_RGB_BIAS                    0
#define DM_IMAGE_DEFAULT10_RGB_SCALE                1023
#define DM_IMAGE_DEFAULT10_RGB_MIN                     0
#define DM_IMAGE_DEFAULT10_RGB_MAX                  1023

#define DM_IMAGE_DEFAULT10_YCrCb_Y_BIAS               64
#define DM_IMAGE_DEFAULT10_YCrCb_CrCb_BIAS            64
#define DM_IMAGE_DEFAULT10_YCrCb_Y_SCALE             876
#define DM_IMAGE_DEFAULT10_YCrCb_CrCb_SCALE          896
#define DM_IMAGE_DEFAULT10_YCrCb_Y_MIN                 4
#define DM_IMAGE_DEFAULT10_YCrCb_CrCb_MIN              4
#define DM_IMAGE_DEFAULT10_YCrCb_Y_MAX              1019
#define DM_IMAGE_DEFAULT10_YCrCb_CrCb_MAX           1019

#define DM_IMAGE_DEFAULT10_LUMINANCE_BIAS              0
#define DM_IMAGE_DEFAULT10_LUMINANCE_SCALE          1023
#define DM_IMAGE_DEFAULT10_LUMINANCE_MIN               0
#define DM_IMAGE_DEFAULT10_LUMINANCE_MAX            1023

#define DM_IMAGE_DEFAULT10_ALPHA                    1023

#define DM_IMAGE_DEFAULT10_RGB_ALPHA_BIAS              0
#define DM_IMAGE_DEFAULT10_RGB_ALPHA_SCALE          1023
#define DM_IMAGE_DEFAULT10_RGB_ALPHA_MIN               0
#define DM_IMAGE_DEFAULT10_RGB_ALPHA_MAX            1023

#define DM_IMAGE_DEFAULT10_YCrCb_ALPHA_BIAS            0
#define DM_IMAGE_DEFAULT10_YCrCb_ALPHA_SCALE        1023
#define DM_IMAGE_DEFAULT10_YCrCb_ALPHA_MIN             4
#define DM_IMAGE_DEFAULT10_YCrCb_ALPHA_MAX          1019

#define DM_IMAGE_DEFAULT10_LUMINANCE_ALPHA_BIAS        0
#define DM_IMAGE_DEFAULT10_LUMINANCE_ALPHA_SCALE    1023
#define DM_IMAGE_DEFAULT10_LUMINANCE_ALPHA_MIN         0
#define DM_IMAGE_DEFAULT10_LUMINANCE_ALPHA_MAX      1023

#define DM_IMAGE_DEFAULT10_CrCb_GRAY                 512


/*******
*
* 12-bit defaults
*
*******/
#define DM_IMAGE_DEFAULT12_RGB_BIAS                    0
#define DM_IMAGE_DEFAULT12_RGB_SCALE                4095
#define DM_IMAGE_DEFAULT12_RGB_MIN                     0
#define DM_IMAGE_DEFAULT12_RGB_MAX                  4095

#define DM_IMAGE_DEFAULT12_YCrCb_Y_BIAS              256
#define DM_IMAGE_DEFAULT12_YCrCb_CrCb_BIAS           256
#define DM_IMAGE_DEFAULT12_YCrCb_Y_SCALE            3504
#define DM_IMAGE_DEFAULT12_YCrCb_CrCb_SCALE         3584
#define DM_IMAGE_DEFAULT12_YCrCb_Y_MIN                16
#define DM_IMAGE_DEFAULT12_YCrCb_CrCb_MIN             16
#define DM_IMAGE_DEFAULT12_YCrCb_Y_MAX              4079
#define DM_IMAGE_DEFAULT12_YCrCb_CrCb_MAX           4079

#define DM_IMAGE_DEFAULT12_LUMINANCE_BIAS              0
#define DM_IMAGE_DEFAULT12_LUMINANCE_SCALE          4095
#define DM_IMAGE_DEFAULT12_LUMINANCE_MIN               0
#define DM_IMAGE_DEFAULT12_LUMINANCE_MAX            4095

#define DM_IMAGE_DEFAULT12_ALPHA                    4095

#define DM_IMAGE_DEFAULT12_RGB_ALPHA_BIAS              0
#define DM_IMAGE_DEFAULT12_RGB_ALPHA_SCALE          4095
#define DM_IMAGE_DEFAULT12_RGB_ALPHA_MIN               0
#define DM_IMAGE_DEFAULT12_RGB_ALPHA_MAX            4095

#define DM_IMAGE_DEFAULT12_YCrCb_ALPHA_BIAS            0
#define DM_IMAGE_DEFAULT12_YCrCb_ALPHA_SCALE        4095
#define DM_IMAGE_DEFAULT12_YCrCb_ALPHA_MIN            16
#define DM_IMAGE_DEFAULT12_YCrCb_ALPHA_MAX          4079

#define DM_IMAGE_DEFAULT12_LUMINANCE_ALPHA_BIAS        0
#define DM_IMAGE_DEFAULT12_LUMINANCE_ALPHA_SCALE    4095
#define DM_IMAGE_DEFAULT12_LUMINANCE_ALPHA_MIN         0
#define DM_IMAGE_DEFAULT12_LUMINANCE_ALPHA_MAX      4095

#define DM_IMAGE_DEFAULT12_CrCb_GRAY                2048



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
*   Sample rate      = 15.0
*   Spatial quality  = DM_IMAGE_QUALITY_NORMAL
*   Temporal quality = DM_IMAGE_QUALITY_NORMAL
*   Compression      = DM_IMAGE_COMP_UNCOMPRESSED
*   Interlacing      = DM_IMAGE_NONINTERLEAVED
*   Image layout     = DM_IMAGE_LAYOUT_FULL_FRAME
*   Orientation      = DM_IMAGE_BOTTOM_TO_TOP
*   Pixel aspect     = 1.0
*
********/

DMstatus dmSetImageDefaults( DMparams* toParam,
			     int       width,
			     int       height,
			     DMpacking packing );

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
