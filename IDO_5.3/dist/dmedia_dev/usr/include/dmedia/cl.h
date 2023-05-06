/*****************************************************************************
 *
 * SGI compression library 
 *
 * cl.h
 *      header file for use with /usr/lib/libcl.so
 *
 * Copyright 1992 & 1993, Silicon Graphics, Inc.
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

#ident "$Revision: 1.42 $"

#ifndef _INC_CL_H_
#define _INC_CL_H_ 1

#ifdef __cplusplus
extern "C" {
#endif

#if     !defined(TRUE) || ((TRUE) != 1)
#define TRUE    (1)
#endif
#if     !defined(FALSE) || ((FALSE) != 0)
#define FALSE   (0)
#endif
#if     !defined(SUCCESS) || ((SUCCESS) != 0)
#define SUCCESS (0)
#endif
#if     !defined(FAILURE) || ((FAILURE) != -1)
#define FAILURE (-1)
#endif
#if     !defined(byte)
typedef unsigned char   byte;
#endif
#if     !defined(FunctionPtr)
typedef int (*FunctionPtr)();
#endif

/*
 * originalFormat parameter values
 */
#define CL_MAX_NUMBER_OF_ORIGINAL_FORMATS    (32)

/* Audio */
#define CL_MONO                 (0)
#define CL_STEREO_INTERLEAVED   (1)

/* Video 
 * YUV is defined to be the same thing as YCrCb (luma and two chroma 
 *     components).
 * 422 is appended to YUV (or YCrCb) if the chroma is sub-sampled by 2 
 *      horizontally, packed as U Y1 V Y2 (byte order).
 * 422HC is appended to YUV (or YCrCb) if the chroma is sub-sampled by 2 
 *      vertically in addition to horizontally, and is packed the same as 
 *      422 except that U & V are not valid on the second line.
 */
#define CL_RGB                  (0)
#define CL_RGBX                 (1)
#define CL_RGBA                 (2)
#define CL_RGB332               (3)     /* Starter's format, RRR BB GGG */

#define CL_GRAYSCALE            (4)
#define CL_Y                    (4)
#define CL_YUV                  (5)     
#define CL_YCbCr                (5)     
#define CL_YUV422               (6)     /* 4:2:2 sampling */
#define CL_YCbCr422             (6)     /* 4:2:2 sampling */
#define CL_YUV422HC             (7)     /* 4:1:1 sampling */
#define CL_YCbCr422HC           (7)     /* 4:1:1 sampling */
#define CL_YUV422DC             (7)     /* 4:1:1 sampling */
#define CL_YCbCr422DC           (7)     /* 4:1:1 sampling */

#define CL_RGB8                 (8)     /* standard format, BB GGG RRR */

#define CL_BEST_FIT             (-1)    

#define CL_BytesPerSample(s) \
(   /* if */ (((s) == CL_MONO) || ((s) == CL_YUV)) ? \
        /* return */ 2 : \
    /* else if */ ((s) == CL_STEREO_INTERLEAVED) ?  \
        /* return */ 4 : \
    /* else */ \
        /* return */ 0 \
)

#define CL_BytesPerPixel(f) \
(   /* if */ (((f) == CL_RGB) || ((f) == CL_YUV)) ? \
        /* return */ 3 : \
    /* else if */ (((f) == CL_RGBX) || ((f) == CL_RGBA)) ?  \
        /* return */ 4 : \
    /* else if */ (((f) == CL_RGB332) || ((f) == CL_GRAYSCALE) || \
                ((f) == CL_RGB8)) ? \
        /* return */ 1 : \
    /* else */ \
        /* return */ 2 \
)

#define CL_AudioFormatName(f) \
(   /* if */ ((f) == CL_MONO) ? \
        /* return */ "MONO" : \
    /* else if */ ((f) == CL_STEREO_INTERLEAVED) ?  \
        /* return */ "STEREO_INTERLEAVED" : \
    /* else */ \
        /* return */ "Not a valid format" \
)

#define CL_VideoFormatName(f) \
(   /* if */ ((f) == CL_RGB) ? \
        /* return */ "RGB" : \
    /* else if */ ((f) == CL_RGBX) ?  \
        /* return */ "RGBX" : \
    /* else if */ ((f) == CL_RGBA) ?  \
        /* return */ "RGBA": \
    /* else if */ ((f) == CL_RGB332) ?  \
        /* return */ "RGB332": \
    /* else if */ ((f) == CL_GRAYSCALE) ? \
        /* return */ "GRAYSCALE" : \
    /* else if */ ((f) == CL_YUV) ? \
        /* return */ "YUV" : \
    /* else if */ ((f) == CL_YUV422) ? \
        /* return */ "YUV422" : \
    /* else if */ ((f) == CL_YUV422DC) ? \
        /* return */ "YUV422DC" : \
    /* else if */ ((f) == CL_RGB8) ?  \
        /* return */ "RGB8": \
    /* else */ \
        /* return */ "Not a valid format" \
)

#define CL_MAX_NUMBER_OF_AUDIO_ALGORITHMS       (32)
#define CL_MAX_NUMBER_OF_VIDEO_ALGORITHMS       (32)

/*
 * Algorithm types
 */
#define CL_AUDIO                (0)
#define CL_VIDEO                (1)

#define CL_AlgorithmNumber(scheme)      ((scheme) & 0x7fff)
#define CL_AlgorithmType(scheme)        (((scheme) >> 15) & 1)
#define CL_Algorithm(type, n)           ((n) | (((type) & 1) << 15))

/*
 * "compressionScheme" argument values
 */
#define CL_UNKNOWN_SCHEME       (-1)

#define CL_UNCOMPRESSED_AUDIO   CL_Algorithm(CL_AUDIO, 0)
#define CL_G711_ULAW            CL_Algorithm(CL_AUDIO, 1)
#define CL_ULAW                 CL_Algorithm(CL_AUDIO, 1)
#define CL_G711_ALAW            CL_Algorithm(CL_AUDIO, 2)
#define CL_ALAW                 CL_Algorithm(CL_AUDIO, 2)
#define CL_AWARE_MPEG_AUDIO     CL_Algorithm(CL_AUDIO, 3)
#define CL_AWARE_MULTIRATE      CL_Algorithm(CL_AUDIO, 4)
    

#define CL_UNCOMPRESSED         CL_Algorithm(CL_VIDEO, 0)
#define CL_UNCOMPRESSED_VIDEO   CL_Algorithm(CL_VIDEO, 0)
#define CL_RLE                  CL_Algorithm(CL_VIDEO, 1)
#define CL_JPEG_SOFTWARE        CL_Algorithm(CL_VIDEO, 2)
#define CL_MPEG_VIDEO           CL_Algorithm(CL_VIDEO, 3)
#define CL_MVC1                 CL_Algorithm(CL_VIDEO, 4)
#define CL_RTR                  CL_Algorithm(CL_VIDEO, 5)
#define CL_RTR1                 CL_Algorithm(CL_VIDEO, 5)
#define CL_HDCC                 CL_Algorithm(CL_VIDEO, 6)
#define CL_MVC2                 CL_Algorithm(CL_VIDEO, 8)
#define CL_RLE24                CL_Algorithm(CL_VIDEO, 9)

/*
 * Parameters
 */
#define CL_MAX_NUMBER_OF_PARAMS (256)
#define CL_ParamNumber(paramID) (((paramID) & 0x80000000) == 0 ? \
                                 (paramID) & 0x7fffffff : \
                                (((paramID) & 0x7fffffff) + \
                                 CL_NUMBER_OF_PARAMS))

#define CL_ParamType(paramID)   (((paramID) >> 31) & 1)
#define CL_ParamID(type, n)             ((n) | (((type) & 1) << 31))

/* Default Parameters */
#define CL_IMAGE_WIDTH              (0)
#define CL_IMAGE_HEIGHT             (1) 
#define CL_ORIGINAL_FORMAT          (2)
#define CL_INTERNAL_FORMAT          (3)
#define CL_COMPONENTS               (4)
#define CL_BITS_PER_COMPONENT       (5)
#define CL_FRAME_RATE               (6)
#define CL_COMPRESSION_RATIO        (7)
#define CL_EXACT_COMPRESSION_RATIO  (8)
#define CL_FRAME_BUFFER_SIZE        (9) 
#define CL_COMPRESSED_BUFFER_SIZE   (10)
#define CL_BLOCK_SIZE               (11)
#define CL_PREROLL                  (12)
#define CL_FRAME_TYPE               (13)
#define CL_ALGORITHM_ID             (14)
#define CL_ALGORITHM_VERSION        (15)
#define CL_ORIENTATION              (16)
#define CL_NUMBER_OF_FRAMES         (17)
#define CL_SPEED                    (18)
#define CL_LAST_FRAME_INDEX         (19)
#define CL_ENABLE_IMAGEINFO         (20)
#define CL_INTERNAL_IMAGE_WIDTH     (21)
#define CL_INTERNAL_IMAGE_HEIGHT    (22)
#define CL_NUMBER_OF_PARAMS         (23)

/* MVC2  Compression Specific Parameters */
#define CL_MVC2_LUMA_THRESHOLD                   (CL_ParamID (1, 0))
#define CL_MVC2_CHROMA_THRESHOLD                 (CL_ParamID (1, 1))
#define CL_MVC2_EDGE_THRESHOLD                   (CL_ParamID (1, 2)) 

/* MVC2 Decompression Specific Parameters */
#define CL_MVC2_BLENDING                         (CL_ParamID (1, 3))
#define         CL_MVC2_BLENDING_OFF             0
#define         CL_MVC2_BLENDING_ON              1


/* JPEG Specific Parameters */
#define CL_JPEG_QUALITY_FACTOR              	(CL_ParamID(1, 0))
#define CL_JPEG_STREAM_HEADERS                 	(CL_ParamID(1, 1))
#define CL_JPEG_QUANTIZATION_TABLES            	(CL_ParamID(1, 2))
#define CL_JPEG_NUM_PARAMS                  	11 

/* MPEG Specific Parameters */
#define CL_END_OF_SEQUENCE                      (CL_ParamID(1, 0))

/* RTR Specific Parameters */
#define CL_RTR_QUALITY_LEVEL                    (CL_ParamID(1, 0))

/* HDCC Specific Parameters */
#define CL_HDCC_TILE_THRESHOLD                  (CL_ParamID(1, 0))
#define CL_HDCC_SAMPLES_PER_TILE                (CL_ParamID(1, 1))

#define CL_TypeIsFloat(v)                  (*(float *)&(v))
#define CL_TypeIsInt(v)                    (*(int *)&(v))

#define FRAME_SIZE 1
#define TOTAL_FRAMES 2

/*
 * Parameter value types
 */
#define CL_ENUM_VALUE           (0) /* only certain constant values are valid*/
#define CL_RANGE_VALUE          (1) /* any value in a given range is valid */
#define CL_FLOATING_ENUM_VALUE  (2) /* only certain constant floating point 
                                       values are valid */
#define CL_FLOATING_RANGE_VALUE (3) /* any value in a given floating point 
                                       range is valid */

/*
 * Algorithm Functionality
 */
#define CL_DECOMPRESSOR         (1)
#define CL_COMPRESSOR           (2)
#define CL_CODEC                (3)

/*
 * Buffer types
 */
#define CL_NONE                 (0)
#define CL_BUF_FRAME            (1)
#define CL_BUF_DATA             (2)

/*
 * Frame types
 */
#define CL_NONE                 (0)
#define CL_KEYFRAME             (1)
#define CL_INTRA                (1)
#define CL_PREDICTED            (2)
#define CL_BIDIRECTIONAL        (3)

/*
 * Orientations
 */
#define CL_TOP_DOWN             (0)
#define CL_BOTTOM_UP            (1)

/*
 * Special values for clCompress/clDecompress frame counts
 */
#define CL_CONTINUOUS_BLOCK	(-1)
#define CL_CONTINUOUS_NONBLOCK	(-2)

/*
 * Special value for clCompress/clDecompress frame or data
 * buffer when connected to external video.
 */
#define CL_EXTERNAL_DEVICE	((void*)-1)

/*
 * SGI Proprietaty Algorithm Header Start Code
 */
#define CL_HEADER_START_CODE    0xc1C0DEC

/*
 * Function pointer definition
 */
typedef int     (*CLfunctionPtr)();
/* Old style equivalent - Please do not use this form */
typedef int     (*CL_FunctionPtr)();

/*
 * Handle definitions
 */
struct CL_CompressorStorage;
typedef struct CL_CompressorStorage     *CLhandle, *CLcompressorHdl;
struct CL_BufferStorage;
typedef struct CL_BufferStorage *CLbufferHdl;
/* Old style equivalents - Please do not use these */
typedef struct CL_CompressorStorage     *CL_Handle, *CL_CompressorHdl;
typedef struct CL_BufferStorage *CL_BufferHdl;


/*
 * error codes
 */

#define CL_BAD_NO_BUFFERSPACE       ( -2) /* no space for internal buffers */
#define CL_BAD_PVBUFFER             ( -3) /* param/val buffer doesn't make 
                                             sense */
#define CL_BAD_BUFFERLENGTH_NEG     ( -4) /* negative buffer length */
#define CL_BAD_BUFFERLENGTH_ODD     ( -5) /* odd length parameter/value 
                                             buffer */
#define CL_BAD_PARAM                ( -6) /* invalid parameter */
#define CL_BAD_COMPRESSION_SCHEME   ( -7) /* compression scheme parameter 
                                             invalid */
#define CL_BAD_COMPRESSOR_HANDLE    ( -8) /* compression handle parameter 
                                             invalid */
#define CL_BAD_COMPRESSOR_HANDLE_POINTER \
                                    ( -9) /* compression handle pointer 
                                             invalid */
#define CL_BAD_BUFFER_HANDLE        (-10) /* buffer handle invalid */
#define CL_BAD_BUFFER_QUERY_SIZE    (-11) /* buffer query size too large or 
                                             small */
#define CL_JPEG_ERROR               (-12) /* JPEG data is corrupt */
#define CL_BAD_FRAME_SIZE           (-13) /* frame size invalid */
#define CL_PARAM_OUT_OF_RANGE       (-14) /* parameter out of range */
#define CL_ADDED_ALGORITHM_ERROR    (-15) /* added algorithm had a unique 
                                             error */
#define CL_BAD_ALGORITHM_TYPE       (-16) /* bad algorithm type */
#define CL_BAD_ALGORITHM_NAME       (-17) /* bad algorithm name */
#define CL_BAD_BUFFERING            (-18) /* bad buffering calls */
#define CL_BUFFER_NOT_CREATED       (-19) /* buffer not created */
#define CL_BAD_BUFFER_EXISTS        (-20) /* buffer already created */
#define CL_BAD_INTERNAL_FORMAT      (-21) /* invalid internal format */
#define CL_BAD_BUFFER_POINTER       (-22) /* invalid buffer pointer */
#define CL_FRAME_BUFFER_SIZE_ZERO   (-23) /* frame buffer has zero size */
#define CL_BAD_STREAM_HEADER        (-24) /* invalid stream header */

#define CL_BAD_LICENSE              (-25) /* netls license not valid */
#define CL_AWARE_ERROR              (-26) /* error from Aware audio codec*/

#define CL_BAD_BUFFER_SIZE_POINTER  (-27) /* invalid buffer size pointer */
#define CL_BAD_BUFFER_SIZE          (-28) /* invalid buffer size */
#define CL_BAD_BUFFER_TYPE          (-29) /* invalid buffer type 
                                             (not CL_DATA or CL_FRAME) */
#define CL_BAD_HEADER_SIZE          (-30) /* invalid header size */
#define CL_BAD_FUNCTION_POINTER     (-31) /* null function pointer */
#define CL_BAD_SCHEME_POINTER       (-32) /* null scheme pointer */
#define CL_BAD_STRING_POINTER       (-33) /* null string pointer */
#define CL_BAD_MIN_GT_MAX           (-33) /* min greater than max */
#define CL_BAD_INITIAL_VALUE        (-34) /* bad initial value */
#define CL_BAD_PARAM_ID_POINTER     (-35) /* bad arg to clAddParam */
#define CL_BAD_PARAM_TYPE           (-36) /* bad arg to clAddParam */
#define CL_BAD_TEXT_STRING_PTR      (-37) /* bad arg to clAddParam */
#define CL_BAD_FUNCTIONALITY        (-38) /* bad arg to clQueryLicense */
#define CL_BAD_NUMBER_OF_BLOCKS     (-39) /* bad arg to clCreateBuf */
#define CL_BAD_BLOCK_SIZE           (-40) /* bad arg to clCreateBuf */
#define CL_BAD_POINTER              (-41) /* bad pointer*/
#define CL_BAD_BOARD                (-42) /* invalid board details*/
#define CL_MVC2_ERROR               (-43) /* error in MVC2 operation */
#define CL_NEXT_NOT_AVAILABLE       (-44) /* no image ready
						      (clGetNextImageInfo) */
#define CL_SCHEME_BUSY              (-45) /* resources required by
					     a scheme are in use */
#define CL_SCHEME_NOT_AVAILABLE     (-46) /* support for the requested scheme
					     is not installed */

/*
 * Argument to clGetNextImageInfo
 */
typedef struct {
    unsigned size;              /* size of compressed image in bytes */
    unsigned long long ustime;  /* time in nanoseconds */
    unsigned imagecount;        /* absolute media stream counter */
    unsigned status;		/* additional information */
} CLimageInfo;

/*
 * Values for CLimageInfo status field
 */
#define CL_IMAGEINFO_FIELDMASK	3		/* Bits in status */
#define		CL_IMAGEINFO_FIELD_ODD	1	/* image was an odd field */
#define		CL_IMAGEINFO_FIELD_EVEN	2	/* image was an even field */



/* Public Routine Prototypes ********************************************/

int clDecompressImage(int decompressionScheme, 
    int width, int height, int originalFormat, 
    int compressedBufferSize, void *compressedBuffer, void *frameBuffer);
int clCompressImage(int compressionScheme, 
    int width, int height, int originalFormat, float compressionRatio, 
    void *frameBuffer, int *compressedBufferSize, void *compressedBuffer);

int clOpenCompressor(int scheme, CLhandle *handle);
int clCompress(CLhandle handle, int numberOfFrames, 
    void *frameBuffer, int *compressedDataSize, void *compressedData);
int clCloseCompressor(CLhandle handle);

int clOpenDecompressor(int scheme, CLhandle *handle);
int clDecompress(CLhandle handle, int numberOfFrames, 
    int compressedDataSize, void *compressedData, void *frameBuffer);
int clCloseDecompressor(CLhandle handle);

/*
 * Buffering Routines
 */
CLbufferHdl clCreateBuf(CLhandle handle, int bufferType, int blocks, 
    int blockSize, void **bufferPtr);
int clDestroyBuf(CLbufferHdl bufferHdl);
CLbufferHdl clQueryBufferHdl(CLhandle handle, int bufferType, void **buffer);
CLhandle clQueryHandle(CLbufferHdl bufferHdl);

int clQueryFree(CLbufferHdl bufferHdl, int space, void **data, 
    int *wrap);
int clUpdateHead(CLbufferHdl bufferHdl, int size);
int clQueryValid(CLbufferHdl bufferHdl, int numberOfFrames, 
                void **frameBuffer, int *wrap);
int clUpdateTail(CLbufferHdl bufferHdl, int numberOfFrames);
int clDoneUpdatingHead(CLbufferHdl bufferHdl);
int clQueryScheme(void *header);
int clQueryMaxHeaderSize(int scheme);
int clReadHeader(CLhandle handle, int compressedDataSize, 
    void *compressedData);

/*
 * Parameter querying, getting, setting, etc.
 */
int clQueryParams(CLhandle handle, int *parameterValuebuffer, int maxLength);
int clGetParams(CLhandle handle, int *parameterValuebuffer, int bufferLength);
int clGetParam(CLhandle handle, int parameter);
int clSetParams(CLhandle handle, int *parameterValuebuffer, int bufferLength);
int clSetParam(CLhandle handle, int parameter, int value);
int clGetMinMax(CLhandle handle, int param,int* minParam, int* maxParam);
int clGetDefault(CLhandle handle, int param);
char* clGetName(CLhandle handle, int param);
int clGetParamID(CLhandle handle, char *name);

/*
 * error handler: you can replace the default error handler with your
 *     own routine by calling clSetErrorHandler
 */
typedef void (*CL_ErrFunc)(CLhandle handle, int code, const char *fmt, ...);
extern CL_ErrFunc clSetErrorHandler(CL_ErrFunc efunc);

/*
 * Routines for querying the list of compression algorithms and getting
 * the scheme IDs or the name.
 */
int clQueryAlgorithms(int algorithmType, int *PVbuffer, int bufferlength);
int clQuerySchemeFromName(int algorithmType, const char *name);
int clQuerySchemeFromHandle(CLhandle handle);
char *clGetAlgorithmName(int scheme);
int clQueryLicense(int scheme, int functionality, char **message);

int clGetNextImageInfo (CLhandle compressorHdl, CLimageInfo *imageInfo, 
                        int size);


/* ALL OF THE FOLLOWING ROUTINES ARE FOR ADDING ALGORITHMS ONLY
 * 
 * Routines for  adding a new algorithm, adding any extra parameters 
 * it may require, and changing default settings for other parameters.
 */
int clAddAlgorithm(char *name, int type, int maxSequenceHeaderSize, 
                   FunctionPtr openCompressor, FunctionPtr compress, 
                   FunctionPtr closeCompressor, FunctionPtr openDecompressor,
                   FunctionPtr decompress, FunctionPtr closeDecompressor, 
                   FunctionPtr readHeader, FunctionPtr queryScheme,
                   FunctionPtr queryLicense, FunctionPtr getParams, 
                   FunctionPtr setParams, int *compressionScheme);
int clAddParam(int compressionScheme, char *name, int type, int min, 
               int max, int initial, int *paramID);
int clSetDefault(int scheme, int paramID, int value);
int clSetMin(int scheme, int paramID, int min);
int clSetMax(int scheme, int paramID, int max);
void * clGetUnique(CLhandle handle);
void * clSetUnique(CLhandle handle, void *unique);

extern void (*clError)(CLhandle handle, int code, const char *fmt, ...);

int clReadData(CLbufferHdl bufferHdl, int requestedDataSize, void **compressedData);
int clDone(CLbufferHdl bufferHdl, int size);


/*************************************************************************** 
 * backward compatibility 
 ***************************************************************************/

#include <dmedia/cl_awareaudio.h>

#define CL_LUMA_THRESHOLD                CL_MVC2_LUMA_THRESHOLD
#define CL_CHROMA_THRESHOLD              CL_MVC2_CHROMA_THRESHOLD 
#define CL_EDGE_THRESHOLD                CL_MVC2_EDGE_THRESHOLD
#define CL_BLENDING                      CL_MVC2_BLENDING
#if     !defined(ON)  || ((ON)  != CL_MVC2_BLENDING_ON)
#define ON  CL_MVC2_BLENDING_ON
#endif
#if     !defined(OFF) || ((OFF) != CL_MVC2_BLENDING_OFF)
#define OFF CL_MVC2_BLENDING_OFF
#endif

#define CL_JPEG                          CL_JPEG_SOFTWARE
#define CL_QUALITY_FACTOR                CL_JPEG_QUALITY_FACTOR 
#define CL_STREAM_HEADERS                CL_JPEG_STREAM_HEADERS
#define CL_NUMBER_OF_QUANTIZATION_TABLES CL_JPEG_NUMBER_OF_QUANTIZATION_TABLES
#define CL_QUANTIZATION_TABLES           CL_JPEG_QUANTIZATION_TABLES  
#define CL_CURRENT_QUANTIZATION_TABLE    CL_JPEG_CURRENT_QUANTIZATION_TABLE

#define CL_QUALITY_LEVEL                 CL_RTR_QUALITY_LEVEL

#define CL_TILE_THRESHOLD                CL_HDCC_TILE_THRESHOLD 
#define CL_SAMPLES_PER_TILE              CL_HDCC_SAMPLES_PER_TILE

#define CL_FRAME                         CL_BUF_FRAME            
#define CL_DATA                          CL_BUF_DATA

#ifdef __cplusplus
}
#endif

#endif
