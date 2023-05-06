/*
 * clInit.c: Does libcl (compression library) initialization
 *
 * 
 * Silicon Graphics Inc., June 1994
 */
#include "dmplay.h"

/*
 *clInit: Does cl initialization
 *
 *  mode 1 => decompressor
 *  mode 0 => compressor
 */

extern int winWidth, winHeight;

static int needToScale (void);

int needToScale ()
{
    int diff;
    int fudge = 5;
    
    if (image.interlacing != DM_IMAGE_NONINTERLACED) {
	if (winWidth == image.width || winWidth == image.width * 2) {
	    diff = abs (0.5*(winHeight+6) - image.height); /*6 trimmed lines*/
	    if (diff <= fudge) 
		return 1;
	}
    }
    return 0;
}


void
clInit (int mode)
{
    int n, paramBuf [50];
    int status;
    int internalHeight;

    n = 0;
    if (mode == 0 || image.display == GRAPHICS) {
	paramBuf [n++] = CL_IMAGE_WIDTH;
	paramBuf [n++] = image.width;
	paramBuf [n++] = CL_IMAGE_HEIGHT;
	paramBuf [n++] = (image.interlacing != DM_IMAGE_NONINTERLACED)? 
	    image.height/2 : image.height;
    } else {
	paramBuf [n++] = CL_IMAGE_WIDTH;
	paramBuf [n++] = winWidth;
	internalHeight = needToScale () ? image.height*2:image.height;
	paramBuf [n++] = CL_IMAGE_HEIGHT;
	paramBuf [n++] = (image.interlacing != DM_IMAGE_NONINTERLACED)? 
	    internalHeight/2 : internalHeight;
	paramBuf [n++] = CL_INTERNAL_IMAGE_WIDTH;
	paramBuf [n++] = image.width;
	paramBuf [n++] = CL_INTERNAL_IMAGE_HEIGHT;
	paramBuf [n++] = (image.interlacing != DM_IMAGE_NONINTERLACED)? 
	    image.height/2 : image.height;
    }
    paramBuf [n++] = CL_STREAM_HEADERS;
    paramBuf [n++] = 1;
    if (image.display == GRAPHICS) {
	paramBuf [n++] = CL_ORIGINAL_FORMAT;
	paramBuf [n++] = codec.OriginalFormat;
    }
    if (codec.engine == CL_JPEG_COSMO) {
        if (image.orientation != DM_TOP_TO_BOTTOM) {
            fprintf(stderr, 
     "Cosmo JPEG can process only images with ``top to bottom'' orientation\n");
	    stopAllThreads ();
        }
	paramBuf [n++] = CL_ENABLE_IMAGEINFO;
	paramBuf [n++] = 1;
        switch (image.display) {
            case GRAPHICS:
              break;
            case VIDEO_1_FIELD:
              paramBuf [n++] = CL_COSMO_VIDEO_TRANSFER_MODE;
              paramBuf [n++] = CL_COSMO_VIDEO_TRANSFER_AUTO_1_FIELD;
              break;
            case VIDEO_2_FIELD:
              paramBuf [n++] = CL_COSMO_VIDEO_TRANSFER_MODE;
              paramBuf [n++] = CL_COSMO_VIDEO_TRANSFER_AUTO_2_FIELD;
              break;
            default:
              fprintf (stderr, "Error in setting display parameters\n");
              break;
          }
    }
    else {
        paramBuf [n++] = CL_ORIENTATION;
        paramBuf [n++] = (image.orientation == DM_BOTTOM_TO_TOP) ? 
                                    CL_BOTTOM_UP : CL_TOP_DOWN;
    }

    /* Open  decompressor */
    if (mode) {
	if ((status=clOpenDecompressor(codec.engine, &codec.Hdl)) != SUCCESS) {
	    if (status == CL_SCHEME_NOT_AVAILABLE)
		fprintf (stderr, "Decompressor is not installed.\n");
	    else
	    if (status == CL_SCHEME_BUSY)
		fprintf (stderr, "Decompressor is in use.\n");
	    else
		fprintf (stderr, "Unable to open decompressor - %u\n",status);
	    stopAllThreads ();
	}
    } else {
	if ((status=clOpenCompressor(codec.engine, &codec.Hdl)) != SUCCESS) {
	    if (status == CL_SCHEME_NOT_AVAILABLE)
		fprintf (stderr, "Compressor is not installed.\n");
	    else
	    if (status == CL_SCHEME_BUSY)
		fprintf (stderr, "Compressor is in use.\n");
	    else
		fprintf (stderr, "Unable to open compressor - %u\n",status);
	    stopAllThreads ();
	}
    }

    if (clSetParams(codec.Hdl, paramBuf, n) != SUCCESS) {
	fprintf (stderr, "Error Setting Parameters.\tExiting\n");
	stopAllThreads ();
    }
}
