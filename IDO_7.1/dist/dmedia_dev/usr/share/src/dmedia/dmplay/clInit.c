/*
 * clInit.c: Does libcl (compression library) initialization
 *
 * 
 * Silicon Graphics Inc., June 1994
 */
#include "dmplay.h"

/*
 * clInit: Does cl initialization
 *
 *  mode 1 => decompressor
 *  mode 0 => compressor
 */
void
clInit (int mode)
{
    int n, paramBuf [50];
    int status;
    int externalHeight;
    int externalWidth;
    int internalHeight;

    if (mode) {  /* decompress */
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
    } else {    /* compress */
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

    externalHeight = image.height;
    externalWidth  = image.width;
    internalHeight = image.height;

    if (image.interlacing != DM_IMAGE_NONINTERLACED) {
	externalHeight /= 2;
	internalHeight /= 2;
    }

    n = 0;
    if (mode == 1 && image.display != GRAPHICS) {
	/*
	 * Set up cosmo video scaling
	 *
	 * CL_IMAGE_WIDTH and CL_IMAGE_HEIGHT are the dimensions we want
	 * to uncompress to.
	 * CL_INTERNAL_IMAGE_WIDTH and CL_INTERNAL_IMAGE_HEIGHT are
	 * the dimensions of the image as it was originally compressed.
	 * The CL will scale the image from CL_INTERNAL_IMAGE_WIDTH/HEIGHT to
	 * CL_IMAGE_WIDTH/HEIGHT as it decompresses.  This only works
	 * for Cosmo.
	 */
	if ( image.width == 640/2 || image.width == 720/2 )
	    externalWidth  *= 2;

	if ( abs(image.height-240) < 8 || abs(image.height-288) < 8)
	    externalHeight *= 2;

	paramBuf [n++] = CL_INTERNAL_IMAGE_WIDTH;
	paramBuf [n++] = image.width;
	paramBuf [n++] = CL_INTERNAL_IMAGE_HEIGHT;
	paramBuf [n++] = internalHeight;
    }

    paramBuf [n++] = CL_IMAGE_WIDTH;
    paramBuf [n++] = externalWidth;
    paramBuf [n++] = CL_IMAGE_HEIGHT;
    paramBuf [n++] = externalHeight;

    if (image.display == GRAPHICS) {
	paramBuf [n++] = CL_ORIGINAL_FORMAT;
	paramBuf [n++] = CL_RGBX;
    }

    if (codec.engine == CL_JPEG_COSMO) {
	paramBuf [n++] = CL_ENABLE_IMAGEINFO;
	paramBuf [n++] = 1;
        if (image.orientation != DM_TOP_TO_BOTTOM) {
            fprintf(stderr, 
     "Cosmo can only process images with ``top to bottom'' orientation.\n");
	    stopAllThreads ();
        }
    }

    paramBuf [n++] = CL_ORIENTATION;
    paramBuf [n++] = CL_TOP_DOWN;

    if (clSetParams(codec.Hdl, paramBuf, n) != SUCCESS) {
	fprintf (stderr, "Error Setting Parameters.\tExiting\n");
	stopAllThreads ();
    }
}
