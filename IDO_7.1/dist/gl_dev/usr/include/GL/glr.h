#ifndef __glr_h__
#define __glr_h__

#ifdef __cplusplus
extern "C" {
#endif

/* Copyright 1995, Silicon Graphics, Inc. All Rights Reserved.

   This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.; the 
   contents of this file may not be disclosed to third parties, copied or
   duplicated in any form, in whole or in part, without the prior written
   permission of Silicon Graphics, Inc.

   RESTRICTED RIGHTS LEGEND: Use, duplication or disclosure by the Government 
   is subject to restrictions as set forth in subdivision (c)(1)(ii) of the
   Rights in Technical Data and Computer Software clause at DFARS
   252.227-7013, and/or in similar or successor clauses in the FAR, DOD or
   NASA FAR Supplement. Unpublished - rights reserved under the Copyright
   Laws of the United States. */

/* OpenGL Remote rendering API (GLR) interface */

/* These values should exactly match the GLX protocol values for the
   attribute values for glXGetConfig.  Note that because the GLR API is
   independent of the GLX API, we avoid including <GL/glxtokens.h> to obtain
   the values. */

#define GLR_BUFFER_SIZE         2  /* depth of the color buffer  */
#define GLR_LEVEL               3  /* level in plane stacking */
#define GLR_RGBA                4  /* true if RGBA mode */
#define GLR_DOUBLEBUFFER        5  /* double buffering supported */
#define GLR_STEREO              6  /* stereo buffering supported */
#define GLR_AUX_BUFFERS         7  /* number of aux buffers */
#define GLR_RED_SIZE            8  /* number of red component bits */
#define GLR_GREEN_SIZE          9  /* number of green component  bits */
#define GLR_BLUE_SIZE           10  /* number of blue component  bits */
#define GLR_ALPHA_SIZE          11  /* number of alpha component bits */
#define GLR_DEPTH_SIZE          12  /* number of depth bits */
#define GLR_STENCIL_SIZE        13  /* number of stencil bits */
#define GLR_ACCUM_RED_SIZE      14  /* number of red accum bits  */
#define GLR_ACCUM_GREEN_SIZE    15  /* number of green accum bits */
#define GLR_ACCUM_BLUE_SIZE     16  /* number of blue accum bits */
#define GLR_ACCUM_ALPHA_SIZE    17  /* number of alpha accum bits */
#define GLR_SAMPLES_SGIS        100000  /* number of samples per pixel */
#define GLR_SAMPLE_BUFFER_SGIS  100001  /* the number of multisample buffers */
#define GLR_SCREEN_HINT         808080  /* hint for screen assignment */

typedef struct _GLrSession *GLrSession;
typedef struct _GLrCanvasType *GLrCanvasType;
typedef struct _GLrCanvas *GLrCanvas;

/* glrOpenSession - open a connection to GLR rendering server.  Returns NULL
   if the session could not be established.  If hostname is NULL, the
   GLR_SERVER environment variable is used.  If GLR_SERVER is not set or the
   empty string ("") is passed for hostname, glrOpenSession tries to find a
   GLR server on the local machine. */
extern GLrSession glrOpenSession (char *hostname);

/* glrCloseSession - close an open GLR rendering server created by
   glrOpenSession. Any allocate GLrCanvasType and/or GLrCanvas objects are
   automatically freed and/or destroyed. */
extern void glrCloseSession (GLrSession session);

/* glrGetCanvasType - obtain a canvas type by supplying a zero terminated
   list of GLR attributes.  The semantics are the same as those for
   glXChooseVisual. For a multi-headed GLR server, a canvas type does not
   determine a particular screen.   NULL is returned if the described canvas
   type is not supported.  */
extern GLrCanvasType glrGetCanvasType (GLrSession session, int *attribList);

/* glrFreeCanvasType - free a canvas type allocated with glrGetCanvasType.  */
extern void glrFreeCanvasType (GLrCanvasType canvasType);

/* glrGetTimeoutLimit - return the server-wide timeout ceiling for rendering
   interval (in milliseconds).  The interval is determine by the system
   manager. */
extern int glrGetTimeoutLimit (GLrSession session);

/* glrCreateCanvas - create a canvas with the same attributes as the
   GLrCanvasType parameter.  The newly created canvas will share displays
   lists (only within the same address space) with the canvas specied by
   shareList. */
extern GLrCanvas glrCreateCanvas (GLrCanvasType canvasType, GLrCanvas shareList);

/* glrGetMaxCanvasHeight - returns the maximum canvas height in pixels.
   Potentially, different canvases (for example on different screens) may
   have different heights.  You are guaranteed to be able to initiate a
   rendering inteval witht the specified height in pixels.  If you choose a
   larger value, you will be silently truncated to the maximum height. */
extern int glrGetMaxCanvasHeight (GLrCanvas canvas);

/* glrGetMaxCanvasWidth - returns the maximum canvas width in pixels.
   Potentially, different canvases (for example on different screens) may
   have different widths.  You are guaranteed to be able to initiate a
   rendering inteval witht the specified width in pixels.  If you choose a
   larger value, you will be silently truncated to the maximum width. */
extern int glrGetMaxCanvasWidth (GLrCanvas canvas);

/* glrDestroyCanvas - destroys the specified canvas.  */
extern void glrDestroyCanvas (GLrCanvas canvas);

/* glrEstablishRenderState - "makes current" to the specified canvas so OpenGL
   routines can be called to initialize the OpenGL state machine, but without 
   a frame buffer.  This can be used outside a rendering interval to
   initialize displays, texture objects, etc. outside of a rendering interval 
   so frame buffer resource do not have to be instatiated.  A return value of 
   1 indicates success, 0 indicates failure. */
extern int glrEstablishRenderState (GLrCanvas canvas);

/* glrBeginRenderInterval - "makes current" to the specified canvas of the
   specified width and height in pixels.  OpenGL routines can be called and a 
   frame buffer is instantiated.  The timeout specifies the number of
   milliseconds the canvas frame buffer is guaranteed to be available
   (limited by the server timeout ceiling).  After the timeout expires, the
   GLR server may (if other clients need the resources invalidate the canvas
   frame buffer.  Once invalidated, the affect of OpenGL frame buffer 
   operations is undefined.  The wait parameter specifies how long in
   milliseconds that glrBeginRenderInterval will block waiting to begin a
   render interval.  A return value of 1 indicates success, 0 indicates
   failure. */
extern int glrBeginRenderInterval (GLrCanvas canvas, int width, int height, int timeout, int wait);

/* glrQueryRenderIntervalStatus - checks if the current render interval is
   expired.  A return value of 1 indicates the render interval is not
   expired.  A return value of 0 indicates the render interval has been
   expired. */
extern int glrQueryRenderIntervalStatus (GLrCanvas canvas);

/* glrResumeRenderInterval - "makes current" back to a canvas within a
   rendering interval.  This can be used if some other API routine (like
   GLX's glXMakeCurrent) changes the thread's OpenGL window/context binding.
   You should not attempt to "make current" to another GLR context from the
   same server since deadlock may result (though the deadlock should
   eventually timeout).  A return value of 1 indicates success, 0 indicates
   failure. */
extern int glrResumeRenderInterval (GLrCanvas canvas);

/* glrEndRenderInterval - completes a rendering interval.  A return value of
   1 indicates  the rendering interval completed without any invalidation of
   the frame buffer. Any and all frame buffer results read back during the
   render interval are valid.  0 indicates the render interval was aborted
   before the interval was completed, meaning frame buffer results are likely 
   to be undefined.  The duration pointer (if not NULL) returns the amount of 
   time in milliseconds during the render interval to form improved estimates 
   on the needed render interval timeout. */
extern int glrEndRenderInterval (GLrCanvas canvas, int *duration);

/* glrQueryQueueLength - return estimate of how many milliseconds of work
   is pending on the render interval queue associated with the specified
   canvas. */
extern int glrQueryQueueLength (GLrCanvas canvas, int *pending);

#ifdef __cplusplus
}
#endif

#endif /* __glr_h__ */
