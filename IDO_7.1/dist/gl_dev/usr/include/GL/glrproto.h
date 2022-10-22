#ifndef __glrproto_h__
#define __glrproto_h__

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

/* Mark J. Kilgard */

/* Revision history for GLR protocol:

   1.1   June 5, 1995 

   The GLR session property describes global GLR session parameters. The
   property is placed by glrmanager on the root window of screen 0 at
   generation startup.  The property is named GLR_SESSION and is of type
   GLR_SESSION.  The GLR session property is fixed for the duration of the
   GLR server's generation.

   The GLR canvas property describing GLR per-canvas parameters.  The
   property is placed on each GLR canvas before the canvas is mapped.
   glrmanager will track changes to the GLR canvas property over the
   life-time of the window.  End of revisions. */

#define GLR_SESSION "GLR_SESSION"

typedef struct _GLrSessionInfo {
  long version;         /* version of GLR protocol, currently 1 */
  long revision;        /* revision of GLR protocol, currently 1 */
  long maxTimeout;      /* maximum guaranteed for rendering interval */
} GLrSessionInfo;

#define GLR_CANVAS "GLR_CANVAS"

typedef struct _GLrCanvasInfo {
  long version;         /* version of GLR protocol, currently 1 */
  long revision;        /* revision of GLR protocol, currently 1 */
  long width;           /* width of canvas in pixels */
  long height;          /* height of canvas in pixels */
  long timeout;         /* requested guaranteed milliseconds for rendering
                           interval */
} GLrCanvasInfo;

#endif /* __glrproto_h__ */
