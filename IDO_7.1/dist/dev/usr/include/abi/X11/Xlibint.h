/* $XConsortium: Xlibint.h,v 11.97.1.1 93/04/26 17:54:27 mor Exp $ */
/* Copyright 1984, 1985, 1987, 1989  Massachusetts Institute of Technology */

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

/*
 *	Xlibint.h - Header definition and support file for the internal
 *	support routines used by the C subroutine interface
 *	library (Xlib) to the X Window System.
 *
 *	Warning, there be dragons here....
 */

#ifdef _ABI_SOURCE
#undef SGI_SHM_TRANS
#endif

#ifndef NEED_EVENTS
#define _XEVENT_
#endif

#include <X11/Xlib.h>

struct _XGC {
    XExtData *ext_data;	/* hook for extension to hang data */
    GContext gid;	/* protocol ID for graphics context */
    Bool rects;		/* boolean: TRUE if clipmask is list of rectangles */
    Bool dashes;	/* boolean: TRUE if dash-list is really a list */
    unsigned long dirty;/* cache dirty bits */
    XGCValues values;	/* shadow structure of values */
};

struct _XDisplay {
	XExtData *ext_data;	/* hook for extension to hang data */
	struct _XFreeFuncs *free_funcs; /* internal free functions */
	int fd;			/* Network socket. */
	int conn_checker;         /* ugly thing used by _XEventsQueued */
	int proto_major_version;/* maj. version of server's X protocol */
	int proto_minor_version;/* minor version of servers X protocol */
	char *vendor;		/* vendor of the server hardware */
        XID resource_base;	/* resource ID base */
	XID resource_mask;	/* resource ID mask bits */
	XID resource_id;	/* allocator current ID */
	int resource_shift;	/* allocator shift to correct bits */
#if NeedFunctionPrototypes
	XID (*resource_alloc)(Display *);/* allocator function */
#else 
	XID (*resource_alloc)();/* allocator function */
#endif
	int byte_order;		/* screen byte order, LSBFirst, MSBFirst */
	int bitmap_unit;	/* padding and data requirements */
	int bitmap_pad;		/* padding requirements on bitmaps */
	int bitmap_bit_order;	/* LeastSignificant or MostSignificant */
	int nformats;		/* number of pixmap formats in list */
	ScreenFormat *pixmap_format;	/* pixmap format list */
	int vnumber;		/* Xlib's X protocol version number. */
	int release;		/* release of the server */
	struct _XSQEvent *head, *tail;	/* Input event queue. */
	int qlen;		/* Length of input event queue */
	unsigned long last_request_read; /* seq number of last event read */
	unsigned long request;	/* sequence number of last request. */
	char *last_req;		/* beginning of last request, or dummy */
	char *buffer;		/* Output buffer starting address. */
	char *bufptr;		/* Output buffer index pointer. */
	char *bufmax;		/* Output buffer maximum+1 address. */
	unsigned max_request_size; /* maximum number 32 bit words in request*/
	struct _XrmHashBucketRec *db;
#if NeedFunctionPrototypes
	int (*synchandler)(Display *);	/* Synchronization handler */
#else 
	int (*synchandler)();	/* Synchronization handler */
#endif
	char *display_name;	/* "host:display" string used on this connect*/
	int default_screen;	/* default screen for operations */
	int nscreens;		/* number of screens on this server*/
	Screen *screens;	/* pointer to list of screens */
	unsigned long motion_buffer;	/* size of motion buffer */
	unsigned long flags;	   /* internal connection flags */
	int min_keycode;	/* minimum defined keycode */
	int max_keycode;	/* maximum defined keycode */
	KeySym *keysyms;	/* This server's keysyms */
	XModifierKeymap *modifiermap;	/* This server's modifier keymap */
	int keysyms_per_keycode;/* number of rows */
	char *xdefaults;	/* contents of defaults from server */
	char *scratch_buffer;	/* place to hang scratch buffer */
	unsigned long scratch_length;	/* length of scratch buffer */
	int ext_number;		/* extension number on this display */
	struct _XExten *ext_procs; /* extensions initialized on this display */
	/*
	 * the following can be fixed size, as the protocol defines how
	 * much address space is available. 
	 * While this could be done using the extension vector, there
	 * may be MANY events processed, so a search through the extension
	 * list to find the right procedure for each event might be
	 * expensive if many extensions are being used.
	 */
	Bool (*event_vec[128])();  /* vector for wire to event */
	Status (*wire_vec[128])(); /* vector for event to wire */
	KeySym lock_meaning;	   /* for XLookupString */
	XPointer lock;		/* is someone in critical section? */
	struct _XInternalAsync *async_handlers; /* for internal async */
	unsigned long bigreq_size; /* max size of big requests */
	/* things above this line should not move, for binary compatibility */
	struct _XKeytrans *key_bindings; /* for XLookupString */
	Font cursor_font;	   /* for XCreateFontCursor */
	struct _XDisplayAtoms *atoms; /* for XInternAtom */
	unsigned int mode_switch;  /* keyboard group modifiers */
	struct _XContextDB *context_db; /* context database */
	Bool (**error_vec)();      /* vector for wire to error */
	/*
	 * Xcms information
	 */
	struct {
	   XPointer defaultCCCs;  /* pointer to an array of default XcmsCCC */
	   XPointer clientCmaps;  /* pointer to linked list of XcmsCmapRec */
	   XPointer perVisualIntensityMaps;
				  /* linked list of XcmsIntensityMap */
	} cms;
	struct _XIMFilter *im_filters;
	unsigned char pad1;
	unsigned char pad2;
	unsigned char level3_shift;  /* level 3 Shift modifiers */
	unsigned char num_lock;     /* numlock modifiers */
#ifndef NO_XKB_EXTENSION
	unsigned	 xkb_flags;
	int		 xkb_ext_number;
	int		 xkb_ext_major_opcode;
	int		 xkb_ext_event_base;
	int		 xkb_ext_error_base;
	int		 xkb_srv_major;
	int		 xkb_srv_minor;
	unsigned	 xkb_selected_events;
	struct _XkbInfoRec	*xkb_info;
#endif
#ifdef SGI_SHM_TRANS
        struct _shmdata *shmdata;  /* used for sgi shared memory transport */
#endif /* SGI_SHM_TRANS */
};

#ifndef _ABI_SOURCE

/*
 * define the following if you want the Data macro to be a procedure instead
 */
#ifdef CRAY
#define DataRoutineIsProcedure
#endif /* CRAY */

#ifndef _XEVENT_
/*
 * _QEvent datatype for use in input queueing.
 */
typedef struct _XSQEvent {
    struct _XSQEvent *next;
    XEvent event;
} _XQEvent;
#endif

#endif /* !_ABI_SOURCE */

#if NeedFunctionPrototypes	/* prototypes require event type definitions */
#define NEED_EVENTS
#endif
#include <X11/Xproto.h>

#ifndef _ABI_SOURCE

#ifdef __sgi
#define _SGI_MP_SOURCE	/* turn this on to get MP safe errno */
#endif /* __sgi */

#include <errno.h>

#ifdef __sgi
#undef _SGI_MP_SOURCE	/* turn it off again to avoid getting (slower) MP safe stdio */
#endif /* __sgi */

#endif /* !_ABI_SOURCE */

#define _XBCOPYFUNC _Xbcopy
#include <X11/Xfuncs.h>
#include <X11/Xosdefs.h>

#ifdef SGI_SHM_TRANS
#include <ulocks.h>
#include "XShmTrans.h"
#endif /* SGI_SHM_TRANS */

/* Utek leaves kernel macros around in include files (bleah) */
#ifdef dirty
#undef dirty
#endif

#ifdef CRAY
#define WORD64
#endif

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#include <string.h>
#else
char *malloc(), *realloc(), *calloc();
void exit();
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
#endif
#endif
#if defined(macII) && !defined(__STDC__)  /* stdlib.h fails to define these */
char *malloc(), *realloc(), *calloc();
#endif /* macII */

/*
 * The following definitions can be used for locking requests in multi-threaded
 * address spaces.
 */
#define LockDisplay(dis)
#define LockMutex(mutex)
#define UnlockMutex(mutex)
#define UnlockDisplay(dis)
#define Xfree(ptr) free((ptr))

/*
 * Note that some machines do not return a valid pointer for malloc(0), in
 * which case we provide an alternate under the control of the
 * define MALLOC_0_RETURNS_NULL.  This is necessary because some
 * Xlib code expects malloc(0) to return a valid pointer to storage.
 */
#ifdef MALLOC_0_RETURNS_NULL

# define Xmalloc(size) malloc(((size) > 0 ? (size) : 1))
# define Xrealloc(ptr, size) realloc((ptr), ((size) > 0 ? (size) : 1))
# define Xcalloc(nelem, elsize) calloc(((nelem) > 0 ? (nelem) : 1), (elsize))

#else

# define Xmalloc(size) malloc((size))
# define Xrealloc(ptr, size) realloc((ptr), (size))
# define Xcalloc(nelem, elsize) calloc((nelem), (elsize))

#endif

#ifndef _ABI_SOURCE

#ifndef NULL
#define NULL 0
#endif
#define LOCKED 1
#define UNLOCKED 0

extern int errno;			/* Internal system error number. */

#ifndef BUFSIZE
#define BUFSIZE 2048			/* X output buffer size. */
#endif
#ifndef PTSPERBATCH
#define PTSPERBATCH 1024		/* point batching */
#endif
#ifndef WLNSPERBATCH
#define WLNSPERBATCH 50			/* wide line batching */
#endif
#ifndef ZLNSPERBATCH
#define ZLNSPERBATCH 1024		/* thin line batching */
#endif
#ifndef WRCTSPERBATCH
#define WRCTSPERBATCH 10		/* wide line rectangle batching */
#endif
#ifndef ZRCTSPERBATCH
#define ZRCTSPERBATCH 256		/* thin line rectangle batching */
#endif
#ifndef FRCTSPERBATCH
#define FRCTSPERBATCH 256		/* filled rectangle batching */
#endif
#ifndef FARCSPERBATCH
#define FARCSPERBATCH 256		/* filled arc batching */
#endif
#ifndef CURSORFONT
#define CURSORFONT "cursor"		/* standard cursor fonts */
#endif

/*
 * display flags
 */
#define XlibDisplayIOError	(1L << 0)
#define XlibDisplayClosing	(1L << 1)
#define	XlibDisplayNoXkb	(1L << 2)

#endif /* !_ABI_SOURCE */

/*
 * X Protocol packetizing macros.
 */

/*   Need to start requests on 64 bit word boundries
 *   on a CRAY computer so add a NoOp (127) if needed.
 *   A character pointer on a CRAY computer will be non-zero
 *   after shifting right 61 bits of it is not pointing to
 *   a word boundary.
 */
#ifdef WORD64
#define WORD64ALIGN if ((long)dpy->bufptr >> 61) {\
           dpy->last_req = dpy->bufptr;\
           *(dpy->bufptr)   = X_NoOperation;\
           *(dpy->bufptr+1) =  0;\
           *(dpy->bufptr+2) =  0;\
           *(dpy->bufptr+3) =  1;\
             dpy->request++;\
             dpy->bufptr += 4;\
         }
#else /* else does not require alignment on 64-bit boundaries */
#define WORD64ALIGN
#endif /* WORD64 */

/* macros used by shared memory transport */
#ifdef SGI_SHM_TRANS

/* do n bytes fit in buffer?? */
#define SHM_BYTES_FIT( head, tail, endofbuf, n) \
	( ( (tail) <= (head) ) ? \
		( ( (head) + (n) ) < (endofbuf) ) : \
		( ( (head) + (n) ) < (tail) ) )

#define _XShmAlloc( ret, type, my_shmptr, my_dpy, nbytes ) \
{ \
	arenahdr_t *my_arenahdr = (my_shmptr)->arenahdr; \
	unsigned char *my_tailptr = (unsigned char *)my_arenahdr->tailptr; \
	if ( (my_dpy)->bufptr != (my_dpy)->buffer ) \
		_XShmCompatBufCopy(my_dpy); \
	if ( SHM_BYTES_FIT( (my_shmptr)->reqendptr, my_tailptr, \
				my_arenahdr->shmbufend, (nbytes) ) ) \
	{ \
/* printf("_XShmAlloc:n = %d, ret = 0x%x, tail = 0x%x\n", nbytes, (my_shmptr)->reqendptr, my_tailptr); */ \
		(ret) = (type)((my_shmptr)->reqendptr); \
		(my_shmptr)->reqendptr += nbytes; \
	} \
	else \
	{ \
		(ret) = (type) _XShmRealAlloc( (my_dpy), (nbytes) ); \
	} \
}
#endif /* SGI_SHM_TRANS */

/*
 * GetReq - Get the next avilable X request packet in the buffer and
 * return it. 
 *
 * "name" is the name of the request, e.g. CreatePixmap, OpenFont, etc.
 * "req" is the name of the request pointer.
 *
 */

#ifdef SGI_SHM_TRANS

#if defined(__STDC__) && !defined(UNIXCPP)
#define GetReq(name, req) \
    WORD64ALIGN\
    { \
	shmdata_t *shmdata = dpy->shmdata;\
	if ( shmdata ) \
	{ \
	    shmdata->curreqptr = shmdata->reqendptr; \
	    _XShmAlloc(req, x##name##Req *, shmdata, dpy, SIZEOF(x##name##Req) );\
	    shmdata->curreqptr = (unsigned char *) req ; \
	} \
	else \
	{ \
	    if ((dpy->bufptr + SIZEOF(x##name##Req)) > dpy->bufmax)\
		_XFlush(dpy);\
	    req = (x##name##Req *)(dpy->last_req = dpy->bufptr);\
	    dpy->bufptr += SIZEOF(x##name##Req);\
	} \
    } \
    req->reqType = X_##name;\
    req->length = (SIZEOF(x##name##Req))>>2;\
    dpy->request++

#else  /* non-ANSI C uses empty comment instead of "##" for token concatenation */
#define GetReq(name, req) \
    WORD64ALIGN\
    { \
	shmdata_t *shmdata = dpy->shmdata;\
	if ( shmdata ) \
	{ \
	    shmdata->curreqptr = shmdata->reqendptr; \
	    _XShmAlloc(req, x/**/name/**/Req *, shmdata, dpy, SIZEOF(x/**/name/**/Req) );\
	    shmdata->curreqptr = (unsigned char *) req ; \
	} \
	else \
	{ \
	    if ((dpy->bufptr + SIZEOF(x/**/name/**/Req)) > dpy->bufmax)\
		_XFlush(dpy);\
	    req = (x/**/name/**/Req *)(dpy->last_req = dpy->bufptr);\
	    dpy->bufptr += SIZEOF(x/**/name/**/Req);\
	} \
    } \
    req->reqType = X_/**/name;\
    req->length = (SIZEOF(x/**/name/**/Req))>>2;\
    dpy->request++
#endif

#else /* SGI_SHM_TRANS */

#if defined(__STDC__) && !defined(UNIXCPP)
#define GetReq(name, req) \
        WORD64ALIGN\
	if ((dpy->bufptr + SIZEOF(x##name##Req)) > dpy->bufmax)\
		_XFlush(dpy);\
	req = (x##name##Req *)(dpy->last_req = dpy->bufptr);\
	req->reqType = X_##name;\
	req->length = (SIZEOF(x##name##Req))>>2;\
	dpy->bufptr += SIZEOF(x##name##Req);\
	dpy->request++

#else  /* non-ANSI C uses empty comment instead of "##" for token concatenation */
#define GetReq(name, req) \
        WORD64ALIGN\
	if ((dpy->bufptr + SIZEOF(x/**/name/**/Req)) > dpy->bufmax)\
		_XFlush(dpy);\
	req = (x/**/name/**/Req *)(dpy->last_req = dpy->bufptr);\
	req->reqType = X_/**/name;\
	req->length = (SIZEOF(x/**/name/**/Req))>>2;\
	dpy->bufptr += SIZEOF(x/**/name/**/Req);\
	dpy->request++
#endif

#endif /* SGI_SHM_TRANS */

/* GetReqExtra is the same as GetReq, but allocates "n" additional
   bytes after the request. "n" must be a multiple of 4!  */

#ifdef SGI_SHM_TRANS

#if defined(__STDC__) && !defined(UNIXCPP)
#define GetReqExtra(name, n, req) \
    WORD64ALIGN\
    { \
	shmdata_t *shmdata = dpy->shmdata;\
	if ( shmdata ) \
	{ \
	    shmdata->curreqptr = shmdata->reqendptr; \
	    _XShmAlloc(req, x##name##Req *, shmdata, dpy, SIZEOF(x##name##Req) + n );\
	    shmdata->curreqptr = (unsigned char *) req ; \
	} \
	else \
	{ \
	    if ((dpy->bufptr + SIZEOF(x##name##Req) + n) > dpy->bufmax)\
		_XFlush(dpy);\
	    req = (x##name##Req *)(dpy->last_req = dpy->bufptr);\
	    dpy->bufptr += SIZEOF(x##name##Req) + n;\
	} \
    } \
    req->reqType = X_##name;\
    req->length = (SIZEOF(x##name##Req) + n)>>2;\
    dpy->request++
#else
#define GetReqExtra(name, n, req) \
    WORD64ALIGN\
    { \
	shmdata_t *shmdata = dpy->shmdata;\
	if ( shmdata ) \
	{ \
	    shmdata->curreqptr = shmdata->reqendptr; \
	    _XShmAlloc(req, x/**/name/**/Req *, shmdata, dpy, SIZEOF(x/**/name/**/Req) + n );\
	    shmdata->curreqptr = (unsigned char *) req ; \
	} \
	else \
	{ \
	    if ((dpy->bufptr + SIZEOF(x/**/name/**/Req) + n) > dpy->bufmax)\
		_XFlush(dpy);\
	    req = (x/**/name/**/Req *)(dpy->last_req = dpy->bufptr);\
	    dpy->bufptr += SIZEOF(x/**/name/**/Req) + n;\
	} \
    } \
    req->reqType = X_/**/name;\
    req->length = (SIZEOF(x/**/name/**/Req) + n)>>2;\
    dpy->request++
#endif

#else /* SGI_SHM_TRANS */

#if defined(__STDC__) && !defined(UNIXCPP)
#define GetReqExtra(name, n, req) \
        WORD64ALIGN\
	if ((dpy->bufptr + SIZEOF(x##name##Req) + n) > dpy->bufmax)\
		_XFlush(dpy);\
	req = (x##name##Req *)(dpy->last_req = dpy->bufptr);\
	req->reqType = X_##name;\
	req->length = (SIZEOF(x##name##Req) + n)>>2;\
	dpy->bufptr += SIZEOF(x##name##Req) + n;\
	dpy->request++
#else
#define GetReqExtra(name, n, req) \
        WORD64ALIGN\
	if ((dpy->bufptr + SIZEOF(x/**/name/**/Req) + n) > dpy->bufmax)\
		_XFlush(dpy);\
	req = (x/**/name/**/Req *)(dpy->last_req = dpy->bufptr);\
	req->reqType = X_/**/name;\
	req->length = (SIZEOF(x/**/name/**/Req) + n)>>2;\
	dpy->bufptr += SIZEOF(x/**/name/**/Req) + n;\
	dpy->request++
#endif

#endif /* SGI_SHM_TRANS */

/*
 * GetResReq is for those requests that have a resource ID 
 * (Window, Pixmap, GContext, etc.) as their single argument.
 * "rid" is the name of the resource. 
 */

#ifdef SGI_SHM_TRANS

#if defined(__STDC__) && !defined(UNIXCPP)
#define GetResReq(name, rid, req) \
    WORD64ALIGN\
    { \
	shmdata_t *shmdata = dpy->shmdata;\
	if ( shmdata ) \
	{ \
	    shmdata->curreqptr = shmdata->reqendptr; \
	    _XShmAlloc(req, xResourceReq *, shmdata, dpy, SIZEOF(xResourceReq) );\
	    shmdata->curreqptr = (unsigned char *) req ; \
	} \
	else \
	{ \
	    if ((dpy->bufptr + SIZEOF(xResourceReq)) > dpy->bufmax)\
		_XFlush(dpy);\
	    req = (xResourceReq *) (dpy->last_req = dpy->bufptr);\
	    dpy->bufptr += SIZEOF(xResourceReq);\
	} \
    } \
    req->reqType = X_##name;\
    req->length = 2;\
    req->id = (rid);\
    dpy->request++
#else
#define GetResReq(name, rid, req) \
    WORD64ALIGN\
    { \
	shmdata_t *shmdata = dpy->shmdata;\
	if ( shmdata ) \
	{ \
	    shmdata->curreqptr = shmdata->reqendptr; \
	    _XShmAlloc(req, xResourceReq *, shmdata, dpy, SIZEOF(xResourceReq) );\
	    shmdata->curreqptr = (unsigned char *) req ; \
	} \
	else \
	{ \
	    if ((dpy->bufptr + SIZEOF(xResourceReq)) > dpy->bufmax)\
		_XFlush(dpy);\
	    req = (xResourceReq *) (dpy->last_req = dpy->bufptr);\
	    dpy->bufptr += SIZEOF(xResourceReq);\
	} \
    } \
    req->reqType = X_/**/name;\
    req->length = 2;\
    req->id = (rid);\
    dpy->request++
#endif

#else /* SGI_SHM_TRANS */

#if defined(__STDC__) && !defined(UNIXCPP)
#define GetResReq(name, rid, req) \
        WORD64ALIGN\
	if ((dpy->bufptr + SIZEOF(xResourceReq)) > dpy->bufmax)\
	    _XFlush(dpy);\
	req = (xResourceReq *) (dpy->last_req = dpy->bufptr);\
	req->reqType = X_##name;\
	req->length = 2;\
	req->id = (rid);\
	dpy->bufptr += SIZEOF(xResourceReq);\
	dpy->request++
#else
#define GetResReq(name, rid, req) \
        WORD64ALIGN\
	if ((dpy->bufptr + SIZEOF(xResourceReq)) > dpy->bufmax)\
	    _XFlush(dpy);\
	req = (xResourceReq *) (dpy->last_req = dpy->bufptr);\
	req->reqType = X_/**/name;\
	req->length = 2;\
	req->id = (rid);\
	dpy->bufptr += SIZEOF(xResourceReq);\
	dpy->request++
#endif

#endif /* SGI_SHM_TRANS */

/*
 * GetEmptyReq is for those requests that have no arguments
 * at all. 
 */

#ifdef SGI_SHM_TRANS

#if defined(__STDC__) && !defined(UNIXCPP)
#define GetEmptyReq(name, req) \
    WORD64ALIGN\
    { \
	shmdata_t *shmdata = dpy->shmdata;\
	if ( shmdata ) \
	{ \
	    shmdata->curreqptr = shmdata->reqendptr; \
	    _XShmAlloc(req, xReq *, shmdata, dpy, SIZEOF(xReq) );\
	    shmdata->curreqptr = (unsigned char *) req ; \
	} \
	else \
	{ \
	    if ((dpy->bufptr + SIZEOF(xReq)) > dpy->bufmax)\
		_XFlush(dpy);\
	    req = (xReq *) (dpy->last_req = dpy->bufptr);\
	    dpy->bufptr += SIZEOF(xReq);\
	} \
    } \
    req->reqType = X_##name;\
    req->length = 1;\
    dpy->request++
#else
#define GetEmptyReq(name, req) \
    WORD64ALIGN\
    { \
	shmdata_t *shmdata = dpy->shmdata;\
	if ( shmdata ) \
	{ \
	    shmdata->curreqptr = shmdata->reqendptr; \
	    _XShmAlloc(req, xReq *, shmdata, dpy, SIZEOF(xReq) );\
	    shmdata->curreqptr = (unsigned char *) req ; \
	} \
	else \
	{ \
	    if ((dpy->bufptr + SIZEOF(xReq)) > dpy->bufmax)\
		_XFlush(dpy);\
	    req = (xReq *) (dpy->last_req = dpy->bufptr);\
	    dpy->bufptr += SIZEOF(xReq);\
	} \
    } \
    req->reqType = X_/**/name;\
    req->length = 1;\
    dpy->request++
#endif

#else /* SGI_SHM_TRANS */

#if defined(__STDC__) && !defined(UNIXCPP)
#define GetEmptyReq(name, req) \
        WORD64ALIGN\
	if ((dpy->bufptr + SIZEOF(xReq)) > dpy->bufmax)\
	    _XFlush(dpy);\
	req = (xReq *) (dpy->last_req = dpy->bufptr);\
	req->reqType = X_##name;\
	req->length = 1;\
	dpy->bufptr += SIZEOF(xReq);\
	dpy->request++
#else
#define GetEmptyReq(name, req) \
        WORD64ALIGN\
	if ((dpy->bufptr + SIZEOF(xReq)) > dpy->bufmax)\
	    _XFlush(dpy);\
	req = (xReq *) (dpy->last_req = dpy->bufptr);\
	req->reqType = X_/**/name;\
	req->length = 1;\
	dpy->bufptr += SIZEOF(xReq);\
	dpy->request++
#endif

#endif /* SGI_SHM_TRANS */

#ifdef WORD64
#define MakeBigReq(req,n) \
    { \
    char _BRdat[4]; \
    unsigned long _BRlen = req->length - 1; \
    req->length = 0; \
    bcopy(((char *)req) + (_BRlen << 2), _BRdat, 4); \
    bcopy(((char *)req) + 4, ((char *)req) + 8, _BRlen << 2); \
    bcopy(_BRdat, ((char *)req) + 4, 4); \
    Data32(dpy, (long *)&_BRdat, 4); \
    }
#else
#define MakeBigReq(req,n) \
    { \
    long _BRdat; \
    unsigned long _BRlen = req->length - 1; \
    req->length = 0; \
    _BRdat = ((long *)req)[_BRlen]; \
    bcopy(((char *)req) + 4, ((char *)req) + 8, _BRlen << 2); \
    ((unsigned long *)req)[1] = _BRlen + n + 2; \
    Data32(dpy, &_BRdat, 4); \
    }
#endif

#define SetReqLen(req,n,badlen) \
    if ((req->length + n) > (unsigned)65535) { \
	if (dpy->bigreq_size) { \
	    MakeBigReq(req,n) \
	} else { \
	    n = badlen; \
	    req->length += n; \
	} \
    } else \
	req->length += n

#define SyncHandle() \
	if (dpy->synchandler) (*dpy->synchandler)(dpy)

#define FlushGC(dpy, gc) \
	if ((gc)->dirty) _XFlushGCCache((dpy), (gc))
/*
 * Data - Place data in the buffer and pad the end to provide
 * 32 bit word alignment.  Transmit if the buffer fills.
 *
 * "dpy" is a pointer to a Display.
 * "data" is a pinter to a data buffer.
 * "len" is the length of the data buffer.
 * we can presume buffer less than 2^16 bytes, so bcopy can be used safely.
 */
#ifndef DataRoutineIsProcedure

# ifdef SGI_SHM_TRANS

#define shmbcopy(src, dest, len) \
	if ( ( (int)(src) | (int)(dest) | (int)(len) ) & 0x3 ) \
	{ /* unaligned in some way */ \
		bcopy( (src), (dest), (len) ); \
	} \
	else /* src and dest are word aligned, and length is mod 4 */ \
	{ \
		int wordlen = (len) >> 2; \
		if ( wordlen > 31 ) \
		{ \
			_XShmFastBcopy( (src), (dest), (len) ); \
		} \
		else \
		{ \
			volatile unsigned int *mysrc; \
			volatile unsigned int *mydest; \
			mysrc = (volatile unsigned int *)(src); \
			mydest = (volatile unsigned int *)(dest); \
			while( wordlen >= 8 ) \
			{ \
			    unsigned long int t1, t2, t3, t4; \
			    t1 = mysrc[0]; \
			    t2 = mysrc[1]; \
			    t3 = mysrc[2]; \
			    t4 = mysrc[3]; \
			    mydest[0] = t1; \
			    mydest[1] = t2; \
			    mydest[2] = t3; \
			    mydest[3] = t4; \
			    t1 = mysrc[4]; \
			    t2 = mysrc[5]; \
			    t3 = mysrc[6]; \
			    t4 = mysrc[7]; \
			    mydest[4] = t1; \
			    mydest[5] = t2; \
			    mydest[6] = t3; \
			    mydest[7] = t4; \
			    wordlen -= 8; \
			    mydest += 8; \
			    mysrc += 8; \
			} \
			switch(wordlen) { \
				case 7: \
					mydest[6] = mysrc[6]; \
				case 6: \
					mydest[5] = mysrc[5]; \
				case 5: \
					mydest[4] = mysrc[4]; \
				case 4: \
					mydest[3] = mysrc[3]; \
				case 3: \
					mydest[2] = mysrc[2]; \
				case 2: \
					mydest[1] = mysrc[1]; \
				case 1: \
					mydest[0] = mysrc[0]; \
			} \
		} \
	}

#define Data(dpy, data, len) \
{ \
	shmdata_t *da_shmdata = dpy->shmdata;\
	if (da_shmdata) { \
		unsigned char *data_ptr; \
		unsigned int *src_ptr = (unsigned int *)(data); \
		int da_len = ( (len) + 3 ) & ~3; \
		_XShmAlloc(data_ptr, unsigned char *, da_shmdata, dpy, da_len);\
		shmbcopy( (int *)src_ptr, (int *)data_ptr, (len) ); \
	} else if (dpy->bufptr + (len) <= dpy->bufmax) {\
		bcopy(data, dpy->bufptr, (int)len);\
		dpy->bufptr += ((len) + 3) & ~3;\
	} else\
		_XSend(dpy,(void *) data, len); \
}

# else /* SGI_SHM_TRANS */

#define Data(dpy, data, len) \
	if (dpy->bufptr + (len) <= dpy->bufmax) {\
		bcopy(data, dpy->bufptr, (int)len);\
		dpy->bufptr += ((len) + 3) & ~3;\
	} else\
		_XSend(dpy, data, len)

# endif /* SGI_SHM_TRANS */

#endif /* DataRoutineIsProcedure */


/* Allocate bytes from the buffer.  No padding is done, so if
 * the length is not a multiple of 4, the caller must be
 * careful to leave the buffer aligned after sending the
 * current request.
 *
 * "type" is the type of the pointer being assigned to.
 * "ptr" is the pointer being assigned to.
 * "n" is the number of bytes to allocate.
 *
 * Example: 
 *    xTextElt *elt;
 *    BufAlloc (xTextElt *, elt, nbytes)
 */

#ifdef SGI_SHM_TRANS

#define BufAlloc(type, ptr, n) \
{ \
    shmdata_t *ba_shmdata = dpy->shmdata;\
    if ( ba_shmdata ) { \
	_XShmAlloc(ptr, type, ba_shmdata, dpy, (n));\
    } \
    else { \
	if (dpy->bufptr + (n) > dpy->bufmax) \
            _XFlush (dpy); \
	ptr = (type) dpy->bufptr; \
	dpy->bufptr += (n); \
    } \
}

#else /* SGI_SHM_TRANS */

#define BufAlloc(type, ptr, n) \
    if (dpy->bufptr + (n) > dpy->bufmax) \
        _XFlush (dpy); \
    ptr = (type) dpy->bufptr; \
    dpy->bufptr += (n);

#endif /* SGI_SHM_TRANS */

/*
 * provide emulation routines for smaller architectures
 */
#ifndef WORD64
#define Data16(dpy, data, len) Data((dpy), (char *)(data), (len))
#define Data32(dpy, data, len) Data((dpy), (char *)(data), (len))
#define _XRead16Pad(dpy, data, len) _XReadPad((dpy), (char *)(data), (len))
#define _XRead16(dpy, data, len) _XRead((dpy), (char *)(data), (len))
#define _XRead32(dpy, data, len) _XRead((dpy), (char *)(data), (len))
#endif /* not WORD64 */

#ifndef _ABI_SOURCE

#define PackData16(dpy,data,len) Data16 (dpy, data, len)
#define PackData32(dpy,data,len) Data32 (dpy, data, len)

/* Xlib manual is bogus */
#define PackData(dpy,data,len) PackData16 (dpy, data, len)

#define min(a,b) (((a) < (b)) ? (a) : (b))
#define max(a,b) (((a) > (b)) ? (a) : (b))

#define CI_NONEXISTCHAR(cs) (((cs)->width == 0) && \
			     (((cs)->rbearing|(cs)->lbearing| \
			       (cs)->ascent|(cs)->descent) == 0))

/* 
 * CI_GET_CHAR_INFO_1D - return the charinfo struct for the indicated 8bit
 * character.  If the character is in the column and exists, then return the
 * appropriate metrics (note that fonts with common per-character metrics will
 * return min_bounds).  If none of these hold true, try again with the default
 * char.
 */
#define CI_GET_CHAR_INFO_1D(fs,col,def,cs) \
{ \
    cs = def; \
    if (col >= fs->min_char_or_byte2 && col <= fs->max_char_or_byte2) { \
	if (fs->per_char == NULL) { \
	    cs = &fs->min_bounds; \
	} else { \
	    cs = &fs->per_char[(col - fs->min_char_or_byte2)]; \
	    if (CI_NONEXISTCHAR(cs)) cs = def; \
	} \
    } \
}

#define CI_GET_DEFAULT_INFO_1D(fs,cs) \
  CI_GET_CHAR_INFO_1D (fs, fs->default_char, NULL, cs)



/*
 * CI_GET_CHAR_INFO_2D - return the charinfo struct for the indicated row and 
 * column.  This is used for fonts that have more than row zero.
 */
#define CI_GET_CHAR_INFO_2D(fs,row,col,def,cs) \
{ \
    cs = def; \
    if (row >= fs->min_byte1 && row <= fs->max_byte1 && \
	col >= fs->min_char_or_byte2 && col <= fs->max_char_or_byte2) { \
	if (fs->per_char == NULL) { \
	    cs = &fs->min_bounds; \
	} else { \
	    cs = &fs->per_char[((row - fs->min_byte1) * \
			        (fs->max_char_or_byte2 - \
				 fs->min_char_or_byte2 + 1)) + \
			       (col - fs->min_char_or_byte2)]; \
	    if (CI_NONEXISTCHAR(cs)) cs = def; \
        } \
    } \
}

#define CI_GET_DEFAULT_INFO_2D(fs,cs) \
{ \
    unsigned int r = (fs->default_char >> 8); \
    unsigned int c = (fs->default_char & 0xff); \
    CI_GET_CHAR_INFO_2D (fs, r, c, NULL, cs); \
}


#ifdef MUSTCOPY

/* for when 32-bit alignment is not good enough */
#define OneDataCard32(dpy,dstaddr,srcvar) \
  { dpy->bufptr -= 4; Data32 (dpy, (char *) &(srcvar), 4); }

#else

/* srcvar must be a variable for large architecture version */
#define OneDataCard32(dpy,dstaddr,srcvar) \
  { *(unsigned long *)(dstaddr) = (srcvar); }

#endif /* MUSTCOPY */

typedef struct _XInternalAsync {
    struct _XInternalAsync *next;
    Bool (*handler)();
    XPointer data;
} _XAsyncHandler;

typedef struct _XAsyncEState {
    unsigned long min_sequence_number;
    unsigned long max_sequence_number;
    unsigned char error_code;
    unsigned char major_opcode;
    unsigned short minor_opcode;
    unsigned char last_error_received;
    int error_count;
} _XAsyncErrorState;

#define DeqAsyncHandler(dpy,handler) { \
    if (dpy->async_handlers == (handler)) \
	dpy->async_handlers = (handler)->next; \
    else \
	_XDeqAsyncHandler(dpy, handler); \
    }

/*
 * This structure is private to the library.
 */
typedef struct _XFreeFuncs {
    void (*atoms)();		/* _XFreeAtomTable */
    int (*modifiermap)();	/* XFreeModifierMap */
    void (*key_bindings)();	/* _XFreeKeyBindings */
    void (*context_db)();	/* _XFreeContextDB */
    void (*defaultCCCs)();	/* _XcmsFreeDefaultCCCs */
    void (*clientCmaps)();	/* _XcmsFreeClientCmaps */
    void (*intensityMaps)();	/* _XcmsFreeIntensityMaps */
    void (*im_filters)();	/* _XFreeIMFilters */
    void (*xkb)();		/* _XkbFreeInfo */
} _XFreeFuncRec;

/*
 * This structure is private to the library.
 */
typedef struct _XExten {	/* private to extension mechanism */
	struct _XExten *next;	/* next in list */
	XExtCodes codes;	/* public information, all extension told */
	int (*create_GC)();	/* routine to call when GC created */
	int (*copy_GC)();	/* routine to call when GC copied */
	int (*flush_GC)();	/* routine to call when GC flushed */
	int (*free_GC)();	/* routine to call when GC freed */
	int (*create_Font)();	/* routine to call when Font created */
	int (*free_Font)();	/* routine to call when Font freed */
	int (*close_display)();	/* routine to call when connection closed */
	int (*error)();		/* who to call when an error occurs */
        char *(*error_string)();  /* routine to supply error string */
	char *name;		/* name of this extension */
	void (*error_values)(); /* routine to supply error values */
} _XExtension;

#endif /* !_ABI_SOURCE */


/* extension hooks */

_XFUNCPROTOBEGIN

#ifndef _ABI_SOURCE

#ifdef DataRoutineIsProcedure
extern void Data();
#endif
extern int _XError();			/* prepare to upcall user handler */
extern int _XIOError();			/* prepare to upcall user handler */
extern int (*_XIOErrorFunction)();	/* X system error reporting routine. */
extern int (*_XErrorFunction)();	/* X_Error event reporting routine. */
#if NeedFunctionPrototypes
#ifdef SGI_SHM_TRANS
extern unsigned char *_XShmRealAlloc(Display *, unsigned long int);
extern void _XShmCompatBufCopy(Display *);
extern void _XShmFastBcopy(volatile int *, volatile int *, int);
#endif
#endif
extern Visual *_XVIDtoVisual();		/* given visual id, find structure */
extern int _XGetHostname();		/* get name of this machine */
extern Screen *_XScreenOfWindow ();	/* get Screen pointer for window */
extern Bool _XAsyncErrorHandler ();	/* internal error handler */
extern char *_XGetAsyncReply();		/* get async reply */

#endif /* !_ABI_SOURCE */

#if NeedFunctionPrototypes
extern void _XEatData(Display *, unsigned long);/* swallow data from server */
extern int _XFlush(Display *);
#ifdef NEED_REPLIES
extern Status _XReply(Display *, xReply *, int, Bool);
#endif
extern int _XRead(Display *, void *, long);
extern int _XSend(Display *, void *, long);
#else
extern void _XEatData();		/* swallow data from server */
#endif
extern char *_XAllocScratch();		/* fast memory allocator */
extern unsigned long _XSetLastRequestRead();	/* update dpy->last_request_read */

extern int (*XESetCreateGC(
#if NeedFunctionPrototypes
    Display*		/* display */,
    int			/* extension */,
    int (*) (
#if NeedNestedPrototypes
	      Display*			/* display */,
	      GC			/* gc */,
	      XExtCodes*		/* codes */
#endif
	    )		/* proc */
#endif
))(
#if NeedNestedPrototypes
    Display*, GC, XExtCodes*
#endif
);

extern int (*XESetCopyGC(
#if NeedFunctionPrototypes
    Display*		/* display */,
    int			/* extension */,
    int (*) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              GC			/* gc */,
              XExtCodes*		/* codes */
#endif
            )		/* proc */	      
#endif
))(
#if NeedNestedPrototypes
    Display*, GC, XExtCodes*
#endif
);

extern int (*XESetFlushGC(
#if NeedFunctionPrototypes
    Display*		/* display */,
    int			/* extenstion */,
    int (*) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              GC			/* gc */,
              XExtCodes*		/* codes */
#endif
            )		/* proc */	     
#endif
))(
#if NeedNestedPrototypes
    Display*, GC, XExtCodes*
#endif
);

extern int (*XESetFreeGC(
#if NeedFunctionPrototypes
    Display*		/* display */,
    int			/* extension */,
    int (*) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              GC			/* gc */,
              XExtCodes*		/* codes */
#endif
            )		/* proc */	     
#endif
))(
#if NeedNestedPrototypes
    Display*, GC, XExtCodes*
#endif
);

extern int (*XESetCreateFont(
#if NeedFunctionPrototypes
    Display*		/* display */,
    int			/* extension */,
    int (*) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              XFontStruct*		/* fs */,
              XExtCodes*		/* codes */
#endif
            )		/* proc */    
#endif
))(
#if NeedNestedPrototypes
    Display*, XFontStruct*, XExtCodes*
#endif
);

extern int (*XESetFreeFont(
#if NeedFunctionPrototypes
    Display*		/* display */,
    int			/* extension */,
    int (*) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              XFontStruct*		/* fs */,
              XExtCodes*		/* codes */
#endif
            )		/* proc */    
#endif
))(
#if NeedNestedPrototypes
    Display*, XFontStruct*, XExtCodes*
#endif
); 

extern int (*XESetCloseDisplay(
#if NeedFunctionPrototypes
    Display*		/* display */,
    int			/* extension */,
    int (*) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              XExtCodes*		/* codes */
#endif
            )		/* proc */    
#endif
))(
#if NeedNestedPrototypes
    Display*, XExtCodes*
#endif
);

extern int (*XESetError(
#if NeedFunctionPrototypes
    Display*		/* display */,
    int			/* extension */,
    int (*) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              xError*			/* err */,
              XExtCodes*		/* codes */,
              int*			/* ret_code */
#endif
            )		/* proc */    
#endif
))(
#if NeedNestedPrototypes
    Display*, xError*, XExtCodes*, int*
#endif
);

extern char* (*XESetErrorString(
#if NeedFunctionPrototypes
    Display*		/* display */,
    int			/* extension */,
    char* (*) (
#if NeedNestedPrototypes
	        Display*		/* display */,
                int			/* code */,
                XExtCodes*		/* codes */,
                char*			/* buffer */,
                int			/* nbytes */
#endif
              )		/* proc */	       
#endif
))(
#if NeedNestedPrototypes
    Display*, int, XExtCodes*, char*, int
#endif
);

extern void (*XESetPrintErrorValues (
#if NeedFunctionPrototypes
    Display*		/* display */,
    int			/* extension */,
    void (*)(
#if NeedNestedPrototypes
	      Display*			/* display */,
	      XErrorEvent*		/* ev */,
	      void*			/* fp */
#endif
	     )		/* proc */
#endif
))(
#if NeedNestedPrototypes
    Display*, XErrorEvent*, void*
#endif
);

extern int (*XESetWireToEvent(
#if NeedFunctionPrototypes
    Display*		/* display */,
    int			/* event_number */,
    Bool (*) (
#if NeedNestedPrototypes
	       Display*			/* display */,
               XEvent*			/* re */,
               xEvent*			/* event */
#endif
             )		/* proc */    
#endif
))(
#if NeedNestedPrototypes
    Display*, XEvent*, xEvent*
#endif
);

extern Status (*XESetEventToWire(
#if NeedFunctionPrototypes
    Display*		/* display */,
    int			/* event_number */,
    int (*) (
#if NeedNestedPrototypes
	      Display*			/* display */,
              XEvent*			/* re */,
              xEvent*			/* event */
#endif
            )		/* proc */   
#endif
))(
#if NeedNestedPrototypes
    Display*, XEvent*, xEvent*
#endif
);

extern Status (*XESetWireToError(
#if NeedFunctionPrototypes
    Display*		/* display */,
    int			/* error_number */,
    Bool (*) (
#if NeedNestedPrototypes
	       Display*			/* display */,
	       XErrorEvent*		/* he */,
	       xError*			/* we */
#endif
            )		/* proc */   
#endif
))(
#if NeedNestedPrototypes
    Display*, XErrorEvent*, xError*
#endif
);

_XFUNCPROTOEND
