/**************************************************************************
 *									  *
 * Copyright (C) 1986-1992 Silicon Graphics, Inc.			  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/
#ifndef __MPLIB_H__
#define __MPLIB_H__

#ident "$Revision: 1.4 $"
#include "ulocks.h"
#include "assert.h"

extern int __fislockfile(struct __file_s *);

/*
 * macros/defines for threading stdio
 */
#ifndef _LIBC_ABI
#define LOCKFILE(x) \
        (__us_rsthread_stdio) ?  flockfile(x), 1 : 0

#define ISLOCKFILE(x) \
        (__us_rsthread_stdio) ?  __fislockfile(x) : 1

#define UNLOCKFILE(x,v) \
	{ if (v) funlockfile(x); }

#define LOCKOPEN \
        (__us_rsthread_stdio) ?  uspsema(__opensema) == 1 : 0

#define ISLOCKOPEN \
        (__us_rsthread_stdio) ?  (ustestsema(__opensema) <= 0) : 1

#define UNLOCKOPEN(v) \
	{ if (v) (void)usvsema(__opensema); }

#define LOCKDIR \
	(__us_rsthread_misc) ? uspsema(__dirsema) == 1 : 0

#define UNLOCKDIR(v) \
	{ if (v) (void)usvsema(__dirsema); }

#define LOCKMISC \
	(__us_rsthread_misc) ? uspsema(__miscsema) == 1 : 0

#define UNLOCKMISC(v) \
	{ if (v) (void)usvsema(__miscsema); }

#define LOCKLOCALE \
        (__us_rsthread_misc) ?  uspsema(__localesema) == 1 : 0

#define ISLOCKLOCALE \
        (__us_rsthread_misc) ?  (ustestsema(__localesema) <= 0) : 1

#define UNLOCKLOCALE(v) \
	{ if (v) (void)usvsema(__localesema); }
#else
#define LOCKFILE(x)		0
#define ISLOCKFILE(x)		1
#define UNLOCKFILE(x,v)	
#define LOCKOPEN		0
#define ISLOCKOPEN		1
#define UNLOCKOPEN(v)
#define LOCKDIR			0
#define UNLOCKDIR(v)
#define LOCKMISC		0
#define UNLOCKMISC(v)
#define LOCKLOCALE		0
#define ISLOCKLOCALE		1
#define UNLOCKLOCALE(v)
#endif /* _LIBC_ABI */


extern usema_t *__dirsema;
extern usema_t *__opensema;
extern usema_t *__miscsema;
extern usema_t *__localesema;

extern int __us_rsthread_stdio;
extern int __us_rsthread_misc;
extern int __us_rsthread_malloc;

#endif
