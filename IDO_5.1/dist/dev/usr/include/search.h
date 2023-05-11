#ifndef __SEARCH_H__
#define __SEARCH_H__
#ifdef __cplusplus
extern "C" {
#endif
#ident "$Revision: 1.14 $"
/*
*
* Copyright 1992, Silicon Graphics, Inc.
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
*/
/*	Copyright (c) 1990, 1991 UNIX System Laboratories, Inc.	*/
/*	Copyright (c) 1988 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF     	*/
/*	UNIX System Laboratories, Inc.                     	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#if !defined(_SIZE_T) && !defined(_SIZE_T_)
#define _SIZE_T
#if (_MIPS_SZLONG == 32)
typedef unsigned int	size_t;
#endif
#if (_MIPS_SZLONG == 64)
typedef unsigned long	size_t;
#endif
#endif

/* HSEARCH(3C) */
typedef enum { FIND, ENTER } ACTION;

struct qelem {
	struct qelem	*q_forw;
	struct qelem	*q_back;
};

typedef struct entry { char *key; void *data; } ENTRY;
int hcreate(size_t);
void hdestroy(void);
ENTRY *hsearch(ENTRY, ACTION);

void insque(struct qelem *, struct qelem *);
void remque(struct qelem *);

/* TSEARCH(3C) */
typedef enum { preorder, postorder, endorder, leaf } VISIT;

void *tdelete(const void *, void **, int (*)(const void *, const void *)); 
void *tfind(const void *, void *const *, int (*)(const void *, const void *));
void *tsearch(const void *, void **, int (*)(const void *, const void *));
void twalk(void *, void (*)(void *, VISIT, int));

/* BSEARCH(3C) */
void *bsearch(const void *, const void *, size_t, size_t,
	    int (*)(const void *, const void *));

/* LSEARCH(3C) */
void *lfind(const void *, const void *, size_t *, size_t, 
	    int (*)(const void *, const void *));
void *lsearch(const void *, void *, size_t *, size_t,
	    int (*)(const void *, const void *));

#ifdef __cplusplus
}
#endif
#endif /* !__SEARCH_H__ */
