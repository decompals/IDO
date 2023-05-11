#ifndef __NLIST_H__
#define __NLIST_H__
#ifdef __cplusplus
extern "C" {
#endif
/* Copyright (C) 1989 Silicon Graphics, Inc. All rights reserved.  */
/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1991, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Avenue                               |
 * |         Sunnyvale, California 94088-3650, USA             |
 * |-----------------------------------------------------------|
 */
/* $Header: /proj/irix5.1/isms/cmplrs/include/RCS/nlist.h,v 7.5 1993/01/12 08:58:35 davea Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/



#ifdef __mips
struct nlist {
    char *n_name;
    unsigned long n_value;
    short n_type;		/* 0 if not there, 1 if found */
    short reserved;
};
#endif	/* __mips */

extern int	nlist(const char *, struct nlist *);

#ifdef __cplusplus
}
#endif

#endif	/* _NLIST_H */
