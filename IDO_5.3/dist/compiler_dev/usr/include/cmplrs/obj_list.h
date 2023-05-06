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
/* $Header: /proj/irix5.3/isms/cmplrs/include/cmplrs/RCS/obj_list.h,v 1.1 1994/05/20 21:04:27 gischer Exp $ */

#ifndef _OBJ_LIST_H_
#define _OBJ_LIST_H_
typedef struct obj_list {
	unsigned long	data;
	struct obj_list	*next;
	struct obj_list	*prev;			/* back link */
} objList;

#define LIST_BEGINNING	0
#define LIST_END	1
#define LIST_ADD_BEFORE         2
#define LIST_ADD_AFTER          3
#define LIST_DELETE             4
#define LIST_REPLACE            5

#endif				/* _OBJ_LIST_H_ */
