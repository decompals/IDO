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
/* $Header: /proj/irix5.1/isms/cmplrs/include/RCS/opnames.h,v 7.7 1993/06/08 01:17:31 bettina Exp $ */

#ifndef __OPNAMES_H__
#define __OPNAMES_H__

#ifdef __cplusplus
extern "C" {
#endif

extern char *op_name[64];
extern char *spec_name[64];
extern char *bcond_name[32];
extern char *cop1func_name[64];
extern char *bc_name[32];
extern char *c0func_name[64];
extern char *c0mfunc_name[64];
extern char *c0reg_name[32];
extern char *cop1xfunc_name[64];

#ifdef __cplusplus
}
#endif

#endif  /* __OPNAMES_H__ */
