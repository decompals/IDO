/*
 * libgen.h
 *
 *
 * Copyright 1991, Silicon Graphics, Inc.
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

/*	declarations of functions found in libgen
*/
#ifndef _LIBGEN_H
#define _LIBGEN_H

#ifdef __cplusplus
extern "C" {
#endif

#ident "$Revision: 1.4 $"

#include <sys/types.h>
#include <stdio.h>

#ifdef _MODERN_C
extern char * basename(char *);
#else
extern char * basename();
#endif

#ifdef _MODERN_C
extern char * bgets(char *, size_t, FILE *, char *);
#else
extern char * bgets();
#endif

#ifdef _MODERN_C
extern size_t bufsplit(char *, size_t, char *);
#else
extern size_t bufsplit();
#endif

#ifdef _MODERN_C
extern char * copylist(const char *, off_t *);
#else
extern char * copylist();
#endif

#ifdef _MODERN_C
extern char * dirname(char *);
#else
extern char * dirname();
#endif

#ifdef _MODERN_C
extern int eaccess(const char *, int);
#else
extern int eaccess();
#endif

#include	<time.h>

#ifdef _MODERN_C
extern int gmatch(const char *, const char *);
#else
extern int gmatch();
#endif

#ifdef _MODERN_C
extern int isencrypt(const char *, size_t);
#else
extern int isencrypt();
#endif

#ifdef _MODERN_C
extern int mkdirp(const char *, mode_t);
#else
extern int mkdirp();
#endif

#ifdef _MODERN_C
extern int p2open(const char *, FILE *[2]);
#else
extern int p2open();
#endif

#ifdef _MODERN_C
extern int p2close(FILE *[2]);
#else
extern int p2close();
#endif

#ifdef _MODERN_C
extern char * pathfind(const char *, const char *, const char *);
#else
extern char * pathfind();
#endif

#ifdef _MODERN_C
extern char * regcmp(const char *, ...);
#else
extern char * regcmp();
#endif

#ifdef _MODERN_C
extern char * regex(const char *, const char *, ...);
#else
extern char * regex();
#endif

#ifdef _MODERN_C
extern int rmdirp(char *, char *);
#else
extern int rmdirp();
#endif

#ifdef _MODERN_C
extern char * strcadd(char *, const char *);
#else
extern char * strcadd();
#endif

#ifdef _MODERN_C
extern char * strccpy(char *, const char *);
#else
extern char * strccpy();
#endif

#ifdef _MODERN_C
extern char * streadd(char *, const char *, const char *);
#else
extern char * streadd();
#endif

#ifdef _MODERN_C
extern char * strecpy(char *, const char *, const char *);
#else
extern char * strecpy();
#endif

#ifdef _MODERN_C
extern int strfind(const char *, const char *);
#else
extern int strfind();
#endif

#ifdef _MODERN_C
extern char * strrspn(const char *, const char *);
#else
extern char * strrspn();
#endif

#ifdef _MODERN_C
extern char * strtrns(const char *, const char *, const char *, char *);
#else
extern char * strtrns();
#endif

#ifdef __cplusplus
}
#endif

#endif /* _LIBGEN_H */
