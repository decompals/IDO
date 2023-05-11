/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dbm.h	5.2 (Berkeley) 85/06/26
 */

#ifndef NULL
/*
 * this is lunacy, we no longer use it (and never should have
 * unconditionally defined it), but, this whole file is for
 * backwards compatability - someone may rely on this.
 */
#define	NULL	((char *) 0)
#endif

#include "ndbm.h"

int	dbminit(const char *);
datum	fetch(datum);
int	store(datum, datum);
int	delete(datum);
datum	firstkey(void);
datum	nextkey(datum);
void	dbmclose(void);
