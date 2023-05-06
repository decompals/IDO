#ifndef __INVENT_H__
#define __INVENT_H__
/*
 * User-level interface to kernel hardware inventory.
 *
 *
 * Copyright 1992-1993 Silicon Graphics, Inc.
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
 *
 *
 * Setinvent initializes a copy of the kernel's hardware inventory list.
 * It returns -1 with errno set if it cannot fetch or allocate the list.
 * Getinvent returns a pointer to the next list item.  Endinvent destroys
 * the list.  Getinvent may be called without first calling setinvent.
 *
 * Call scaninvent with an int function pointer and an optional opaque
 * pointer to iterate over inventory items.  For each item, scaninvent
 * calls the pointed-at (scan) function with a pointer to that item as
 * the first argument, and with the opaque pointer as the scan function's
 * second argument.  If the scan function returns a non-zero value,
 * scaninvent stops scanning and returns that value.  Otherwise it
 * returns 0 after scanning all items.  Scaninvent returns -1 if it
 * cannot successfully setinvent before scanning.
 *
 * Scaninvent normally calls endinvent before returning.  To prevent
 * scaninvent from calling endinvent, set _keepinvent to 1.
 *
 * For multi-threaded applications there are recursive versions of
 * these routines.
 */

#ident "$Revision: 1.8 $"

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/invent.h>

int		setinvent(void);
inventory_t	*getinvent(void);
void		endinvent(void);
int		scaninvent(int (*)(inventory_t *, void *), void *);

extern int	_keepinvent;

typedef struct {
	int inv_count;	/* count of records */
	inventory_t *inv_table;
	int inv_curr;	/* current record */
} inv_state_t;

int		setinvent_r(inv_state_t **);
inventory_t	*getinvent_r(inv_state_t *);
void		endinvent_r(inv_state_t *);

#ifdef __cplusplus
}
#endif
#endif /* !__INVENT_H__ */
