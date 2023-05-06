#ifndef __INC_AUDIOUTIL_H__
#define __INC_AUDIOUTIL_H__  

/***************************************************************************
 * SGI Audio File Library
 *
 * audioutil.h
 *    header file for use with /usr/lib/libaudiofile.a
 ***************************************************************************
 * 
 * Copyright 1993, Silicon Graphics, Inc.
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
 ****************************************************************************/

#ident "$Revision: 1.10 $"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * license utilities
 */
#define AU_LICENSE_AWARE_MPEG_ENCODER        1
#define AU_LICENSE_AWARE_MPEG_DECODER        2
#define AU_LICENSE_AWARE_MULTIRATE_ENCODER   3
#define AU_LICENSE_AWARE_MULTIRATE_DECODER   4

extern int AUchecklicense(int /*product*/, int */*errorval*/, char ** /*msg*/);

#define AU_LICENSE_OK     0
#define AU_LICENSE_ERR   -1
#define AU_BAD_PRODUCT   -2

/*
 * parameter-value list utilities
 */
typedef struct _AUpvlist *AUpvlist;
/*
 * valid types
 */
#define AU_PVTYPE_LONG   1	/* setparam arg is pointer to long */
#define AU_PVTYPE_DOUBLE 2	/* setparam arg is pointer to double */
#define AU_PVTYPE_PTR    3	/* setparam arg is pointer to (void *) */

#define AU_NULL_PVLIST ((struct _AUpvlist *)0)

/*
 * each returns a nonnegative value on success
 * each returns a negative value on failure
 */
extern AUpvlist AUpvnew(int /* max items */);
extern int AUpvgetmaxitems(AUpvlist);
extern int AUpvfree(AUpvlist);
extern int AUpvsetparam(AUpvlist, int /*item*/, int /*param*/);
extern int AUpvsetvaltype(AUpvlist,  int /*item*/, int /*type*/);
extern int AUpvsetval(AUpvlist,   int /*item*/, void * /*val*/);
extern int AUpvgetparam(AUpvlist, int /*item*/, int * /*param*/);
extern int AUpvgetvaltype(AUpvlist,  int /*item*/, int * /*type*/);
extern int AUpvgetval(AUpvlist,   int /*item*/, void * /*val*/);

#ifdef __cplusplus 
}
#endif

#endif /* ! __INC_AUDIOUTILH__ */
