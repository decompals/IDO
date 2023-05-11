#ifndef __CORE_H__
#define __CORE_H__
/****************************************************************/
/*	Copyright (c) 1987,1989 Silicon Graphics, Inc.		*/
/*			All Rights Reserved			*/
/****************************************************************/
/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ident "$Revision: 1.8 $"

/* Machine dependent stuff for core files */

#define TXTRNDSIZ 4096
#define stacktop(siz) 0x7FFFF000
#define stackbas(siz) (0x7FFFF000 - siz)

#endif /* !__CORE_H__ */
