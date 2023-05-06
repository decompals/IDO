#ifndef __CLEARANCE_H__
#define __CLEARANCE_H__
#ifdef __cplusplus
extern "C" {
#endif
#ident "$Revision: 1.1 $"
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

#define CLEARANCE	"/etc/clearance"

struct clearance {
	char *cl_name;		/* Name */
	char *cl_default;	/* Default Clearance */
	char *cl_allowed;	/* Allowed Clearances */
};

/*
 * Use ia_openinfo to read this database.
 */

#ifdef __cplusplus
}
#endif
#endif /* !__CLEARANCE_H__ */
