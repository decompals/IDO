#ifndef __INC_DM_GENERAL_H__
#define __INC_DM_GENERAL_H__  

/*****************************************************************************
*
*  Copyright 1993, Silicon Graphics, Inc.
*  All Rights Reserved.
*
*  This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
*  the contents of this file may not be disclosed to third parties, copied or
*  duplicated in any form, in whole or in part, without the prior written
*  permission of Silicon Graphics, Inc.
*
*  RESTRICTED RIGHTS LEGEND:
*  Use, duplication or disclosure by the Government is subject to restrictions
*  as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
*  and Computer Software clause at DFARS 252.227-7013, and/or in similar or
*  successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
*  rights reserved under the Copyright Laws of the United States.
* 
*****************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

/**********************************************************************
*
* Data Types
*
**********************************************************************/

/********
*
* DMboolean
*
********/

typedef int DMboolean;

#define DM_FALSE  0
#define DM_TRUE   1

/********
*
* DMfraction
*
********/

typedef struct __DMfraction {
    int numerator;
    int denominator;
} DMfraction;

/**********************************************************************
*
* Return Codes
*
**********************************************************************/

typedef enum __DMstatus
{
    DM_SUCCESS		= 0,
    DM_FAILURE		= -1
} DMstatus;

/**********************************************************************
*
* General purpose functions
*
**********************************************************************/

struct timeval;
int dmGetCurrentTime(struct timeval *);
int dmGetUST(unsigned long long *);

#ifdef __cplusplus 
}
#endif

#endif /* ! __INC_DM_GENERAL_H__  */

