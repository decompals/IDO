/**************************************************************************
 *									  *
 * 		 Copyright (C) 1990, Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************
 *
 * Module: rs232_vlan.h
 *
 * $Revision: 1.1 $
 *
 * Description: Include file for the RS-232 V-LAN interface functions.
 *
 **************************************************************************/


#include <limits.h>


#ifndef TRUE
#define TRUE    1
#endif
#ifndef FALSE
#define FALSE   0
#endif


typedef struct
    {
    int valid;
    char dev_name[PATH_MAX];
    char resp_buf[PATH_MAX];
    int desc;
    int semid;
    } VLAN232_DEV;


/* V-LAN Status Info */
#define RS232_VLAN_NOTRUNNING	0
#define RS232_VLAN_NTSC		1
#define RS232_VLAN_PAL		2
#define RS232_VLAN_UNKNOWN	3

/* Semaphore Info */
#define RS232_VLAN_SEMKEY	0x071160

/* Timeout info */
#define RS232_VLAN_TIMEOUT	2	/* seconds */


/* Error Codes */

#define RS232_VLAN_ERROR	-1
#define RS232_VLAN_NOERROR	0
#define RS232_VLAN_ERR_OPEN	1
#define RS232_VLAN_ERR_BADDEV	2
#define RS232_VLAN_ERR_IOCTL	3
#define RS232_VLAN_ERR_BADCMD	4
#define RS232_VLAN_ERR_SEMGET	5
#define RS232_VLAN_ERR_SEMRM	6
#define RS232_VLAN_ERR_SEMCLEAR	7
#define RS232_VLAN_ERR_TIMEOUT	8


extern char *rs232_vlan_errlist[];
extern int rs232_vlan_nerr;
extern int rs232_vlan_errno;

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

extern VLAN232_DEV *rs232_vlan_open (char*);
extern int rs232_vlan_close (VLAN232_DEV*);
extern int rs232_vlan_alive (VLAN232_DEV*);
extern char *rs232_vlan_cmd (VLAN232_DEV*, char*);
extern void rs232_vlan_perror (char*);

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

