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
 * Module: valn232alive.c
 *
 * $Revision: 1.1 $
 *
 * Description:
 *	This program determines the state of the RS-232 V-LAN transmitter
 *	connected to the specified serial port. The program will exit with
 *	one of the following exit status codes:
 *
 *		0	- No V-LAN transmitter is connected to the specified
 *			  serial port.
 *		1	- An NTSC format V-LAN transmitter is connected to
 *			  the specified serial port.
 *		2	- A PAL format V-LAN transmitter is connected to
 *			  the specified serial port.
 *		3	- A V-LAN transmitter is connected to the specified
 *			  serial port but is configured for an unknown
 *			  network format.
 *		9	- An execution error has occured.
 *
 *	In addition a status message will be printed to stderr unless
 *	the quiet flag (-q) is specified.
 *
 * Syntax:
 *	vlan232alive [-s serial_dev] [-q]
 *
 *	s serial_dev
 *		The serial port device filename. This is the serial port
 *		to which the V-LAN transmitter is connected. The default
 *		serial port is /dev/ttyd2 (serial port 2).
 *
 *	q	Run in quiet mode. Suppresses the printing of the 
 *		status message. 
 *
 **************************************************************************/


#include <stdio.h>
#include <string.h>
#include "rs232_vlan.h"


#define USAGE_STR       "[-s serial_dev] [-q]"
#define	SDEV_OPT	's'
#define	QUIET_OPT	'q'


extern char* optarg;

char *progname;
char *def_dev = "/dev/ttyd2";


/**************************************************************************
 *
 * Function: main
 *
 * Description: Program entry point.
 *
 * Prototype: int main (int argc, char *argv[])
 *
 * Parameters: 
 *	argc (I) - command line arguement count
 *	argv (I) - command line parameters
 *
 * Return:
 *	0	- VLAN transmitter not present.
 *	1	- VLAN active, NTSC
 *	2	- VLAN active, PAL
 *	3	- VLAN active, unknown
 *	9	- Program execution error
 *
 **************************************************************************/

int main(argc,argv)
int argc;
char *argv[];
{
    VLAN232_DEV *vlanptr;
    int opt, quiet_flag = 0, errflag = 0;
    int vlan_status;
    char *devptr;

    /*
     * Handle the command line options
     */
    progname = argv[0];
    devptr = def_dev;
    while ((opt = getopt(argc,argv,"s:q")) != -1)
	{
	switch (opt)
	    { 
	    case QUIET_OPT:
	        quiet_flag = 1;
	        break;
	    case SDEV_OPT:
	        devptr = optarg;
	        break;
	    case '?':
	        errflag++;
	        break;
	    }
	}
    if (errflag)
	{
	fprintf(stderr,"Usage: %s %s\n",argv[0],USAGE_STR);
	exit(9);
	}

    /*
     * Do the test
     */
    if ((vlanptr = rs232_vlan_open(devptr)) == (VLAN232_DEV*)NULL)
	{
	rs232_vlan_perror(argv[0]);
	exit(9);
	}
    if ((vlan_status = rs232_vlan_alive(vlanptr)) < 0)
	{
	rs232_vlan_perror(argv[0]);
	exit(9);
	}
    if (rs232_vlan_close(vlanptr) < 0)
	{
	rs232_vlan_perror(argv[0]);
	exit(9);
	}
    if (!quiet_flag)
	{
	switch (vlan_status)
	    {
	    case RS232_VLAN_NTSC:
	        fprintf(stderr,"NTSC format V-LAN transmitter ");
		break;
	    case RS232_VLAN_PAL:
	        fprintf(stderr,"PAL format V-LAN transmitter ");
		break;
	    case RS232_VLAN_UNKNOWN:
	        fprintf(stderr,"Unknown format V-LAN transmitter ");
		break;
	    case RS232_VLAN_NOTRUNNING:
	        fprintf(stderr,"V-LAN transmitter not ");
		break;
	    }
	fprintf(stderr,"connected to %s\n",devptr);
	}
    return(vlan_status);
}

