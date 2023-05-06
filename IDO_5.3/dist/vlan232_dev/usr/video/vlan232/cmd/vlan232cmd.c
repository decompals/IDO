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
 * Module: vlan232cmd.c
 *
 * $Revision: 1.1 $
 *
 * Description:
 *	A shell level program for issuing VLAN commands. Commands are
 *	entered as strings (possibly requiring quotes) on the command
 *	line. After processing of the command the VLAN response string
 *	is printed followed by the OK message. Error diagnostics are
 *	also printed if appropriate.
 *
 * Syntax:
 *	vlan232cmd [-s serial_dev] [-i] [cmd]
 *
 *      s serial_dev
 *              The serial port device filename. This is the serial port
 *              to which the V-LAN transmitter is connected. The default
 *              serial port is /dev/ttyd2 (serial port 2).
 *
 *	-i	Run the program in interactive mode. A prompt is given
 *		and VLAN commands can be run interactively.
 *
 *	cmd	The two character VLAN command with any additional
 *		parameters. If spaces or characters meaningful to the
 *		shell are to be included in cmd then the string should be
 *		quoted with single quotes ''. This parameter is not used
 *		if the -i flag is specified.
 *
 **************************************************************************/


#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include "rs232_vlan.h"


#define USAGE_STR       "[-s serial_dev] [-i] [cmd]"
#define SDEV_OPT        's'
#define INTERACT_OPT    'i'


extern char *optarg;
extern int optind;

char *def_dev = "/dev/ttyd2";
int interact_flag;


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
 *	A 1 is returned if an error has occured. If successful a 0 is
 *	returned.
 *
 **************************************************************************/

int main(argc,argv)
int argc;
char *argv[];
{
    int errflag = 0, quit = 1;
    char *cmd, *resp;
    char interact_cmd[256];
    VLAN232_DEV *vlanptr;
    int opt;
    char *devptr;

    /*
     * Handle the command line options
     */
    devptr = def_dev;
    while ((opt = getopt(argc,argv,"s:i")) != -1)
	{
	switch (opt)
	    {
	    case INTERACT_OPT:
		quit = 0;
	        interact_flag = 1;
		break;
	    case SDEV_OPT:
		devptr = optarg;
	        break;
	    case '?':
		errflag++;
	        break;
	    }
	}

    /*
     * Get cmd from the command line
     */
    if (!interact_flag && !errflag)
	{
        if (optind == argc)
	    {
	    errflag = 1;
	    fprintf(stderr,"%s: must specify a VLAN command or -i\n",argv[0]);
	    }
        cmd = argv[optind];
	}

    /*
     * Process command line errors
     */
    if (errflag)
	{
	fprintf(stderr,"Usage: %s %s\n",argv[0],USAGE_STR);
	exit(1);
	}

    /*
     * Issue the VLAN command
     */
    if ((vlanptr = rs232_vlan_open(devptr)) == (VLAN232_DEV*)NULL)
	{
	rs232_vlan_perror(argv[0]);
	exit(1);
	}
    if (rs232_vlan_alive(vlanptr) == RS232_VLAN_NOTRUNNING)
	{
	fprintf(stderr,"%s: no V-LAN transmitter connected\n",argv[0]);
        rs232_vlan_close(vlanptr);
	exit(1);
	}
    do {
        if (interact_flag)
	    {
	    printf("Command ('q' = quit): ");
	    gets(interact_cmd);
	    cmd = interact_cmd;
	    }
	if (!strcasecmp(cmd,"q"))
	    quit = 1;
	else if (*cmd != '\0')
	    {
            if ((resp = rs232_vlan_cmd(vlanptr,cmd)) == (char*)NULL)
	        {
	        rs232_vlan_perror(argv[0]);
		rs232_vlan_close(vlanptr);
	        exit(1);
	        }
            if (interact_flag)
	        printf("Response: ");
            if (*resp != '\r')
	        printf("%s\n",resp);
            printf("OK\n");
	    }
	} while (!quit);
    if (rs232_vlan_close(vlanptr) < 0)
	{
	rs232_vlan_perror(argv[0]);
	exit(1);
	}
    return(0);
}
