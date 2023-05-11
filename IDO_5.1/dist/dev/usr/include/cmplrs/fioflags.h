/*     @(#)fioflags.h	  */
/*  $Header: /proj/irix5.1/isms/cmplrs/include/cmplrs/RCS/fioflags.h,v 1.3 1992/01/20 13:00:00 daveb Exp $ */

/*
**	This header defines all the external flags that can be set
**	when using the FORTRAN I/O library with a C main program.
**	To set a particular flag the appropriate array element
**	(VMS_CC for the equivalent -vms_cc option, OLD_RL for the 
**	-old_rl option, VMS_IN for the -vms_stdin option, and 
**	VMS_EF for -vms_endfile option) must be set to 1.  For example,
**	in order to use the VMS carrage control interpretation on unit 6
**	you have to add the statement:
**
**	f77vms_flag_[VMS_CC] = 1;
**	
**	to the C main program before Fortran I/O is done.
*/

#define VMS_FLAGS	4

#define VMS_CC  0
#define OLD_RL	1
#define VMS_IN  2
#define VMS_EF  3

extern unsigned short f77vms_flag_[VMS_FLAGS];
