/*
 *   plpstat.c:
 *
 *    Displays the status of the Personal Iris (4D/[20,25]) parallel port.
 *
 *  References:  PLP(7), IOCTL(2), OPEN(2), PERROR(3C) 
 *
 *                                     Grant Dorman - 1990
 */

#include <stdio.h>
#include <fcntl.h>
#include <sys/plp.h>

main ()
{ 
    int f1, iostat;

    if ((f1 = open("/dev/plp", O_WRONLY)) == -1) { 
	perror("open"); 
	exit(1); 
    }
    if (ioctl(f1, PLPIOCRESET, 0) == -1) { 
        perror("reset"); 
	exit(1); 
    }
    for (;;) { 
	if ((iostat = ioctl(f1, PLPIOCSTATUS, 0)) == -1) { 
	    perror("status"); 
	    exit(1); 
	}
        if (iostat & PLPONLINE) 
	    fprintf(stderr, "Online, ");
        if (iostat & PLPEOP) 
	    fprintf(stderr, "Paper out, ");
        if (iostat & PLPEOI) 
	    fprintf(stderr, "Ink out, ");
        if (iostat & PLPFAULT) 
	    fprintf(stderr, "Fault. ");
        fprintf(stderr, "\n");
        sleep(2);
    }
}
