/*
 * tcpmuxserver --
 *
 *	This program provides a simple service and is invoked by the TCPMUX 
 *	handler in inetd(1M). Modify the following line and add to
 *	/usr/etc/inetd.conf:

tcpmux/current_time stream tcp nowait guest /usr/tmp/tcpmuxserver curtime

 *
 * The information in this software is subject to change without notice
 * and should not be construed as a commitment by Silicon Graphics, Inc.
 */

#include <sys/types.h>
#include <stdio.h>

main()
{
	time_t t;

	printf("+Go\r\n");
	fflush(stdout);

	time(&t);
	printf("%d = %s", t, ctime(&t));
	fflush(stdout);
}
