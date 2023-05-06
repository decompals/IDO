/* Example UNIX domain datagram socket reader */

/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)udgramread.c	6.3 (Berkeley) 5/8/86
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

/*
 * In the included file <sys/un.h> a sockaddr_un is defined as follows
 * struct sockaddr_un {
 *	short	sun_family;
 *	char	sun_path[108];
 * }; 
 */


#define NAME "udsocket"

/*
 * This program creates a UNIX domain datagram socket, binds a name to it,
 * then reads from the socket. 
 */
main()
{
	int sock, length;
	struct sockaddr_un name;
	char buf[1024];

	/* Create socket from which to read. */
	sock = socket(AF_UNIX, SOCK_DGRAM, 0);
	if (sock < 0) {
		perror("opening datagram socket");
		exit(1);
	}
	/* Create name. */
	name.sun_family = AF_UNIX;
	strcpy(name.sun_path, NAME);
	if (bind(sock, &name, 
		strlen(name.sun_path) + sizeof(name.sun_family)) < 0) {
		perror("binding name to datagram socket");
		exit(1);
	}
	printf("socket --> %s\n", NAME);
	/* Read from the socket */
	if (read(sock, buf, 1024) < 0)
		perror("receiving datagram packet");
	printf("-->%s\n", buf);
	close(sock);
	unlink(NAME);
}