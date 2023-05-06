/* Example UNIX domain datagram socket writer */
/*
 *  Copyright (c) 1986 Regents of the University of California.
 *  All rights reserved.  The Berkeley software License Agreement
 *  specifies the terms and conditions for redistribution.
 * 
 * 	@(#)udgramsend.c	6.2 (Berkeley) 5/8/86
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>

#define DATA "The sea is calm tonight, the tide is full . . ."

/*
 * Here I send a datagram to a receiver whose name I get from the command
 * line arguments.  The form of the command line is udgramsend pathname 
 */

main(argc, argv)
	int argc;
	char *argv[];
{
	int sock;
	struct sockaddr_un name;

	if (argc < 2) {
		printf("usage: %s socketname\n", argv[0]);
		exit(1);
	}

	/* Create socket on which to send. */
	sock = socket(AF_UNIX, SOCK_DGRAM, 0);
	if (sock < 0) {
		perror("opening datagram socket");
		exit(1);
	}
	/* Construct name of socket to send to. */
	name.sun_family = AF_UNIX;
	strcpy(name.sun_path, argv[1]);
	/* Send message. */
	if (sendto(sock, DATA, sizeof(DATA), 0,
	    &name, strlen(name.sun_path) + sizeof(name.sun_family)) < 0) {
		perror("sending datagram message");
	}
	close(sock);
}
