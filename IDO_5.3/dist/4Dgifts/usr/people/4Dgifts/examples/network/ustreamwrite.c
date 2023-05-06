/* Example UNIX domain stream socket writer */
/*
 *  Copyright (c) 1986 Regents of the University of California.
 *  All rights reserved.  The Berkeley software License Agreement
 *  specifies the terms and conditions for redistribution.
 * 
 * 	@(#)ustreamwrite.c	6.3 (Berkeley) 5/8/86
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>

#define DATA "Half a league, half a league . . ."

/*
 * This program connects to the socket named in the command line and sends a
 * one line message to that socket. The form of the command line is
 * ustreamwrite pathname 
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	int i;
	int sock;
	struct sockaddr_un server;
	char buf[1024];

	if (argc < 2) {
		printf("usage: %s socketname\n", argv[0]);
		exit(1);
	}

	/* Create socket */
	sock = socket(AF_UNIX, SOCK_STREAM, 0);
	if (sock < 0) {
		perror("opening stream socket");
		exit(1);
	}
	/* Connect socket using name specified by command line. */
	server.sun_family = AF_UNIX;
	strcpy(server.sun_path, argv[1]);

	if (connect(sock, &server, 
		strlen(server.sun_path) + sizeof(server.sun_family)) < 0) {
		close(sock);
		perror("connecting stream socket");
		exit(1);
	}
	for (i=0; i < 10; i++) {
	    if (write(sock, DATA, sizeof(DATA)) < 0)
		perror("writing on stream socket");
	    sleep(2);
	}
}
