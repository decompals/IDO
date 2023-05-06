/*
 * select --
 *
 *	This program demonstrates a simple server program. Any text
 *	sent by a client is displayed. When a client closes the
 *	connection, the server waits for new connection requests.
 *
 * The information in this software is subject to change without notice
 * and should not be construed as a commitment by Silicon Graphics, Inc.
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>

/* Listen for connect requests on this arbitrarily-chosen port */
#define SERVER_PORT	(IPPORT_USERRESERVED + 123)

#define LINE_LEN	80

main()
{
    int sock, length, msgsock, cnt, nfound;
    char line[LINE_LEN];
    fd_set ready;
    struct sockaddr_in sin;
    struct timeval timeout;
    int	maxsock;

    /* create a socket */

    sock = socket (AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
	perror("opening stream socket");
	exit(1);
    }

    /* initialize socket data structure */

    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = INADDR_ANY;
    sin.sin_port = htons(SERVER_PORT);

    /* bind socket data structure to this socket */

    if (bind (sock, &sin, sizeof(sin)) < 0) {
	perror("binding stream socket");
	exit(1);
    }

    /* Prepare socket queue for connection requests */

    listen(sock, 5);

    /* Maximum of sockets to listen on */
    maxsock = sock + 1;

    timeout.tv_sec = 10;	/* Wait 10 seconds for a connect request */
    timeout.tv_usec = 0;

    for (;;) {
	FD_ZERO(&ready);
	FD_SET(sock, &ready);

	/* 
	 * Wait for a connect request. The socket becomes readable when
	 * a request arrives. 
	 */
	nfound = select(maxsock, &ready, 0, 0, &timeout);   
	if (nfound < 0) {
	    perror("select");
	    exit(1);
	} else if (nfound == 0) {
	    printf("Timed-out waiting for connection\n");
	    continue;
	}

	if (FD_ISSET(sock, &ready)) {
	    FD_CLR(sock, &ready);

	    length = sizeof(sin);
	    msgsock = accept(sock, &sin, &length);
	    if (msgsock < 0) {
		perror("accept");
		exit(1);
	    }

	    printf("Connection from host %s, port %u\n",
		inet_ntoa(sin.sin_addr), ntohs(sin.sin_port));
	    fflush(stdout);

	    while ((cnt = read(msgsock, line, LINE_LEN)) > 0) {
		(void) write(1, line, cnt);
	    }
	    close(msgsock);
	    printf("Connection closed\n");
	} else {
	    /* With 1 socket, this shouldn't happen */
	    printf("??? sock not ready\n");  
	}
	fflush(stdout);
    }
}
