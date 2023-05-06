/*
 * accept --
 *
 *	This program demonstrates a simple server program. Any text
 *	sent by a client is displayed. When the client closes the
 *	connection, the server exits.
 *
 * The information in this software is subject to change without notice
 * and should not be construed as a commitment by Silicon Graphics, Inc.
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>

/* Listen for connect requests on this arbitrarily-chosen port */
#define SERVER_PORT	(IPPORT_USERRESERVED + 123)

#define LINE_LEN	80

main()
{
    int sock, length, msgsock, cnt;
    char line[LINE_LEN];
    struct sockaddr_in sin;

    /* Create a socket */

    if ((sock = socket (AF_INET,SOCK_STREAM,0)) < 0) {
	perror("opening stream socket");
	exit(1);
    }

    /* Initialize the socket's address structure */

    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = INADDR_ANY;
    sin.sin_port = htons(SERVER_PORT);

    /* Assign an address to this socket */

    if (bind (sock,&sin,sizeof(sin)) < 0) {
	perror("binding stream socket");
	exit(1);
    }

    printf("Listening for connect requests on port %d\n", SERVER_PORT);

    /* Prepare the socket queue for connection requests */

    listen(sock,5);

    length = sizeof(sin);
    msgsock = accept(sock, &sin, &length);
    if (msgsock < 0) {
	perror("accept");
	exit(1);
    }

    printf("Connection from host %s, port %u\n",
	inet_ntoa(sin.sin_addr), ntohs(sin.sin_port));
    fflush(stdout);

    /* Read from the message socket and write to standard output */

    while ((cnt = read(msgsock, line, LINE_LEN)) > 0) {
	write(1, line, cnt);
    }

    close(msgsock);
    printf("Connection closed\n");
}
