/*
 * connect --
 *
 *	This program connects to the server program, 'accept' and sends 
 *	any data typed by the user to the server.
 *
 * The information in this software is subject to change without notice
 * and should not be construed as a commitment by Silicon Graphics, Inc.
 */

#define _BSD_SIGNALS
#include <sys/types.h>
#include <sys/socket.h>
#include <signal.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>

static int die();

#define LINE_LEN	80

/* The server listens for connect requests on this port */
#define SERVER_PORT	(IPPORT_USERRESERVED + 123)

main(argc,argv)
    int argc;
    char **argv;
{
    int cnt, sock;
    struct sockaddr_in sin;
    struct hostent *hp;
    char line[LINE_LEN];

    if (argc != 2) {
	printf("usage: %s host\n", argv[0]);
	exit(1);
    }

    if ((sock = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
	perror("Can't open socket");
	exit(1);
    }

    /* Initialize the socket address to the server's address. */

    bzero((char *) &sin, sizeof(sin));
    sin.sin_family = AF_INET;
    hp = gethostbyname(argv[1]);    /* to get host address */
    if (hp == NULL) {
	herror(argv[1]);
	exit(1);
    }
    bcopy (hp->h_addr, &(sin.sin_addr.s_addr), hp->h_length);
    sin.sin_port = htons(SERVER_PORT);

    /* Connect to the server. */

    if (connect(sock, &sin,sizeof(sin)) < 0) {
	close(sock);
	perror("Connect to server");
	exit(1);
    }
    printf("Connection established. Enter your message below. To exit, type ^D\n");

    /*
     * If the server goes away while sending data, we'll get a SIGPIPE signal. 
     * Catch it so we can print an error message.
     */
    (void) signal(SIGPIPE, die);

    while ((cnt = read(0, line, LINE_LEN)) > 0) {
	printf("sending <");
	fflush(stdout);
	write(1, line, cnt-1);
	printf("\\n>\n");
	fflush(stdout);

	/* Send input to the remote host. */

	if (write(sock, line, cnt) < 0) {
	    perror("Error writing to socket");
	    exit(1);
	}
    }
    printf ("Done\n");
    exit(0);
}

static int
die()
{
    fprintf(stderr, "Server closed connection\n");
    exit(1);
}
