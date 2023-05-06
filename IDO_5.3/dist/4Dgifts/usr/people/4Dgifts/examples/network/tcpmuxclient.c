/*
 * tcpmuxclient --
 *
 *	This program connects to the TCPMUX server, sends the service
 *	name, and prints the data returned by the service.
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
#include <strings.h>

static int die();

main(argc,argv)
    int argc;
    char **argv;
{
    int sock;
    struct sockaddr_in sin;
    struct hostent *hp;
    struct servent *sp;
    char line[BUFSIZ], *lp;
    FILE *fp;

    if (argc < 2) {
	printf("usage: %s host [service]\n", argv[0]);
	exit(1);
    }

    /* Find the port for the tcpmux service */
    sp = getservbyname("tcpmux", "tcp");
    if (sp == NULL) {
	fprintf(stderr, "Can't find \"tcpmux\" service\n");
	exit(1);
    }

    /* Initialize the socket address to the server's address. */

    bzero((char *) &sin, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_port = sp->s_port;
    hp = gethostbyname(argv[1]);
    if (hp == NULL) {
	herror(argv[1]);
	exit(1);
    }
    bcopy (hp->h_addr, &(sin.sin_addr.s_addr), hp->h_length);

    if ((sock = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
	perror("Can't open socket");
	exit(1);
    }

    /* Use stdio for reading data from the server. */
    fp = fdopen(sock, "r");
    if (fp == NULL) {
	fprintf(stderr, "Can't create file pointer\n");
	exit(1);
    }

    /* Connect to the server. */

    if (connect(sock, &sin,sizeof(sin)) < 0) {
	close(sock);
	perror("Connect to server");
	exit(1);
    }
    printf("Connection established.\n");

    /*
     * If the server goes away while sending data, we'll get a SIGPIPE signal. 
     * Catch it so we can print an error message.
     */
    (void) signal(SIGPIPE, die);

    /* Send service request */
    sprintf(line, "%s\r\n", argc == 3 ? argv[2] : "current_time");
    if (write(sock, line, strlen(line)) < 0) {
	perror("write");
	exit(1);
    }

    /* Get ACK/NAK response from the server. */
    if (fgets(line, sizeof(line), fp) == NULL) {
	if (feof(fp)) {
	    die();
	} else {
	    fprintf(stderr, "Error reading response\n");
	    exit(1);
	}
    }

    /* Delete <CR> */
    if ((lp = index(line, '\r')) != NULL) {
	*lp = '\0';
    }

    switch (line[0]) {
	case '+':
		printf("Got ACK: %s\n", &line[1]);
		break;
	case '-':
		printf("Got NAK: %s\n", &line[1]);
		exit(0);
	default:
		printf("Got unknown response: %s\n", line);
		exit(1);
    }

    /* Get rest of data from the server. */
    while ((fgets(line, sizeof(line), fp)) != NULL) {
	fputs(line, stdout);
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
