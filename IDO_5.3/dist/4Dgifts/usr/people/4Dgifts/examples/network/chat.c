/* 
 * chat -- 
 *
 *    A primitive talk program that illustrates remote communications using the
 *    following system calls: socket, connect, bind, listen, accept, select.
 *
 *    Usage:
 *	chat 		-- to accept a connection from a remote system
 *	chat hostname	-- to talk to a remote system
 *
 *	You need to run 'chat' first on host 1, then 'chat hostname' on host 2
 *
 * The information in this software is subject to change without notice
 * and should not be construed as a commitment by Silicon Graphics, Inc.
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/time.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <fcntl.h>

/* Listen for connect requests on this arbitrarily-chosen port */
#define SERVER_PORT	(IPPORT_USERRESERVED + 123)

char *usage = "usage: %s [host]\n";

main(argc, argv)
int argc;
char **argv;
{
	int	sock;
	int	flag = 1;
	int 	msgsock, keyboard;
	char	line[80];
	int	count;
	struct	timeval time_out;
	int	nfound;
	fd_set	readers;

	if (argc > 2) {
		printf(usage, argv[0]);
		exit(0);
	}

	/* open up an fd for the keyboard */
	if (keyboard = open("/dev/tty",O_RDWR) < 0) {
		perror("Error opening /dev/tty");
		exit(1);
	}

	/* create a socket */
	sock = socket (AF_INET,SOCK_STREAM,0);
	if (sock < 0) {
		perror("Error opening socket");
		exit(1);
	}

	if (argc == 2) { /* connecting to remote host */
		msgsock = connect_to_host(sock,argv[1]);
	} else {
		msgsock = wait_for_connection(sock);
	}
	do {
		/* initialize the fd_set */
		FD_ZERO(&readers); 

		/* add the keyboard */
		FD_SET(keyboard, &readers);

		/* add the socket */
		FD_SET(msgsock, &readers); 

	        /* set a 5 second time out */
		time_out.tv_sec = 5;
		time_out.tv_usec = 0;

		/* look for i/o events */
		nfound = select(FD_SETSIZE,&readers,0,0,&time_out);
		if (nfound == -1) {
			perror("select");
			exit(1);
		}

		/* process all events */
		while (nfound) {  
		    /* is there something on the socket? */
		    if (FD_ISSET(msgsock, &readers))
		    {
			FD_CLR(msgsock,&readers);
			nfound--;
			count = read(msgsock, line, 80);
			if (count == -1)
			    perror("read");
			else if (count) {
			    write(1, "> ", 2);
			    if (write(1,line,count) < 0) {
				perror("write on screen");
				exit(1);
			    }
			}
			else  /* disconnect on eof */
			    flag = 0;
		    }

		    /* is there something on the keyboard? */
		    if (FD_ISSET(keyboard, &readers))
		    {
			FD_CLR(keyboard,&readers);
			nfound--;
			count = read(keyboard, line, 80);
			if (count) {
			    if (write(msgsock,line,count) < 0) {
				perror("write on socket");
				exit(1);
			    }
			}
			else /* disconnect on eof */
			    flag = 0;
		    }
		}
	} while (flag);
	close(msgsock);
	printf("Done\n");
}

connect_to_host(sock,hostname)
int sock;
char *hostname;
{
	struct hostent *hp;
	struct sockaddr_in sin;

	/* initialize socket address structure */
	bzero((char *) &sin, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_port = htons(SERVER_PORT);

	hp = gethostbyname(hostname);      
	if (hp == NULL) {
	    herror(hostname);
	    exit(1);
	}
	bcopy (hp->h_addr, &(sin.sin_addr.s_addr), hp->h_length); 

	/* connect to remote host */

	if (connect(sock,&sin,sizeof(sin)) < 0) {
		close(sock);
		perror("Can't connect to server");
		exit(1);
	}
	printf("Connection established\n");
	fflush(stdout);
	return(sock);
}

wait_for_connection(sock)
int sock;
{
	int msgsock;
	int length;
	struct sockaddr_in sin;

	/* initialize socket address structure */

	bzero((char *) &sin, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_port = htons(SERVER_PORT);
	sin.sin_addr.s_addr = INADDR_ANY;

	/* bind socket data structure to this socket */

	if (bind (sock,&sin,sizeof(sin))) {
		perror("Error binding socket");
		close(sock);
		exit(1);
	}

	/* prepare socket queue for connection requests and accept 
	   connections */

	listen(sock,5);

	length = sizeof(sin);
	msgsock = accept(sock, &sin, &length);
	if (msgsock < 0) {
	    perror("accept");
	    close(sock);
	    exit(1);
	}

	printf("Connection from host %s, port %u\n",
	    inet_ntoa(sin.sin_addr), ntohs(sin.sin_port));
	fflush(stdout);

	close(sock);
	
	return(msgsock);
}
