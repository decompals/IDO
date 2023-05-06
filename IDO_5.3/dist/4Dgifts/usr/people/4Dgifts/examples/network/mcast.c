/*
 * mcast --
 *
 *	This program demonstrates how to send and receive UDP multicast 
 *	datagrams. It's also useful for testing multicast forwarding within
 *	a LAN. In sending mode, a simple message containing the hostname and
 *	current time is sent every 5 seconds. In receiving mode, messages
 *	from every sender are printed.
 *
 *	To be the sender:	  	
 *
 *	    mcast -s [-t ttl] [-i interface] [-g mcast-group-address] [-p port]
 *
 *	On all receiving machines:	
 *
 *	    mcast [-i interface] [-g mcast-group-address] [-p port]
 *
 *	By default, mcast will use the primary interface, a group address of
 *	225.0.0.250, and UDP port number 8123 and a TTL of 1. You may 
 *	override these defaults with command-line options.
 *
 *	To check multicast connectivity between machines on different
 *	networks, run:
 *	    mcast -s -t 16 &		# send mode, at most 15 routers
 *	    mcast			# receive mode
 *	on each machine. They should receive messages from themselves
 *	and all the others if multicast routers are configured properly.
 *
 *	See the Network Programmer's Guide for an explanation of the
 *	setsockopt() calls for multicasting. See mrouted(1M) for information
 *	on multicast routing.
 *
 *
 * The information in this software is subject to change without notice
 * and should not be construed as a commitment by Silicon Graphics, Inc.
 */

#include <stdlib.h>
#include <bstring.h>
#include <getopt.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <net/if.h>
#include <net/soioctl.h>
#include <arpa/inet.h>

/* 
 * Note: these values are chosen not to collide with known multicast 
 * protocols and ports, but they might be wrong...
 */

#define EXAMPLE_PORT	8123
#define EXAMPLE_GROUP	"239.0.0.239"

#define EXAMPLE_TTL	1	/* Time-to-live, 1 == don't forward  */

/* Send a message every x seconds */
#define INTERVAL        5


static void
usage(char *name)
{
    fprintf(stderr,
	"usage: %s [-s [-t ttl]] [-i interface] [-p port] [-g group]\n",
	name);
    exit(1);
}

main(int argc, char **argv)
{
    struct sockaddr_in	addr;
    int			addrlen, fd, cnt, i;
    struct ip_mreq	mreq;
    char		message[80];
    char		*group = EXAMPLE_GROUP;
    u_short		port = EXAMPLE_PORT;
    u_char		ttl = EXAMPLE_TTL;
    int			send_mode = 0;
    int			iflag = 0;
    char		*interface;
    struct in_addr	ifaddr;
    struct in_addr	grpaddr;

    while ((i = getopt(argc, argv, "g:i:p:st:")) != EOF) {
	switch (i) {
	    case 'g':
		group = optarg;
		break;
	    case 'i':
		iflag++;
		interface = optarg;
		break;
	    case 'p':
		port = atoi(optarg);
		break;
	    case 's':
		send_mode = 1;
		break;
	    case 't':
		ttl = atoi(optarg);	/* affects send mode only */
		break;
	    default:
		usage(argv[0]);
		/*NOTREACHED*/
	}
    }
    if (optind < argc - 1)
	usage(argv[0]);

    grpaddr.s_addr = inet_addr(group);
    if (!IN_MULTICAST(grpaddr.s_addr)) {
	fprintf(stderr, "Invalid multicast group address: %s\n",group);
	exit(1);
    }

    if (port != EXAMPLE_PORT)
	fprintf(stderr,
    "Warning: port %hu may be in use, check /etc/services for defined ports\n",
		port);


    fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) {
        perror("socket");
        exit(1);
    }

    bzero(&addr, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons(port);
    addrlen = sizeof(addr);

    ifaddr.s_addr = htonl(INADDR_ANY);

    if (iflag) {
	/* 
	 * Make sure the specified interface exists and is capable of doing
	 * multicasting.
	 */

	struct ifconf ifc;
	struct ifreq *ifr;
	char buf[BUFSIZ];

	ifc.ifc_len = sizeof(buf);
	ifc.ifc_buf = buf;
	if (ioctl(fd, SIOCGIFCONF, &ifc) < 0) {
	    perror("ioctl SIOCGIFCONF");
	    exit(1);
	}

	ifr = ifc.ifc_req;
	for (i = ifc.ifc_len/sizeof(*ifr); --i >= 0; ifr++) {
	    if (ifr->ifr_addr.sa_family != AF_INET)
		continue;
	    if (strncmp(ifr->ifr_name, interface, strlen(ifr->ifr_name)) == 0) {

		/* Obtain the interface's assigned network address */
		ifaddr = ((struct sockaddr_in *)&ifr->ifr_addr)->sin_addr;

		if (ioctl(fd, SIOCGIFFLAGS, ifr) < 0) {
		    perror("ioctl SIOCGIFFLAGS");
		    exit(1);
		}
		if (!(ifr->ifr_flags & IFF_MULTICAST)) {
		    fprintf(stderr,
			"%s: interface doesn't support multicasting\n",
			interface);
		    exit(1);
		}

		/* Specify the interface to use when sending packets */
		if (send_mode && setsockopt(fd, IPPROTO_IP, IP_MULTICAST_IF,
				    &ifaddr, sizeof(ifaddr)) < 0) {
		    perror("setsockopt mcast if");
		    exit(1);
		}
		break;
	    }
	}
	if (ifaddr.s_addr == htonl(INADDR_ANY)) {
	    fprintf(stderr, "%s: invalid or unknown interface\n", interface);
	    exit(1);
	}
    }

    if (send_mode) {

	char hostname[MAXHOSTNAMELEN];
	gethostname(hostname, sizeof(hostname));

	if (setsockopt(fd, IPPROTO_IP, IP_MULTICAST_TTL, &ttl, sizeof(ttl))) {
	    perror("setsockopt ttl");
	    exit(1);
	}

	addr.sin_addr = grpaddr;
	while (1) {
	    time_t t = time(0);
	    sprintf(message, "%s: %-24.24s", hostname, ctime(&t));
            cnt = sendto(fd, message, sizeof(message), 0, &addr, addrlen);
            if (cnt < 0) {
                perror("sendto");
                exit(1);
            }
            sleep(INTERVAL);
        }

    } else {  /* mode = receive  */
	int on;

	/* 
	 * Allow multiple instances of this program to listen on the same
	 * port on the same host. By default, only 1 program can bind
	 * to the port on a host.
	 */
	on = 1;
	if (setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, &on, sizeof(on)) < 0) {
	    perror("setsockopt REUSEPORT");
	    exit(1);
	}

	if (bind(fd, &addr, sizeof(addr)) < 0) {
            perror("bind");
            exit(1);
        }

        mreq.imr_multiaddr = grpaddr;
        mreq.imr_interface = ifaddr;
        if (setsockopt(fd, IPPROTO_IP, IP_ADD_MEMBERSHIP,
                    &mreq, sizeof(mreq)) < 0) {
            perror("setsockopt add membership");
            exit(1);
	}
	
	while (1) {
	    cnt = recvfrom(fd, message, sizeof(message), 0, &addr, &addrlen);
            if (cnt < 0) {
                perror("recvfrom");
                exit(1);
            } 
	    if (cnt == 0) {
                break;
            }
            printf("%s sent \"%s\"\n",
                    inet_ntoa(addr.sin_addr), message);
        }
    }
    exit(0);
}
