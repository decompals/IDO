/*
 * NAME
 *	ercv - receive Ethernet packets.
 * SYNOPSIS
 *	ercv [-b bufsize] [-c count] [-i interface] [-t type] [-v] [fromether]
 * NOTES
 *	A negative count causes ercv to receive indefinitely.
 *	Without fromether, ercv captures all packets of the desired type.
 *
 *	written by: Brendan Eich.
 */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <net/raw.h>
#include <netinet/if_ether.h>	/* declares struct ether_header */
#include <netinet/in.h>		/* for htons() macro */

char	*malloc();
char	*ether_aton();

#define	DEFAULT_BUFSIZE		1000
#define	DEFAULT_COUNT		1000
#define	DEFAULT_TYPE		0xfeed

struct bufhdr {
	struct snoopheader	snoop;
	u_char			pad[RAW_HDRPAD(sizeof(struct ether_header))];
	struct ether_header	ether;
};

main(argc, argv)
	int argc;
	char **argv;
{
	int bufsize, count, verbose;
	char *interface;
	u_short type;
	int opt, sock, cc;
	struct sockaddr_raw sr;
	struct snoopfilter sf;
	struct ether_header *eh;
	struct ifreq ifreq;
	struct bufhdr *bh;
	u_long lastseq;
	int dropped, got;
	struct timeval tv0, tv;
	u_char *bp;
	extern char *optarg;
	extern int optind;
	long strtol();

	/*
	 * Initialize.
	 */
	bufsize = DEFAULT_BUFSIZE;
	count = DEFAULT_COUNT;
	interface = 0;
	type = DEFAULT_TYPE;
	verbose = 0;

	/*
	 * Process options.
	 */
	while ((opt = getopt(argc, argv, "b:c:i:t:v")) != EOF)
		switch (opt) {
		  case 'b':
			bufsize = atoi(optarg);
			break;
		  case 'c':
			count = atoi(optarg);
			break;
		  case 'i':
			interface = optarg;
			break;
		  case 't':
			type = strtol(optarg, (char **) 0, 0);
			break;
		  case 'v':
			verbose = 1;
			break;
		  default:
usage:
			fprintf(stderr,
"usage: %s [-b bufsize] [-c count] [-i interface] [-t type] [-v] [fromether]\n",
				argv[0]);
			exit(-1);
		}
	if (optind < argc - 1)
		goto usage;

	/*
	 * Create a socket and bind it to a free port on interface.
	 */
	sock = socket(AF_RAW, SOCK_RAW, RAWPROTO_SNOOP);
	if (sock < 0) {
		perror(interface ? interface : "default interface");
		exit(errno);
	}
	sr.sr_family = AF_RAW;
	sr.sr_port = 0;
	/*
	 * The raw sockaddr interface name field, sr_ifname, must be padded
	 * with zeroes, so use strncpy.
	 */
	if (interface)
		(void) strncpy(sr.sr_ifname, interface, sizeof sr.sr_ifname);
	else
		bzero(sr.sr_ifname, sizeof sr.sr_ifname);
	if (bind(sock, &sr, sizeof sr) < 0) {
		perror("bind");
		exit(errno);
	}

	/*
	 * If the interface wasn't specified, inquire its name.
	 */
	if (interface == 0) {
		cc = sizeof sr;
		if (getsockname(sock, (struct sockaddr *)&sr, &cc) < 0) {
			perror("getsockname");
			exit(errno);
		}
		interface = sr.sr_ifname;
	}

	/*
	 * The following magic number was derived by plugging IRIX kernel
	 * parameters into the BSD formula used to limit socket buffer
	 * reservation.
	 */
	opt = 61680;
	if (setsockopt(sock, SOL_SOCKET, SO_RCVBUF, (char *) &opt, sizeof opt)
	    < 0) {
		perror("setsockopt");
		exit(errno);
	}

	/*
	 * Initialize the snoop filter's mask first.
	 */
	bzero((char *) &sf, sizeof sf);
	eh = RAW_HDR(sf.sf_mask, struct ether_header);
	(void) memset(eh->ether_dhost, 0xff, sizeof eh->ether_dhost);
	if (argv[optind])
		(void) memset(eh->ether_shost, 0xff, sizeof eh->ether_shost);
	eh->ether_type = 0xffff;

	/*
	 * Now initialize the snoop filter's match vector.
	 */
	eh = RAW_HDR(sf.sf_match, struct ether_header);
	(void) strncpy(ifreq.ifr_name, interface, sizeof ifreq.ifr_name);
	if (ioctl(sock, SIOCGIFADDR, &ifreq) < 0) {
		perror("getting interface address");
		exit(errno);
	}
	bcopy(ifreq.ifr_addr.sa_data, eh->ether_dhost, sizeof eh->ether_dhost);
	if (argv[optind]
	    && ether_hostton(argv[optind], eh->ether_shost) != 0) {
		u_char *ehost = ether_aton(argv[optind]);
		if (ehost == 0) {
			fprintf(stderr, "%s: unknown Ethernet host %s\n",
				argv[0], argv[optind]);
			exit(-1);
		}
		bcopy(ehost, eh->ether_shost, sizeof eh->ether_shost);
	}
	eh->ether_type = htons(type);

	/*
	 * Finally, add the filter.
	 */
	if (ioctl(sock, SIOCADDSNOOP, (char *) &sf) < 0) {
		perror("adding snoop filter");
		exit(errno);
	}

	/*
	 * Allocate a buffer starting with an Ethernet header.
	 */
	bufsize += sizeof *bh;
	bh = (struct bufhdr *) malloc(bufsize);
	if (bh == 0) {
		perror("malloc");
		exit(errno);
	}

	/*
	 * Turn snooping on.
	 */
	opt = 1;
	if (ioctl(sock, SIOCSNOOPING, (char *) &opt) < 0) {
		perror("turning snooping on");
		exit(errno);
	}

	/*
	 * Receive loop.
	 */
	setbuf(stdout, (char *) 0);
	lastseq = -1;
	dropped = 0;
	(void) gettimeofday(&tv0, (struct timezone *) 0);
	for (got = 0; count != 0; --count, got++) {
		/*
		 * Read a packet.
		 */
		cc = read(sock, (char *) bh, bufsize);
		if (cc < 0) {
			perror("read");
			exit(errno);
		}
		if (verbose)
			putchar('.');
		if (cc != bufsize) {
			printf("read %d of %d bytes after %d good packet%s\n",
				cc, bufsize, got, (got != 1) ? "s" : "");
			break;
		}

		/*
		 * Notice receiver sequence gaps, indicating local
		 * buffer shortage or queue overflow.
		 */
		if (bh->snoop.snoop_seq != lastseq + 1)
			dropped += bh->snoop.snoop_seq - lastseq - 1;

		/*
		 * Check test pattern.
		 */
		cc -= sizeof *bh;
		for (bp = (u_char *)(bh + 1); --cc >= 0; bp++) {
			if (*bp != ((bp - (u_char *)(bh + 1)) & 0xff))
				break;
		}
		if (cc >= 0) {
			printf("data corrupt after %d good packet%s\n",
				got, (got != 1) ? "s" : "");
			printf("index sent rcvd\n");
			for (bp = (u_char *)(bh + 1); --cc >= 0; bp++) {
				int index = bp - (u_char *)(bh + 1);
				if (*bp != (index & 0xff)) {
					fprintf(stderr,
						"%04d: %4d %4d\n",
						index, (index & 0xff),
						*bp);
				}
			}
			break;
		}
		lastseq = bh->snoop.snoop_seq;
	}

	/*
	 * Calculate rates and display results.
	 */
	(void) gettimeofday(&tv, (struct timezone *) 0);
	tv.tv_sec -= tv0.tv_sec;
	tv.tv_usec -= tv0.tv_usec;
	if (tv.tv_usec < 0) {
		tv.tv_usec += 1000000;
		--tv.tv_sec;
	}

	if (dropped)
		printf("dropped %d packets\n", dropped);
	printf("%d.%06d seconds: %g kbytes/second, %g packets/second\n",
		tv.tv_sec, tv.tv_usec,
		(got * bufsize * 1000000.)
		 / (1024 * (tv.tv_sec * 1000000. + tv.tv_usec)),
		(got * 1000000.)
		 / (tv.tv_sec * 1000000. + tv.tv_usec));
	exit(0);
}
