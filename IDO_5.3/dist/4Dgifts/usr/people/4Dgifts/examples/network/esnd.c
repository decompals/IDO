/*
 * NAME
 *	esnd - send Ethernet packets to a host.
 * SYNOPSIS
 *	esnd [-b bufsize] [-c count] [-i interface] [-t type] [-v] toether
 * NOTES
 *	A negative count causes esnd to send indefinitely.
 *
 *	written by: Brendan Eich.
 */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <net/raw.h>
#include <netinet/if_ether.h>

char	*malloc();
char	*ether_aton();

#define	DEFAULT_BUFSIZE		1000
#define	DEFAULT_COUNT		1000
#define	DEFAULT_TYPE		0xfeed

main(argc, argv)
	int argc;
	char **argv;
{
	int bufsize, count, verbose;
	char *interface;
	u_short type;
	int opt, sock, cc;
	struct sockaddr_raw sr;
	struct ether_header *eh;
	struct ifreq ifreq;
	u_char *bp;
	struct timeval tv0, tv;
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
	"usage: %s [-b bufsize] [-c count] [-i interface] [-t type] [-v] toether\n",
				argv[0]);
			exit(-1);
		}
	if (optind != argc - 1)
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
	if (setsockopt(sock, SOL_SOCKET, SO_SNDBUF, (char *) &opt, sizeof opt)
	    < 0) {
		perror("setsockopt");
		exit(errno);
	}

	/*
	 * Allocate a buffer starting with an Ethernet header.
	 */
	bufsize += sizeof *eh;
	eh = (struct ether_header *) malloc(bufsize);
	if (eh == 0) {
		perror("malloc");
		exit(errno);
	}

	/*
	 * Initialize the Ethernet header.
	 */
	if (ether_hostton(argv[optind], eh->ether_dhost) != 0) {
		u_char *ehost = ether_aton(argv[optind]);
		if (ehost == 0) {
			fprintf(stderr, "%s: unknown Ethernet host %s\n",
				argv[0], argv[optind]);
			exit(-1);
		}
		bcopy(ehost, eh->ether_dhost, sizeof eh->ether_dhost);
	}
	(void) strncpy(ifreq.ifr_name, interface, sizeof ifreq.ifr_name);
	if (ioctl(sock, SIOCGIFADDR, &ifreq) < 0) {
		perror("ioctl");
		exit(errno);
	}
	bcopy(ifreq.ifr_addr.sa_data, eh->ether_shost, sizeof eh->ether_shost);
	eh->ether_type = type;

	/*
	 * Fill rest of buffer with test pattern.
	 */
	cc = bufsize - sizeof *eh;
	for (bp = (u_char *)(eh + 1); --cc >= 0; bp++)
		*bp = bp - (u_char *)(eh + 1);

	/*
	 * Blast packets.
	 */
	setbuf(stdout, (char *) 0);
	(void) gettimeofday(&tv0, (struct timezone *) 0);
	for (cc = count; cc != 0; --cc) {
		if (write(sock, (char *) eh, bufsize) < 0) {
			perror("write");
			exit(errno);
		}
		if (verbose)
			putchar('.');
	}

	/*
	 * Calculate and display results.
	 */
	(void) gettimeofday(&tv, (struct timezone *) 0);
	tv.tv_sec -= tv0.tv_sec;
	tv.tv_usec -= tv0.tv_usec;
	if (tv.tv_usec < 0) {
		tv.tv_usec += 1000000;
		--tv.tv_sec;
	}
	printf("%d.%06d seconds: %g kbytes/second, %g packets/second\n",
		tv.tv_sec, tv.tv_usec,
		(count * bufsize * 1000000.)
		 / (1024 * (tv.tv_sec * 1000000. + tv.tv_usec)),
		(count * 1000000.)
		 / (tv.tv_sec * 1000000. + tv.tv_usec));
	exit(0);
}
