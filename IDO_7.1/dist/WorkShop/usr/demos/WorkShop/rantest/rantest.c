/* test.c - random program to use for demo of procview */

#include	<signal.h>

int	i;	/* used for random number switch */
char	*exitmsg = "rantest: signal received, exiting\n";

void	sigint();	/* signal handling routine for termination */
void	systime();	/* routine to use a bunch of system time */
void	usrtime();	/* routine to use a bunch of user time */

main()
{
	/* set up to exit upon SIGINT or SIGTERM */
	signal(SIGINT, sigint);
	signal(SIGTERM, sigint);

	/* loop forever */
	for (;;) {
		/* generate a random number between 1 and 9 */

		i = (rand()/32)%10;

		/* switch on its value */

		switch (i) {
		case 0:
		case 1:
		case 2:
		case 3:
		case 4:
		case 5:
		case 6:
			/* this will use user CPU time for a while */
			usrtime();
			break;

		case 7:
		case 8:
			/* this will accumulate mostly system time */
			systime();
			break;

		case 9:
			/* this will use no resources for ~1 sec. */
			sleep(1);
			break;
		}
	}
}

/*	systime - loop to use a bunch of system time */

void
systime()
{
	int	j;	/* temp value for loop */
	int	k;	/* temp value for storing pid */

	for(j=0; j<10000; j++) {
		k = getpid();
	}
}

/*	usrtime - loop to use a bunch of user time */

void
usrtime()
{
	int	j;	/* temp value for loop */
	float	x;	/* temp variable for f.p. calculation */

	x = 0.0;
	for(j=0; j<100000; j++) {
		x = x + 1.0;
	}
}

/*  sigint - signal handler for SIGINT and SIGTERM
 *	calls exit, so that pixified version will write counts
 */

void
sigint(sig, code, sc)
	int	sig;	/* signal number */
	int	code;	/* code from signal */
	struct	sigcontext	*sc;	/* signal context */
{
	/* write a message  to stderr, fd = 2 */

	write(2, exitmsg, strlen(exitmsg));
	exit(0);
}
