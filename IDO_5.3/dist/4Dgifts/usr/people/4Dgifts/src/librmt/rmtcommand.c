#ident	"$Header: /proj/irix5.3/isms/4Dgifts/src/librmt/RCS/rmtcommand.c,v 1.2 1992/08/02 20:54:17 ism Exp $"

#include <signal.h>
#include <errno.h>

#include "rmtlib.h"

extern int errno;		/* Not in 4.2's <errno.h> */

/*
 *	_rmt_command --- attempt to perform a remote tape command
 */

int _rmt_command(fildes, buf)
int fildes;
char *buf;
{
	register int blen;
	void (*pstat)();

/*
 *	save current pipe status and try to make the request
 */

	blen = strlen(buf);
	pstat = signal(SIGPIPE, SIG_IGN);
	if (write(WRITE(fildes), buf, blen) == blen)
	{
		signal(SIGPIPE, pstat);
		return(0);
	}

/*
 *	something went wrong. close down and go home
 */

	signal(SIGPIPE, pstat);
	_rmt_abort(fildes);

	errno = EIO;
	return(-1);
}



