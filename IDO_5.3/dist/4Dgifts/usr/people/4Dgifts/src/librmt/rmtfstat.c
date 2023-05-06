#ident	"$Header: /proj/irix5.3/isms/4Dgifts/src/librmt/RCS/rmtfstat.c,v 1.5 1994/06/17 06:14:43 doucette Exp $"

#include "rmtlib.h"

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

static int _rmt_fstat(int, char *);

/*
 *	Get file status.  Looks just like fstat64(2) to caller.
 */
 
int rmtfstat64 (fildes, buf)
int fildes;
struct stat64 *buf;
{
	if (isrmt (fildes))
	{
		return (_rmt_fstat (fildes - REM_BIAS, (char *)buf));
	}
	else
	{
		return (fstat64 (fildes, buf));
	}
}

/*
 *	Get file status.  Looks just like fstat(2) to caller.
 */

int rmtfstat (fildes, buf)
int fildes;
struct stat *buf;
{
	if (isrmt (fildes))
	{
		return (_rmt_fstat (fildes - REM_BIAS, (char *)buf));
	}
	else
	{
		return (fstat (fildes, buf));
	}
}

static int
_rmt_fstat(int fildes, char *arg)
{
	char buffer[ BUFMAGIC ];
	int rc, cnt;

	if (server_version == -1) {
		/* server doesn't know about this command
		** just return -1 and set errno and hope
		** that user program can handle that
		*/
		errno = EOPNOTSUPP;
		return(-1);
	}
		
	sprintf( buffer, "Z%d\n", fildes );

	/*
	 *	grab the status and read it directly into the structure
	 *	this assumes that the status buffer is (hopefully) not
	 *	padded and that 2 shorts fit in a long without any word
	 *	alignment problems, ie - the whole struct is contiguous
	 *	NOTE - this is probably NOT a good assumption.
	 */

	if (_rmt_command(fildes, buffer) == -1 ||
	    (rc = _rmt_status(fildes)) == -1)
		return(-1);

	for (; rc > 0; rc -= cnt, arg += cnt)
	{
		cnt = read(READ(fildes), arg, rc);
		if (cnt <= 0)
		{
			_rmt_abort(fildes);
			errno = EIO;
			return(-1);
		}
	}
	return(0);
}
