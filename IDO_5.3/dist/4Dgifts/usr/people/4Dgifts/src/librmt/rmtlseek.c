#ident	"$Header: /proj/irix5.3/isms/4Dgifts/src/librmt/RCS/rmtlseek.c,v 1.3 1994/06/17 06:14:45 doucette Exp $"

#include "rmtlib.h"
#include <sys/types.h>

static long _rmt_lseek(int, long, int);

/*
 *	Perform lseek on file.  Looks just like lseek64(2) to caller.
 */

off64_t rmtlseek64 (fildes, offset, whence)
int fildes;
off64_t offset;
int whence;
{
	if (isrmt (fildes))
	{
		return (_rmt_lseek (fildes - REM_BIAS, offset, whence));
	}
	else
	{
		return (lseek64 (fildes, offset, whence));
	}
}

/*
 *	Perform lseek on file.  Looks just like lseek(2) to caller.
 */

long rmtlseek (fildes, offset, whence)
int fildes;
long offset;
int whence;
{
	if (isrmt (fildes))
	{
		return (_rmt_lseek (fildes - REM_BIAS, offset, whence));
	}
	else
	{
		return (lseek (fildes, offset, whence));
	}
}


/*
 *	_rmt_lseek --- perform an imitation lseek operation remotely
 */

static long _rmt_lseek(int fildes, long offset, int whence)
{
	char buffer[BUFMAGIC];

	sprintf(buffer, "L%d\n%d\n", offset, whence);
	if (_rmt_command(fildes, buffer) == -1)
		return(-1);

	return(_rmt_status(fildes));
}


