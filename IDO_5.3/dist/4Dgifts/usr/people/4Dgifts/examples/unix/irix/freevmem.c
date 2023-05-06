/*
// Grant Dorman - 1990
// Matt D. Robinson - 1992 (Modifications to use swapctl(2))
*/
#include <stdio.h>
#include <sys/types.h>
#include <sys/immu.h>
#include <sys/swap.h>
#include <sys/sysmp.h>
#include <sys/sysmips.h>
#include <mntent.h>

#define ptok(x) ((x)<<12)

/*
// getswapinfo() -- Get standard swap information.  Try to obtain some
//                  standard statistics for swap space sizes.
*/
int getswapinfo(pgno_t *max, pgno_t *free)
{
	register swaptbl_t *sinfo;
	int i;

	/*
	// Use the new swapctl() call to get this information.
	*/
	if (swapctl(SC_GETSWAPMAX, max) < 0)
	{
		perror("swapctl");
		return -1;
	}

	if (swapctl(SC_GETFREESWAP, free) < 0)
	{
		perror("swapctl");
		return -2;
	}
	return 0;
}

/*
// getmeminfo()  -- Get standard memory information.  In this function,
//                  we utilize the sysmp() system call, and pass the
//                  data back to the caller.
*/
int getmeminfo(struct rminfo *rmi)
{
	int rminfosize;

	/*
	// Get real memory information.
	*/
	rminfosize = sysmp(MP_SASZ, MPSA_RMINFO);

	/*
	// Now pass sysmp() four arguments, the first pointing to
	// the request, the second the type of information we want
	// (in this case, real memory information), the third which
	// is the point to a buffer in the address space, and
	// the last argument is the number of bytes to transfer.
	*/
	if (sysmp(MP_SAGET, MPSA_RMINFO, (char *) rmi, rminfosize) < 0)
	{
		perror("sysmp");
		return -1;
	}
	return 0;
}

/*
//
*/
int main()
{
	struct rminfo rmi;
	pgno_t        pt_max;
	pgno_t        pt_free;

	/*
	// Try to find out swap information.
	*/
	if (getswapinfo(&pt_max, &pt_free) < 0)
	{
		/*
		// Getting swap information failed.
		*/
		return 1;
	}
	else
	{
		/*
		// Print out our results.
		*/
		printf("Total swap space      : %2.2d pages (%2.2d bytes)\n",
			pt_max, ((int)pt_max * (int)getpagesize()));
		printf("Total free swap space : %2.2d pages (%2.2d bytes)\n",
			pt_free, ((int)pt_free * (int)getpagesize()));
	}

	/*
	// Try to find out the real memory information.
	*/
	if (getmeminfo(&rmi) < 0)
	{
		/*
		// Getting real memory information failed.
		*/
		return 1;
	}
	else
	{
		/*
		// Print out our results.
		*/
		printf("Total physical memory : %2.2d bytes\n", ptok(rmi.physmem));
		printf("Total free memory     : %2.2d bytes\n", ptok(rmi.freemem)); 
	}
	return 0;
}
