/*
**  NAME
**      cyclecntr.c - syssgi(SGI_QUERY_CYCLECNTR) example which reads the
**		      system cycle counter
**
**  DESCRIPTION
**      The following program reads the system cycle counter for interval
**      timing purposes.  Different systems have different tick intervals.
**      The actual interval is returned as the number of picoseconds via arg1 
**      of syssgi(SGI_QUERY_CYCLECNTR, arg1) call.  The counter is available 
**      after using mmap(2) to make the counter accessible in the program's 
**	address space.
**
**  COMPILATION INSTRUCTIONS
**      R4400:  cc -O -DR4400 -mips2 cyclecntr.c -o cyclecntr
**      Other:  cc -O cyclecntr.c -o cyclecntr
**
**  EXECUTION INSTRUCTIONS
**	Run as root for less tick interval variations:  npri -h 30 ./cyclecntr
**
**  CAVEATS
**	On the R4400 hardware family, the counter is 64 bits.  On most other
**	SGI systems that support this function, the address of a 32 bit counter 
**	is returned (R3000 Indigo, 4D/30, and 4D/35 have 24 bit counters).  The
** 	number of bits defines the point at which the next increment wraps to 0.
*/

#include <fcntl.h>
#include <sys/syssgi.h>
#include <sys/immu.h>
#include <sys/mman.h>
#include <limits.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>

#define MICROSECS_PER_PICOSEC 1.0e-6
#define SECS_PER_PICOSEC 1.0e-12
#define ITERATIONS 100


/*
** counterPointer is the pointer to cycle counter.  It must be volatile so 
** that the counter is re-read from (mmap-ed) memory upon each reference 
** instead of from cache.     
*/
#if defined(_LONGLONG) && defined(R4400)		 	    
volatile unsigned long long *counterPointer;
#else					   
volatile unsigned int* counterPointer;		    
#endif

unsigned int interval[ITERATIONS+1];

void prn_timer_info(int, unsigned, unsigned, unsigned, size_t);
void read_counter(void);
void prn_results(int);
void close_up(int, unsigned, size_t);

main()
{
    int fd, counterResolution;
    size_t   counterMapSize;
    unsigned counterAddr, counterBase, counterMapBase;


    /*
    **  Get address (not pointer) of free running counter
    */
    counterAddr = (unsigned) syssgi(SGI_QUERY_CYCLECNTR, &counterResolution);
    if (counterAddr == (unsigned) -1)
    {
        if (errno==ENODEV){
                fprintf(stderr,"No timer exists on this machine.\n");
                exit(1);
        }
        perror("syssgi SGI_QUERY_CYCLECNTR");
        exit(1);
    }

    if ((fd = open("/dev/mmem", O_RDONLY, 0)) == -1) {
        perror("open /dev/mmem");
        exit(1);
    }

    /*
    **  Not all SGI systems return a page aligned address for the counter, 
    **  so account for that.
    */
    counterBase = (unsigned) counterAddr & ~POFFMASK;

    counterMapSize = (size_t)counterAddr - (size_t)counterBase +
            sizeof(*counterPointer);

    counterMapBase = (unsigned) mmap((caddr_t)0,
            counterMapSize, PROT_READ, MAP_PRIVATE, fd, (off_t)counterBase);
    if (counterMapBase == -1) {
        perror("mmap counterMapBase");
        exit(1);
    }

#if defined(_LONGLONG) && defined(R4400) 
    counterPointer = (unsigned long long *)(counterMapBase + poff(counterAddr));
#else
    counterPointer = (unsigned *) (counterMapBase + poff(counterAddr));
#endif


   prn_timer_info(counterResolution, counterAddr, counterBase, counterMapBase, 
		  counterMapSize);
   read_counter();
   prn_results(counterResolution);
   close_up(fd, counterMapBase, counterMapSize);
}


/*
** Print out timer information.
*/
void prn_timer_info(int counterResolution, unsigned counterAddr, 
	            unsigned counterBase, unsigned counterMapBase, 
		    size_t counterMapSize)
{
    printf("Resolution is %d picoseconds (%4.1f megahertz).\n",
                counterResolution,1000000./counterResolution);
#if defined(_LONGLONG) && defined(R4400)
    printf("Wrap-around time = %16.16llu seconds.\n",
                counterResolution * SECS_PER_PICOSEC * ULONGLONG_MAX);
#else
    printf("Wrap-around time = %0.2f seconds.\n",
                counterResolution * SECS_PER_PICOSEC * UINT_MAX);
#endif
    printf("counterAddr = 0x%lx\n", counterAddr);
    printf("counterBase = 0x%lx\n", counterBase);
    printf("counterMapBase = 0x%lx\n", counterMapBase);
    printf("counterMapSize = %ld\n", counterMapSize);
}


/*
**  Read the counter
*/
void read_counter(void )
{
    int i;
#if defined(_LONGLONG) && defined(R4400) 		 	    
    unsigned long long currentCounter, previousCounter=0; 
#else					   
    unsigned currentCounter, previousCounter=0; 
#endif


    for (i=0; i<=ITERATIONS; i++)
    {
        currentCounter = *counterPointer; /* reads current value from mmapped */
                                          /* counter */

#if defined(_LONGLONG) && defined(R4400) && defined(VERBOSE)
	printf ("Counter is %llu.\n",currentCounter);  
#elif defined(VERBOSE)
	printf ("Counter is %u.\n",currentCounter);  
#endif

        /*
        **  Compute time interval and take into account counter wrap-around.
        */
        interval[i] = (currentCounter >= previousCounter) ?
            	       currentCounter - previousCounter :
        	       currentCounter + UINT_MAX - previousCounter;

        previousCounter = currentCounter;
    }
}


/*
**  Print results.  Results are not printed in loop because they increase
**  interval.
*/
void prn_results(int counterResolution)
{
    int i;

    for (i=1; i<=ITERATIONS; i++)
    {
        printf("Interval %d: ticks %d, time %.3f microsecs\n", i, interval[i],
                  interval[i] * counterResolution * MICROSECS_PER_PICOSEC);
    }
}


/*
** Close down and clean up.  The munmap is redundant since that happens upon
** close and/or exit automatically.
*/
void close_up(int fd, unsigned counterMapBase, size_t counterMapSize)
{
    if (munmap((void *) counterMapBase, counterMapSize) == -1)
    {
        perror("munmap");
        exit(1);
    }

    if (close(fd) == -1)
    {
        perror("close /dev/mmem");
        exit(1);
    }
}
