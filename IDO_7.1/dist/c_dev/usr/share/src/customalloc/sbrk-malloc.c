static char *sbrk_base = 0;
static char *sbrk_ptr = 0;
static char *sbrk_end = 0;

static int is_sbrk(void *mem)
{
  SizeType* foo = (SizeType *) mem;
  return( foo[-1] & 0x2 );
}

/*
 * The sbrk malloc allocates 2 * SIZE_SZ overhead, just like the other
 * malloc, because we want to consistency check routines to be happy.
 */
static MallocPtrType __sbrk_malloc(MallocArgType size)
{
  int rounded_size = ((size + 0x3) & (~0x3)) + 2 * SIZE_SZ;
  SizeType *next = (unsigned int *) (sbrk_ptr + rounded_size);
  SizeType *base; 

  if ( (SizeType) next >= (SizeType) sbrk_end ) {

#ifndef __customalloc_HowMuchToSbrk_
#  define __customalloc_HowMuchToSbrk_ ((8 * 1024) - (4 * SIZE_SZ))
#endif

    int alloc = __customalloc_HowMuchToSbrk_;
    int over = sbrk_end - sbrk_ptr;

    /*
     * Return unused portion of this extent
     */
    if ( sbrk_base && over > MINSIZE ) {
      /*
       * We know this will never copy.
       */
      int required = sbrk_ptr - sbrk_base + 4;
      char *new = __internal_realloc(sbrk_base, required + 4);
      if ( (unsigned int) new != (unsigned int) sbrk_base ) abort();

      sbrk_base = 0;
      sbrk_ptr = 0;
      sbrk_end = 0;
    }

    if ( alloc < rounded_size ) {
      return( __internal_malloc(size) );
    } 
    sbrk_base = (char *) __internal_malloc( alloc );
    sbrk_ptr = sbrk_base;
    sbrk_end = sbrk_ptr + alloc;
    next = (SizeType *) (sbrk_ptr + rounded_size);
  }

  base = (SizeType *) sbrk_ptr;
  base[0] = rounded_size | 0x3; /* Mark front end of the chunk */
  sbrk_ptr = (char *) next;
  return( (MallocPtrType) &base[1] );
}

