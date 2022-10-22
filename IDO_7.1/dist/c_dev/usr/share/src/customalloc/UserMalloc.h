/* -*-c-*- */
#ifndef _UserMalloc_h_
#define _UserMalloc_h_

#ifndef _UserMalloc_pre_h_
#  include "UserMalloc-pre.h"
#endif

#ifndef SIZE_SZ
#  define SIZE_SZ                   (sizeof(SizeType))
#endif

#ifndef MINSIZE
#  define MINSIZE			  16 /* from malloc.c */
#endif

/*
 * Return the malloc_chunk structure from a value returned by malloc
 */

static MallocChunk *external_to_malloc(SizeType *v)
{
  return( (MallocChunk*) &v[-1]) ;
}

/*
 * The other way around
 */
static SizeType *malloc_to_external(MallocChunk *p)
{
  SizeType *v = (SizeType*) p;
  return( &v[1]);
}

static int malloc_size(MallocChunk *p)
{
  return ( (p -> size) & ~0x1 );
}

/*****************************************************************************/


#ifndef CUSTOMALLOC_CACHE_BITS
#  define CUSTOMALLOC_CACHE_BITS	(5)
#endif

#define CUSTOMALLOC_CACHE_VALUE	(1 << CUSTOMALLOC_CACHE_BITS)
#define CUSTOMALLOC_CACHE_MASK	(~(CUSTOMALLOC_CACHE_VALUE - 1 ))

#ifdef _IN_OUTPUT_
static void *__customalloc_unlink(char **list)
{
  char **p = (char **) *list;
  *list = p[0];
  return( &p[0] );
}

static void __customalloc_link(char **list, void *ptr)
{
  char **p = (char **) ptr;
  p[0] = *list;
  *list = (char *) ptr;
}
#endif

static int size_external_to_internal (int size)
{
  return( (size + CUSTOMALLOC_CACHE_VALUE - 1) & CUSTOMALLOC_CACHE_MASK );
}

static int size_malloc_to_internal (int size)
{
  return( size & CUSTOMALLOC_CACHE_MASK );
}

#endif
