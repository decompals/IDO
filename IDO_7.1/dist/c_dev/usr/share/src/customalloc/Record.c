#include <stdio.h>
#include <assert.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

static CacheItem *Root = 0;

static unsigned int max(unsigned int a, unsigned int b)
{
  if (a > b ) return(a); else return(b);
}

static unsigned int min(unsigned int a, unsigned int b)
{
  if (a > b ) return(b); else return(a);
}

/*
 * Allocates and initializes a new CacheItem
 */
int TotalEvents = 0;

static CacheItem *cache_new(int size, CacheItem *next)
{
  CacheItem *item = (CacheItem *) __internal_malloc(sizeof(CacheItem));
  memset(item, 0, sizeof(CacheItem) );
  item -> size = size;
  item -> next = next;
  item -> last_event = TotalEvents;
  return( item );
}

static update_sums(CacheItem *item)
{
  double delta = (TotalEvents - item -> last_event);
  item -> sum_objects_in_free_list += delta * item -> objects_in_free_list;
  item -> sum_objects_in_use += delta * item -> objects_in_use;
  item -> last_event = TotalEvents;
}

/*
 * Takes an adjusted size, returns the adjusted CacheItem and moves
 * the CacheItem to the front
 */
static CacheItem *cache_find(int size)
{
  CacheItem *prior = 0;
  CacheItem *item;

  assert (size >= 0 );

  for (item = Root; item != 0; item = item -> next) {
    if ( item -> size ==  size ) {
      if ( prior != 0 ) {

	/* unlink */

	prior -> next = item -> next;
	
	/* relink */
	item -> next = Root;
	Root = item;
      }
      return (item);
    }
    prior = item;
  }
  Root = cache_new(size, Root);
  return( Root );
}

/*
 * Check that an address is valid
 */
static unsigned int trap_address = 0x0;
static valid_address(void *xx)
{
#if _DEBUG_
  unsigned int x = (unsigned int) xx;
#if 0
  static unsigned int foo = 0x0ff00000;
  extern int end;	
  static unsigned int end_addr = (unsigned int) &end;
#else
  static unsigned int foo = 0xffffffff;
  static unsigned int end_addr = (unsigned int) &Root;
#endif
  
  if ( x != 0 ) {  	
    assert (  x >  end_addr );
    assert (  x !=  trap_address );
    assert (  x <  foo );
  } 
#endif
}

/*
 * Adds a MallocChunk to a CacheItem
 */
static void cache_enq(CacheItem *item, MallocChunk *store)
{
  valid_address( (void *)store);
  valid_address( (void *)item);
  valid_address( (void *)item->root);

  store -> fd = item -> root;
  item -> root = store;

#ifndef _IGNORE_COUNTS_
  item -> objects_in_free_list += 1;
  item -> max_objects_in_free_list = max(item -> max_objects_in_free_list,
					 item -> objects_in_free_list);
  update_sums(item);
#endif

  valid_address( (void *)item);
  valid_address( (void *)item->root);
}

/*
 * Removes a MallocChunk from a CacheItem
 */
static MallocChunk* cache_deq(CacheItem *item)
{
  MallocChunk *store;

  valid_address( (void *)item);
  valid_address( (void *)item->root);

  store = item -> root;
  item -> root = store -> fd;

#ifndef _IGNORE_COUNTS_
  item -> objects_in_free_list -= 1;
  item -> sum_objects_in_free_list += item -> objects_in_free_list;
  update_sums(item);
#endif

  valid_address( (void *)item);
  valid_address( (void *)item->root);
  valid_address( (void *)store);

  return( store );
}



static int __cache_malloc_initialized__ = 0;

static void __cache_exit()
{
  CacheItem *item;
  for (item = Root; item != 0; item = item -> next) {
    update_sums(item);
  }
  customalloc_write_database(Root);
}

static void __cache_malloc_initialize()
{
  __cache_malloc_initialized__ = 1;
  Root = customalloc_read_database();
  atexit(__cache_exit); 
}



MallocPtrType malloc(MallocArgType size)
{
  int adjusted_size;
  CacheItem *item;
  void *return_value;
  
  TotalEvents++;
  
  if (size == 0) {
    /*
     * Record occurance of 0-sized item...
     */
    item = cache_find( 0 ); 
    item -> mallocs += 1;
    size = CUSTOMALLOC_CACHE_VALUE;
  }
  assert( size > 0 );
  
  if ( ! __cache_malloc_initialized__ ) {
    __cache_malloc_initialize();
  }
  
  adjusted_size = size_external_to_internal( size );
  
  item = cache_find( adjusted_size );
  
#ifndef _IGNORE_COUNTS_
  item -> mallocs += 1;
  item -> objects_in_use += 1;
  item -> max_objects_in_use= max(item -> max_objects_in_use,
				  item -> objects_in_use);
  update_sums(item);
#endif
  
  if ( item -> root ) {
    return_value = (void *) malloc_to_external( cache_deq(item) );
  } else {
    return_value = (void *) __internal_malloc( adjusted_size );
  }
  valid_address( (void *)return_value);
  
  if ( return_value == 0 ) { 
    static  char *argle = "Out of memory!\n";
    write(2,argle,strlen(argle));
    abort();
  }
  
  return ( return_value );
}

/*
 * We're inefficient about this in order to gather malloc stats..
 */ 

void* realloc(void* mem, unsigned int bytes)
{

 if ( mem == 0 ) {
    /*
     * Some people do the weirdest things.
     */
    return( (void *) malloc(bytes) );
  } else {
    MallocChunk *chunk = external_to_malloc(mem);
    int data_size = size_malloc_to_internal(malloc_size(chunk));
    int copy_size = (data_size < bytes) ? data_size : bytes;
    void *newmem = malloc(bytes);
    bcopy(mem, newmem, copy_size );
    free(mem);
    return( newmem );
  }
}


FreeRetType free(FreePtrType  p)
{
  TotalEvents++;
  
  valid_address( (void *)p);
  
  if ( p ) {
    MallocChunk *chunk = external_to_malloc(p);
    int adjusted_size =
      size_malloc_to_internal(malloc_size(chunk) - (2 * SIZE_SZ) );
    
    CacheItem *item;
    
    assert( adjusted_size > 0 );
    
    item = cache_find( adjusted_size );

    valid_address( (void *)item);
    valid_address( (void *)item->root);
    valid_address( (void *) chunk );
#ifndef _IGNORE_COUNTS_
    item -> objects_in_use -= 1;
    item -> sum_objects_in_use += item -> objects_in_use;
    item -> frees += 1;
    update_sums(item);
#endif
    cache_enq(item, chunk);
  }
}

static sanity_check(mchunkptr p, int size)
{
#if _DEBUG_
  /* a quick sanity check */
  unsigned int sz = p -> size & ~0x1;
  if ( size > 0 ) {
    if (sz != size || sz != *((int*)((char*)(p) + sz - SIZE_SZ)))
      malloc_user_error();
  } else {
    if (sz != *((int*)((char*)(p) + sz - SIZE_SZ)))
      malloc_user_error();
  }
#endif
}


void
check_user(void *p, int s)
{
  MallocChunk *chunk = external_to_malloc(p);
  unsigned foo = malloc_size(chunk);

  assert( size_malloc_to_internal(foo) >= size_malloc_to_internal(s) );

  assert( (foo - (2 * SIZE_SZ)) >= s );

  sanity_check( (mchunkptr)chunk, 0 );
}
 
