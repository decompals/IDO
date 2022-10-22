/*
 * Like cachemalloc-parts.c, but we use sbrk_malloc rather than
 * internal_malloc and we never release cache slots
 */

typedef struct CacheItemStruct CacheItem;

struct CacheItemStruct {
  int size;
  /* Pointer to linked list of cached items */
  MallocChunk *root;

  /* Pointer to linked list of cache records */
  CacheItem *next;
};

static CacheItem *Root = 0;

static CacheItem *cache_new(int size, CacheItem *next)
{
  CacheItem *item = (CacheItem *) __sbrk_malloc(sizeof(CacheItem));
  item -> size = size;
  item -> root = 0;
  item -> next = next;
  return( item );
}

static void cache_enq(CacheItem *item, MallocChunk *store)
{
  store -> fd = item -> root;
  item -> root = store;
}

static MallocChunk* cache_deq(CacheItem *item)
{
  MallocChunk *store;
  store = item -> root;
  item -> root = store -> fd;
  return( store );
}


static CacheItem *cache_find(int size)
{
  CacheItem *prior = 0;
  CacheItem *item;

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

MallocPtrType realloc(MallocPtrType mem, MallocArgType bytes)
{
   if ( mem == 0 ) {
     return( malloc(bytes) );
   } else {
     if ( is_sbrk(mem) ) {
        return( __sbrk_realloc(mem,bytes));
     } else {
        return( __internal_realloc(mem,bytes));
     }
   }
}

MallocPtrType malloc(MallocArgType size)
{
  int adjusted_size;
  CacheItem *item;
  void *return_value;
  
  if (size == 0) {
    size = 32;
  }

  adjusted_size = size_external_to_internal( size );
  
  item = cache_find( adjusted_size );
  
  if ( item -> root ) {
    return_value = (void *) malloc_to_external( cache_deq(item) );
  } else {
    return_value = (void *) __sbrk_malloc( adjusted_size );
  }
  
  if ( return_value == 0 ) { 
    static  char *argle = "Out of memory!\n";
    write(2,argle,strlen(argle));
    abort();
  }
  return ( return_value );
}

FreeRetType free(FreePtrType p)
{

  if ( p ) {
    MallocChunk *chunk = external_to_malloc(p);
    int adjusted_size =
      size_malloc_to_internal(malloc_size(chunk) - (2 * SIZE_SZ) );
    CacheItem *item;
    item = cache_find( adjusted_size );
    cache_enq(item, chunk);
  }
}
