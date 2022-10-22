/* -*-c-*- */
#ifndef _Database_h_
#define _Database_h_

#ifndef _UserMalloc_pre_h_
#  include "UserMalloc-pre.h"
#endif

/*
 * Data structures for CustoMalloc caching
 */

typedef struct CacheItemStruct CacheItem;

struct CacheItemStruct {
  int size;
  
  int runs;

  double last_event;
  double mallocs;
  double frees;

  double objects_in_use;
  double max_objects_in_use;
  /*
   * Sum of space used over all allocations/deallocations (across all runs)
   */
  double sum_objects_in_use;
  double sum_objects_in_free_list;

  double objects_in_free_list;
  double max_objects_in_free_list;

  /* Pointer to linked list of cached items */
  MallocChunk *root;

  /* Pointer to linked list of cache records */
  CacheItem *next;
};

extern CacheItem *customalloc_read_database();
extern void customalloc_write_database();

#endif
