/*
 * This is executed when we attempt to free an sbrk'd item and it's not a
 * size that matches a free list. We hang it on the free list which is
 * the best size match.
 */
static FreeRetType  __sbrk_free(MallocPtrType  mem,
				       MallocArgType size)
{
  int best_match = -1;
  int best_delta = size;
  int i;

  for (i = 0; i < __customalloc_SizeClasses; i++) {
    if ( __customalloc_ObjectSize[i] <= size ) {
      int delta = size - __customalloc_ObjectSize[i];
      if ( delta < best_delta ) {
	best_match = i;
	best_delta = delta;
      }
    }
  }
  if ( best_match >= 0 ) {
    __customalloc_link(&(__customalloc_FreeList[best_match]), mem);
  }
}
