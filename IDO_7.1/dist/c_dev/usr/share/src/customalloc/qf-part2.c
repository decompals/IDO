/*
 * Quick fit
 */

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

MallocPtrType malloc(MallocArgType bytes)
{
  if ( bytes <= 32 ) {
    /*
     * Round size up by 4
     */
    int index = (bytes+3) >> 2;
    int size = index << 2;
    if (__customalloc_FreeList[index]) 
      return(__customalloc_unlink(&(__customalloc_FreeList[index])));
    else
      /*
       * create it of appropriate size to fit in this bin..
       */
      return( __sbrk_malloc(size) ); 
  } else {
    return __internal_malloc(bytes);
  }
}

FreeRetType free(FreePtrType p)
{
  if ( p ) {
     MallocChunk *chunk = external_to_malloc(p);
     int bytes = malloc_size(chunk) - (2 * SIZE_SZ);

     if ( bytes <= 32 ) {
       /*
	* Truncate size
	*/
       int index = bytes >> 2;
       __customalloc_link(&(__customalloc_FreeList[index]), p);
     } else {
       /*
	* If it's an sbrk, it must be returned to a freelist
	*/
       if (is_sbrk(p)) {
	 int index = bytes >> 2;
	 if ( index >= __customalloc_SizeClasses ) {
	   index = __customalloc_SizeClasses - 1;
	 }
	 __customalloc_link(&(__customalloc_FreeList[index]), p);
       } else {
	 __internal_free(p);
       }
     }
   }
}
