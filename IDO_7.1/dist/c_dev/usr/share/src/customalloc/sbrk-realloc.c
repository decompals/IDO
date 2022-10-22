static MallocPtrType __sbrk_realloc(MallocPtrType mem,
					     MallocArgType bytes)
{
  if ( mem == 0 ) {
    /*
     * Some people do the weirdest things.
     */
    return( (MallocPtrType) malloc(bytes) );
  } else {
    /*
     * We know p is a pointer to an sbrk'd region at this point
     */
    int new_user_size = ((bytes + 0x3) & (~0x3));
    int new_rounded_size = new_user_size + (2 * SIZE_SZ);
    
    SizeType *ptr = (SizeType *) mem;
    char *base = (char *) &ptr[-1];
    int old_rounded_size = ptr[-1] & ~0x3;
    int old_user_size = old_rounded_size - (2 * SIZE_SZ);
    
    /*
     *  OK to simply extend?
     */
    if ( (base + old_rounded_size) == sbrk_ptr
	&& ((base + new_rounded_size) < sbrk_end ) ) {
      sbrk_ptr = base + new_rounded_size;
      ptr[-1] = ((SizeType) new_rounded_size) | 0x3;
      return( (MallocPtrType) ptr );
    } else {
      int copy_size = (old_user_size < new_user_size )
	? old_user_size  : new_user_size;
      
      MallocPtrType newmem = (MallocPtrType) malloc(new_user_size);
      bcopy(mem, newmem, copy_size);
      free( mem );
      return( (MallocPtrType) newmem );
    }
  }
}
