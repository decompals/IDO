/* -*-c-*- */
#ifndef _UserMalloc_pre_h_
#define _UserMalloc_pre_h_

/*****************************************************************************/
/**  Using Gnu G++ Malloc                                                    */
/*****************************************************************************/

#ifndef MallocArgType
#  ifdef __cplusplus
#     define MallocArgType int
#     define MallocPtrType void *
#     define FreePtrType void *
#     define FreeRetType void
#  else
#     define MallocArgType unsigned int
#     define MallocPtrType void *
#     define FreePtrType void *
#     define FreeRetType void
#  endif
#endif

extern MallocPtrType __internal_malloc(MallocArgType);

typedef unsigned int SizeType;

#define MallocChunk struct __CustomAllocMallocChunkStruct
struct __CustomAllocMallocChunkStruct
{
  SizeType       size;
  MallocChunk *fd;
};

#endif
