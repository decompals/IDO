/*

	LEB128.H

*/
 

void *uncompress_u4(char *, __uint32_t * const );
void *uncompress_u8(char *, __uint64_t * const );
void *uncompress_s4(char *, __int32_t * const );
void *uncompress_s8(char *, __int64_t * const );

__uint32_t compress_u8(char * , __uint64_t );
__uint32_t compress_s8(char * , __int64_t );
