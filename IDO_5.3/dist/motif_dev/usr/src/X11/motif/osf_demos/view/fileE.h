/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
/*   $RCSfile: fileE.h,v $ $Revision: 1.1 $ $Date: 1992/05/08 18:13:57 $ */


#if ( defined file_h )
#define extern 
#endif

#ifdef _NO_PROTO
FILE * OpenFile();
void CloseFile();
char * ReadFile();
#else

FILE * OpenFile(char * filename);

void CloseFile(FILE * file);

char * ReadFile(FILE * file, int *filesize);

#endif

#if ( defined extern )
#undef extern 
#endif