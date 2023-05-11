/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
#ifdef REV_INFO
#ifndef lint
static char rcsid[] = "$RCSfile: file.c,v $ $Revision: 1.1 $ $Date: 1992/05/08 18:13:52 $"
#endif
#endif

/************************************************************
 *     file.c -- Code for dealing with files and filenames
 *
 *  Contains code to read, write, copy, move & remove files
 *     
 ************************************************************/

#include <limits.h>
#include <stdio.h>

#include "basic.h"

/************************************************************
 * Remove File
 ************************************************************/

FileRemove( filnam )
    char *filnam;
{
    return -1;
}

/************************************************************
 * Save Text to File
 ************************************************************/

void FileSaveText( fil, textchars )
    FILE *fil;
    char *textchars;
{
    rewind( fil );
    fprintf( fil, "%s", textchars );
    fflush( fil );
}

/************************************************************
 * Read Text from File
 ************************************************************/

char *FileGetText( fil )
    FILE *fil;
{
    char *textchars;
    int position = 0;
    int num;

    textchars = BasicMalloc( BUFSIZ );
    rewind( fil );
    while ( (num = read( fileno(fil),
                         textchars+position, BUFSIZ)) > 0 ) {
      position += num;
      textchars = BasicRealloc( textchars, position+BUFSIZ );
    }
    *( textchars+position ) = 0;
    return textchars;
}

/************************************************************
 * Return Trailing part of current filename
 ************************************************************/

char *FileTrailingPart( filnam )
    char *filnam;
{
    char *trailnam;
    while (*filnam != '\0') filnam++;
    while (*filnam != '/') filnam--;
    filnam++;
    strdup( trailnam, filnam );
    return trailnam;
}
