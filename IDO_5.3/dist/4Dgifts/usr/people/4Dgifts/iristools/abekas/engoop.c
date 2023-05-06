/* engoop.c       1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* This program takes a YUV file and adds the formatting    */
/* necessary for transfer to the A60 through the SCSI port. */
/* Ethernet transfers use only the active portion of the    */
/* display, SCSI format requires line syncs (like CCIR 601  */
/* digital video) and line number information to be added   */
/* to the data stream.  The program has to know whether the */
/* field is a field 1 or a field 2. The next parameter is   */
/* therefore the destination field number on the disk       */
/* (numbered from 0).  On 525 line systems field 0 on the   */
/* disk is a field 1 and contains the even lines.  For 625  */
/* it contains the odd lines.                               */
/*                                                          */
/* Usage engoop <field> infile outfile                      */
/* field is the intended dest field on the A60              */
/* eg field 100 is a f1 (as an rcp file spec f50.yuv)       */
/*    field 101 is a f2 (as an rcp file spec f50+.yuv)      */
/*                                                          */
/*           cc -g engoop.c -lds -o engoop                  */

#include <stdio.h>

#define LINELEN 1456
#define PREPAD 10

#ifdef LINES_525
#define FIELDLINES 243
#define FIELDLEN 0x5D000
#define VBLANK 9
#else
#define FIELDLINES 288
#define FIELDLEN 0x6E000
#define VBLANK 23
#endif


main(argc, argv)
int argc;
char *argv[];
{
FILE *infile, *outfile;
int tmp, i, field;
char line[LINELEN];
long *ptr;

if(argc != 4)
    {
    printf("Usage: engoop <field> infile outfile\n");
    exit();
    }

field = atoi(argv[1]) & 1;

if(!(infile= fopen(argv[2], "r")))
    {
    printf("Unable to open file %s\n", argv[2]);
    exit();
    }
if(!(outfile= fopen(argv[3], "w")))
    {
    printf("Unable to open file %s\n", argv[3]);
    exit();
    }

/* initialise the sync sequences */

line[0] = 0;
line[1] = 0xFF;
line[2] = 0xFF;
line[3] = 0x64;
line[4] = 0x80;
line[5] = 0x10;

line[8] = 0xFF;
line[9] = 0;
line[10] = 0;
if(field) line[11] = 0xC7;
else line[11] = 0x80;

/* initialise line to black */

ptr = (long *) &line[12];
for(i=12; i<1452; i+=4) *ptr++ = 0x80108010;

line[1452] = 0xFF;
line[1453] = 0;
line[1454] = 0;
if(field) line[1455] = 0xDA;
else line[1455] = 0x9D;

/* 525: if f1 (field == 0) first line = 9 else f2 : 8 */
/* 625: if f1 (field == 0) first line = 23 else f2 : 22 */

tmp = VBLANK-field; 
tmp -= PREPAD;

/* put ten blank lines up front to get things going */

for(i=0;i<PREPAD;i++,tmp+=2)
    {
    /* construct the wierd line number */
    line[6] = 0x80 | (0x3E & (tmp<<1));
    line[7] = 0x80 | (0x3E & (tmp>>4));
    fwrite(line, 1, LINELEN, outfile);
    }

for(i=0;i<FIELDLINES;i++,tmp+=2)
    {
    /* read a line from the source file */
    fread(&line[12], 1, LINELEN-16, infile);
    /* construct the wierd line number */
    line[6] = 0x80 | (0x3E & (tmp<<1));
    line[7] = 0x80 | (0x3E & (tmp>>4));
    fwrite(line, 1, LINELEN, outfile);
    }

/* finally pad to the end of the field with junk */
for(i=0;i<((FIELDLEN/LINELEN)-FIELDLINES);i++)
    fwrite(line, 1, LINELEN, outfile);
fwrite(line, 1, FIELDLEN % LINELEN, outfile);
}
