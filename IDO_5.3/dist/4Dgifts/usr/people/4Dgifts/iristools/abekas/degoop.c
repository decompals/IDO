/* degoop.c       1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* The following program takes a YUV field in Abekas tape   */
/* (SCSI) format and strips line numbers and syncs for a    */
/* clean YUV field.                                         */
/*                                                          */
/* Usage degoop <field> infile outfile                      */
/* field is the intended dest field on the A60              */
/* eg field 100 is a f1 (as an rcp file spec f50.yuv)       */
/*    field 101 is a f2 (as an rcp file spec f50+.yuv)      */
/*                                                          */
/*           cc -g degoop.c -lds -o degoop                  */

#include <stdio.h>

#define LINELEN 1440
#define FIELDLINES 243
#define OFFSET 0x2D92

main(argc, argv)
int argc;
char *argv[];
{
FILE *infile, *outfile;
int tmp, i;
char line[LINELEN];

if(argc != 3)
    {
    printf("Usage: degoop infile outfile\n");
    exit();
    }
if(!(infile= fopen(argv[1], "r")))
    {
    printf("Unable to open file %s\n", argv[1]);
    exit();
    }
if(!(outfile= fopen(argv[2], "w")))
    {
    printf("Unable to open file %s\n", argv[2]);
    exit();
    }

fseek(infile, OFFSET, 0);		/* skip the blanked lines */
for(i=0;i<FIELDLINES;i++)
    {
    fread(line, 1, LINELEN, infile);
    fwrite(line, 1, LINELEN, outfile);
    fseek(infile, 16, 1);		/* skip the line # and syncs */
    }
}
