/* scscan.c       1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* this proggy dumps Abekas SCSI format decoding syncs and line #'s */
/*                                                          */
/*           cc -g scscan.c -lds -o scscan           */

#include <stdio.h>

/* scscan
 * this proggy dumps Abekas SCSI format decoding syncs and line #'s
 */

int getsync(file)
    FILE *file;
    {
    char c, lastc;
    int ret;

    lastc = 42;
    while(!feof(file))
        {
        c = fgetc(file) & 0xFF;
        if(((c == 0) && (lastc == 0xFF))
          || ((c == 0xFF) && (lastc == 0)))
            {
            ret = (lastc << 8) | c;
            ret = ret << 8 | (fgetc(file) & 0xFF);
            ret = ret << 8 | (fgetc(file) & 0xFF);
            return ret;
            }
        lastc = c;
        }
    return 0;
    }


main(argc, argv)
    int argc;
    char *argv[];
    {
    FILE *myfile;
    char c1, c2;
    int tmp, i;

    if(argc != 2)
        {
        printf("Usage: scscan file\n");
        exit();
        }

    if(!(myfile= fopen(argv[1], "r")))
        {
        printf("Unable to open file %s\n", argv[1]);
        exit();
        }

    while(!feof(myfile))
        {
        tmp = getsync(myfile);
        if(tmp == 0xFF00009D) printf("<f1 EOL>\n");
        else if(tmp == 0xFF000080) 
            {
            printf("<f1 start> ");
            for(i=0;i<8;i++)
                {
                c1 = (0xFF & fgetc(myfile));
                c2 = (0xFF & fgetc(myfile));
                printf("%04X ", (c1 << 8) | c2); 
                }
            printf("... ");
            }
        else if(tmp == 0xFF0000DA) printf("<f2 EOL>\n");
        else if(tmp == 0xFF0000C7) 
            {
            printf("<f2 start> ");
            for(i=0;i<8;i++)
                {
                c1 = (0xFF & fgetc(myfile));
                c2 = (0xFF & fgetc(myfile));
                printf("%04X ", (c1 << 8) | c2); 
                }
            printf("... ");
            }
        else if(tmp == 0x00FFFF64) 
            {
            fgetc(myfile);
            fgetc(myfile);
            c1 = (0x3E & fgetc(myfile));
            c2 = (0x3E & fgetc(myfile));
            tmp = (c2 << 4) | (c1 >> 1);

            printf("[%7X] %3d ", ftell(myfile)+4,  tmp);
            }
        else printf("Unknown sync %08X\n", tmp);
        }
    }
