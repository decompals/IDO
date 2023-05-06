/* readlabel.c    1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* This example reads the first block of the A60.           */
/*                                                          */
/*           cc -g readlabel.c -lds -o readlabel            */
 
#include <stdio.h>
#include <fcntl.h>
#include <dslib.h>

#define BLOCKSIZE 512

char *devpath = "/dev/scsi/sc0d5l0";       /* device for a60 */
int dsreqflags;
int dsdebug;
int dsblksize;

unsigned char block[BLOCKSIZE];
int i, chunk, res;
struct dsreq *dsp;
char *ptr;

main(argc,argv)
int argc;
char **argv;
{
int i, j;
char *ptr;

if(argc!=1) 
    {
    fprintf(stderr,"usage readlabel\n");
    exit(1);
    }

dsreqflags = DSRQ_SENSE | DSRQ_DISC;
dsdebug = 0;
dsblksize = BLOCKSIZE;

if ((dsp = dsopen(devpath,O_RDONLY)) == NULL)
	{
	fprintf(stderr, "unable to open %s\n", devpath);
	exit(1);
	}

/* clear check cond.    */
if (testunitready00(dsp) != 0)
    {
    fprintf(stderr, "device not ready\n");
    exit(1);
    }

if(res = read08(dsp, block, 512, 0, 0))
    {
    fprintf(stderr, "error from read %d\n", i);
    if (testunitready00(dsp) != 0)
	fprintf(stderr, "device not ready\n");
    exit(1);
    }

ptr = block;
for(i=0; i<32; i++)
    {
    for(j=0; j<16; j++)
	printf("%02X ", 0xFF & *ptr++);
    printf("\n");
    }
}
