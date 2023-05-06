
/* status.c       1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* This program demonstrates how to read the A60 status by  */
/* reading a single block from block 0 through via SCSI.    */
/*                                                          */
/*           cc -g status.c -lds -o status                  */

#include <fcntl.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>

# include <dslib.h>

#define BLOCKSIZE 512

extern int dsreqflags;
extern int dsdebug;

int dsblksize;

struct a60_status
    {
    short int position;
    short int speed;
    unsigned short int llcstat;
    unsigned short int seteng;
    unsigned short int fs_setup;
    unsigned short int modes;
    unsigned short int more_modes;
    unsigned short int spare;
    };


myread08(dsp, data, datalen, lba, vu)
struct dsreq *dsp;
caddr_t data;
long datalen, lba;
char vu;
{
fillg0cmd(dsp, CMDBUF(dsp), G0_READ, B3(lba), B1(datalen), B1(vu<<6));
filldsreq(dsp, data, datalen*dsblksize, DSRQ_READ);
return(doscsireq(getfd(dsp), dsp));
}


main(argc, argv)
int argc;
char *argv[];
{
char *devpath = "/dev/scsi/sc0d5l0";       /* name of device to open */
FILE *myfile;
int i, j, res;
struct dsreq *dsp;
short int data[BLOCKSIZE/2], *ptr;
struct a60_status *status;


dsreqflags = DSRQ_SENSE | DSRQ_DISC;
dsdebug = 0;
dsblksize = BLOCKSIZE;

if ((dsp = dsopen(devpath,O_RDONLY)) == NULL)
    {
    printf("unable to open device\n");
    exit();
    }

if(res = myread08(dsp, data, 1, 0, 0))
    {
    printf("error from read\n");
    if (testunitready00(dsp) != 0)
	printf("device not ready\n");
    exit();
    }

/*
ptr = &data[64];
for(i=0; i<8; i++)
    printf("%04X ", *ptr++);
printf("\n");
*/

status = (struct a60_status *) &data[64];
printf("position  %d%c\n", status->position>>1, status->position&1 ? '+' : ' ');
if(!(0x20 & status->modes)) printf("stopped\n");
else printf("speed     %d\n", status->speed);
printf("disp mode ");
if(8 & status->seteng) 
    {
    printf("field ");
    if(0x20 & status->fs_setup) printf("interpolator on\n");
    else printf("\n");
    }
else if(0x10 & status->seteng) printf("frame\n");
else if(0x20 & status->seteng) printf("auto frame\n");
printf("source    ");
if(2 & status->fs_setup) 
    {
    printf("frozen ");
    if(4 & status->fs_setup) printf("(field 1)\n");
    else if(8 & status->fs_setup) printf("(field 2)\n");
    else if(0x10 & status->fs_setup) printf("(frame)\n");
    }
else if(1 & status->fs_setup) 
    printf("input %c\n", (0x80 & status->seteng) ? '2' : '1');
else printf("offdisk\n");
if(1 & status->modes) printf("segment play\n");
}

