
/* scread.c         1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* This program reads one field at a time from an A60 on    */
/* the SCSI port.  The syncs and line numbers included in   */
/* the SCSI transfer must be stripped separately.  Degoop   */
/* may be used for this function.                           */
/*                                                          */
/*           cc -g scread.c -lds -o scread                      */

#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>

# include <dslib.h>

#ifdef LINES_525
#define FIELDLEN 0x5D000
#define TMO 400
#else
#define FIELDLEN 0x6E000
#define TMO 800
#endif
#define BLOCKSIZE 512


extern int dsreqflags;
extern int dsdebug;

int dsblksize;

myreadextended28(dsp, data, datalen, lba, vu)
struct dsreq *dsp;
caddr_t data;
long datalen, lba;
char vu;
{
fillg1cmd(dsp, CMDBUF(dsp), G1_READ, 0, B4(lba), 0, B2(datalen), B1(vu<<6));
filldsreq(dsp, data, datalen*dsblksize, DSRQ_READ /* |DSRQ_CTRL2 */ );
dsp->ds_time = TMO;   /* often takes a while */
return(doscsireq(getfd(dsp), dsp));
}


main(argc, argv)
int argc;
char *argv[];
{
char *devpath = "/dev/scsi/sc0d5l0";       /* name of device to open */
FILE *myfile;
int i, chunk, fieldnum, block, res;
struct dsreq *dsp;
char field[FIELDLEN], *ptr;

if(argc != 3)
    {
    printf("Usage scread <field> file\n");
    exit();
    }

fieldnum = atoi(argv[1]);
if(!(myfile = fopen(argv[2], "w")))
    {
    printf("Unable to open file %s\n", argv[2]);
    exit();
    }


dsreqflags = DSRQ_SENSE | DSRQ_DISC;
dsdebug = 0;
dsblksize = BLOCKSIZE;

if ((dsp = dsopen(devpath,O_RDONLY)) == NULL)
	{
	printf("unable to open device\n");
	exit();
	}

/* clear check cond.    */
if (testunitready00(dsp) != 0)
    {
    printf("device not ready\n");
    exit();
    }


block = fieldnum * (FIELDLEN/BLOCKSIZE);
chunk = (FIELDLEN/BLOCKSIZE)/8;
ptr = field;
for(i=0;i<8;i++,ptr+=(chunk*BLOCKSIZE),block+=chunk)
    if(res = myreadextended28(dsp, ptr, chunk, block, 0)) break;

if(res)
    {
    printf("error from scread %d\n", i);
    if (testunitready00(dsp) != 0)
	printf("device not ready\n");
    exit();
    }

fwrite(field, 1, FIELDLEN, myfile);
}

