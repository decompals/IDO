/* scwrite.c      1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* This program writes one field at a time to a specified   */
/* location on the A60 via the SCSI port.  The syncs and    */
/* line numbers needed for SCSI transfer must be added      */
/* separately.  Engoop may be used for this function.       */
/*                                                          */
/*           cc -g scwrite.c -L../lib -lds -o scwrite       */

#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>

# include <dslib.h>

#ifdef LINES_525
#define FIELDLEN 0x5D000
#define TMO 500
#else
#define FIELDLEN 0x6E000
#define TMO 800
#endif
#define BLOCKSIZE 512


extern int dsreqflags;
extern int dsdebug;

int dsblksize;

/*
|| writeextended2a - issue group 1 "Write Extended" command (0x2a)
   - different from dslib because their routine can't handle blocks properly
*/

mywriteextended2a(dsp, data, datalen, lba, vu)
  struct dsreq *dsp;
  caddr_t data;
  long datalen, lba;
  char vu;
{
  fillg1cmd(dsp, CMDBUF(dsp), G1_WRIT, 0, B4(lba), 0, B2(datalen), B1(vu<<6));
  filldsreq(dsp, data, datalen*dsblksize, DSRQ_WRITE /* |DSRQ_CTRL2 */ );
  dsp->ds_time = TMO;	/* longer timeout for A60 */
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
    printf("Usage a60write file <field>\n");
    exit(1);
    }

if(!(myfile = fopen(argv[1], "r")))
    {
    printf("Unable to open file %s\n", argv[1]);
    exit(1);
    }
fieldnum = atoi(argv[2]);



dsreqflags = DSRQ_SENSE | DSRQ_DISC;
dsdebug = 0;
dsblksize = BLOCKSIZE;

if ((dsp = dsopen(devpath,O_RDONLY)) == NULL)
	{
	printf("unable to open device\n");
	exit(1);
	}

/* clear check cond.    */
if (testunitready00(dsp) != 0)
    {
    printf("device not ready\n");
    exit(1);
    }

fread(field, 1, FIELDLEN, myfile); /* read 'engooped' field */

block = fieldnum * (FIELDLEN/BLOCKSIZE);
chunk = (FIELDLEN/BLOCKSIZE)/8;
ptr = field;
for(i=0;i<8;i++,ptr+=(chunk*BLOCKSIZE),block+=chunk)
    if(res = mywriteextended2a(dsp, ptr, chunk, block, 0)) break;

if(res)
    {
    printf("error from scwrite %d\n", i);
    if (testunitready00(dsp) != 0)
	printf("device not ready\n");
    exit(1);
    }

}
