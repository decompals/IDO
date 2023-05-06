
/* ready.c        1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* This example determines if a SCSI device is ready.       */
/*                                                          */
/*           cc -g ready.c -lds -o ready                    */

#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
# include <dslib.h>

int dsreqflags;
int dsdebug;

main(argc, argv)
int argc;
char *argv[];
{
char *devpath = "/dev/scsi/sc0d5l0";       /* name of device to open */
struct dsreq *dsp;

dsreqflags = DSRQ_SENSE | DSRQ_DISC;
dsdebug = 9;

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
printf("device ready\n");
}
