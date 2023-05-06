/* Date: 3/15/91
*/

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <math.h>
#include <string.h>
#include <sys/time.h>
#include <poll.h>

#define MAXCOLORS   27
#define OPTIONS     3
#define BATCH       64
#define SLEEP       50

static char *option[] =
{
     "-help         -h     print out this message\n",
     "-sleep num           suspend drawing a line for number of milliseconds\n                     The default sleeptime is 50 ms.\n",
     "-batch num           set number of things per batch\n                     The default batch is 64.\n"
};

int          batchcount;
unsigned int sleeptime;
