/*
 *	ical - 
 *		A very simple desk calendar.  Use the left and middle mouse
 *	mouse buttons to go forward and backward in time.
 *	
 *				Paul Haeberli - 1985
 *
 *	Now self-updating!  
 *      Enhancements by Andrew Myers and Robert Reimann  8/89
 */
#include <sys/time.h>
#include <sys/types.h>
#include <fcntl.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <fmclient.h>

#define NUMFD 10
#define HSPACE 32

struct	caltime {
    int	day;
    int	month;
    int	year;
};

void drawcal(struct caltime *, struct caltime *, fmfonthandle);
int myqread(short *, struct timeval *);
int dayofweek(int, int);
int jan1(int);

char *monthnames[]= {
    "January", "February", "March", "April",
    "May", "June", "July", "August",
    "September", "October", "November", "December",
};

char *daynames[] = {
    "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};

char monlength[] = {
    0,
    31, 29, 31, 30,
    31, 30, 31, 31,
    30, 31, 30, 31,
};

char tempstr[100];
struct tm *tm;
long thetime;
int devqueue;
fmfonthandle font1, numberfont, dayfont;

static double PtoPfactor = -1;

float pixtopoints(float npix)
{

    if(PtoPfactor < 0.0)
        PtoPfactor = (72.0*getgdesc(GD_XMMAX)/25.4)/getgdesc(GD_XPMAX);
    return npix*PtoPfactor;
}

main(int argc, char *argv[])
{
    short val;
    struct caltime today, show;
    struct timeval t;
    int oldday, oldmonth;

    thetime = time(0);
    tm = localtime(&thetime);
    today.day = tm->tm_mday;
    today.month = tm->tm_mon + 1;
    today.year = tm->tm_year + 1900;

    if (argc >= 2) {
	show.month = atoi(argv[1]);
	show.year = atoi(argv[2]);
	show.day = 1;
    } else 
	show = today;

    prefsize(260,185);
    winopen("cal");

    devqueue = qgetfd();
    t.tv_sec = 60;
    t.tv_usec = 0;

    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    grey(0.9);
    clear();
    fminit();
    if ((font1 = fmfindfont("Helvetica-BoldOblique")) == 0)
       exit (-1);
    numberfont = fmscalefont(font1, pixtopoints(14.0));
    dayfont = fmscalefont(font1, pixtopoints(12.0));
    drawcal(&show, &today, font1);
    while (1) {
	switch (myqread(&val,&t)) {
	    case ESCKEY:
		if(val)
		    exit(0);
		break;
	    case LEFTMOUSE:
		if (val) {
		    show.month--;
		    if (show.month == 0) {
			show.year--;
			show.month = 12;
		    }
		    drawcal(&show, &today, font1);
		}
		break;
	    case MIDDLEMOUSE:
		if (val) {
		    show.month++;
		    if (show.month == 13) {
			show.year++;
			show.month = 1;
		    }
		    drawcal(&show, &today, font1);
		}
		break;
	    case REDRAW:
		drawcal(&show, &today, font1);
		break;
	    case -1:		/* a timeout occurred from myqread */
		oldday = today.day;
		oldmonth = today.month;

    		thetime = time(0);
		tm = localtime(&thetime);
    		today.day = tm->tm_mday;
		today.month = tm->tm_mon + 1;
		today.year = tm->tm_year + 1900;

		show.day = today.day;

		if (oldday != today.day) {
		    if ((show.month == oldmonth) &&
                        (oldmonth != today.month) &&
  		        ((show.year == today.year) ||
 		         ((show.year == today.year - 1) &&
	                  (today.month == 1)))) {

			    show.month = today.month;
			    show.year = today.year;
		    }
    		    drawcal(&show, &today, font1);
	        }
		break;
	    case 0:
		break;		/* ignore NULLDEV's from qread */
	}
    }
}


void drawcal(struct caltime *show, struct caltime *today, fmfonthandle font1)
{
    register i;
    int c;
    int ycoord;
    int samemonth;

    reshapeviewport();
    samemonth = 0;
    if ((today->year == show->year) && (today->month == show->month))
	    samemonth = 1;
    grey(0.9);
    clear();
    grey(0.0);

    grey(0.5);
    move2i(20,163);
    draw2i(240,163);

    fmsetfont(numberfont);
    rgb(0.0,0.0,1.0);
    cmov2i(20,165);
    fmprstr(monthnames[show->month-1]);
    rgb(1.0,0.0,0.0);
    sprintf(tempstr,"%u",show->year);
    cmov2i(208,165);
    fmprstr(tempstr);

    grey(0.5);
    move2i(20,143);
    draw2i(240,143);

    grey(0.0);

    fmsetfont(dayfont);
    for (i=0; i<7; i++){
	cmov2i(24+HSPACE*i,145);
	fmprstr(daynames[i]);
    }

    fmsetfont(numberfont);
    c = dayofweek(show->month, show->year);
    ycoord = 125;
    for (i=1; i<=monlength[show->month]; i++) {
	cmov2i(24+HSPACE*c,ycoord);
	sprintf(tempstr,"%2d",i); 
	if ((i == show->day) && samemonth)
	    rgb(1.0,0.0,0.0);
	fmprstr(tempstr);
	if ((i == show->day) && samemonth)
	    grey(0.0);
	if (++c == 7) {
	    c = 0;
	    ycoord -= 20;
	}
    }
}

int dayofweek(int m, int y)
{
    register int d, i;

    d = jan1(y);
    if (((jan1(y+1)+7-d)%7) == 1)
	monlength[2] = 28;
    else
	monlength[2] = 29;
    for (i=1; i<m; i++)
	d += monlength[i];
    return d%7;
}

/*
 *	return day of the week
 *	of jan 1 of given year
 */
int jan1(int y)
{
    register int d;

    d = y+4+(y+3)/4;
    if (y > 1752) {
	d += 3;
        if (y > 1800) {
	    d -= (y-1701)/100;
	    d += (y-1601)/400;
        }
    }
    return d%7;
}

int myqread(short *d, struct timeval *t)
{
    fd_set a;
    int n;

    FD_ZERO(&a);
    FD_SET(devqueue,&a);
    n = select(NUMFD,&a,0,0,t);
    if (n == 0) 
	return -1;
    if FD_ISSET(devqueue,&a) 
	return qread(d);
    return -1;			/* can't happen */
}
