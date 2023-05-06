/*
|| iisc.h - Apple LaserWriter IISC local definitions (dslib version)
||
| Copyright 1988, 1989, by
||   Gene Dronek (Vulcan Laboratory) and
||   Rich Morin  (Canta Forda Computer Laboratory).
|| Freely redistributable as long as this notice is preserved.
*/
#ident "iisc.h: $Revision: 1.1 $"

#define DATA_MAX	(8000)

#define IISC_HMIN	(2)
#define IISC_HMAX	(3150)
#define IISC_WMIN	(2)
#define IISC_WMAX	(2400)

uchar_t dsr_data[DATA_MAX];

struct inqu_ret {
  uchar_t
    pdt,		/* periph. dev. type.	*/
    tap,		/* type of Apple prin.	*/
    z2,			/* always zero		*/
    z3,			/* always zero		*/
    add_len,		/* additional length	*/
    t_r_p,		/* toner, RAM, paper	*/
    z6,			/* always zero		*/
    z7,			/* always zero		*/
    id_str[28],		/* ID string		*/
    other[8];		/* other parameters	*/
} *inqu_retp = (struct inqu_ret *) dsr_data;


/*
|| Specialized SCSI CCS Command functions
*/

/*
|| format04 - issue group 0 "Format Unit" command (0x04)
||
|| type==0		for LW, this is the position of the image
|| type==2		for LW, this is the size of the image being sent
*/

format04(dsp, type, p1, p2)
  struct dsreq *dsp;
  uchar_t type;
  long p1, p2;
{
  uchar_t data[4];

  fillg0cmd(dsp, CMDBUF(dsp), G0_FORM, type, 0, 0, 4, 0);
  filldsreq(dsp, data, sizeof data, DSRQ_WRITE);
  data[0] = get_b1(p1); data[1] = get_b0(p1);
  data[2] = get_b1(p2); data[3] = get_b0(p2);
  return(doscsireq(getfd(dsp), dsp));
}


/*
|| drawbits05 - issue group 0 "Draw Bits" command (0x05)
||
|| tlx/y	top left x/y
|| brx/y	bottom right x/y
|| mode		transfer mode
*/

drawbits05(dsp, data, datalen, tlx, tly, brx, bry, mode)
  struct dsreq *dsp;
  caddr_t data;
  long datalen, tlx, tly, brx, bry, mode;
{
  int ret, flags;
  flags = DSRQ_BUF & devflags;
  fillg0cmd(dsp, CMDBUF(dsp), 0x05, 0, 0, 0, 10, 0);
  filldsreq(dsp, data, datalen, flags | DSRQ_WRITE);
  data[0] = get_b1(tlx);  data[1] = get_b0(tlx);
  data[2] = get_b1(tly);  data[3] = get_b0(tly);
  data[4] = get_b1(brx);  data[5] = get_b0(brx);
  data[6] = get_b1(bry);  data[7] = get_b0(bry);
  data[8] = get_b1(mode); data[9] = get_b0(mode);
  ret = doscsireq(getfd(dsp), dsp);
  return ret;
}


/*
|| clearbits06 - issue group 0 "Clear Bits" command (0x06)
||
|| tlx/y	top left x/y
|| brx/y	bottom right x/y
*/

clearbits06(dsp, tlx, tly, brx, bry)
  struct dsreq *dsp;
  long tlx, tly, brx, bry;
{
  uchar_t data[8];
  int flags;
  flags = DSRQ_BUF & devflags;

  fillg0cmd(dsp, CMDBUF(dsp), 0x06, 0, 0, 0, 8, 0);
  filldsreq(dsp, data, sizeof data, flags | DSRQ_WRITE);
  data[0] = get_b1(tlx); data[1] = get_b0(tlx);
  data[2] = get_b1(tly); data[3] = get_b0(tly);
  data[4] = get_b1(brx); data[5] = get_b0(brx);
  data[6] = get_b1(bry); data[7] = get_b0(bry);
  return(doscsireq(getfd(dsp), dsp));
}


/*
|| print0a - issue group 0 "Print" command (0x0a)
||
|| tlx/y	top left x/y
|| brx/y	bottom right x/y
||
|| flags	printing mode (0x80 if from RAM)
||		continue bit  (0x40 to do cont. mode)
||		clear buffer  (0x20 to clear buffer)
*/

print0a(dsp, flags)
  struct dsreq *dsp;
  uchar_t flags;
{
  fillg0cmd(dsp, CMDBUF(dsp), G0_PRIN, 0, 0, 0, 0, flags);
  filldsreq(dsp, 0, 0, DSRQ_WRITE);
  return(doscsireq(getfd(dsp), dsp));
}
