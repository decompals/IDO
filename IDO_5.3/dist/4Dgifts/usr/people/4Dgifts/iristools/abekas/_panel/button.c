/*
 ****************************************************************
 * VERSION 2.0							*
 * September 1990						*
 ****************************************************************
 *
 * button.c
 * Copyright 1990 Rhonda Graphics, Inc.
 *		  2235 W. Alice
 *		  Phoenix, AZ 85021
 *
 * This module contains the functions to make the 
 * groovy "3D buttons" that make the Control Panel
 * so much fun to use.  The function add_button creates
 * a new button with the location, type of button, and
 * a function to be called whenever the button is pressed.
 *
 */

#include <stdio.h>
#include <gl.h>
#include <device.h>
#include <fmclient.h>
#include "a60.h"


/****************************************************************
 * local definitions						*
 ****************************************************************/

#define MAX_BUTTONS	100
#define BUTTONSIZE	75
#define BEVELSIZE	6
#define XSIZE		150
#define YSIZE		155

#define LCD_X		540
#define LCD_Y		355
#define REGS_X		20
#define REGS_Y		375

typedef struct {
	int	minx,maxx;	/* location of the button */
	int	miny,maxy;
	char	button;		/* type of button */
	char	status;		/* current status */
	char	*label;		/* line 1 of label */
	char	*label2;	/* line 2 of label */
	int	(*func)();	/* function to call when pressed */
	} BUTTON;



/****************************************************************
 * private variables						*
 ****************************************************************/

int			num_buttons;
BUTTON			list[MAX_BUTTONS];
int			reg100,reg10,reg1;
int			reg_100,reg_10,reg_1000;
int			dot;
int			sign;
int 			inreg,outreg;
int			playmode;



/****************************************************************
 * external variables						*
 ****************************************************************/

extern fmfonthandle 	times,times12,times32;
extern fmfonthandle 	helv,helv14;
extern long 		xorig,yorig;



/****************************************************************
 * init_buttons - initialize the buttons 			*
 ****************************************************************/

init_buttons()
{
  playmode = NORM;
  num_buttons = 0;
  reg100   = 0;
  reg10    = 0;
  reg1     = 0;
  reg_1000 = 0;
  reg_100  = 0;
  reg_10   = 0;
  dot	   = 0;
  sign	   = 0;
  inreg    = 0;
  outreg   = 0;
}



/****************************************************************
 * add_button - create a new button				*
 *	minx,miny - the location of the button on the panel	*
 *	button - the type of button (does it have an LED?)	*
 *	status - status of button (active)			*
 *	label,label2 - label for the button (2 lines)		*
 *	func - a function to call when the button is pushed	*
 ****************************************************************/

add_button(minx,miny,button,status,label,label2,func)
  int 		minx,miny;
  char 		button,status,*label,*label2;
  int 		(*func)();
{
  if (num_buttons == MAX_BUTTONS)
	return(0);
  else
	{
	list[num_buttons].minx = minx;
	list[num_buttons].miny = miny;
	list[num_buttons].maxx = minx + BUTTONSIZE;
	list[num_buttons].maxy = miny + BUTTONSIZE;
	list[num_buttons].button = button;
	list[num_buttons].status = status;
	list[num_buttons].label = label;
	list[num_buttons].label2 = label2;
	list[num_buttons].func = func;
	num_buttons++;
	return(1);
	}
}



query_buttons(x,y)
int x,y;
{
  int i;

  i = 0;
  while (i < num_buttons)
	{
	if ((y <= list[i].maxy) && (y >= list[i].miny))
		{
		if ((x <= list[i].maxx) && (x >= list[i].minx))
			{
			(*list[i].func)();
			return(1);
			}
		}
	i++;
	}
  return(0);
}



toggle_button(i)
int i;
{
  if (list[i].status == 0)
	list[i].status = 1;
  else
	list[i].status = 0;
}



draw_panel()
{
  int i;

  color(COLOR0);
  clear();
  for (i = 0; i < num_buttons; i++)
	draw_button(i);
  draw_lcd();
  draw_regs();
  draw_mousepad();
}



draw_button(i)
int i;
{
  short a[2],b[2],c[2],d[2];
  int x,y;
  int xlabel,ylabel;

  x = list[i].minx;
  y = list[i].miny;
  color(COLOR3);
  rectfi(x+BEVELSIZE,y+BEVELSIZE,x+BUTTONSIZE-BEVELSIZE,y+BUTTONSIZE-BEVELSIZE);

  color(COLOR1);
  a[0] = x;  a[1] = y+BUTTONSIZE;
  b[0] = x+BUTTONSIZE; b[1] = y+BUTTONSIZE;
  c[0] = x+BUTTONSIZE-BEVELSIZE; c[1] = y+BUTTONSIZE-BEVELSIZE;
  d[0] = x+BEVELSIZE; d[1] = y+BUTTONSIZE-BEVELSIZE;
  bgnpolygon();
  v2s(a); v2s(b); v2s(c); v2s(d);
  endpolygon();

  color(COLOR5);
  a[0] = x;  a[1] = y;
  b[0] = x+BUTTONSIZE; b[1] = y;
  c[0] = x+BUTTONSIZE-BEVELSIZE; c[1] = y+BEVELSIZE;
  d[0] = x+BEVELSIZE; d[1] = y+BEVELSIZE;
  bgnpolygon();
  v2s(a); v2s(b); v2s(c); v2s(d);
  endpolygon();

  color(COLOR2);
  a[0] = x+BUTTONSIZE;  a[1] = y;
  b[0] = x+BUTTONSIZE; b[1] = y+BUTTONSIZE;
  c[0] = x+BUTTONSIZE-BEVELSIZE; c[1] = y+BUTTONSIZE-BEVELSIZE;
  d[0] = x+BUTTONSIZE-BEVELSIZE; d[1] = y+BEVELSIZE;
  bgnpolygon();
  v2s(a); v2s(b); v2s(c); v2s(d);
  endpolygon();

  color(COLOR4);
  a[0] = x;  a[1] = y;
  b[0] = x; b[1] = y+BUTTONSIZE;
  c[0] = x+BEVELSIZE; c[1] = y+BUTTONSIZE-BEVELSIZE;
  d[0] = x+BEVELSIZE; d[1] = y+BEVELSIZE;
  bgnpolygon();
  v2s(a); v2s(b); v2s(c); v2s(d);
  endpolygon();

  fmsetfont(helv14);
  color(BLACK);
  xlabel = (BUTTONSIZE / 2) - ((fmgetstrwidth(helv14,list[i].label) / 2));
  ylabel = (BUTTONSIZE / 2) - 6;

  if (list[i].label2 == NULL)
	{
  	cmov2i(x+xlabel,y+ylabel);
  	fmprstr(list[i].label);
	}
  else
	{
	cmov2i(x+xlabel,y+(ylabel+9));
	fmprstr(list[i].label);
	xlabel = (BUTTONSIZE / 2) - ((fmgetstrwidth(helv14,list[i].label2) / 2));
	cmov2i(x+xlabel,y+(ylabel-9));
	fmprstr(list[i].label2);
	}

  if ((list[i].button == 1) && (list[i].status == 1))
	{
	xlabel = x + (BUTTONSIZE / 2);
  	color(RED);
  	rectfi(xlabel-15,y+10,xlabel+18,y+15);
	}
}




draw_lcd()
{
  char lcd[10];

  color(COLOR0);
  rectfi(LCD_X-10,LCD_Y-10,LCD_X+220,LCD_Y+40);
  fmsetfont(times32);
  color(WHITE);
  cmov2i(LCD_X,LCD_Y);
  if ((dot) && (sign))
	sprintf(lcd," -%d%d%d.%d%d%d ",reg100,reg10,reg1,reg_10,reg_100,reg_1000);
  else if (dot)
	sprintf(lcd,"  %d%d%d.%d%d%d ",reg100,reg10,reg1,reg_10,reg_100,reg_1000);
  else if (sign)
	sprintf(lcd," -%d%d%d ",reg100,reg10,reg1);
  else
	sprintf(lcd,"  %d%d%d ",reg100,reg10,reg1);
  fmprstr(lcd);
}


draw_regs()
{
  char str[40];

  color(COLOR0);
  rectfi(REGS_X-5,REGS_Y-5,REGS_X+220,REGS_Y+40);
  fmsetfont(helv14);
  color(WHITE);
  cmov2i(REGS_X,REGS_Y);
  sprintf(str," IN: %3d   OUT: %3d",inreg,outreg);
  fmprstr(str);
}


draw_mousepad()
{
  short a[2],b[2],c[2],d[2];
  int x,y;
  int xlabel,ylabel;

/****************************************************************/
  x = 795;
  y = 20;
  color(COLOR7);
  rectfi(x+BEVELSIZE,y+BEVELSIZE,x+XSIZE-BEVELSIZE,y+YSIZE-BEVELSIZE);

  color(COLOR1);
  a[0] = x;  a[1] = y+YSIZE;
  b[0] = x+XSIZE; b[1] = y+YSIZE;
  c[0] = x+XSIZE-BEVELSIZE; c[1] = y+YSIZE-BEVELSIZE;
  d[0] = x+BEVELSIZE; d[1] = y+YSIZE-BEVELSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(COLOR5);
  a[0] = x;  a[1] = y;
  b[0] = x+XSIZE; b[1] = y;
  c[0] = x+XSIZE-BEVELSIZE; c[1] = y+BEVELSIZE;
  d[0] = x+BEVELSIZE; d[1] = y+BEVELSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(COLOR2);
  a[0] = x+XSIZE;  a[1] = y;
  b[0] = x+XSIZE; b[1] = y+YSIZE;
  c[0] = x+XSIZE-BEVELSIZE; c[1] = y+YSIZE-BEVELSIZE;
  d[0] = x+XSIZE-BEVELSIZE; d[1] = y+BEVELSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(COLOR4);
  a[0] = x;  a[1] = y;
  b[0] = x; b[1] = y+YSIZE;
  c[0] = x+BEVELSIZE; c[1] = y+YSIZE-BEVELSIZE;
  d[0] = x+BEVELSIZE; d[1] = y+BEVELSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  fmsetfont(helv14);
  color(BLACK);
  xlabel = (XSIZE / 2) - ((fmgetstrwidth(helv14,"JOG") / 2));
  ylabel = 110;
  cmov2i(x+xlabel,y+ylabel);
  fmprstr("JOG");
/****************************************************************/
  y = 180;
  color(COLOR7);
  rectfi(x+BEVELSIZE,y+BEVELSIZE,x+XSIZE-BEVELSIZE,y+YSIZE-BEVELSIZE);

  color(COLOR1);
  a[0] = x;  a[1] = y+YSIZE;
  b[0] = x+XSIZE; b[1] = y+YSIZE;
  c[0] = x+XSIZE-BEVELSIZE; c[1] = y+YSIZE-BEVELSIZE;
  d[0] = x+BEVELSIZE; d[1] = y+YSIZE-BEVELSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(COLOR5);
  a[0] = x;  a[1] = y;
  b[0] = x+XSIZE; b[1] = y;
  c[0] = x+XSIZE-BEVELSIZE; c[1] = y+BEVELSIZE;
  d[0] = x+BEVELSIZE; d[1] = y+BEVELSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(COLOR2);
  a[0] = x+XSIZE;  a[1] = y;
  b[0] = x+XSIZE; b[1] = y+YSIZE;
  c[0] = x+XSIZE-BEVELSIZE; c[1] = y+YSIZE-BEVELSIZE;
  d[0] = x+XSIZE-BEVELSIZE; d[1] = y+BEVELSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(COLOR4);
  a[0] = x;  a[1] = y;
  b[0] = x; b[1] = y+YSIZE;
  c[0] = x+BEVELSIZE; c[1] = y+YSIZE-BEVELSIZE;
  d[0] = x+BEVELSIZE; d[1] = y+BEVELSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  fmsetfont(helv14);
  color(BLACK);
  xlabel = (XSIZE / 2) - ((fmgetstrwidth(helv14,"PLAY") / 2));
  ylabel = 110;
  cmov2i(x+xlabel,y+ylabel);
  fmprstr("PLAY");
}



check_mousepad(x,y)
int x,y;
{
  if ((y <= 335) && (y >= 180))
	{
	if ((x <= 945) && (x >= 795))
		return(1);
	}
  else if ((y <= 175) && (y >= 20))
	{
	if ((x <= 945) && (x >= 795))
		return(2);
	}
  return(0);
}



float get_register()
{
  float reg;

  reg = reg100 * 100 + reg10 * 10 + reg1 + 
	reg_10 * .1 + reg_100 * .01 + reg_1000 * .001;

  if (sign)
	reg = -reg;

  return(reg);
}


get_inreg()
{
  return(inreg);
}


get_outreg()
{
  return(outreg);
}


clear_register()
{
  reg100 = 0;
  reg10 = 0;
  reg1 = 0;
  reg_1000 = 0;
  reg_100 = 0;
  reg_10 = 0;
  dot = 0;
  sign = 0;
}


set_dot()
{
  if (dot == 0)
	dot = 1;
}

toggle_sign()
{
  if (sign == 0)
	sign = 1;
  else
	sign = 0;
}


add_register(num)
int num;
{
  if (dot)
	{
	switch (dot)
		{
		case 1:
			reg_10 = num;
			break;
		case 2:
			reg_100 = num;
			break;
		case 3:
			reg_1000 = num;
			break;
		default:
			reg_1000 = num;
			break;
		}
	dot++;
	}
  else
	{
  	reg100 = reg10;
  	reg10 = reg1;
  	reg1 = num;
	}
}


set_inreg()
{
  inreg = get_register();
}


set_outreg()
{
  outreg = get_register();
}


get_status(i)
int i;
{
  return(list[i].status);
}


set_mode(m)
int m;
{
  toggle_button(playmode);
  draw_button(playmode);
  playmode = m;
  toggle_button(playmode);
  draw_button(playmode);
}


get_mode()
{
  return(playmode);
}


draw_where(num)
int num;
{
  char str[10];

  color(COLOR0);
  rectfi(795,340,945,390);
  fmsetfont(times32);
  color(RED);
  cmov2i(870,355);
  sprintf(str,"%3d",num);
  fmprstr(str);

  fmsetfont(helv14);
  color(WHITE);
  cmov2i(795,364);
  sprintf(str,"FRAME:");
  fmprstr(str);
}


clear_where()
{
  color(COLOR0);
  rectfi(795,340,945,390);
}


