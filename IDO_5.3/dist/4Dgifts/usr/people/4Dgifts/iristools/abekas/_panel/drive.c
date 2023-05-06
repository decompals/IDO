/*
 ****************************************************************
 * VERSION 2.0.1						*
 * December 1990						*
 ****************************************************************
 *
 * drive.c 
 * copyright 1990 Rhonda Graphics Inc.
 *		  2235 W. Alice
 *		  Phoenix, AZ 85021
 *
 * The Abekas Control Panel is designed to emulate
 * the behavior of the Abekas Keyboard.
 *
 * Driver module for Abekas Control Panel.
 * This module contains: main, the user-interaction
 * loops and the higher level functions that send
 * specific commands to the A60.
 *
 * For more information on A60 commands and interfacing
 * using Ethernet see the Abekas A60 Ethernet Manual.
 *
 * Some known problems:
 *	Progam won't accept frame numbers larger than 999
 *	  (this is only a problem for 50 second machines)
 *
 * Version 2.0.1:
 * 	The "Black" and "Sub Black" choices were inadvertently left off
 * 	the patterns menu in version 2.0; this has been fixed.
 *	I have reorganized the menus, and created more submenus.
 *
 */
#include <stdio.h>
#include <gl.h>
#include <device.h>
#include <fmclient.h>
#include "a60.h"


#define NORM		29
#define LOOP		7
#define SEG		11
#define XWINSIZE	965
#define YWINSIZE	400
#define DI_BEVSIZE	4
#define DI_BUTSIZEX	120
#define DI_BUTSIZEY	60	


/****************************************************************
 * Prototypes							*
 ****************************************************************/

extern float get_register();
int fun_play_forwards(),fun_play_backwards();
int fun_jog_forwards(),fun_jog_backwards();
int fun_stop(),fun_goto(),fun_clear(),fun_loop();
int fun_seg(),fun_seg_play(),fun_normal(),fun_in(),fun_out();
int fun_video_in(),fun_freeze(),fun_dot(),fun_var_play();
int fun_minus(),fun_blank();
int fun_1(),fun_2(),fun_3();
int fun_4(),fun_5(),fun_6();
int fun_7(),fun_8(),fun_9();
int fun_0();


/****************************************************************
 * Private Variables						*
 ****************************************************************/

fmfonthandle 		times,times12,times32;
fmfonthandle 		helv,helv14,helv24;
long 			xorig,yorig;
long			panelid;
int			video;
int			freeze;
char			trigger[256];

int 			menu,sub1,sub2,sub3,sub4,sub5;

char *m1 = "Control Panel%t|makemap%x104|patterns%m%l|clear segments%x100|machine mode%m|input select%m|record lockout%m|keyboard%m|timecode trigger%x107|exit%x103";
char *s1 = "Record Lockout%t|protect%x200|unprotect%x201";
char *s2 = "Patterns%t|100 Combo%x12|95 Combo%x13|75 Combo%x14%l|100 Bars%x0|95 Bars%x1|75 Bars%x2%l|Black%x15|Sub Black%x16%l|Lin Ramp%x3|Mod Ramp%x4|10 Step%x5|X Hatch%x6|2T pulse%x7|Pluge%x8|Multiburst%x9|Bow Tie%x10|Digital Test%x11";
char *s3 = "601 Input Select%t|input #1%x108|input #2%x109";
char *s4 = "Machine Mode%t|frame mode%x105|field mode%x106";
char *s5 = "Keyboard%t|enable%x101|disable%x102";



/****************************************************************
 * main								*
 ****************************************************************/

main(argc,argv)
  int		argc;
  char		*argv[];
{
  strcpy(trigger,"acptrigger");
  video = 0;
  freeze = 0;
  setup_gl();
  setup_buttons();
  establish_connection();
  mainloop();
}


/****************************************************************
 * establish_connection - make an Ethernet connection		*
 ****************************************************************/

establish_connection()
{
  int flag;

  if (try_connection() == 0)
	{
	flag = 1;
	while (connect_dialog() == 1)
		{
		if (try_connection() == 1)
			{
			flag = 0;
			break;
			}
		}
	if (flag)
	  	exit(0);
	}
}

try_connection()
{
  int result;

  setHourGlass();
  result = open_connection();
  setNormalCursor();
  return result;
}


/****************************************************************
 * setup_gl - initialize windows and other SGI GL stuff		*
 ****************************************************************/

setup_gl()
{

  prefsize(XWINSIZE,YWINSIZE);
  panelid = winopen("");
  wintitle("Rhonda Graphics A60 Control Panel Version 2.0.1");
  icontitle("acp");
  getorigin(&xorig,&yorig);
  makemap();
  color(COLOR0);
  clear();
  sub1 = defpup(s1);
  sub2 = defpup(s2);
  sub3 = defpup(s3);
  sub4 = defpup(s4);
  sub5 = defpup(s5);
  menu = defpup(m1,sub2,sub4,sub3,sub1,sub5);
  setpup(menu,8,PUP_GREY);

  qdevice(LEFTMOUSE);
  qdevice(MIDDLEMOUSE);
  qdevice(MENUBUTTON);
  qdevice(WINQUIT);
  qdevice(REDRAW);
  qdevice(REDRAWICONIC);
  qdevice(WINFREEZE);
  qdevice(WINTHAW);

  fminit();
  if ((times = fmfindfont("Times-Roman")) == 0)
	{
	fprintf(stderr,"ERROR: couldn't find font: Times-Roman\n");
	exit(1);
	}
  else
	{
	times12 = fmscalefont(times,12.0);
	times32 = fmscalefont(times,32.0);
	}
  if ((helv = fmfindfont("Helvetica-Bold")) == 0)
	{
	fprintf(stderr,"ERROR: couldn't find font: Helvetica-Bold\n");
	exit(1);
	}
  else
	{
	helv14 = fmscalefont(helv,12.0);
	helv24 = fmscalefont(helv,18.0);
	}
}


/****************************************************************
 * setup_buttons - initialize all the buttons			*
 ****************************************************************/

setup_buttons()
{

  init_buttons();

  add_button( 20,20,0,0,"<",NULL,fun_play_backwards);
  add_button(100,20,0,0,"STOP",NULL,fun_stop);
  add_button(180,20,0,0,">",NULL,fun_play_forwards);
  add_button( 20,100,0,0,"-1",NULL,fun_jog_backwards);
  add_button(180,100,0,0,"+1",NULL,fun_jog_forwards);
  add_button( 20,180,0,0,"VAR","PLAY",fun_var_play);
  add_button(180,180,0,0,"RECORD",NULL,fun_blank);

  add_button(280,20,1,0,"LOOP",NULL,fun_loop);
  add_button(360,20,0,0,"SEG",NULL,fun_seg);
  add_button(280,100,0,0,"IN",NULL,fun_in);
  add_button(360,100,0,0,"OUT",NULL,fun_out);
  add_button(280,180,1,0,"SEGS","PLAY",fun_seg_play);
  add_button(360,180,0,0,"TRIM",NULL,fun_blank);

  add_button(460,20,0,0,"+ / -",NULL,fun_minus);
  add_button(540,20,0,0,"0",NULL,fun_0);
  add_button(620,20,0,0,".",NULL,fun_dot);
  add_button(700,20,0,0,"GOTO",NULL,fun_goto);
  add_button(460,100,0,0,"1",NULL,fun_1);
  add_button(540,100,0,0,"2",NULL,fun_2);
  add_button(620,100,0,0,"3",NULL,fun_3);
  add_button(700,100,0,0,"CLEAR",NULL,fun_clear);
  add_button(460,180,0,0,"4",NULL,fun_4);
  add_button(540,180,0,0,"5",NULL,fun_5);
  add_button(620,180,0,0,"6",NULL,fun_6);
  add_button(700,180,0,0,"FIELD",NULL,fun_blank);
  add_button(460,260,0,0,"7",NULL,fun_7);
  add_button(540,260,0,0,"8",NULL,fun_8);
  add_button(620,260,0,0,"9",NULL,fun_9);
  add_button(700,260,0,0,"ENTER",NULL,fun_blank);

  add_button( 20,280,1,1,"NORM","PLAY",fun_normal);
  add_button(100,280,0,0,"FREEZE",NULL,fun_freeze);
  add_button(180,280,0,0,"VIDEO","IN",fun_video_in);
  add_button(260,280,0,0," ",NULL,fun_blank);
  add_button(340,280,0,0,"MENU",NULL,fun_blank);
}


/****************************************************************
 * mainloop - the main body of action				*
 ****************************************************************/

mainloop()
{
  int 		choice;
  int		pad;
  int		flag;
  short 	val;
  long 		token;
  long 		mx,my;

  draw_panel();
  get_command("where\n");
  draw_where(get_command("where\n"));
  while (1)
	{
	token = qread(&val);
	switch (token)
		{
		case REDRAW:
			reshapeviewport();
			getorigin(&xorig,&yorig);
			draw_panel();
  			get_command("where\n");
  			draw_where(get_command("where\n"));
			break;
		case LEFTMOUSE:
			if (!getbutton(LEFTMOUSE))
				break;
			mx = getvaluator(MOUSEX);
			my = getvaluator(MOUSEY);
			mx -= xorig;
			my -= yorig;
			if (!(query_buttons(mx,my)))
				{
				if (pad = check_mousepad(mx,my))
					{
					if (pad == 1)
					  fun_play_backwards();
					else
					  jog_controller(LEFTMOUSE,-1);
					}
				}
			break;
		case MIDDLEMOUSE:
			if (!getbutton(MIDDLEMOUSE))
				break;
			mx = getvaluator(MOUSEX);
			my = getvaluator(MOUSEY);
			mx -= xorig;
			my -= yorig;
			if (!(query_buttons(mx,my)))
				{
				if (pad = check_mousepad(mx,my))
					fun_stop();
				}
			break;
		case MENUBUTTON:
			if (!getbutton(MENUBUTTON))
				break;
			mx = getvaluator(MOUSEX);
			my = getvaluator(MOUSEY);
			mx -= xorig;
			my -= yorig;
			if (!(query_buttons(mx,my)))
				{
				if (pad = check_mousepad(mx,my))
					{
					if (pad == 1)
					  fun_play_forwards();
					else
					  jog_controller(MENUBUTTON,1);
					}
				else
					{
					choice = dopup(menu);
					menu_action(choice);
					}
				}
			break;
		case REDRAWICONIC:
			break;
		case WINFREEZE:
			close_connection();
			break;
		case WINTHAW:
			reshapeviewport();
			getorigin(&xorig,&yorig);
			color(COLOR0);
			clear();
			establish_connection();
			draw_panel();
			break;
		case WINQUIT:
			close_connection();
			exit(0);
			break;
		}
	}
}


/****************************************************************
 * menu_action - handles the menu choices.			*
 ****************************************************************/

menu_action(choice)
int choice;
{
  if ((0 <= choice) && (choice <= 16))
	{
	fun_patterns(choice);
	}
  else
	{
	switch (choice)
		{
		case 100:
			send_command("clearseg\n");
			break;
		case 101:
			send_command("enable\n");
			break;
		case 102:
			send_command("disable\n");
			break;
		case 103:
			close_connection();
			exit(0);
			break;
		case 104:
			makemap();
			draw_panel();
			break;
		case 105:
			send_command("mode frame\n");
			break;
		case 106:
			send_command("mode field\n");
			break;
		case 107:
			system(trigger);
			break;
		case 108:
			send_command("input 1\n");
			break;
		case 109:
			send_command("input 2\n");
			break;
		case 200:
			fun_protect();
			break;
		case 201:
			fun_unprotect();
			break;
		}
	}
}


/****************************************************************
 * makemap - reset the colors that this program uses		*
 ****************************************************************/

makemap()
{
  mapcolor(COLOR0,50,50,50);
  mapcolor(COLOR1,250,250,250);
  mapcolor(COLOR2,210,210,210);
  mapcolor(COLOR3,170,170,170);
  mapcolor(COLOR4,130,130,130);
  mapcolor(COLOR5,90,90,90);
  mapcolor(COLOR6,100,100,100);
  mapcolor(COLOR7,114,109,220);
}


/****************************************************************
 * jog_controller						*
 ****************************************************************/

jog_controller(button,direction)
  int button;
  int direction;
{
  while (getbutton(button))
	{
	if (direction > 0)
		fun_jog_forwards();
	else
		fun_jog_backwards();
	}
}

/****************************************************************
 * functions that implement the keys				*
 ****************************************************************/

fun_play_forwards()
{
  int mode;
  char str[40];

  mode = get_mode();
  if (mode == LOOP)
	{
	sprintf(str,"loop %d %d\n",get_inreg(),get_outreg());
	send_command(str);
	}
  else if (mode == SEG)
	send_command("playseg\n");
  else
  	send_command("play\n");
  clear_where();
}

fun_play_backwards()
{
  int mode;
  char str[40];

  mode = get_mode();
  if (mode == LOOP)
	{
	sprintf(str,"loop %d %d -1\n",get_inreg(),get_outreg());
	send_command(str);
	}
  else if (mode == SEG)
	send_command("playseg -1\n");
  else
	send_command("play -1\n");
  clear_where();
}

fun_var_play()
{
  int mode;
  float speed;
  char str[40];

  speed = get_register();
  mode = get_mode();
  if (mode == LOOP)
	{
	sprintf(str,"loop %d %d %7.3f\n",get_inreg(),get_outreg(),speed);
	send_command(str);
	}
  else if (mode == SEG)
	{
	sprintf(str,"playseg %7.3f\n",speed);
	send_command(str);
	}
  else
	{
	sprintf(str,"play %7.3f\n",speed);
	send_command(str);
	}
  clear_where();
}

fun_stop()
{
  send_command("stop\n");
  get_command("where\n");
  draw_where(get_command("where\n"));
}

fun_jog_forwards()
{
  send_command("jog +1\n");
  get_command("where\n");
  draw_where(get_command("where\n"));
  fun_stop();
}

fun_jog_backwards()
{
  send_command("jog -1\n");
  get_command("where\n");
  draw_where(get_command("where\n"));
  fun_stop();
}

fun_loop()
{
  set_mode(LOOP);
}

fun_seg_play()
{
  set_mode(SEG);
}

fun_normal()
{
  set_mode(NORM);
}

fun_in()
{
  if (get_register() < 0)
	{
	ringbell();
	return(0);
	}
  set_inreg();
  draw_regs();
}

fun_out()
{
  if (get_register() < 0)
	{
	ringbell();
	return(0);
	}
  set_outreg();
  draw_regs();
}

fun_seg()
{
  char str[40];

  sprintf(str,"defseg %d %d pause\n",get_inreg(),get_outreg());
  send_command(str);
}

fun_goto()
{
  int mode;
  int val;
  char str[10];
  
  val = (int)get_register();
  mode = get_mode();
  if (val < 0)
	{
	ringbell();
	return(0);
	}
  if (mode == SEG)
	sprintf(str,"goseg %d\n",val);
  else
  	sprintf(str,"goto %d\n",val);
  send_command(str);
  get_command("where\n");
  draw_where(get_command("where\n"));
  fun_stop();
}

fun_clear()
{
  clear_register();
  draw_lcd();
}

fun_protect()
{
  char str[40];

  sprintf(str,"protect %d %d\n",get_inreg(),get_outreg());
  send_command(str);
}

fun_unprotect()
{
  char str[40];

  sprintf(str,"unprotect %d %d\n",get_inreg(),get_outreg());
  send_command(str);
}

fun_patterns(num)
int num;
{
  char str[40];

  sprintf(str,"pattern %d\n",num);
  send_command(str);
}

fun_1()
{
  add_register(1);
  draw_lcd();
}

fun_2()
{
  add_register(2);
  draw_lcd();
}

fun_3()
{
  add_register(3);
  draw_lcd();
}

fun_4()
{
  add_register(4);
  draw_lcd();
}

fun_5()
{
  add_register(5);
  draw_lcd();
}

fun_6()
{
  add_register(6);
  draw_lcd();
}

fun_7()
{
  add_register(7);
  draw_lcd();
}

fun_8()
{
  add_register(8);
  draw_lcd();
}

fun_9()
{
  add_register(9);
  draw_lcd();
}

fun_0()
{
  add_register(0);
  draw_lcd();
}


fun_video_in()
{
  if (video)
	{
	video = 0;
	send_command("stop\n");
	}
  else
	{
	video = 1;
	send_command("input 1\n");
	}
}


fun_freeze()
{
  if (freeze)
	{
	freeze = 0;
	send_command("unfreeze\n");
	}
  else
	{
	freeze = 1;
	send_command("freeze\n");
	}
}


fun_dot()
{
  set_dot();
  draw_lcd();
}

fun_minus()
{
  toggle_sign();
  draw_lcd();
}


fun_blank()
{
  return(1);
}


/****************************************************************
 * connect_dialog - dialog box for connection error		*
 ****************************************************************/

connect_dialog()
{
  long		xmin1,xmax1,xmin2,xmax2;
  long		ax,ay;
  int		x,y;
  short		val;
  long		token;

  qdevice(MOUSEX);
  qdevice(MOUSEY);
  ringbell();
  draw_connect_dialog();

  xmin1 = XWINSIZE/2 - (30+DI_BUTSIZEX);
  xmax1 = xmin1 + DI_BUTSIZEX;
  xmin2 = XWINSIZE/2 + 30;
  xmax2 = xmin2 + DI_BUTSIZEX;

  while (1)
	{
	while (!qtest());
	token = qread(&val);
	switch (token)
		{
		case LEFTMOUSE:
		case MIDDLEMOUSE:
		case MENUBUTTON:
			ax = getvaluator(MOUSEX);
			ay = getvaluator(MOUSEY);
			ax -= xorig; ay -= yorig;
			if ((ax >= xmin1) && (ax <= xmax1) &&
			    (ay >= 150) && (ay <= 210))
				{
  				invert_dialog_button(xmin1,150,"TRY AGAIN");
				return(1);
				}
			else if ((ax >= xmin2) && (ax <= xmax2) &&
			         (ay >= 150) && (ay <= 210))
				{
  				invert_dialog_button(xmin2,150,"QUIT");
				return(0);
				}
			break;
		case REDRAW:
			draw_connect_dialog();
			break;
		case WINQUIT:
			return(0);
		}
	}
}


draw_connect_dialog()
{
  int x,y;
  static char str[255];

  color(COLOR3); clear();
  fmsetfont(helv24);
  color(BLACK);
  sprintf(str,"ERROR: Unable to establish connection to A60.");
  x = (XWINSIZE / 2) - ((fmgetstrwidth(helv24,str) / 2));
  y = 350;
  cmov2i(x,y); fmprstr(str);

  fmsetfont(helv14);
  sprintf(str,"There might be another connection open already.");
  x = (XWINSIZE / 2) - ((fmgetstrwidth(helv14,str) / 2));
  y = 310;
  cmov2i(x,y); fmprstr(str);

  sprintf(str,"Only one connection at a time is allowed.");
  x = (XWINSIZE / 2) - ((fmgetstrwidth(helv14,str) / 2));
  y = 290;
  cmov2i(x,y); fmprstr(str);

  draw_dialog_button((XWINSIZE/2-(30+DI_BUTSIZEX)),150,"TRY AGAIN");
  draw_dialog_button(XWINSIZE/2+30,150,"QUIT");
}


draw_dialog_button(x,y,str)
int x,y;
char *str;
{
  int xlabel,ylabel;
  short a[2],b[2],c[2],d[2];

  color(COLOR3);
  rectfi(x+DI_BEVSIZE,y+DI_BEVSIZE,x+DI_BUTSIZEX-DI_BEVSIZE,y+DI_BUTSIZEY-DI_BEVSIZE);

  color(COLOR1);
  a[0] = x;  a[1] = y+DI_BUTSIZEY;
  b[0] = x+DI_BUTSIZEX; b[1] = y+DI_BUTSIZEY;
  c[0] = x+DI_BUTSIZEX-DI_BEVSIZE; c[1] = y+DI_BUTSIZEY-DI_BEVSIZE;
  d[0] = x+DI_BEVSIZE; d[1] = y+DI_BUTSIZEY-DI_BEVSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(COLOR5);
  a[0] = x;  a[1] = y;
  b[0] = x+DI_BUTSIZEX; b[1] = y;
  c[0] = x+DI_BUTSIZEX-DI_BEVSIZE; c[1] = y+DI_BEVSIZE;
  d[0] = x+DI_BEVSIZE; d[1] = y+DI_BEVSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(COLOR2);
  a[0] = x+DI_BUTSIZEX;  a[1] = y;
  b[0] = x+DI_BUTSIZEX; b[1] = y+DI_BUTSIZEY;
  c[0] = x+DI_BUTSIZEX-DI_BEVSIZE; c[1] = y+DI_BUTSIZEY-DI_BEVSIZE;
  d[0] = x+DI_BUTSIZEX-DI_BEVSIZE; d[1] = y+DI_BEVSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(COLOR4);
  a[0] = x;  a[1] = y;
  b[0] = x; b[1] = y+DI_BUTSIZEY;
  c[0] = x+DI_BEVSIZE; c[1] = y+DI_BUTSIZEY-DI_BEVSIZE;
  d[0] = x+DI_BEVSIZE; d[1] = y+DI_BEVSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(BLACK);
  xlabel = x + (DI_BUTSIZEX / 2) - ((fmgetstrwidth(helv14,str) / 2));
  ylabel = y + (DI_BUTSIZEY / 2) - 7;
  cmov2i(xlabel,ylabel); fmprstr(str);
}


invert_dialog_button(x,y,str)
int x,y;
char *str;
{
  int xlabel,ylabel;
  short a[2],b[2],c[2],d[2];

  color(BLACK);
  rectfi(x+DI_BEVSIZE,y+DI_BEVSIZE,x+DI_BUTSIZEX-DI_BEVSIZE,y+DI_BUTSIZEY-DI_BEVSIZE);

  color(COLOR5);
  a[0] = x;  a[1] = y+DI_BUTSIZEY;
  b[0] = x+DI_BUTSIZEX; b[1] = y+DI_BUTSIZEY;
  c[0] = x+DI_BUTSIZEX-DI_BEVSIZE; c[1] = y+DI_BUTSIZEY-DI_BEVSIZE;
  d[0] = x+DI_BEVSIZE; d[1] = y+DI_BUTSIZEY-DI_BEVSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(COLOR1);
  a[0] = x;  a[1] = y;
  b[0] = x+DI_BUTSIZEX; b[1] = y;
  c[0] = x+DI_BUTSIZEX-DI_BEVSIZE; c[1] = y+DI_BEVSIZE;
  d[0] = x+DI_BEVSIZE; d[1] = y+DI_BEVSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(COLOR4);
  a[0] = x+DI_BUTSIZEX;  a[1] = y;
  b[0] = x+DI_BUTSIZEX; b[1] = y+DI_BUTSIZEY;
  c[0] = x+DI_BUTSIZEX-DI_BEVSIZE; c[1] = y+DI_BUTSIZEY-DI_BEVSIZE;
  d[0] = x+DI_BUTSIZEX-DI_BEVSIZE; d[1] = y+DI_BEVSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(COLOR2);
  a[0] = x;  a[1] = y;
  b[0] = x; b[1] = y+DI_BUTSIZEY;
  c[0] = x+DI_BEVSIZE; c[1] = y+DI_BUTSIZEY-DI_BEVSIZE;
  d[0] = x+DI_BEVSIZE; d[1] = y+DI_BEVSIZE;
  bgnpolygon(); v2s(a); v2s(b); v2s(c); v2s(d); endpolygon();

  color(COLOR3);
  xlabel = x + (DI_BUTSIZEX / 2) - ((fmgetstrwidth(helv14,str) / 2));
  ylabel = y + (DI_BUTSIZEY / 2) - 7;
  cmov2i(xlabel,ylabel); fmprstr(str);
}

/********************************************************
 * setHourGlass                                         *
 ********************************************************/
setHourGlass()
{
  static short curs2[64] = {
        0x3fff,0xfffc,
        0x3cff,0xff3c,
        0x0c3f,0xfc30,
        0x0c1f,0xf830,
        0x0c0f,0xf030,
        0x0c03,0xc030,
        0x0601,0x8060,
        0x0300,0x00c0,
        0x0180,0x0180,
        0x00c0,0x0300,
        0x0060,0x0600,
        0x0030,0x0c00,
        0x0018,0x1800,
        0x000c,0x3000,
        0x000c,0x3000,
        0x000c,0x3000,
        0x000c,0x3000,
        0x000c,0x3000,
        0x000c,0x3000,
        0x0019,0x9800,
        0x0033,0xcc00,
        0x0067,0xe600,
        0x00cf,0xf300,
        0x0180,0x0180,
        0x0300,0x00c0,
        0x0600,0x0060,
        0x0c00,0x0030,
        0x0c00,0x0030,
        0x0c00,0x0030,
        0x0c00,0x0030,
        0x3fff,0xfffc,
        0x3fff,0xfffc };

  drawmode(CURSORDRAW);
  curstype(C32X1);
  mapcolor(1,255,0,0);
  defcursor(1,curs2);
  setcursor(1,0,0);
  drawmode(NORMALDRAW);
}

setNormalCursor()
{
  drawmode(CURSORDRAW);
  setcursor(0,0,0);
  drawmode(NORMALDRAW);
}

