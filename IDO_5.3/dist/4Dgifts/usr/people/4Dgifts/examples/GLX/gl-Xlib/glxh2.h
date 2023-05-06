/*
** Function prototype for the useful functions defined in glxh2.c
** Read that file for more info on how to call these.
*/

extern unsigned long GLXgetvalue(GLXconfig* conf, int buffer, int mode);
extern GLXconfig* GLXCreateWindow(
	Display* dpy,Window parent,
	int x,int y,
	int w,int h,
	int borderWidth,
	...);
