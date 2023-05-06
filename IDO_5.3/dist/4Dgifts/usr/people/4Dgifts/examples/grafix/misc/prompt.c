/*
 *    prompt.c:
 *
 *       This is a very cheap, and very rudimentary one-line "textport"
 *   demo prog that employs one basic entry-point routine called
 *
 *                getUserString(prompt, result, maxlength) 
 *                char *prompt, *userStr;
 *                int maxlen;
 *
 *   which leaves things as it found them and has a fairly general interface.
 *
 *   It drops all events on the floor if they are not keyboard events. 
 *   (i.e. it loses REDRAW and RIGHTMOUSE and anything else that comes down 
 *   the pipe before the user responds.)
 *
 *   It is also very modal:  once you call it you are stuck till you give it 
 *   what it wants (a carriage return). This is very bad, but perhaps can be 
 *   lived with in certain situations.
 *
 *                  Peter Broadwell & dave ratcliffe   -  1989
 *
 *
 *	Added code inside getUserString to handle multi-matrix mode
 *   applications.
 *
 *		    Ivan M. Hajadi - 5-Mar-91
 *
 */
#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define PROMPT        1
#define EXIT          2

long menu;		/* defined menu's identifier */
char aString[40];
char anotherString[] = "a test string";

Matrix idm = {  1.0, 0.0, 0.0, 0.0,
		0.0, 1.0, 0.0, 0.0,
		0.0, 0.0, 1.0, 0.0,
		0.0, 0.0, 0.0, 1.0 };

main() {

        Device dev;
        short val;
        long menuval;


        init();

        /* process events forever */
        while(1) {
            dev=qread(&val);
            switch(dev) {
                case ESCKEY:
                    exit();
                    break;
                case REDRAW:
		    reshapeviewport();
                    color(BLUE);
                    clear();
                    break;
                case RIGHTMOUSE:
                    if(val) {
                        menuval = dopup(menu);
                        switch (menuval) {
                            case PROMPT:
                               /* prompt to get file name */
                                getUserString("File: ",aString,sizeof(aString));
				fprintf(stderr, "I got \"%s\", %s\n",
					    aString, anotherString);
                                break;
                            case EXIT:
                                exit();
                                break;
                            default:
                                break;
                        }
                    }
                    break;
                default:
                    break;
            }
        }
}

	
init()				/* do all the basic graphics setup */
{
    long sx, sy;

    winopen("simple stdin/out prompt");
    qdevice(ESCKEY);
    qdevice(RIGHTMOUSE);
    qenter(REDRAW, 1);
    menu = defpup("cheapee gl-stdin/out emulator %t|Prompt Me|Exit");
}


/* Clear prompt, move to start of prompt box, and output requested prompt */
getUserString(prompt,userStr,maxlen)			/* get name of file */
    char *prompt, *userStr;
    int maxlen;
{

/* lower left corner of prompt box */
#define FILEX 5
#define FILEY 15
#define FILEYHI (30+FILEY)        /* 30 pixels hi */
#define TEXTX (FILEX+5)
#define TEXTY (FILEY+10)
#define clearprompt(aprmpt)						\
    color(PUP_WHITE); clear(); color(PUP_BLACK); linewidth(2);		\
    recti(FILEX+2, FILEY+2, wxsize-8, FILEYHI-1);			\
    linewidth(1); cmov2i(TEXTX, TEXTY); charstr(aprmpt); 

    int cur_str_len;
    short c; 
    Device dev;
    long maxwidth, maxxval;	/* max length of window's width in pixels */
    char *str;
    char *prmpt = prompt, keyBoardWasQueued;
    long oldmode, xorig, yorig, wxsize, wysize;
    Screencoord mask1, mask2, mask3, mask4;
    Matrix oldmatrix;

		/* save old state to restore latter */
    pushmatrix();

    if (getmmode() != MSINGLE) {/* in case in multi-matrix mode */
       loadmatrix(idm);         /* load the identity matrix */
       getmatrix(oldmatrix); 
    }

    oldmode = getdrawmode();
    getscrmask(&mask1, &mask2, &mask3, &mask4);
    keyBoardWasQueued = isqueued(KEYBD);
		/* enable overlay, and be sure rubber band box will be red */
    drawmode(PUPDRAW);
		/* figure out how big we are this time around */
    getorigin(&xorig,&yorig);  
    getsize(&wxsize,&wysize);
    ortho2(-0.5,(float)wxsize, -0.5, (float)wysize);
    maxxval = wxsize + xorig;

    userStr[0] = '\0';
    maxwidth = (wxsize-11) - (FILEX + strwidth(prompt));
    scrmask(FILEX, (Screencoord)(wxsize-6), FILEY, FILEYHI);
        
    /* display prompt */
    cur_str_len = strlen(userStr);
    clearprompt(prmpt);

    qdevice(KEYBD);
    /* read till eof ('\n' or '\r') */
    while(dev = qread(&c)) {
        if(dev != KEYBD)
            continue;        /* don't care */
        switch(c) {
            case '\027':        /* ^W sets cursor back to start */
                cur_str_len = 0;
                clearprompt(prmpt);
                break;
            case '\n':
            case '\r':
                goto done;
            case '\b':
                if(cur_str_len) {
                    userStr[--cur_str_len] = '\0';
                    clearprompt(prmpt);
                    /* display rightmost portion */
                    for(str=userStr; *str && strwidth(str) > maxwidth; str++)
                        ;
                    charstr(str);
                }
                break;
            default:
		if(cur_str_len < (maxlen -1)) {
		    str = &userStr[cur_str_len];
		    userStr[cur_str_len++] = c;
		    userStr[cur_str_len] = '\0';
		    charstr(str);
		}
		else {
		    ringbell();
		}
                break;
        }
    }
done:
    if(!keyBoardWasQueued) unqdevice(KEYBD);
    scrmask(mask1, mask2, mask3, mask4);              /* restore old */
    drawmode(PUPDRAW);
    color(PUP_CLEAR);
    clear();
    drawmode(oldmode);

    if (getmmode() != MSINGLE) {
       loadmatrix(oldmatrix);     
    }
    popmatrix();
    userStr[maxlen] = '\0';
}
