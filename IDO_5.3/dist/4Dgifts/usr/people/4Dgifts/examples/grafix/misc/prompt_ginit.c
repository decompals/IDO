/*
 *    prompt_ginit.c:
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
 *   This program differs from prompt.c in that it uses ginit() instead of
 *   winopen(), thus taking over the entire screen and cutting off access
 *   to the default window-manager popup menu.
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
 */

#include <sys/types.h>
#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define PROMPT        1 
#define EXIT          2

Icoord maxxval;            /* width of prompt window ( in screen coords) */
long menu;                 /* defined menu's identifier */
long oldmode;              /* previous drawmode */
char filename[80];         /* array to store standard input for the prompt */

            /* setup for the prompt box */
Screencoord mask1, mask2, mask3, mask4;  /* full window */
long xmaxscrn;                           /* maximum size of screen in x */


main()
{
        Device dev;
        short val;
        long menuval;


	
        init();
        color(BLUE);
	clear();

        /* process events forever */
        while(1) {
            dev=qread(&val);
            switch(dev) {
                case ESCKEY:
                    endit();
                    break;
                case RIGHTMOUSE:
                    if(val) {
                        menuval = dopup(menu);
                        switch (menuval) {
                            case PROMPT:
                               /* prompt to get file name */
				mktxtport();
                                break;
                            case EXIT:
                                endit();
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

endit() {
    clr_overlay();
    gexit();
    exit(0);
}

Boolean MACHMODE;

init() {                               /* do all the basic graphics setup */

    xmaxscrn = getgdesc(GD_XPMAX)-1;

    ginit();

    /* enable overlay, and be sure rubber band box will be red */
    if (getgdesc(GD_BITS_OVER_SNG_CMODE) < 2)
        MACHMODE = PUPDRAW;
    else {
        MACHMODE = OVERDRAW;
        overlay(2);                             /* define/setup overlays */
        gconfig();
    }

    drawmode(MACHMODE);
    mapcolor(BLACK, 0, 0, 0);
    mapcolor(RED, 255, 0, 0);
    drawmode(NORMALDRAW);

    maxxval = 590;                   /* prompt window's width in pixels */

    /* initial values for restoring screenmask */
    getscrmask(&mask1, &mask2, &mask3, &mask4);

    qdevice(ESCKEY);
    qdevice(RIGHTMOUSE);
      
    menu = defpup("cheapee gl-stdin/out emulator %t|Prompt Me|Exit");

}


clearpup() {     /*  clear the pup plane where we asked for the filename */

    setupprompt(0);
    color(PUP_CLEAR);
    clear();                 /* clear the PUPDRAW plane */
    endprompt();
}


clr_overlay() {    /* clear any left over junk in overlay and pup planes */

    drawmode(MACHMODE);
    color(BLACK);
    clear();
    drawmode(PUPDRAW);
    color(PUP_CLEAR);
    clear();
    drawmode(NORMALDRAW);
}


/* screen positionings of prompt box */
#define FILEX 345
#define FILEY 30
#define FILEYHI (30+FILEY)        /* 30 pixels hi */
#define TEXTX (FILEX+5)
#define TEXTY (FILEY+10)

/* Clear prompt, move to start of prompt box, and output requested prompt */
clearprompt(prmpt)
char *prmpt;
{ 
    color(PUP_WHITE); 
    clear(); 
    color(PUP_BLACK);
    linewidth(2);
    recti(FILEX+2, FILEY+2, FILEX+maxxval-6, FILEYHI-3);
    linewidth(1);
    cmov2i(TEXTX, TEXTY);
    charstr(prmpt); 
}


setupprompt(sav) {

    if(sav)
        oldmode = getdrawmode();
    drawmode(PUPDRAW);
    /* so can clear just text */
    scrmask(FILEX, (Screencoord)(FILEX+maxxval-6), FILEY, FILEYHI);
}


endprompt() {

    scrmask(mask1, mask2, mask3, mask4);        /* restore old */
    drawmode(oldmode);
}


mktxtport()                               /* get name of file */
{
    int curstrlen;
    short c; 
    Device dev;
    long maxwidth;
    size_t maxlen = sizeof(filename);
    char *str;
    static char fprompt[] = "File: ";
    char *prmpt = fprompt;

    maxwidth = (xmaxscrn-11) - (FILEX + strwidth(fprompt));
    setupprompt(1);
        
    /* display prompt */
    curstrlen = 0;
    clearprompt(prmpt);

    qdevice(KEYBD);
    /* read until carriage return or linefeed */
    while(dev = qread(&c)) {
        if(dev != KEYBD)
            continue;            /* don't care */
        switch(c) {
            case '\027':         /* ctrl-W sets back to start of prompt */
                curstrlen = 0;
                clearprompt(prmpt);
                break;
            case '\n':
            case '\r':
		fprintf(stderr, "%s\n", filename);
                goto done;
            case '\b':
                if(curstrlen) {
                    filename[--curstrlen] = '\0';
                    clearprompt(prmpt);
                    /* display rightmost portion */
                    for(str=filename; *str && strwidth(str) > maxwidth; str++)
                        ;
                    charstr(str);
                }
                break;
            default:
                str = &filename[curstrlen];
                filename[curstrlen++] = c;
                filename[curstrlen] = '\0';
                charstr(str);
            break;
        }
    }

done:
    unqdevice(KEYBD);
    endprompt();
    clearpup();
    clr_overlay();

}
