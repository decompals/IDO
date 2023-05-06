/*
 *        Hemanth Kanekal
 *        4 April 1988
 *
 *        Program to exercise the CG2 board on CLOVER1G
 */

#include <stdio.h>
#include <get.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <gl/cg2vme.h>
#include <gl/addrs.h>

/* mode values */
#define RGBMODE        0
#define COLORINDEX 1
#define X170 647
#define Y170 486
#define XPAL 781
#define YPAL 576
#define XHRES 1280
#define YHRES 1024
#define DHPHASE 0x30
#define DCPHASE 0x40

/* takes argv[0] */
char *progname;

/* initialized modes */
long Cmode = FALSE;
long leftshift = FALSE;
long rightshift = FALSE;
long ctrlkey = FALSE;

/* updated by getorigin() and getsize() whenever window is changed */
long xorigin,yorigin;
long xsize,ysize;
long xll,yll,xur,yur;

unsigned char switchbits;
char *montype();


main(argc,argv)
int argc;
char *argv[];
{
    int i,k,err;
    int x, y, c;
    short dev,val;
    unsigned short phase;
    int mouse,keybd;
    int menu,mval;
    int palmenu,ntscmenu,helpmenu,infomenu;
    int phasemenu,reghphasemenu,regcphasemenu,standalonemenu;
    extern getopt();
    extern char *optarg;
    int optrcd = 0;

    err = 0;
    mouse = 0;
    keybd = 0;

    while ((c = getopt (argc, argv, "mk")) != EOF) {
        switch (c) {
            case 'm':
                optrcd = 1;
                mouse = 1;
                keybd = 0;
                break;

            case 'k':
                optrcd = 1;
                mouse = 0;
                keybd = 1;
                break;

            default:
                fprintf(stderr,"usage : %s -k (keyboard) or -m (mouse)\n ",argv[0]);
                exit(0);
                break;
        }
    }

    if(!optrcd) {
       fprintf(stderr,"\n");
       fprintf(stderr,"usage : %s -k (keyboard) or -m (mouse)\n",argv[0]);
       exit(0);
    }

    progname = argv[0];
    winopen(argv[0]);
    RGBmode();
    singlebuffer();
    gconfig();
    if (keybd) {
        print_help();
        fprintf(stderr,
            "Please make sure the mouse is attached to the window opened.\n");
    }
    qdevice(RIGHTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(LEFTMOUSE);
    qdevice(ESCKEY);
    qdevice(KEYBD);
    qdevice(AKEY);
    qdevice(BKEY);
    qdevice(CKEY);
    qdevice(DKEY);
    qdevice(EKEY);
    qdevice(FKEY);
    qdevice(GKEY);
    qdevice(HKEY);
    qdevice(IKEY);
    qdevice(JKEY);
    qdevice(KKEY);
    qdevice(LKEY);
    qdevice(MKEY);
    qdevice(NKEY);
    qdevice(OKEY);
    qdevice(PKEY);
    qdevice(QKEY);
    qdevice(RKEY);
    qdevice(SKEY);
    qdevice(TKEY);
    qdevice(UKEY);
    qdevice(VKEY);
    qdevice(WKEY);
    qdevice(XKEY);
    qdevice(YKEY);
    qdevice(ZKEY);
    qdevice(ONEKEY);
    qdevice(TWOKEY);


    infomenu =
        defpup("Information menu %t|current monitor %x2000|current mode %x2001|current horizontal phase %x4003|current color phase %x5003");
    regcphasemenu =
        defpup("Register Color Phase control menu %t|Increment by 10 %x5001|Decrement by 10 %x5002|Inform on current color phase %x5003");
    reghphasemenu =
        defpup("Register Horizontal Phase control menu %t|Increment by 10 %x4001|Decrement by 10 %x4002|Inform on current horizontal phase %x4003");
    phasemenu =
        defpup("DAC Phase  menu %t|Potentiometer control %x4000|Register horizontal phase control %m|Register Color phase control %m",reghphasemenu,regcphasemenu);
    helpmenu =
        defpup("Help menu %t|You can set video modes| for the CG2 board on CLOVER1G| by using NTSC or PAL menuiods. | Four modes are avaliable for each | video mode . Genlocking is | set up using these modes . | Right mouse is used to set |the modes of choice | Middle mouse is used to draw | circles and Left mouse is used| to change color to a specific set value| ESC key resets the monitor|You can switch back and forth between|keyboard driven and menu|driven program by appropriate selections");
    standalonemenu =
        defpup("Stand alone monitor menu %t|Stand alone 60Hz %x3000|Stand alone 30Hz %x3001|Stand alone NTSC %x3002|Stand alone PAL %x3003|Hi Res Genlock 60Hz %x3004|Hi Res Genlock 30Hz %x3005");
    ntscmenu =
        defpup("NTSC %t|Genlock NTSC:mode 0 (Composite Lock, Color Subcarier Locked, Overlay) %x100|Genlock NTSC:mode 1 (Composite or Sync lock, No Color Lock, No Overlay) %x101|Genlock NTSC:mode 2 (Standalone, No Overlay) %x102|Genlock NTSC:mode 3 (Composite or Sync lock, Color Lock, No Overlay) %x103|Genlock NTSC: mode 0 (Composite Lock, Color Subcarrier Locked, Overlay, High Brightness %x104");
    palmenu =
        defpup("PAL %t|Genlock PAL:mode 0 (Composite Lock, Color Subcarier Locked, Overlay) %x1000|Genlock PAL:mode 1 (Composite or Sync lock, No Color Lock, No Overlay) %x1001|Genlock PAL:mode 2 (Standalone, No Overlay) %x1002|Genlock PAL:mode 3 (Composite or Sync lock, Color Lock, No Overlay) %x1003|Genlock PAL: mode 0 (Composite Lock, Color Subcarrier Locked, Overlay, High Brightness %x1004");
    menu =
        defpup("Video Options %t|NTSC %m|PAL %m|Stand alone %m|Phase control %m|Help  %m|Info %m|Keybord input %x2|Exit %x1",ntscmenu,palmenu,standalonemenu,phasemenu,helpmenu,infomenu);

lmouse:
    while (mouse) {
        while (qtest()) {
            dev = qread(&val);
            switch (dev) {
                case RIGHTMOUSE:
                    switch(dopup(menu)) {
                        case 1:
                            doexit(0);
                            break;
                        case 2:
                            mouse = 0;
                            keybd = 1;
                            goto lkeybd;
                        case 100:
                            switchbits = getvideo(CG_MODE) & 0xF0 ;
                            switchbits &= ~CG2_M_HIGHOUT;
                            setvideo(DE_R1,DER1_G_170|DER1_UNBLANK);
                            setvideo(CG_MODE,switchbits|CG2_M_MODE0);
                            break;
                        case 101:
                            switchbits = getvideo(CG_MODE) & 0xF0 ;
                            switchbits &= ~CG2_M_HIGHOUT;
                            setvideo(DE_R1,DER1_G_170|DER1_UNBLANK);
                            setvideo(CG_MODE,switchbits|CG2_M_MODE1);
                            break;
                        case 102:
                            switchbits = getvideo(CG_MODE) & 0xF0 ;
                            switchbits &= ~CG2_M_HIGHOUT;
                            setvideo(DE_R1,DER1_G_170|DER1_UNBLANK);
                            setvideo(CG_MODE,switchbits|CG2_M_MODE2);
                            break;
                        case 103:
                            switchbits = getvideo(CG_MODE) & 0xF0 ;
                            switchbits &= ~CG2_M_HIGHOUT;
                            setvideo(DE_R1,DER1_G_170|DER1_UNBLANK);
                            setvideo(CG_MODE,switchbits|CG2_M_MODE3);
                            break;
                        case 104:
                            switchbits = getvideo(CG_MODE) & 0xF0 ;
                            switchbits |= CG2_M_HIGHOUT;
                            setvideo(DE_R1,DER1_G_170|DER1_UNBLANK);
                            setvideo(CG_MODE,switchbits|CG2_M_MODE0);
                            break;
                        case 1000:
                            switchbits = getvideo(CG_MODE) & 0xF0;
                            switchbits &= ~CG2_M_HIGHOUT;
                            setvideo(DE_R1,DER1_G_PAL|DER1_UNBLANK);
                            setvideo(CG_MODE,switchbits|CG2_M_FORMAT|CG2_M_MODE0);
                            break;
                        case 1001:
                            switchbits = getvideo(CG_MODE) & 0xF0 ;
                            switchbits &= ~CG2_M_HIGHOUT;
                            setvideo(DE_R1,DER1_G_PAL|DER1_UNBLANK);
                            setvideo(CG_MODE,switchbits|CG2_M_FORMAT|CG2_M_MODE1);
                            break;
                        case 1002:
                            switchbits = getvideo(CG_MODE) & 0xF0 ;
                            switchbits &= ~CG2_M_HIGHOUT;
                            switchbits |= CG2_M_SUBPHASE;
                            setvideo(DE_R1,DER1_G_PAL|DER1_UNBLANK);
                            setvideo(CG_MODE,switchbits|CG2_M_FORMAT|CG2_M_MODE2);
                            break;
                        case 1003:
                            switchbits = getvideo(CG_MODE) & 0xF0 ;
                            switchbits &= ~CG2_M_HIGHOUT;
                            switchbits |= CG2_M_SUBPHASE;
                            setvideo(DE_R1,DER1_G_PAL|DER1_UNBLANK);
                            setvideo(CG_MODE,switchbits|CG2_M_FORMAT|CG2_M_MODE3);
                            break;
                        case 1004:
                            switchbits = getvideo(CG_MODE) & 0xF0;
                            switchbits |= CG2_M_HIGHOUT;
                            setvideo(DE_R1, DER1_G_PAL|DER1_UNBLANK);
                            setvideo(CG_MODE,switchbits|CG2_M_FORMAT|CG2_M_MODE0);
                            break;
                        case 2000:
                            curmon();
                            break;
                        case 2001:
                            curmode();
                            break;
                        case 3000:
                            setvideo(DE_R1,DER1_60HZ|DER1_SYNCG|DER1_UNBLANK);        
                            break;
                        case 3001:
                            setvideo(DE_R1,DER1_30HZ|DER1_UNBLANK);
                            break;
                        case 3002:
                            setvideo(DE_R1,DER1_170|DER1_UNBLANK);
                            break;
                        case 3003:
                            setvideo(DE_R1,DER1_PAL|DER1_UNBLANK);
                            break;
                        case 3004:
                            switchbits = getvideo(CG_MODE) & 0xF0;
                            setvideo(DE_R1,DER1_G_60HZ|DER1_SYNCG|DER1_UNBLANK);
                            setvideo(CG_MODE,switchbits|CG2_M_FORMAT|CG2_M_MODE4);
                            break;
                        case 3005:
                            switchbits = getvideo(CG_MODE) & 0xF0;
                            setvideo(DE_R1,DER1_G_30HZ|DER1_UNBLANK);
                            setvideo(CG_MODE,switchbits|CG2_M_MODE4);
                            break;
                        case 4000:
                            switchbits = getvideo(CG_MODE);
                            switchbits &= ~CG2_M_HORPHASE;
                            setvideo(CG_MODE,switchbits);
                            break;
                        case 4001:
                            switchbits = getvideo(CG_MODE);
                            switchbits |= CG2_M_HORPHASE;
                            setvideo(CG_MODE,switchbits);
                            phase = getvideo(CG_HPHASE);
                            if(phase <= 245) 
                                phase += 10;
                            else
                                phase = 255;
                            setvideo(CG_HPHASE,phase);
                            break;
                        case 4002:
                            switchbits = getvideo(CG_MODE);
                            switchbits |= CG2_M_HORPHASE;
                            setvideo(CG_MODE,switchbits);
                            phase = getvideo(CG_HPHASE);
                            if(phase >= 10) 
                                phase -= 10;
                            else
                                phase = 0;
                            setvideo(CG_HPHASE,phase);
                            break;
                        case 4003:
                            phase = getvideo(CG_HPHASE);
                            fprintf(stderr,"Horizontal phase = %d\n ",phase);
                            break;
                        case 5001:
                            switchbits = getvideo(CG_MODE);
                            switchbits |= CG2_M_SUBPHASE;
                            setvideo(CG_MODE,switchbits);
                            phase = getvideo(CG_CPHASE);
                            if(phase <= 245) 
                                phase += 10;
                            else
                                phase = 255;
                            setvideo(CG_CPHASE,phase);
                            break;
                        case 5002:
                            switchbits = getvideo(CG_MODE);
                            switchbits |= CG2_M_SUBPHASE;
                            setvideo(CG_MODE,switchbits);
                            phase = getvideo(CG_CPHASE);
                            if(phase >= 10) 
                                phase -= 10;
                            else
                                phase = 0;
                            setvideo(CG_CPHASE,phase);
                            break;
                        case 5003:
                            phase = getvideo(CG_CPHASE);
                            fprintf(stderr,"Color phase = %d\n ",phase);
                            break;
                    }
                    break;
                case ESCKEY:
                    doreset();
                    break;
                case REDRAW:
                    redraw();
                    break;
                case MIDDLEMOUSE:
                    xll = getvaluator(MOUSEX);
                    yll = getvaluator(MOUSEY);
                    circfi(xll,yll,100);
                    swapbuffers();
                    break;
                case LEFTMOUSE:
                    RGBcolor(0,0,1);
                    clear();
                    RGBcolor(255,127,0);
                    break;
                default:
                    break;
            }
        }
    }        

lkeybd:
    while (keybd) {
        while (qtest()) {
            dev = qread(&val);
            switch (dev) {
                case AKEY:
                    switchbits = getvideo(CG_MODE) & 0xF0 ;
                    switchbits &= ~CG2_M_HIGHOUT;
                    setvideo(DE_R1,DER1_G_170|DER1_UNBLANK);
                    setvideo(CG_MODE,switchbits|CG2_M_MODE0);
                    break;
                case BKEY:
                    switchbits = getvideo(CG_MODE) & 0xF0 ;
                    switchbits &= ~CG2_M_HIGHOUT;
                    setvideo(DE_R1,DER1_G_170|DER1_UNBLANK);
                    setvideo(CG_MODE,switchbits|CG2_M_MODE1);
                    break;
                case CKEY:
                    switchbits = getvideo(CG_MODE) & 0xF0 ;
                    switchbits &= ~CG2_M_HIGHOUT;
                    setvideo(DE_R1,DER1_G_170|DER1_UNBLANK);
                    setvideo(CG_MODE,switchbits|CG2_M_MODE2);
                    break;
                case DKEY:
                    switchbits = getvideo(CG_MODE) & 0xF0 ;
                    switchbits &= ~CG2_M_HIGHOUT;
                    setvideo(DE_R1,DER1_G_170|DER1_UNBLANK);
                    setvideo(CG_MODE,switchbits|CG2_M_MODE3);
                    break;
                case EKEY:
                    switchbits = getvideo(CG_MODE) & 0xF0 ;
                    switchbits |= CG2_M_HIGHOUT;
                    setvideo(DE_R1,DER1_G_170|DER1_UNBLANK);
                    setvideo(CG_MODE,switchbits|CG2_M_MODE0);
                    break;
                case FKEY:
                    switchbits = getvideo(CG_MODE) & 0xF0;
                    switchbits &= ~CG2_M_HIGHOUT;
                    setvideo(DE_R1,DER1_G_PAL|DER1_UNBLANK);
                    setvideo(CG_MODE,switchbits|CG2_M_FORMAT|CG2_M_MODE0);
                    break;
                case GKEY:
                    switchbits = getvideo(CG_MODE) & 0xF0 ;
                    switchbits &= ~CG2_M_HIGHOUT;
                    setvideo(DE_R1,DER1_G_PAL|DER1_UNBLANK);
                    setvideo(CG_MODE,switchbits|CG2_M_FORMAT|CG2_M_MODE1);
                    break;
                case HKEY:
                    print_help();
                    break;
                case IKEY:
                    switchbits = getvideo(CG_MODE) & 0xF0 ;
                    switchbits &= ~CG2_M_HIGHOUT;
                    switchbits |= CG2_M_SUBPHASE;
                    setvideo(DE_R1,DER1_G_PAL|DER1_UNBLANK);
                    setvideo(CG_MODE,switchbits|CG2_M_FORMAT|CG2_M_MODE2);
                    break;
                case JKEY:
                    switchbits = getvideo(CG_MODE) & 0xF0 ;
                    switchbits &= ~CG2_M_HIGHOUT;
                    switchbits |= CG2_M_SUBPHASE;
                    setvideo(DE_R1,DER1_G_PAL|DER1_UNBLANK);
                    setvideo(CG_MODE,switchbits|CG2_M_FORMAT|CG2_M_MODE3);
                    break;
                case KKEY:
                    switchbits = getvideo(CG_MODE) & 0xF0;
                    switchbits |= CG2_M_HIGHOUT;
                    setvideo(DE_R1, DER1_G_PAL|DER1_UNBLANK);
                    setvideo(CG_MODE,switchbits|CG2_M_FORMAT|CG2_M_MODE0);
                    break;
                case LKEY:
                    curmon();
                    break;
                case MKEY:
                    mouse = 1;
                    keybd = 0;
                    goto lmouse;
                case NKEY:
                    curmode();
                    break;
                case OKEY:
                    setvideo(DE_R1,DER1_60HZ|DER1_SYNCG|DER1_UNBLANK);        
                    break;
                case PKEY:
                    setvideo(DE_R1,DER1_30HZ|DER1_UNBLANK);
                    break;
                case QKEY:
                    doexit(0);
                    break;
                case RKEY:
                    setvideo(DE_R1,DER1_170|DER1_UNBLANK);
                    break;
                case SKEY:
                    setvideo(DE_R1,DER1_PAL|DER1_UNBLANK);
                    break;
                case TKEY:
                    switchbits = getvideo(CG_MODE) & 0xF0;
                    setvideo(DE_R1,DER1_G_60HZ|DER1_SYNCG|DER1_UNBLANK);
                    setvideo(CG_MODE,switchbits|CG2_M_FORMAT|CG2_M_MODE4);
                    break;
                case UKEY:
                    switchbits = getvideo(CG_MODE) & 0xF0;
                    setvideo(DE_R1,DER1_G_30HZ|DER1_UNBLANK);
                    setvideo(CG_MODE,switchbits|CG2_M_MODE4);
                    break;
                case VKEY:
                    switchbits = getvideo(CG_MODE);
                    switchbits &= ~CG2_M_HORPHASE;
                    setvideo(CG_MODE,switchbits);
                    break;
                case WKEY:
                    switchbits = getvideo(CG_MODE);
                    switchbits |= CG2_M_HORPHASE;
                    setvideo(CG_MODE,switchbits);
                    phase = getvideo(CG_HPHASE);
                    if(phase <= 245) 
                        phase += 10;
                    else
                        phase = 255;
                    setvideo(CG_HPHASE,phase);
                    break;
                case XKEY:
                    switchbits = getvideo(CG_MODE);
                    switchbits |= CG2_M_HORPHASE;
                    setvideo(CG_MODE,switchbits);
                    phase = getvideo(CG_HPHASE);
                    if(phase >= 10) 
                        phase -= 10;
                    else
                        phase = 0;
                    setvideo(CG_HPHASE,phase);
                    break;
                case YKEY:
                    phase = getvideo(CG_HPHASE);
                    fprintf(stderr,"Horizontal phase = %d\n ",phase);
                    break;
                case ZKEY:
                    switchbits = getvideo(CG_MODE);
                    switchbits |= CG2_M_SUBPHASE;
                    setvideo(CG_MODE,switchbits);
                    phase = getvideo(CG_CPHASE);
                    if(phase <= 245) 
                        phase += 10;
                    else
                        phase = 255;
                    setvideo(CG_CPHASE,phase);
                    break;
                case ONEKEY:
                    switchbits = getvideo(CG_MODE);
                    switchbits |= CG2_M_SUBPHASE;
                    setvideo(CG_MODE,switchbits);
                    phase = getvideo(CG_CPHASE);
                    if(phase >= 10) 
                        phase -= 10;
                    else
                        phase = 0;
                    setvideo(CG_CPHASE,phase);
                    break;
                case TWOKEY:
                    phase = getvideo(CG_CPHASE);
                    fprintf(stderr,"Color phase = %d\n ",phase);
                    break;
                case ESCKEY:
                    doreset();
                    break;
                case (SKEY && CTRLKEY):
                    doblock();
                    break;
                default:
                    break;
            }
        }
    }        
} /* end of main */
print_help() {
fprintf(stderr,"\n");
fprintf(stderr," AKEY : Genlock NTSC:mode 0 (Composite Lock,Color Subcarier Locked,Overlay).\n");
fprintf(stderr," BKEY : Genlock NTSC:mode 1 (Composite/Sync lock, No Color Lock, No Overlay).\n");
fprintf(stderr," CKEY: Genlock NTSC:mode 2 (Standalone, No Overlay) .\n");
fprintf(stderr," DKEY : Genlock NTSC:mode 3 (Composite or Sync lock, Color Lock, No Overlay).\n");
fprintf(stderr," EKEY : Genlock NTSC: mode 0 (same as above except High Brightness) .\n");
fprintf(stderr," FKEY : Genlock PAL:mode 0 (Composite Lock, Color Subcarier Locked, Overlay).\n");
fprintf(stderr," GKEY : Genlock PAL:mode 1 (Composite or Sync lock, No Color Lock, No Overlay).\n");
fprintf(stderr," HKEY : This menu .\n");
fprintf(stderr," IKEY : Genlock PAL:mode 2 (Standalone, No Overlay) .\n");
fprintf(stderr," JKEY : Genlock PAL:mode 3 (Composite/Sync lock, Color Lock, No Overlay).\n");
fprintf(stderr," KKEY : Genlock PAL: mode 0 (same as above except High Brightness).\n");
fprintf(stderr," LKEY : current monitor .\n");
fprintf(stderr," MKEY : Mouse input .\n");
fprintf(stderr," NKEY : current mode .\n");
fprintf(stderr," OKEY : Stand alone 60Hz .\n");
fprintf(stderr," PKEY : Stand alone 30Hz .\n");
fprintf(stderr," QKEY : Exit .\n");
fprintf(stderr," RKEY : Stand alone NTSC .\n");
fprintf(stderr," SKEY : Stand alone PAL .\n"); 
fprintf(stderr," TKEY : Hi Res Genlock 60Hz .\n"); 
fprintf(stderr," UKEY : Hi Res Genlock 30Hz .\n");
fprintf(stderr," VKEY : Potentiometer control .\n");
fprintf(stderr," WKEY : Increment current horizontal phase by 10 .\n");
fprintf(stderr," XKEY : Decrement current horizontal phase by 10 .\n");
fprintf(stderr," YKEY : Inform on current horizontal phase .\n");
fprintf(stderr," ZKEY : Increment current color phase by 10 .\n");
fprintf(stderr," ONEKEY : Decrement current color phase by 10 .\n");
fprintf(stderr," TWOKEY : Inform on current color phase .\n");
}
doblock() {
    /* wait on queue until the Q key is pressed */
    short dev,val;
    wintitle("WAITING - CTRL Q to continue");
    while (1) {
        dev = qread(&val);
        if ((dev == QKEY) && val && ctrlkey) {
            break;
        } else if (dev == CTRLKEY)
            ctrlkey = val;
        else if (dev == ESCKEY)
            doexit(0);
    }
}

redraw() {
    getorigin(&xorigin,&yorigin);
    getsize(&xsize,&ysize);
    viewport(0,xsize-1,0,ysize-1);
    ortho2(-0.5,(float)xsize-0.5,-0.5,(float)ysize-0.5);
}
doexit(i) {
    setvideo(DE_R1,0x4a);
    gexit();
    exit(i);
}
doreset() {
    setvideo(DE_R1,0x4a);
}
curmode()
{
    fprintf(stderr,"mode %x\n",(long) getvideo(CG_MODE));
}
curmon()
{
    fprintf(stderr,"current getmonitor() is %s\n", montype(getmonitor()));
}
char *montype(monitor)
long monitor;
{

    switch ( monitor ) {

        case HZ30:
                return("HZ30");
                break;
        case HZ60:
                return("HZ60");
                break;
        case NTSC:
                return("NTSC");
                break;
        case HZ50:
                return("HZ50");
                break;
        case MONA:
                return("MONA");
                break;
        case MONB:
                return("MONB");
                break;
        case MONC:
                return("MONC");
                break;
        case MOND:
                return("MOND");
                break;
        case PAL:
                return("PAL");
                break;
        case HZ30_SG:
                return("HZ30_SG");
                break;
        case MON_ALL:
                return("MON_ALL");
                break;
        case MON_GEN_ALL:
                return("MON_GEN_ALL");
                break;
        case MONSPECIAL:
                return("MONSPECIAL");
                break;
        default:
                return("unknown");
                break;
        }
}

loadmap(clr)
int clr;
{
        int i;
        switch (clr)
                {
                case 1:
                        for (i=0;i<256;i++)
                                mapcolor(i+10,i,0,0);
                        break;
                case 2:
                        for (i=0;i<256;i++)
                                mapcolor(i+10,0,i,0);
                        break;
                case 3:
                        for (i=0;i<256;i++)
                                mapcolor(i+10,0,0,i);
                        break;
                case 4:
                        for (i=0;i<256;i++)
                                mapcolor(i+10,i,i,i);
                        break;
                }
}
