/*
 *	getset - 
 *		Get and set values stored in ~/.gamma
 *
 *				Paul Haeberli - 1984
 *
 */
#include "stdio.h"
#include "unistd.h"
#include "port.h"
#include "gl.h"

FILE *configopen();

float getgamma()
{
    FILE *gamfile;
    float gam;

    if ((gamfile = fopen("/etc/config/system.glGammaVal","r")) )  {
        if (fscanf(gamfile,"%f\n",&gam) == 1) {
	    fclose(gamfile);
	    return gam;
	} else 
	    fclose(gamfile);
    }
    return 1.7;
}

setgamma( gam )
float gam;
{
    FILE *gamfile;
    float curgam;

    /*check to see if it's necessary to open the file for writing*/
    curgam = getgamma();
    if(curgam == gam) {
      newgamtables();
      exit(0);
    }

    if ((gamfile = fopen("/etc/config/system.glGammaVal.new","w")) == 0) {
	fprintf(stderr,"Only root can change gamma\n");
	return;
    }
    fprintf(gamfile,"%f\n",gam);
    fclose(gamfile);
    /* use rename to prevent trashing the gammaval file in case of power off*/
    rename("/etc/config/system.glGammaVal.new","/etc/config/system.glGammaVal");
    /* add a sync to ease file system damage done by new users hitting the power
       button before bdflush is run */
    sync();
    newgamtables();
}

getcolorbal(r,g,b)
unsigned int *r, *g, *b;
{
    FILE *cbfile;

    if ((cbfile = configopen(".cbal","r")) ) { 
        if (fscanf(cbfile,"%d %d %d\n",r,g,b) == 3) {
	    if (*r>255)
		*r = 255;	
	    if (*g>255)
		*g = 255;	
	    if (*b>255)
		*b = 255;	
            fclose(cbfile);
            return;
        } else 
            fclose(cbfile);
    }
    *r = 255;
    *g = 255;
    *b = 255;
    return;
}

setcolorbal(r,g,b)
int r, g, b;
{
    FILE *cbfile;

    if ((cbfile = configopen(".cbal","w")) == 0) {
	fprintf(stderr,"couldn't open .cbal\n");
	return;
    }
    fprintf(cbfile,"%d %d %d\n",r,g,b);
    fclose(cbfile);
    newgamtables();
}

FILE *configopen( name, mode )
char name[];
char mode[];
{
    char homepath[100];
    FILE *f;
    char *cptr;

    cptr = (char *)getenv("HOME");
    if (!cptr)
	return 0;
    strcpy(homepath,cptr);
    strcat(homepath,"/");
    strcat(homepath,name);
    return fopen(homepath,mode);
}
