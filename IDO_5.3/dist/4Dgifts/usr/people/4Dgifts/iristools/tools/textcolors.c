/*
 *	textcolors - 
 *		Set the color indexes used for the textport.
 *
 *				Paul Haeberli - 1985
 */
#include <stdio.h>
#include <string.h>
#include "gl.h"

static char* termtype;
int is_3k(), is_4d();
extern char *getenv();

main(argc,argv)
int argc;
char **argv;
{
    int i;
    int args[10];

    if ((argc == 5) || (argc == 7)) {
	for (i=1;i<=argc-1;i++) {
	    if (sscanf(argv[i],"%ld",&args[i])!=1)
		goto error;
	}
	if (is_3k()) {
	    printf("\0337F%c",'0'+args[1]);
	    printf("\0337B%c",'0'+args[2]);
	    printf("\0337R%c",'0'+args[3]);
	    printf("\0337C%c",'0'+args[4]);
	} else if (is_4d()) {
	    printf("\033[101;%d/y",args[1]);
	    printf("\033[102;%d/y",args[2]);
	    printf("\033[103;%d/y",args[3]);
	    printf("\033[104;%d/y",args[4]);
	    if (argc == 7)
		printf("\033[111;%d;%d/y", args[5], args[6]);
	}
	exit(0);
    }

error:
    fprintf(stderr, "usage: textcolors text page reverse cursor");
    if (is_4d())
	fprintf(stderr, " {selfg selbg}");
    fprintf(stderr, "\n");
    exit(1);
}


/*
 * termcap type textport support
 *     is_3k(), is_4d() test TERM variable for
 *
 * if the TERM variable contains "iris", but is not prefixed 
 * by "iris-" the terminal is judged to be a IRIS3XXX terminal emulator
 */
static int is_3k()
{
   int i;

   termtype = getenv("TERM");
   if (!termtype) 
       return FALSE;
   if (!strncmp( termtype, "iris-", 5 )) 
       return FALSE;
   for (i=1;i<strlen(termtype); i++) {
       if (*(termtype+i)=='i') {
	   if (!strncmp( (termtype+i), "iris", 4)) 
	       return TRUE;
       }
   }
   return FALSE;
}

static int is_4d() 
{
   termtype = getenv("TERM");
   if (!termtype) 
       return FALSE;
   if (!strncmp( termtype, "iris-", 5 )) 
       return TRUE;
   return FALSE;
}
