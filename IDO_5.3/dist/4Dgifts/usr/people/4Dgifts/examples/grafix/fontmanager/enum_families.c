/*
 *  enum_families.c:
 *
 *    Enumerate/print out the names of all fonts currently avaiable on the 
 *  machine this program is executed on.
 *	
 *				Glen Williams - 1987
 */

#include <fmclient.h>

void printname();


main(argc,argv)
int argc;
char **argv;
{
    fminit();
    fmenumerate(printname);
}


void printname(str)
char *str;
{
    printf("%s\n", str);
}
