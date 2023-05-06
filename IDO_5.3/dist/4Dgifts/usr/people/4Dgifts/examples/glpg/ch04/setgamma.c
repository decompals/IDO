#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gl/gl.h>

main(argc, argv)
int argc;
char *argv[];
{
    int i;
    double val;
    short tab[256];

    if (argc != 2) {
	fprintf(stderr, "Usage: setgamma <value>\n");
	return 0;
    }
    val = atof(argv[1]);

    noport();
    winopen("setgamma");
    for (i = 0; i < 256; i++)
	tab[i] = 255.0 * pow(i / 255.0, 1.0 / val) + 0.5;
    gammaramp(tab, tab, tab);
    gexit();
    return 0;
}
