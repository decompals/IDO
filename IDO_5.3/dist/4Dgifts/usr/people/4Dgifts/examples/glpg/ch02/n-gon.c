#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gl/gl.h>

#define X	0
#define Y	1
#define XY	2

main(argc, argv)
int argc;
char *argv[];
{
    int n, i;
    float vert[XY];

    if (argc != 2) {
	fprintf(stderr, "Usage: n-gon <number of sides>\n");
	return 1;
    }
    n = atoi(argv[1]);

    prefsize(400, 400);
    winopen("n-gon");
    color(WHITE);
    clear();
    color(RED);
    bgnclosedline();
    for (i = 0; i < n; i++) {
	vert[X] = 200.0 + 100.0 * fcos(i * 2.0 * M_PI / n);
	vert[Y] = 200.0 + 100.0 * fsin(i * 2.0 * M_PI / n);
	v2f(vert);
    }
    endclosedline();
    sleep(10);
    gexit();
    return 0;
}
