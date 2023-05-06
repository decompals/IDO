#include <stdio.h>
#include <string.h>
#include <gl/gl.h>

#define BUFSIZE	    110

float vert[3][2] = {
    {0.1, 0.2},
    {0.7, 0.4},
    {0.2, 0.7}
};

void drawit()
{
    pushmatrix();
	color(WHITE);
	bgnpolygon();
	    v2f(vert[0]);
	    v2f(vert[1]);
	    v2f(vert[2]);
	endpolygon();
	translate(0.1, 0.1, 0.0);
	color(RED);
	bgnline();
	    v2f(vert[0]);
	    v2f(vert[1]);
	    v2f(vert[2]);
	endline();
	translate(0.1, 0.1, 0.0);
	color(GREEN);
	bgnpoint();
	    v2f(vert[0]);
	    v2f(vert[1]);
	    v2f(vert[2]);
	endpoint();
    popmatrix();
}

/* feedback buffer is an array of floats on Personal Iris and VGX */
Boolean floatfb()
{
    char model[12];
    Boolean isPI, isVGX;

    gversion(model);
    isPI = (strncmp(&model[4], "PI", 2) == 0);
    isVGX = (strncmp(&model[4], "VGX", 3) == 0);
    return (isPI || isVGX);
}

main()
{
    short sbuf[BUFSIZE];
    float fbuf[BUFSIZE];
    void *buf;
    long i, count;
    Boolean hasfloatfb;

    foreground();
    prefsize(400, 400);
    winopen("feedback");
    color(BLACK);
    clear();
    ortho2(0.0, 1.0, 0.0, 1.0);
    hasfloatfb = floatfb();
    drawit();
    if (hasfloatfb)
	buf = fbuf;
    else
	buf = sbuf;
    feedback(buf, BUFSIZE);
	drawit();
    count = endfeedback(buf);
    if (count == BUFSIZE) {
	printf("Feedback buffer overflow\n");	
	return 1;
    }
    else 
	printf("Got %d items:\n", count);
    for (i = 0; i < count; i++) {
	if (hasfloatfb)
	    printf("%.2f", fbuf[i]);
	else
	    printf("%d", sbuf[i]);
	if (i % 8 == 7)
	    printf("\n");
	else
	    printf("\t");
    }
    printf("\n");    
    sleep(10);
    gexit();
    return 0;
}
