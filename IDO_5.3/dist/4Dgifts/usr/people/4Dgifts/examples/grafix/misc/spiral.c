/*
 *        spiral - 
 *                Draw a spiral pattern using a version of turtlegraphics.
 *
 *                try "% spiral 121 0.3" then try something else.
 *
 *                                Paul Haeberli - 1984
 *
 */
#include <stdio.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>

double turnangle, growth;

main(argc,argv) 
int argc;
char *argv[];
{
    short val;
    Device dev;

    if (argc<3) {
            fprintf(stderr,"usage: spiral <angle> <growth>\n");
            exit(1);
    }
    turnangle = atof(argv[1]);        
    growth = atof(argv[2]);        
    keepaspect(1,1);
    winopen("spiral");
    qdevice(ESCKEY);

    drawit();
    while (1) {
        dev = qread(&val);
        switch (dev) {
            case ESCKEY:
                gexit();
                exit(0);
                break;
            case REDRAW:
                drawit();
                break;
        }
    }
}

drawit()
{
    int i;

    reshapeviewport();
    ortho2(-100.0,100.0,-100.0,100.0);
    color(7);
    clear();
    color(0);
    rectfi(-97,-97,97,97);
    color(7);
    rectfi(-95,-95,95,95);
    spiral(turnangle, growth);
}

spiral( deltaxangle, deltaxdistance )
float deltaxangle, deltaxdistance;
{
    register float distance;
    register int k, i;

    k = i = 0;
    pushmatrix();
    for (distance = 0.0; distance < 200.0; distance += deltaxdistance) {
        MOVE(distance);
        TURN(deltaxangle);
        if ((k++ % 5) == 0) {
            PENCOLOR(8+i);
            i = ((i+1)%40);
        }
    }
    popmatrix();
}

#define INVISIBLE -1
int  penc = INVISIBLE;

/*
 *        CLEARSCREEN - erase the entire screen area.
 */
CLEARSCREEN()
{
    clear();
}

/*
 *        MOVE - move some distance.
 */
MOVE( distance )
float distance;
{
    translate(distance,0.0,0.0);
    if (penc != INVISIBLE)
        draw(0.0,0.0,0.0);
    else
        move(0.0,0.0,0.0);
}

/*
 *        TURN - turn the turtle.
 */
TURN(angle)
float        angle;
{
    rotate((int)(10.0*angle),'z');
}

/*
 *        PENCOLOR - set the color of the turtle.
 */
PENCOLOR( threadcolor )
int threadcolor;
{
    penc = threadcolor;
    if (penc != INVISIBLE)
      color(penc); 
}

/*
 *        WHATCOLOR - return the color of the turtle.        
 */
WHATCOLOR()
{
    return penc;
}
