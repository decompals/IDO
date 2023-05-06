/*
 *	pstri -
 *		Postscript support for drawing lines, and 
 *	shaded triangles.
 *
 *			    Paul Haeberli - 1990
 */
#include "vect.h"
#include "stdio.h"
#include "lum.h"

static intersect();
static subdivtri();
static outtri();
static plerp();

static FILE *outf;

#define FULLPAGEYSIZE	(11.0)

#define PNTS		(72.0)
#define PAGEXSIZE	(8.0)
#define PAGEYSIZE	(10.5)
#define PAGEASPECT	(PAGEXSIZE/PAGEYSIZE)
#define PAGEMARGIN	(0.25)

beginps(theoutf,aspect,x1,x2,y1,y2)
FILE *theoutf;
float aspect, x1, x2, y1, y2;
{
    int i;
    float *fptr;
    float imgxinches, imgyinches;
    float pagexinches, pageyinches, pageaspect;
    float dx, dy;

    outf = theoutf;
    fprintf(outf,"%%!PS-Adobe-2.0 EPSF-1.2\n");
    fprintf(outf,"%%%%Creator: IRIS program output\n");
    fprintf(outf,"%%%%BoundingBox: %f %f %f %f\n",PAGEMARGIN*72.0,PAGEMARGIN*72.0,
		            (PAGEXSIZE-PAGEMARGIN)*72.0,(PAGEYSIZE-PAGEMARGIN)*72.0);
    fprintf(outf,"%%%%EndComments\n");
    fprintf(outf,"0.0001 setlinewidth\n");
    fprintf(outf,"1 setlinecap\n");
    fprintf(outf,"1 setlinejoin\n");
    fprintf(outf,"gsave\n");

    dx = x2-x1;
    dy = y2-y1;
    if(aspect>1.0) {
	fprintf(outf,"%f %f translate\n",0.0,PNTS*FULLPAGEYSIZE);
	fprintf(outf,"-90.0 rotate\n");
	pagexinches = PAGEYSIZE;
	pageyinches = PAGEXSIZE;
	pageaspect = 1.0/PAGEASPECT;
    } else {
	pagexinches = PAGEXSIZE;
	pageyinches = PAGEYSIZE;
	pageaspect = PAGEASPECT;
    }
    if(aspect<=pageaspect) {
	imgyinches = pageyinches;
	imgxinches = imgyinches*aspect;
	fprintf(outf,"%f %f translate\n",
		 PNTS*(PAGEMARGIN+(pagexinches-imgxinches)/2.0),PNTS*PAGEMARGIN);
    } else {
	imgxinches = pagexinches;
	imgyinches = imgxinches/aspect;
	fprintf(outf,"%f %f translate\n",
		 PNTS*PAGEMARGIN,PNTS*(PAGEMARGIN+(pageyinches-imgyinches)/2.0));
    }
    fprintf(outf,"%f %f scale\n",PNTS*imgxinches,PNTS*imgyinches);
    fprintf(outf,"%f %f scale\n",1.0/dx,1.0/dy);
    fprintf(outf,"%f %f translate\n",-x1,-y1);

    fprintf(outf,"0.0 setgray\n");

    fprintf(outf,"/x {\n");
    fprintf(outf,"dup\n");
    fprintf(outf,"dup\n");
    fprintf(outf,"} bind def\n");

    fprintf(outf,"/s {\n");
    fprintf(outf,"setlinewidth\n");
    fprintf(outf,"} bind def\n");

    fprintf(outf,"/d {\n");
    fprintf(outf,"newpath\n");
    fprintf(outf,"moveto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"closepath\n");
    fprintf(outf,"stroke\n");
    fprintf(outf,"} bind def\n");

    fprintf(outf,"/q {\n");
    fprintf(outf,"newpath\n");
    fprintf(outf,"moveto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"closepath\n");
    fprintf(outf,"stroke\n");
    fprintf(outf,"} bind def\n");

    fprintf(outf,"/d3 {\n");
    fprintf(outf,"setgray\n");
    fprintf(outf,"newpath\n");
    fprintf(outf,"moveto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"closepath\n");
    fprintf(outf,"stroke\n");
    fprintf(outf,"} bind def\n");

    fprintf(outf,"/f3 {\n");
    fprintf(outf,"setgray\n");
    fprintf(outf,"newpath\n");
    fprintf(outf,"moveto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"closepath\n");
    fprintf(outf,"fill\n");
    fprintf(outf,"} bind def\n");

    fprintf(outf,"/f4 {\n");
    fprintf(outf,"setgray\n");
    fprintf(outf,"newpath\n");
    fprintf(outf,"moveto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"closepath\n");
    fprintf(outf,"fill\n");
    fprintf(outf,"} bind def\n");

    fprintf(outf,"/f5 {\n");
    fprintf(outf,"setgray\n");
    fprintf(outf,"newpath\n");
    fprintf(outf,"moveto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"closepath\n");
    fprintf(outf,"fill\n");
    fprintf(outf,"} bind def\n");

    fprintf(outf,"/rf3 {\n");
    fprintf(outf,"setrgbcolor\n");
    fprintf(outf,"newpath\n");
    fprintf(outf,"moveto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"closepath\n");
    fprintf(outf,"fill\n");
    fprintf(outf,"} bind def\n");

    fprintf(outf,"/dl {\n");
    fprintf(outf,"newpath\n");
    fprintf(outf,"moveto\n");
    fprintf(outf,"lineto\n");
    fprintf(outf,"stroke\n");
    fprintf(outf,"} bind def\n");

    fprintf(outf,"/dist { exch 4 1 roll sub dup mul 3 1 roll sub dup mul\n");
    fprintf(outf,"add } bind def /vdist { 4 index 4 index 11 index 11\n"); 
    fprintf(outf,"index dist } bind def /longrot { vdist 16 6 roll vdist\n"); 
    fprintf(outf,"11 1 roll 10 5 roll 17 -5 roll vdist 17 -1 roll 2 copy\n"); 
    fprintf(outf,"gt { exch 7 -5 roll 12 -5 roll 17 10 roll } if exch pop\n");
    fprintf(outf,"17 -1 roll 2 copy gt { exch 7 -5 roll 12 5 roll } if\n");
    fprintf(outf,"exch pop } bind def /midpoint { { 4 index 10 index add\n");
    fprintf(outf,".5 mul } 11 1 roll 10 index exec 11 index exec 12 index\n");
    fprintf(outf,"exec 13 index exec 15 -1 roll exec } bind def\n");
    fprintf(outf,"/subdivide { 15 -5 roll midpoint 5 copy 25 -5 roll 5\n");
    fprintf(outf,"copy 30 5 roll 25 -5 roll } bind def /trishow { gsave\n");
    fprintf(outf,"15 3 roll moveto 10 3 roll lineto 5 3 roll lineto\n");
    fprintf(outf,"closepath 3 -1 roll 6 -1 roll add 8 -1 roll add 3. div\n");
    fprintf(outf,"3 -1 roll 5 -1 roll add 6 -1 roll add 3. div 5 2 roll\n");
    fprintf(outf,"add add 3. div setrgbcolor fill grestore } bind def\n");
    fprintf(outf,"/subshow { 16 1 roll longrot 16 index gt { subdivide 30\n");
    fprintf(outf,"index subshow 16 -1 roll subshow } { trishow pop }\n");
    fprintf(outf,"ifelse } bind def\n");
}

endps()
{
    fprintf(outf,"grestore\n");
    fprintf(outf,"showpage\n");
}

psoutline(outf,p0,p1,p2)
FILE *outf;
vect *p0, *p1, *p2;
{
    fprintf(outf,"%0.4g %0.4g ",p0->x,p0->y);
    fprintf(outf,"%0.4g %0.4g ",p1->x,p1->y);
    fprintf(outf,"%0.4g %0.4g ",p2->x,p2->y);
    fprintf(outf,"1.0 f3\n");
    fprintf(outf,"%0.4g %0.4g ",p0->x,p0->y);
    fprintf(outf,"%0.4g %0.4g ",p1->x,p1->y);
    fprintf(outf,"%0.4g %0.4g ",p2->x,p2->y);
    fprintf(outf,"0.0 d3\n");
}

pstriangle(outf,p0,p1,p2,tol)
FILE *outf;
vect *p0, *p1, *p2;
float tol;
{
    fprintf(outf,"%g %g ",p0->x,p0->y);
    fprintf(outf,"%g x\n",p0->z);
    fprintf(outf,"%g %g ",p1->x,p1->y);
    fprintf(outf,"%g x\n",p1->z);
    fprintf(outf,"%g %g ",p2->x,p2->y);
    fprintf(outf,"%g x\n",p2->z);
    fprintf(outf,"%g subshow\n",tol*tol);
}

#define TRIGAMMA	(1.0)
#define FLATTOL		(0.00001)

cpstriangle(outf,p0,p1,p2,tol)
FILE *outf;
vect *p0, *p1, *p2;
float tol;
{
    vect *v[3], *temp;
    vect points[20], pos;
    int nlevels, npoints;
    float min, max, shade;
    int i, j, didoutput;

    v[0] = p0;
    v[1] = p1;
    v[2] = p2;
    if(v[0]->z>v[1]->z) {
	temp = v[0];
	v[0] = v[1];
	v[1] = temp;
    }
    if(v[1]->z>v[2]->z) {
	temp = v[1];
	v[1] = v[2];
	v[2] = temp;
    }
    if(v[0]->z>v[1]->z) {
	temp = v[0];
	v[0] = v[1];
	v[1] = temp;
    }
    nlevels = 1.0/tol;
    if(nlevels>256)
	nlevels = 256;
    if(nlevels<2)
	nlevels = 2;
    didoutput = 0;

    max = (float)(1)/nlevels;
    max = pow(max,TRIGAMMA);
    if(v[2]->z<=max) {
	for(j=0; j<3; j++) 
	    fprintf(outf,"%0.4g %0.4g ",v[j]->x,v[j]->y);
	fprintf(outf,"%0.4g f%1d\n",0.0,3);
	return;
    }

    for(i=0; i<nlevels; i++) {
       	min = (float)(i+0)/nlevels;
       	max = (float)(i+1)/nlevels;
       	shade = i/(nlevels-1.0);
	min = pow(min,TRIGAMMA);
	max = pow(max,TRIGAMMA);
	shade = pow(shade,TRIGAMMA);
	npoints = 0;

	for(j=0; j<3; j++) {
	    if(intersect(v,2-j,min,&pos)) {
		points[npoints].x = pos.x;
		points[npoints].y = pos.y;
		npoints++;
	    }
	}

	for(j=0; j<3; j++) {
	    if(v[j]->z>=min && v[j]->z<max) {
		points[npoints].x = v[j]->x;
		points[npoints].y = v[j]->y;
		npoints++;
	    }
	}

	for(j=0; j<3; j++) {
	    if(intersect(v,j,max,&pos)) {
		points[npoints].x = pos.x;
		points[npoints].y = pos.y;
		npoints++;
	    }
	}

	if(npoints>=3 && npoints<=5) {
	    didoutput++;
	    for(j=0; j<npoints; j++) 
		fprintf(outf,"%0.4g %0.4g ",points[j].x,points[j].y);
	    fprintf(outf,"%0.4g f%1d\n",shade,npoints);
	} else if(npoints>0) {
	    fprintf(stderr,"badpoints %d\n",npoints);
	}
    }
    if(!didoutput) {
	fprintf(stderr,"no output for tri\n");
    }
}

static intersect(v,edge,shade,pos) 
vect *v[3];
int edge;
float shade;
vect *pos;
{
    vect *p0, *p1, *temp;
    float p, del;

    p0 = v[edge];
    p1 = v[(edge+1)%3];
    if(p0->z>p1->z) {
	temp = p0;
	p0 = p1;
	p1 = temp;
    }
    if(p1->z<shade)
	return 0;
    if(p0->z>=shade)
	return 0;
    del = (p1->z-p0->z);
    if(del<FLATTOL)
	return 0;
    p = (shade-p0->z)/(p1->z-p0->z);
    pos->x = flerp(p0->x,p1->x,p);
    pos->y = flerp(p0->y,p1->y,p);
    return 1;
}

/*
 *	subdiv stuff follows
 *
 */
#define GAMMA		1.0
#define DELGAMMA	0.5

static FILE *psout;
static float subdivtol;

pssubdivtriangle(outf,p0,p1,p2,c0,c1,c2,tol)
FILE *outf;
vect *p0, *p1, *p2;
vect *c0, *c1, *c2;
float tol;
{
    float x0[5], x1[5], x2[5];

    psout = outf;
    subdivtol = tol;
    x0[0] = p0->x;
    x0[1] = p0->y;
    x0[2] = c0->x;
    x0[3] = c0->y;
    x0[4] = c0->z;
    x1[0] = p1->x;
    x1[1] = p1->y;
    x1[2] = c1->x;
    x1[3] = c1->y;
    x1[4] = c1->z;
    x2[0] = p2->x;
    x2[1] = p2->y;
    x2[2] = c2->x;
    x2[3] = c2->y;
    x2[4] = c2->z;
    subdivtri(x0,x1,x2);
}

static float cdelta(p0,p1)
float *p0, *p1;
{
    float dr, dg, db;

    dr = p0[2] - p1[2];
    if(dr<0)
	dr = -dr;
    dg = p0[3] - p1[3];
    if(dg<0)
	dg = -dg;
    db = p0[4] - p1[4];
    if(db<0)
	db = -db;
    return sqrt(RLUM*dr*dr+GLUM*dg*dg+BLUM*db*db);
}

#define NONE	0x0
#define D0	0x1
#define D1	0x2
#define D2	0x4

static subdivtri(v0,v1,v2)
float *v0, *v1, *v2;
{
    float d0, d1, d2;
    float i0[5];
    float i1[5];
    float i2[5];
    int code;

    d0 = cdelta(v0,v1);
    d1 = cdelta(v1,v2);
    d2 = cdelta(v2,v0);
    code = NONE;
    if(d0>subdivtol)
	code |= D0;
    if(d1>subdivtol)
	code |= D1;
    if(d2>subdivtol)
	code |= D2;
    switch(code) {
	case NONE:
	    outtri(v0,v1,v2);
	    break;
	case D0:
	    plerp(v0,v1,i0);
	    subdivtri(v0,i0,v2);
	    subdivtri(v2,i0,v1);
	    break;
	case D1:
	    plerp(v1,v2,i0);
	    subdivtri(v1,i0,v0);
	    subdivtri(v0,i0,v2);
	    break;
	case D2:
	    plerp(v2,v0,i0);
	    subdivtri(v2,i0,v1);
	    subdivtri(v1,i0,v0);
	    break;
	case D0|D1:
	    plerp(v0,v1,i0);
	    plerp(v1,v2,i1);
	    subdivtri(v0,i0,v2);
	    subdivtri(v2,i0,i1);
	    subdivtri(v1,i1,i0);
	    break;
	case D1|D2:
	    plerp(v1,v2,i0);
	    plerp(v2,v0,i1);
	    subdivtri(v1,i0,v0);
	    subdivtri(v0,i0,i1);
	    subdivtri(v2,i1,i0);
	    break;
	case D2|D0:
	    plerp(v2,v0,i0);
	    plerp(v0,v1,i1);
	    subdivtri(v2,i0,v1);
	    subdivtri(v1,i0,i1);
	    subdivtri(v0,i1,i0);
	    break;
	case D2|D1|D0:
	    plerp(v0,v1,i0);
	    plerp(v1,v2,i1);
	    plerp(v2,v0,i2);
	    subdivtri(v0,i0,i2);
	    subdivtri(v1,i1,i0);
	    subdivtri(v2,i2,i1);
	    subdivtri(i0,i1,i2);
	    break;
    }
}

static plerp(v0,v1,l)
float *v0, *v1, *l;
{
    l[0] = (v0[0]+v1[0])/2.0;
    l[1] = (v0[1]+v1[1])/2.0;
    l[2] = (v0[2]+v1[2])/2.0;
    l[3] = (v0[3]+v1[3])/2.0;
    l[4] = (v0[4]+v1[4])/2.0;
}

static outtri(v0,v1,v2)
float *v0, *v1, *v2;
{
    float ar, ag, ab;

    fprintf(psout,"%g %g ",v0[0],v0[1]);
    fprintf(psout,"%g %g ",v1[0],v1[1]);
    fprintf(psout,"%g %g ",v2[0],v2[1]);
    ar = (v0[2]+v1[2]+v2[2])/3.0;
    ag = (v0[3]+v1[3]+v2[3])/3.0;
    ab = (v0[4]+v1[4]+v2[4])/3.0;
    fprintf(psout,"%g %g %g rf3\n",ar,ag,ab);
}

pslinewidth(outf,width)
FILE *outf;
float width;
{
     fprintf(outf,"%g s\n",width);
}

psline(outf,v0,v1)
FILE *outf;
float *v0, *v1;
{
     fprintf(outf,"%g %g %g %g dl\n",v0[0],v0[1],v1[0],v1[1]);
}

psgrey(g)
float g;
{
    fprintf(outf,"%g setgray\n",g);
}

psnewpath()
{
    fprintf(outf,"newpath\n");
}

psmoveto(x,y)
float x, y;
{
    fprintf(outf,"%f %f moveto\n",x,y);
}

pslineto(x,y)
float x, y;
{
    fprintf(outf,"%f %f lineto\n",x,y);
}

psclosepath()
{
    fprintf(outf,"closepath\n");
}

psfill()
{
    fprintf(outf,"fill\n");
}
