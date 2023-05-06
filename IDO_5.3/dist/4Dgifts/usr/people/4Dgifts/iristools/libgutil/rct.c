/*
 *	rct -
 *		Management of rectangles.
 *
 *				Paul Haeberli - 1986
 */
#include "rct.h"
#include "port.h"

rct *rctnew()
{
    rct *r; 

    r = (rct *)malloc(sizeof(rct));
    r->xmin = 0;
    r->ymin = 0;
    r->xmax = 0;
    r->ymax = 0;
    return r;
}

rctfree(r)
rct *r;
{
    free(r);
}

rct *rctclone(r)
rct *r;
{
     rct *c; 

     c = rctnew();
     *c = *r;
     return c;
}

rctcopy(scr,dst)
rct *scr, *dst;
{
     *dst = *scr;
}

rctset(r,x1,y1,x2,y2) 
rct *r; 
int x1, y1, x2, y2;
{
    r->xmin = x1;
    r->ymin = y1;
    r->xmax = x2;
    r->ymax = y2;
    rctnormal(r);
}

rctsize(r,ox,oy,sizex,sizey) 
rct *r; 
int ox, oy;
int sizex, sizey;
{
    r->xmin = ox;
    r->ymin = oy;
    r->xmax = ox+sizex;
    r->ymax = oy+sizey;
    rctnormal(r);
}

rctnormal(r) 
rct *r; 
{
    int temp;

    if(r->xmin > r->xmax) {
	temp = r->xmin;
	r->xmin = r->xmax;
	r->xmax = temp;
    }
    if(r->ymin > r->ymax) {
	temp = r->ymin;
	r->ymin = r->ymax;
	r->ymax = temp;
    }
}

rctoffset(r,dx,dy) 
rct *r; 
int dx, dy;
{
    r->xmin += dx;
    r->ymin += dy;
    r->xmax += dx;
    r->ymax += dy;
}

rctshrink(r,dx,dy)
rct *r; 
int dx, dy;
{
    r->xmin += dx;
    r->ymin += dy;
    r->xmax -= dx;
    r->ymax -= dy;
    rctnormal(r);
}

int rctinter(src1,src2,dest)
rct *src1, *src2, *dest;
{
    int xmin, xmax;
    int ymin, ymax;

    xmin = MAX(src1->xmin,src2->xmin);
    xmax = MIN(src1->xmax,src2->xmax);
    ymin = MAX(src1->ymin,src2->ymin);
    ymax = MIN(src1->ymax,src2->ymax);
    if(xmax>=xmin && ymax>=ymin) {
	dest->xmin = xmin;
	dest->xmax = xmax;
	dest->ymin = ymin;
	dest->ymax = ymax;
	return 1;
    } else {
	dest->xmin = 0;
	dest->xmax = 0;
	dest->ymin = 0;
	dest->ymax = 0;
	return 0;
    }
}

rctunion(src1,src2,dest)
rct *src1, *src2, *dest;
{
    int xmin, xmax;
    int ymin, ymax;

    xmin = MIN(src1->xmin,src2->xmin);
    xmax = MAX(src1->xmax,src2->xmax);
    ymin = MIN(src1->ymin,src2->ymin);
    ymax = MAX(src1->ymax,src2->ymax);
    dest->xmin = xmin;
    dest->xmax = xmax;
    dest->ymin = ymin;
    dest->ymax = ymax;
}

rctinside(r,x,y)
rct *r;
int x, y;
{
    if ( x >= r->xmin && x <= r->xmax &&
	 	y >= r->ymin && y <= r->ymax)
	return 1;
    else
	return 0;
}

rctfinside(r,x,y)
rct *r;
float x, y;
{
    if ( x >= r->xmin && x <= r->xmax &&
	 	y >= r->ymin && y <= r->ymax)
	return 1;
    else
	return 0;
}

rctequal(r1,r2)
rct *r1, *r2;
{
    if ( r1->xmin == r2->xmin && r1->xmax == r2->xmax &&
       		r1->ymin == r2->ymin && r1->ymax == r2->ymax )
	return 1;
    else
	return 0;
}

rctempty(r) 
rct *r;
{
    if(r->xmin == r->xmax || r->ymin == r->ymax)
	return 1;
    else
	return 0;
}

rctdraw(r)
rct *r;
{
    drawrect((float)r->xmin,(float)r->ymin,(float)r->xmax,(float)r->ymax);
}

rctfill(r)
rct *r;
{
    fillrect((float)r->xmin,(float)r->ymin,(float)r->xmax,(float)r->ymax);
}

rctcenter(r,xcent,ycent)
rct *r;
int *xcent, *ycent;
{
    *xcent = (r->xmin+r->xmax)/2;
    *ycent = (r->ymin+r->ymax)/2;
}

rctprint(r)
rct *r;
{
    fprintf(stderr,"xy min %d % xy max: %d %d\n",
	       r->xmin,r->ymin,r->xmax,r->ymax);
}

rctscale(r,scale)
rct *r;
float scale;
{
    r->xmin = r->xmin*scale;
    r->ymin = r->ymin*scale;
    r->xmax = r->xmax*scale;
    r->ymax = r->ymax*scale;
}

rctrdraw(r,iradius)
rct *r;
int iradius;
{
    float delx, dely;
    float partx, party;
    float radius;

    radius = iradius;
    if(radius<0)
	radius = 0;
    delx = r->xmax-r->xmin;
    dely = r->ymax-r->ymin;
    if(radius > delx)
	radius = delx;
    if(radius > dely)
	radius = dely;
    partx = delx - radius;
    party = dely - radius;
    pushmatrix();
      scale(0.5,0.5,1.0);
      translate((float)r->xmin+r->xmax,(float)r->ymin+r->ymax,0.0);
      pushmatrix();
        translate(-partx,-party,0.0);
        scale(radius,radius,1.0);
	arc(0.0,0.0,1.0,1800,2700);
      popmatrix();
      move2(-partx,-dely);
      draw2( partx,-dely);
      pushmatrix();
        translate( partx,-party,0.0);
        scale(radius,radius,1.0);
	arc(0.0,0.0,1.0,2700,3600);
      popmatrix();
      move2( delx,-party);
      draw2( delx, party);
      pushmatrix();
        translate( partx, party,0.0);
        scale(radius,radius,1.0);
	arc(0.0,0.0,1.0,0,900);
      popmatrix();
      move2( partx, dely);
      draw2(-partx, dely);
      pushmatrix();
        translate(-partx, party,0.0);
        scale(radius,radius,1.0);
	arc(0.0,0.0,1.0,900,1800);
      popmatrix();
      move2(-delx, party);
      draw2(-delx,-party);
    popmatrix();
}

rctrfill(rr,iradius)
rct *rr;
int iradius;
{
    int delx, dely;
    float partx, party;
    float radius;
    rct *r, *fr;

    if(iradius<0)
	iradius = 0;
    r = rctclone(rr);
    delx = r->xmax-r->xmin;
    dely = r->ymax-r->ymin;
    if(iradius > delx)
	iradius = delx;
    if(iradius > dely)
	iradius = dely;
    partx = delx - iradius;
    party = dely - iradius;
    radius = iradius - 0.4;
    pushmatrix();
      scale(0.5,0.5,1.0);
      translate((float)r->xmin+r->xmax,(float)r->ymin+r->ymax,0.0);

      fr = rctnew();
      rctset(fr,(int)-partx,-dely,(int)partx,dely);
      rctfill(fr);
      rctset(fr,-delx,(int)-party,delx,(int)party);
      rctfill(fr);
      rctfree(fr);

      pushmatrix();
        translate(-partx,-party,0.0);
        scale(radius,radius,1.0);
	arcf(0.0,0.0,1.0,1800,2700);
      popmatrix();
      pushmatrix();
        translate(partx,-party,0.0);
        scale(radius,radius,1.0);
	arcf(0.0,0.0,1.0,2700,3600);
      popmatrix();
      pushmatrix();
        translate(partx,party,0.0);
        scale(radius,radius,1.0);
	arcf(0.0,0.0,1.0,0,900);
      popmatrix();
      pushmatrix();
        translate(-partx,party,0.0);
        scale(radius,radius,1.0);
	arcf(0.0,0.0,1.0,900,1800);
      popmatrix();
    popmatrix();
    rctfree(r);
}
