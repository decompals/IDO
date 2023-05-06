////////////////////////////////////////////////////////////////////////////////
///////   Copyright 1992, Silicon Graphics, Inc.  All Rights Reserved.   ///////
//                                                                            //
// This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;     //
// the contents of this file may not be disclosed to third parties, copied    //
// or duplicated in any form, in whole or in part, without the prior written  //
// permission of Silicon Graphics, Inc.                                       //
//                                                                            //
// RESTRICTED RIGHTS LEGEND:                                                  //
// Use,duplication or disclosure by the Government is subject to restrictions //
// as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data     //
// and Computer Software clause at DFARS 252.227-7013, and/or in similar or   //
// successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -    //
// rights reserved under the Copyright Laws of the United States.             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
// Box.c++
///////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "Box.h"
#include "copycmap.h"

#define TIMER 20

// information for setting up colormap

static Colorindex red, green, yellow, blue, magenta, cyan;
static Colorindex background;

// The following structure is declared in copycmap.h and is used by
// CopyGlColormap (in this demo directory) to set up a clormap
// with the specified colors.  Note that while we are using GLXC_NAMED
// to look up named colors, we could also use GLXC_ABSOLUTE and specify
// hardcoded pixel numbers 1-6 to copy from the X colormap.

static struct glxcColorInfo colorInfo[] =
{
    { GLXC_NAMED, (caddr_t)"red", &red},
    { GLXC_NAMED, (caddr_t)"green", &green},
    { GLXC_NAMED, (caddr_t)"yellow", &yellow},
    { GLXC_NAMED, (caddr_t)"blue", &blue},
    { GLXC_NAMED, (caddr_t)"magenta", &magenta},
    { GLXC_NAMED, (caddr_t)"cyan", &cyan},
    { GLXC_RESOURCE, (caddr_t)XmNbackground, &background},
};

Box::Box(const char *name, Widget parent) : VkGLX(name, parent)
{
  _rotX.obj = _rotY.obj = _rotZ.obj = this;
  _rotX.id = _rotY.id = _rotZ.id = NULL;
  _rotX.axis = 'x';
  _rotY.axis = 'y';
  _rotZ.axis = 'z';
}

Box::~Box()
{
    // Empty
}


void Box::ginit(GlxDrawCallbackStruct *)
{
    CopyGlColormap(_baseWidget, colorInfo, XtNumber(colorInfo));
    initialize_gl();
}

void Box::expose(GlxDrawCallbackStruct *)
{
    viewcube('t');
}

void Box::resize(GlxDrawCallbackStruct *cb)
{
    viewport(0, (Screencoord) cb->width-1,
	     0, (Screencoord) cb->height-1);
    viewcube('t');
}

void Box::input(GlxDrawCallbackStruct *cb)
{
    if (cb->event->type == ButtonPress)
    {
	if (cb->event->xbutton.button == Button3)
	    _rotZ.id = XtAppAddTimeOut(XtWidgetToApplicationContext(_baseWidget),
				      TIMER, &Box::rotateCallback, (XtPointer) &_rotZ);
	else if (cb->event->xbutton.button == Button2)
	    _rotY.id = XtAppAddTimeOut(XtWidgetToApplicationContext(_baseWidget),
				      TIMER, &Box::rotateCallback, (XtPointer) &_rotY);
	else if (cb->event->xbutton.button == Button1)
	    _rotX.id = XtAppAddTimeOut(XtWidgetToApplicationContext(_baseWidget),
				      TIMER, &Box::rotateCallback, (XtPointer) &_rotX);
    }
    else if (cb->event->type == ButtonRelease)
    {
	if (cb->event->xbutton.button == Button3)
	{
	    if (_rotZ.id)
	    {
		XtRemoveTimeOut(_rotZ.id);
		_rotZ.id = NULL;
	    }
	}
	else if (cb->event->xbutton.button == Button2)
	{
	    if (_rotY.id)
	    {
		XtRemoveTimeOut(_rotY.id);
		_rotY.id = NULL;
	    }
	}
	else if (cb->event->xbutton.button == Button1)
	{
	    if (_rotX.id)
	    {
		XtRemoveTimeOut(_rotX.id);
		_rotX.id = NULL;
	    }
	}
    }
}

//////////////////////////////////////////////////////////////////////
// Taken from ~4Dgifts/examples/GLX/glxwidget/demos/mscrn_rotate.c 
//////////////////////////////////////////////////////////////////////

static Coord ident [4][4] =     { 1.0, 0.0, 0.0, 0.0,   
				  0.0, 1.0, 0.0, 0.0,
				  0.0, 0.0, 1.0, 0.0,
				  0.0, 0.0, 0.0, 1.0};

static Coord cm [4][4] = { 1.0, 0.0, 0.0, 0.0,    // cumulative matrix
                           0.0, 1.0, 0.0, 0.0,
                           0.0, 0.0, 1.0, 0.0,
                           0.0, 0.0, 0.0, 1.0};

//  Define the sides of the cube in world coordinates.

static Coord pfrnt[4][3] = {{    0.0,    0.0,    0.0},
                            {  100.0,    0.0,    0.0},
                            {  100.0,  100.0,    0.0},
                            {    0.0,  100.0,    0.0}};
    
static Coord pback[4][3] = {{    0.0,    0.0, -100.0},
                            {    0.0,  100.0, -100.0},
                            {  100.0,  100.0, -100.0},
                            {  100.0,    0.0, -100.0}};
    
static Coord ptop[4][3] =  {{    0.0,  100.0,    0.0},
                            {  100.0,  100.0,    0.0},
                            {  100.0,  100.0, -100.0},
                            {    0.0,  100.0, -100.0}};
    
static Coord pbot[4][3] =  {{    0.0,    0.0,    0.0},
                            {    0.0,    0.0, -100.0},
                            {  100.0,    0.0, -100.0},
                            {  100.0,    0.0,    0.0}};
    
static Coord prsid[4][3] = {{  100.0,    0.0,    0.0},
                            {  100.0,    0.0, -100.0},
                            {  100.0,  100.0, -100.0},
                            {  100.0,  100.0,    0.0}};
    
static Coord plsid[4][3] = {{    0.0,    0.0,    0.0},
                            {    0.0,  100.0,    0.0},
                            {    0.0,  100.0, -100.0},
                            {    0.0,    0.0, -100.0}};

void Box::initialize_gl() {

    float scrnaspect;            // aspect ratio value
    long xscrnsize;              // size of screen in x used to set globals 

    xscrnsize = getgdesc(GD_XPMAX);          // get/set screen size[/aspect] 
    if (xscrnsize == 1280)
    {
        keepaspect(5, 4);
	scrnaspect = 1.25;
    }
    else if (xscrnsize == 1023)
    {
        keepaspect(4, 3);
	scrnaspect = 1.34;
    }
    else
    {
	fprintf(stderr, "Something's EXTREMELY wrong:  ");
	fprintf(stderr, "xscrnsize=%d\n", xscrnsize);
        exit(-1) ;
    }

//  initialize and set the display to double buffer mode

    doublebuffer();
    writemask((unsigned short) (1<<getplanes())-1);

#ifdef BACKFACE    // compile with "-DBACKFACE" if you desire to use GL's 
    backface(TRUE);
#endif

    perspective(470,scrnaspect,1.0,10000.0); 

//  initialize the modeling transformation values         
    _rx =     0;  _ry =     0;  _rz =      0;
     _x = -50.0;   _y = -50.0;   _z = -400.0;
}

void Box::viewcube(char axis) 
{

//  transform the cube in world space and (if BACKFACE not defined,
//   in software, ) check each face for back face elimination

    color(background);
    clear();
    gflush();

    pushmatrix();
    translate(_x, _y, _z);
    pushmatrix();
    translate(50.0,50.0,-50.0);

// apply rotation about a single axis
    
    switch(axis) {
        case('x'): 
            rotate(_rx,'x');
            break;
        case('y'): 
            rotate(_ry,'y');
            break;
        case('z'): 
            rotate(_rz,'z');
            break;
        case('t'): 
            break;
    }

// apply all prior rotations
    
    multmatrix(cm);
    translate(-50.0,-50.0,50.0);

#ifdef BACKFACE    // compile with "-DBACKFACE" if you desire to use GL's 
    color(red);
    polf(4,pfrnt);
    color(green);
    polf(4,pback);
    color(yellow);
    polf(4,ptop);
    color(blue);
    polf(4,pbot);
    color(magenta);
    polf(4,prsid);
    color(cyan);
    polf(4,plsid);
#else
    color(red);
    if(norm_dot(pfrnt) >= 0.0) polf(4,pfrnt);
    color(green);
    if(norm_dot(pback) >= 0.0) polf(4,pback);
    color(yellow);
    if(norm_dot(ptop) >= 0.0) polf(4,ptop);
    color(blue);
    if(norm_dot(pbot) >= 0.0) polf(4,pbot);
    color(magenta);
    if(norm_dot(prsid) >= 0.0) polf(4,prsid);
    color(cyan);
    if(norm_dot(plsid) >= 0.0) polf(4,plsid);
#endif

    popmatrix(); 
    popmatrix();
    swapbuffers();
    gflush();
}


//  Function to postmultiply cumulative rotations
// by rotation about a single axis 

void Box::updatemat(char axis) 
{
    pushmatrix();
    loadmatrix(ident);

    switch (axis) {
        case ('x'): 
            rotate(_rx,'x');
            break;
        case ('y'): 
            rotate(_ry,'y');
            break;
        case ('z'): 
            rotate(_rz,'z');
            break;
    }
    
    multmatrix(cm);
    getmatrix(cm);
    popmatrix();

}

//  This function takes as input an array of points in homogeneous coord.
//    which make up a surface or plane.  The unit normal of the surface and
//    the eyepoint to surface unit vector are computed and the dot product
//    is calculated. norm_dot returns the dot product floating point value
//    and the transformed points for the surface.


float Box::norm_dot(Coord passpoly[][3])
{
    int i;
    float a[3],b[3],c[3],d,abs;

    Coord postrans [4][3];


//  apply the current transformation to the surface points
    
    transform(4,passpoly,postrans);

//  determine two vectors which lie in the specified plane.  The first three
//    points are taken from the surface array.  p1, p2, p3 are ordered by the 
//    right-hand rule in the right-hand coordinate system:  i.e. points ordered 
//    counter-clockwise when on the positive side of the plane or surface are 
//    visible--not backfacing--surfaces.
//    a[] gets the xyz coords of row 2
//    b[] gets the xyz coords of row 0.

// 1]  determine two vectors.  note this routine assumes they are not in-line

    for(i = 0; i < 3; i++)
        a[i] = postrans[2][i] - postrans[1][i];

    for(i = 0; i < 3; i++)
        b[i] = postrans[0][i] - postrans[1][i];

// 2]  find the cross product of the two vectors 

    c[0] = a[1] * b[2] - a[2] * b[1];
    c[1] = a[2] * b[0] - a[0] * b[2];
    c[2] = a[0] * b[1] - a[1] * b[0];


// 3]  calculate the unit normal vector for the plane or poly using the square
//    root of the sum of the squares of x, y, and z to determine length of vec-
//    tor, then dividing each axis by that length (x/l, y/l, z/l).

    abs = 0.0;
    
    for (i = 0; i < 3; i++) 
        abs += (c[i]*c[i]);
    d = sqrt(abs);

    if (fabs(d) > 0.000001)    {
        for (i = 0; i < 3; i++)
            a[i] = c[i]/d;

// 4] calculate the unit vector pointing from the eyepoint to the normal of 
//    the plane or poly

        abs = 0.0;

        for (i = 0; i < 3; i++) 
            c[i] = postrans[1][i];
        for (i = 0; i < 3; i++) 
            abs = abs + (c[i]*c[i]);
        d = sqrt(abs); 
    
        if (fabs(d) > 0.000001)    {
            for (i = 0; i < 3; i++)
                b[i] = c[i]/d;

// 5] return the dot product between the eye vector and the plane normal
	    
            for (i = 0, d=0.0; i < 3; i++)
                d = d + a[i]*b[i];
        }
        else 
             printf("\n\n Magnitude of surface vector is zero!");
    }
    else 
        printf("\n\n Magnitude of eye vector is zero!");
    return(d);
}


// the transform function.  this demonstrates what would be the equivalent of
//   using feedback with xfpt() on the 2000/3000 series IRIS machines: 
//   i.e.  simply multiply each vertex point with the current transformtion
//   matrix without any clipping, scaling, etc. to derive transformed world
//   coordinate values.


void Box::transform(long, Coord passpoly[][3], Coord postrans[][3])
{
    Matrix ctm;

    pushmatrix();
    getmatrix(ctm);

    postrans[0][0] = passpoly[0][0]*ctm[0][0] + 
                     passpoly[0][1]*ctm[1][0] +
                     passpoly[0][2]*ctm[2][0] + ctm[3][0];
    postrans[0][1] = passpoly[0][0]*ctm[0][1] + 
                     passpoly[0][1]*ctm[1][1] +
                     passpoly[0][2]*ctm[2][1] + ctm[3][1];
    postrans[0][2] = passpoly[0][0]*ctm[0][2] +
                     passpoly[0][1]*ctm[1][2] +
                     passpoly[0][2]*ctm[2][2] + ctm[3][2];
          
    postrans[1][0] = passpoly[1][0]*ctm[0][0] + 
                     passpoly[1][1]*ctm[1][0] +
                     passpoly[1][2]*ctm[2][0] + ctm[3][0];
    postrans[1][1] = passpoly[1][0]*ctm[0][1] + 
                     passpoly[1][1]*ctm[1][1] +
                     passpoly[1][2]*ctm[2][1] + ctm[3][1];
    postrans[1][2] = passpoly[1][0]*ctm[0][2] +
                     passpoly[1][1]*ctm[1][2] +
                     passpoly[1][2]*ctm[2][2] + ctm[3][2];
                     
    postrans[2][0] = passpoly[2][0]*ctm[0][0] + 
                     passpoly[2][1]*ctm[1][0] +
                     passpoly[2][2]*ctm[2][0] + ctm[3][0];
    postrans[2][1] = passpoly[2][0]*ctm[0][1] + 
                     passpoly[2][1]*ctm[1][1] +
                     passpoly[2][2]*ctm[2][1] + ctm[3][1];
    postrans[2][2] = passpoly[2][0]*ctm[0][2] +
                     passpoly[2][1]*ctm[1][2] +
                     passpoly[2][2]*ctm[2][2] + ctm[3][2];
                     
    postrans[3][0] = passpoly[3][0]*ctm[0][0] + 
                     passpoly[3][1]*ctm[1][0] +
                     passpoly[3][2]*ctm[2][0] + ctm[3][0];
    postrans[3][1] = passpoly[3][0]*ctm[0][1] + 
                     passpoly[3][1]*ctm[1][1] +
                     passpoly[3][2]*ctm[2][1] + ctm[3][1];
    postrans[3][2] = passpoly[3][0]*ctm[0][2] +
                     passpoly[3][1]*ctm[1][2] +
                     passpoly[3][2]*ctm[2][2] + ctm[3][2];
                     
    popmatrix();
}

void Box::resetAction()
{
    int i, j;

    _x  =  -50.0;           
    _y  =  -50.0;                 
    _z  = -400.0;            
    _rx = 0;                      
    _ry = 0; 
    _rz = 0;

    for(i=0;i<4;i++)
    {
	for(j=0;j<4;j++)
	    cm[i][j] = ident[i][j];
    }
    
    viewcube('t');
}

void Box::translateAction(char axis, float amount)
{
    register Coord *cp;
    
	switch (axis)
	{
	case 'x':
	    cp = &_x;
	    break;
	case 'y':
	    cp = &_y;
	    break;
	case 'z':
	    cp = &_z;
	    break;
	default:
	    XtWarning ("Unknown axis for translate translation");
	    return;
	}
        *cp += amount;
	viewcube('t');
}

void Box::rotateAction(char axis, int amount)
{
    register Angle *ap;

    switch (axis)
    {
    case 'x':
	ap = &_rx;
	break;
    case 'y':
	ap = &_ry;
	break;
    case 'z':
	ap = &_rz;
	break;
    default:
	XtWarning ("Unknown axis for rotate translation");
	return;
    }

    *ap += amount;
    viewcube(axis);
    updatemat(axis);       // incorporate this rotation into
    *ap = 0;              // cumulative rotation matrix
}


void Box::rotateCallback(XtPointer clientData, XtIntervalId *)
{
    RotateInfo *info = (RotateInfo *) clientData;

    info->obj->rotateAction(info->axis, 100);
    info->id = XtAppAddTimeOut(XtWidgetToApplicationContext(info->obj->baseWidget()),
			       TIMER, &Box::rotateCallback, (XtPointer) clientData);
}



