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


/////////////////////////////////////////////////////////////////////
// pie.c++
////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Xm/RowColumn.h>
#include <Vk/VkPie.h>
#include <Vk/VkPeriodic.h>
#include <unistd.h>
#include <math.h>
#include <stdio.h>

class PieDriver : public VkPeriodic {
    
protected:
    
    VkPie *_pie;
    
    void tick();
    
public:
    
    PieDriver(int, VkPie*);
    ~PieDriver();
};

PieDriver::PieDriver(int interval, VkPie *pie) : VkPeriodic(interval)
{
    srand48( (long) getpid() );
    
    _pie = pie;
}

PieDriver::~PieDriver()
{
    // Empty
}

void PieDriver::tick()
{
    _pie->reset(100);
    _pie->add ((int) (10.0 * drand48()), "red");
    _pie->add( (int)(20.0 * drand48()), "green");
    _pie->add( (int) (30.0 * drand48()), "blue");
    _pie->add( (int) (40.0 * drand48()), "orange");	 
    
    _pie->update( );
}

class PieWindow: public VkSimpleWindow {
    
    
public:
    
    PieWindow ( const char *name );
    ~PieWindow ();
    virtual const char* className();
};


PieWindow::~PieWindow ()
{
    
}

const char* PieWindow::className() { return "PieWindow"; }

PieWindow::PieWindow(const char *name) : VkSimpleWindow(name)
{
    VkPie *pie = new VkPie("pie", (*this));

    PieDriver *driver = new PieDriver(100, pie);

    pie->setRadius(300);
    pie->reset(100);
    pie->add ((int) (10.0 ), 20, "red");
    pie->add( (int) (20.0),  30, "green");
    pie->add( (int) (30.0 ), 10, "blue");
    pie->add( (int) (40.0),  10, "orange");	 
    
    pie->update( );
    
    pie->show();
//    driver->start();
    
    addView( pie);
}


void main ( int argc, char **argv )
{
    VkApp        *pieApp = new VkApp("PieApp", &argc, argv);
    PieWindow  *pieWin = new PieWindow("pie");
    pieWin->show();
    pieApp->run();
}





