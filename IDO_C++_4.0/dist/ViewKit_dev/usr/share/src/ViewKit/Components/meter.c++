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
// meter.c++
////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Xm/RowColumn.h>
#include <Vk/VkMeter.h>
#include <Vk/VkPeriodic.h>
#include <unistd.h>
#include <math.h>
#include <stdio.h>

class MeterDriver : public VkPeriodic {
    
protected:
    
    VkMeter *_meter;
    
    void tick();
    
public:
    
    MeterDriver(int, VkMeter*);
    ~MeterDriver();
};

MeterDriver::MeterDriver(int interval, VkMeter *meter) : VkPeriodic(interval)
{
    srand48( (long) getpid() );
    
    _meter = meter;
}

MeterDriver::~MeterDriver()
{
    // Empty
}

void MeterDriver::tick()
{
    _meter->reset(100);
    _meter->add ((int) (10.0 * drand48()), "red");
    _meter->add( (int)(20.0 * drand48()), "green");
    _meter->add( (int) (30.0 * drand48()), "blue");
    _meter->add( (int) (40.0 * drand48()), "orange");	 
    
    _meter->update( );
}

class MeterWindow: public VkSimpleWindow {
    
    
public:
    
    MeterWindow ( const char *name );
    ~MeterWindow ();
    virtual const char* className();
};


MeterWindow::~MeterWindow ()
{
    
}

const char* MeterWindow::className() { return "MeterWindow"; }

MeterWindow::MeterWindow(const char *name) : VkSimpleWindow(name)
{
    VkMeter *meter = new VkMeter("meter", (*this));

    MeterDriver * driver = new MeterDriver(100, meter);

    meter->reset(100, 10);
    meter->add ((int) (10.0 ), 20, "red"  );
    meter->add( (int) (20.0),  20, "green" );
    meter->add( (int) (30.0 ), 20, "blue" );
    meter->add( (int) (40.0),  20, "orange");	 
    
    meter->update( );
    
    meter->show();
//    driver->start();
    
    addView( meter);
}


void main ( int argc, char **argv )
{
    VkApp        *meterApp = new VkApp("MeterApp", &argc, argv);
    
    MeterWindow  *meterWin = new MeterWindow("meter");
    meterWin->show();
    meterApp->run();
}





