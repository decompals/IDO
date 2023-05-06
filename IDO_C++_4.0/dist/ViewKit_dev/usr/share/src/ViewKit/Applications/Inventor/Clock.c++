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
/////////////////////////////////////////////////////
// Clock.c++
///////////////////////////////////////////////////////
#include <stdio.h>
#include "Clock.h"
#include <math.h>

#include <Xm/Form.h>
#include <Inventor/SoDB.h>
#include <Inventor/Xt/SoXt.h>
#include <Inventor/Xt/viewers/SoXtExaminerViewer.h>
#include <Inventor/engines/SoTimeCounter.h>
#include <Inventor/engines/SoCounter.h>
#include <Inventor/engines/SoCalculator.h>
#include <Inventor/nodes/SoDirectionalLight.h>
#include <Inventor/nodes/SoGroup.h>
#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/nodes/SoPerspectiveCamera.h>
#include <Inventor/nodes/SoRotationXYZ.h>

#define FILENAME "clockData.iv"

// update rate in seconds (1.0 = every second)
#define EVERY_SECOND 	  1.0
#define EVERY_MINUTE 	  60.0

class ClockHands {
  public:
    SoRotationXYZ   *hour;
    SoRotationXYZ   *minute;
    SoRotationXYZ   *second;
};


Clock::Clock(const char *name, Widget parent) : VkComponent(name)
{

    _baseWidget = XmCreateForm(parent, _name, NULL, 0);

    // read in the scene graph
    
    SoInput in;
    
    if (!in.openFile(FILENAME))
    {
	fprintf(stderr, "Error - could not open %s.\n", FILENAME);
	exit(1);
    }
    
    SoSeparator *root = SoDB::readAll(&in);
    
    if (root == NULL)
    {
	fprintf(stderr, "Error - could not read %s.\n", FILENAME);
	exit(1);
    }
    
    root->ref();
  
    if (! setupHands(root))
    {
	fprintf(stderr, "Error - cannot use %s for the clock.\n", 
		FILENAME);
	exit(1);
    }
  
  // Build and initialize the scenario render area widget
    
    SoXtExaminerViewer *examiner = new SoXtExaminerViewer(_baseWidget);
    examiner->setSize(SbVec2s(100, 100));
    examiner->setDecoration(FALSE);
    examiner->setFeedbackVisibility(FALSE);
    examiner->setSceneGraph(root);
    examiner->setTitle("Inventor Time");

    XtVaSetValues(examiner->getWidget(), 
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);

  examiner->show();
}

Clock::~Clock()
{
    // Empty
}

const char *Clock::className()
{
  return "Clock";
}


SbBool Clock::setupHands(SoGroup *root)
{
    SbBool foundSecond = FALSE;
    int currentHour, currentMinute, currentSecond;

    // Engines to time the ticks of the clock hands
    SoTimeCounter *minuteTimer, *secondTimer;
    SoCounter *minuteCounter, *hourCounter;

    // Engines to convert the clock ticks into radian angles of rotation
    SoCalculator *secondRadians = new SoCalculator; 
    SoCalculator *minuteRadians = new SoCalculator; 
    SoCalculator *hourRadians = new SoCalculator; 

    // Rotation of the hands
    ClockHands	*rotation = new ClockHands;
    rotation->hour = NULL;
    rotation->minute = NULL;
    rotation->second = NULL;

    // Get current time of day to set the initial time
    getCurrentTime(currentHour, currentMinute, currentSecond);
    
    // Search for Names which denote the hands of the clock,
    // then insert these rotations into the clock geometry and
    // attach engines to update the rotations.
    // Also, since the hands will be constantly moving, turn 
    // render caching off for these separators. 

    SoSeparator *s;

    // Second hand (optional)
    s = (SoSeparator *)root->getByName("SecondHand");
    if (s != NULL && s->isOfType(SoSeparator::getClassTypeId()))
    {

	rotation->second = new SoRotationXYZ;
	rotation->second->axis = SoRotationXYZ::Z;
	s->insertChild(rotation->second, 0);

        // Second hand ticks from 0 to 59 in one minute (60 seconds)
	secondTimer = new SoTimeCounter;
        secondTimer->min = 0;
        secondTimer->max = 59;
        secondTimer->frequency = 1./60.;
        secondTimer->reset = currentSecond;
        secondRadians->a.connectFrom(&secondTimer->output);
        secondRadians->expression = "oa=-a*M_PI/30.0";
        rotation->second->angle.connectFrom(&secondRadians->oa);

	s->renderCaching = SoSeparator::OFF;
	foundSecond = TRUE;
    }

    // Hour hand 
    s = (SoSeparator *)root->getByName("HourHand");
    if (s != NULL && s->isOfType(SoSeparator::getClassTypeId()))
    {
	s->renderCaching = SoSeparator::OFF;
	rotation->hour = new SoRotationXYZ;
	rotation->hour->axis = SoRotationXYZ::Z;
	s->insertChild(rotation->hour, 0);

	hourCounter = new SoCounter;
	hourCounter->min = 0;
	hourCounter->max = 11;
        hourCounter->reset = currentHour;
        hourRadians->a.connectFrom(&hourCounter->output);
        hourRadians->expression = "oa=-((a+b/60.)*M_PI/6.0)";
        rotation->hour->angle.connectFrom(&hourRadians->oa);
    }
    else
    {
	return (FALSE); // The hour hand is required
    }

    // Minute hand 
    s = (SoSeparator *)root->getByName("MinuteHand");
    if (s != NULL && s->isOfType(SoSeparator::getClassTypeId()))
    {
	s->renderCaching = SoSeparator::OFF;
	rotation->minute = new SoRotationXYZ;
	rotation->minute->axis = SoRotationXYZ::Z;
	s->insertChild(rotation->minute, 0);

 	// If the clock has a second hand:
	// use the second timer to figure out when to tick the minutes
  	if (foundSecond)
	{
	    minuteCounter = new SoCounter;
	    minuteCounter->min = 0;
	    minuteCounter->max = 59;
            minuteCounter->reset = currentMinute;
	    minuteCounter->trigger.connectFrom(&secondTimer->syncOut);
            minuteRadians->a.connectFrom(&minuteCounter->output);

	    hourCounter->trigger.connectFrom(&minuteCounter->syncOut);
            hourRadians->b.connectFrom(&minuteCounter->output);
	} 
	// If the clock doesn't have a second hand:
        // use a minute timer
	else
	{
	    minuteTimer = new SoTimeCounter;
            minuteTimer->min = 0;
            minuteTimer->max = 59;
            minuteTimer->frequency = 1./3600.;
            minuteTimer->reset = currentMinute;
            minuteRadians->a.connectFrom(&minuteTimer->output);

	    hourCounter->trigger.connectFrom(&minuteTimer->syncOut);
            hourRadians->b.connectFrom(&minuteTimer->output);
	}

        minuteRadians->expression = "oa=-a*M_PI/30.0";
        rotation->minute->angle.connectFrom(&minuteRadians->oa);
    }
    else
    {
	return (FALSE);  // The minute hand is required
    }

    return (TRUE);
}


void Clock::getCurrentTime( int &h, int &m, int &s )
{
    long clock;
    struct tm *timeofday;

    time(&clock);
    timeofday = (struct tm *)localtime(&clock);
    h = timeofday->tm_hour;
    if (h >= 12) h -= 12;
    m = timeofday->tm_min;
    s = timeofday->tm_sec;
}
