/*

      mainWindow.c++


*/


#include <stdio.h>
#include <sys/types.h>
#include <iostream.h> 
#include <string.h>

// Motif Include Files
#include <Xm/RowColumn.h>

// ViewKit Include Files
#include <Vk/VkFileSelectionDialog.h>

// Custom Classes Include Files
#include "MainWindow.h"
#include "Player.h"


//
// Data Tables (Resources/Menu descriptions)
//
String MainWindow::_defaultResources[] = {

    "",  
    NULL  
};

VkMenuDesc MainWindow::mainMenuPane[] = {

    { SUBMENU,  "File",	    NULL, MainWindow::fileMenuPane },
//    { SUBMENU,  "Sample",   NULL, MainWindow::sampleMenuPane },
    { END }
};

VkMenuDesc MainWindow::fileMenuPane[] = {

    { ACTION,   "Open...",        &MainWindow::fileOpenCallback },
//  { ACTION,   "Two",          &MainWindow::sampleCallback },
    { SEPARATOR },
    { ACTION,   "Quit",         &MainWindow::quitCallback },
    { END }
};


VkMenuDesc MainWindow::sampleMenuPane[] = {
    { LABEL,    "Test Label" },
    { SEPARATOR },
    { ACTION,   "An Action",    &MainWindow::sampleCallback },
    { SUBMENU,  "A Submenu",    NULL, MainWindow::subMenu },
    { END }
};

VkMenuDesc MainWindow::subMenu[] = {
    { ACTION,  "foo",   &MainWindow::fileOpenCallback },
    { ACTION,  "bar",   &MainWindow::sampleCallback },
    { END }
};


//
// MainWindow Class Code
//
MainWindow::MainWindow(const char *name) : VkWindow(name) {

    // create an instance of the MIDI File Player Class
    thePlayer = new Player(this);
    _appName = strdup(name);
}

const char *MainWindow::className() {

    return "MainWindow";
}


MainWindow::~MainWindow()
{
    if(thePlayer != None) {
	delete(thePlayer);
	thePlayer = None;
    }
}


void MainWindow::sample() {
    cout << "sample callback\n" << flush;
}

void MainWindow::sampleCallback(Widget, XtPointer clientData, XtPointer)
{
    MainWindow *obj = (MainWindow *)clientData;
    obj->sample();
}

void MainWindow::quitCallback(Widget, XtPointer, XtPointer)
{
    theApplication->quitYourself();
}


Widget MainWindow::setUpInterface(Widget parent) {

    Widget container, stop, play, rewind, forward, rc;
    Arg args[10];
    int n;
    


    // install the menu bar
    
    setMenuBar(mainMenuPane);


    // setup default resources
    
    setDefaultResources(mainWindowWidget(), _defaultResources);
    
    
    // create FORM widget as master container

    container = XtVaCreateManagedWidget("form",
					xmFormWidgetClass, 
					parent,
					NULL);
					
    // create filename label widget
    
    _filenameLabel = XtVaCreateManagedWidget("filenameLabel",
			    xmLabelWidgetClass, 
			    container,  
			    XmNleftAttachment, XmATTACH_FORM, 
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_FORM,  
			    XmNleftOffset, 10, 
			    XmNtopOffset, 16,
			    XmNalignment, XmALIGNMENT_BEGINNING, 
			    NULL);	

#if  0
    // create multiline TEXT widget (ScrolledText)

    n = 0;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, _filenameLabel); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNleftOffset, 10); n++;
    XtSetArg(args[n], XmNtopOffset, 12); n++;
    
    _textBox = XmCreateScrolledText(container, "textBox",
				    args, n);

    XtManageChild(_textBox);
#endif    					                 
    // create ROWCOLUMN widget for transport controls

    rc = XtVaCreateManagedWidget("transportRowColumn",
				 xmRowColumnWidgetClass,
				 container,
				 XmNorientation, XmHORIZONTAL, 
				 XmNleftAttachment, XmATTACH_FORM, 
				 XmNtopAttachment, XmATTACH_WIDGET,
				 // XmNtopWidget, _textBox, 			
				 XmNtopWidget, _filenameLabel, 			
				 NULL);


    // create REWIND, STOP, START, FORWARD transport control buttons

    // REWIND //    
    rewind = XtVaCreateManagedWidget("rewind", xmArrowButtonWidgetClass, 
				    rc, 
				    NULL);
    XtAddCallback(rewind, XmNarmCallback, rewindBeginCallback, this);
    XtAddCallback(rewind, XmNdisarmCallback, rewindEndCallback, this);

    // STOP //
    stop = XtVaCreateManagedWidget("stop", xmPushButtonWidgetClass, 
				    rc, 
				    NULL);
    XtAddCallback(stop, XmNarmCallback, stopCallback, this);


    // PLAY //		    
    play = XtVaCreateManagedWidget("play", xmPushButtonWidgetClass, 
				    rc, 
				    NULL);
    XtAddCallback(play, XmNarmCallback, playCallback, this);

    // FORWARD //				    				
    forward = XtVaCreateManagedWidget("forward", xmArrowButtonWidgetClass, 
				    rc, 
				    NULL);
    XtAddCallback(forward, XmNarmCallback, forwardBeginCallback, this);
    XtAddCallback(forward, XmNdisarmCallback, forwardEndCallback, this);	


    // Create slider (SCALE) control for speed multiplier
    _speedScale =  XtVaCreateManagedWidget("speedSlider", 
				    xmScaleWidgetClass, 
				    rc, 
				    XmNorientation, XmHORIZONTAL,
				    XmNminimum, 100, 
				    XmNmaximum, 2000, 
				    XmNshowValue, True, 
				    XmNdecimalPoints, 1,
				    XmNvalue, 1000,  
				    NULL);
    XtAddCallback(_speedScale, 
		    XmNdragCallback, 
		    speedCallback, this);		
		    


		      
    // Create slider (SCALE) control for song position
    _songPosScale =  XtVaCreateManagedWidget("songPosSlider", 
				    xmScaleWidgetClass, 
				    container,
				    XmNtopAttachment, XmATTACH_WIDGET, 
				    XmNtopWidget, _speedScale, 
				    XmNleftAttachment, XmATTACH_FORM, 
				    XmNrightAttachment, XmATTACH_FORM,  
				    XmNorientation, XmHORIZONTAL,
				    XmNshowValue, True,
				    XmNmaximum, 1000, 
				    XmNdecimalPoints, 1,   
				    NULL);
    XtAddCallback(_songPosScale, 
		    XmNdragCallback, 
		    songPosCallback, this);		   
		                  
    return container;
}

