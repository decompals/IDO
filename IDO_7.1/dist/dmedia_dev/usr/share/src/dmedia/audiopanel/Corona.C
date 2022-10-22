/* **************************************************************
 *    Original contributors by Terry Weissman, Bruce Karsh, Doug Cook,
 *	Amit Shoham, Marc Callow, Candace Obert 
 *    ViewKit/Motif version by Gints Klimanis
 *				1991-4
 * ************************************************************** */
#include "Corona.h"
#include <Xm/MwmUtil.h>
#include <Vk/VkApp.h>
#include <Vk/VkFormat.h> 
#include <Vk/VkResource.h>
#include <Vk/VkWarningDialog.h>

#include <Xm/RowColumn.h> 
#include <Xm/Label.h> 
#include <Xm/Scale.h> 
#include <Xm/ToggleB.h> 

#include <Sgm/Grid.h> 
#include <Sgm/SpringBox.h> 

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h> 

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>

/* ******************************************************************
 *  PresenterWindow:	class constructor 
 * ****************************************************************** */
PresenterWindow::PresenterWindow (const char	*windowName,
				const char	*windowIconName,
				int		graphicsFD) 
			    : VkSimpleWindow (windowName) 
{
int	i;

_windowName = (char *) malloc(strlen(windowName)+1);
if (_windowName != NULL)
    strcpy(_windowName, windowName);

/* set icon name (may be different from window name) */
//setTitle(windowTitle);
setIconName(windowIconName);

/* geometry in format widthxheight+x+y.  Saw off width and height */
char *userGeometry = (char *) VkGetResource(_baseWidget, "geometry", "Geometry", XmRString, NULL );
if (userGeometry != NULL)
    {
    /* first '+' character marks start of new string */
    int length = strlen(userGeometry);
    for (i = 0; i < length && userGeometry[i] != '+'; i++) 
	    {}

    XtVaSetValues(_baseWidget, XmNgeometry, &userGeometry[i], NULL);
    }

/* remove resize utility from window border and window border menu */
XtVaSetValues(_baseWidget, 
	    XmNmwmDecorations,	    MWM_DECOR_ALL | MWM_DECOR_RESIZEH | MWM_DECOR_MAXIMIZE, 
	    XmNmwmFunctions,	    MWM_FUNC_ALL | MWM_FUNC_RESIZE | MWM_FUNC_MAXIMIZE, 
	    XmNminWidth,	    170,
	    NULL );

/* install callback to guard against unexpected widget destruction */
installDestroyHandler();

_mainSpringBox = XtVaCreateManagedWidget("mainPresenterSpringBox",
				      sgSpringBoxWidgetClass,
				      mainWindowWidget(), 
				      NULL);


_blockLabelAndSlider = XtVaCreateManagedWidget("presenterSliderBlockContainer",
				      sgGridWidgetClass,
				      _mainSpringBox, 
				      XmNnumRows,		3, 
				      XmNnumColumns,		1, 
				      NULL);

// need this line to get Speaker label to line up w/right side of rightmost slider
SgGridSetColumnResizability(_blockLabelAndSlider, 0, 0);

/* add block label lines up w/right most slider right edge */
_speakerLabel = XtVaCreateManagedWidget("presenterOutputLabel",
					xmLabelWidgetClass,
					_blockLabelAndSlider, 
					XmNgravity, 		CenterGravity,
					XmNresizeHorizontal,	False, 
					XmNrow,			0, 
					XmNcolumn,		0, 
					XmNmarginRight,		0, 
					XmNrightOffset,		0, 
//					XmNalignment,		XmALIGNMENT_END, 
					XmNborderWidth,		0, 
					XmNmarginWidth,		0, 
					NULL);

_springBox = XtVaCreateManagedWidget("presenterSpringBox",
				      sgSpringBoxWidgetClass,
				      _blockLabelAndSlider, 
				      XmNrow, 		1, 
				      XmNcolumn,	0, 
				      NULL);

/*
 * ----------- create presenter tone control slider
 */
_toneBlock = XtVaCreateManagedWidget("toneBlock",
				    sgGridWidgetClass,
				    _springBox, 
				    XmNorientation, 	    XmVERTICAL, 
				      XmNnumRows,		2, 
				      XmNnumColumns,		1, 
				      XmNleftSpring,		25, 
				    NULL);

_toneSliderLabel = XtVaCreateManagedWidget("toneSliderLabel",
				   xmLabelWidgetClass,
				   _toneBlock, 
				   XmNrow,	0, 
				   XmNcolumn,	0, 
				   XmNgravity,	WestGravity, 
				   XmNalignment,	XmALIGNMENT_BEGINNING, 
				   NULL);    

_toneSliderBox = XtVaCreateManagedWidget("toneSliderBox",
				    sgGridWidgetClass,
				    _toneBlock, 
				    XmNrow,	    1, 
				    XmNcolumn,	    0, 
				    XmNnumRows,	    1, 
				    XmNnumColumns,  2, 
				    NULL);

/* slider range labeled as [-12..15] but graphics driver parameter range [0..9] */
_toneSlider = XtVaCreateManagedWidget("slider",
				    xmScaleWidgetClass,
				    _toneSliderBox, 
				    XmNrow,	    0, 
				    XmNcolumn,	    0, 
				    XmNminimum,	    0, 
				    XmNmaximum,	    9, 
				    NULL);

/* add callback to tone slider */
XtAddCallback(_toneSlider, XmNvalueChangedCallback,
		&PresenterWindow::toneSliderCB,
		(XtPointer) this); 
XtAddCallback(_toneSlider, XmNdragCallback,
		&PresenterWindow::toneSliderCB,
		(XtPointer) this); 

/* add tick marks to left of slider */
_toneSliderTickMarks = new VkTickMarks("tickLabel1", 
					_toneSliderBox, 
					False, 
					False, 
					False);
XtVaSetValues(_toneSliderTickMarks->baseWidget(),
		XmNrow,	    0, 
		XmNcolumn,  1, 
		NULL);

/* set up decade scale scale tick marks */
_toneSliderTickMarks->setScale(-12, 15, 27, 3);
_toneSliderTickMarks->addLabel(0);
_toneSliderTickMarks->show();

/*
 * ----------- create presenter speaker level sliders
 */
_levelBlock = XtVaCreateManagedWidget("levelSliderBlock1",
					sgGridWidgetClass,
					_springBox, 
					XmNnumRows,	    2, 
					XmNnumColumns,	    3,
				      XmNrightSpring,		25, 
					NULL);

/* create left and right sliders */
_levelSliderLabel[0] = XtVaCreateManagedWidget("leftLabel",
				   xmLabelWidgetClass,
				   _levelBlock, 
				   XmNrow,	    0, 
				   XmNcolumn,	    0+1, 
				   NULL);    
_levelSliderLabel[1] = XtVaCreateManagedWidget("rightLabel",
				   xmLabelWidgetClass,
				   _levelBlock, 
				   XmNrow,	    0, 
				   XmNcolumn,	    1+1, 
				   NULL);    
for (i = 0; i < 2; i++)
    {
/* slider range [0..100] */
    _levelSlider[i] = XtVaCreateManagedWidget("slider",
					xmScaleWidgetClass,
					_levelBlock, 
					XmNrow,		    1, 
					XmNcolumn,	    i+1, 
					XmNminimum,	    0, 
					XmNmaximum,	    100, 
					NULL);
/* add callback to level slider */
    XtAddCallback(_levelSlider[i], XmNvalueChangedCallback,
		    &PresenterWindow::levelSliderCB,
		    (XtPointer) this); 
    XtAddCallback(_levelSlider[i], XmNdragCallback,
		    &PresenterWindow::levelSliderCB,
		    (XtPointer) this); 
    }

/* add tick marks to right of sliders */
_levelSliderTickMarks = new VkTickMarks("tickLabel1", 
					_levelBlock, 
					True, 
					False, 
					False);

XtVaSetValues(_levelSliderTickMarks->baseWidget(),
		XmNrow,		1, 
		XmNcolumn,	0, 
		XmNmarginLeft,	0, 
		XmNmarginRight,	0, 
		XmNleftOffset,	0, 
		XmNrightOffset,	0, 
		XmNmarginWidth,	0, 
		XmNborderWidth,	0, 
		NULL);
setLevelSliderTickMarkType(SCALE_DECADE);
_levelSliderTickMarks->show();



/*
 * ----------- create presenter mute button
 */
/* to line this button up with output controls, add to main grid widget */
/* specify resizability.  Need to set XmNresizeHorizontal
	to false so labels aren't spaced away from toggle buttons */
_muteToggle = XtVaCreateManagedWidget("muteToggle",
					xmToggleButtonWidgetClass,
					_blockLabelAndSlider,
					XmNresizeHorizontal,	False, 
					XmNrow, 		2, 
					XmNcolumn, 		0, 
					XmNmarginBottom,	0, 
					XmNbottomOffset,	0, 
//					XmNleftOffset,		30, 
					XmNmarginHeight,	0, 
					XmNalignment,           XmALIGNMENT_CENTER,
					XmNgravity,		EastGravity, 
					NULL );
XtAddCallback(_muteToggle, 
		XmNvalueChangedCallback,    &PresenterWindow::enableMuteCB,
		(XtPointer) this); 

// set timed callback for Conrona server poll (in milliSeconds) 
_pollInterval = (int)(DEFAULT_PRESENTER_POLL_INTERVAL_VALUE*1000 + 0.5);
_pollTimer = new VkPeriodic(_pollInterval);
VkAddCallbackMethod(VkPeriodic::timerCallback,
		    _pollTimer, 
		    this, 
		    PresenterWindow::pollState, 
		    NULL );

_graphicsFD = graphicsFD;
/* initialize to impossible state */
_muteStatus = -1;
_tone = -1;
_level[0] = -1;
_level[1] = -1;

//_muteStatus = GetFlatPanelSpeakerMute(_graphicsFD);
//_tone = GetFlatPanelSpeakerTone(_graphicsFD);
// initialize slider gang values 
_gangState = True;
for (i = 0; i < 2; i++)
    {
    _gangValue[i] = GetFlatPanelSpeakerLevel(_graphicsFD, i);
//    XmScaleSetValue(_levelSlider[i], _gangValue[i]);
//    _level[i] = GetFlatPanelSpeakerLevel(_graphicsFD, i);
    }
} /* ---- end PresenterWindow() ---- */

/* ******************************************************************
 * className:	return class name
 * ****************************************************************** */
    const char * 
PresenterWindow::className()
{
return "PresenterWindow";
} /* ---- end className() ---- */

/* ******************************************************************
 * ~PresenterWindow:    destructor
 * ****************************************************************** */
PresenterWindow::~PresenterWindow()
{
} /* ---- end ~PresenterWindow() ---- */

/* ******************************************************************
 * stateChanged:    window iconization state 
 * ****************************************************************** */
    void
PresenterWindow::stateChanged(IconState state)
{
VkSimpleWindow::stateChanged(state);

if (state == VkSimpleWindow::OPEN)
    {
    pollState();
    _pollTimer->start(_pollInterval);
    }
 else
    {
    _pollTimer->stop();
    }
} /* ---- end stateChanged() ---- */

/* ******************************************************************
 * handleWmDeleteMessage:    override window delete
 * ****************************************************************** */
    void
PresenterWindow::handleWmDeleteMessage()
{
hide();
} /* ---- end handleWmDeleteMessage() ---- */

/* ******************************************************************
 * handleWmQuitMessage:    override window quit
 * ****************************************************************** */
    void
PresenterWindow::handleWmQuitMessage()
{
hide();
} /* ---- end handleWmQuitMessage() ---- */

/* ******************************************************************
 * update:		
 * ****************************************************************** */
    void 
PresenterWindow::update(VkComponent *component)
{
// retrieve commnand line arguments
VkRunOnce *obj = (VkRunOnce*) component;

/* parse command line */
int argCount = obj->numArgs();
for (int i = 0; i < argCount; i++)
    {
/* iconify main window */
    if (!strcmp(obj->arg(i), "-iconic"))
	iconify();
    }
} /* ---- end update() ---- */

/* ******************************************************************
 * toneSliderCB:
 * ****************************************************************** */
    void 
PresenterWindow::toneSliderCB(Widget w, XtPointer clientData, XtPointer callData) 
{ 
((PresenterWindow *) clientData)->toneSliderMoved(w, callData);
} /* ---- end toneSliderCB() ---- */

/* ******************************************************************
 * toneSliderMoved:
 * ****************************************************************** */
    void 
PresenterWindow::toneSliderMoved(Widget toneSlider, XtPointer) 
{ 
// tone slider range [0 .. 9]
int	toneValue;
XmScaleGetValue(toneSlider, &toneValue);
SetFlatPanelSpeakerTone(_graphicsFD, toneValue);
} /* ---- end toneSliderMoved() ---- */

/* ******************************************************************
 * levelSliderCB:
 * ****************************************************************** */
    void 
PresenterWindow::levelSliderCB(Widget w, XtPointer clientData, XtPointer callData) 
{ 
((PresenterWindow *) clientData)->levelSliderMoved(w, callData);
} /* ---- end levelSliderCB() ---- */

/* ******************************************************************
 * levelSliderMoved: Presenter level slider callback
 * ****************************************************************** */
    void 
PresenterWindow::levelSliderMoved(Widget masterSlider, XtPointer) 
{
int masterPosition, slavePosition;
int masterChannel, isSlaveChannel[2];

/* determine channel of master slider */
for (int i = 0; i < 2; i++)
    {
    if (masterSlider == _levelSlider[i])
	{
	isSlaveChannel[i] = False;
	masterChannel = i;
	}
    else
	isSlaveChannel[i] = True;
    }

/* get master slider value */
XmScaleGetValue(masterSlider, &masterPosition);

//printf("PresenterWindow::levelSliderMoved(): ch%d=%d\n", masterChannel, masterPosition);

/* if gang enabled, move both faders with gang behavior */
if (_gangState) 
    {
    for (int i = 0; i < 2; i++)
	{
	if (isSlaveChannel[i] && i != masterChannel)
	    {
	/* slave slider position = master slider position + difference in
		values when master and slave sliders were ganged */
	    slavePosition = masterPosition + 
				    (_gangValue[i] - _gangValue[masterChannel]);
	
	/* bound (required !!!!) to range [0..100] and set slave slider value */
	    if	    (slavePosition < 0)
		slavePosition = 0;
	    else if (slavePosition > 100)
		slavePosition = 100;
	    XmScaleSetValue(_levelSlider[i], slavePosition);

	    SetFlatPanelSpeakerLevel(_graphicsFD, i, int(0.26*float(slavePosition))); 
	    }
	}
    }

// reset gang values when Presenter levels adjusted from Presenter window
for (i = 0; i < 2; i++)
    {
    int value;
    XmScaleGetValue(_levelSlider[i], &value);
    _gangValue[i] = value;
    }

// change Presenter speaker level of specified channel
SetFlatPanelSpeakerLevel(_graphicsFD, masterChannel, int(0.26*float(masterPosition))); 
} /* ---- end levelSliderMoved() ---- */

/* ******************************************************************
 * gangLevelSliders:
 * ****************************************************************** */
    void 
PresenterWindow::gangLevelSliders(Boolean slidersGanged)
{
_gangState = slidersGanged;
/* store values of sliders at time gang operation applied */
if (_gangState)
    {
    int	value;
    for (int i = 0; i < 2; i++)
	{
	XmScaleGetValue(_levelSlider[i], &value);
	_gangValue[i] = value;
	}
    }
} /* ---- end gangLevelSliders() ---- */

/* ******************************************************************
 * enableMuteCB:	mute toggle button callback
 * ****************************************************************** */
    void 
PresenterWindow::enableMuteCB(Widget, XtPointer clientData, XtPointer callData)
{
XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;
PresenterWindow *window = (PresenterWindow *) clientData;

/* update flat panel hardware mute state */
window->_muteStatus = cbs->set;
SetFlatPanelSpeakerMute(window->_graphicsFD, window->_muteStatus);
} /* ---- end enableMuteCB() ---- */

/* ******************************************************************
 * pollState:	    acquire current values of 
 *		    Presenter parameters and update
 *		    graphical user interface.
 *		    Does not run when window is iconized
 * ****************************************************************** */
    void 
PresenterWindow::pollState()
{
//printf("PresenterWindow::pollState(): polling _graphicsFD=%d\n", _graphicsFD);

/* query for level slider values */
for (int channel = 0; channel < 2; channel++)
    {
    int level = GetFlatPanelSpeakerLevel(_graphicsFD, channel);
    if (_level[channel] != level)
	{
    /* change hardware range [0..26] to slider range [0 .. 100] */
	_level[channel] = level;
	XmScaleSetValue(_levelSlider[channel], int(float(level)*(100.0/26.0) + 0.5));

    /* if level has changed, unset mute in graphics driver ONLY */
//	if (level != 0)
//	    GetFlatPanelSpeakerMute(_graphicsFD, False);
    	}
    }

/* AFTER level check: query mute status */
int muteStatus = GetFlatPanelSpeakerMute(_graphicsFD);
if (_muteStatus != muteStatus)
    {
    _muteStatus = muteStatus;
    XtVaSetValues(_muteToggle, XmNset, muteStatus, NULL);
    }

/* query for tone value */
int tone = GetFlatPanelSpeakerTone(_graphicsFD);
if (_tone != tone)
    {
/* convert driver range [0..9] to display range [-12..15] */
    _tone = tone;
    XmScaleSetValue(_toneSlider, _tone);
    }

//printf("\npollState(): _level[0]=%d, _level[1]=%d\n", _level[0], _level[1]);
//printf("pollState(): _muteStatus=%d, _tone=%d\n", _muteStatus, _tone);
} /* ---- end pollState() ---- */

/* ******************************************************************
 * intervalToMilliseconds:  return value of milliSeconds	
 * ****************************************************************** */
    int 
PresenterWindow::intervalToMilliseconds(float intervalInSeconds)
{
// round to nearest milliSecond
return ((int)(intervalInSeconds*1000 + 0.5));
} /* ---- end intervalToMilliseconds() ---- */

/* ******************************************************************
 * setLevelSliderTickMarkType:    set slider tick mark type
 * ****************************************************************** */
    void 
PresenterWindow::setLevelSliderTickMarkType(char type) 
{
_sliderTickMarkType = type;
if (type == SCALE_SPINAL_TAP)
    _levelSliderTickMarks->setScale(0, 11, 11, 1);
else
    _levelSliderTickMarks->setScale(0, 10, 5, 1);
} /* ---- end setLevelSliderTickMarkType() ---- */

/* ******************************************************************
 * setLevelSliderValue:    set GUI level slider value, not hardware
 * ****************************************************************** */
    void 
PresenterWindow::setLevelSliderValue(int channel, int value) 
{
// expects range [0..100]
XmScaleSetValue(_levelSlider[channel], value);
} /* ---- end setLevelSliderValue() ---- */

/* ******************************************************************
 * setMuteToggle:    set GUI mute toggle, not hardware
 * ****************************************************************** */
    void 
PresenterWindow::setMuteToggle(Boolean status) 
{
XtVaSetValues(_muteToggle, XmNset, status, NULL);
} /* ---- end setMuteToggle() ---- */

/* ******************************************************************
 * setToneSliderValue:    set GUI tone slider value, not hardware
 * ****************************************************************** */
    void 
PresenterWindow::setToneSliderValue(int value) 
{
// expects range [0..100]
XmScaleSetValue(_toneSlider, value);
} /* ---- end setToneSliderValue() ---- */

/* ******************************************************************
 * getLevelSliderValue:    get GUI level slider value, not hardware
 * ****************************************************************** */
    int 
PresenterWindow::getLevelSliderValue(int channel) 
{
// range [0..100]
int value;
XmScaleGetValue(_levelSlider[channel], &value);
return (value);
} /* ---- end getLevelSliderValue() ---- */

/* ******************************************************************
 * getMuteToggle:    set GUI mute toggle, not hardware
 * ****************************************************************** */
    Boolean 
PresenterWindow::getMuteToggle() 
{
int status;
XtVaGetValues(_muteToggle, XmNset, &status, NULL);
return (status);
} /* ---- end getMuteToggle() ---- */

/* ******************************************************************
 * getToneSliderValue:    get GUI tone slider value, not hardware
 * ****************************************************************** */
    int 
PresenterWindow::getToneSliderValue()
{
// range [0..100]
int value;
XmScaleGetValue(_toneSlider, &value);
return (value);
} /* ---- end getToneSliderValue() ---- */
