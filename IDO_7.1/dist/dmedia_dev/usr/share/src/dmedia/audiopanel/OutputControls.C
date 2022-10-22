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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <Xm/Label.h> 
#include <Xm/Scale.h> 
#include <Xm/ToggleB.h> 

#include <Sgm/Grid.h> 
#include <Sgm/SpringBox.h> 

#include <Vk/VkFormat.h> 
#include <Vk/VkMenuItem.h>
#include <Vk/VkMenuBar.h>
#include <Vk/VkResource.h>

#include "Corona.h"
#include "OutputControls.h"
#include <audio.h>

VkMenuDesc OutputControls::optionPaneDescription[] = {
  { ACTION,   "samplingRateLabel1",	&OutputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel2",	&OutputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel3",	&OutputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel4",	&OutputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel5",	&OutputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel6",	&OutputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel7",	&OutputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel8",	&OutputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel9",	&OutputControls::optionMenuCB},
  { END},
};

/* ******************************************************************
 *  OutputControls:
 * ****************************************************************** */
OutputControls::OutputControls(const char	*name, 
				Widget		parent,
				int		blockCapacity,
				int		channelCapacity,
				Boolean		useLevelMeters,
				Boolean		canDetermineDigitalInputRate,
				Boolean		useSamplingRateOptionMenus,

				Boolean		presenterFlatPanelPresent,
				int		graphicsFD,
				PresenterWindow **presenterWindowHandle) : 
				VkComponent(name) 
{ 
int	i;

// when main speaker levels move,  this module adjusts Presenter speaker levels.
// Controls Presenter hardware directly rather than manipulate the Presenter Controls
// GUI.  This way, can adjust Presenter even when Presenter control panel not created
_presenterWindowHandle = presenterWindowHandle;
_presenterFlatPanelPresent = presenterFlatPanelPresent;
_graphicsFD = graphicsFD;


_blockCapacity = blockCapacity;
_channelCapacity = channelCapacity;
_useLevelMeters = useLevelMeters;
_useSamplingRateOptionMenus = useSamplingRateOptionMenus;
_canDetermineDigitalInputRate = canDetermineDigitalInputRate;

_baseWidget = XtVaCreateManagedWidget(_name,
				      sgSpringBoxWidgetClass,
				      parent, 
				      XmNnumRows,	1, 
				      XmNnumColumns,	_blockCapacity, 
				      XmNrow, 		0, 
				      XmNcolumn,	0, 

		XmNresizeHorizontal,	    False,
		XmNresizePolicy,	    XmRESIZE_NONE,
// AFTA
//		XmNresizeHorizontal,	    True,
//		XmNrecomputeSize,	    True,

				      NULL);

// install callback to guard against unexpected widget destruction (after base widget)
installDestroyHandler();

int channelIndex = 0;
for (int blockCount = 0; blockCount < _blockCapacity; blockCount++)
    {
    _block[blockCount] = XtVaCreateManagedWidget("outputSliderBlockContainer",
					  sgSpringBoxWidgetClass,
					  _baseWidget, 
					XmNorientation,         XmVERTICAL,

		XmNresizeHorizontal,	    False,

// AFTA
//		XmNresizeHorizontal,	    True,
//		XmNrecomputeSize,	    True,

					  NULL);

/* Speaker label lines up with right most slider right edge */
	_blockLabel[blockCount] = XtVaCreateManagedWidget("speakerLabel",
						xmLabelWidgetClass,
						_block[blockCount], 
						XmNgravity, 		CenterGravity,
						XmNresizeHorizontal,	False, 
						XmNmarginRight,		0, 
						XmNrightOffset,		0, 
						XmNalignment,		XmALIGNMENT_END, 
						XmNborderWidth,		0, 
						XmNmarginWidth,		0, 
						NULL);

    int blockColumnCount = 4;
    if (_useLevelMeters)
	blockColumnCount += 3;
    _sliderBlock[blockCount] = XtVaCreateManagedWidget(VkFormat("sliderBlock%d", blockCount+1),
						    sgGridWidgetClass,
						    _block[blockCount], 
						    XmNnumRows,	    2, 
						    XmNnumColumns,  blockColumnCount,
						    NULL);
/* set this otherwise sliders space themselves wider */
    for (i = 0; i < blockColumnCount; i++)
	SgGridSetColumnResizability(_sliderBlock[blockCount], i, 0);

    int columnIndex = 0;
    int rowIndex = 0;

/* add tick marks to slider */
    /* add slider tick marks to a widget shuffle deck */
    if (blockCount == 0)
	{
	_sliderTickDeck[blockCount] = new VkDeck(VkFormat("outputDeck%d", blockCount+1), 
					_sliderBlock[blockCount]);
	XtVaSetValues(_sliderTickDeck[blockCount]->baseWidget(),
			XmNrow,		1, 
			XmNcolumn,	columnIndex, 

			XmNmarginLeft,	0, 
			XmNmarginRight,	0, 
			XmNleftOffset,	0, 
			XmNrightOffset,	0, 
			XmNmarginWidth,	0, 
			XmNborderWidth,	0, 
			NULL);

	for (i = 0; i < 2; i++)
	    {
	    _tickMarks[blockCount][i] = new VkTickMarks(VkFormat("tickLabel%d", i+1), 
							_sliderTickDeck[blockCount]->baseWidget(), 
							True, 
							False, 
							False);

	    XtVaSetValues(_tickMarks[blockCount][i]->baseWidget(),
			XmNmarginLeft,	0, 
			XmNmarginRight,	0, 
			XmNleftOffset,	0, 
			XmNrightOffset,	0, 
			XmNmarginWidth,	0, 
			XmNborderWidth,	0, 
			NULL);


	    _sliderTickDeck[blockCount]->addView(_tickMarks[blockCount][i]);
	    _tickMarks[blockCount][i]->show();
	    }

    /* set up decade scale, spinal tap scale tick marks */
	_tickMarks[blockCount][SCALE_DECADE]->setScale(0, 10, 5, 1);
	_tickMarks[blockCount][SCALE_SPINAL_TAP]->setScale(0, 11, 11, 1);

	_sliderTickDeck[blockCount]->show();
	}
    columnIndex++;

/* create slider labels */
    _sliderLabel[0] = XtVaCreateManagedWidget("leftLabel",
					   xmLabelWidgetClass,
					   _sliderBlock[blockCount], 
					   XmNrow,	rowIndex, 
					   XmNcolumn,	1, 
					   NULL);    
    _sliderLabel[1] = XtVaCreateManagedWidget("rightLabel",
				       xmLabelWidgetClass,
				       _sliderBlock[blockCount], 
				       XmNrow,	    rowIndex, 
				       XmNcolumn,   2, 
				       NULL);

/* create left slider */
    _slider[channelIndex] = XtVaCreateManagedWidget("slider",
					    xmScaleWidgetClass,
					    _sliderBlock[blockCount], 
					    XmNrow,	1, 
					    XmNcolumn,	columnIndex, 
					    NULL);
    columnIndex++;

if (_useLevelMeters)
    {
/* create left meter */
    _meter[channelIndex] = new VkVuMeter("meter", _sliderBlock[blockCount]);
    XtVaSetValues(_meter[channelIndex]->baseWidget(),
		  XmNrow,	1, 
		  XmNcolumn,	columnIndex, 
		  NULL);
    columnIndex++;
    
/* add 11 meter segment labels (XmNorientation=vertical necessary) */
    _meterSegmentLabels[blockCount] = XtVaCreateManagedWidget("meterSegmentLabels",
				    sgSpringBoxWidgetClass,
				    _sliderBlock[blockCount], 
				    XmNorientation, 	    XmVERTICAL, 
				    XmNrow, 		    1, 
				    XmNcolumn, 		    columnIndex, 
				    NULL);
    for (int i = 0; i < 11; i++)
	{
	XtVaCreateManagedWidget(VkFormat("label_%d", i),
			       xmLabelWidgetClass,
			       _meterSegmentLabels[blockCount], 
			       NULL);	
	}
    columnIndex++;
    
/* create right meter */
    channelIndex++;
    _meter[channelIndex] = new VkVuMeter("meter", _sliderBlock[blockCount]);
    XtVaSetValues(_meter[channelIndex]->baseWidget(),
		  XmNrow,	1, 
		  XmNcolumn,	columnIndex, 
		  NULL);
    columnIndex++;
    }
else
    channelIndex++;

/* create right slider */
    _slider[channelIndex] = XtVaCreateManagedWidget("slider",
					xmScaleWidgetClass,
					_sliderBlock[blockCount], 
					XmNrow,		1, 
					XmNcolumn,	columnIndex, 
					NULL);
    channelIndex++;
    columnIndex++;

/* add invisible (faux) tick marks to right of deck to help center sampling rate label */
    if (_blockCapacity < 2)
	{
	_fauxTickMarks[blockCount] = new VkTickMarks("tickLabel1", 
							_sliderBlock[blockCount], 
							False, 
							False, 
							False);

	XtVaSetValues(_fauxTickMarks[blockCount]->baseWidget(),
		    XmNrow,		1, 
		    XmNcolumn,		columnIndex, 	
		    XmNmarginLeft,	0, 
		    XmNmarginRight,	0, 
		    XmNleftOffset,	0, 
		    XmNrightOffset,	0, 
		    XmNmarginWidth,	0, 
		    XmNborderWidth,	0, 
		      NULL);

    /* set up decade scale, spinal tap scale tick marks */
	_fauxTickMarks[blockCount]->setScale(0, 10, 5, 1);
//	_fauxTickMarks[blockCount]->show();
	}

if (_useSamplingRateOptionMenus == False)
    {
    VkDeck *deck = new VkDeck("deck", _block[blockCount]);
    XtVaSetValues(deck->baseWidget(),
		    XmNresizeHorizontal,    False,
		    XmNrecomputeSize,	    False,
		    NULL);

    // set up with widest label anticipated 
    XmString label;
    if (_canDetermineDigitalInputRate)
	label = XmStringCreateSimple(VkGetResource("LongestSamplingRateStringDigital", ApplicationClassName));
    else
	label = XmStringCreateSimple(VkGetResource("LongestSamplingRateString", ApplicationClassName));
    _samplingRateLabel[blockCount] = XtVaCreateManagedWidget("samplingRateLabel13",
						    xmLabelWidgetClass,

//						    _block[blockCount], 
						    deck->baseWidget(),
//						    XmNmarginRight,	    0, 
//						    XmNrightOffset,	    0, 
//						    XmNborderWidth,	    0, 
		XmNresizeHorizontal,	    False,
		XmNrecomputeSize,	    False,
//						    XmNmarginWidth,	    0, 
						    XmNlabelString,	    label,
						    NULL);
    XmStringFree(label);
    deck->addView(_samplingRateLabel[blockCount]);
    deck->pop(_samplingRateLabel[blockCount]);
    deck->show();
    }
else	
    {
    _samplingRateOptionMenu[blockCount] = new VkOptionMenu(_block[blockCount], 
							    "optionMenu",
							    optionPaneDescription, 
							    _baseWidget);
    XtVaSetValues(_samplingRateOptionMenu[blockCount]->baseWidget(),
		XmNalignment,	    XmALIGNMENT_CENTER,
		XmNgravity,	    CenterGravity,
		  NULL);
    _samplingRateOptionMenu[blockCount]->show();
    }
   }

// ????? needed now that VkPeriodic fixed to poll at start of interval?
// initialize slider gang values 
long    pv[4];
pv[0]  = AL_LEFT_SPEAKER_GAIN;
pv[2]  = AL_RIGHT_SPEAKER_GAIN;
ALgetparams(AL_DEFAULT_DEVICE, pv, 4);
_dacValue[0] = pv[1];
_dacValue[1] = pv[3];
_gangState = True;

for (i = 0; i < _channelCapacity; i++)
    {
// initialize slider&gang values 
    _gangValue[i] = SliderValueFromHardwareLevel(_dacValue[i]);
    XmScaleSetValue(_slider[i], _gangValue[i]);

// add callbacks to sliders 
    XtAddCallback(_slider[i], XmNvalueChangedCallback,
		    &OutputControls::levelSliderCB,
		    (XtPointer) this); 
    XtAddCallback(_slider[i], XmNdragCallback,
		    &OutputControls::levelSliderCB,
		    (XtPointer) this); 
// initialize meters 
    if (_useLevelMeters)
	{
	_meter[i]->show();
	setMeter(i, 109, 109);
	}
    }
} /* ---- end OutputControls() ---- */

/* ******************************************************************
 * ~OutputControls:
 * ****************************************************************** */
OutputControls::~OutputControls() 
{
// Empty Destructor. Base class destroys widgets
} /* ---- end ~OutputControls() ---- */

/* ******************************************************************
 * className:
 * ****************************************************************** */
    const char * 
OutputControls::className() // classname
{
return ("OutputControls");
} /* ---- end className() ---- */

/* ******************************************************************
 * levelSliderCB:
 * ****************************************************************** */
    void 
OutputControls::levelSliderCB(Widget w, XtPointer clientData, XtPointer callData) 
{ 
((OutputControls *) clientData)->levelSliderMoved(w, callData);
} /* ---- end levelSliderCB() ---- */

/* ******************************************************************
 * levelSliderMoved:
 * ****************************************************************** */
    void 
OutputControls::levelSliderMoved(Widget masterSlider, XtPointer) 
{
// determine channel of master slider 
int masterChannel, isSlaveChannel[MAX_OUTPUT_CHANNELS];
int slaveChannel;
for (int i = 0; i < _channelCapacity; i++)
    {
    if (masterSlider == _slider[i])
	{
	isSlaveChannel[i] = False;
	masterChannel = i;
	}
    else
	{
	isSlaveChannel[i] = True;
	slaveChannel = i;
	}
    }

// set up main Speaker parameter array 
int	abacus = 0;
long	pv[2*MAX_OUTPUT_CHANNELS];
long	parameter[MAX_OUTPUT_CHANNELS];
parameter[0] = AL_LEFT_SPEAKER_GAIN;
parameter[1] = AL_RIGHT_SPEAKER_GAIN;
pv[abacus++] = parameter[masterChannel];

// map GUI slider linear range [0 .. 100] to Audio Library exponential range [0..255] 
int newMasterPosition;
XmScaleGetValue(masterSlider, &newMasterPosition);
pv[abacus++] = _dacValue[masterChannel] = HardwareLevelFromSliderValue(newMasterPosition);

//printf("levelSliderMoved(): ch%d: dac level=%d\n", masterChannel, _dacValue[masterChannel]);

// if gang enabled, make both Speaker sliders move with gang behavior 
// For Presenter LCD graphics option, move both Presenter level sliders with Speaker sliders
if (_gangState) 
    {
// this only works for a single slave slider
    int oldeSlavePosition, slavePosition;

    for (i = 0; i < _channelCapacity; i++)
	{
	if (isSlaveChannel[i] && i != masterChannel)
	    {
	    XmScaleGetValue(_slider[i], &oldeSlavePosition);

	/* slave slider position = master slider position + difference in
		values when master and slave sliders were ganged */
	    slavePosition = newMasterPosition + (_gangValue[i] - _gangValue[masterChannel]);
	
	/* bound (required !!!!) to range [0..100] and set slave slider value */
	    if	    (slavePosition > 100)
		slavePosition = 100;
	    else if (slavePosition < 0)
		slavePosition = 0;
	    XmScaleSetValue(_slider[i], slavePosition);
	
	/* determine value to write to Audio Hardware */
	    pv[abacus++] = parameter[i];
	    pv[abacus++] = _dacValue[i] = HardwareLevelFromSliderValue(slavePosition);
	    }
	}

//printf("apanel:  ch%d    %d -> %d (%d)\n", masterChannel, oldeSlavePosition, slavePosition,
//    slavePosition-oldeSlavePosition);

// update Presenter audio levels 
// compute determine change from previous position 
// Should be controlling the Presenter window sliders but want this to work
    if (_presenterFlatPanelPresent)
	{
    // scale difference from range [0..100] range to [0..100] 
    // add 0.5 for rounding BEFORE clipping
	float difference = 0.5 + float(slavePosition-oldeSlavePosition);	   

	for (i = 0; i < 2; i++)
	    {
	    float newPosition;

	    if (*_presenterWindowHandle)
		{
		int position = (*_presenterWindowHandle)->getLevelSliderValue(i);
		newPosition = difference + position;
		}
	    else
		newPosition = difference + GetFlatPanelSpeakerLevel(_graphicsFD, i);

	// bound to range [0..100] and set slave slider value 
	    if	    (newPosition > 100)
		newPosition = 100;
	    else if (newPosition < 0)
		newPosition = 0;

	    if (*_presenterWindowHandle)
		(*_presenterWindowHandle)->setLevelSliderValue(i, (int)newPosition);
	    else
    		SetFlatPanelSpeakerLevel(_graphicsFD, i, int(0.26*newPosition));
	    }
	}
    }

// set Audio Hardware speaker levels 
ALsetparams(AL_DEFAULT_DEVICE, pv, abacus);
} /* ---- end levelSliderMoved() ---- */

/* ******************************************************************
 *  setLevelSliderPosition:
 * ****************************************************************** */
    void 
OutputControls::setLevelSliderPosition(int channel, long newAudioHardwareValue) 
{
// compute new GUI Speaker slider position
int newSpeakerPosition = SliderValueFromHardwareLevel(newAudioHardwareValue);

// for existing Presenter window, adjust Presenter sliders by difference
if (_presenterFlatPanelPresent)
    {
// get current GUI Speaker slider position and compute difference
    int oldSpeakerPosition;
    XmScaleGetValue(_slider[channel], &oldSpeakerPosition);
    int sliderDifference = newSpeakerPosition - oldSpeakerPosition;

// get current Presenter level slider value, range [0..26] 
    float presenterLevel = (float) GetFlatPanelSpeakerLevel(_graphicsFD, channel);

// scale change in Speaker sliders range [0..100] to a change for the Presenter
// level sliders, range [0..100] 
// convert from range [0..26] to [0..100]
    presenterLevel *= 100.0/26.0;

// bound (required !!!!) to range [0..100] and set Presenter hardware level 
    presenterLevel += 0.5 + (float) sliderDifference;  // add 0.5 for rounding BEFORE clipping
    if	    (presenterLevel > 100)
	presenterLevel = 100;
    else if (presenterLevel < 0)
	presenterLevel = 0;
    SetFlatPanelSpeakerLevel(_graphicsFD, channel, int(0.26*presenterLevel));
    }

// set new GUI Speaker slider position
XmScaleSetValue(_slider[channel], newSpeakerPosition);
_dacValue[channel] = newAudioHardwareValue;
//printf("setLevelSliderPosition(): channel%d=%d\n", channel, newAudioHardwareValue);
} /* ---- end setLevelSliderPosition() ---- */

/*
20 values:
0, 1, 2, 3, 4, 6, 8, 11, 14, 18, 23, 37, 47, 74, 93, 117, 147, 184, 231, 255

17 values:
255, 191, 143, 107, 80, 59, 44, 32, 23, 17, 12, 8, 5, 3, 2, 1 0
*/

/* ******************************************************************
 *  enableLevelSliders:	toggle sensitivity of sliders
 *					(grey/ungrey sliders)
 * ****************************************************************** */
    void 
OutputControls::enableLevelSliders(Boolean enable) 
{
for (int i = 0; i < _channelCapacity; i++)
    XtSetSensitive(_slider[i], enable);
} /* ---- end enableLevelSliders() ---- */

/* ******************************************************************
 *  gangSliders:
 * ****************************************************************** */
    void 
OutputControls::gangSliders(Boolean slidersGanged)
{
int	sliderValue;

_gangState = slidersGanged;
/* store values of sliders at time gang operation applied */
if (_gangState)
    {
    for (int i = 0; i < _channelCapacity; i++)
	{
	XmScaleGetValue(_slider[i], &sliderValue);
	_gangValue[i] = sliderValue;
	}
    }
} /* ---- end gangSliders() ---- */

/* ******************************************************************
 *  setMeter:
 * ****************************************************************** */
    void 
OutputControls::setMeter(int channel, int shortTime, int longTime)
{  
if (_useLevelMeters)
    _meter[channel]->setValue(shortTime, longTime);
} /* ---- end setMeter() ---- */

/* ******************************************************************
 *  showMeters:
 * ****************************************************************** */
    void 
OutputControls::showMeters(Boolean showTheMeters)
{
/* if meters are to be shut off, erase values sitting in meter display */
if (_useLevelMeters && showTheMeters == False)
    {
    for (int i = 0; i < _channelCapacity; i++)
	setMeter(i, 109, 109);
    }
} /* ---- end showMeters() ---- */

/* ******************************************************************
 *  HardwareLevelFromSliderValue:  map linear slider range [0 .. 100]
 *				    to exponenetial Audio Library range 
 *				    [0 .. 255]
 * ****************************************************************** */
    long 
OutputControls::HardwareLevelFromSliderValue(int value)
{
float level;

if (value == 0) 
    level = 0;
/*  10^(0.01*value*log10(255)),      0.01*log10(255) = 0.02406540183 */
else
    level = pow(10, 0.02406540183*double(value)) + 0.5;

//printf("HardwareLevelFromSliderValue(): value=%d -> level=%d\n", value, (int)level);

return (long(level));
} /* ---- end HardwareLevelFromSliderValue() ---- */

/* ******************************************************************
 *  SliderValueFromHardwareLevel: 
 *				return slider value (range [0..100]) from
 *				Audio Library level parameter (range 0..255)
 * ****************************************************************** */
    int 
OutputControls::SliderValueFromHardwareLevel(long level)
{
if (level == 0)
    return (0);

/* 100*log10(level)/log10(255),     100/log10(255) = 41.5534304 */
float value = 0.5 + 41.5534304*log10((double) level);

/* bound slider range to [0..100] */
if	(value < 0)
    value = 0;
else if (value > 100)
    value = 100;

//printf("SliderValueFromHardwareLevel(): level=%d -> value=%g\n", level, value);

return ((int) value);
} /* ---- end SliderValueFromHardwareLevel() ---- */

/* ******************************************************************
 *  optionMenuCB:	
 * ****************************************************************** */
    void 
OutputControls::optionMenuCB(Widget w, XtPointer, XtPointer)
{
long    pv[2];

/* if parameter valid, change audio hardware state */
/* ignore invalid rates */
long index = (long) VkGetResource(w, "value", "Value", XmRInt, NULL );
switch (index)
    {
    case AL_RATE_8000:
    case AL_RATE_11025:
    case AL_RATE_16000:
    case AL_RATE_22050:
    case AL_RATE_32000:
    case AL_RATE_44100:
    case AL_RATE_48000:
    case AL_RATE_AES_1:
    case AL_RATE_INPUTRATE:
	pv[0] = AL_OUTPUT_RATE;
	pv[1] = index;
	ALsetparams(AL_DEFAULT_DEVICE, pv, 2);
    }
} /* ---- end OutputControls::optionMenuCB() ---- */

/* ******************************************************************
 *  OutputControls::setSamplingRateLabel:
 * ****************************************************************** */
    void 
OutputControls::setSamplingRateLabel(int samplingRate, int digitalSamplingRate) 
{
XmString    label;
char	    s1[30], s2[30];
float	    value;

// assemble "xxx kHz" string for sampling rate
if	(samplingRate > -1)
    {
    if (True == _useSamplingRateOptionMenus)
	{
	int index = SOME_BOGUS_SAMPLING_RATE;
	switch (samplingRate)
	    {
	    case AL_RATE_8000:
		index = 0;
	    break;
	    case AL_RATE_11025:
		index = 1;
	    break;
	    case AL_RATE_16000:
		index = 2;
	    break;
	    case AL_RATE_22050:
		index = 3;
	    break;
	    case AL_RATE_32000:
		index = 4;
	    break;
	    case AL_RATE_44100:
		index = 5;
	    break;
	    case AL_RATE_48000:
		index = 6;
	    break;
	    }
	if (index != SOME_BOGUS_SAMPLING_RATE)
	    _samplingRateOptionMenu[0]->set(index);
	}
    else
	{
	value = ((float) samplingRate)*0.001;
	sprintf(s1, VkFormat("%g %s", value, VkGetResource("kHz", ApplicationClassName)));
	}
    }

else if	(samplingRate == AL_RATE_INPUTRATE)
    {
    if (False == _useSamplingRateOptionMenus)
	strcpy(s1, VkGetResource("InputRate", ApplicationClassName));
    else
	_samplingRateOptionMenu[0]->set(8);
    }

// assemble "Digital xxx kHz" string for sampling rate
else if (samplingRate >= AL_RATE_AES_1s && samplingRate <= AL_RATE_AES_1)
    {
/*
 * the following for machines that can determine the digital input
 * sampling rate
 */

/* exclusion of rate 0 Hz compensates for driver's digital input rate
    determination invalid state of 0 Hz */
    if (_canDetermineDigitalInputRate == False ||
	    digitalSamplingRate == AL_RATE_UNDEFINED ||
	    digitalSamplingRate == AL_RATE_NO_DIGITAL_INPUT ||
	    digitalSamplingRate == AL_RATE_UNACQUIRED)
	{
	if	(samplingRate == AL_RATE_AES_1)
	    strcpy(s1, VkGetResource("Digital", ApplicationClassName));
	else if (samplingRate == AL_RATE_AES_2)
	    strcpy(s1, VkGetResource("12Digital", ApplicationClassName));
	else if (samplingRate == AL_RATE_AES_3)
	    strcpy(s1, VkGetResource("13Digital", ApplicationClassName));
	else if (samplingRate == AL_RATE_AES_4)
	    strcpy(s1, VkGetResource("14Digital", ApplicationClassName));
	else if (samplingRate == AL_RATE_AES_6)
	    strcpy(s1, VkGetResource("16Digital", ApplicationClassName));
	else if (samplingRate == AL_RATE_AES_1s)
	    strcpy(s1, VkGetResource("23Digital", ApplicationClassName));
	else 
	    strcpy(s1, VkGetResource("Digital", ApplicationClassName));
	}
/*
 * the following for machines that can determine the digital input
 * sampling rate
 */
    else
	{
	if	(samplingRate == AL_RATE_AES_1)
	    {
	    strcpy(s1, VkGetResource("Digital", ApplicationClassName));
	    value = ((float) digitalSamplingRate)*0.001;
	    sprintf(s2, " %g", value);
	    s2[5] = '\0';	// only 3 digits + decimal point
	    strcat(s1, s2);
	    strcat(s1, VkFormat(" %s", VkGetResource("kHz", ApplicationClassName)));
	    }
	else
	    {
	    if	    (samplingRate == AL_RATE_AES_2)
		strcpy(s1, VkGetResource("12Digital", ApplicationClassName));
	    else if (samplingRate == AL_RATE_AES_3)
		strcpy(s1, VkGetResource("13Digital", ApplicationClassName));
	    else if (samplingRate == AL_RATE_AES_4)
		strcpy(s1, VkGetResource("14Digital", ApplicationClassName));
	    else if (samplingRate == AL_RATE_AES_6)
		strcpy(s1, VkGetResource("16Digital", ApplicationClassName));
	    else if (samplingRate == AL_RATE_AES_1s)
		strcpy(s1, VkGetResource("23Digital", ApplicationClassName));
	    else 
		strcpy(s1, VkGetResource("Digital", ApplicationClassName));
	    }
	}
    if (True == _useSamplingRateOptionMenus)
	{
	_samplingRateOptionMenu[0]->set(7);
	VkMenuItem *item = _samplingRateOptionMenu[0]->getItem();
	label = XmStringCreateSimple(s1);
	XtVaSetValues(item->baseWidget(), 
		    XmNlabelString, label,
		    NULL);
	XmStringFree(label);
	}
    }
else 
    strcpy(s1, VkGetResource("Dunno", ApplicationClassName));

if (False == _useSamplingRateOptionMenus)
    {
    label = XmStringCreateSimple(s1);
    XtVaSetValues(_samplingRateLabel[0], 
		XmNlabelString, label,
		NULL);
    XmStringFree(label);
    }
} /* ---- end OutputControls::setSamplingRateLabel() ---- */

/* ******************************************************************
 *  setSliderTickMarkType:    set slider tick mark type
 * ****************************************************************** */
    void 
OutputControls::setSliderTickMarkType(char type) 
{
_sliderTickMarkType = type;
for (int j = 0; j < _blockCapacity; j++)
     _sliderTickDeck[j]->pop(_tickMarks[j][type]);
} /* ---- end setSliderTickMarkType() ---- */


#ifdef TEST
{
int s0, s1;
float diff = 0.5 + ((float)(slavePosition-oldeSlavePosition));	   

XmScaleGetValue(_slider[0], &s0);
XmScaleGetValue(_slider[1], &s1);
printf("levelSliderMoved(): L=%d: R=%d  LCD L=%d, R=%d,   d=%g\n", 
	s0, s1, 
	GetFlatPanelSpeakerLevel(_graphicsFD, 0),
	GetFlatPanelSpeakerLevel(_graphicsFD, 1),
	diff);
printf("levelSliderMoved(): old=%d: new=%d\n\n\n", 
	oldeSlavePosition, slavePosition);

}

{
int abacus = 0;
for (i = 0; i < 101; i += 1)
    {
    if (i != SliderValueFromHardwareLevel(HardwareLevelFromSliderValue(i)))
	{
	abacus++;
	printf("slider level=%d ->hard %d -> slider %d\n",
		i,
		HardwareLevelFromSliderValue(i),
		SliderValueFromHardwareLevel(HardwareLevelFromSliderValue(i)));
	}
    }
printf("%d mismatches\n", abacus);
}
#endif
