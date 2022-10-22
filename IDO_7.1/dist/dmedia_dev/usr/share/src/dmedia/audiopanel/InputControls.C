/* **************************************************************
 *    Original contributors by Terry Weissman, Bruce Karsh, Doug Cook,
 *	Amit Shoham, Marc Callow 
 *    ViewKit/Motif version by Gints Klimanis
 *	Corona code by Candace Obert
 *				1991-4
 * ************************************************************** */
#include "InputControls.h" 
#include <Sgm/Grid.h> 
#include <Sgm/SpringBox.h> 

#include <Xm/DrawingA.h> 
#include <Xm/Frame.h> 
#include <Xm/Label.h> 
#include <Xm/Scale.h> 
#include <Xm/ToggleB.h> 
#include <Xm/XmP.h> 

#include <Vk/VkFormat.h> 

#include "audio.h"
#include <math.h>

#include <Vk/VkMenuItem.h>
#include <Vk/VkMenuBar.h>
#include <Vk/VkResource.h>
#include <stdlib.h>
#include <stdio.h>

VkMenuDesc InputControls::optionPaneDescription[] = {
  { ACTION,   "samplingRateLabel1",	&InputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel2",	&InputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel3",	&InputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel4",	&InputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel5",	&InputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel6",	&InputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel7",	&InputControls::optionMenuCB},
  { ACTION,   "samplingRateLabel8",	&InputControls::optionMenuCB},
  { END},
};

/*
 * SGI audio hardware employs a Multiplying Digital to Analog Converter (MDAC) 
 * for signal attenuation in the audio input hardware. The MDAC attenutation
 * resolution is range 0..63 (six bits), each increment a 1.5 decibel 
 * change in attenuation.  Values 60..63 yield infinite attenuation.
 * MDAC data sheet states monotonicity is not guaranteed when moving thru 
 * values 58 and 59.  Thus, values 58 and 59 are excluded.  The Audio Library
 * parameters AL_LEFT_INPUT_ATTEN and AL_RIGHT_INPUT_ATTEN of range 0..255 are
 * mapped into the MDAC six bit range w/a divide by 4.
 * 
 * sliderToAttenuation[] maps linear slider range into MDAC 0..48dB 
 * attenuation + rapid decrease to infinity, 
 * w/minimum attenuation corresponding to maximum value of slider range.
 * 
 * However, gain of audio line input hardware is 8 (+18dB) with
 * respect to level of audio line output.  
 *
 */
/* table to provide MDAC 0..48dB attenuation.  When 18 dB audio hardware gain
    considered, this table actually does -18..30dB attenuation */ 
static const char sliderToAttenuation[] = {
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,	/* linear */
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,	/* linear */
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,	/* linear */
    30, 31, 32,					/* linear */
    34, 37, 40, 45, 50, 55,			/* non-linear */
    63};					/* infinite attenuation */

static const char lastLinearAttenuationStep = 32;
static const char numAttenuationSteps = sizeof(sliderToAttenuation)/sizeof(char);

/* ******************************************************************
 * InputControls:
 * ****************************************************************** */
InputControls::InputControls(const char *name, 
			    Widget	parent,
			    int		blockCapacity,
			    int		channelCapacity,
			    Boolean	useLevelMeters,
			    Boolean	canDetermineDigitalInputRate,
			    Boolean	useSamplingRateOptionMenus) : 
			    VkComponent(name) 
{ 
int	i;

_blockCapacity = blockCapacity;
_channelCapacity = channelCapacity;
_useLevelMeters = useLevelMeters;
_useListeningLED = False;
_listeningLEDFrame = NULL;

_useSamplingRateOptionMenus = useSamplingRateOptionMenus;
_canDetermineDigitalInputRate = canDetermineDigitalInputRate;
_sliderTickMarkType = SCALE_DECADE;

// Create unmanaged widget as element of widget hierarchy
_baseWidget = XtVaCreateWidget(_name,
//			       sgGridWidgetClass,
			       sgSpringBoxWidgetClass,
			       parent,
			       NULL);
// next line allows spring behavior of slider blocks
//SgGridSetColumnResizability(_baseWidget, 0, 1);

/* for > 1 blocks, center label over several blocks */
_useGroupLabels = False;
Boolean useSubscriptedSliderLabels = False;
if (_blockCapacity > 1)
    {
    useSubscriptedSliderLabels = True;
    _useGroupLabels = True;
    }

if (_useGroupLabels == True)
    {
    _blockContainerParent = XtVaCreateManagedWidget(_name,
						   sgGridWidgetClass,
						   _baseWidget,
						   XmNnumRows,		3, 
						   XmNnumColumns,	1, 
						   NULL);

/* create block label */
    _blockLabel[0] = XtVaCreateManagedWidget("inputLabel",
					     sgSpringBoxWidgetClass,
					     _blockContainerParent, 
					    XmNrow,			0, 
					    XmNcolumn,			0, 
					     NULL);
    	createListeningLED();
  
/* create Input section label, followed by input source */
    _inputSourceTitle = XtVaCreateManagedWidget("inputSourceTitle",
						 xmLabelWidgetClass,
						 _blockLabel[0], 
						XmNrightSpring,	    0,						
						 NULL);

    /* create with largest label */
    _inputSourceLabel = XtVaCreateManagedWidget("inputSourceLabel",
						 xmLabelWidgetClass,
						 _blockLabel[0], 
						XmNleftSpring,	    0,
						XmNrightSpring,	    50,
						 NULL);

/* create slider box spring box */
    _blockContainer = XtVaCreateManagedWidget("blockContainer",
					    sgSpringBoxWidgetClass,
					    _blockContainerParent, 
					    XmNnumRows,		1, 
					    XmNnumColumns,	_blockCapacity, 
					    XmNrow, 		1, 
					    XmNcolumn,		0, 
					    NULL);

/* create group sampling rate label */
    if (_useSamplingRateOptionMenus == False)
	{
	VkDeck *deck = new VkDeck("deck", _blockContainerParent);
	XtVaSetValues(deck->baseWidget(),
			XmNrow,			2, 
			XmNcolumn,		0, 
			XmNresizeHorizontal,    False,
			XmNrecomputeSize,	False,
			XmNgravity,		CenterGravity,
			NULL);

	// set up with widest label anticipated 
	XmString label;
	if (_canDetermineDigitalInputRate == True)
	    label = XmStringCreateSimple(VkGetResource("LongestSamplingRateStringDigital", ApplicationClassName));
	else
	    label = XmStringCreateSimple(VkGetResource("LongestSamplingRateString", ApplicationClassName));
	_samplingRateLabel[0] = XtVaCreateManagedWidget("samplingRateLabel1",
							xmLabelWidgetClass,
							deck->baseWidget(), 
							XmNmarginRight,	    0, 
							XmNrightOffset,	    0, 
							XmNalignment,	    XmALIGNMENT_CENTER, 
							XmNborderWidth,	    0, 
							XmNmarginWidth,	    0, 
							XmNlabelString,	    label,
		XmNresizeHorizontal,	    False,
		XmNrecomputeSize,	    False,
							NULL);
	XmStringFree(label);
    deck->addView(_samplingRateLabel[0]);
    deck->pop(_samplingRateLabel[0]);
    deck->show();
	}
    else
	{
	_samplingRateOptionMenu[0] = new VkOptionMenu(_blockContainerParent, 
						    "optionMenu",
						    optionPaneDescription, 
						    _blockContainerParent);
	_samplingRateOptionMenu[0]->show();
	XtVaSetValues(_samplingRateOptionMenu[0]->baseWidget(),
		    XmNalignment,   XmALIGNMENT_CENTER,
		    XmNgravity,	    CenterGravity,
		      NULL);
	}
    }
else
    {
    _blockContainerParent = _baseWidget;
    _blockContainer = XtVaCreateManagedWidget("blockContainer",
					    sgSpringBoxWidgetClass,
					    _blockContainerParent, 
					    XmNrow, 		0, 
					    XmNcolumn,		0, 
					    NULL);
    }

// install callback to guard against unexpected widget destruction (after base widget)
installDestroyHandler();


// add space to sides of block container for larger channel systems
if (_blockCapacity > 1)
    XtVaSetValues(_blockContainer, XmNmarginWidth, 15, NULL);

int channelIndex = 0;
for (int blockCount = 0; blockCount < _blockCapacity; blockCount++)
    {
    int blockRowCount = 1;
    if (_useGroupLabels == False)
	blockRowCount += 2;
    
    _block[blockCount] = XtVaCreateManagedWidget("inputSliderBlockContainer",
					  sgGridWidgetClass,
					  _blockContainer, 
					  XmNnumRows,		blockRowCount, 
					  XmNnumColumns,	1, 
					  XmNrow,		0, 
					  XmNcolumn,		blockCount, 
					  NULL);
//    SgGridSetColumnResizability(_block[blockCount], 0, 1);
if (_blockCapacity == 2)
{
if (blockCount == 0)
    XtVaSetValues(_block[blockCount], XmNleftSpring, 50, XmNrightSpring, 0, NULL);
else 
    XtVaSetValues(_block[blockCount], XmNleftSpring, 0, XmNrightSpring, 50, NULL);
}

    int columnIndex = 0;
    int rowIndex = 0;
    int blockColumnCount = 3;

    if (_useGroupLabels == False)
	{
	rowIndex++;
	if (_blockCapacity == 1)
	    blockColumnCount++;
	}
    if (_useLevelMeters == True)
	blockColumnCount += 3;
/* grid contains sliders&meters in low row, slider labels L&R in top row*/
    _sliderBlock[blockCount] = XtVaCreateManagedWidget("sliderBlock",
						    sgGridWidgetClass,
						    _block[blockCount], 
						    XmNnumRows,	    2, 
						    XmNnumColumns,  blockColumnCount, 
						    XmNrow, 	    rowIndex, 
						    XmNcolumn, 	    0, 
						    NULL);

/* add labels to first block only */
    if (blockCount == 0 && _useGroupLabels == False)
	{
	_blockLabel[0] = XtVaCreateManagedWidget("inputLabel",
						 sgSpringBoxWidgetClass,
						 _block[blockCount], 
						XmNrow,			0, 
						XmNcolumn,		0, 
						 NULL);
    	createListeningLED();


/* create Input section label, followed by input source */
    _inputSourceTitle = XtVaCreateManagedWidget("inputSourceTitle",
					 xmLabelWidgetClass,
					 _blockLabel[0], 
					 NULL);
    XtVaSetValues(_inputSourceTitle, XmNrightSpring, 0, NULL);

    /* create with largest label */
    _inputSourceLabel = XtVaCreateManagedWidget("inputSourceLabel",
					 xmLabelWidgetClass,
					 _blockLabel[0], 
					XmNleftSpring,	    0,
					XmNrightSpring,	    50,
					 NULL);

    /* create invisible tick mark the enables centering of sampling rate label */
	_fauxTickMarks[blockCount] = new VkTickMarks("tickLabel3", 
						    _sliderBlock[blockCount], 
						    True, 
						    False, 
						    False);

	XtVaSetValues(_fauxTickMarks[blockCount]->baseWidget(),
		      XmNrow,		1, 
		      XmNcolumn,	columnIndex, 
		    XmNmarginLeft,	0, 
		    XmNmarginRight,	0, 
		    XmNleftOffset,	0, 
		    XmNrightOffset,	0, 
		    XmNmarginWidth,	0, 
		    XmNborderWidth,	0, 
		      NULL);
    /* set up decibel scale, widest to add same width as added on right side */
	_fauxTickMarks[blockCount]->setScale(-30, 18, 10, 2);
//	_fauxTickMarks[blockCount]->setScale(-30, 18, 6, 1);
//	_fauxTickMarks[blockCount]->show();
	columnIndex++;
	}

/* Add tick marks to right of even-numbered blocks, to left of odd-numbered
	blocks.  Add slider tick marks to widget shuffle deck */
    if ((blockCount&0x1) == 1)
	{
	_sliderTickDeck[blockCount] = new VkDeck(VkFormat("inputDeck%d", blockCount+1), 
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

	for (i = 0; i < 3; i++)
	    {
	    _tickMarks[blockCount][i] = new VkTickMarks(VkFormat("tickLabel%d", i+1), 
						    _sliderTickDeck[blockCount]->baseWidget(), 
						    True, 
						    True, 
						    _useGroupLabels);
	    _sliderTickDeck[blockCount]->addView(_tickMarks[blockCount][i]);

	XtVaSetValues(_tickMarks[blockCount][i]->baseWidget(),
		    XmNmarginLeft,	0, 
		    XmNmarginRight,	0, 
		    XmNleftOffset,	0, 
		    XmNrightOffset,	0, 
		    XmNmarginWidth,	0, 
		    XmNborderWidth,	0, 
			NULL);

	    _tickMarks[blockCount][i]->show();
	    }

    /* set up decade scale, spinal tap and decibel scale tick marks */
	_tickMarks[blockCount][SCALE_DECADE]->setScale(0, 10, 5, 1);
	_tickMarks[blockCount][SCALE_SPINAL_TAP]->setScale(0, 11, 11, 1);
	_tickMarks[blockCount][SCALE_DECIBEL]->setScale(-30, 18, 10, 2);
//	_tickMarks[blockCount][SCALE_DECIBEL]->setScale(-30, 18, 6, 1);
	_tickMarks[blockCount][SCALE_DECIBEL]->addLabel(0);

	_sliderTickDeck[blockCount]->show();
	columnIndex++;
	}

/* create slider labels */
    XmString	s1, s2;
    char	string1[5];

    s1 = XmStringCreate(VkGetResource("leftLabel", ApplicationClassName), "regular");
    if (_blockCapacity > 1)
	{
	sprintf(string1, "%d\n", blockCount+1);
	s2 = XmStringCreate(string1, "subscript");
	s1 = XmStringConcat(s1, s2);
	}
    _sliderLabel[0] = XtVaCreateManagedWidget(VkFormat("inputSlider%dLabel", 2*blockCount+0+1),
					    xmLabelWidgetClass,
					    _sliderBlock[blockCount], 
					    XmNrow,	    0, 
					    XmNgravity,	    CenterGravity,
					    XmNcolumn,	    columnIndex, 
					    XmNlabelString, s1,
					    NULL); 

    XmStringFree(s1);
    if (_blockCapacity > 1)
	XmStringFree(s2);

    s1 = XmStringCreate(VkGetResource("rightLabel", ApplicationClassName), "regular");
    if (_blockCapacity > 1)
	{
	sprintf(string1, "%d\n", blockCount+1);
	s2 = XmStringCreate(string1, "subscript");
	s1 = XmStringConcat(s1, s2);
	}
    _sliderLabel[1] = XtVaCreateManagedWidget(VkFormat("inputSlider%dLabel", 2*blockCount+1+1),
					    xmLabelWidgetClass,
					    _sliderBlock[blockCount], 
					    XmNrow,	    0, 
					    XmNcolumn,	    columnIndex+4, 
					    XmNgravity,	    CenterGravity,
					    XmNlabelString, s1,
					    NULL);
    XmStringFree(s1);
    if (_blockCapacity > 1)
	XmStringFree(s2);

/* create left slider */
    _slider[channelIndex] = XtVaCreateManagedWidget("slider",
					    xmScaleWidgetClass,
					    _sliderBlock[blockCount], 
					    XmNrow, 		    1, 
					    XmNcolumn, 		    columnIndex, 
					    NULL);
    columnIndex++;

if (_useLevelMeters == True)
    {
/* create left meter */
/* shadow box around the meter to make it appear recessed */
    _meterFrame[channelIndex] = XtVaCreateManagedWidget("meterFrame",
					    xmFrameWidgetClass,
					    _sliderBlock[blockCount], 
					    XmNrow, 		    1, 
					    XmNcolumn, 		    columnIndex, 
					    NULL);

/* meter itself */
    _meter[channelIndex] = new VkVuMeter("meter", _meterFrame[channelIndex]);
#ifdef USE_SCHEME_COLOR
    Pixel color = (Pixel) VkGetResource(_listeningLED, 
				    "basicBackground", 
				    "BasicBackground",
				    XmRPixel, "basicBackground");
    XtVaSetValues(_meter[channelIndex]->baseWidget(), XmNbackground, color, NULL);
#endif
    columnIndex++;
    
/* add 11 meter segment labels (XmNorientation=vertical necessary) */
    _meterSegmentLabelBox[blockCount] = XtVaCreateManagedWidget("meterSegmentLabelBox",
				    sgSpringBoxWidgetClass,
				    _sliderBlock[blockCount], 
				    XmNorientation, 	    XmVERTICAL, 
				    XmNrow, 		    1, 
				    XmNcolumn, 		    columnIndex, 
				    XmNmarginHeight,	    0, 
				    NULL);

// seem to control height in section
    for (i = 0; i < 11; i++)
	{
	_meterSegmentLabels[i] = XtVaCreateManagedWidget(VkFormat("label_%d", i),
						       xmLabelWidgetClass,
						       _meterSegmentLabelBox[blockCount], 
							XmNmarginHeight,    0, 
							XmNmarginBottom,    0, 
							XmNmarginTop,	    0, 
							XmNalignment,	    XmALIGNMENT_CENTER, 
							NULL);	
	}

/* set to twice default value to get labels to center in box */
    XtVaSetValues(_meterSegmentLabels[0], XmNtopSpring, /*50*/ 100, NULL);
    XtVaSetValues(_meterSegmentLabels[i-1], XmNbottomSpring, /*50*/ 100, NULL);

    columnIndex++;

/* create right meter */
    channelIndex++;
/* shadow box around the meter to make it appear recessed */
    _meterFrame[channelIndex] = XtVaCreateManagedWidget("meterFrame",
					    xmFrameWidgetClass,
					    _sliderBlock[blockCount], 
					    XmNrow, 		    1, 
					    XmNcolumn, 		    columnIndex, 
					    NULL);

/* meter itself */
    _meter[channelIndex] = new VkVuMeter("meter", _meterFrame[channelIndex]);
    columnIndex++;
    }
else
    channelIndex++;

/* create right slider */
    _slider[channelIndex] = XtVaCreateManagedWidget("slider",
						xmScaleWidgetClass,
						_sliderBlock[blockCount], 
						XmNrow, 		    1, 
						XmNcolumn, 		    columnIndex, 
						NULL);
    columnIndex++;

/* Add tick marks to right of even-numbered blocks, to left of odd-numbered
	blocks.  Add slider tick marks to widget shuffle deck */
    if ((blockCount&0x1) == 0)
	{
	_sliderTickDeck[blockCount] = new VkDeck(VkFormat("inputDeck%d", blockCount+1), 
						_sliderBlock[blockCount]);
	XtVaSetValues(_sliderTickDeck[blockCount]->baseWidget(),
		      XmNrow,		    1, 
		      XmNcolumn,	    columnIndex, 
		      NULL);

	for (i = 0; i < 3; i++)
	    {
	    _tickMarks[blockCount][i] = new VkTickMarks(VkFormat("tickLabel%d", i+1), 
							_sliderTickDeck[blockCount]->baseWidget(), 
							False, 
							False, 
							_useGroupLabels);
	    _sliderTickDeck[blockCount]->addView(_tickMarks[blockCount][i]);
	    _tickMarks[blockCount][i]->show();
	    }

    /* set up decade scale, spinal tap and decibel scale tick marks */
	_tickMarks[blockCount][SCALE_DECADE]->setScale(0, 10, 5, 1);
	_tickMarks[blockCount][SCALE_SPINAL_TAP]->setScale(0, 11, 11, 1);
	_tickMarks[blockCount][SCALE_DECIBEL]->setScale(-30, 18, 10, 2 /* 6, 1 */);
	_tickMarks[blockCount][SCALE_DECIBEL]->addLabel(0);

	_sliderTickDeck[blockCount]->show();
	}

    channelIndex++;

    if (blockCount == 0 && _useGroupLabels == False)
	{
	if (useSamplingRateOptionMenus == False)
	    {	
	    VkDeck *deck = new VkDeck("deck", _block[blockCount]);
	    XtVaSetValues(deck->baseWidget(),
			    XmNresizeHorizontal,    False,
			    XmNrecomputeSize,	    False,
			    XmNrow,		    2, 
			    XmNcolumn,		    0, 
			    XmNgravity,		    CenterGravity, 
			    NULL);

	    // set up with widest label anticipated 
	    XmString label;
	    if (_canDetermineDigitalInputRate == True)
		label = XmStringCreateSimple(VkGetResource("LongestSamplingRateStringDigital", ApplicationClassName));
	    else
		label = XmStringCreateSimple(VkGetResource("LongestSamplingRateString", ApplicationClassName));
	    _samplingRateLabel[blockCount] = XtVaCreateManagedWidget("samplingRateLabel1",
						    xmLabelWidgetClass,
						    deck->baseWidget(), 
						    XmNmarginRight,	    0, 
						    XmNrightOffset,	    0, 
						    XmNalignment,	    XmALIGNMENT_CENTER, 
						    XmNborderWidth,	    0, 
						    XmNmarginWidth,	    0, 
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
   }

gangSliders(True);
for (i = 0; i < _channelCapacity; i++)
    {
/* initialize slider&gang values */
    XmScaleSetValue(_slider[i], _gangValue[i]);

/* add callbacks to sliders */
    XtAddCallback(_slider[i],
		    XmNvalueChangedCallback,
		    &InputControls::levelSliderCB,
		    (XtPointer) this); 
    XtAddCallback(_slider[i],
		    XmNdragCallback,
		    &InputControls::levelSliderCB,
		    (XtPointer) this); 

// initialize meters 
    if (_useLevelMeters == True)
	{
	_meter[i]->show();
	setMeter(i, 109, 109);
	}
    }
// set tick mark type
setSliderTickMarkType(_sliderTickMarkType);
} /* ---- end InputControls() ---- */

/* ******************************************************************
 * ~InputControls: class destructor
 * ****************************************************************** */
InputControls::~InputControls() 
{
} /* ---- end ~InputControls() ---- */

/* ******************************************************************
 * className: return class namd
 * ****************************************************************** */
    const char * 
InputControls::className() 
{
return ("InputControls");
} /* ---- end className() ---- */

/* ******************************************************************
 * levelSliderCB:    graphics callback to
 *					    handle slider changes
 * ****************************************************************** */
    void 
InputControls::levelSliderCB(Widget w, XtPointer clientData, XtPointer callData) 
{ 
((InputControls *) clientData)->levelSliderMoved(w, callData);
} /* ---- end levelSliderCB() ---- */

/* ******************************************************************
 * levelSliderMoved:
 * ****************************************************************** */
    void 
InputControls::levelSliderMoved(Widget masterSlider, XtPointer) 
{
long	pv[2*MAX_INPUT_CHANNELS];
long	alParameterName[MAX_INPUT_CHANNELS];
int	abacus;
int	masterSliderPosition, slaveSliderPosition;
int	masterChannel, isSlaveChannel[MAX_INPUT_CHANNELS];

/* convert slider value to AL range and send to audio hardware */
for (int i = 0; i < _channelCapacity; i++)
    {
    if (masterSlider == _slider[i])
	{
	isSlaveChannel[i] = False;
	masterChannel = i;
	}
    else
	isSlaveChannel[i] = True;
    }

alParameterName[0] = AL_LEFT_INPUT_ATTEN;
alParameterName[1] = AL_RIGHT_INPUT_ATTEN;
alParameterName[2] = AL_LEFT2_INPUT_ATTEN;
alParameterName[3] = AL_RIGHT2_INPUT_ATTEN;
abacus = 0;
pv[abacus++] = alParameterName[masterChannel];

// slider range is 0 .. 100 
XmScaleGetValue(masterSlider, &masterSliderPosition);

/* convert slider value to Audio Library parameter range and send to audio hardware */
_dacValue[masterChannel] = HardwareLevelFromSliderValue(masterSliderPosition);
pv[abacus++] = _dacValue[masterChannel];

/* if gang enabled, make both faders move w/gang behavior */
if (_gangState == True) 
    {
    for (i = 0; i < _channelCapacity; i++)
	{
	if (isSlaveChannel[i] == True && i != masterChannel)
	    {
	/* slave slider position = master slider position + difference in
		values when master and slave sliders were ganged */
	    slaveSliderPosition = masterSliderPosition + 
			(_gangValue[i] - _gangValue[masterChannel]);
	
	/* bound (required !!!!) to range [0..100] and set slave slider value */
	    if	    (slaveSliderPosition < 0)
		slaveSliderPosition = 0;
	    else if (slaveSliderPosition > 100)
		slaveSliderPosition = 100;
	    XmScaleSetValue(_slider[i], slaveSliderPosition);
	
	/* determine value to write to Audio Hardware */
	    pv[abacus++] = alParameterName[i];
	    _dacValue[i] = HardwareLevelFromSliderValue(slaveSliderPosition);
	    pv[abacus++] = _dacValue[i];
	    }
	}
    }
ALsetparams(AL_DEFAULT_DEVICE, pv, abacus);
} /* ---- end levelSliderMoved() ---- */

/* ******************************************************************
 * setLevelSliderPosition:
 * ****************************************************************** */
    void 
InputControls::setLevelSliderPosition(int channelNumber, long audioHardwareValue) 
{
int sliderValue = SliderValueFromHardwareLevel(audioHardwareValue);
/* slider value is bound to range [0 .. 100] in SliderValueFromHardwareLevel() */
XmScaleSetValue(_slider[channelNumber], sliderValue);
_dacValue[channelNumber] = audioHardwareValue;
} /* ---- end setLevelSliderPosition() ---- */

/* ******************************************************************
 * enableLevelSliders:	toggle sensitivity of sliders
 * ****************************************************************** */
    void 
InputControls::enableLevelSliders(Boolean enableStatus) 
{
for (int i = 0; i < _channelCapacity; i++)
    XtSetSensitive(_slider[i], enableStatus);
} /* ---- end enableLevelSliders() ---- */

/* ******************************************************************
 * setSourceLabel:    update input source label 
 * ****************************************************************** */
    void 
InputControls::setSourceLabel(int labelIndex) 
{
XmString    label;

switch (labelIndex)
    {
    case AL_INPUT_MIC:
    /* enable level sliders */
	enableLevelSliders(True);

	label = XmStringCreateSimple(VkGetResource("inputSourceMicrophone", ApplicationClassName));
    break;
    case AL_INPUT_LINE:
    /* enable level sliders */
	enableLevelSliders(True);

	label = XmStringCreateSimple(VkGetResource("inputSourceLine", ApplicationClassName));
    break;
    case AL_INPUT_DIGITAL:
    /* disable level sliders */
	enableLevelSliders(False);

	label = XmStringCreateSimple(VkGetResource("inputSourceDigital", ApplicationClassName));
    break;
    }

if (label)
    {
    XtVaSetValues(_inputSourceLabel, XmNlabelString, label, NULL);
    XmStringFree(label);
    }
} /* ---- end setSourceLabel() ---- */

/* ******************************************************************
 * gangSliders:
 * ****************************************************************** */
    void 
InputControls::gangSliders(Boolean slidersGanged)
{
int	i;
long    pv[8];
pv[0] = AL_LEFT_INPUT_ATTEN;
pv[2] = AL_RIGHT_INPUT_ATTEN;
pv[4] = AL_LEFT2_INPUT_ATTEN;
pv[6] = AL_RIGHT2_INPUT_ATTEN;
ALgetparams(AL_DEFAULT_DEVICE, pv, 8);

_gangState = slidersGanged;
if (_gangState)
    {
    for (i = 0; i < _channelCapacity; i++)
	{ 
// PREVIOUS
//	int	sliderValue;
//	XmScaleGetValue(_slider[i], &sliderValue);
//	_gangValue[i] = sliderValue;

	_dacValue[i] = pv[i*2 + 1];
	_gangValue[i] = SliderValueFromHardwareLevel(_dacValue[i]);
	XmScaleSetValue(_slider[i], _gangValue[i]);
	}
    }
} /* ---- end gangSliders() ---- */

/* ******************************************************************
 * setMeter:
 * ****************************************************************** */
    void 
InputControls::setMeter(int channelNumber, int shortTimeValue, int longTimeValue)
{  
if (_useLevelMeters == True)
    _meter[channelNumber]->setValue(shortTimeValue, longTimeValue);
} /* ---- end setMeter() ---- */

/* ******************************************************************
 * showMeters:
 * ****************************************************************** */
    void 
InputControls::showMeters(Boolean showTheMeters)
{
/* if meters are to be shut off, erase values sitting in meter display */
if ((_useLevelMeters == True)&&(showTheMeters == False))
    {
    for (int i = 0; i < _channelCapacity; i++)
	setMeter(i, 109, 109);
    }
} /* ---- end showMeters() ---- */

/* ******************************************************************
 * HardwareLevelFromSliderValue:	
 *					determine Audio Library input
 *					attenuation parameter value
 *					from slider value	
 * ****************************************************************** */
    long 
InputControls::HardwareLevelFromSliderValue(int sliderValue)
{
float value = 0.01*((float)(100 - sliderValue));

/* slider value assumed to be in range 1 .. 0,  w/1 corresponding
    to minimum attenuation and 0 corresponding to maximum attenuation */
/* numAttenuationSteps == 40 */
int index = (int) rint(value * (numAttenuationSteps - 1));

/* Six-bit MDAC value is passed through HDSP driver in
 * six most significant bits of least significant byte of a
 * 32-bit word hence the left shift.
 */
long someLong = ((long)(sliderToAttenuation[index]))<<2;
return (someLong);
} /* ---- end HardwareLevelFromSliderValue() ---- */

/* ******************************************************************
 * DecibelsToScale:	
 * ****************************************************************** */
    float 
InputControls::DecibelsToScale(float db)
{
if	(db <= float(lastLinearAttenuationStep) * 1.5) 
    return (db/(1.5*(numAttenuationSteps-1)));
else if (db < sliderToAttenuation[lastLinearAttenuationStep+1]*1.5)
    return float(lastLinearAttenuationStep)/(numAttenuationSteps - 1);
else if (db < sliderToAttenuation[lastLinearAttenuationStep+2]*1.5)
    return float(lastLinearAttenuationStep+1)/(numAttenuationSteps - 1);
else if (db < sliderToAttenuation[lastLinearAttenuationStep+3]*1.5)
    return float(lastLinearAttenuationStep+2)/(numAttenuationSteps - 1);
else if (db < 62 * 1.5)
    return float(lastLinearAttenuationStep+3)/(numAttenuationSteps - 1);
else 
    return (1);
} /* ---- end DecibelsToScale() ---- */

/* ******************************************************************
 * SliderValueFromHardwareLevel:	
 * ****************************************************************** */
    int 
InputControls::SliderValueFromHardwareLevel(long hardwareLevel)
{
/* MDAC increment is 1.5 db per step */
/* FIXXXXX: bet this is the cause of slider stuttering */
int sliderValue = 100 - (int) (100*DecibelsToScale(1.5*((float)(hardwareLevel>>2))));

/* bound slider range to [0..100] */
if (sliderValue < 0)
    sliderValue = 0;
else if (sliderValue > 100)
    sliderValue = 100;
 
return (sliderValue);
} /* ---- end SliderValueFromHardwareLevel() ---- */

/* ******************************************************************
 * optionMenuCB:	
 * ****************************************************************** */
    void 
InputControls::optionMenuCB(Widget w, XtPointer, XtPointer)
{
long    alParameters[2];

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
	alParameters[0] = AL_INPUT_RATE;
	alParameters[1] = index;
	ALsetparams(AL_DEFAULT_DEVICE, alParameters, 2);
    }

/* don't need to call ComputeMeterShortBlockLength() here because audio 
hardware state poll will handle it */ 
} /* ---- end optionMenuCB() ---- */

/* ******************************************************************
 * setSamplingRateLabel:    update label reflecting
 *					    input sampling rate
 * ****************************************************************** */
    void 
InputControls::setSamplingRateLabel(int samplingRate, int digitalSamplingRate) 
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
	sprintf(s1, "%g %s", value, VkGetResource("kHz", ApplicationClassName));
	}
    }

// assemble "Digital xxx kHz" string for sampling rate
else if (samplingRate >= AL_RATE_AES_1s && samplingRate <= AL_RATE_AES_1)
    {
/*
 * the following for machines that can determine the digital input
 * sampling rate
 */
/* exclusion of rate 0 Hz compensates for driver's digital input rate
    determination bogus state of 0 Hz */
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
} /* ---- end setSamplingRateLabel() ---- */

/* ******************************************************************
 *  setSliderTickMarkType:    set slider tick mark type
 * ****************************************************************** */
    void 
InputControls::setSliderTickMarkType(char type) 
{
_sliderTickMarkType = type;
for (int j = 0; j < _blockCapacity; j++)
     _sliderTickDeck[j]->pop(_tickMarks[j][type]);
} /* ---- end setSliderTickMarkType() ---- */

/* ******************************************************************
 *  setListeningLED:    set listening LED state
 * ****************************************************************** */
    void 
InputControls::setListeningLED(Boolean state) 
{
/* ON:  set to Schemes check mark color */
if (state)
    {
    Pixel color = (Pixel) VkGetResource(_listeningLED, 
				    "checkColor", 
				    "CheckColor",
				    XmRPixel, "checkColor");
    XtVaSetValues(_listeningLED, XmNbackground, color, NULL);
    }
/* OFF:  set to Schemes alternate background color */
else
    {
    Pixel color = (Pixel) VkGetResource(_listeningLED, 
				    "alternateBackground5", 
				    "AlternateBackground5",
				    XmRPixel, "alternateBackground5");
    XtVaSetValues(_listeningLED, XmNbackground, color, NULL);
    }
} /* ---- end setListeningLED() ---- */

/* ******************************************************************
 *  showListeningLED:    show LED state
 * ****************************************************************** */
    void 
InputControls::showListeningLED(Boolean state) 
{
_useListeningLED = state;
if (state)
    {
/* get listening LED to butt up against input title */
    XtVaSetValues(_inputSourceTitle,
		    XmNleftSpring,	0,						
		    NULL);

    XtManageChild(_listeningLEDFrame);

    XtVaSetValues(_listeningLED, XmNwidth, 4, NULL);
    }
else 
    {
/* let input title spring back */
    XtVaSetValues(_inputSourceTitle,
		    XmNleftSpring,	50,						
		    NULL);
    XtUnmanageChild(_listeningLEDFrame);
    }
} /* ---- end showListeningLED() ---- */

/* ******************************************************************
 *  createListeningLED:    create listening LED 
 * ****************************************************************** */
    void 
InputControls::createListeningLED() 
{
_listeningLEDFrame = XtVaCreateWidget("listeningLEDFrame",
					xmFrameWidgetClass,
					 _blockLabel[0], 
					XmNleftSpring,		50,
					XmNrightSpring,		0,						
					XmNresizeHorizontal,	False,						
					XmNhorizontalSpring,	0,						
					XmNmarginTop,		1, 
					XmNmarginBottom,	1, 
					 NULL);
_listeningLED = XtVaCreateManagedWidget("listeningLED",
					xmDrawingAreaWidgetClass,
					 _listeningLEDFrame, 
					XmNmarginTop,		0, 
					XmNmarginBottom,	0, 
					XmNmarginLeft,		0, 
					XmNmarginRight,		0, 
					XmNmarginHeight,	0, 
					XmNmarginWidth,		0, 
					XmNborderWidth,		0, 
					 NULL);
} /* ---- end createListeningLED() ---- */
