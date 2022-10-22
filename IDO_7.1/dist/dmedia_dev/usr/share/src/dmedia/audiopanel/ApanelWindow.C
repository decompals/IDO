/* **************************************************************
 *    Original contributors: Terry Weissman, Bruce Karsh, Doug Cook,
 *	Gints Klimanis, Amit Shoham, Marc Callow, 
 *    ViewKit version since IRIX 5.2 by Gints Klimanis
 *				1991-5
 * ************************************************************** */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h> 
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <Xm/FileSB.h>
#include <Xm/RowColumn.h> 
#include <Xm/Scale.h> 
#include <Xm/ToggleB.h> 
#include <Xm/MwmUtil.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>

#include <Sgm/Grid.h> 
#include <Sgm/SpringBox.h> 

#include <Vk/VkApp.h>
#include <Vk/VkFileSelectionDialog.h>
#include <Vk/VkFormat.h>
#include <Vk/VkMenuItem.h>
#include <Vk/VkMenuBar.h>
#include <Vk/VkResource.h>
#include <Vk/VkWarningDialog.h>

#include "ApanelWindow.h"

////////////////////////////////////////////////////////////////////////////////
// Menubar  description
////////////////////////////////////////////////////////////////////////////////

VkMenuDesc  ApanelWindow::menuBarDescription[5] = {
  { SUBMENU,	    "menuTitleFile",	NULL, fileMenuDescription},
  { RADIOSUBMENU,   "menuTitleInput",	NULL, inputSourcesMenuDescription},
};

VkMenuDesc ApanelWindow::fileMenuDescription[] = {
  { ACTION,   "fileMenuItemOpen",   &openParameterFileCB },
  { ACTION,   "fileMenuItemSave",   &saveParameterFileCB },
  { ACTION,   "fileMenuItemSaveAs", &saveAsParameterFileCB },
  { SEPARATOR },
  { ACTION,   "fileMenuItemQuit",   &quitCB },
  { END},
};

VkMenuDesc ApanelWindow::inputSourcesMenuDescription[] = {
  { TOGGLE, "inputSourceMicrophone",&inputSourceCB },
  { TOGGLE, "inputSourceLine",	    &inputSourceCB },
  { TOGGLE, "inputSourceDigital",   &inputSourceCB },
  { END},
};

VkMenuDesc ApanelWindow::samplingRateMenuDescription[] = {
  { RADIOSUBMENU, "inputSamplingRateSubMenu",    NULL, inputSamplingRateMenuDescription},
  { RADIOSUBMENU, "outputSamplingRateSubMenu",   NULL, outputSamplingRateMenuDescription},
  { END},
};

VkMenuDesc ApanelWindow::inputSamplingRateMenuDescription[] = {
  { TOGGLE, "samplingRateLabel1",     &inputSamplingRateCB },
  { TOGGLE, "samplingRateLabel2",     &inputSamplingRateCB },
  { TOGGLE, "samplingRateLabel3",     &inputSamplingRateCB },
  { TOGGLE, "samplingRateLabel4",     &inputSamplingRateCB },
  { TOGGLE, "samplingRateLabel5",     &inputSamplingRateCB },
  { TOGGLE, "menuSamplingRateLabel6", &inputSamplingRateCB },
  { TOGGLE, "samplingRateLabel7",     &inputSamplingRateCB },
  { TOGGLE, "samplingRateLabel8",     &inputSamplingRateCB },
  { END},
};

VkMenuDesc ApanelWindow::outputSamplingRateMenuDescription[] = {
  { TOGGLE, "samplingRateLabel1",     &outputSamplingRateCB },
  { TOGGLE, "samplingRateLabel2",     &outputSamplingRateCB },
  { TOGGLE, "samplingRateLabel3",     &outputSamplingRateCB },
  { TOGGLE, "samplingRateLabel4",     &outputSamplingRateCB },
  { TOGGLE, "samplingRateLabel5",     &outputSamplingRateCB },
  { TOGGLE, "menuSamplingRateLabel6", &outputSamplingRateCB },
  { TOGGLE, "samplingRateLabel7",     &outputSamplingRateCB },
  { TOGGLE, "samplingRateLabel8",     &outputSamplingRateCB },
  { TOGGLE, "samplingRateLabel9",     &outputSamplingRateCB },
  { END},
};

VkMenuDesc ApanelWindow::optionsMenuDescription[11] = {
  { ACTION, "launchRBView",		  &launchRBViewCB },
  { TOGGLE, "independentInputSliders",    &gangInputSlidersCB },
  { TOGGLE, "independentOutputSliders",   &gangOutputSlidersCB },
  { TOGGLE, "decibelScale",		  &decibelScaleCB },
  { TOGGLE, "listeningLED",		  &listeningLEDCB },
  { END},
};

// input meter ring buffer size 
// max sampling rate * MAX_INPUT_CHANNELS / min meter update rate 
// extra 2 in MAX_IN_RING_BUFFER_FRAMES for new double length queue
#define MAX_IN_RING_BUFFER_FRAMES	2*MAXIMUM_SAMPLING_RATE/DEFAULT_METER_UPDATE_RATE	
#define MAX_IN_RING_BUFFER_SAMPLES	MAX_IN_RING_BUFFER_FRAMES*MAX_INPUT_CHANNELS	

int abacus = 0;

/* ******************************************************************
 * ApanelWindow:	class constructor to create audiopanel 
 *				object displayed in top-level window
 * ****************************************************************** */
ApanelWindow::ApanelWindow (const char	*windowName,
			    const char	*windowTitle,
			    const char	*windowIconTitle,
			    int		inputChannelCapacity,
			    int		outputChannelCapacity,
			    int		overallChannelCapacity,
			    Boolean	haveStereoMicrophoneAbility,
			    Boolean	canChangeChannelMode,
			    Boolean	useSamplingRateOptionMenus,
			    Boolean	showInputListeningLED,
			    Boolean	havePresenterFlatPanelDisplay,
			    Boolean	showPresenter,
			    int		graphicsFD,
			    Boolean	useDisplay) 
			: VkWindow (windowName) 
{
int	i;

// geometry in format widthxheight+x+y.  Saw off width and height 
char *userGeometry = (char *) VkGetResource(_baseWidget, "geometry", "Geometry", XmRString, NULL );
if (userGeometry)
    {
// first '+' character marks start of new string 
    int length = strlen(userGeometry);
    for (i = 0; i < length && userGeometry[i] != '+' && userGeometry[i]; i++) {}
    XtVaSetValues(_baseWidget, XmNgeometry, &userGeometry[i], NULL);
    }

_useDisplay = useDisplay;
_audioInputPort = NULL;
_monitorOn = False;
_muteOn    = False;
_meterOn   = False;

_spinalTapSliderScale    = False;
_decibelInputSliderScale = False;
_inputSlidersGanged      = True;
_outputSlidersGanged     = True;
_listeningLEDOn          = False;

_channelCount = 2;	//  keep this initialization
_microphoneChannelCount = -1;
// set source to non-value, to be set properly in pollAudioHardwareState() 
_inputSource = SOME_BOGUS_INPUT_SOURCE;
_inputSamplingRate = SOME_BOGUS_SAMPLING_RATE;
_inputDigitalSamplingRate = SOME_BOGUS_SAMPLING_RATE;

_useSamplingRateOptionMenus = useSamplingRateOptionMenus;
_fileName = NULL;

_havePresenterFlatPanelDisplay = havePresenterFlatPanelDisplay;
_showPresenter = showPresenter;
_graphicsFD    = graphicsFD;
_presenterWindow = NULL;

_inputChannelCapacity   = inputChannelCapacity;
_outputChannelCapacity  = outputChannelCapacity;
_overallChannelCapacity = overallChannelCapacity;
_haveStereoMicrophoneAbility = haveStereoMicrophoneAbility;
_canChangeChannelMode = canChangeChannelMode;
_useInputMeters  = True;
_useOutputMeters = False;

if (ALgetdefault(AL_DEFAULT_DEVICE, AL_DIGITAL_INPUT_RATE) >= 0)
    _canDetermineDigitalInputRate = True;
else
    _canDetermineDigitalInputRate = False;

// initialize parameters 
alParameters = alParameters1;
oldeAlParameters = alParameters2;
alParameters[0]  = AL_LEFT_SPEAKER_GAIN;
alParameters[2]  = AL_RIGHT_SPEAKER_GAIN;
alParameters[4]  = AL_LEFT_INPUT_ATTEN;
alParameters[6]  = AL_RIGHT_INPUT_ATTEN;
alParameters[8]  = AL_INPUT_RATE;
alParameters[10] = AL_OUTPUT_RATE;
alParameters[12] = AL_INPUT_SOURCE;
alParameters[14] = AL_INPUT_COUNT;
alParameters[16] = AL_MONITOR_CTL;
alParameters[18] = AL_SPEAKER_MUTE_CTL;
alParameters[20] = AL_LEFT2_INPUT_ATTEN;
alParameters[22] = AL_RIGHT2_INPUT_ATTEN;
alParameters[24] = AL_MIC_MODE;
alParameters[26] = AL_CHANNEL_MODE;
alParameters[28] = AL_DIGITAL_INPUT_RATE;

alParameterCount  = DEFAULT_AL_PARAMETER_COUNT;
if (!_canDetermineDigitalInputRate)
    alParameters[28] = AL_INPUT_SOURCE; // XXXwtw - something that'll always work
if (_canChangeChannelMode == False)
    alParameters[26] = AL_INPUT_SOURCE; // XXXwtw - something that'll always work
// set olde parameter to obviously invalid values 
for (i = 0; i < alParameterCount; i += 2)
    {
    oldeAlParameters[i]   = alParameters[i];
    oldeAlParameters[i+1] = -100;
    }
// due to over-complicated listening LED logic, need to initialize this value 
alParameters[15]     = 0;
oldeAlParameters[15] = 0;

windowWidthTwoChannelMode  = atoi(VkGetResource("minimumWindowWidth2Channel", ApplicationClassName)),
windowWidthFourChannelMode = atoi(VkGetResource("minimumWindowWidth4Channel", ApplicationClassName)),

// remove resize utility from window border and window border menu 
XtVaSetValues(_baseWidget, 
	    XmNmwmDecorations,	    MWM_DECOR_ALL | MWM_DECOR_RESIZEH | MWM_DECOR_MAXIMIZE, 
	    XmNmwmFunctions,	    MWM_FUNC_ALL | MWM_FUNC_RESIZE | MWM_FUNC_MAXIMIZE | MWM_FUNC_QUIT_APP, 
	    XmNminWidth,	    windowWidthTwoChannelMode,
	    XmNmarginBottom,	    0, 
	    XmNmarginTop,	    0, 
	    XmNbottomOffset,	    0, 
	    XmNtopOffset,	    0, 
	    XmNmarginHeight,	    0, 
	    NULL );

// install callback to guard against unexpected widget destruction 
installDestroyHandler();

if (_useDisplay == True)
    {
// set icon name (may be different from window name) 
    if (!windowTitle)
	_windowTitle = strdup(ApplicationClassName);
    else
	_windowTitle = strdup(windowTitle);
    theWarningDialog->setTitle(windowTitle);
    setTitle(windowTitle);
    setIconName(windowIconTitle);
    

// add Rate menu title in the absence of the rate option menus 
int menuIndex = 2;
if (False == _useSamplingRateOptionMenus)
    {
    menuBarDescription[menuIndex].menuType  = SUBMENU;
    menuBarDescription[menuIndex].name      = "menuTitleRate";
    menuBarDescription[menuIndex].callback  = NULL;
    menuBarDescription[menuIndex++].submenu = samplingRateMenuDescription;
    }
menuBarDescription[menuIndex].menuType  = SUBMENU;
menuBarDescription[menuIndex].name      = "menuTitleOptions";
menuBarDescription[menuIndex].callback  = NULL;
menuBarDescription[menuIndex++].submenu = optionsMenuDescription;

menuBarDescription[menuIndex].menuType = END;

menuIndex = 5;
// add four channel menu item to Options menu 
// add single separator between gang slider items and channel mode, 
//	microphone mode 
if (_overallChannelCapacity == 4 || _haveStereoMicrophoneAbility == True || 
	_havePresenterFlatPanelDisplay == True)
    {
    optionsMenuDescription[menuIndex++].menuType = SEPARATOR;
    if (_overallChannelCapacity == 4)
	{
	optionsMenuDescription[menuIndex].menuType   = TOGGLE;
	optionsMenuDescription[menuIndex].name       = "fourChannelMode";
	optionsMenuDescription[menuIndex++].callback = &fourChannelModeCB;
	}
// Options menu: add stereo microphone menu item 
    if (_haveStereoMicrophoneAbility == True)
	{
	optionsMenuDescription[menuIndex].menuType   = TOGGLE;
	optionsMenuDescription[menuIndex].name       = "stereoMicrophone";
	optionsMenuDescription[menuIndex++].callback = &stereoMicrophoneCB;
	}

// Options menu: add flat panel display menu item  *
    if (_havePresenterFlatPanelDisplay == True)
	{
	optionsMenuDescription[menuIndex++].menuType = SEPARATOR;

	optionsMenuDescription[menuIndex].name       = "flatPanelDisplay";
	optionsMenuDescription[menuIndex].menuType   = ACTION;
	optionsMenuDescription[menuIndex++].callback = &presenterPanelDisplayCB;
	}
    }

// create menu bar 
    optionsMenuDescription[menuIndex].menuType = END;

    setMenuBar(menuBarDescription);
    
// set some menu items to default state 
    _optionsMenuTitle = (VkMenu *) menu()->findNamedItem("menuTitleOptions");
    _inputMenuTitle = (VkMenu *) menu()->findNamedItem("menuTitleInput");
    if (False == _useSamplingRateOptionMenus)
	{
	_inputSamplingRateSubMenu = (VkMenu *) 
				menu()->findNamedItem("inputSamplingRateSubMenu");
	_outputSamplingRateSubMenu = (VkMenu *) 
				menu()->findNamedItem("outputSamplingRateSubMenu");

// set input rate submenu item digital, because if audiopanel starts up with digital input
//    source and digital input rate, poll routine DOESN'T set menu item 
	VkMenuToggle *menuItem = (VkMenuToggle *) 
		    _inputSamplingRateSubMenu->findNamedItem("samplingRateLabel8");
	menuItem->setVisualState(True);
	}

    VkMenuToggle *menuItem;
    menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("independentInputSliders");
    menuItem->setVisualState(False);
    menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("independentOutputSliders");
    menuItem->setVisualState(False);  

    menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("listeningLED");
    menuItem->setVisualState(False);  

    menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("decibelScale");
    menuItem->setVisualState(False);  

    if (_overallChannelCapacity > 2)
	{
	_fourChannelModeMenuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("fourChannelMode");
	
	}
// disable launch of rbview on IP20 systems
    else
	_optionsMenuTitle->findNamedItem("launchRBView")->deactivate();  

    if (_haveStereoMicrophoneAbility == True)
	_stereoMicrophoneModeMenuItem = (VkMenuToggle *) 
	    _optionsMenuTitle->findNamedItem("stereoMicrophone");

// create graphics widgets 
    _grid = XtVaCreateManagedWidget( "gridWidget",
				     sgGridWidgetClass,
				     mainWindowWidget(),
				     XmNnumRows, 		2, 
				     XmNnumColumns,		2,     
					XmNmarginBottom,	0, 
					XmNmarginTop,		0, 
					XmNbottomOffset,	0, 
					XmNtopOffset,		0, 
					XmNmarginHeight,	0,    
					XmNmarginLeft,		0, 
					XmNmarginRight,		0, 
					XmNleftOffset,		0, 
					XmNrightOffset,		0, 
					XmNmarginWidth,		0, 
					XmNborderWidth,		0,     
				     NULL );
    
    
// create input and output controls:  sliders and meters 
    _inputControls = new InputControls( "inputControls",
					_grid, 
					_inputChannelCapacity/2, 
					_inputChannelCapacity, 
					_useInputMeters,
					_canDetermineDigitalInputRate,
					_useSamplingRateOptionMenus);
    XtVaSetValues( _inputControls->baseWidget(),
		  XmNrow,	    0, 
		  XmNcolumn,	    0, 
		  NULL );
    _inputControls->show();
    
    _outputControls = new OutputControls( "outputControls", 
					_grid,
    				    1 /*_outputChannelCapacity/2*/ , 
    				    2 /*_outputChannelCapacity*/, 
					_useOutputMeters,
					_canDetermineDigitalInputRate,
					_useSamplingRateOptionMenus,
					_havePresenterFlatPanelDisplay,
					_graphicsFD,
					&_presenterWindow);
    XtVaSetValues( _outputControls->baseWidget(),
		  XmNrow,	    0, 
		  XmNcolumn,	    1, 
		  NULL );
    _outputControls->show();    
    
// create toggle buttons:  meter, monitor, mute 
    int toggleColumnCount = 1;
    if (_useInputMeters)
	toggleColumnCount++;
    _toggleButtonContainer = XtVaCreateManagedWidget( "toggleButtonContainer",
						     sgSpringBoxWidgetClass,
						     _grid,
						      XmNrow,		1, 
						      XmNcolumn,	0, 
						    XmNmarginBottom,	0, 
						    XmNbottomOffset,	0, 
						    XmNmarginHeight,	0, 		    
						     NULL );
    int columnIndex = 0;    
    if (_useInputMeters)
	{
// NULL out ptrs for tables used by metering routines, 
//	    allocated on first use of meters 
	_levelToDecibelTable = NULL;	 
#ifdef DC_FILTER_BOX   
	for (i = 0; i < MAX_INPUT_CHANNELS; i++)
	    boxBuffer[i] = NULL;
#endif

// specify resizability.  Need to set XmNresizeHorizontal
//	to false so labels aren't spaced away from toggle buttons 
	_meterToggle = XtVaCreateManagedWidget( "meterToggle",
						xmToggleButtonWidgetClass,
						_toggleButtonContainer,
						XmNresizeHorizontal,		False, 
						XmNrow, 			0, 
						XmNcolumn, 			columnIndex, 
						XmNmarginBottom,		0, 
						XmNbottomOffset,		0, 
						XmNmarginHeight,		0, 
						NULL );
	XtAddCallback( _meterToggle, 
			XmNvalueChangedCallback,    &ApanelWindow::showMeterCB,
			(XtPointer) this); 
	columnIndex++;
	}
    
// specify resizability.  Need to set XmNresizeHorizontal
//	to false so labels aren't spaced away from toggle buttons 
    _monitorToggle = XtVaCreateManagedWidget( "monitorToggle",
					    xmToggleButtonWidgetClass,
					    _toggleButtonContainer,
					    XmNresizeHorizontal,	False, 
					    XmNrow, 		0, 
					    XmNcolumn, 		columnIndex, 
					    XmNmarginBottom,	0, 
					    XmNbottomOffset,	0, 
					    XmNmarginHeight,	0, 
// set right spring to 0 to get buttons to move to center of layout
					    XmNrightSpring,	0, 
					    NULL );
    XtAddCallback(_monitorToggle, 
		    XmNvalueChangedCallback,    &ApanelWindow::enableMonitorCB,
		    (XtPointer) this); 
    columnIndex++;
    
// to line this button up with output controls, add to main grid widget 
// specify resizability.  Need to set XmNresizeHorizontal
//	to false so labels aren't spaced away from toggle buttons 
    _toggleButtonContainer2 = XtVaCreateManagedWidget( "toggleButtonContainer",
						     sgSpringBoxWidgetClass,
						     _grid,
						      XmNrow,		1, 
						      XmNcolumn,	1, 
						    XmNmarginBottom,	0, 
						    XmNbottomOffset,	0, 
						    XmNmarginHeight,	0, 		    
						     NULL );

    _muteToggle = XtVaCreateManagedWidget("muteToggle",
					    xmToggleButtonWidgetClass,
					    _toggleButtonContainer2,
					    XmNresizeHorizontal,    False, 
					    XmNmarginBottom,	    0, 
					    XmNbottomOffset,	    0, 
					    XmNmarginHeight,	    0, 
					    NULL );
    XtAddCallback(_muteToggle, 
		    XmNvalueChangedCallback, &ApanelWindow::enableMuteCB,
		    (XtPointer) this); 
    
    addView(_grid);		

// set timed callback for meter display (in milliSeconds) 
    if (_useInputMeters)
	{
	_meterShortBlockLength = 0;
	_shortBlocksPerLongBlock = (int)(0.5 + 1.0/(DEFAULT_METER_UPDATE_INTERVAL_VALUE));
	_meterTimerInterval = intervalToMilliseconds(DEFAULT_METER_UPDATE_INTERVAL_VALUE);
	_meterTimer = new VkPeriodic(_meterTimerInterval);
	if (_useInputMeterDCFilter)
	    VkAddCallbackMethod(VkPeriodic::timerCallback,
			    _meterTimer, 
			    this, 
			    ApanelWindow::computeAndRenderMetersDCBoxFilter, 
			    NULL );
	else
	    VkAddCallbackMethod(VkPeriodic::timerCallback,
			    _meterTimer, 
			    this, 
			    ApanelWindow::computeAndRenderMeters, 
			    NULL );
	}

useInputListeningLED(showInputListeningLED);

// set timed callback for audio hardware poll (in milliSeconds) 
    _audioHardwarePollInterval = intervalToMilliseconds(DEFAULT_AUDIO_HARDWARE_POLL_INTERVAL_VALUE);
    _audioHardwarePollTimer    = new VkPeriodic(_audioHardwarePollInterval);
    VkAddCallbackMethod(VkPeriodic::timerCallback,
			_audioHardwarePollTimer, 
			this, 
			ApanelWindow::pollAudioHardwareState, 
			NULL );
    }

XtAddCallback( _meterToggle, 
		XmNvalueChangedCallback,    &ApanelWindow::showMeterCB,
		(XtPointer) this); 
_fileSelectionInfo.obj  = this;
_fileSelectionInfo.reading = False;
theFileSelectionDialog->addCallback(VkDialogManager::prepostCallback,
				    (VkCallbackFunction) ApanelWindow::filePrePostCB,
				    &_fileSelectionInfo);

// show Presenter panel if necessary
if (_havePresenterFlatPanelDisplay)
    CreatePresenterPanel();
if (_showPresenter)
    showPresenterPanelDisplay();
} /* ---- end ApanelWindow() ---- */

/* ******************************************************************
 * className:	return class name
 * ****************************************************************** */
    const char * 
ApanelWindow::className()
{
return "ApanelWindow";
} /* ---- end className() ---- */

/* ******************************************************************
 * ~ApanelWindow:    destructor
 * ****************************************************************** */
ApanelWindow::~ApanelWindow()
{
delete _inputControls;
delete _outputControls;	
delete _presenterWindow;	
} /* ---- end ~ApanelWindow() ---- */

/* ******************************************************************
 * stateChanged:    window iconization state 
 * ****************************************************************** */
    void
ApanelWindow::stateChanged(IconState state)
{
VkWindow::stateChanged(state);

if (state == VkWindow::OPEN)
    {
    pollAudioHardwareState();
    _audioHardwarePollTimer->start(_audioHardwarePollInterval);
    startMeterTimer();
    }
 else
    {
    _audioHardwarePollTimer->stop();
    stopMeterTimer();
    }
} /* ---- end stateChanged() ---- */

/* ******************************************************************
 * update:		
 * ****************************************************************** */
    void 
ApanelWindow::update(VkComponent *component)
{
int	i;

// retrieve commnand line arguments
VkRunOnce *obj = (VkRunOnce*) component;
VkMenuToggle *menuItem;

InputControls  *inPtr	      = (InputControls *) _inputControls;
OutputControls  *outPtr	      = (OutputControls *) _outputControls;
PresenterWindow *presenterPtr = (PresenterWindow *) _presenterWindow;

// parse command line 
int argCount = obj->numArgs();
for (i = 0; i < argCount; i++)
    {
// enable/disable on graphical signal level meters 
// CAREFUL that "meteron" and "meteroff" are checked before shorter
//    string "meter" 
    if (!strcmp(obj->arg(i), "-meteroff"))
	toggleInputMeter(False);

    else if (!strcmp(obj->arg(i), "-meteron") ||
	     !strcmp(obj->arg(i), "-meter"))
	toggleInputMeter(True);

// gang input sliders 
    else if (!strcmp(obj->arg(i), "-ganginput"))
	{
    // set menu item "Independent Input Sliders" to gang state 
	menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("independentInputSliders");
	menuItem->setVisualState(False);
	_inputSlidersGanged = True;
	inPtr->gangSliders(True);
	}

// ungang input sliders 
    else if (!strcmp(obj->arg(i), "-unganginput"))
	{
    // set Options menu item "Independent Input Sliders" to gang state 
	menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("independentInputSliders");
	menuItem->setVisualState(True);
	_inputSlidersGanged = False;
	inPtr->gangSliders(False);
	}

// gang output sliders 
    else if (!strcmp(obj->arg(i), "-gangoutput"))
	{
    // set Options menu item "Independent Output Sliders" to gang state 
	menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("independentOutputSliders");
	menuItem->setVisualState(False);
	_outputSlidersGanged = True;
	outPtr->gangSliders(True);

	if (presenterPtr)
	    presenterPtr->gangLevelSliders(True);
	}

// ungang output sliders 
    else if (!strcmp(obj->arg(i), "-ungangoutput"))
	{
    // set Options menu item "Independent Output Sliders" to gang state 
	menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("independentOutputSliders");
	menuItem->setVisualState(True);
	_outputSlidersGanged = False;
	outPtr->gangSliders(False);

	if (presenterPtr)
	    presenterPtr->gangLevelSliders(False);
	}

// set slider tick mark type to dbscale [-30..+18] (input sliders only) 
    else if (!strcmp(obj->arg(i), "-dbscale"))
	{
	menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("decibelScale");
	menuItem->setVisualState(True);

	_decibelInputSliderScale = True;
	inPtr->setSliderTickMarkType(SCALE_DECIBEL);
	}

// set slider tick mark type to spinal tap [0..11] scale (input AND output sliders) 
    else if (!strcmp(obj->arg(i), "-spinaltap"))
	{
	menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("decibelScale");
	menuItem->setVisualState(False);

	_spinalTapSliderScale = True;
	_decibelInputSliderScale = False;
	inPtr->setSliderTickMarkType(SCALE_SPINAL_TAP);
	outPtr->setSliderTickMarkType(SCALE_SPINAL_TAP);

    // presenter has tick marks in absence of tick deck 
	if (presenterPtr)
	    presenterPtr->setLevelSliderTickMarkType(SCALE_SPINAL_TAP);
	}

// set slider tick mark type to decade scale [0..10] (input AND output sliders) 
    else if (!strcmp(obj->arg(i), "-decadescale"))
	{
	menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("decibelScale");
	menuItem->setVisualState(False);

	_spinalTapSliderScale = False;
	_decibelInputSliderScale = False;
	inPtr->setSliderTickMarkType(SCALE_DECADE);
	outPtr->setSliderTickMarkType(SCALE_DECADE);
    // presenter has tick marks in absence of tick deck 
	if (presenterPtr)
	    presenterPtr->setLevelSliderTickMarkType(SCALE_DECADE);
	}

// iconify main window 
    else if (!strcmp(obj->arg(i), "-iconic"))
	iconify();

// listening LED (this code for second instance) 
    else if (!strcmp(obj->arg(i), "-listeningled") || 
	     !strcmp(obj->arg(i), "-listening")    || 
	     !strcmp(obj->arg(i), "-listen"))
	{
	menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("listeningLED");
	menuItem->setVisualState(True);

	useInputListeningLED(True);
	}

// set audio hardware poll interval 
    else if (!strcmp(obj->arg(i), "-pollInterval"))
	{
    // convert string to float 
	i++;
	if (i >= argCount)
	    fprintf(stderr, "no pollinterval provided.\n"); 
	else
	    {
	    int interval = (int)(1000*atof(obj->arg(i)) + 0.5);
	// ensure poll interval is >= 0 
	    if (interval <= 0)
		fprintf(stderr, "Invalid pollinterval: '%s'.\n", obj->arg(i)); 
	// stop and restart poll routine 
	    else
		{
		_audioHardwarePollTimer->stop();
		_audioHardwarePollInterval = intervalToMilliseconds(atof(obj->arg(i)));
		_audioHardwarePollTimer->start(_audioHardwarePollInterval);
		}
	    }
	}

// read configuration file 
    else if (!strcmp(obj->arg(i), "-openfile"))
	{
	i++;
	if (i >= argCount)
	    fprintf(stderr, "openfile: no file name provided.\n"); 
	else
	    readFullStateFromFile(obj->arg(i));
	}
// write configuration file 
    else if (!strcmp(obj->arg(i), "-savefile"))
	{
	i++;
	if (i >= argCount)
	    fprintf(stderr, "-savefile: no file name provided.\n"); 
	else
	    writeFullStateToFile(obj->arg(i));
	}

// try to open a configuration file 
    else if (access(obj->arg(i), R_OK) != -1)
	readFullStateFromFile(obj->arg(i));
    }
} /* ---- end update() ---- */

/* ******************************************************************
 * afterRealizeHook:		
 * ****************************************************************** */
    void 
ApanelWindow::afterRealizeHook()
{
// set here rather than as app-defaults, because by product of resource set
//    is that extra space is visibly added to window below toggle buttons, 
//    thus unnecessarily increasing window height 
#ifdef SAFE
if (_canChangeChannelMode == True)
    XtVaSetValues(_baseWidget, XmNallowShellResize, True, NULL );
#endif
} /* ---- end afterRealizeHook() ---- */

/* ******************************************************************
 * readFullStateFromFile:  read parameter file and set Audio Hardware and 
 *			application state
 *			    return   0: success, 
 *				    -1: no filename provided
 *				    -2: can't open file
 *				    -3: file contains bad parameter value
 *				    -4: not a file
 * ****************************************************************** */
    int 
ApanelWindow::readFullStateFromFile(char *fileName)
{
// returns 0 if all went well, otherwise return error
int returnCode = LoadAudioHardwareState(fileName);
if (returnCode)
    return (returnCode);

// Yes, good LoadAudioHardwareState() makes it here 
returnCode = readPreferencesFromFile(fileName);
return (returnCode);
} /* ---- end readFullStateFromFile() ---- */

/* ******************************************************************
 * readPreferencesFromFile:  read parameter file and set application 
 *				state only
 *			    return   0: success, 
 *				    -1: no filename provided
 *				    -2: can't open file
 *				    -3: file contains bad parameter value
 *				    -4: not a file
 * ****************************************************************** */
    int 
ApanelWindow::readPreferencesFromFile( char *fileName )
{
FILE	*fd;
char	format[100];
char	item[100];
char	c;

#define SKIP_LINE while (((c=fgetc(fd)) != (char)EOF) && (c != '\n'));
#define BAD_FILE_PARAMETER {\
Print("invalid parameter: '%s'\n", item);\
fclose(fd);\
return(-3);\
}

if (!fileName) 
    return (-1);

// if path is not file or unable to open
struct stat info;
stat(fileName, &info);
if (!S_ISREG(info.st_mode))
    return (-4);

if (!(fd = fopen(fileName, "r"))) 
    return (-2);

// parse file 
// Can not change application class name because of the following line 
// Many users are likely to already have files 
// NOTE space character at beginning of format string.  Don't even think about 
//    removing it. 
sprintf(format," %s*%%s", ApplicationClassName);
int i = 0;
int code;
while ((code = fscanf(fd, format, item)) != EOF) 
    {
#ifdef DEBUG
printf("item='%s' (code=%d)\n", item, code);
#endif
    if (code)
	{
	if	(_useInputMeters &&
		    (!strcmp(item, "initMeter") || !strcmp(item, "initMeterOn")))
		{
		if (_useDisplay == True)
		    toggleInputMeter(True);
		} 
	else if (_useInputMeters && !strcmp(item, "initMeterOff"))
		{
		if (_useDisplay == True)
		    toggleInputMeter(False);
		} 
	else if (!strcmp(item, "initGangInput")) 
		{
	    // set Options menu item "Independent Input Sliders" to gang state 
		if (_useDisplay == True)
		    {
		    VkMenuToggle *menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("independentInputSliders");
		    menuItem->setVisualState(False);
		    _inputControls->gangSliders(True);
		    }
		} 
	else if (!strcmp(item, "initUngangInput")) 
		{
	    // set Options menu item "Independent Input Sliders" to gang state 
		if (_useDisplay == True)
		    {
		    VkMenuToggle *menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("independentInputSliders");
		    menuItem->setVisualState(True);
		    _inputControls->gangSliders(False);
		    }
		} 
	else if (!strcmp(item, "initGangOutput")) 
		{
	    // set Options menu item "Independent Output Sliders" to gang state //
		if (_useDisplay == True)
		    {
		    VkMenuToggle *menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("independentOutputSliders");
		    menuItem->setVisualState(False);
		    _outputControls->gangSliders(True);
    
		    if (_presenterWindow)
			_presenterWindow->gangLevelSliders(True);
		    }
		} 
	else if (!strcmp(item, "initUngangOutput")) 
		{
	    // set Options menu item "Independent Output Sliders" to gang state 
		if (_useDisplay == True)
		    {
		    VkMenuToggle *menuItem = (VkMenuToggle *) _optionsMenuTitle->findNamedItem("independentOutputSliders");
		    menuItem->setVisualState(True);
		    _outputControls->gangSliders(False);
    
		    if (_presenterWindow)
			_presenterWindow->gangLevelSliders(False);
		    }
		} 
    
// NOTE order of decade scale, decibel scale and spinal tap 
//    set menu flag for decadeScale 
	else if (!strcmp(item, "decadeScale")) 
		{
		VkMenuToggle *menuItem = (VkMenuToggle *) 
			_optionsMenuTitle->findNamedItem("decibelScale");
		menuItem->setVisualState(False);
    
		_spinalTapSliderScale    = False;
		_decibelInputSliderScale = False;
		_inputControls->setSliderTickMarkType(SCALE_DECADE);
		_outputControls->setSliderTickMarkType(SCALE_DECADE);
		} 

    // set menu flag for spinalTap 
	else if (!strcmp(item, "spinalTap")) 
		{
		VkMenuToggle *menuItem = (VkMenuToggle *) 
			_optionsMenuTitle->findNamedItem("decibelScale");
		menuItem->setVisualState(False);
		
		_spinalTapSliderScale    = True;
		_decibelInputSliderScale = False;
		_inputControls->setSliderTickMarkType(SCALE_SPINAL_TAP);
		_outputControls->setSliderTickMarkType(SCALE_SPINAL_TAP);
		} 

    // set menu flag for decibelScale 
	else if (!strcmp(item, "decibelScale")) 
		{
		VkMenuToggle *menuItem = (VkMenuToggle *) 
			_optionsMenuTitle->findNamedItem("decibelScale");
		menuItem->setVisualState(True);

		_decibelInputSliderScale = True;
		_inputControls->setSliderTickMarkType(SCALE_DECIBEL);
		} 

    
    // set flag for listeningLED 
	else if (!strcmp(item, "listeningLED")) 
		{
		VkMenuToggle *menuItem = (VkMenuToggle *) 
			_optionsMenuTitle->findNamedItem("listeningLED");
		menuItem->setVisualState(True);
    
		useInputListeningLED(True);
		} 

// show Presenter ?
	else if (_havePresenterFlatPanelDisplay && !strcmp(item, "showPresenter"))
		showPresenterPanelDisplay();
	}

    SKIP_LINE;

    if (i>=60) 
	BAD_FILE_PARAMETER
}

fclose(fd);

return(0);
#undef SKIP_LINE
#undef BAD_FILE_PARAMETER
} /* ---- end readPreferencesFromFile() ---- */

/* ******************************************************************
 * writeFullStateToFile:  write current 
 *			    Audio Hardware state and applciation settings
 *			    to parameter file
 *			    return  0: success
 *				   -1: no filename
 *				   -2: can't open file
 * ****************************************************************** */
    int 
ApanelWindow::writeFullStateToFile(char *fileName)
{
if (!fileName) 
    return(-1);

// try to open file 
FILE *fd = fopen(fileName, "w");
if (!fd) 
    return(-2);

WriteAudioHardwareState(fd, _inputChannelCapacity, _overallChannelCapacity,   
			_haveStereoMicrophoneAbility);

// 
// write User Interface parameters
//
if (_useDisplay == True)
    {
    fprintf(fd,"\n! Personal Options:\n");
    fprintf(fd, "%s*initMeter%s\n", 
	    ApplicationClassName, _meterOn ? "On" : "Off");

    fprintf(fd, "%s*init%sInput\n", 
	    ApplicationClassName, (_inputControls->_gangState) ? "Gang" : "Ungang");
    
    fprintf(fd, "%s*init%sOutput\n", 
	    ApplicationClassName, (_outputControls->_gangState) ? "Gang" : "Ungang");
    
    if (_useInputListeningLED == True)
	fprintf(fd, "%s*listeningLED\n", ApplicationClassName);

    if (_spinalTapSliderScale == True)
	fprintf(fd, "%s*spinalTap\n", ApplicationClassName);
    else
	fprintf(fd, "%s*decadeScale\n", ApplicationClassName);
    if (_decibelInputSliderScale == True)
	fprintf(fd, "%s*decibelScale\n", ApplicationClassName);

// show Presenter by default or not
    if (_havePresenterFlatPanelDisplay && _presenterWindow && 
	    (_presenterWindow->iconic() || _presenterWindow->visible()))
	    fprintf(fd, "%s*showPresenter\n", ApplicationClassName);
    }

fclose(fd);
return (0);
} /* ---- end writeFullStateToFile() ---- */

/* ******************************************************************
 * newParameterFileCB:  create new parameter file
 * ****************************************************************** */
    void 
ApanelWindow::newParameterFileCB(Widget, XtPointer clientData, XtPointer)
{
ApanelWindow *obj = (ApanelWindow *) clientData;
if (obj->_fileName) 
    {
    delete[] obj->_fileName;
    obj->_fileName = NULL;
    }
} /* ---- end newParameterFileCB() ---- */

/* ******************************************************************
 * filePrePostCB:  
 * ****************************************************************** */
    void 
ApanelWindow::filePrePostCB(Widget, XtPointer clientData, XtPointer)
{
CallbackStruct *data = (CallbackStruct *) clientData;
ApanelWindow   *obj  = data->obj;

// if file doesn't exist, delete fileName
if (obj->_fileName && access(obj->_fileName, F_OK) == -1) 
    {
printf("ApanelWindow::filePrePostCB: file doesn't exist\n");
    free(obj->_fileName);
    obj->_fileName = NULL;
    }

// check for home directory 
char *path;
if (!obj->_fileName)
    {
    path = getenv("HOME");
    if (path)
	obj->_fileName = strdup(VkFormat("%s/%s", path, DefaultParameterFileName));
    else
	obj->_fileName = strdup(DefaultParameterFileName);
    }

// set file browser list path and path string 
if (path = GetPathLead(obj->_fileName))
    theFileSelectionDialog->setDirectory(path);
else
    theFileSelectionDialog->setDirectory("./");

// for file open and no existing file, open with directory string in SgFinder
XmString label;
if (data->reading && access(obj->_fileName, F_OK) == -1) 
    label = XmStringCreateSimple(path);
else
    label = XmStringCreateSimple(obj->_fileName);

XtVaSetValues(theFileSelectionDialog->baseWidget(), XmNtextString, label, NULL);
XmStringFree(label);
} /* ---- end filePrePostCB() ---- */

/* ******************************************************************
 * openParameterFileCB:  open parameter file
 * ****************************************************************** */
    void 
ApanelWindow::openParameterFileCB(Widget, XtPointer clientData, XtPointer)
{
ApanelWindow *obj = (ApanelWindow *) clientData;
char	s[1000];

// if file name selected, load file and send values to Audio Hardware 
sprintf(s, "%s: %s", 
	VkGetResource("OpenFile", ApplicationClassName),
	obj->_windowTitle);
theFileSelectionDialog->setTitle(s);
obj->_fileSelectionInfo.reading = True;
if (theFileSelectionDialog->postAndWait() == VkDialogManager::OK)
    {
    char *newFileName = (char *) theFileSelectionDialog->fileName();

    if (newFileName)
	{
    // nuke previous filename 
	if (obj->_fileName)
	    free(obj->_fileName);
    
	obj->_fileName = strdup(newFileName);
    
    // expand path if necessary, shell will do this but constructor parameter 
    // independent of shell
	RemoveSpaces(obj->_fileName);
	if (obj->_fileName && obj->_fileName[0] == '~')
	    {
	    char *tmp = ExpandPath(obj->_fileName);
	    free(obj->_fileName);
	    obj->_fileName = tmp;
	    }
    
	switch (obj->readFullStateFromFile(obj->_fileName))
	    {
	    case 0:
	    break;
	    case -1:
		theWarningDialog->post(VkGetResource("NoFileName", ApplicationClassName), 
					    NULL, obj);
	    break;
	    case -2:
	    default:
		sprintf(s, "%s:\n'%s'\n%s.",
			VkGetResource("UnableToOpenFileForReading", ApplicationClassName),
			obj->_fileName, strerror(errno));
		theWarningDialog->post(s, NULL, obj);
	    break;
	    case -3:
		sprintf(s, "%s:\n'%s'\n",
			VkGetResource("BadParameterInFile", ApplicationClassName),
			obj->_fileName);
		theWarningDialog->post(s, NULL, obj);
	    break;
	    case -4:
		sprintf(s, "'%s'\n%s", 
			obj->_fileName,
			VkGetResource("NotAFile", ApplicationClassName));
		theWarningDialog->post(s, NULL, obj);
	    break;
	    }
	}
    }

// force directory scan 
XmFileSelectionDoSearch(theFileSelectionDialog->baseWidget(), NULL);
} /* ---- end openParameterFileCB() ---- */

/* ******************************************************************
 * saveParameterFileCB:  save parameter file
 * ****************************************************************** */
    void 
ApanelWindow::saveParameterFileCB(Widget w, XtPointer clientData, XtPointer callData)
{
ApanelWindow *obj = (ApanelWindow *) clientData;

// if no file name exists, save with default file name 
if (!obj->_fileName)
    {
    saveAsParameterFileCB(w, clientData, callData);
    return;
    }
else
    {
// expand path if necessary, shell will do this but constructor parameter 
// independent of shell
    RemoveSpaces(obj->_fileName);
    if (obj->_fileName && obj->_fileName[0] == '~')
	{
	char *tmp = ExpandPath(obj->_fileName);
	free(obj->_fileName);
	obj->_fileName = tmp;
	}
    }

// if file name exists, write file 
int error = obj->writeFullStateToFile(obj->_fileName);
if (error)
    {
    char s[1000];
    sprintf(s, "%s: %s", obj->_windowTitle,
	    VkGetResource("SaveAs", ApplicationClassName));
    theFileSelectionDialog->setTitle(s);
       
    switch (error)
	{
	case 0:
	break;
	case -1:
	    theWarningDialog->post(VkGetResource("NoFileName", ApplicationClassName),
					    NULL, obj);
	break;
	case -2:
	    sprintf(s, "%s:\n'%s'\n%s.",
			VkGetResource("UnableToOpenFileForWriting", ApplicationClassName),
			obj->_fileName, strerror(errno));
	    theWarningDialog->post(s, NULL, obj);
	break;
	default:
	    sprintf(s, "%s:\n'%s'\n%s.",
			VkGetResource("FailedToWriteToFile", ApplicationClassName),
			obj->_fileName, strerror(errno));
	    theWarningDialog->post(s, NULL, obj);
	break;
	}
    }

// force directory scan (only if file selection box created) 
if (theFileSelectionDialog->baseWidget())
    XmFileSelectionDoSearch(theFileSelectionDialog->baseWidget(), NULL);
} /* ---- end saveParameterFileCB() ---- */

/* ******************************************************************
 * saveAsParameterFileCB:  save parameter file with name input via file
 *			    browser
 * ****************************************************************** */
    void 
ApanelWindow::saveAsParameterFileCB(Widget, XtPointer clientData, XtPointer)
{
char		s[1000];
ApanelWindow	*obj = (ApanelWindow *) clientData;

// if file name selected, acquire Audio Hardware state and write to file 
sprintf(s, "%s: %s", VkGetResource("SaveAs", ApplicationClassName),
	obj->_windowTitle);
theFileSelectionDialog->setTitle(s);

obj->_fileSelectionInfo.reading = False;
if (theFileSelectionDialog->postAndWait() == VkDialogManager::OK)
    {
    char *newFileName = (char *) theFileSelectionDialog->fileName();
// nuke previous filename
    if (obj->_fileName)
	free(obj->_fileName);
    obj->_fileName = strdup(newFileName);

// expand path if necessary, shell will do this but constructor parameter 
// independent of shell
    RemoveSpaces(obj->_fileName);
// expand '~' character at start of path
    if (newFileName && newFileName[0] == '~')
	newFileName = ExpandPath(newFileName);

    if (newFileName)
	{
	switch (obj->writeFullStateToFile(newFileName))
	    {
	    case 0:
	    break;
	    case -1:
		theWarningDialog->post(VkGetResource("NoFileName", ApplicationClassName), 
						NULL, obj);
	    break;
	    case -2:
		sprintf(s, "%s:\n'%s'\n%s.",
			    VkGetResource("UnableToOpenFileForWriting", ApplicationClassName),
			    newFileName, strerror(errno));
		theWarningDialog->post(s, NULL, obj);
	    break;
	    default:
		sprintf(s, "%s:\n'%s'\n%s.",
			    VkGetResource("FailedToWriteToFile", ApplicationClassName),
			    newFileName, strerror(errno));
		theWarningDialog->post(s, NULL, obj);
	    break;
	    }
	}
    }

// force directory scan 
XmFileSelectionDoSearch(theFileSelectionDialog->baseWidget(), NULL);
} /* ---- end saveAsParameterFileCB() ---- */

/* ******************************************************************
 * quitCB:		exit application
 * ****************************************************************** */
    void 
ApanelWindow::quitCB(Widget, XtPointer, XtPointer)
{
theApplication->terminate(0);
} /* ---- end quitCB() ---- */

/* ******************************************************************
 * fourChannelModeCB: menu callback to toggle channel mode
 * ****************************************************************** */
    void 
ApanelWindow::fourChannelModeCB(Widget, XtPointer, XtPointer callData)
{
long alParameters[2];
XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;

// set audio hardware state 
alParameters[0] = AL_CHANNEL_MODE;
if (cbs->set)
    alParameters[1] = AL_4CHANNEL;
else
    alParameters[1] = AL_STEREO;
ALsetparams(AL_DEFAULT_DEVICE, alParameters, 2);
} /* ---- end fourChannelModeCB() ---- */

/* ******************************************************************
 * stereoMicrophoneCB: menu callback to toggle 
 *					    microphone mode
 * ****************************************************************** */
    void 
ApanelWindow::stereoMicrophoneCB(Widget, XtPointer, XtPointer callData)
{
long alParameters[2];

XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;

// update audio hardware state 
alParameters[0] = AL_MIC_MODE;
if (cbs->set == False) 
    alParameters[1] = AL_MONO;
else
    alParameters[1] = AL_STEREO;
ALsetparams(AL_DEFAULT_DEVICE, alParameters, 2);
} /* ---- end stereoMicrophoneCB() ---- */

/* ******************************************************************
 * presenterPanelDisplayCB: menu callback to show 
 *				Presenter controls window
 * ****************************************************************** */
    void 
ApanelWindow::presenterPanelDisplayCB(Widget, XtPointer clientData, XtPointer)
{
((ApanelWindow *) clientData)->showPresenterPanelDisplay();
} /* ---- end presenterPanelDisplayCB() ---- */

/* ******************************************************************
 * CreatePresenterPanel:  create/show Presenter control panel
 * ****************************************************************** */
    void 
ApanelWindow::CreatePresenterPanel()
{
// create single-instance window 
if (!_presenterWindow)
    {
    char *windowName = Get4DwmTitle(VkGetResource("PresenterAudio", ApplicationClassName));
    char *windowIconName = Get4DwmTitle(VkGetResource("Presenter", ApplicationClassName));
    _presenterWindow = new PresenterWindow(windowName, windowIconName, _graphicsFD);
    }
} /* ---- end CreatePresenterPanel() ---- */

/* ******************************************************************
 * showPresenterPanelDisplay:  create/show Presenter control panel
 * ****************************************************************** */
    void 
ApanelWindow::showPresenterPanelDisplay()
{
if (_havePresenterFlatPanelDisplay)
    {
// create single-instance window 
    if (!_presenterWindow)
	CreatePresenterPanel();

// open if iconified (ViewKit bug: thinks hidden window is also iconified) 
    if (_presenterWindow->iconic())
	_presenterWindow->open();
    
    _presenterWindow->show();
    }
} /* ---- end showPresenterPanelDisplay() ---- */

/* ******************************************************************
 * launchRBViewCB: menu callback to launch 'rbview'
 * ****************************************************************** */
    void 
ApanelWindow::launchRBViewCB(Widget, XtPointer clientData, XtPointer)
{
ApanelWindow *obj = (ApanelWindow *) clientData;

if (!fork())
    {
    if (execl("/usr/sbin/rbview", "rbview", "&", NULL) == -1) 
	{
	theWarningDialog->setTitle(obj->_windowTitle);
	theWarningDialog->post("Failed to launch 'rbview'.");
	}
    }
} /* ---- end launchRBViewCB() ---- */

/* ******************************************************************
 * gangInputSlidersCB: menu callback to toggle 
 *					    input slider gang state
 * ****************************************************************** */
    void 
ApanelWindow::gangInputSlidersCB(Widget, XtPointer clientData, XtPointer callData)
{
XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;
((ApanelWindow *) clientData)->gangInputSliders(!cbs->set);
} /* ---- end gangInputSlidersCB() ---- */

/* ******************************************************************
 * gangInputSliders: toggle input slider gang state
 * ****************************************************************** */
    void 
ApanelWindow::gangInputSliders(char state)
{
_inputSlidersGanged = state;
_inputControls->gangSliders(state);
} /* ---- end gangInputSliders() ---- */

/* ******************************************************************
 * gangOutputSlidersCB:	menu callback to toggle 
 *						output slider gang state
 *						of base audio hardware
 *						AND optional Presenter
 *						audio hardware
 * ****************************************************************** */
    void 
ApanelWindow::gangOutputSlidersCB(Widget, XtPointer clientData, XtPointer callData)
{
XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;
((ApanelWindow *) clientData)->gangOutputSliders(!cbs->set);
} /* ---- end gangOutputSlidersCB() ---- */

/* ******************************************************************
 * gangOutputSliders: toggle input slider gang state
 * ****************************************************************** */
    void 
ApanelWindow::gangOutputSliders(char state)
{
_outputSlidersGanged = state;
_outputControls->gangSliders(state);

// if Presenter flat panel present, toggle its ganging 
if (_presenterWindow)
   _presenterWindow->gangLevelSliders(state);
} /* ---- end gangOutputSliders() ---- */

/* ******************************************************************
 * listeningLEDCB    toggle optional listening LED
 * ****************************************************************** */
    void 
ApanelWindow::listeningLEDCB(Widget, XtPointer clientData, XtPointer callData)
{
XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;
ApanelWindow *obj = (ApanelWindow *) clientData;

obj->useInputListeningLED(cbs->set);
} /* ---- end listeningLEDCB() ---- */

/* ******************************************************************
 * useInputListeningLED    toggle optional listening LED
 * ****************************************************************** */
    void 
ApanelWindow::useInputListeningLED(Boolean state)
{
_useInputListeningLED = state;

// since poll audio code will not reiterate when audio hardware state
//    has not changed, do this to light up LED 
if (_useInputListeningLED)
    {
    if	    (alParameters[15] > (_audioInputPort != NULL)) 
	{
	_listeningLEDOn = True;
	_inputControls->setListeningLED(True);
	}
    else if (alParameters[15] <= (_audioInputPort != NULL))
	{
 	_listeningLEDOn = False;
	_inputControls->setListeningLED(False);
	}
    }

// this statement follows the audio hardware poll to hides a possible color 
//flicker caused by a change in state from the LED create color and the color
//that reflects the current LED state 
_inputControls->showListeningLED(state);
} /* ---- end useInputListeningLED() ---- */

/* ******************************************************************
 * decibelScaleCB    toggle decade and decibel scale
 * ****************************************************************** */
    void 
ApanelWindow::decibelScaleCB(Widget, XtPointer clientData, XtPointer callData)
{
XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;
ApanelWindow *obj = (ApanelWindow *) clientData;
InputControls *inPtr = (InputControls *) obj->_inputControls;

obj->_decibelInputSliderScale = cbs->set;

// (input sliders only) 
// set slider tick mark type to db scale [-30..+18] 
if (cbs->set)
    inPtr->setSliderTickMarkType(SCALE_DECIBEL);
// set slider tick mark type to decade scale [1..10] 
else 
    inPtr->setSliderTickMarkType(SCALE_DECADE);
} /* ---- end decibelScaleCB() ---- */

/* ******************************************************************
 * setInputSliderScale    set input slider tick mark scale type
 * ****************************************************************** */
    void 
ApanelWindow::setInputSliderScale(char type)
{
if	(SCALE_SPINAL_TAP == type)
    _spinalTapSliderScale = True;
else if (SCALE_DECADE == type)
    _spinalTapSliderScale = False;

if	(SCALE_DECIBEL == type)
    _decibelInputSliderScale = True;
else if (SCALE_DECADE == type)
    _decibelInputSliderScale = False;

_inputControls->setSliderTickMarkType(type);
} /* ---- end setInputSliderScale() ---- */

/* ******************************************************************
 * setOutputSliderScale    set output slider tick mark scale type
 * ****************************************************************** */
    void 
ApanelWindow::setOutputSliderScale(char type)
{
if	(SCALE_SPINAL_TAP == type)
    _spinalTapSliderScale = True;
else if (SCALE_DECADE == type)
    _spinalTapSliderScale = False;

_outputControls->setSliderTickMarkType(type);
if (_presenterWindow)
    _presenterWindow->setLevelSliderTickMarkType(type);
} /* ---- end setOutputSliderScale() ---- */

/* ******************************************************************
 * useInputMeterDCFilter    toggle input meter DC filter
 * ****************************************************************** */
    void 
ApanelWindow::useInputMeterDCFilter(Boolean state)
{
//printf("ApanelWindow::useInputMeterDCFilter: start=%d\n", state);

_useInputMeterDCFilter = state;
stopMeterTimer();
_meterTimer->removeAllCallbacks();

if (state)
    VkAddCallbackMethod(VkPeriodic::timerCallback,
		    _meterTimer, 
		    this, 
		    ApanelWindow::computeAndRenderMetersDCBoxFilter, 
		    NULL );
else
    VkAddCallbackMethod(VkPeriodic::timerCallback,
		    _meterTimer, 
		    this, 
		    ApanelWindow::computeAndRenderMeters, 
		    NULL );

startMeterTimer();
} /* ---- end useInputMeterDCFilter() ---- */

/* ******************************************************************
 * inputSamplingRateCB:	menu callback to change 
 *						input sampling rate
 * ****************************************************************** */
    void 
ApanelWindow::inputSamplingRateCB(Widget w, XtPointer, XtPointer)
{
long	alParameters[2];
long	samplingRate = (long) VkGetResource(w, "value", "Value", XmRInt, NULL );

// if parameter valid, change audio hardware state 
switch (samplingRate)
    {
    case AL_RATE_8000:
    case AL_RATE_11025:
    case AL_RATE_16000:
    case AL_RATE_22050:
    case AL_RATE_32000:
    case AL_RATE_44100:
    case AL_RATE_48000:
    case AL_RATE_INPUTRATE:
    case AL_RATE_AES_1:
    case AL_RATE_AES_2:
    case AL_RATE_AES_3:
    case AL_RATE_AES_4:
    case AL_RATE_AES_6:
    case AL_RATE_AES_1s:
	alParameters[0] = AL_INPUT_RATE;
	alParameters[1] = samplingRate;
	ALsetparams(AL_DEFAULT_DEVICE, alParameters, 2);
// don't need to call ComputeMeterShortBlockLength() here because audio 
//hardware state poll will handle it 
    break;
    default:
#ifdef DEBUG
	fprintf(stderr, "ApanelWindow::inputSamplingRateCB(): invalid samplingRate=%d\n", 
		samplingRate);
#endif
    break;
    }
} /* ---- end inputSamplingRateCB() ---- */

/* ******************************************************************
 * outputSamplingRateCB:	menu callback to change 
 *						output sampling rate
 * ****************************************************************** */
    void 
ApanelWindow::outputSamplingRateCB(Widget w, XtPointer, XtPointer)
{
long	pv[2];
long	samplingRate = (long) VkGetResource(w, "value", "Value", XmRInt, NULL );

// if parameter valid, change audio hardware state
switch (samplingRate)
    {
    case AL_RATE_8000:
    case AL_RATE_11025:
    case AL_RATE_16000:
    case AL_RATE_22050:
    case AL_RATE_32000:
    case AL_RATE_44100:
    case AL_RATE_48000:
    case AL_RATE_INPUTRATE:
    case AL_RATE_AES_1:
    case AL_RATE_AES_2:
    case AL_RATE_AES_3:
    case AL_RATE_AES_4:
    case AL_RATE_AES_6:
    case AL_RATE_AES_1s:
	pv[0] = AL_OUTPUT_RATE;
	pv[1] = samplingRate;
	ALsetparams(AL_DEFAULT_DEVICE, pv, 2);
// don't need to call ComputeMeterShortBlockLength() here because audio 
//    hardware state poll will handle it 
    break;
    default:
#ifdef DEBUG
fprintf(stderr, "ApanelWindow::outputSamplingRateCB(): invalid samplingRate=%d\n", 
		samplingRate);
#endif
    break;
    }
} /* ---- end outputSamplingRateCB() ---- */

/* ******************************************************************
 * inputSourceCB:	menu callback to change 
 *					input source
 * ****************************************************************** */
    void 
ApanelWindow::inputSourceCB(Widget w, XtPointer, XtPointer)
{
// NOTE FOR HERE: Don't set internal input source variable.
//    Don't reset DC estimator.  Audio hardware poll routine does both 
// set audio hardware input source state 
long	pv[2];
pv[0] = AL_INPUT_SOURCE;
pv[1] = (int) VkGetResource(w, "value", "Value", XmRInt, NULL);
ALsetparams(AL_DEFAULT_DEVICE, pv, 2);
} /* ---- end inputSourceCB() ---- */

/* ******************************************************************
 * showMeterCB:	toggle button callback for display
 *					of graphical level meter
 * ****************************************************************** */
    void 
ApanelWindow::showMeterCB(Widget, XtPointer clientData, XtPointer callData)
{
ApanelWindow *obj = (ApanelWindow *) clientData;
XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;
Boolean enable = cbs->set;

obj->_meterOn = enable;
obj->stopMeterTimer();
obj->ReconfigureAudioPorts(enable);
if (enable)
    {
// show window warning if unable to open port    
    if (enable && !obj->_audioInputPort)
	{
	obj->_meterOn = False;
	XtVaSetValues(obj->_meterToggle, XmNset, False, NULL );
    
	theWarningDialog->post(VkGetResource("UnableToSecureAudioPortForMetering.", ApplicationClassName));
	}
    else
	obj->startMeterTimer();
    }

// show/hide meters 
obj->_inputControls->showMeters(obj->_meterOn);
} /* ---- end showMeterCB() ---- */

/* ******************************************************************
 * toggleInputMeter:    toggle display of graphical level meters 
 * ****************************************************************** */
    void 
ApanelWindow::toggleInputMeter(Boolean enable)
{
_meterOn = enable;
stopMeterTimer();

// set up Audio Hardware 
ReconfigureAudioPorts(enable);

// show window warning if unable to open port    
if (_meterOn)
    {
    if (!_audioInputPort)
	{
	_meterOn = False;
    // toggle UI button off 
	XtVaSetValues(_meterToggle, XmNset, False, NULL );
    
	theWarningDialog->post(VkGetResource("UnableToSecureAudioPortForMetering", ApplicationClassName));
	}
    else
	{
    // toggle UI button on 
	XtVaSetValues(_meterToggle, XmNset, True, NULL );

    // show/hide meters 
	_inputControls->showMeters(True);
	startMeterTimer();
	}
    }
else
    {
    // toggle UI button off 
	XtVaSetValues(_meterToggle, XmNset, False, NULL );
    // hide meters 
    _inputControls->showMeters(False);
    }
} /* ---- end toggleInputMeter() ---- */

/* ******************************************************************
 * enableMonitorCB:  toggle button callback for monitor
 *				    function
 * ****************************************************************** */
    void 
ApanelWindow::enableMonitorCB(Widget, XtPointer clientData, XtPointer callData)
{
XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;

ApanelWindow *obj = (ApanelWindow *) clientData;
Boolean enabled = cbs->set;
obj->_monitorOn = enabled;

// set audio hardware monitor state 
long pv[2];
pv[0] = AL_MONITOR_CTL;
if (enabled)
    pv[1] = AL_MONITOR_ON;
else
    pv[1] = AL_MONITOR_OFF;
ALsetparams(AL_DEFAULT_DEVICE, pv, 2);
} /* ---- end enableMonitorCB() ---- */

/* ******************************************************************
 * enableMuteCB:	mute toggle button callback
 * ****************************************************************** */
    void 
ApanelWindow::enableMuteCB(Widget, XtPointer clientData, XtPointer callData)
{
XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;
ApanelWindow *obj = (ApanelWindow *) clientData;

Boolean enabled = cbs->set;
obj->_muteOn = enabled;

// update audio hardware mute state 
long pv[2];
pv[0] = AL_SPEAKER_MUTE_CTL;
if (enabled)
    pv[1] = AL_SPEAKER_MUTE_ON;
else
    pv[1] = AL_SPEAKER_MUTE_OFF;
ALsetparams(AL_DEFAULT_DEVICE, pv, 2);

// set Presenter Mute
if (obj->_graphicsFD)
    {
    SetFlatPanelSpeakerMute(obj->_graphicsFD, enabled);
    if (obj->_presenterWindow)
	obj->_presenterWindow->setMuteToggle(enabled);
    }
} /* ---- end enableMuteCB() ---- */

/* ******************************************************************
 * enableMuteButton:	toggle sensitivity of mute button
 *					(grey/ungrey mute)
 * ****************************************************************** */
    void 
ApanelWindow::enableMuteButton(Boolean enable) 
{
XtSetSensitive(_muteToggle, enable);
} /* ---- end enableMuteButton() ---- */

/* ******************************************************************
 * pollAudioHardwareState: acquire current values of audio 
 *				    hardware parameters and update
 *				    graphical user interface.
 *				    Does not poll when audiopanel is iconized
 * ****************************************************************** */
    void 
ApanelWindow::pollAudioHardwareState()
{
int	i;

// Read audio hardware status (comparison between dual buffers)
ALgetparams(AL_DEFAULT_DEVICE, alParameters, alParameterCount);
for (i = 1; i < alParameterCount; i += 2)
    {
    if (alParameters[i] != oldeAlParameters[i])
	break;
    }
if (i >= alParameterCount)
    return;

// switch deck between 2 and 4 channel modes 
if (_canChangeChannelMode == True &&
	_overallChannelCapacity > 2 && _channelCount != alParameters[27])
    {
    _channelCount = alParameters[27];

// reconfigure Audio port and set Options menu item 
    _outputChannelCapacity = 2;
    if	    (_channelCount == 4)
	{
	_inputChannelCapacity = 4;
	_fourChannelModeMenuItem->setVisualState(True);

    // disable speaker level sliders and mute button 
	_outputControls->enableLevelSliders(False);
	enableMuteButton(False);

// set right spring to 50 to get buttons to center in four channel mode
	XtVaSetValues(_monitorToggle, XmNrightSpring, 50, NULL );
	}
    else if (_channelCount == 2)
	{
	_inputChannelCapacity = 2;
	_fourChannelModeMenuItem->setVisualState(False);

    // enable speaker level sliders and mute button 
	_outputControls->enableLevelSliders(True);
	enableMuteButton(True);

// set right spring to 0 to get buttons to center in two channel mode
	XtVaSetValues(_monitorToggle, XmNrightSpring, 0, NULL );
	}
    else
	{
	_inputChannelCapacity = (int) _channelCount;
	_fourChannelModeMenuItem->setVisualState(False);
	}
    ReconfigureAudioPorts(_meterOn);

    XtVaSetValues(_baseWidget, XmNallowShellResize, True, NULL );

// need to change window size.  Try to reduce redraw flicker.
//    setting XmNmappedWhenManaged for _grid causes all _grid widgets to
//	    disappear, big white space, then a reasonable redraw.  
//	    The following attempts to minimize mode switch delay 
InputControls *bufferInputControls;
    if (_inputChannelCapacity > 0 &&((_inputChannelCapacity&0x1) == 0))
	{
	bufferInputControls = new InputControls("inputControls", 
					    _grid,
					    _inputChannelCapacity/2, 
					    _inputChannelCapacity, 
					    _useInputMeters,
					    _canDetermineDigitalInputRate,
					    _useSamplingRateOptionMenus);

// set VkDeck parameters to non-values popped properly by pollAudioHardwareState() 
	_inputSource = SOME_BOGUS_INPUT_SOURCE;
	_inputSamplingRate = SOME_BOGUS_SAMPLING_RATE;
	_inputDigitalSamplingRate = SOME_BOGUS_SAMPLING_RATE;
    
	delete _inputControls;	// delete BEFORE reassignment so child count same
	_inputControls = bufferInputControls;
	XtVaSetValues(_inputControls->baseWidget(),
		      XmNrow,	    0, 
		      XmNcolumn,    0, 
		      NULL );

// set minimum window width here to prevent menu wrapping when
// menu title list width exceeds Motif widget feature width
// Without minWidth, English  menu wraps to 2nd line in two channel mode only
// Without minWidth, Japanese menu wraps to 2nd line in two AND four channel mode
	if (_channelCount == 2)
	    XtVaSetValues(_baseWidget, XmNminWidth, windowWidthTwoChannelMode, NULL );
	else 
	    XtVaSetValues(_baseWidget, XmNminWidth, windowWidthFourChannelMode, NULL );

// configure to previous preferences 
	if	(_decibelInputSliderScale)
	    _inputControls->setSliderTickMarkType(SCALE_DECIBEL);
	else if (_spinalTapSliderScale)
	    _inputControls->setSliderTickMarkType(SCALE_SPINAL_TAP);
	useInputListeningLED(_useInputListeningLED);
	_inputControls->gangSliders(_inputSlidersGanged);
	_inputControls->show();
	}

    XtVaSetValues(_baseWidget, XmNallowShellResize, False, NULL );
    }

// Handle monitor state (set graphics toggle button) 
if	(!_monitorOn && alParameters[17] == AL_MONITOR_ON) 
    {
    _monitorOn = True;
    XtVaSetValues(_monitorToggle, XmNset, _monitorOn, NULL );
    } 
else if (_monitorOn && alParameters[17] == AL_MONITOR_OFF)
    {
    _monitorOn = False;
    XtVaSetValues(_monitorToggle, XmNset, _monitorOn, NULL );
    }

// Handle mute state (set graphics toggle button) 
if	(!_muteOn && alParameters[19] == AL_SPEAKER_MUTE_ON) 
    {
    _muteOn = True;
    XtVaSetValues(_muteToggle, XmNset, _muteOn, NULL );

// update Presenter
    SetFlatPanelSpeakerMute(_graphicsFD, True);
    if (_presenterWindow)
	_presenterWindow->setMuteToggle(True);
    } 
else if (_muteOn && alParameters[19] == AL_SPEAKER_MUTE_OFF) 
    {
    _muteOn = False;
    XtVaSetValues(_muteToggle, XmNset, _muteOn, NULL );

// update Presenter
    SetFlatPanelSpeakerMute(_graphicsFD, False);
    if (_presenterWindow)
	_presenterWindow->setMuteToggle(False);
    }

// Handle stereo microphone mode: set item in Options menu 
if (_haveStereoMicrophoneAbility == True && _microphoneChannelCount != alParameters[25])
    {
    _microphoneChannelCount = alParameters[25];
    if	(_microphoneChannelCount == AL_STEREO)
	_stereoMicrophoneModeMenuItem->setVisualState(True);
    else if (_microphoneChannelCount == AL_MONO)
	_stereoMicrophoneModeMenuItem->setVisualState(False);
// clear samples from previous sampling rate 
    if (_meterOn)
	emptyAudioPortRingBuffer(_audioInputPort);
    }

// Handle speaker gain (set graphics sliders) 
// Presenter level adjustment done in setLevelSliderPosition() 
if (alParameters[1] != _outputControls->_dacValue[0])
    _outputControls->setLevelSliderPosition(0, alParameters[1]);
if (alParameters[3] != _outputControls->_dacValue[1])
    _outputControls->setLevelSliderPosition(1, alParameters[3]);

// Handle input attenuation level (set graphics sliders) 
if (alParameters[5] != _inputControls->_dacValue[0])
    _inputControls->setLevelSliderPosition(0, alParameters[5]);
if (alParameters[7] != _inputControls->_dacValue[1])
    _inputControls->setLevelSliderPosition(1, alParameters[7]);
if (_inputChannelCapacity > 2)
    {
    if (alParameters[21] != _inputControls->_dacValue[2])
	_inputControls->setLevelSliderPosition(2, alParameters[21]);
    if (_inputChannelCapacity > 3)
	{
	if (alParameters[23] != _inputControls->_dacValue[3])
	    _inputControls->setLevelSliderPosition(3, alParameters[23]);
	}
    }

static long inputSourceTokens[] = 
    {AL_INPUT_LINE, AL_INPUT_MIC, AL_INPUT_DIGITAL};
static const int numInputSources = sizeof(inputSourceTokens)/sizeof(long);
static char *(inputSourcesMenuItemNames[]) = 
    { "inputSourceLine", "inputSourceMicrophone", "inputSourceDigital" };

static long samplingRateTokens[] = 
    { AL_RATE_8000,  AL_RATE_11025, AL_RATE_16000, AL_RATE_22050,
      AL_RATE_32000, AL_RATE_44100, AL_RATE_48000, AL_RATE_AES_1,
      AL_RATE_INPUTRATE };
static const int numSamplingRates = sizeof(samplingRateTokens)/sizeof(long);
static char *(samplingRateMenuItemNames[]) = 
    {
    "samplingRateLabel1", "samplingRateLabel2",
    "samplingRateLabel3", "samplingRateLabel4",
    "samplingRateLabel5", "menuSamplingRateLabel6", 
    "samplingRateLabel7", "samplingRateLabel8",
    "samplingRateLabel9",
    "samplingRateLabel10",
    "samplingRateLabel11",
    "samplingRateLabel12",
    };

// Handle change in input source 
if (_inputSource != alParameters[13]) 
    {
// set input source menu item 
    _inputSource = alParameters[13];
    for (i = 0; i < numInputSources; i++) 
	{
	VkMenuToggle *t = (VkMenuToggle *) 
		_inputMenuTitle->findNamedItem(inputSourcesMenuItemNames[i]);
	t->setVisualState(_inputSource == inputSourceTokens[i]);
	}

// set input source above level sliders 
    _inputControls->setSourceLabel((int)_inputSource);

// for every change in input source, ensure input sampling rate update 
// metering details thus handled in rate code 
    _inputSamplingRate = SOME_BOGUS_SAMPLING_RATE; 
    _inputDigitalSamplingRate = SOME_BOGUS_SAMPLING_RATE; 
    }

// if input or output sampling rates are AES word clock, force an update 
//    in input/output sampling rate label 
if (_canDetermineDigitalInputRate && _inputDigitalSamplingRate != alParameters[29])
    {
    _inputDigitalSamplingRate = alParameters[29];
    if (_inputSource == AL_INPUT_DIGITAL ||
	(alParameters[9] >= AL_RATE_AES_1s && alParameters[9] <= AL_RATE_AES_1))
	_inputSamplingRate = SOME_BOGUS_SAMPLING_RATE;
    if (alParameters[11] >= AL_RATE_AES_1s && alParameters[11] <= AL_RATE_AES_1)
	_outputSamplingRate = SOME_BOGUS_SAMPLING_RATE;
    }

// set input rate label (code must follow that for setting input source) 
if (_inputSamplingRate != alParameters[9])
    {
    _inputSamplingRate = alParameters[9];
    if (False == _useSamplingRateOptionMenus)
	{
    // Set one submenu button with radio button behavior 
    // scan one fewer sampling rate because input rate is never AL_RATE_INPUTRATE
	for (i = 0; i < numSamplingRates-1; i++) 
	    {
	    VkMenuToggle *t = (VkMenuToggle *) 
		_inputSamplingRateSubMenu->findNamedItem(samplingRateMenuItemNames[i]);
	    t->setVisualState(_inputSamplingRate == samplingRateTokens[i]);
	    }
	}

// set sampling rate label under level sliders 
    if (_inputSource == AL_INPUT_DIGITAL)
	_inputControls->setSamplingRateLabel(AL_RATE_AES_1, (int)_inputDigitalSamplingRate);
    else
	_inputControls->setSamplingRateLabel((int)_inputSamplingRate, (int)_inputDigitalSamplingRate);

// recompute metering intervals any time sampling rate changes  
    if (_meterOn)
	{
	_meterShortBlockLength = ComputeMeterShortBlockLength();
	ResetMeters();
// clear samples from previous sampling rate 
	emptyAudioPortRingBuffer(_audioInputPort);
	}
    }

// set output rate label and menu item Rate, outputRate 
// must follow input sampling rate code 
if (_outputSamplingRate != alParameters[11]) 
    {
    _outputSamplingRate = alParameters[11];
    if (False == _useSamplingRateOptionMenus)
	{
    // set sampling rate label under level sliders 
	for (i = 0; i < numSamplingRates; i++)
	    {
	    VkMenuToggle *t = (VkMenuToggle *) 
		_outputSamplingRateSubMenu->findNamedItem(samplingRateMenuItemNames[i]);
	// set menu item state 
	    t->setVisualState(_outputSamplingRate == samplingRateTokens[i]);
	    }
	}

// set sampling rate label under level sliders 
    _outputControls->setSamplingRateLabel((int)_outputSamplingRate, 
					    (int)_inputDigitalSamplingRate);
    }

// Handle listening LED (turn on if any audio ports in use, even by other 
// applications) Post used by audiopanel should not be considered, because it 
// is obvious no other application is listeninging 
    if (_useInputListeningLED)
	{
        if	(alParameters[15] > (_audioInputPort != NULL)) 
	    _inputControls->setListeningLED(_listeningLEDOn = True);
	else if (alParameters[15] <= (_audioInputPort != NULL))
	    _inputControls->setListeningLED(_listeningLEDOn = False);
	}

// swap double parameter buffers
long	*tmpPtr = oldeAlParameters;
oldeAlParameters = alParameters;
alParameters = tmpPtr;
} /* ---- end pollAudioHardwareState() ---- */

/* ******************************************************************
 * computeAndRenderMeters:   compute short time and long time
 *				    peak meter values and update graphical 
 *				    meters on user interface
 *				    Does not iterate when window is iconized
 *			    No DC filter !! 
 * ****************************************************************** */
    void
ApanelWindow::computeAndRenderMeters()
{
int	    i;
short	    inputSamples[MAX_IN_RING_BUFFER_SAMPLES];
int	    input;
int	    shortBlockPeak1, shortBlockPeak2, shortBlockPeak3, shortBlockPeak4;

//printf("ApanelWindow::computeAndRenderMeters: start\n");

// read block of samples form audio input port (non-blocking) 
int samplesToRead = _meterShortBlockLength;
int samplesInQueue = (int) ALgetfilled(_audioInputPort);
if (samplesInQueue < samplesToRead)
    return;
ALreadsamps(_audioInputPort, inputSamples, (long) samplesInQueue);

// scan block of samples for short time peaks for each channel 
switch (_channelCount)
    {
    case 2:
    // short time peaks are reset every short block 
	shortBlockPeak1 = 0;
	shortBlockPeak2 = 0;
	for (i = 0; i < samplesInQueue; i += 2)
	    {
	// full-wave rectify input signal
	    input = (int) inputSamples[i];

	// register absolute value as short time peak 
	    if	    (input > shortBlockPeak1) 
		shortBlockPeak1 = input;
	    else 
		{
		input = -input;
		if (input > shortBlockPeak1) 
		    shortBlockPeak1 = input;
		}
	
	    input = (int) inputSamples[i+1];
	    if	    (input > shortBlockPeak2) 
		shortBlockPeak2 = input;
	    else 
		{
		input = -input;
		if (input > shortBlockPeak2) 
		    shortBlockPeak2 = input;
		}
	    }

	_shortBlockPeak[0] = shortBlockPeak1;
	_shortBlockPeak[1] = shortBlockPeak2;
    break;

    case 4:
    // short time peaks are reset every short block 
	shortBlockPeak1 = 0;
	shortBlockPeak2 = 0;
	shortBlockPeak3 = 0;
	shortBlockPeak4 = 0;
	for (i = 0; i < samplesInQueue; i += 4)
	    {
	// full-wave rectify input signal 
	    input = (int) inputSamples[i];
	    if	    (input > shortBlockPeak1) 
		shortBlockPeak1 = input;
	    else if (-input > shortBlockPeak1) 
		shortBlockPeak1 = -input;

	    input = (int) inputSamples[i+1];
	    if	    (input > shortBlockPeak2) 
		shortBlockPeak2 = input;
	    else if (-input > shortBlockPeak2) 
		shortBlockPeak2 = -input;

	    input = (int) inputSamples[i+2];
	    if	    (input > shortBlockPeak3) 
		shortBlockPeak3 = input;
	    else if (-input > shortBlockPeak3) 
		shortBlockPeak3 = -input;

	    input = (int) inputSamples[i+3];
	    if	    (input > shortBlockPeak4) 
		shortBlockPeak4 = input;
	    else if (-input > shortBlockPeak4) 
		shortBlockPeak4 = -input;
	    }

	_shortBlockPeak[0] = shortBlockPeak1;
	_shortBlockPeak[1] = shortBlockPeak2;
	_shortBlockPeak[2] = shortBlockPeak3;
	_shortBlockPeak[3] = shortBlockPeak4;
    break;

#ifdef SAFE
// code for arbitrary # of channels
//  !!!!!!!!! this code not needed yet,  and untested !!!  Looks wrong
    default:
	for (int channelIndex = 0; channelIndex < _channelCount; channelIndex++) 
	    {
    // short time peaks are reset every short block 
	    shortBlockPeak1 = 0;
	    for (i = channelIndex; i < samplesInQueue; i += (int)_channelCount)
		{
	    // collect maximum absolute value of input samples 
		input = (int) inputSamples[i+channelIndex];
		if	(input > shortBlockPeak1) 
		    shortBlockPeak1 = input;
		else if (-input > shortBlockPeak1) 
		    shortBlockPeak1 = -input;
		}
	    _shortBlockPeak[channelIndex] = shortBlockPeak1;
	    }
    break;
#endif
    }

// 
// update meters every meterShortBlockLength using dual
// metering scheme:   
// Long time peak interval:		1 second 
// Short time peak interval:		1/16 second 
// IMPORTANT:  >= operation is safeguard for dynamic changes in
// meterShortBlockLength, which occur w/sampling rate changes.
//
for (i = 0; i < _channelCount; i++)
    {
    if (_shortBlockCounter[i] >= _shortBlocksPerLongBlock) 
	{
	_shortBlockCounter[i] = 0;
	_longBlockPeak[i] = 0;
	}
    }

// use short block peaks to update long block peaks 
// look up meter value in decibel table and render on screen 
for (i = 0; i < _channelCount; i++)
    {
    // saturate short block maximum values 
    if (_shortBlockPeak[i] > REFERENCE_LEVEL)
	_shortBlockPeak[i] = REFERENCE_LEVEL;

//#define RESCALE 

#ifdef RESCALE
    float   scale = REFERENCE_LEVEL;
    int shortPeak = (int) (scale*((float) _levelToDecibelTable[_shortBlockPeak[i]]));
    scale /= (float)(REFERENCE_LEVEL - (_dcOffset[i]>>BOX_BUFFER_SHIFT));
#else
    int shortPeak = (int) _levelToDecibelTable[_shortBlockPeak[i]];
#endif
    if (_shortBlockPeak[i] > _longBlockPeak[i]) 
	{
	_shortBlockCounter[i] = 0;
	_longBlockPeak[i] = _shortBlockPeak[i];	    
	_inputControls->setMeter(i, shortPeak, shortPeak);
	}
    else
	{
	_shortBlockCounter[i]++;

#ifdef RESCALE
	int longPeak = (int) (scale*((float) _levelToDecibelTable[_longBlockPeak[i]]));
#else
	int longPeak = (int) _levelToDecibelTable[_longBlockPeak[i]];
#endif	
	_inputControls->setMeter(i, shortPeak, longPeak);
	}
    }
} /* ---- end computeAndRenderMeters() ---- */

/* ******************************************************************
 * computeAndRenderMetersDCBoxFilter:   compute short time and long time
 *				    peak meter values and update graphical 
 *				    meters on user interface
 *				    Does not iterate when window is iconized
 *			    Use simple complementary box filter for DC offset removal
 * ****************************************************************** */
    void
ApanelWindow::computeAndRenderMetersDCBoxFilter()
{
int	    i;
short	    inputSamples[MAX_IN_RING_BUFFER_SAMPLES];
int	    input;
int	    shortBlockPeak1, shortBlockPeak2, shortBlockPeak3, shortBlockPeak4;

// THIS computeAndRenderMetersDCBoxFilter() USED IN PRODUCTION VERSION !!!
//printf("ApanelWindow::computeAndRenderMetersDCBoxFilter: start\n");

// read block of samples form audio input port (non-blocking) 
long samplesInQueue = ALgetfilled(_audioInputPort);
//if (samplesInQueue < (long) _meterShortBlockLength)
//   return;
samplesInQueue = (samplesInQueue/_channelCount)*_channelCount;
ALreadsamps(_audioInputPort, inputSamples, samplesInQueue);

// scan block of samples for short time peaks for each channel 
switch (_channelCount)
    {
    case 2:
    // short time peaks are reset every short block 
	shortBlockPeak1 = 0;
	shortBlockPeak2 = 0;
	for (i = 0; i < samplesInQueue; i += 2)
	    {
// Update DC offset estimation boxcar filter 
	    _dcOffset[0] -= (int) boxBuffer[0][boxBufferIndex];
	    _dcOffset[1] -= (int) boxBuffer[1][boxBufferIndex];
	    _dcOffset[0] += (int) inputSamples[i];
	    _dcOffset[1] += (int) inputSamples[i+1];

	    boxBuffer[0][boxBufferIndex] = inputSamples[i];
	    boxBuffer[1][boxBufferIndex] = inputSamples[i+1];

	// advance and bound buffer index 
	    boxBufferIndex++;
	    boxBufferIndex &= (BOX_BUFFER_LENGTH-1);

	// full-wave rectify input signal 
	    input = ((int) inputSamples[i]) - (_dcOffset[0]>>BOX_BUFFER_SHIFT);

	// register absolute value as short time peak 
	    if	    (input > shortBlockPeak1) 
		shortBlockPeak1 = input;
	    else 
		{
		input = -input;
		if (input > shortBlockPeak1) 
		    shortBlockPeak1 = input;
		}
	
	    input = ((int) inputSamples[i+1]) - (_dcOffset[1]>>BOX_BUFFER_SHIFT);
	    if	    (input > shortBlockPeak2) 
		shortBlockPeak2 = input;
	    else 
		{
		input = -input;
		if (input > shortBlockPeak2) 
		    shortBlockPeak2 = input;
		}
	    }

	_shortBlockPeak[0] = shortBlockPeak1;
	_shortBlockPeak[1] = shortBlockPeak2;
    break;

    case 4:
    // short time peaks are reset every short block 
	shortBlockPeak1 = 0;
	shortBlockPeak2 = 0;
	shortBlockPeak3 = 0;
	shortBlockPeak4 = 0;
	for (i = 0; i < samplesInQueue; i += 4)
	    {
// Update DC offset estimation boxcar filter 
	    _dcOffset[0] -= (int) boxBuffer[0][boxBufferIndex];
	    _dcOffset[1] -= (int) boxBuffer[1][boxBufferIndex];
	    _dcOffset[2] -= (int) boxBuffer[2][boxBufferIndex];
	    _dcOffset[3] -= (int) boxBuffer[3][boxBufferIndex];
	    _dcOffset[0] += (int) inputSamples[i];
	    _dcOffset[1] += (int) inputSamples[i+1];
	    _dcOffset[2] += (int) inputSamples[i+2];
	    _dcOffset[3] += (int) inputSamples[i+3];

	    boxBuffer[0][boxBufferIndex] = inputSamples[i];
	    boxBuffer[1][boxBufferIndex] = inputSamples[i+1];
	    boxBuffer[2][boxBufferIndex] = inputSamples[i+2];
	    boxBuffer[3][boxBufferIndex] = inputSamples[i+3];

	// advance and bound buffer index 
	    boxBufferIndex++;
	    boxBufferIndex &= (BOX_BUFFER_LENGTH-1);

	// full-wave rectify input signal 
	    input = ((int) inputSamples[i]) - (_dcOffset[0]>>BOX_BUFFER_SHIFT);
	    if	    (input > shortBlockPeak1) 
		shortBlockPeak1 = input;
	    else if (-input > shortBlockPeak1) 
		shortBlockPeak1 = -input;

	    input = ((int) inputSamples[i+1]) - (_dcOffset[1]>>BOX_BUFFER_SHIFT);
	    if	    (input > shortBlockPeak2) 
		shortBlockPeak2 = input;
	    else if (-input > shortBlockPeak2) 
		shortBlockPeak2 = -input;

	    input = ((int) inputSamples[i+2]) - (_dcOffset[2]>>BOX_BUFFER_SHIFT);
	    if	    (input > shortBlockPeak3) 
		shortBlockPeak3 = input;
	    else if (-input > shortBlockPeak3) 
		shortBlockPeak3 = -input;

	    input = ((int) inputSamples[i+3]) - (_dcOffset[3]>>BOX_BUFFER_SHIFT);
	    if	    (input > shortBlockPeak4) 
		shortBlockPeak4 = input;
	    else if (-input > shortBlockPeak4) 
		shortBlockPeak4 = -input;
	    }

	_shortBlockPeak[0] = shortBlockPeak1;
	_shortBlockPeak[1] = shortBlockPeak2;
	_shortBlockPeak[2] = shortBlockPeak3;
	_shortBlockPeak[3] = shortBlockPeak4;
    break;
    }

// 
// update meters every meterShortBlockLength using dual
// metering scheme:   
// Long time peak interval:		1 second 
// Short time peak interval:		1/16 second 
// IMPORTANT:  >= operation is safeguard for dynamic changes in
// meterShortBlockLength, which occur w/sampling rate changes.
//
for (i = 0; i < _channelCount; i++)
    {
    if (_shortBlockCounter[i] >= _shortBlocksPerLongBlock) 
	{
	_shortBlockCounter[i] = 0;
	_longBlockPeak[i] = 0;
	}
    }

// use short block peaks to update long block peaks 
// look up meter value in decibel table and render on screen 
for (i = 0; i < _channelCount; i++)
    {
    // saturate short block maximum values 
    if (_shortBlockPeak[i] > REFERENCE_LEVEL)
	_shortBlockPeak[i] = REFERENCE_LEVEL;

    int shortPeak = (int) _levelToDecibelTable[_shortBlockPeak[i]];
    if (_shortBlockPeak[i] > _longBlockPeak[i]) 
	{
	_shortBlockCounter[i] = 0;
	_longBlockPeak[i] = _shortBlockPeak[i];	    
	_inputControls->setMeter(i, shortPeak, shortPeak);
	}
    else
	{
	_shortBlockCounter[i]++;

	int longPeak = (int) _levelToDecibelTable[_longBlockPeak[i]];
	_inputControls->setMeter(i, shortPeak, longPeak);
	}
    }
} /* ---- end computeAndRenderMetersDCBoxFilter() ---- */

#if defined(DC_FILTER_IIR)
/* ******************************************************************
 * computeAndRenderMetersDCIIRFilter:   compute short time and long time
 *				    peak meter values and update graphical 
 *				    meters on user interface
 *				    Does not iterate when window is iconized
 * ****************************************************************** */
    void
ApanelWindow::computeAndRenderMetersDCIIRFilter()
{
int	    i;
short	    inputSamples[MAX_IN_RING_BUFFER_SAMPLES];
int	    input;
int	    shortBlockPeak1, shortBlockPeak2, shortBlockPeak3, shortBlockPeak4;
float	    x, x1, x2, y, y1, y2;
float	    a0, b1, b2;

// read block of samples form audio input port (non-blocking) 
long samplesToRead = (long) _meterShortBlockLength;
long samplesInQueue = ALgetfilled(_audioInputPort);
//if (samplesInQueue < samplesToRead)
//   return;
ALreadsamps(_audioInputPort, inputSamples, samplesInQueue);

// scan block of samples for short time peaks for each channel 
switch (_channelCount)
    {
    case 2:
    // short time peaks are reset every short block 
	shortBlockPeak1 = 0;
	shortBlockPeak2 = 0;
	for (i = 0; i < samplesInQueue; i += 2)
	    {
// Compute DC offset IIR filter 
// coefficients a1=-2 and a2=1, so the multiplies are replaced by additions 
// y = a0*x  + a1*x1 + a2*x2 + b1*y1 + b2*y2 
		x*= a0;
		y = x - (x1+x1) + x2 ;
	 //   input = ((int) inputSamples[i]) - (0);

	// register absolute value as short time peak 
	    if	    (input > shortBlockPeak1) 
		shortBlockPeak1 = input;
	    else 
		{
		input = -input;
		if (input > shortBlockPeak1) 
		    shortBlockPeak1 = input;
		}
	
	    input = (int) inputSamples[i+1];
	    if	    (input > shortBlockPeak2) 
		shortBlockPeak2 = input;
	    else 
		{
		input = -input;
		if (input > shortBlockPeak2) 
		    shortBlockPeak2 = input;
		}
	    }

	_shortBlockPeak[0] = shortBlockPeak1;
	_shortBlockPeak[1] = shortBlockPeak2;
    break;

    case 4:
    // short time peaks are reset every short block 
	shortBlockPeak1 = 0;
	shortBlockPeak2 = 0;
	shortBlockPeak3 = 0;
	shortBlockPeak4 = 0;
	for (i = 0; i < samplesInQueue; i += 4)
	    {
// Compute DC offset IIR filter 

// full-wave rectify input signal 
//   input = ((int) inputSamples[i]) - (0);

	// full-wave rectify input signal 
	    input = (int) inputSamples[i];
	    if	    (input > shortBlockPeak1) 
		shortBlockPeak1 = input;
	    else if (-input > shortBlockPeak1) 
		shortBlockPeak1 = -input;

	    input = (int) inputSamples[i+1];
	    if	    (input > shortBlockPeak2) 
		shortBlockPeak2 = input;
	    else if (-input > shortBlockPeak2) 
		shortBlockPeak2 = -input;

	    input = (int) inputSamples[i+2];
	    if	    (input > shortBlockPeak3) 
		shortBlockPeak3 = input;
	    else if (-input > shortBlockPeak3) 
		shortBlockPeak3 = -input;

	    input = (int) inputSamples[i+3];
	    if	    (input > shortBlockPeak4) 
		shortBlockPeak4 = input;
	    else if (-input > shortBlockPeak4) 
		shortBlockPeak4 = -input;
	    }

	_shortBlockPeak[0] = shortBlockPeak1;
	_shortBlockPeak[1] = shortBlockPeak2;
	_shortBlockPeak[2] = shortBlockPeak3;
	_shortBlockPeak[3] = shortBlockPeak4;
    break;
    }

// 
// update meters every meterShortBlockLength using dual
// metering scheme:   
// Long time peak interval:		1 second 
// Short time peak interval:		1/16 second 
// IMPORTANT:  >= operation is safeguard for dynamic changes in
// meterShortBlockLength, which occur w/sampling rate changes.
//
for (i = 0; i < _channelCount; i++)
    {
    if (_shortBlockCounter[i] >= _shortBlocksPerLongBlock) 
	{
	_shortBlockCounter[i] = 0;
	_longBlockPeak[i]     = 0;
	}
    }

// use short block peaks to update long block peaks 
// look up meter value in decibel table and render on screen 
for (i = 0; i < _channelCount; i++)
    {
// saturate short block maximum values 
    if (_shortBlockPeak[i] > REFERENCE_LEVEL)
	_shortBlockPeak[i] = REFERENCE_LEVEL;

    int shortPeak = (int) _levelToDecibelTable[_shortBlockPeak[i]];
    if (_shortBlockPeak[i] > _longBlockPeak[i]) 
	{
	_shortBlockCounter[i] = 0;
	_longBlockPeak[i] = _shortBlockPeak[i];	    
	_inputControls->setMeter(i, shortPeak, shortPeak);
	}
    else
	{
	_shortBlockCounter[i]++;

	int longPeak = (int) _levelToDecibelTable[_longBlockPeak[i]];
	_inputControls->setMeter(i, shortPeak, longPeak);
	}
    }
} /* ---- end computeAndRenderMetersDCIIRFilter() ---- */
#endif

/* ******************************************************************
 * ComputeLevelToMeterLookUpTable:   create table 
 *					used to map
 *					linear signal levels in range
 *					[0..REFERENCE_LEVEL] to meter
 *					range [0..109]
 * ****************************************************************** */
    void 
ApanelWindow::ComputeLevelToMeterLookUpTable(char *table)
    /* table	ptr to table */
{
int	    i;
float	    lowLinearLevel, highLinearLevel;
int	    cap, floor, ceiling;
double	    widgetValue, delta;
int	    segment;
#define METER_SEGMENT_COUNT 10 
float	    headRoom[METER_SEGMENT_COUNT+1];

// headroom value per segment: labels 0,2,4,7,10,20,30,40,50,60 
headRoom[0]  = 60 + 1;
headRoom[1]  = 50 + 1;
headRoom[2]  = 40 + 1;
headRoom[3]  = 30 + 1;
headRoom[4]  = 20 + 1;
headRoom[5]  = 10 + 1;
headRoom[6]  = 7 + 1;
headRoom[7]  = 4 + 1;
headRoom[8]  = 2 + 1;
headRoom[9]  = 0 + 1;
headRoom[10] = -1 + 1;

/*
			    32767*10^(dB/20)
HeadRoom    Signal level    16-bit signal	GUI Meter Widget Range
C	     0		    32767 - 32768		09..00
0	    -1  ..   0	    29204 - 32766		19..10
2	    -3	..  -1	    23197 - 29203		29..20
4	    -5  ..  -3	    18426 - 23196		39..30
7	    -8  ..  -5	    13045 - 18425		49..40
10	    -11 ..  -8	     9235 - 13044		59..50
20	    -21 ..  -11	     2920 -  9234		69..60
30	    -31 ..  -21	      923 -  2919		79..70
40	    -41 ..  -31	      292 -   922		89..80
50	    -51 ..  -41	       92 -   291		99..90
60	    -61 ..  -51	       29 -    91	       109..100
*/

// minimumDisplayValue (in decibels) = 20*log10(minimum level/maximum level) 
cap = (int) (((float) REFERENCE_LEVEL)*powf(10, -headRoom[0]/20) + 0.5);
for (i = 0; i < cap; i++)
    table[i] = (char) 0;

// now do generalized headroom display 
highLinearLevel = (float) (cap-1);
for (segment = 0; segment < METER_SEGMENT_COUNT; segment++)
    {
// lowLinearLevel = highLinearLevel + 1;
    lowLinearLevel = ((float) REFERENCE_LEVEL)*powf(10, -headRoom[segment]/20);
    highLinearLevel = -1 + ((float) REFERENCE_LEVEL)*powf(10, -headRoom[segment+1]/20);

    floor = (int) (lowLinearLevel + 0.5);
    ceiling = (int) (highLinearLevel + 0.5);
    delta = 10.0/((double)(ceiling - floor + 1));

    widgetValue = 10*((double) segment);
    for (; i <= ceiling; i++, widgetValue += delta)
	table[i] = (char)(widgetValue);
    }

table[REFERENCE_LEVEL] = (char) 109;
table[REFERENCE_LEVEL+1] = (char) 109;

// flip for ViewKit VkVUMeter widget input 
for (i = 0; i < REFERENCE_LEVEL+2; i++)
    {
    table[i] = 109 - table[i];
    if (table[i] <= 1)
	table[i] = 1;
    }
} /* ---- end ComputeLevelToMeterLookUpTable() ---- */

/* ******************************************************************
 * ReconfigureAudioPorts:	
 * ****************************************************************** */
    void 
ApanelWindow::ReconfigureAudioPorts(Boolean audioInputPortNeeded)
{
if (audioInputPortNeeded) 
    {
    ALconfig alConfiguration = ALnewconfig();
    int queueLength = (int) (((float)MAXIMUM_SAMPLING_RATE)/
				((float)DEFAULT_METER_UPDATE_RATE));
    ALsetqueuesize(alConfiguration, queueLength*_channelCount*2); 
    ALsetwidth(alConfiguration, AL_SAMPLE_16);
    if (_channelCount == 4)
	ALsetchannels(alConfiguration, AL_4CHANNEL);
    else
	ALsetchannels(alConfiguration, AL_STEREO);
    if (_audioInputPort) 
	ALcloseport(_audioInputPort);
    _audioInputPort = ALopenport("Audio Panel: meters", "r", alConfiguration);
 // if Alopenport fails, warning window popped in meter toggle button callback 
    ALfreeconfig(alConfiguration);
    } 
else 
    {
// close existing input port
    if (_audioInputPort) 
	{
	ALcloseport(_audioInputPort);
	_audioInputPort = NULL;
	}
    }
} /* ---- end ReconfigureAudioPorts() ---- */

/* ******************************************************************
 * ComputeMeterShortBlockLength:	    size of meter block
 *						    in samples.  Scales
 *						    with channel count
 * ****************************************************************** */
    int
ApanelWindow::ComputeMeterShortBlockLength()
{
long	    alParameters[6];
float	    value;
long	    inputRate, digitalSamplingRate;

// Read input rate and source 
alParameters[0] = AL_INPUT_SOURCE;
alParameters[2] = AL_INPUT_RATE;
alParameters[4] = AL_DIGITAL_INPUT_RATE;
ALgetparams(AL_DEFAULT_DEVICE, alParameters, 6);

// Compute # of input samples per second.  If
//   input source is digital or input rate is AES, then we don't
//   know value of incoming rate, so use kinda-middle but typical case value
//   of 44100 samples/second. In fact, incoming data
//   rate could be much slower, and meters will not update as rapidly. 
inputRate = alParameters[3];
digitalSamplingRate = alParameters[5];
if (alParameters[1] == AL_INPUT_DIGITAL)
    {
   // assume typical and value in range 32 .. 48 kHz 
    if (!_canDetermineDigitalInputRate                  ||
	digitalSamplingRate == AL_RATE_UNDEFINED        ||
	digitalSamplingRate == AL_RATE_NO_DIGITAL_INPUT ||
	digitalSamplingRate == AL_RATE_UNACQUIRED)
	value = 44100;
    else
	value = digitalSamplingRate;
    }
else if (inputRate < 0)
    {
   // assume typical and value in range 32 .. 48 kHz 
    if (!_canDetermineDigitalInputRate                  ||
	digitalSamplingRate == AL_RATE_UNDEFINED        ||
	digitalSamplingRate == AL_RATE_NO_DIGITAL_INPUT ||
	digitalSamplingRate == AL_RATE_UNACQUIRED)
	value = 44100;
    else
	value = digitalSamplingRate;

    if	    (inputRate == AL_RATE_AES_2)
	value /= 2;
    else if (inputRate == AL_RATE_AES_3)
	value /= 3;
    else if (inputRate == AL_RATE_AES_4)
	value /= 4;
    else if (inputRate == AL_RATE_AES_6)
	value /= 6;
    else if (inputRate == AL_RATE_AES_1s)
	value *= 2.0/3.0;

    }
// sampling rate is analog line sampling rate 
else
    value = inputRate;

value *= (float) (DEFAULT_METER_UPDATE_INTERVAL_VALUE*_channelCount);
return (value + 0.5);
} /* ---- end ComputeMeterShortBlockLength() ---- */

/* ******************************************************************
 * ResetMeters:	reset meters
 * ****************************************************************** */
    void 
ApanelWindow::ResetMeters()
{
// clear counters and peak memories 
#ifdef DC_FILTER_BOX
boxBufferIndex = 0;
#endif
for (int i = 0; i < _inputChannelCapacity; i++)
    {
    _shortBlockCounter[i] = 0;
    _shortBlockPeak[i] = 0;
    _longBlockPeak[i] = 0;

#ifdef DC_FILTER_BOX
    _dcOffset[i] = 0;
// clear run length buffer 
    for (int j = 0; j < BOX_BUFFER_LENGTH; j++)
	boxBuffer[i][j] = 0;
#elif defined(DC_FILTER_IIR)
    z[i][0] = 0;
    z[i][1] = 0;
    z[i][2] = 0;
    z[i][3] = 0;
#endif
    }
} /* ---- end ResetMeters() ---- */

/* ******************************************************************
 * intervalToMilliseconds:  return value of milliSeconds	
 * ****************************************************************** */
    int 
ApanelWindow::intervalToMilliseconds(float intervalInSeconds)
{
// round to nearest milliSecond
return ((int)(intervalInSeconds*1000 + 0.5));
} /* ---- end intervalToMilliseconds() ---- */

/* ******************************************************************
 * startMeterTimer:	
 * ****************************************************************** */
    void 
ApanelWindow::startMeterTimer()
{
if (_useInputMeters && _meterOn)
    {
    _meterShortBlockLength = ComputeMeterShortBlockLength();
// compute linear to decibel look up table 
    if (!_levelToDecibelTable)
	{
	_levelToDecibelTable = (char *) malloc((REFERENCE_LEVEL+2)*sizeof(char));
	if (_levelToDecibelTable)
	    ComputeLevelToMeterLookUpTable(_levelToDecibelTable);
	}
    
#ifdef DC_FILTER_BOX
    for (int i = 0; i < MAX_INPUT_CHANNELS; i++)
	{
	if (!boxBuffer[i])
	    boxBuffer[i] = (short *) malloc(BOX_BUFFER_LENGTH*sizeof(short));
	}
#endif

    ResetMeters();
    if (_audioInputPort)
	_meterTimer->start(_meterTimerInterval);
    }
} /*---- end startMeterTimer() ---- */

/* ******************************************************************
 * stopMeterTimer:	
 * ****************************************************************** */
    void 
ApanelWindow::stopMeterTimer()
{
if (_useInputMeters)
    _meterTimer->stop();
} /*---- end stopMeterTimer() ---- */

/* ******************************************************************
 * emptyAudioPortRingBuffer:	
 * ****************************************************************** */
    void 
ApanelWindow::emptyAudioPortRingBuffer(ALport port)
{
// inquire # samples in buffer and read them 
int samplesInBuffer = (int) ALgetfilled(port);
short *buffer = (short *) malloc(samplesInBuffer*sizeof(short));
if (buffer)
    {
    ALreadsamps(port, buffer, (long)samplesInBuffer);
    free(buffer);
    }
} /* ---- end emptyAudioPortRingBuffer() ---- */

