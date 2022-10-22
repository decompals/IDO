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

#ifndef APANELWINDOW_H
#define APANELWINDOW_H

#include <Vk/VkComponent.h>
#include <Vk/VkMenu.h>
#include <Vk/VkMenuBar.h>
#include <Vk/VkMenuItem.h>
#include <Vk/VkPeriodic.h>
#include <Vk/VkRunOnce.h>
#include <Vk/VkWindow.h>

#include <audio.h>

#include "apaneldefs.h"
#include "Corona.h"
#include "InputControls.h"
#include "OutputControls.h"

#define DC_FILTER_BOX
//#define DC_FILTER_IIR

#define	REFERENCE_LEVEL 32767

#define DEFAULT_AUDIO_HARDWARE_POLL_INTERVAL_VALUE  0.5
#define DEFAULT_AUDIO_HARDWARE_POLL_INTERVAL	    "0.5"

class ApanelWindow: public VkWindow 
{
  public:

    ApanelWindow( const char	*windowName, 
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
		    Boolean	useDisplay);
    ~ApanelWindow();
    const char* className();  // Identify this class

    char    *_windowTitle;
    int	    windowWidthTwoChannelMode;
    int	    windowWidthFourChannelMode;

    void    update(VkComponent *component);

    int	    intervalToMilliseconds(float intervalInSeconds);
    int	    _audioHardwarePollInterval;	// in milliSeconds 
    void    pollAudioHardwareState();

    InputControls   *_inputControls;
    OutputControls  *_outputControls;
    PresenterWindow *_presenterWindow;	/* Presenter graphics */

    void toggleInputMeter(Boolean enable);

    void CreatePresenterPanel();
    void showPresenterPanelDisplay();
    void gangInputSliders	(Boolean state);
    void gangOutputSliders	(Boolean state);
    void useInputListeningLED	(Boolean state);
    void setInputSliderScale	(char type);
    void setOutputSliderScale	(char type);
    void useInputMeterDCFilter	(Boolean state);

    int readFullStateFromFile	(char *fileName);
    int readPreferencesFromFile	(char *fileName);
    int writeFullStateToFile	(char *fileName);

  protected:

    char    *_fileName;

// menu stuf 
    static VkMenuDesc menuBarDescription[];    
    static VkMenuDesc fileMenuDescription[];

    static VkMenuDesc inputSourcesMenuDescription[];
 
    static VkMenuDesc samplingRateMenuDescription[];
    static VkMenuDesc inputSamplingRateMenuDescription[];
    static VkMenuDesc outputSamplingRateMenuDescription[];

    static VkMenuDesc optionsMenuDescription[];

    VkMenu	    *_inputMenuTitle;
    VkMenu	    *_inputSamplingRateSubMenu, *_outputSamplingRateSubMenu;
    VkMenu	    *_optionsMenuTitle;

    VkMenuToggle    *_fourChannelModeMenuItem;
    VkMenuToggle    *_stereoMicrophoneModeMenuItem;

// graphics 
    Widget		_grid;
    Widget		    _toggleButtonContainer;
    Widget			_monitorToggle;
    Widget			_meterToggle;
    Widget		    _toggleButtonContainer2;
    Widget			_muteToggle;


// Audio Hardware State variables 
    VkPeriodic	    *_audioHardwarePollTimer;

// double buffered (always wanted to say that in audio) parameters 
#define DEFAULT_AL_PARAMETER_COUNT  30
    int		    alParameterCount;
    long	    alParameters1[DEFAULT_AL_PARAMETER_COUNT], 
		    alParameters2[DEFAULT_AL_PARAMETER_COUNT];
    long	    *alParameters, *oldeAlParameters;

    ALport	    _audioInputPort;
    Boolean	    _monitorOn;
    Boolean	    _muteOn;
    long	    _inputDACValue[MAX_INPUT_CHANNELS];
    long	    _inputSource;
    long	    _microphoneChannelCount;
    long	    _channelCount;
    long	    _inputSamplingRate;
    long	    _inputDigitalSamplingRate;
    long	    _outputSamplingRate;
 
    void	ReconfigureAudioPorts(Boolean audioInputPortNeeded);

    int		_inputChannelCapacity;
    int		_outputChannelCapacity;
    int		_overallChannelCapacity;
    Boolean	_haveStereoMicrophoneAbility;
    Boolean	_canDetermineDigitalInputRate;
    Boolean	_canChangeChannelMode;
    Boolean	_useSamplingRateOptionMenus;

    Boolean	_useDisplay;
    Boolean	_havePresenterFlatPanelDisplay;
    Boolean	_showPresenter;
    int		_graphicsFD;

// needed to preserve state of preferences during channel mode switch 
    Boolean	_spinalTapSliderScale;
    Boolean	_decibelInputSliderScale;
    char	_inputSlidersGanged;
    char	_outputSlidersGanged;
    Boolean	_useInputListeningLED;
    Boolean	_listeningLEDOn;

// meter stuf 
    Boolean	    _useInputMeters, _useOutputMeters;
    Boolean	    _meterOn;
    VkPeriodic	    *_meterTimer;
    int		    _meterTimerInterval;	// in milliSeconds 
    int		    _shortBlocksPerLongBlock;
    char	    *_levelToDecibelTable;     // can be reduced in size?
    
    Boolean	    _useInputMeterDCFilter;

#ifdef DC_FILTER_BOX
    int		    _dcOffset[MAX_INPUT_CHANNELS];
#define BOX_BUFFER_LENGTH    256	// power of two for integer BOX_BUFFER_SHIFT 
#define BOX_BUFFER_SHIFT     8	 
    short	    *boxBuffer[MAX_INPUT_CHANNELS];
    unsigned int    boxBufferIndex;
#else if defined (DC_FILTER_IIR)
    float	    z[MAX_INPUT_CHANNELS][4];	    
#endif

    int		    _shortBlockCounter[MAX_INPUT_CHANNELS];
    int		    _longBlockPeak[MAX_INPUT_CHANNELS], 
		    _shortBlockPeak[MAX_INPUT_CHANNELS];
    int		    _meterShortBlockLength;	/* length of block (in samples) for meters */

    void    ComputeLevelToMeterLookUpTable(char *levelToDecibelTable);
    void    showMeters(Boolean);
    void    computeAndRenderMeters();
    void    computeAndRenderMetersDCBoxFilter();

    int	    ComputeMeterShortBlockLength();

    void    ResetMeters();
    void    startMeterTimer();
    void    stopMeterTimer();
    void    emptyAudioPortRingBuffer(ALport port);

  private:
    struct CallbackStruct {
	class ApanelWindow *obj;
	Boolean		    reading;
    };
    CallbackStruct _fileSelectionInfo;

// File menu 
    static void newParameterFileCB	(Widget, XtPointer, XtPointer);
    static void openParameterFileCB	(Widget, XtPointer, XtPointer);
    static void saveParameterFileCB	(Widget, XtPointer, XtPointer);
    static void saveAsParameterFileCB	(Widget, XtPointer, XtPointer);
    static void quitCB			(Widget, XtPointer, XtPointer);

// Input menu 
    static void inputSourceCB		(Widget, XtPointer, XtPointer);

// Rate menu 
    static void inputSamplingRateCB	(Widget, XtPointer, XtPointer);
    static void outputSamplingRateCB	(Widget, XtPointer, XtPointer);

// Options menu 
    static void launchRBViewCB	    (Widget, XtPointer, XtPointer);

    static void gangInputSlidersCB  (Widget, XtPointer, XtPointer);
    static void gangOutputSlidersCB (Widget, XtPointer, XtPointer);
    static void listeningLEDCB	    (Widget, XtPointer, XtPointer);
    static void decibelScaleCB	    (Widget, XtPointer, XtPointer);

    static void stereoMicrophoneCB	(Widget, XtPointer, XtPointer);
    static void fourChannelModeCB	(Widget, XtPointer, XtPointer);
    static void presenterPanelDisplayCB	(Widget, XtPointer, XtPointer);

    void afterRealizeHook();
    void stateChanged	    (IconState state);
    void enableMuteButton   (Boolean enable);

    static VkMenuDesc optionMenuDesc[];
    static void microphoneInputCB   (Widget, XtPointer, XtPointer);
    static void lineInputCB	    (Widget, XtPointer, XtPointer);
    static void digitalInputCB	    (Widget, XtPointer, XtPointer);
    static void enableMuteCB	    (Widget, XtPointer, XtPointer);
    static void enableMonitorCB	    (Widget, XtPointer, XtPointer);
    static void showMeterCB	    (Widget, XtPointer, XtPointer);
    static void filePrePostCB	    (Widget, XtPointer, XtPointer);
};

#endif

