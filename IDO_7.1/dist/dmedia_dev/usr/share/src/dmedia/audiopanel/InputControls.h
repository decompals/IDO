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

#ifndef INPUTCONTROLPANEL_H
#define INPUTCONTROLPANEL_H
#include <Vk/VkComponent.h>
#include <Vk/VkDeck.h> 
#include <Vk/VkMenu.h> 
#include <Vk/VkMenuBar.h> 
#include <Vk/VkMenuItem.h> 
#include <Vk/VkOptionMenu.h>
#include <Vk/VkTickMarks.h>
#include <Vk/VkVuMeter.h> 
#include <Vk/VkWindow.h>
#include "apaneldefs.h"

#define INPUT_SLIDER_BLOCKS		    2
#define CHANNELS_PER_INPUT_SLIDER_BLOCK	    2
#define MAX_INPUT_CHANNELS		    INPUT_SLIDER_BLOCKS*CHANNELS_PER_INPUT_SLIDER_BLOCK
#define NUM_INPUT_SOURCES		    3

class InputControls : public VkComponent
{ 
  public:

    InputControls(const char	*name, 
		    Widget	parent,
		    int		blockCapacity, 
		    int		channelCapacity, 
		    Boolean	useLevelMeters,
		    Boolean	canDetermineDigitalInputRate,
		    Boolean	useSamplingRateOptionMenus);
    ~InputControls();
    const char *className();

    int		    _blockCapacity;
    int		    _channelCapacity;
    Boolean	    _useLevelMeters;
    Boolean	    _useListeningLED;
    Boolean	    _useSamplingRateOptionMenus;
    Boolean	    _canDetermineDigitalInputRate;

    Boolean	    _gangState;		// True=ganged
    short	    _gangValue[MAX_INPUT_CHANNELS];
    long	    _dacValue[MAX_INPUT_CHANNELS];
    int		    _sliderTickMarkType;

    Boolean _useGroupLabels;
    Widget  _blockContainerParent;
    Widget	 _blockContainer;
    Widget	    _block[INPUT_SLIDER_BLOCKS];
    Widget		_sliderBlock[INPUT_SLIDER_BLOCKS];
    Widget		_slider[MAX_INPUT_CHANNELS];
    Widget		_sliderLabel[MAX_INPUT_CHANNELS];
    VkDeck		*_sliderTickDeck[INPUT_SLIDER_BLOCKS];
    VkTickMarks		    *_tickMarks[INPUT_SLIDER_BLOCKS][3];
    VkTickMarks		*_fauxTickMarks[INPUT_SLIDER_BLOCKS];

    Widget		_meterFrame[MAX_INPUT_CHANNELS];
    Widget		_meterSegmentLabelBox[INPUT_SLIDER_BLOCKS];
    Widget		    _meterSegmentLabels[15];
    VkVuMeter		*_meter[MAX_INPUT_CHANNELS];

    void setLevelSliderPosition(int channelNumber, long audioHardwareValue);
    void enableLevelSliders(Boolean enableStatus);
 
    void setSourceLabel(int labelIndex); 
    void setSamplingRateLabel(int samplingRate, int digitalSamplingRate); 

    void gangSliders(Boolean);
    void showMeters(Boolean);
    void setMeter(int channelNumber, int averageValue, int peakValue);
    void setSliderTickMarkType(char type);

    void createListeningLED();
    void setListeningLED(Boolean status);
    void showListeningLED(Boolean status);

  protected: 
    Widget	    _blockLabel[INPUT_SLIDER_BLOCKS];
    Widget		_inputSourceTitle;
    Widget		_inputSourceLabel;
    Widget		_listeningLEDFrame;
    Widget		    _listeningLED;

/* no sampling rate option menu */
    Widget	    _samplingRateLabel[INPUT_SLIDER_BLOCKS];
/* stuf for sampling rate option menu */
    VkOptionMenu    *_samplingRateOptionMenu[INPUT_SLIDER_BLOCKS];

    void    levelSliderMoved(Widget, XtPointer);
    long    HardwareLevelFromSliderValue(int sliderValue);
    int	    SliderValueFromHardwareLevel(long alHardwareLevel);
    float   DecibelsToScale(float decibels);

  private: 
    static void		levelSliderCB(Widget, XtPointer, XtPointer);

/* stuff for option menus */
    static VkMenuDesc	optionPaneDescription[];
    static void		optionMenuCB(Widget, XtPointer, XtPointer);

};
#endif

