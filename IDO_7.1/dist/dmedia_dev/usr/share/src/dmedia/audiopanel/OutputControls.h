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

#ifndef OUTPUTCONTROLS_H
#define OUTPUTCONTROLS_H
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
#include "Corona.h"

#define OUTPUT_SLIDER_BLOCKS		    2
#define CHANNELS_PER_OUTPUT_SLIDER_BLOCK    2
#define MAX_OUTPUT_CHANNELS		    OUTPUT_SLIDER_BLOCKS*CHANNELS_PER_OUTPUT_SLIDER_BLOCK

class OutputControls : public VkComponent
{ 
public:

    OutputControls( const char	    *name, 
		    Widget	    parent,
		    int		    blockCapacity, 
		    int		    channelCapacity, 
		    Boolean	    useLevelMeters,
		    Boolean	    canDetermineDigitalInputRate,
		    Boolean	    useSamplingRateOptionMenus,

		    Boolean	    presenterFlatPanelPresent,
		    int		    graphicsFD,
		    PresenterWindow **presenterWindowHandle);
    ~OutputControls();
    const char *className();

    int		    _blockCapacity;
    int		    _channelCapacity;
    Boolean	    _useLevelMeters;
    Boolean	    _useSamplingRateOptionMenus;
    Boolean	    _canDetermineDigitalInputRate;
    Boolean	    _presenterFlatPanelPresent;
    PresenterWindow    **_presenterWindowHandle;				

    int		    _graphicsFD;

    Boolean	_gangState;		// True = ganged
    int		_gangValue[MAX_OUTPUT_CHANNELS];
    long	_dacValue[MAX_OUTPUT_CHANNELS];
    int		    _sliderTickMarkType;


    Widget	 _blockContainer;
    Widget	    _block[OUTPUT_SLIDER_BLOCKS];
    Widget	    _blockLabelAndSlider[OUTPUT_SLIDER_BLOCKS];
    Widget		_sliderBlock[OUTPUT_SLIDER_BLOCKS];
    Widget		_meterSegmentLabels[OUTPUT_SLIDER_BLOCKS];
    Widget		_slider[MAX_OUTPUT_CHANNELS];
    Widget		_sliderLabel[MAX_OUTPUT_CHANNELS];
    VkDeck		*_sliderTickDeck[OUTPUT_SLIDER_BLOCKS];
    VkTickMarks		    *_tickMarks[OUTPUT_SLIDER_BLOCKS][2];
    VkTickMarks		*_fauxTickMarks[OUTPUT_SLIDER_BLOCKS];

    Widget		_meterFrame[MAX_OUTPUT_CHANNELS];
    VkVuMeter		*_meter[MAX_OUTPUT_CHANNELS];

    void setLevelSliderPosition( int channelNumber, long audioHardwareValue );
    void enableLevelSliders(Boolean enableStatus);
    void gangSliders( Boolean );

    void setSamplingRateLabel(int samplingRate, int digitalSamplingRate); 
    void showMeters( Boolean );
    void setMeter( int channelNumber, int averageValue, int peakValue );

    void setSliderTickMarkType(char type);

protected: 

    Widget	    _blockLabel[OUTPUT_SLIDER_BLOCKS];

/* no sampling rate option menu */
    Widget	    _samplingRateLabel[OUTPUT_SLIDER_BLOCKS];
/* stuf for sampling rate option menu */
    VkOptionMenu    *_samplingRateOptionMenu[OUTPUT_SLIDER_BLOCKS];

    void levelSliderMoved( Widget, XtPointer );
    long HardwareLevelFromSliderValue( int sliderValue );
    int SliderValueFromHardwareLevel( long alHardwareLevel );

private: 
    static void		levelSliderCB( Widget, XtPointer, XtPointer );

/* stuff for option menus */
    static VkMenuDesc	optionPaneDescription[];
    static void		optionMenuCB( Widget, XtPointer, XtPointer );
};
#endif

