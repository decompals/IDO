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
#ifndef __APANEL_H__
#define __APANEL_H__ 
#include <stdio.h>
#include <Vk/VkApp.h>

// 16 Hz meter update rate
#define DEFAULT_METER_UPDATE_RATE		    16
#define DEFAULT_METER_UPDATE_INTERVAL_VALUE	     0.0625
#define DEFAULT_METER_UPDATE_INTERVAL		    "0.0625"

#define ApplicationClassName "apanel"
#define DefaultParameterFileName ".audiopanelrc"

#define MAXIMUM_SAMPLING_RATE		50000
#define SOME_BOGUS_SAMPLING_RATE	-2000
#define SOME_BOGUS_INPUT_SOURCE		-1

// values for slider tick mark types
#define SCALE_DECADE	    0
#define SCALE_SPINAL_TAP    1
#define SCALE_DECIBEL	    2

char *GetPathLead(char *path);
char *GetPathTail(char *path);
void Punt(char *message);
void Print(char *format, ...);

typedef struct somedata {
    float	audioHardwarePollInterval;	// seconds 
    Boolean	autoFork;
    int		initMonitor, initMonitorOff, initMonitorOn;
    int		initMeter, initMeterOff, initMeterOn;
    int		initMute, initUnmute;
    int		initChannels;
    char	*initMicrophoneMode;
    int		initSamplingRate;
    int		initInputSamplingRate;	    // samples/second 
    int		initOutputSamplingRate;    // samples/second 

    int		initInputLevels;
    int		initInputLevelLeft1;
    int		initInputLevelRight1;
    int		initInputLevelLeft2;
    int		initInputLevelRight2;

    int		initOutputLevels;
    int		initOutputLevelLeft;
    int		initOutputLevelRight;
    char	*initInputSource;

// member functions to be initialized from command line
    Boolean	synchronize;
    Boolean	debug;

    float	meterUpdateInterval;		// seconds 

    int		initGangInputSliders;
    int		initGangOutputSliders;

    Boolean	inputMeterDCFilter;
    Boolean	sliderScaleDecade, sliderScaleDecibel, sliderScaleSpinalTap;
    Boolean	useDisplay;
    Boolean	iconify;
    Boolean	moreThanOneInstancePerHostOnXDisplay;
    char	*openFileName;
    char	*saveFileName;
    Boolean	useListeningLED;
    Boolean	useSamplingRateOptionMenus;

// Presenter flat panel options
    int		initPresenterLevels;
    int		initPresenterTone;
    Boolean     showPresenter;
} SomeData;

void DataStructureToAudioHardware(SomeData *d,
				char	*inputChannelCapacity, 
				char	*outputChannelCapacity,
				char	*overallChannelCapacity, 
				Boolean *haveStereoMicrophoneAbility,	    
				int	*presenterGraphicsFD,
				Boolean *canChangeChannelMode,
				char	**applicationUsage,
				char	*IP22ApplicationUsage,
				char	*IP12IP20ApplicationUsage);
int  LoadAudioHardwareState(char  *fileName);
void WriteAudioHardwareState(FILE *fd, 
			    char inputChannelCapacity, 
			    char overallChannelCapacity,
			    char haveStereoMicrophoneAbility);
void PrintAudioHardwareState();

char *ExpandPath(char *path);
int RemoveSpaces(char *s);

char *Get4DwmTitle(char *applicationTitle);

// Presenter (Corona Flat Panel Display) functions
int	OpenPresenter();
Boolean ClosePresenter(int graphicsFD);

Boolean SetFlatPanelSpeakerLevel(int graphicsFD, int channel, int value);
Boolean SetFlatPanelSpeakerTone (int graphicsFD, int value); 
Boolean SetFlatPanelSpeakerMute (int graphicsFD, Boolean value);

int GetFlatPanelSpeakerLevel(int graphicsFD, int channel); 
int GetFlatPanelSpeakerTone (int graphicsFD);
int GetFlatPanelSpeakerMute (int graphicsFD); 

#endif /* __APANEL_H__ */
