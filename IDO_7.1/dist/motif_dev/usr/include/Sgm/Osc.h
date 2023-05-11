#ifndef OSC_H
#define OSC_H
#ifdef __cplusplus
extern "C" {
#endif 
extern WidgetClass sgOscWidgetClass;

int 	SgOscGetOffset 	(Widget 	w, 
			 int 		time);
Widget 	SgCreateOsc 	(Widget 	parent, 
			 char 		*name, 
			 ArgList 	args, 
			 Cardinal 	nArgs);
void 	SgOscUpdate 	(Widget 	w); 
void 	SgOscMove 	(Widget 	w, 
			 int 		offset);

enum SgOscDataFormat { SgTWOSCOMP_8, SgTWOSCOMP_16, SgTWOSCOMP_24, SgFLOAT_32, SgDOUBLE_64 };

enum SgOscDisplayMode { SgNONE, SgLEFT, SgRIGHT, SgBOTH };

enum SgOscBufferFormat { SgMONO, SgSTEREO };

typedef struct _SgOscClassRec *SgOscWidgetClass;
typedef struct _SgOscRec *SgOscWidget;

#define SgNtimeScale 	"timeScale"
#define SgNsampleRate	"sampleRate"
#define SgNminimum	"minimum"
#define SgNmaximum	"maximum"
#define SgNdisplayMode	"displayMode"
#define SgNlive		"live"
#define SgNrepeat	"repeat"
#define SgNlChannelOffset "lChannelOffset"
#define SgNrChannelOffset "rChannelOffset"
#define SgNsampleFormat	"sampleFormat"
#define SgNbufferFormat "bufferFormat"
#define SgNsamples 	"samples"
#define SgNplace	"place"
#define	SgNstart	"start"
#define	SgNend		"end"
#define SgNnumSamps	"numSamps"
#define SgNtotalSamps	"totalSamps"
#define SgNlChannelColor "lChannelColor"
#define SgNrChannelColor "rChannelColor"
#define SgNgridColor	"gridColor"
#define	SgNrangeColor	"rangeColor"
#define SgNgridType	"gridType"

#define SgCTimeScale	"TimeScale"
#define SgCSampleRate	"SampleRate"
#define SgCSamples	"Samples"
#define SgCSampleFormat	"SampleFormat"
#define SgCBufferFormat "BufferFormat"
#define	SgCPlace	"Place"
#define	SgCStart	"Start"
#define	SgCEnd		"End"
#define SgCNumSamps	"NumSamps"
#define SgCTotalSamps	"TotalSamps"
#define SgCGridType	"GridType"
#define SgCDisplayMode	"DisplayMode"
#define SgCMin		"Min"
#define SgCMax		"Max"
#define SgCOff		"Off"

#define SgNvalueChangedCallback XmNvalueChangedCallback

typedef struct {
  int		reason;
  XEvent	*event;
  int		value;
} SgOscCallbackStruct;

#ifdef __cplusplus
}
#endif
#endif /* OSC_H */
