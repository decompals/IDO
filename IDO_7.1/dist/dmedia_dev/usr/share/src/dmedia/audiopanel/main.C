/* **************************************************************
 *    Original contributors by Terry Weissman, Bruce Karsh, Doug Cook,
 *	Amit Shoham, Marc Callow 
 *    ViewKit/Motif version by Gints Klimanis
 *				1991-5
 * ************************************************************** */
#include <bstring.h>
#include <ctype.h>
#include <errno.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#include <audio.h>

// for Presenter flat panel display
#include <sys/fpanel.h>
#include <sys/gfx.h>
#include <sys/ng1.h>
#include <sys/fcntl.h>

#include <Vk/VkApp.h>
#include <Vk/VkDeck.h>
#include <Vk/VkFormat.h>
#include <Vk/VkWindow.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkResource.h>
#include <Xm/Label.h>
#include <Vk/VkRunOnce.h>
#include <Vk/VkCallbackList.h>
#include <Vk/VkNameList.h>
#include <Vk/VkWarningDialog.h>

#include "ApanelWindow.h"
#include "apaneldefs.h"

char	    inputChannelCapacity, outputChannelCapacity, 
	    overallChannelCapacity;
Boolean	    haveStereoMicrophoneAbility;
Boolean	    canDetermineDigitalInputRate;
Boolean	    canChangeChannelMode;

Boolean	    havePresenterFlatPanelDisplay;

int	    presenterGraphicsFD;
Boolean	    printErrorsToConsole = False;

// keep some extra storage around so Presenter levels can be appended 
//  string to specify audiopanel usage 
char IP12IP20ApplicationUsage[800] = 
"  [-dbscale]    [-decadescale]\n\
  [-ganginput]  [-unganginput]\n\
  [-gangoutput] [-ungangoutput]\n\
  [-listeningled]\n\
  [-meteron]    [-meteroff]\n\
  [-monitoron]  [-monitoroff]\n\
  [-mute]       [-unmute]\n\
  [-openfile    filename]\n\
  [-savefile    filename]\n\
  [-iconic]\n\
  [-print]\n\
  [-nodisplay]\n\
  [-nofork]\n\
  [-pollinterval t (Seconds)]\n\
  [-source      {microphone, line, digital}]\n\
  [-rate r]     [-inrate r] [-outrate r]\n\
  [-inlevels l  {0..255}]   [-outlevels l {0..255}]\n\
  [-inlevelleft  l] [-inlevelright  l]\n\
  [-outlevelleft l] [-outlevelright l]\n";

char IP22ApplicationUsage[900] = 
"  [-dbscale]    [-decadescale]\n\
  [-ganginput]  [-unganginput]\n\
  [-gangoutput] [-ungangoutput]\n\
  [-iconic]\n\
  [-listeningled]\n\
  [-meteron]    [-meteroff]\n\
  [-monitoron]  [-monitoroff]\n\
  [-mute]       [-unmute]\n\
  [-openfile    filename]\n\
  [-savefile    filename]\n\
  [-iconic]\n\
  [-print]\n\
  [-nodisplay]\n\
  [-nofork]\n\
  [-pollinterval t (Seconds)]\n\
  [-channels    {2, 4}]\n\
  [-micmode     {mono, stereo}]\n\
  [-source      {microphone, line, digital}]\n\
  [-rate r]     [-inrate r] [-outrate r]\n\
  [-inlevels    l {0..255}] [-outlevels l {0..255}]\n\
  [-inlevelleft1 l] [-inlevelright1 l]\n\
  [-inlevelleft2 l] [-inlevelright2 l]\n\
  [-outlevelleft l] [-outlevelright l]\n";


char PresenterFlatPanelOptions[] = 
"  [-presenterlevels]     {0..26}]\n\
  [-presentertone]       {0..9}]\n\
  [-presenter]\n";

char *applicationUsage = IP12IP20ApplicationUsage; 

SomeData	someData;

class Ginsu : public VkApp 
{
public:
    Ginsu(char		    *applicationClassName,
	char		    *windowName,
	int		    *arg_c,
	char		    **arg_v,
	SomeData	    *startUpData,
	XrmOptionDescRec    *optionList = NULL,
	int		    sizeOfOptionList = 0);  
    ~Ginsu();

    SomeData data;
    ApanelWindow *window;

    void configureGUIWidgets(SomeData *d);

protected:

private:
    static XtResource _resources[]; 
};

#ifdef OLDE
// Description of command line options 
static XrmOptionDescRec _commandLineOptions[] = 
{
    {"-pollinterval",	"*pollInterval",	XrmoptionSepArg, "0"},
    {"-nofork",		"*autoFork",		XrmoptionNoArg,	 "False"},
    {"-monitor",	"*initMonitor",		XrmoptionNoArg,	 "1"},
    {"-monitoroff",	"*initMonitorOff",	XrmoptionNoArg,	 "1"},
    {"-monitoron",	"*initMonitorOn",	XrmoptionNoArg,	 "1"},
    {"-meter",		"*initMeter",		XrmoptionNoArg,	 "1"},
    {"-meteroff",	"*initMeterOff",	XrmoptionNoArg,	 "1"},
    {"-meteron",	"*initMeterOn",		XrmoptionNoArg,	 "1"},
    {"-mute",		"*initMute",		XrmoptionNoArg,	 "1"}, 
    {"-unmute",		"*initUnmute",		XrmoptionNoArg,	 "1"},
    {"-rate",		"*initSamplingRate",	XrmoptionSepArg, "0"},
    {"-inrate",		"*initInSamplingRate",	XrmoptionSepArg, "0"},
    {"-outrate",	"*initOutSamplingRate",	XrmoptionSepArg, "0"},
    {"-inlevels",	"*initInLevels",	XrmoptionSepArg, "-1"},
    {"-outlevels",	"*initOutLevels",	XrmoptionSepArg, "-1"},
    {"-source",		"*initSource",		XrmoptionSepArg, "False"},
    {"-dbscale",	"*dbScale", 		XrmoptionNoArg,	 "True"},
    {"-decadescale",	"*decadeScale",		XrmoptionNoArg,	 "True"},
    {"-nodisplay",	"*noDisplay",		XrmoptionNoArg,	 "False"}, 
    {"-iconic ",	"*iconify",		XrmoptionNoArg,	 "False"}, 
    {"-openfile",	"*openFileName",	XrmoptionSepArg, "0"},
    {"-savefile",	"*saveFileName",	XrmoptionSepArg, "0"},

    {"-ganginput",	"*initGangInput",	XrmoptionNoArg,  "1"},
    {"-gangoutput",	"*initGangOutput",	XrmoptionNoArg,  "1"},

    {"-unganginput",	"*initGangInput",	XrmoptionNoArg,  "0"},
    {"-ungangoutput",	"*initGangOutput",	XrmoptionNoArg,  "0"},

    {"-listeningled",	"*listeningLED",	XrmoptionNoArg,	 "True"},
    {"-listening",	"*listeningLED",	XrmoptionNoArg,	 "True"},
    {"-listen",	        "*listeningLED",	XrmoptionNoArg,	 "True"},

// Indy/Indigo2 options 
// nchans is olde style 
    {"-nchans",		"*initChannels",	XrmoptionSepArg, "0"},

    {"-channels",	"*initChannels",	XrmoptionSepArg, "0"},
    {"-micmode",	"*initMicMode",		XrmoptionSepArg, "0"},
    {"-micmode",	"*initMicrophoneMode",	XrmoptionSepArg, "0"},

// Presenter flat panel audio options 
    {"-presenterlevels", "*initPresenterLevels", XrmoptionSepArg, "-1"},
    {"-presentertone",   "*initPresenterTone",   XrmoptionSepArg, "-1"},
    {"-presenter",       "*showPresenter",       XrmoptionNoArg,  "True"},

    {"-inlevelleft",	"*initInLevelLeft1",	XrmoptionSepArg, "-1"},
    {"-inlevelright",	"*initInLevelRight1",	XrmoptionSepArg, "-1"},
    {"-inlevelleft1",	"*initInLevelLeft1",	XrmoptionSepArg, "-1"},
    {"-inlevelright1",	"*initInLevelRight1",	XrmoptionSepArg, "-1"},
    {"-inlevelleft2",	"*initInLevelLeft2",	XrmoptionSepArg, "-1"},
    {"-inlevelright2",	"*initInLevelRight2",	XrmoptionSepArg, "-1"},
    {"-outlevelleft",	"*initOutLevelLeft",	XrmoptionSepArg, "-1"},
    {"-outlevelright",	"*initOutLevelRight",	XrmoptionSepArg, "-1"},
    {"-nounique",	"*noUnique",		XrmoptionNoArg,	 "True"},

// not presented on command line -- INVISIBLE 
    {"-spinaltap",	"*spinalTap",		XrmoptionNoArg,	 "True"}, 
    {"-sync",		"*synchronize",		XrmoptionNoArg,	 "True"},
    {"-debug",		"*debug",		XrmoptionNoArg,	 "True"},
    {"-meterInterval",	"*meterInterval",	XrmoptionSepArg, NULL},
    {"-ratemenus",	"*samplingRateOptionMenus", XrmoptionNoArg, "True"},
};
#endif

// Description of resources to be retrieved 
XtResource Ginsu::_resources[] = 
{
// resource name, class, type, size, offset, 
//    default type in default address field, default value of resource 
{"pollInterval",	"PollInterval",	    XmRFloat,	    sizeof(XmRFloat),	XtOffset(Ginsu *, data.audioHardwarePollInterval),XmRString, (XtPointer) DEFAULT_AUDIO_HARDWARE_POLL_INTERVAL},
{"autoFork",		"AutoFork",	    XmRBoolean,	    sizeof(XmRBoolean), XtOffset(Ginsu *, data.autoFork),		XmRString, (XtPointer) "True"},
{"initMonitor",		"InitMonitor",	    XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initMonitor),		XmRString, (XtPointer) "-1"},
{"initMonitorOff",	"InitMonitorOff",   XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initMonitorOff),	        XmRString, (XtPointer) "-1"},
{"initMonitorOn",	"InitMonitorOn",    XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initMonitorOn),		XmRString, (XtPointer) "-1"},
{"initMeter",		"InitMeter",	    XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initMeter),		XmRString, (XtPointer) "-1"},
{"initMeterOff",	"InitMeterOff",	    XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initMeterOff),		XmRString, (XtPointer) "-1"},
{"initMeterOn",		"InitMeterOn",	    XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initMeterOn),		XmRString, (XtPointer) "-1"},
{"initMute",		"InitMute",	    XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initMute),		XmRString, (XtPointer) "-1"},
{"initUnmute",		"InitUnmute",	    XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initUnmute),		XmRString, (XtPointer) "-1"},
{"initChannels",	"InitChannels",	    XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initChannels),		XmRString, (XtPointer) "-1"},
{"initMicrophoneMode",	"InitMicrophone",   XmRString,	    sizeof(XmRString),	XtOffset(Ginsu *, data.initMicrophoneMode),	XmRString, (XtPointer) "False"},
{"initInSamplingRate",	"InitInSamplingRate",XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initInputSamplingRate),  XmRString,(XtPointer) "0"},
{"initOutSamplingRate",	"InitOutSamplingRate",XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initOutputSamplingRate), XmRString,(XtPointer) "0"},
{"initInLevels",	"InitInLevels",	    XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initInputLevels),	XmRString, (XtPointer) "-1"},
{"initOutLevels",	"InitOutLevels",    XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initOutputLevels),	XmRString, (XtPointer) "-1"},
{"initSource",		"InitSource",	    XmRString,	    sizeof(XmRString),	XtOffset(Ginsu *, data.initInputSource),	XmRString, (XtPointer) "False"},
{"inputMeterDCFilter",  "InputMeterDCFilter",XmRBoolean,    sizeof(XmRBoolean),	XtOffset(Ginsu *, data.inputMeterDCFilter),	XmRString, (XtPointer) "True"},
{"dbScale",		"DbScale",	    XmRBoolean,	    sizeof(XmRBoolean),	XtOffset(Ginsu *, data.sliderScaleDecibel),	XmRString, (XtPointer) "False"},
{"decadeScale",		"DecadeScale",	    XmRBoolean,	    sizeof(XmRBoolean),	XtOffset(Ginsu *, data.sliderScaleDecade),	XmRString, (XtPointer) "False"},
{"spinalTap",		"SpinalTap",	    XmRBoolean,	    sizeof(XmRBoolean),	XtOffset(Ginsu *, data.sliderScaleSpinalTap),	XmRString, (XtPointer) "False"},
{"noDisplay",		"NoDisplay",	    XmRBoolean,	    sizeof(XmRBoolean), XtOffset(Ginsu *, data.useDisplay),		XmRString, (XtPointer) "True"},
{"iconify",		"Iconify",	    XmRBoolean,	    sizeof(XmRBoolean),	XtOffset(Ginsu *, data.iconify),		XmRString, (XtPointer) "False"},
{"openFileName",	"OpenFileName",	    XmRString,	    sizeof(XmRString),	XtOffset(Ginsu *, data.openFileName),		XmRString, (XtPointer) NULL},
{"saveFileName",	"SaveFileName",	    XmRString,	    sizeof(XmRString),	XtOffset(Ginsu *, data.saveFileName),		XmRString, (XtPointer) NULL},
{"initGangInput",	"InitGangInput",    XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initGangInputSliders),	XmRString, (XtPointer) "-1"},
{"initGangOutput",	"InitGangOutput",   XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initGangOutputSliders),	XmRString, (XtPointer) "-1"},
{"listeningLED",	"UseListeningLED",  XmRBoolean,	    sizeof(XmRBoolean),	XtOffset(Ginsu *, data.useListeningLED),	XmRString, (XtPointer) "False"},
{"samplingRateOptionMenus", "UseSamplingRateOptionMenus", XmRBoolean, sizeof(XmRBoolean), XtOffset(Ginsu *, data.useSamplingRateOptionMenus), XmRString, (XtPointer) "False"},


// Presenter flat panel options 
{"initPresenterLevels",	"InitPresenterLevels",XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initPresenterLevels),	XmRString, (XtPointer) "-1"},
{"initPresenterTone",	"InitPresenterTone",XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initPresenterTone),	XmRString, (XtPointer) "-1"},
{"showPresenter",       "ShowPresenter",    XmRBoolean,	    sizeof(XmRBoolean),	XtOffset(Ginsu *, data.showPresenter),		XmRString, (XtPointer) "False"},

// not yet presented in command line 
{"meterInterval",	"MeterInterval",    XmRString,	    sizeof(XmRString),  XtOffset(Ginsu *, data.meterUpdateInterval),	XmRString, (XtPointer) DEFAULT_METER_UPDATE_INTERVAL},
{"synchronize",		"Synchronize",	    XmRBoolean,	    sizeof(XmRBoolean),	XtOffset(Ginsu *, data.synchronize),		XmRString, (XtPointer) "False"},
{"debug",		"Debug",	    XmRBoolean,	    sizeof(XmRBoolean),	XtOffset(Ginsu *, data.debug),			XmRString, (XtPointer) "False"},
{"noUnique",		"NoUnique",	    XmRBoolean,	    sizeof(XmRBoolean),	XtOffset(Ginsu *, data.moreThanOneInstancePerHostOnXDisplay),XmRString, (XtPointer) "False"},

{"initSamplingRate",	"InitSamplingRate", XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initSamplingRate),	XmRString, (XtPointer) "0"},
{"initInLevelLeft1",	"InitInLevelLeft1", XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initInputLevelLeft1),	XmRString,(XtPointer) "-1"},
{"initInLevelRight1",	"InitInLevelRight1",XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initInputLevelRight1),	XmRString,(XtPointer) "-1"},
{"initInLevelLeft2",	"InitInLevelLeft2", XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initInputLevelLeft2),	XmRString,(XtPointer) "-1"},
{"initInLevelRight2",	"InitInLevelRight2",XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initInputLevelRight2),	XmRString,(XtPointer) "-1"},

{"initOutLevelLeft",	"InitOutLevelLeft", XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initOutputLevelLeft),	XmRString,(XtPointer) "-1"},
{"initOutLevelRight",	"InitOutLevelRight",XmRInt,	    sizeof(XmRInt),	XtOffset(Ginsu *, data.initOutputLevelRight),	XmRString,(XtPointer) "-1"},
};

/* ******************************************************************
 * MatchArg	Return TRUE if given userString matches targetString
 *		to at least maxOf(minimum match, length of userString)
 * ****************************************************************** */
    char 
MatchArg( char *user, char *target, int minimumMatch )
{
#define max(a,b) ( ((a)>(b)) ? (a) : (b))
int matchlen = max(strlen(user), minimumMatch);
#undef max

return ( !strncmp(user, target, matchlen) );
} /* ---- end MatchArg() ---- */

/* ******************************************************************
 * InitApplicationData:
 * ****************************************************************** */
    void 
InitApplicationData( SomeData *d )
{
d->audioHardwarePollInterval = DEFAULT_AUDIO_HARDWARE_POLL_INTERVAL_VALUE;	
d->autoFork       = True;
d->initMonitor    = -1;
d->initMonitorOff = -1;
d->initMonitorOn  = -1;
d->initMeter      = -1;
d->initMeterOff   = -1;
d->initMeterOn    = -1;
d->initMute       = -1;
d->initUnmute     = -1;
d->initChannels   = -1;
d->initMicrophoneMode     = "False";
d->initInputSamplingRate  = 0;	
d->initOutputSamplingRate = 0;    

d->initInputLevels       = -1;
d->initInputLevelLeft1   = -1;
d->initInputLevelRight1  = -1;
d->initInputLevelLeft2   = -1;
d->initInputLevelRight2  = -1;
d->initGangInputSliders  = -1;

d->initOutputLevels      = -1;
d->initOutputLevelLeft   = -1;
d->initOutputLevelRight  = -1;
d->initInputSource       = "False";
d->initGangOutputSliders = -1;

d->synchronize = False;
d->debug       = False;
d->meterUpdateInterval = DEFAULT_METER_UPDATE_INTERVAL_VALUE;		

d->initSamplingRate = 0;

d->inputMeterDCFilter    = True;
d->sliderScaleDecade     = True;
d->sliderScaleDecibel    = False;
d->sliderScaleSpinalTap  = False;
d->useDisplay	       = True;
d->iconify	       = False;
d->moreThanOneInstancePerHostOnXDisplay = False;
d->openFileName	       = NULL;
d->saveFileName	       = NULL;
d->useListeningLED       = False;
d->useSamplingRateOptionMenus = False;

// Presenter flat panel options
d->initPresenterLevels = -1;
d->initPresenterTone   = -1;
d->showPresenter       = False;
} /* ---- end InitApplicationData() ---- */

/* ***************************************************************
 * ParseCommandLine:    parse input command line for user options 
 *			Just check that argument, where required,
 *			are present.  Argument validity is verified
 *			later on.
 * *************************************************************** */
    static void
ParseCommandLine(int argc, char **argv, SomeData *d) 
{ 
for (int i = 1; i < argc; i++)
    { 
//printf("argc%d: '%s'\n", i, argv[i]);

// check for -h or -help 
    if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "-help"))
	{ 
	printf("Usage: %s\n", applicationUsage); 
	exit(1); 
	}

    else if (!strcmp(argv[i], "-pollinterval"))
	{
	char	*s;
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	s = argv[i];
	if (!isdigit(s[0]))
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	d->audioHardwarePollInterval = atof(s);
	}

    else if (MatchArg(argv[i], "-nofork", 2))
	d->autoFork = False;

    else if (!strcmp(argv[i], "-monitor"))
	d->initMonitor    = True;
    else if (!strcmp(argv[i], "-monitoroff"))
	d->initMonitorOff = True;
    else if (!strcmp(argv[i], "-monitoron"))
	d->initMonitorOn  = True;

    else if (!strcmp(argv[i], "-meter"))
	d->initMeter    = True;
    else if (!strcmp(argv[i], "-meteroff"))
	d->initMeterOff = True;
    else if (!strcmp(argv[i], "-meteron"))
	d->initMeterOn  = True;

    else if (!strcmp(argv[i], "-mute"))
	d->initMute   = True;
    else if (!strcmp(argv[i], "-unmute"))
	d->initUnmute = True;

    else if (!strcmp(argv[i], "-channels"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	if (!isdigit(s[0]))
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	d->initChannels = atoi(s);
	}

    else if (!strcmp(argv[i], "-micmode"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	d->initMicrophoneMode = strdup(s);
	}

    else if (!strcmp(argv[i], "-rate"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	if (isdigit(s[0]) && s[0] != '0')
	    d->initSamplingRate = atoi(s);
    /* allow -2 = digital input rate */
	else if (s[0] == '-' && s[1] == '2')
	    d->initSamplingRate = atoi(s);
	else
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	}

    else if (!strcmp(argv[i], "-inrate"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }

	char *s = argv[i];
	if (isdigit(s[0]) && s[0] != '0')
	    d->initInputSamplingRate = atoi(s);
   /* allow -2 = digital input rate */
    /*  allow -7 .. -2 */
	else if (s[0] == '-' && (s[1] >= '2' || s[1] <= '7'))
	    d->initInputSamplingRate = atoi(s);
	else
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	}

    else if (!strcmp(argv[i], "-outrate"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	if (isdigit(s[0]) && s[0] != '0')
	    d->initOutputSamplingRate = atoi(s);
    /* allow -1 = use input rate and -2 = digital input rate */
    /*  allow -7 .. -1 */
	else if (s[0] == '-' && (s[1] >= '1' || s[1] <= '7'))
	    d->initOutputSamplingRate = atoi(s);
	else
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	}

    else if (!strcmp(argv[i], "-inlevels"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	if (!isdigit(s[0]))
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	d->initInputLevels = atoi(s);
	}

    else if (!strcmp(argv[i], "-outlevels"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	if (!isdigit(s[0]))
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	d->initOutputLevels = atoi(s);
	}

    else if (!strcmp(argv[i], "-inlevelleft1"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	if (!isdigit(s[0]))
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	d->initInputLevelLeft1 = atoi(s);
	}

    else if (!strcmp(argv[i], "-inlevelleft2"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	if (!isdigit(s[0]))
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	d->initInputLevelLeft2 = atoi(s);
	}

    else if (!strcmp(argv[i], "-inlevelright1"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	if (!isdigit(s[0]))
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	d->initInputLevelRight1 = atoi(s);
	}

    else if (!strcmp(argv[i], "-inlevelright2"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	if (!isdigit(s[0]))
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	d->initInputLevelRight2 = atoi(s);
	}

    else if (!strcmp(argv[i], "-outlevelleft"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	if (!isdigit(s[0]))
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	d->initOutputLevelLeft = atoi(s);
	}

    else if (!strcmp(argv[i], "-outlevelright"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	if (!isdigit(s[0]))
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	d->initOutputLevelRight = atoi(s);
	}

    else if (!strcmp(argv[i], "-source"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	d->initInputSource = strdup(s);
	}

    else if (!strcmp(argv[i], "-ganginput"))
	d->initGangInputSliders = True;
    else if (!strcmp(argv[i], "-unganginput"))
	d->initGangInputSliders = False;

    else if (!strcmp(argv[i], "-gangoutput"))
	d->initGangOutputSliders = True;
    else if (!strcmp(argv[i], "-ungangoutput"))
	d->initGangOutputSliders = False;

    else if (!strcmp(argv[i], "-dbscale"))
	d->sliderScaleDecibel = True;
    else if (!strcmp(argv[i], "-decadescale"))
	d->sliderScaleDecade = True;
    else if (!strcmp(argv[i], "-spinaltap"))
	d->sliderScaleSpinalTap = True;

    else if (!strcmp(argv[i], "-nodisplay"))
	d->useDisplay = False;
    else if (!strcmp(argv[i], "-iconic"))
	d->iconify = False;

    else if (!strcmp(argv[i], "-openfile"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	d->openFileName = strdup(argv[i]);
	}
    else if (!strcmp(argv[i], "-savefile"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	d->saveFileName = strdup(argv[i]);
	}

    else if (!strcmp(argv[i], "-listeningled"))
	d->useListeningLED = True;

    else if (!strcmp(argv[i], "-ratemenus"))
	d->useSamplingRateOptionMenus = True;

    else if (!strcmp(argv[i], "-presenterlevels"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	if (!isdigit(s[0]))
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	d->initPresenterLevels = atoi(s);
	}

    else if (!strcmp(argv[i], "-presentertone"))
	{
	if (++i >= argc)
	    {
	    fprintf(stderr, "Well, supply %s argument !\n", argv[i-1]);
	    exit   (-1);
	    }
	char *s = argv[i];
	if (!isdigit(s[0]))
	    {
	    fprintf(stderr, "Usage: %s\n", applicationUsage);
	    exit   (-1);
	    }
	d->initPresenterTone = atoi(s);
	}

    else if (!strcmp(argv[i], "-presenter"))
	d->showPresenter = True;

    else if (!strcmp(argv[i], "-print"))
	{
	PrintAudioHardwareState();
	exit(0);
	}

// uh oh, a bogus option  
    else if (argv[i][0] == '-')
	{ 
	fprintf(stderr, "Invalid option: '%s'\n", argv[i]); 
	fprintf(stderr, "Usage: %s\n", applicationUsage); 
	exit(1); 
	}
// try to open a configuration file 
    else if (access(argv[i], R_OK) != -1)
	{
	d->openFileName = strdup(argv[i]);
	}
    } 
}   /* ---- end ParseCommandLine() ---- */

/* ******************************************************************
 * Ginsu:	class constructor	
 * ****************************************************************** */
Ginsu::Ginsu(char		*applicationClassName,
	    char		*    /* windowName */,
	    int			*arg_c,
	    char		**arg_v,
	    SomeData		*d,
	    XrmOptionDescRec	*optionList,
	    int			sizeOfOptionList) 
	    : VkApp(applicationClassName,
		    arg_c, 
		    arg_v,
		    optionList,
		    sizeOfOptionList)
{
getResources(_resources, XtNumber(_resources));

// copy back to global 
bcopy(&data, d, sizeof(SomeData));
}   /* ---- end Ginsu::Ginsu() ---- */

/* ******************************************************************
 * ~Ginsu:	class destructor	
 * ****************************************************************** */
Ginsu::~Ginsu()
{
}   /* ---- end ~Ginsu() ---- */

/* ******************************************************************
 * configureGUIWidgets:	configure GUI widgets
 * ****************************************************************** */
    void
Ginsu::configureGUIWidgets(SomeData *d)
{ 
// enable/disable signal level meters  
//    (True = turn on, False = turn off, -1 = do nothing) 
if	(d->initMeter == True || d->initMeterOn == True)
    window->toggleInputMeter(True);
else if (d->initMeterOff == True) 
    window->toggleInputMeter(False);

// gang input sliders (don't need this code because gang is default state) 
VkMenu *menuTitle = (VkMenu *) window->menu()->findNamedItem("menuTitleOptions");
VkMenuToggle *menuItem;

// gang input sliders initGangInputSliders = {-1, 0=False, 1=True} 
if	(d->initGangInputSliders == True) 
    {
    menuItem = (VkMenuToggle *) menuTitle->findNamedItem("independentInputSliders");
    menuItem->setVisualState(False);
    window->gangInputSliders(True);    
    }
// ungang input sliders 
else if (d->initGangInputSliders == False) 
    {
    menuItem = (VkMenuToggle *) menuTitle->findNamedItem("independentInputSliders");
    menuItem->setVisualState(True);
    window->gangInputSliders(False);    
    }

// gang output sliders initGangOutputSliders = {-1, 0=False, 1=True} 
if	(d->initGangOutputSliders == True) 
    {
    menuItem = (VkMenuToggle *) menuTitle->findNamedItem("independentOutputSliders");
    menuItem->setVisualState(False);

    window->gangOutputSliders(True);    
    }
// ungang output sliders 
else if (d->initGangOutputSliders == False) 
    {
    menuItem = (VkMenuToggle *) menuTitle->findNamedItem("independentOutputSliders");
    menuItem->setVisualState(True);

    window->gangOutputSliders(False);    
    }

// set listening led 
if (d->useListeningLED) 
    {
    menuItem = (VkMenuToggle *) menuTitle->findNamedItem("listeningLED");
    menuItem->setVisualState(True);
// do not make this call.  Set up in ApanelWindow constructor
//    window->useInputListeningLED(True);    
    }

// set slider tick mark type to decade scale [0..10] (input AND output sliders) 
if (d->sliderScaleDecade) 
    {
    menuItem = (VkMenuToggle *) menuTitle->findNamedItem("decibelScale");
    menuItem->setVisualState(False);

    window->setInputSliderScale(SCALE_DECADE);
    window->setOutputSliderScale(SCALE_DECADE);
// can't do Presenter deck here: not yet created 
    }

// set slider tick mark type to spinal tap [0..11] scale (input AND output sliders) 
if (d->sliderScaleSpinalTap) 
    {
    menuItem = (VkMenuToggle *) menuTitle->findNamedItem("decibelScale");
    menuItem->setVisualState(False);

    window->setInputSliderScale(SCALE_SPINAL_TAP);
    window->setOutputSliderScale(SCALE_SPINAL_TAP);
// can't do Presenter deck here: not yet created 
    }

// set slider tick mark type to dbscale [-30..+18] (input sliders only).
//	Output sliders get decade scale. 
if (d->sliderScaleDecibel) 
    {
    menuItem = (VkMenuToggle *) menuTitle->findNamedItem("decibelScale");
    menuItem->setVisualState(True);

    window->setInputSliderScale(SCALE_DECIBEL);
    }
} /* ---- end configureGUIWidgets() ---- */

// This code provided by fujinaga@engr.sgi.com   Yukitomi Fujinaga  
static String
__myLangProc(Display *, String xnl, XtPointer)
{
    if (! setlocale(LC_ALL, xnl))
        XtWarning("locale not supported by C library, locale unchanged");
    if (! XSupportsLocale()) {
        XtWarning("locale not supported by Xlib, locale set to C");
        setlocale(LC_ALL, "C");
    }
    if (! XSetLocaleModifiers(""))
        XtWarning("X locale modifiers not supported, using default");
    setlocale(LC_NUMERIC, "C");
    return setlocale(LC_ALL, NULL);
}

/* ******************************************************************
 * main:	
 * ****************************************************************** */
    void 
main(int argc, char **argv)
{ 
// Code for Internationalization 
XtSetLanguageProc(NULL, __myLangProc, NULL);

//   change behavior according to context.  
//   If launched by typing, print messages and report errors. 
//   If launched from within application, do not print messages or report errors.
if (isatty(0))
    printErrorsToConsole = True;

// need to check if an X display open is possible.  This check will prevent
// ViewKit from printing an "Unable to open display" error into shell
Display *display = XOpenDisplay(getenv("DISPLAY"));
Boolean ableToOpenDisplay = False;
if (display)
    {
    XCloseDisplay(display);
    ableToOpenDisplay = True;
    }

// Attempt to open Indigo style audio IO port.  On failure, machine has 
// no audio ability, show warning box with message regarding absence 
//  of audio hardware, and exit application 
int audioLibraryFileID = ::open("/dev/hdsp/hdsp0master", O_RDONLY);
if (audioLibraryFileID < 0)
    {
    if (ableToOpenDisplay)
	{
	VkApp *app = new VkApp(ApplicationClassName, &argc, argv);
	if (app)
	    {
	    theWarningDialog->setTitle(VkGetResource("AudioPanel", ApplicationClassName));
	// this dialog post must be blocked or application will exit immediately afterward,
	// causing the dialog box to flash on and off before its contents may be read.
	    theWarningDialog->postBlocked(VkFormat("%s\n%s", 
					VkGetResource("NoAudioDriver", ApplicationClassName),
					VkGetResource("CannotOperate", ApplicationClassName)));
	    }
	}
    else
	fprintf(stderr, "This machine has no audio driver. Audio Panel unable tos operate.\n");

    for (int i = 0; i < 3; i++)
	close(i);
    exit(1);
    }
close(audioLibraryFileID);

// initialize values
InitApplicationData(&someData);
someData.useDisplay = ableToOpenDisplay;

// check for presence of Presenter (Corona) flat panel display
if (ableToOpenDisplay)
    {
    presenterGraphicsFD = OpenPresenter();
    if (presenterGraphicsFD == -1)
	havePresenterFlatPanelDisplay = False;
    else
	{
	havePresenterFlatPanelDisplay = True;
    // append command line options 
	strcat(applicationUsage, PresenterFlatPanelOptions);
	}
    }

// instantiate class and parse command line 
if (!ableToOpenDisplay)
    {
    ParseCommandLine(argc, argv, &someData);
    DataStructureToAudioHardware(&someData, 
				&inputChannelCapacity,   &outputChannelCapacity,
				&overallChannelCapacity, &haveStereoMicrophoneAbility,	    
				&presenterGraphicsFD,	 &canChangeChannelMode,
				&applicationUsage,
				IP22ApplicationUsage,	 IP12IP20ApplicationUsage);
// configure audio hardware state
    if (someData.openFileName)
	LoadAudioHardwareState(someData.openFileName);

// save audio hardware state
    if (someData.saveFileName)
	{
	FILE *fd = fopen(someData.saveFileName, "w");
	if (fd) 
	    {
	    WriteAudioHardwareState(fd, inputChannelCapacity, overallChannelCapacity,   
				    haveStereoMicrophoneAbility);
	    fclose(fd);
	    }
	}

    exit(0);
    }

// application created with start up specification:
// 1st resources, 2nd (optional) $HOME/.audiopanelrc, 3rd command line options (including openfile)
Ginsu *application = new Ginsu(ApplicationClassName, NULL, &argc, argv, &someData);
if (!application) 
    Punt("UH OH.  Application error.");

// get user set up and blast applicable parameters to the hardware
ParseCommandLine(argc, argv, &someData);
DataStructureToAudioHardware(&someData, 
			    &inputChannelCapacity,	&outputChannelCapacity,
			    &overallChannelCapacity,	&haveStereoMicrophoneAbility,	    
			    &presenterGraphicsFD,	&canChangeChannelMode,
			    &applicationUsage,
			    IP22ApplicationUsage,	IP12IP20ApplicationUsage);

// this code here to prevent current instance of audiopanel from popping up
// out of iconized state
if (someData.useDisplay == False)
    exit(0);

// copy command line into RunOnce object 
VkNameList *args = new VkNameList();
for (int i = 1; i < argc; i++)
    args->add(argv[i]);

// code to ensure single instance of audiopanel
// MUST follow DataStructureToAudioHardware() or the command line parameters
// will not be blasted to hardware ??
VkRunOnce *runOnce;
if (someData.moreThanOneInstancePerHostOnXDisplay == False)
    runOnce = new VkRunOnce(args, True, ApplicationClassName);

// create window 
char *windowName        = Get4DwmTitle(VkGetResource("AudioPanel", ApplicationClassName));
ApanelWindow *windowPtr = application->window;
windowPtr = application->window = new ApanelWindow(ApplicationClassName,
						windowName,
						windowName,
						inputChannelCapacity,
						outputChannelCapacity,
						overallChannelCapacity,
						haveStereoMicrophoneAbility,
						canChangeChannelMode,
						someData.useSamplingRateOptionMenus,
						someData.useListeningLED,
						havePresenterFlatPanelDisplay,	 
						someData.showPresenter,
						presenterGraphicsFD,   
						someData.useDisplay);

// install handler to process command line for non-unique instances
if (someData.moreThanOneInstancePerHostOnXDisplay == False)
    VkAddCallbackMethod(VkRunOnce::invokedCallback, 
			runOnce, 
			windowPtr, 
			ApanelWindow::update, 
			NULL);

// configure widgets, show window and run application 
windowPtr->_audioHardwarePollInterval = 
    windowPtr->intervalToMilliseconds(someData.audioHardwarePollInterval);

windowPtr->useInputMeterDCFilter(someData.inputMeterDCFilter);

// check home directory for configuration file ".audiopanelrc"
{
char *homePath = getenv("HOME");
if (homePath)
    {
    char audiopanelrcPath[1000];
    strcpy(audiopanelrcPath, homePath);
    strcat(audiopanelrcPath, "/.audiopanelrc");
    if (access(audiopanelrcPath, R_OK) != -1) 
	windowPtr->readPreferencesFromFile(audiopanelrcPath);
    }
}

// read application preferences from file (AFTER adding meter VkPeriodic) 
if (someData.openFileName)
    windowPtr->readFullStateFromFile(someData.openFileName);

// save audiopanel state if specified
if (someData.saveFileName)
    windowPtr->writeFullStateToFile(someData.saveFileName);

// for no display option, exit here
if (!someData.useDisplay)
    exit(0);

// fork or not to fork 
if (someData.autoFork) 
    {
    switch (fork()) 
	{
	case 0:
	break;
	case -1:
	    Punt("Can't fork.");
	default:
	    exit(0);
	}
    }

windowPtr->pollAudioHardwareState();
application->configureGUIWidgets(&someData);
windowPtr->show();

theApplication->setVersionString("productInfo");
application->run();
} /* ---- end main() ---- */

