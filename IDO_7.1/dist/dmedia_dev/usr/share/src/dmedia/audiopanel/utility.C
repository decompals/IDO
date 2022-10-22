/* **************************************************************
 *    Original contributors by Terry Weissman, Bruce Karsh, Doug Cook,
 *	Amit Shoham, Marc Callow 
 *    ViewKit/Motif version by Gints Klimanis
 *				1991-4
 * ************************************************************** */
#include <bstring.h>
#include <fcntl.h>
#include <errno.h>
#include <invent.h>
#include <pwd.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>

#include <sys/gfx.h>
#include <sys/fcntl.h>
#include <sys/fpanel.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <audio.h>

#include "apaneldefs.h"

/* ******************************************************************
 * Punt:	print string and exit application
 * ****************************************************************** */
    void 
Punt(char *message) 
{
fprintf(stderr, "%s\n", message);
exit(1);
} /* ---- end Punt() ---- */

/* ******************************************************************
 * Print 	
 * ****************************************************************** */
    void 
Print(char *format, ...)
{
extern Boolean printErrorsToConsole;

if (printErrorsToConsole)
    {
    va_list ap;
    va_start(ap, format);
    vfprintf( stdout, format, ap );
    va_end(ap);
    
    fprintf(stdout, "\n" );
    }
}   /* ---- end Print() ---- */

/* ******************************************************************
 * GetPathLead    allocate and return ptr to path lead up to file name
 * ****************************************************************** */
    char *
GetPathLead(char *path)
{
char *lead;
char *p = strrchr(path, '/');

if (!p)
    return (NULL);

lead = strdup(path);
lead[p-path+1] = '\0';

return (lead);
} /* ---- end GetPathLead() ---- */

/* ******************************************************************
 * GetPathTail    get tail of file's path name
 * ****************************************************************** */
    char *
GetPathTail(char *path)
{
char *p = strrchr(path, '/');
if (p) 
    p++;
else 
    p = path;

return (strdup(p));
} /* ---- end GetPathTail() ---- */

/* ******************************************************************
 * SearchDisplayFunction 	
 * ****************************************************************** */
    int
SearchDisplayFunction(inventory_t *pinvent, void *)
{
if (pinvent->inv_class == INV_DISPLAY && pinvent->inv_type == INV_PRESENTER_PANEL) 
    return (1);
return (0);
}   /* ---- end SearchDisplayFunction() ---- */

/* **********************************************************************
 * Get4DwmTitle:     create application title name in format
 * 
 *		    if display == host: 
 *   				return title
 *		    if display != host: 
 *  				return host:title
 * ********************************************************************** */
    char *
Get4DwmTitle(char *applicationTitle)
{
#define MAX_HOST_NAME_LENGTH	    100
#define MAX_APPLICATION_NAME_LENGTH 100
char hostName[MAX_HOST_NAME_LENGTH];

/* acquire host name and assemble window and window icon names */
gethostname(hostName, MAX_HOST_NAME_LENGTH);
hostName[MAX_HOST_NAME_LENGTH-1] = '\0';
/* terminate host name at first '.', i.e., "indy.sgi.com" becomes "indy" */
char	*dot;
if (dot = strchr(hostName, '.')) 
    *dot = '\0';

/* terminate display host name at first '.' or ':', i.e., "indy:0" becomes "indy" */
char displayName[MAX_HOST_NAME_LENGTH];
char *displayVariable = getenv("DISPLAY");
if (displayVariable)
    {
    strcpy(displayName, displayVariable);
    if (dot = strchr(displayName, '.')) 
	*dot = '\0';
    if (dot = strchr(displayName, ':')) 
	*dot = '\0';
    }
else
    displayName[0] = '\0';

/* hostname same as display name */
if (	    displayName[0] == '\0'    ||    /* DISPLAY = :0 */
    !strcmp(displayName, "localhost") ||    /* DISPLAY = localhost:0 */
    !strcmp(displayName, hostName))	    /* DISPLAY = <hostname>:0 */
    return (applicationTitle);

/* hostname NOT same as display name, format is hostname: applicationTitle */
char   *windowName = (char *) malloc(strlen(hostName) + strlen(applicationTitle) + 4);
sprintf(windowName, "%s:%s", hostName, applicationTitle);

return (windowName);
}	/* ---- end Get4DwmTitle() ---- */

/* ******************************************************************
 * OpenPresenter:	
 *
 * Note, before returning 0, may want to free info, detach board, and close 
 *	/dev/graphics 
 *
 * If flat panel is attached and everything else is in order the following
 * routine sets things up and returns 1.  Otherwise it returns 0.
 * ****************************************************************** */
    int
OpenPresenter() 
{
int				presenterGraphicsFD;
struct gfx_attach_board_args	graphicsAttach;
//Boolean				verbose;

//verbose = True;
if (scaninvent(SearchDisplayFunction, (void *)0) == 0)
    {
//  fprintf(stderr, "%s: no flat panel attached\n", ApplicationClassName);
    return (-1);
    }

presenterGraphicsFD = open("/dev/graphics", O_RDWR | O_NOCTTY);
if (presenterGraphicsFD < 0) 
    {
//    if (verbose)
//	fprintf(stderr, "%s: cannot open /dev/graphics\n", ApplicationClassName);
    return (-1);
    }
  
/* attach board */
graphicsAttach.board = 0;
//IRIX 5.3 graphicsAttach.vaddr = (void *) 0x02000000;
// IRIX 5.2
graphicsAttach.vaddr = (void *) 0x02000000;
if (ioctl(presenterGraphicsFD, GFX_ATTACH_BOARD, &graphicsAttach) == -1) 
    {
//    if (verbose)
//	fprintf(stderr, "%s: graphics attach failed\n", ApplicationClassName);
    close(presenterGraphicsFD);
    return (-1);
    }
  
/* XXX map board so we don't fault */
if (ioctl(presenterGraphicsFD, GFX_MAPALL, NULL) == -1) 
    {
//    if (verbose)
//	fprintf(stderr, "%s: graphics mapall failed\n", ApplicationClassName);
    close(presenterGraphicsFD);
    return (-1);
    }
  
return (presenterGraphicsFD);  
} /* ---- end OpenPresenter() ---- */

/* ******************************************************************
 * ClosePresenter:	
 *
 * Note, before returning False, may want to free info, detach board, and close 
 *	/dev/graphics 
 *
 * If flat panel is attached and everything else is in order the following
 * routine sets things up and returns 1.  Otherwise it returns 0.
 * ****************************************************************** */
    Boolean 
ClosePresenter(int presenterGraphicsFD) 
{
if (ioctl(presenterGraphicsFD, GFX_DETACH_BOARD, 0) == -1) 
    {
//    fprintf(stderr, "%s: graphics detach failed\n", ApplicationClassName);
    return (False);
    }

close (presenterGraphicsFD);
return (True);
} /* ---- end ClosePresenter() ---- */

/* ******************************************************************
 * SetFlatPanelSpeakerLevel:		
 * ****************************************************************** */
    Boolean 
SetFlatPanelSpeakerLevel(int presenterGraphicsFD, int channel, int value) 
{
struct I2C_CMD_ARG commands;

/* add 0xc0 for first two bits plus 0x23 for audible volume */
if (value < 0 || value > 26) 
    {
//    fprintf(stderr,
//	    "%s: SetFlatPanelSpeakerLevel(): bogus level: %d\n", ApplicationClassName, value);
    return (False);
    }

if	(channel == 0)
  commands.cmd = I2C_AUDIO_VOL_LEFT;
else if (channel == 1)
  commands.cmd = I2C_AUDIO_VOL_RIGHT;
else
    {
//    fprintf(stderr, "%s: SetFlatPanelSpeakerLevel(): bogus channel#: %d\n", value);
    return (False);
    }

int argument = value + 0xe3;
commands.args = &argument;
if (ioctl(presenterGraphicsFD, PANEL_I2C_CMD, &commands) == -1) 
    {
//    fprintf(stderr, "%s: Newport i2c command failed\n", ApplicationClassName);
    return (False);
    }
if (commands.return_val == 0) 
    {
//    fprintf(stderr, "%s: Unable to execute I2C command\n", ApplicationClassName);
    return (False);
    }

return (True);  
} /* ---- end SetFlatPanelSpeakerLevel() ---- */

/* ******************************************************************
 * GetFlatPanelSpeakerLevel:		
 * ****************************************************************** */
    int 
GetFlatPanelSpeakerLevel(int presenterGraphicsFD, int channel) 
{
struct I2C_CMD_ARG  commands;

if	(channel == 0)
  commands.cmd = I2C_LEFT_VAL;
else if (channel == 1)
  commands.cmd = I2C_RIGHT_VAL;
else
    {
//    fprintf(stderr, "%s: GetFlatPanelSpeakerLevel(): bogus channel#: %d\n", channel);
    return (-1);
    }
  
if (ioctl(presenterGraphicsFD, PANEL_I2C_CMD, &commands) == -1)
    { 
//    fprintf(stderr, "%s: Newport i2c command failed\n", ApplicationClassName); 
    }

// bound to range [0 .. 26]
int value = commands.return_val - 0xe3;
if	(value < 0)
    value = 0;
else if (value > 26)
    value = 26;

return (value);
} /* ---- end GetFlatPanelSpeakerLevel() ---- */

/* ******************************************************************
 * SetFlatPanelSpeakerTone:		
 * ****************************************************************** */
    Boolean 
SetFlatPanelSpeakerTone(int presenterGraphicsFD, int value) 
{
struct I2C_CMD_ARG commands;

// adding 0xf0 for first 4 bits plus 0x02 for beginning of bass 
if (value < 0 || value > 9) 
    {
//    fprintf(stderr,
//	    "%s: Attempting to set tone to an invalid level\n", ApplicationClassName);
    return (False);
    }

commands.cmd  = I2C_AUDIO_TREBLE;
int argument  = value + 0xf2;
commands.args = &argument;
if (ioctl(presenterGraphicsFD, PANEL_I2C_CMD, &commands) == -1) 
    {
//    fprintf(stderr, "%s: Newport i2c command failed\n", ApplicationClassName);
    return (False);
    }
if (commands.return_val == 0) 
    {
//    fprintf(stderr, "%s: Unable to execute I2C command\n", ApplicationClassName);
    return (False);
    }

return (True);  
}   /* ---- end SetFlatPanelSpeakerTone() ---- */

/* ******************************************************************
 * GetFlatPanelSpeakerTone:		
 * ****************************************************************** */
    int 
GetFlatPanelSpeakerTone(int presenterGraphicsFD) 
{
struct I2C_CMD_ARG commands;

commands.cmd = I2C_TREBLE_VAL;
if (ioctl(presenterGraphicsFD, PANEL_I2C_CMD, &commands) == -1) 
    {
//    fprintf(stderr, "%s: Newport i2c command failed\n", ApplicationClassName);
    }

// bound to range [0 .. 9]
int value = commands.return_val - 0xf2;
if	(value < 0)
    value = 0;
else if (value > 9)
    value = 9;

return (value);
}   /* ---- end GetFlatPanelSpeakerTone() ---- */

/* ******************************************************************
 * SetFlatPanelSpeakerMute:		
 * ****************************************************************** */
    Boolean 
SetFlatPanelSpeakerMute(int presenterGraphicsFD, Boolean value) 
{
struct I2C_CMD_ARG commands;
int		    argument;

if (value) 
    argument = 0xee; // mute 
else 
    argument = 0xce; // unmute 

commands.cmd = I2C_AUDIO_SWITCH;
commands.args = &argument;
if (ioctl(presenterGraphicsFD, PANEL_I2C_CMD, &commands) == -1) 
    {
//    fprintf(stderr, "%s: Newport i2c command failed\n", ApplicationClassName);
    return (False);
    }
if (commands.return_val == 0) 
    {
//    fprintf(stderr, "%s: Unable to execute I2C command\n", ApplicationClassName);
    return (False);
    }

return (True);  
}   /* ---- end SetFlatPanelSpeakerMute() ---- */

/* ******************************************************************
 * GetFlatPanelSpeakerMute:		
 * ****************************************************************** */
    int 
GetFlatPanelSpeakerMute(int presenterGraphicsFD) 
{
struct I2C_CMD_ARG commands;

commands.cmd = I2C_MUTE_VAL;
if (ioctl(presenterGraphicsFD, PANEL_I2C_CMD, &commands) == -1) 
    {
//    fprintf(stderr, "%s: Newport i2c command failed\n", ApplicationClassName);
    }

return (commands.return_val);
}   /* ---- end GetFlatPanelSpeakerMute() ---- */

extern "C" {
#include <sys/procfs.h>
}
/* ******************************************************************
 * ProcessIDToName:	Translate process ID to name	
 * ****************************************************************** */
    static char *
ProcessIDToName(int pid)
{
char buf[200];
int fd;
struct prpsinfo psinfo;

sprintf(buf, "/proc/pinfo/%05d", pid);
fd = open(buf, O_RDONLY);
if (fd < 0) 
    return NULL;

if (ioctl(fd, PIOCPSINFO, &psinfo) == -1) 
    return NULL;

return (strdup(psinfo.pr_fname));
}   /* ---- end ProcessIDToName() ---- */

/* ******************************************************************
 * PrintAudioHardwareState:  print to shell
 * ****************************************************************** */
    void 
PrintAudioHardwareState()
{
long	b[50];
char	s[100], s1[100];

fprintf(stderr, "\nAudio Hardware State\n");

/* disable AL error handler */
ALseterrorhandler(0);

/* fill with parameter name codes */
b[0]  = AL_LEFT_SPEAKER_GAIN;
b[2]  = AL_RIGHT_SPEAKER_GAIN;
b[4]  = AL_LEFT_INPUT_ATTEN;
b[6]  = AL_RIGHT_INPUT_ATTEN;
b[8]  = AL_INPUT_RATE;
b[10] = AL_OUTPUT_RATE;
b[12] = AL_INPUT_SOURCE;
b[14] = AL_INPUT_COUNT;
b[16] = AL_MONITOR_CTL;
b[18] = AL_SPEAKER_MUTE_CTL;
b[20] = AL_LEFT2_INPUT_ATTEN;
b[22] = AL_RIGHT2_INPUT_ATTEN;
b[24] = AL_MIC_MODE;
b[26] = AL_CHANNEL_MODE;
b[28] = AL_DIGITAL_INPUT_RATE;
b[30] = AL_LEFT_MONITOR_ATTEN;
b[32] = AL_RIGHT_MONITOR_ATTEN;
b[34] = AL_OUTPUT_COUNT;
b[36] = AL_UNUSED_COUNT;
ALgetparams(AL_DEFAULT_DEVICE, b, 38);

/* print input attenuation and speaker gain */
fprintf(stderr, "Speaker Gain        %3d,%3d (L,R)\n",   b[1], b[3]);
fprintf(stderr, "Input Attenuation   %3d,%3d (L,R)\n",   b[5], b[7]);
fprintf(stderr, "Input Attenuation   %3d,%3d (L2,R2)\n", b[21], b[23]);


/* print monitor function attenuation (if available) */
if (ALgetdefault(AL_DEFAULT_DEVICE, AL_MIC_MODE) >= 0)
    fprintf(stderr, "Monitor Attenuation %3d,%3d (L,R)\n", 
	    b[31], b[33]);
else
    fprintf(stderr, "Monitor Attenuation Not supported on this machine\n");

/* print input and output sampling rates */
switch (b[9])
    {
    case AL_RATE_INPUTRATE:
	strcpy(s, "AL_RATE_INPUTRATE");
    break;
    case AL_RATE_AES_1:
	strcpy(s, "AL_RATE_AES_1");
    break;
    case AL_RATE_AES_2:
	strcpy(s, "AL_RATE_AES_2");
    break;
    case AL_RATE_AES_3:
	strcpy(s, "AL_RATE_AES_3");
    break;
    case AL_RATE_AES_4:
	strcpy(s, "AL_RATE_AES_4");
    break;
    case AL_RATE_AES_6:
	strcpy(s, "AL_RATE_AES_6");
    break;
    case AL_RATE_AES_1s:
	strcpy(s, "AL_RATE_AES_1s");
    break;
    default:
	sprintf(s, "AL_RATE_%d", b[9]);
    break;
    }
switch (b[11])
    {
    case AL_RATE_INPUTRATE:
	strcpy(s1, "AL_RATE_INPUTRATE");
    break;
    case AL_RATE_AES_1:
	strcpy(s1, "AL_RATE_AES_1");
    break;
    case AL_RATE_AES_2:
	strcpy(s1, "AL_RATE_AES_2");
    break;
    case AL_RATE_AES_3:
	strcpy(s1, "AL_RATE_AES_3");
    break;
    case AL_RATE_AES_4:
	strcpy(s1, "AL_RATE_AES_4");
    break;
    case AL_RATE_AES_6:
	strcpy(s1, "AL_RATE_AES_6");
    break;
    case AL_RATE_AES_1s:
	strcpy(s1, "AL_RATE_AES_1s");
    break;
    default:
	sprintf(s1, "AL_RATE_%d", b[11]);
    break;
    }
fprintf(stderr, "Sampling Rate       %s,%s (I,O)\n", s, s1);

/* print digital input sampling rate (if avilable) */
if (ALgetdefault(AL_DEFAULT_DEVICE, AL_DIGITAL_INPUT_RATE) >= 0)
    {
    if (b[29] != AL_RATE_UNDEFINED)
	fprintf(stderr, "Digital Input Rate  %d\n", b[29]);
    else
	fprintf(stderr, "Digital Input Rate  AL_RATE_UNDEFINED\n");
    }
else
    fprintf(stderr, "Digital Input Rate  Not supported on this machine\n");

/* print input source */
switch (b[13])
    {
    case AL_INPUT_LINE:
	strcpy(s, "AL_INPUT_LINE");
    break;
    case AL_INPUT_MIC:
	strcpy(s, "AL_INPUT_MIC");
    break;
    case AL_INPUT_DIGITAL:
	strcpy(s, "AL_INPUT_DIGITAL");
    break;
    default:
	sprintf(s, "Bogus value=%d", b[13]);
    break;
    }
fprintf(stderr, "Input Source        %s\n", s);

/* print monitor state */
if	(b[17] == AL_MONITOR_OFF)
    fprintf(stderr, "Monitor             AL_MONITOR_OFF\n");
else if (b[17] == AL_MONITOR_ON)
    fprintf(stderr, "Monitor             AL_MONITOR_ON\n");
else
    fprintf(stderr, "Monitor             Bogus value=%d\n", b[17]);


/* print speaker mute state */
if	(b[19] == AL_SPEAKER_MUTE_OFF)
    fprintf(stderr, "Mute                AL_SPEAKER_MUTE_OFF\n");
else if (b[19] == AL_SPEAKER_MUTE_ON)
    fprintf(stderr, "Mute                AL_SPEAKER_MUTE_ON\n");
else
    fprintf(stderr, "Mute                Bogus value=%d\n", b[19]);


/* print microphone mode (if available) */
if (ALgetdefault(AL_DEFAULT_DEVICE, AL_MIC_MODE) >= 0)
    {
    switch (b[25])
	{
	case AL_MONO:
	    strcpy(s, "AL_MONO");
	break;
	case AL_STEREO:
	    strcpy(s, "AL_STEREO");
	break;
	case AL_4CHANNEL:
	    strcpy(s, "AL_4CHANNEL");
	break;
	default:
	    if (ALgetdefault(AL_DEFAULT_DEVICE, AL_CHANNEL_MODE) >= 0)
		sprintf(s, "Bogus value=%d", b[25]);
	    else
		sprintf(s, "Not supported on this machine");
	break;
	}
    fprintf(stderr, "Microphone Mode     %s\n", s);
    }
else
    fprintf(stderr, "Microphone Mode     Not supported on this machine\n");

/* print channel mode */
switch (b[27])
    {
    case AL_MONO:
	strcpy(s, "AL_MONO");
    break;
    case AL_STEREO:
	strcpy(s, "AL_STEREO");
    break;
    case AL_4CHANNEL:
	strcpy(s, "AL_4CHANNEL");
    break;
    default:
    if (ALgetdefault(AL_DEFAULT_DEVICE, AL_CHANNEL_MODE) >= 0)
	sprintf(s, "Bogus value=%d", b[27]);
    else
	sprintf(s, "Not supported on this machine");
    break;
    }
fprintf(stderr, "Channel Mode        %s\n", s);


/*
 * print info about audio hardware usage
 */
/* audio hardware idle:  monitor || AL_INPUT_COUNT || AL_OUTPUT_COUNT */
if	(! (b[17] == AL_MONITOR_ON || b[15] || b[35]))
    fprintf(stderr, "Audio Hardware not in use.  Ports available: %d\n", b[37]);

/* audio hardware in use */
else 
    {
    char s[500], s1[100];
    strcpy(s, "Audio Hardware in Use: ");

/* monitoring */
    if (b[17] == AL_MONITOR_ON)
	strcat(s, "Monitoring");

/* audio input ports in use */
    if (b[15])
	{
	if (b[17] == AL_MONITOR_ON)
	    sprintf(s1, ", %d input port(s)", b[15]);
	else
	    sprintf(s1, "%d input port(s)", b[15]);
	strcat(s, s1);
	}

/* audio output ports in use */
    if (b[35])
	{
	if (b[17] == AL_MONITOR_ON || b[15])
	    sprintf(s1, ", %d output port(s)", b[35]);
	else
	    sprintf(s1, "%d output port(s)", b[35]);
	strcat(s, s1);
	}

/* print composite */
    fprintf(stderr, "%s\n", s);

/* print unused ports */
    fprintf(stderr, "Ports available: %d\n", b[37]);

/* following works only on Indigo2 and Indy and Onyx w/ASO board */
    if (b[15] || b[35])
	{
	int fd = open("/dev/hdsp/hdsp0master", O_RDONLY);
	if (fd >= 0) 
	    {
#include "sys/hdsp.h"
	    char rbuf[2048];
	    int portCount = ioctl(fd, HDSP_GET_ALL_RB_INFO, rbuf);
	    if (portCount >= 0) 
		{
		int i;
		hdsp_getrbinfo_t *rbt;

	    /* print input port list input ports in use */
		if (b[15])
		    {
		    fprintf(stderr, "Input Ports:\n");
		    rbt = (hdsp_getrbinfo_t *) rbuf;
		    for (i = 0; i < portCount; i++, rbt++)
			{
			if (rbt->type == 0)
			    fprintf(stderr, "    %s: pid=%d, port name='%s'\n", 
				    ProcessIDToName(rbt->pid), rbt->pid, rbt->name);
			}
		    }

	    /* print output port list input ports in use */
		if (b[35])
		    {
		    fprintf(stderr, "Output Ports:\n");
		    rbt = (hdsp_getrbinfo_t *) rbuf;
		    for (i = 0; i < portCount; i++, rbt++)
			{
			if (rbt->type == 1)
			    fprintf(stderr, "    %s: pid=%d, port name='%s'\n", 
				    ProcessIDToName(rbt->pid), rbt->pid, rbt->name);
			}
		    }
		}
	    }
	}
    }
}   /* ---- end PrintAudioHardwareState() ---- */

/* ******************************************************************
 * DataStructureToAudioHardware:	blast data structure contents
 *					to Audio Hardware	
 * ****************************************************************** */
    void
DataStructureToAudioHardware(SomeData	*d,
				char	*inputChannelCapacity, 
				char	*outputChannelCapacity,
				char	*overallChannelCapacity, 
				Boolean *haveStereoMicrophoneAbility,	    
				int	*presenterGraphicsFD,
				Boolean *canChangeChannelMode,
				char    **applicationUsage,
				char	*IP22ApplicationUsage,
				char	*IP12IP20ApplicationUsage)
{
char s[100];

// disable AL error handler 
ALseterrorhandler(0);

// Add Options menu items for four channel mode and stereo microphone,
// if those abilities are present in Audio Hardware 
long minimum, maximum;
*haveStereoMicrophoneAbility = True;
if (ALgetminmax(AL_DEFAULT_DEVICE, AL_MIC_MODE, &minimum, &maximum) != 0)
    *haveStereoMicrophoneAbility = False;
else if (maximum < AL_STEREO || minimum > AL_STEREO)
    *haveStereoMicrophoneAbility = False;

*inputChannelCapacity   = 4;
*outputChannelCapacity  = 4;
*overallChannelCapacity = 4;
if (ALgetminmax(AL_DEFAULT_DEVICE, AL_CHANNEL_MODE, &minimum, &maximum) != 0)
    {
    *inputChannelCapacity   = 2;
    *outputChannelCapacity  = 2;
    *overallChannelCapacity = 2;
    }
if (maximum < AL_4CHANNEL)
    {
    *inputChannelCapacity   = 2;
    *outputChannelCapacity  = 2;
    *overallChannelCapacity = 2;
    }
if (*overallChannelCapacity == 4)
    {
    *canChangeChannelMode = True;
    *applicationUsage     = IP22ApplicationUsage; 
    }
else
    {
    *canChangeChannelMode = False;
    *applicationUsage     = IP12IP20ApplicationUsage; 
    }

// read audio hardware state parameters from file 
// DISPLAY !!!!
if (d->openFileName)
    {
    int code = LoadAudioHardwareState(d->openFileName);
    if (code != 0)
	{
	char s[1000];  
	switch (code)
	    {
#ifdef OLDE
	    {
	    case -1:
		sprintf(s, "%s", VkGetResource("NoFileName", ApplicationClassName));
	    break;
	    case -2:
	    default:
		sprintf(s, "%s:\n'%s'\n%s.",
			VkGetResource("UnableToOpenFileForReading", ApplicationClassName),
			d->openFileName, strerror(errno));
	    break;
	    case -3:
		sprintf(s, "%s:\n'%s'\n",
			VkGetResource("BadParameterInFile", ApplicationClassName),
			d->openFileName);
	    break;
	    case -4:
		sprintf(s, "'%s'\n%s", 
			d->openFileName,
			VkGetResource("NotAFile", ApplicationClassName));
	    break;
	    }
	if (d->useDisplay)
	    {
	    theWarningDialog->setTitle(windowName);
	    theWarningDialog->post(s, NULL, window);
	    }
	else
	    fprintf(stderr, "%s", s);
#endif
	    case -1:
		strcpy(s, "No file name!\n");
	    break;
	    case -2:
	    default:
		sprintf(s, "Unable to open file for reading:\n'%s'\n%s.",
			d->openFileName, strerror(errno));
	    break;
	    case -3:
		sprintf(s, "Bad parameter in file:\n'%s'\n", d->openFileName);
	    break;
	    case -4:
		sprintf(s, "'%s'\nNot a file!", d->openFileName);
	    break;
	    }
	fprintf(stderr, "%s", s);
	}
    }

// if poll interval <= 0, exit program 
if (d->audioHardwarePollInterval <= 0.0) 
    {
    sprintf(s, "Invalid audioHardwarePollInterval: %g", d->audioHardwarePollInterval);
    Punt(s);
    }

// gather parameters to send to audio hardware 
// resource mechanism allows values of -1 to be ignored by this code 
long	pv[50];
int	abacus = 0;

// initialize 2/4 channel mode  
if (d->initChannels > 0) 
    {
    if	    (d->initChannels == 2 && d->initChannels <= *overallChannelCapacity)
	{
	pv[abacus++] = AL_CHANNEL_MODE;
	pv[abacus++] = AL_STEREO;
	} 
    else if (d->initChannels == 4 && d->initChannels <= *overallChannelCapacity)
	{
	pv[abacus++] = AL_CHANNEL_MODE;
	pv[abacus++] = AL_4CHANNEL;
	} 
// Invalid channel #: exit program 
    else 
	{
	if	(*overallChannelCapacity == 2)
	    sprintf(s, "Invalid channel count: %d. Choose from {2}.", d->initChannels);
	else if (*overallChannelCapacity == 4)
	    sprintf(s, "Invalid channel count: %d. Choose from {2, 4}.", d->initChannels);
	else 
	    sprintf(s, "Invalid channel count: %d.", d->initChannels);
	Punt(s);
	}
    }
// -1 = not specified
else if (d->initChannels != -1) 
	{
	if	(*overallChannelCapacity == 2)
		{
		Print("Invalid channel count: %d. Choose from {2}",    d->initChannels);
		exit(1);
		}
	else if	(*overallChannelCapacity == 4)
		{
		Print("Invalid channel count: %d. Choose from {2, 4}", d->initChannels);
		exit(1);
		}
	}

// initialize input and output sampling rates
if	(d->initSamplingRate != 0)
	{
	if (d->initSamplingRate >= AL_RATE_AES_1s)
	    {
	    d->initInputSamplingRate  = d->initSamplingRate;
	    d->initOutputSamplingRate = d->initSamplingRate;
	    }
	else
	    {
	    Print("Invalid sampling rate: %d.", d->initSamplingRate);
	    exit(1);
	    }
	}

if	(d->initInputSamplingRate != 0) 
	{
	if (d->initInputSamplingRate >= AL_RATE_AES_1s &&
	    d->initInputSamplingRate != AL_RATE_INPUTRATE)
	    {
	    pv[abacus++] = AL_INPUT_RATE;
	    pv[abacus++] = d->initInputSamplingRate;
	    }
	else
	    {
	    Print("Invalid input sampling rate: %d.", d->initInputSamplingRate);
	    exit(1);
	    }
	}

if	(d->initOutputSamplingRate != 0) 
	{
	if (d->initOutputSamplingRate >= AL_RATE_AES_1s)
	    {
	    pv[abacus++] = AL_OUTPUT_RATE;
	    pv[abacus++] = d->initOutputSamplingRate;
	    }
	else
	    {
	    Print("Invalid output sampling rate: %d.", d->initOutputSamplingRate);
	    exit(1);
	    }
	}

// initialize input source 
if (strcasecmp(d->initInputSource, "False")) 
    {
    if	    (!strcasecmp(d->initInputSource, "microphone") ||
	     !strcasecmp(d->initInputSource, "mic"))
	    {
	    pv[abacus++] = AL_INPUT_SOURCE;
	    pv[abacus++] = AL_INPUT_MIC;
	    } 
    else if (!strcasecmp(d->initInputSource, "line"))
	    {
	    pv[abacus++] = AL_INPUT_SOURCE;
	    pv[abacus++] = AL_INPUT_LINE;
	    } 
    else if ((!strcasecmp(d->initInputSource, "digital"))||
		(!strcasecmp(d->initInputSource, "aes"))) 
	    {
	    pv[abacus++] = AL_INPUT_SOURCE;
	    pv[abacus++] = AL_INPUT_DIGITAL;
	    } 
    else 
	{
	sprintf(s, "Invalid input source: '%s'. Choose {microphone, line, digital}.", 
		d->initInputSource);
    // Invalid source given: exit application 
	Punt(s);
	}
    }

// initialize microphone mode  
if (strcasecmp(d->initMicrophoneMode, "False")) 
    {
    if (*haveStereoMicrophoneAbility == True)
	{
	if	(!strncasecmp(d->initMicrophoneMode, "mono", sizeof("mono")-1)) 
		{
		pv[abacus++] = AL_MIC_MODE;
		pv[abacus++] = AL_MONO;
		} 
	else if (!strncasecmp(d->initMicrophoneMode, "stereo", sizeof("stereo")-1)) 
		{
		pv[abacus++] = AL_MIC_MODE;
		pv[abacus++] = AL_STEREO;
		} 
	else 
	    {
	    sprintf(s, "Invalid microphone mode: '%s'. Choose {mono, stereo}.", d->initMicrophoneMode);
	// Invalid mode given: exit application 
	    Punt(s);
	    }
	}
    else
        {
	fprintf(stderr, "Usage: %s\n%s\n", ApplicationClassName, *applicationUsage);
	exit(-1);
	}
    }

// initialize input levels 
if	(d->initInputLevels >= 0 && d->initInputLevels <= 255)
	{
	pv[abacus++] = AL_LEFT_INPUT_ATTEN;
	pv[abacus++] = 255 - d->initInputLevels;
	pv[abacus++] = AL_RIGHT_INPUT_ATTEN;
	pv[abacus++] = 255 - d->initInputLevels;
	pv[abacus++] = AL_LEFT2_INPUT_ATTEN;
	pv[abacus++] = 255 - d->initInputLevels;
	pv[abacus++] = AL_RIGHT2_INPUT_ATTEN;
	pv[abacus++] = 255 - d->initInputLevels;
	}
// -1 = not specified
else if (d->initInputLevels != -1) 
	{
	Print("Invalid input levels %d: range is [0..255]", d->initInputLevels);
	exit(1);
	}

if	(d->initInputLevelLeft1 >= 0 && d->initInputLevelLeft1 <= 255) 
	{
	pv[abacus++] = AL_LEFT_INPUT_ATTEN;
	pv[abacus++] = 255 - d->initInputLevelLeft1;
	}
// -1 = not specified
else if (d->initInputLevelLeft1 != -1) 
	{
	Print("Invalid input left1 level %d: range is [0..255]", d->initInputLevelLeft1);
	exit(1);
	}
if	(d->initInputLevelLeft2 >= 0 && d->initInputLevelLeft2 <= 255)
	{
	pv[abacus++] = AL_LEFT2_INPUT_ATTEN;
	pv[abacus++] = 255 - d->initInputLevelLeft2;
	}
// -1 = not specified
else if (d->initInputLevelLeft2 != -1) 
	{
	Print("Invalid input left2 level %d: range is [0..255]", d->initInputLevelLeft2);
	exit(1);
	}

if	(d->initInputLevelRight1 >= 0 && d->initInputLevelRight1 <= 255) 
	{
	pv[abacus++] = AL_RIGHT_INPUT_ATTEN;
	pv[abacus++] = 255 - d->initInputLevelRight1;
	}
// -1 = not specified
else if (d->initInputLevelRight1 != -1) 
	{
	Print("Invalid input left1 level %d: range is [0..255]", d->initInputLevelRight1);
	exit(1);
	}
if	(d->initInputLevelRight2 >= 0 && d->initInputLevelRight2 <= 255)
	{
	pv[abacus++] = AL_RIGHT2_INPUT_ATTEN;
	pv[abacus++] = 255 - d->initInputLevelRight2;
	}
// -1 = not specified
else if (d->initInputLevelRight2 != -1) 
	{
	Print("Invalid input left2 level %d: range is [0..255]", d->initInputLevelRight2);
	exit(1);
	}

// initialize output levels 
if	(d->initOutputLevels >= 0 && d->initOutputLevels <= 255)
	{
	pv[abacus++] = AL_LEFT_SPEAKER_GAIN;
	pv[abacus++] = d->initOutputLevels;
	pv[abacus++] = AL_RIGHT_SPEAKER_GAIN;
	pv[abacus++] = d->initOutputLevels;
	}
// -1 = not specified
else if (d->initOutputLevels != -1) 
	{
	Print("Invalid output levels %d: range is [0..255]", d->initOutputLevels);
	exit(1);
	}

if	(d->initOutputLevelLeft >= 0 && d->initOutputLevelLeft <= 255)
	{
	pv[abacus++] = AL_LEFT_SPEAKER_GAIN;
	pv[abacus++] = d->initOutputLevelLeft;
	}
// -1 = not specified
else if (d->initOutputLevelLeft != -1) 
	{
	Print("Invalid output left level %d: range is [0..255]", d->initOutputLevelLeft);
	exit(1);
	}
if	(d->initOutputLevelRight >= 0 && d->initOutputLevelRight <= 255) 
	{
	pv[abacus++] = AL_RIGHT_SPEAKER_GAIN;
	pv[abacus++] = d->initOutputLevelRight;
	}
// -1 = not specified
else if (d->initOutputLevelRight != -1) 
	{
	Print("Invalid output right level %d: range is [0..255]", d->initOutputLevelRight);
	exit(1);
	}

// enable/disable hardware monitor  
// (True = turn on, False = turn off, -1 = do nothing) 
if	(d->initMonitor == True || d->initMonitorOn == True)
	{
	pv[abacus++] = AL_MONITOR_CTL;
	pv[abacus++] = AL_MONITOR_ON;
	}
else if (d->initMonitorOff == True) 
	{
	pv[abacus++] = AL_MONITOR_CTL;
	pv[abacus++] = AL_MONITOR_OFF;
	}

// enable/disable hardware mute  
// this code MUST be executed AFTER the speaker adjustments have been
// made.  Otherwise, the speaker adjustments will unmute the system.
// (True = turn on, False = turn off, -1 = do nothing) 
if	(d->initMute == True) 
	{
	pv[abacus++] = AL_SPEAKER_MUTE_CTL;
	pv[abacus++] = AL_SPEAKER_MUTE_ON;
	}
else if (d->initUnmute == True) 
	{
	pv[abacus++] = AL_SPEAKER_MUTE_CTL;
	pv[abacus++] = AL_SPEAKER_MUTE_OFF;
	}

// set hardware parameters 
if (abacus > 0) 
    ALsetparams(AL_DEFAULT_DEVICE, pv, abacus);

// get current channel mode 
pv[0] = AL_CHANNEL_MODE;
ALgetparams(AL_DEFAULT_DEVICE, pv, 2);
if	(pv[1] == AL_STEREO)
    {
    *inputChannelCapacity = 2;
    *outputChannelCapacity = 2;
    }
else if (pv[1] == AL_4CHANNEL)
    {
    *inputChannelCapacity = 4;
    *outputChannelCapacity = 4;
    }

// Presenter Flat Panel options 
// initialize Presenter flat panel levels 
if	(d->initPresenterLevels >= 0 && d->initInputLevels <= 26) 
	{
	for (int i = 0; i < 2; i++)
	    SetFlatPanelSpeakerLevel(*presenterGraphicsFD, i, d->initPresenterLevels); 
	}
// -1 = not specified
else if (d->initPresenterLevels != -1) 
	{
	Print("Invalid Presenter levels: %d, range is [0..26]", d->initPresenterLevels);
	exit(1);
	}

// initialize Presenter flat panel tone  
if	(d->initPresenterTone >= 0 && d->initPresenterTone <= 9)
	SetFlatPanelSpeakerTone(*presenterGraphicsFD, d->initPresenterTone); 

// -1 = not specified
else if (d->initPresenterTone != -1) 
	{
	Print("Invalid Presenter tone: %d, range is [0..9]", d->initPresenterTone);
	exit(1);
	}
}   /* ---- end DataStructureToAudioHardware() ---- */

/* ******************************************************************
 *  LoadAudioHardwareState:  read parameter file
 *			    and set Audio Hardware
 *			    return   0: success, 
 *				    -1: no filename provided
 *				    -2: can't open file
 *				    -3: file contains bad parameter value
 *				    -4: not a file
 * ****************************************************************** */
    int 
LoadAudioHardwareState(char *fileName)
{
FILE	*fd;
long	pv[100];
char	format[100];
char	item[200], string[200];
long	val = 0;
char	c;

#define SKIP_LINE while (((c=fgetc(fd)) != (char)EOF) && (c != '\n'));

#define BAD_FILE_PARAMETER {\
Print("invalid parameter: '%s'\n", item);\
fclose(fd);\
return(-3);\
}
#define BAD_FILE_PARAMETER_STRING_VALUE {\
Print("Invalid '%s'='%s'\n", item, string);\
fclose(fd);\
return(-3);\
}
#define BAD_FILE_PARAMETER_INTEGER_VALUE {\
Print("Invalid '%s'=%d\n", item, val);\
fclose(fd);\
return(-3);\
}

/* try to open file */
if (!fileName) 
    return (-1);

// if path is not file or unable to open
struct stat info;
stat(fileName, &info);
if (!S_ISREG(info.st_mode))
    return (-4);

if (!(fd = fopen(fileName, "r")))
    return (-2);

/* parse file */
/* Can not change application class name because of the following line */
/* Many users are likely to already have files */
/* NOTE space character at beginning of format string.  Don't even think about removing it. */
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
	if	(!strcmp(item, "initMute")) 
		{
		pv[i++] = AL_SPEAKER_MUTE_CTL;
		pv[i++] = AL_SPEAKER_MUTE_ON;
		} 
	else if (!strcmp(item, "initUnmute")) 
		{
		pv[i++] = AL_SPEAKER_MUTE_CTL;
		pv[i++] = AL_SPEAKER_MUTE_OFF;
		} 
    
	else if (!strcmp(item, "initMonitor") || !strcmp(item, "initMonitorOn")) 
		{
		pv[i++] = AL_MONITOR_CTL;
		pv[i++] = AL_MONITOR_ON;
		} 
	else if (!strcmp(item, "initMonitorOff")) 
		{
		pv[i++] = AL_MONITOR_CTL;
		pv[i++] = AL_MONITOR_OFF;
		} 
    
	else if (!strcmp(item, "initMicrophoneMode:") || !strcmp(item, "initMicMode:"))
	    {
	    if (fscanf(fd, " %s", string) == 1) 
		{
		if	(!strcasecmp(string, "mono")) 
		    {
		    pv[i++] = AL_MIC_MODE;
		    pv[i++] = AL_MONO;
		    } 
		else if (!strcasecmp(string, "stereo")) 
		    {
		    pv[i++] = AL_MIC_MODE;
		    pv[i++] = AL_STEREO;
		    }
		else
		    BAD_FILE_PARAMETER_STRING_VALUE
		} 
	    else 
		BAD_FILE_PARAMETER
	    } 
    
	else if (!strcmp(item, "initChannels:")) 
	    {
	    if (fscanf(fd, " %d", &val) == 1)
		{
		switch (val) 
		    {
		    case 2:
			pv[i++] = AL_CHANNEL_MODE;
			pv[i++] = AL_STEREO;
		    break;
		    case 4:
			pv[i++] = AL_CHANNEL_MODE;
			pv[i++] = AL_4CHANNEL;
		    break;
		    default: 
			BAD_FILE_PARAMETER_INTEGER_VALUE
		    }
		} 
	    else 
		BAD_FILE_PARAMETER
	    } 
    
	else if (!strcmp(item, "initInRate:")) 
	    {
	    if (fscanf(fd, " %d", &val) == 1)  
		{
		pv[i++] = AL_INPUT_RATE;
		pv[i++] = val;
		} 
	    else 
		BAD_FILE_PARAMETER_INTEGER_VALUE
	    } 
	else if (!strcmp(item, "initOutRate:")) 
	    {
	    if (fscanf(fd, " %d", &val) == 1)  
		{
		pv[i++] = AL_OUTPUT_RATE;
		pv[i++] = val;
		} 
	    else 
		BAD_FILE_PARAMETER_INTEGER_VALUE
	    } 
	else if (!strcmp(item, "initRate:")) 
	    {
	    if (fscanf(fd, " %d", &val) == 1)  
		{
		pv[i++] = AL_INPUT_RATE;
		pv[i++] = val;
		pv[i++] = AL_OUTPUT_RATE;
		pv[i++] = val;
		} 
	    else 
		BAD_FILE_PARAMETER_INTEGER_VALUE
	    } 
    
	else if (!strcmp(item, "initSource:")) 
	    {
	    if (fscanf(fd, " %s", string) == 1) 
		{
		if ((!strcasecmp(string, "mic")) || 
			(!strcasecmp(string, "microphone")))
		    {
		    pv[i++] = AL_INPUT_SOURCE;
		    pv[i++] = AL_INPUT_MIC;
		    } 
		else if (!strcasecmp(string, "line")) 
		    {
		    pv[i++] = AL_INPUT_SOURCE;
		    pv[i++] = AL_INPUT_LINE;
		    } 
		else if (!strcasecmp(string, "digital")) 
		    {
		    pv[i++] = AL_INPUT_SOURCE;
		    pv[i++] = AL_INPUT_DIGITAL;
		    } 
		else 
		    BAD_FILE_PARAMETER_STRING_VALUE
		} 
	    else 
		BAD_FILE_PARAMETER
	    } 
    
	else if (!strcmp(item, "initInLevels:")) 
	    {
	    if (fscanf(fd, " %d", &val) == 1 && val >= 0 && val <= 255) 
		{
		pv[i++] = AL_LEFT_INPUT_ATTEN;
		pv[i++] = 255 - val;
		pv[i++] = AL_RIGHT_INPUT_ATTEN;
		pv[i++] = 255 - val;
		pv[i++] = AL_LEFT2_INPUT_ATTEN;
		pv[i++] = 255 - val;
		pv[i++] = AL_RIGHT2_INPUT_ATTEN;
		pv[i++] = 255 - val;
		} 
	    else 
		BAD_FILE_PARAMETER_INTEGER_VALUE
	    } 
	else if (!strcmp(item, "initInLevelLeft1:")) 
	    {
	    if (fscanf(fd, " %d", &val) == 1 && val >= 0 && val <= 255) 
		{
		pv[i++] = AL_LEFT_INPUT_ATTEN;
		pv[i++] = 255 - val;
		} 
	    else
		BAD_FILE_PARAMETER_INTEGER_VALUE
	    } 
	else if (!strcmp(item, "initInLevelRight1:")) 
	    {
	    if (fscanf(fd, " %d", &val) == 1 && val >= 0 && val <= 255) 
		{
		pv[i++] = AL_RIGHT_INPUT_ATTEN;
		pv[i++] = 255 - val;
		} 
	    else 
		BAD_FILE_PARAMETER_INTEGER_VALUE
	    } 
	else if (!strcmp(item, "initInLevelLeft2:")) 
		{
		if (fscanf(fd, " %d", &val) == 1 && val >= 0 && val <= 255) 
		    {
		    pv[i++] = AL_LEFT2_INPUT_ATTEN;
		    pv[i++] = 255 - val;
		    } 
		else 
		    BAD_FILE_PARAMETER_INTEGER_VALUE
		} 
	else if (!strcmp(item, "initInLevelRight2:")) 
		{
		if (fscanf(fd, " %d", &val) == 1 && val >= 0 && val <= 255) 
		    {
		    pv[i++] = AL_RIGHT2_INPUT_ATTEN;
		    pv[i++] = 255 - val;
		    } 
		else 
		    BAD_FILE_PARAMETER_INTEGER_VALUE
		} 
    
	else if (!strcmp(item, "initOutLevels:")) 
	    {
	    if (fscanf(fd, " %d", &val) == 1 && val >= 0 && val <= 255) 
		{
		pv[i++] = AL_LEFT_SPEAKER_GAIN;
		pv[i++] = val;
		pv[i++] = AL_RIGHT_SPEAKER_GAIN;
		pv[i++] = val;
		} 
	    else 
		BAD_FILE_PARAMETER_INTEGER_VALUE
	    } 
	else if (!strcmp(item, "initOutLevelLeft:")) 
	    {
	    if (fscanf(fd, " %d", &val) == 1 && val >= 0 && val <= 255) 
		{
		pv[i++] = AL_LEFT_SPEAKER_GAIN;
		pv[i++] = val;
		} 
	    else 
		BAD_FILE_PARAMETER_INTEGER_VALUE
	    } 
	else if (!strcmp(item, "initOutLevelRight:")) 
	    {
	    if (fscanf(fd, " %d", &val) == 1 && val >= 0 && val <= 255) 
		{
		pv[i++] = AL_RIGHT_SPEAKER_GAIN;
		pv[i++] = val;
		} 
	    else 
		BAD_FILE_PARAMETER_INTEGER_VALUE
	    }
    	}

    SKIP_LINE;

    if (i >= 60) 
	BAD_FILE_PARAMETER
}

fclose(fd);

/* for parameters found and even parameter count (indicating paired
    parameter name and values) */
if (i != 0 && ((i&0x1) == 0))
    ALsetparams(AL_DEFAULT_DEVICE, pv, i);

return (0);
#undef SKIP_LINE
#undef BAD_FILE_PARAMETER
} /* ---- end LoadAudioHardwareState() ---- */

/* ******************************************************************
 * WriteAudioHardwareState:  acquire current 
 *			    Audio Hardware state and save as parameter file
 *			    return  0: success
 *				   -1: no filename
 *				   -2: can't open file
 * ****************************************************************** */
    void 
WriteAudioHardwareState(FILE *fd, 
			char inputChannelCapacity, 
			char overallChannelCapacity,
			char haveStereoMicrophoneAbility)
{
// acquire current Audio Hardware state 
long pv[30];
pv[0]  = AL_INPUT_SOURCE;
pv[2]  = AL_INPUT_RATE;
pv[4]  = AL_OUTPUT_RATE;
pv[6]  = AL_MONITOR_CTL;
pv[8]  = AL_SPEAKER_MUTE_CTL;
pv[10] = AL_CHANNEL_MODE;
pv[12] = AL_MIC_MODE;
pv[14] = AL_LEFT_INPUT_ATTEN;
pv[16] = AL_RIGHT_INPUT_ATTEN;
pv[18] = AL_LEFT_SPEAKER_GAIN;
pv[20] = AL_RIGHT_SPEAKER_GAIN;
pv[22] = AL_LEFT2_INPUT_ATTEN;
pv[24] = AL_RIGHT2_INPUT_ATTEN;
ALgetparams(AL_DEFAULT_DEVICE, pv, 26);

// 
// write Audio Hardware parameters
//
fprintf(fd,"! Audio Hardware State:\n");

fprintf(fd, "%s*initSource:\t\t", ApplicationClassName);
switch (pv[1]) 
    {
    case AL_INPUT_LINE: 
	fprintf(fd, "line\n");
    break;
    case AL_INPUT_DIGITAL: 
	fprintf(fd, "digital\n");
    break;
    case AL_INPUT_MIC: 
	fprintf(fd, "microphone\n");
    break;
    }
fprintf(fd, "%s*initInRate:\t\t%d\n",  ApplicationClassName, pv[3]);
fprintf(fd, "%s*initOutRate:\t\t%d\n", ApplicationClassName, pv[5]);

fprintf(fd, "%s*initMonitor%s\n", 
	ApplicationClassName,
	(pv[7] == AL_MONITOR_ON) ? "On" : "Off");

fprintf(fd, "%s*initInLevelLeft1:\t%d\n",  ApplicationClassName, 255 - pv[15]);
fprintf(fd, "%s*initInLevelRight1:\t%d\n", ApplicationClassName, 255 - pv[17]);
if (inputChannelCapacity == 4) 
    {
    fprintf(fd, "%s*initInLevelLeft2:\t%d\n",  ApplicationClassName, 255 - pv[23]);
    fprintf(fd, "%s*initInLevelRight2:\t%d\n", ApplicationClassName, 255 - pv[25]);
    }
fprintf(fd, "%s*initOutLevelLeft:\t%d\n",  ApplicationClassName, pv[19]);
fprintf(fd, "%s*initOutLevelRight:\t%d\n", ApplicationClassName, pv[21]);

fprintf(fd, "! NOTE:  initMute must follow any initOutLevel entries or no muting happens.\n\
! The IRIX Audio driver unmutes the speaker during level adjustments.\n");
//
// NOTE:  the mute entry follow any speaker output level settings.
//	Otherwise, 
fprintf(fd, "%s*init%s\n", 
	ApplicationClassName,
	(pv[9] == AL_SPEAKER_MUTE_ON) ? "Mute" : "Unmute");

if (haveStereoMicrophoneAbility == True)
    fprintf(fd, "%s*initMicrophoneMode:\t%s\n", 
	    ApplicationClassName, 
	    (pv[13] == AL_STEREO) ? "stereo" : "mono");
if (overallChannelCapacity == 4)
    fprintf(fd, "%s*initChannels:\t\t%d\n", 
	    ApplicationClassName, (pv[11] == AL_4CHANNEL) ? 4 : 2);
} /* ---- end WriteAudioHardwareState() ---- */

/* **********************************************************************
 * ExpandPath:   decode ~ and expand path
 * **********************************************************************/
    char *
ExpandPath(char *path)
{
/* if path starts with a tilde, expand */
if (path[0] == '~')
    {   
    int expandedLength;
    int pathLength;
    char *expanded;
    char *tmp;

/* substitute $HOME for ~ */
    if ((path[1] == '/') || (path[1] == '\0'))
	{
    /* HOME may not be defined */
	expanded = getenv("HOME");
	if (!expanded)
	    return (NULL);
	expandedLength = strlen(expanded) + 1;

    /* one less because tilde will be no longer present */
	pathLength = strlen(path)+1;
	tmp = (char *) malloc(pathLength+expandedLength-1);
	strcpy(tmp, expanded);
	strcat(tmp, path+1);
	return(tmp);
	}

/* substitute user's home directory for ~user */
    else
	{
	int i;
	char *userName;
	struct passwd *passWord;

    /* isolate and convert user name */
	userName = strdup(path);
	i = 0;
	while (userName[i] != '\0')
	    {
	    if (userName[i] == '/')
		userName[i] = '\0';
	    else
		i++;
	    }

    /* look up user home directory */
	passWord = getpwnam(userName+1);
	free(userName);
	if (passWord)
	    {
	    expanded = strdup(passWord->pw_dir);
	    free(passWord);
	    }
	else
	    return(NULL);

    /* user may not be defined */
	if (!expanded)
	    return (NULL);
	expandedLength = strlen(expanded) + 1;

    /* one less because tilde will be no longer present */
	pathLength = strlen(path+i)+1;
	tmp = (char *) malloc(pathLength+expandedLength-1);
	
	strcpy(tmp, expanded);
	strcat(tmp, path+i);
	return(tmp);
	}
    }
else
    return (strdup(path));
}   /* ---- end ExpandPath() ---- */

/* **********************************************************************
 * RemoveSpaces:   remove all space ' ' characters
 *		    Return # spaces removed.
 * **********************************************************************/
    int 
RemoveSpaces(char *s)
{
if (!s)
    return (0);

/* scan for last space=' ' character at beginning of string */
for (int i = 0, j = 0; s[j] != '\0'; j++)
    {    
    if (s[j] != ' ')
	s[i++] = s[j];
    }
s[i] = '\0';

return (j-i);
}   /* ---- end RemoveSpaces() ---- */

