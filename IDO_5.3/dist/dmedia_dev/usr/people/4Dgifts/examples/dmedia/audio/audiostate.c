/*******************************************************************
 *	Code to print out Audio Hardware parameters and ports in use
 *
 *			    Written by Gints Klimanis 
 *			Silicon Graphics Computer Systems
 *				 1993
 *******************************************************************/
#include <stdio.h>
#include <dmedia/audio.h>

main()
{
long	buf[60];
char	s[100], s1[100], s2[100];

fprintf(stderr, "---------- Audio Hardware State ----------\n");

/* disable AL error handler */
ALseterrorhandler(0);

/* fill buffer with parameter name codes */
buf[0]  = AL_LEFT_SPEAKER_GAIN;
buf[2]  = AL_RIGHT_SPEAKER_GAIN;
buf[4]  = AL_LEFT_INPUT_ATTEN;
buf[6]  = AL_RIGHT_INPUT_ATTEN;
buf[8]  = AL_INPUT_RATE;
buf[10] = AL_OUTPUT_RATE;
buf[12] = AL_INPUT_SOURCE;
buf[14] = AL_INPUT_COUNT;
buf[16] = AL_MONITOR_CTL;
buf[18] = AL_SPEAKER_MUTE_CTL;
buf[20] = AL_LEFT2_INPUT_ATTEN;
buf[22] = AL_RIGHT2_INPUT_ATTEN;
buf[24] = AL_MIC_MODE;
buf[26] = AL_CHANNEL_MODE;
buf[28] = AL_DIGITAL_INPUT_RATE;
buf[30] = AL_LEFT_MONITOR_ATTEN;
buf[32] = AL_RIGHT_MONITOR_ATTEN;
buf[34] = AL_INPUT_COUNT;
buf[36] = AL_OUTPUT_COUNT;
ALgetparams(AL_DEFAULT_DEVICE, buf, 38);

/* print input attenuation and speaker gain */
fprintf(stderr, "Input Attenuation   L =%3d,R =%3d\n", 
	buf[5], buf[7]);
/* print second set of input levels in four channel mode only */
if (buf[27] == AL_4CHANNEL)
    {
    fprintf(stderr, "Input Attenuation   L2=%3d,R2=%3d\n", 
	    buf[21], buf[23]);
    }
fprintf(stderr, "Speaker Gain        L =%3d,R =%3d\n", 
	buf[1], buf[3]);


/* print monitor function attenuation (if available) */
if (ALgetdefault(AL_DEFAULT_DEVICE, AL_LEFT_MONITOR_ATTEN) >= 0)
    fprintf(stderr, "Monitor Attenuation L =%3d,R =%3d\n", 
	    buf[31], buf[33]);
else
    fprintf(stderr, "Monitor Attenuation Not supported on this machine\n");

/* print input and output sampling rates */
switch (buf[9])
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
	sprintf(s, "AL_RATE_%d", buf[9]);
    break;
    }
switch (buf[11])
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
	sprintf(s1, "AL_RATE_%d", buf[11]);
    break;
    }
fprintf(stderr, "Sampling Rate       In=%s,Out=%s\n", s, s1);

/* print digital input sampling rate (if avilable) */
if (ALgetdefault(AL_DEFAULT_DEVICE, AL_DIGITAL_INPUT_RATE) >= 0)
    {
    if (buf[29] != AL_RATE_UNDEFINED)
	fprintf(stderr, "Digital Input Rate  %d\n", buf[29]);
    else
	fprintf(stderr, "Digital Input Rate  AL_RATE_UNDEFINED\n");
    }
else
    fprintf(stderr, "Digital Input Rate  Not supported on this machine\n");

/* print input source */
switch (buf[13])
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
	sprintf(s, "Bogus value=%d", buf[13]);
    break;
    }
fprintf(stderr, "Input Source        %s\n", s);


/* print monitor state */
if	(buf[17] == AL_MONITOR_OFF)
    fprintf(stderr, "Monitor             AL_MONITOR_OFF\n");
else if (buf[17] == AL_MONITOR_ON)
    fprintf(stderr, "Monitor             AL_MONITOR_ON\n");
else
    fprintf(stderr, "Monitor             Bogus value=%d\n", buf[17]);


/* print speaker mute state */
if	(buf[19] == AL_SPEAKER_MUTE_OFF)
    fprintf(stderr, "Mute                AL_SPEAKER_MUTE_OFF\n");
else if (buf[19] == AL_SPEAKER_MUTE_ON)
    fprintf(stderr, "Mute                AL_SPEAKER_MUTE_ON\n");
else
    fprintf(stderr, "Mute                Bogus value=%d\n", buf[19]);


/* print microphone mode (if available) */
if (ALgetdefault(AL_DEFAULT_DEVICE, AL_MIC_MODE) >= 0)
    {
    switch (buf[25])
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
		sprintf(s, "Bogus value=%d", buf[25]);
	    else
		sprintf(s, "Not supported on this machine");
	break;
	}
    fprintf(stderr, "Microphone Mode     %s\n", s);
    }
else
    fprintf(stderr, "Microphone Mode     Not supported on this machine\n");

/* print channel mode */
switch (buf[27])
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
	sprintf(s, "Bogus value=%d", buf[27]);
    else
	sprintf(s, "Not supported on this machine");
    break;
    }
fprintf(stderr, "Channel Mode        %s\n", s);

fprintf(stderr, "Input  Ports Open:  %d\n", buf[35]);
fprintf(stderr, "Output Ports Open:  %d\n", buf[37]);

/* if monitor on, input or output ports open, audio hardware in use */
if (buf[17] || buf[35] || buf[37])
    fprintf(stderr, "Someone is using the Audio Hardware\n");
else
    fprintf(stderr, "No one is using the Audio Hardware.\n");
}


