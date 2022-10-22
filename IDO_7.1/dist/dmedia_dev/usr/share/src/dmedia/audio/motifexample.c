#include "stdio.h"
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include "ulocks.h"
#include <sys/types.h>
#include <sys/prctl.h>
#include <stropts.h>
#include <poll.h>
#include "audio.h"
#include <limits.h>
#include <sys/schedctl.h>
#include "audiofile.h"
#include <fcntl.h>


/*--------------------------------------------------------------------------*/

/*
 * The following is a simple audio application.
 */

char *buttons[] ={ "Start", "Pause", "Stop"};
XtCallbackProc callback_handler();
void SendAudioCommand(char *cmd);
void InitializeAudioProcess(void);

main(int argc, char **argv)
{
    Widget toplevel, rowcol, w;
    int i;
    toplevel = XtInitialize(argv[0], "AudioDemo", NULL, 0, &argc, argv);
    rowcol=XtCreateManagedWidget("rowcol",xmRowColumnWidgetClass,toplevel,NULL,0);
    for(i=0;i<XtNumber(buttons); i++){
	w=XtCreateManagedWidget(buttons[i],xmPushButtonWidgetClass,rowcol,NULL,0);
        XtAddCallback(w,XmNactivateCallback,(XtCallbackProc)callback_handler,buttons[i]);

    }
    XtRealizeWidget(toplevel);
    InitializeAudioProcess();
    XtMainLoop();
}

XtCallbackProc
callback_handler(widget, client_data, callback_data)
{
    SendAudioCommand((char *)client_data);
}

/*--------------------------------------------------------------------------*/

/*
 * The following variables are shared between the audio handling
 * process and the main process
*/

usema_t *SendSema, *RcvSema;
char* command;

/*--------------------------------------------------------------------------*/

/*
 * This is how we create the audio handling process.
 */

void AudioProcess(void *);

void
InitializeAudioProcess()
{
    usptr_t *arena;
    static short int signum=9;
    usconfig(CONF_ARENATYPE,US_SHAREDONLY);
    arena=usinit(tmpnam(0));
    SendSema=usnewpollsema(arena, 0);
    RcvSema =usnewsema(arena, 1);
    prctl (PR_SETEXITSIG, (void *)signum);
    sproc(AudioProcess,PR_SALL);
}

/*--------------------------------------------------------------------------*/

/*
 * This is how we send commands to the audio handling process.
 */

void
SendAudioCommand(char *cmd)
{
    /* Send the command */
    command=(char *)cmd;
    usvsema(SendSema);
    /* Wait for response */
    uspsema(RcvSema);
}

/*--------------------------------------------------------------------------*/

/*
 * This is the audio handling process.
 */

ALconfig config;
ALport port;
AFfilehandle file;

int nowplaying = 0;
short sampbuf[28800];
short *soundstart, *soundptr, *soundend;

#define AUDIO_LOW_WATER_MARK   9600
#define AUDIO_HIGH_WATER_MARK 28800

static char filename[] = "soundfile.aifc";

void
AudioProcess(void *v)
{
    struct pollfd PollFD[2];

    if (LoadAudioSamples() < 0)
        exit(-1);
    if (OpenAudioPort() < 0)
        exit(-1);

    PollFD[1].fd=ALgetfd(port);
    PollFD[1].events=POLLOUT;
    schedctl(NDPRI,NDPHIMIN);
    PollFD[0].fd=usopenpollsema(SendSema, 0777);
    PollFD[0].events=POLLIN;
    uspsema(SendSema);
    while(1) {
	ALsetfillpoint(port,ALgetqueuesize(config)-AUDIO_LOW_WATER_MARK);
	poll(PollFD,2,-1);
        if(PollFD[0].revents&POLLIN){
	    ProcessAudioCommand();
	    uspsema(SendSema);
	    usvsema(RcvSema);
	}
        if(PollFD[1].revents&POLLOUT){
            GetMoreAudioSamples();
	}
    }
}

int 
OpenAudioPort()
{
    long pvbuf[4];

    pvbuf[0] = AL_OUTPUT_RATE;
    pvbuf[2] = AL_OUTPUT_COUNT;
    ALgetparams(AL_DEFAULT_DEVICE, pvbuf, 4);

    if (pvbuf[1] != 44100) {
        if (pvbuf[3] > 0)
            return(-1);
        else {
            pvbuf[1] = 44100;
            ALsetparams(AL_DEFAULT_DEVICE, pvbuf, 2); 
        }
    } 
    config=ALnewconfig();
    ALsetchannels(config,AL_4CHANNEL);
    ALsetwidth(config,AL_SAMPLE_16);
    port=ALopenport("demoprog","w",config);
}

int 
LoadAudioSamples()
{
    int fd;
    int nsamps, nframes;
	int nchannels = 4;
    double rate;

    fd = open(filename, O_RDONLY);

    if (fd > 0) 
        file = afOpenFD(fd, "r", 0);
    else 
        printf("couldn't open file %s\n", filename);

    rate = afGetRate(file, AF_DEFAULT_TRACK);
    if (rate != 44100.0) {
        printf("%s is not a 44.1 kHz input file\n", filename);
        return(-1);
    }
	
    nframes = afGetFrameCount(file, AF_DEFAULT_TRACK);
    nsamps = nchannels * afGetFrameCount(file, AF_DEFAULT_TRACK);

    afSetVirtualChannels(file, AF_DEFAULT_TRACK, nchannels);

    soundstart = (short *)malloc(nsamps * sizeof(short));
    soundend = soundstart + nsamps;
    soundptr = soundstart;

    mpin(soundstart, nsamps*sizeof(short));
    afReadFrames(file, AF_DEFAULT_TRACK, soundstart, nframes);
    return(0);
}

ProcessAudioCommand()
{
    if (!strcmp(command, "Start")) 
        nowplaying = 1;
    else if (!strcmp(command, "Pause"))
        nowplaying = 0;
    else if (!strcmp(command, "Stop")) {
        nowplaying = 0;
        soundptr = soundstart;
    }
}

#define MIN(a,b) ((a)<(b)?(a):(b))

GetMoreAudioSamples()
{
    int nsamps,n;
    int nsampsgot = 0;
    int nchannels = 4;

    nsamps=AUDIO_HIGH_WATER_MARK-ALgetfilled(port);
    nsamps=nsamps - nsamps%nchannels;
    if(nsamps>0) {
        if (nowplaying) {
            while (nsampsgot < nsamps) {
                n = MIN(nsamps, soundend-soundptr);
                bcopy(soundptr, sampbuf, n*sizeof(short));
                nsampsgot+=n;
                if (n >= soundend-soundptr) {
                    soundptr = soundstart;
                }
                else 
                    soundptr += n;
            }
        } 
        else 
            bzero(sampbuf, nsamps*sizeof(short));
        ALwritesamps(port,sampbuf,nsamps);
    }
}

