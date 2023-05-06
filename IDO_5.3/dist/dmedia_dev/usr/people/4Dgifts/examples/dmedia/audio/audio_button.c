/*
 *    audio_button.c - a simple demo combining a Motif widget and audio.
 *    
 *    N.B - The Motif widget code is taken from the push_button.c example 
 *          in /usr/people/4Dgifts/examples/X11/toolkits/motif/widgets.
 *          Please peruse that code for details of the Motif calls.
 *
 *    to compile:
 *        cc audio_button.c -o audio_button -lXm -lXt -lX11 -laudio
 */

/*
 *    stuff for Motif thingies
 */
#include <Xm/Xm.h>      /* X/Motif header file */
#include <Xm/PushB.h>   /* header file for pushbutton widget type. */

/*
 *    stuff for sproc(2) system call to fork a second thread.
 */
#include <sys/types.h>
#include <sys/prctl.h>

/*
 *    stuff for audio library.
 */
#include <dmedia/audio.h>

/*
 *    the following define enables 4.3BSD behavior for the kill(3B)
 *    system call, and the include file is necessary for this function.
 */
#define _BSD_SIGNALS
#include <signal.h>

#include <stdio.h>
/*
 *    globals
 */
int   audio_pid;
int   rate_to_hertz(long);

main(int argc, char **argv)
{
   Widget      toplevel, button;
   Arg         arg;
   XmString    xmstr;
   void        callback_handler(Widget, caddr_t, caddr_t);
   void        do_audio();

   int         audio_pid;    /* process ID for the audio thread */

   /*
    *    Use the sproc(2) call to create an audio thread. The audio
    *    thread begins execution at the do_audio entry point. The 
    *    second argument to sproc, PR_SALL, allows the two processes
    *    to share all attributes. See the manual page for the sproc(2)
    *    system call for more details.
    */
   audio_pid = sproc(do_audio, PR_SALL);
   if (audio_pid == -1)
   {
       fprintf(stderr, "unable to create audio thread...aborting.\n");
       exit(-1);
   }

   /*
    *    Perform necessary functions to get Motif up and running.
    */
   toplevel = XtInitialize(argv[0], "MotifDemo", NULL, 0, &argc, argv);

   xmstr = XmStringCreateSimple("Stop The Noise");
   XtSetArg(arg, XmNlabelString, xmstr);

   button = XmCreatePushButton(toplevel, "button", &arg, 1);

   /*
    *    Add the callback for the button. Note the additional work the
    *    callback must perform in order to stop the audio process.
    */
   XtAddCallback(button, XmNactivateCallback, callback_handler, NULL);

   XtManageChild(button);

   XtRealizeWidget(toplevel);
   XtMainLoop();
}

void 
callback_handler(Widget widget, caddr_t client_data, caddr_t callback_data)
{
    kill(audio_pid, SIGKILL);
    exit(0); 
}

/*
 *    Cool, fun audio code. This function is the entry point for the 
 *    audio thread which is created above by the sproc function.
 */
void
do_audio()
{
    ALconfig  config;
    ALport    input_port, output_port;
    long      pvbuf[2];
    short     *buffer_ptr;
    int       sampling_rate;

    /*
     *  Figure out the hardware sample rate.
     */
    pvbuf[0] = AL_INPUT_RATE;
    ALgetparams(AL_DEFAULT_DEVICE, pvbuf, 2);
    sampling_rate = rate_to_hertz(pvbuf[1]);
    if (sampling_rate == -1)
    {
        fprintf(stderr, "Whoa there! Weird sampling rate.\n");
        exit(-1);
    }

    /*
     *  Create the input and output ports
     */
    config = ALnewconfig();
    input_port = ALopenport("input", "r", config);
    output_port = ALopenport("output", "w", config);
    if (input_port == NULL || output_port == NULL)
    {
        fprintf(stderr, "Sorry, couldn't find enough open ports.\n");
        exit(-1);
    }

    /*
     *  Create the buffer to hold 1/2 second of audio data based on
     *  the sample rate. Obviously, this will be incorrect if the 
     *  sampling rate is change during execution of the program.
     */
    buffer_ptr = (short *) malloc(sizeof(short) * sampling_rate); 
    if (buffer_ptr == NULL)
    {
        fprintf(stderr, "malloc failure: no sample buffer allocated.\n");
        exit(-1);
    }

    /*
     *  Now, what we've been waiting for: send the input samples to
     *  the output.
     */
    while (1)
    {
        ALreadsamps(input_port, buffer_ptr, sampling_rate);
        ALwritesamps(output_port, buffer_ptr, sampling_rate);
    }
}

static int
rate_to_hertz(long rate)
/*
 *    Translate the AL enumerated type AL_RATE_XXXXX to an integer
 *    number of hertz (cycles per second). Returns -1 if there is 
 *    a problem interpreting the sample rate token.
 */
{
    int result;

    switch (rate)
    {
        case AL_RATE_8000:
            result = 8000;
            break; 
        case AL_RATE_11025:
            result = 11025;
            break; 
        case AL_RATE_16000:
            result = 16000;
            break; 
        case AL_RATE_22050:
            result = 22050;
            break; 
        case AL_RATE_32000:
            result = 32000;
            break; 
        case AL_RATE_44100:
            result = 44100;
            break; 
        case AL_RATE_48000:
            result = 48000;
            break; 
        default:
            result = -1;
            break;
    }
    return(result);
}
