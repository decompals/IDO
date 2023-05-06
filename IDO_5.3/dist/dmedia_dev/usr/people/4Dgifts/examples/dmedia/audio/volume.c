/*
 * volume.c - a simple command line program to control the output gain
 *            of the audio using the Audio Library.
 */

#include <dmedia/audio.h>
#include <stdio.h>

main(int argc, char **argv)
{
    long    parameter_buffer[4];
    long    left_gain, right_gain;

    if (argc < 3)
    {
        fprintf(stderr,"usage: volume left_gain right_gain\n");
        exit(-1);
    }

    left_gain  = atoi(argv[1]);
    right_gain = atoi(argv[2]);

    if (left_gain < 0 || left_gain > 255 || right_gain < 0 || right_gain > 255)
    {
        fprintf(stderr,"gain level out of range: 0 <= gain <= 255\n");
        exit(-1);
    }

    parameter_buffer[0] = AL_LEFT_SPEAKER_GAIN;
    parameter_buffer[2] = AL_RIGHT_SPEAKER_GAIN;
    ALgetparams(AL_DEFAULT_DEVICE, parameter_buffer, 4);

    /*
    fprintf(stderr,"left speaker gain = %d\nright speaker gain = %d\n",
        parameter_buffer[1], parameter_buffer[3]);
    */

    parameter_buffer[1] = left_gain;
    parameter_buffer[3] = right_gain;

    ALsetparams(AL_DEFAULT_DEVICE, parameter_buffer, 4);
}
