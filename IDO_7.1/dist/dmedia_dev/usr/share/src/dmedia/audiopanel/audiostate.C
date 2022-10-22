/*******************************************************************
 *	Code to print, load and save Audio Hardware state
 *
 *			    Written by Gints Klimanis 
 *			Silicon Graphics Computer Systems
 *				 1993-5
 *******************************************************************/
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <audio.h>

#include "apaneldefs.h"		

Boolean printErrorsToConsole = True;

char applicationUsage[] = 
"Usage: audiostate [Options]\n\
Options:\n\
  [-open filename] [-save filename]\n\
Absence of Options will yield a print out of audio hardware state";

/* ******************************************************************
 * main:	
 * ****************************************************************** */
    void 
main(int argc, char **argv)
{
int	i;

/* Attempt to open Indigo style audio IO port.  On failure, machine has 
 * no audio ability, show warning box with message regarding absence 
 * of audio hardware.  Exit application  
 */ 
{
int audioLibraryFileID = ::open("/dev/hdsp/hdsp0master", O_RDONLY);
if (audioLibraryFileID < 0)
    {
    fprintf(stderr, "This machine has no audio driver.\n");
    for (i = 0; i < 3; i++)
	close(i);
    exit(1);
    }
close(audioLibraryFileID);
}

// disable AL error handler 
ALseterrorhandler(0);

// print audio hardware state on no command line arguments
if (argc == 1)
    {
    PrintAudioHardwareState();
    exit(0);
    }

for (i = 1; i < argc; i++)
    { 
//printf("argc%d: '%s'\n", i, argv[i]);

// check for -h or -help 
    if	    (!strcmp(argv[i], "-h") || !strcmp(argv[i], "-help"))
	{ 
	printf("%s\n", applicationUsage); 
	exit(1); 
	}

// check for -o or -open or -openfile 
    else if (!strcmp(argv[i], "-o") || !strcmp(argv[i], "-open") || 
		!strcmp(argv[i], "-openfile"))
	{ 
	if (++i > argc)
	    {
	    fprintf(stderr, "Well, supply %s!\n", argv[i-1]);
	    exit(-1);
	    }
    // configure audio hardware state
	LoadAudioHardwareState(argv[i]);
	}
// check for -s or -save or -savefile 
    else if (!strcmp(argv[i], "-s") || !strcmp(argv[i], "-save") || 
		!strcmp(argv[i], "-savefile"))
	{ 
	FILE *fd;
	if (++i > argc)
	    {
	    fprintf(stderr, "Well, supply %s!\n", argv[i-1]);
	    exit(-1);
	    }

	fd = fopen(argv[i], "w");
	if (fd) 
	    {
	    long minimum, maximum;
	    char inputChannelCapacity = 4, overallChannelCapacity = 4;
	    char haveStereoMicrophoneAbility = True;

	// Add Options menu items for four channel mode and stereo microphone,
	// if those abilities are present in Audio Hardware 
	    if (ALgetminmax(AL_DEFAULT_DEVICE, AL_MIC_MODE, &minimum, &maximum) != 0)
		haveStereoMicrophoneAbility = False;
	    else if (maximum < AL_STEREO || minimum > AL_STEREO)
		haveStereoMicrophoneAbility = False;

	    if (ALgetminmax(AL_DEFAULT_DEVICE, AL_CHANNEL_MODE, &minimum, &maximum) != 0)
		{
		inputChannelCapacity   = 2;
		overallChannelCapacity = 2;
		}
	    if (maximum < AL_4CHANNEL)
		{
		inputChannelCapacity   = 2;
		overallChannelCapacity = 2;
		}
	    WriteAudioHardwareState(fd, inputChannelCapacity, overallChannelCapacity,   
				    haveStereoMicrophoneAbility);
	    fclose(fd);
	    }
	}
    }

}


