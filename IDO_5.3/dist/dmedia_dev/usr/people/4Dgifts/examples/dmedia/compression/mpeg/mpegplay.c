/*
 * plaympeg.c - Play MPEG.
 *
 */
#define __GETOPT_H__
#include <stdio.h>  /* For i/o */
#include <fcntl.h>  /* For open/read/close */
#include <stdlib.h> /* For malloc */
#include <sys/types.h> /* For sproc */
#include <sys/prctl.h> /* For sproc */
#include <sys/signal.h> /* For kill */
#include <gl.h>	    /* For graphics library */
#include <cl.h>	    /* For compression library */

/* Command line parsing code */
#include "egetopt.c"

int	debugLevel = 0;
#define	DEBUG(a)    if(debugLevel > 0) a
#define	DEBUG2(a)    if(debugLevel > 1) a

int inFile;
static int	    blockSize = 2048;
CL_BufferHdl dataHdl;
static int outOfData = FALSE;
static int compressedBufferSize = 0;
static int loopCount = 1;
static int headerSize = 0;

void GetDataProc(void *huh)
{
    int dataSize, readDataSize, wrap;
    void *data;
    
    while(!outOfData) {
	dataSize = clQueryFree(dataHdl, 0, &data, &wrap);
	if((dataSize >= blockSize) || (wrap > 0))
	{
	    readDataSize = read(inFile, data, dataSize);
	    clUpdateHead(dataHdl, readDataSize);
	    if(readDataSize != dataSize) {
		if(--loopCount > 0)
		    lseek(inFile, headerSize, SEEK_SET);
		else {
		    clDoneUpdatingHead(dataHdl);
		    outOfData = TRUE;
		    return;
		}
	    }
	}
	else /* Can go do something else - In this example we just sleep. */
	    sleep(1);
    }
}

static void
usage (char * progname)
/* complain about bad command line */
{
  fprintf(stderr, "usage: %s ", progname);
  fprintf(stderr, " [-l loopCount] [-z zoomFactor] [-p] [-s speedFactor] [-b blockSize] \n    [-c compressedBufferSize] inputfile\n");

  exit(FAILURE);
}

/*
 * Main program 
 */
main(argc,argv)
int argc;
char **argv;
{
    CL_Handle    decompressorHdl;
    CL_BufferHdl frameHdl;
    int	    width, height, k;
    int    *frameBuffer, frameSize, actualDataSize, wrap;
    int	    c, alg, pid, wpid;
    char    algString[256], string[256];
    long    windowID;
    int    zoom;
    int    grayscale, yuv;
    float   speed;
    short   r[256], g[256], b[256];
    int    dataSize, decompressionScheme;
    void    *data, *header, *dummy;
    int    printLevel;
    int	nextGOPFrame, sizeOfData;
    unsigned char *nextGOPAddress;
    static int    paramBuf[][2] = {
	CL_IMAGE_WIDTH, 0, 
	CL_IMAGE_HEIGHT, 0, 
	CL_ORIGINAL_FORMAT, 0, 
	CL_BITS_PER_COMPONENT, 0, 
	CL_FRAME_RATE, 0, 
	CL_SPEED, 0, 
	CL_INTERNAL_FORMAT, 0, 
	CL_COMPRESSION_RATIO, 0, 
    };
    static int    paramBuf2[][2] = {
	CL_END_OF_SEQUENCE, 0, 
	CL_LAST_FRAME_INDEX, 0, 
	CL_FRAME_TYPE, 0, 
    };
    static int    paramBufQuery[][2] = {
	CL_IMAGE_WIDTH, 0, 
	CL_IMAGE_HEIGHT, 0, 
	CL_FRAME_RATE, 0, 
	CL_INTERNAL_FORMAT, 0, 
	CL_COMPRESSION_RATIO, 0, 
	CL_ALGORITHM_VERSION, 0, 
	CL_NUMBER_OF_FRAMES, 0, 
	CL_ORIENTATION, 0, 
    };
    

    loopCount = 1;
    zoom = 1;
    speed = 1.0;
    printLevel = 0;
    
    /* Scan command line options, adjust parameters */
    while ((c = egetopt(argc, argv, "l:z:b:ps:c:")) != EOF)
    switch (c) {
	case 'l': { 			/* Number of times to loop. */
	    int val;
	    if (optarg == NULL)
		val = 1000000;
	    if (sscanf(optarg, "%d", &val) != 1)
		usage(argv[0]);
	    loopCount = val;
	    break;
	}
	case 'z': { 			/* Zoom factor. */
	    int val;
	    if (optarg == NULL)
		val = 1000000;
	    if (sscanf(optarg, "%d", &val) != 1)
		usage(argv[0]);
	    zoom = val;
	    break;
	}
	case 'b': { 			/* Block size for CB. */
	    int val;
	    if (optarg == NULL)
		val = 1000000;
	    if (sscanf(optarg, "%d", &val) != 1)
		usage(argv[0]);
	    blockSize = val;
	    if(blockSize > 500000) {
		blockSize = 500000;
		printf("blockSize limited to %d\n", blockSize);
	    }
	    break;
	}
	case 's': { 			/* speed factor. */
	    float val;
	    if (optarg == NULL)
		val = 1000000;
	    if (sscanf(optarg, "%f", &val) != 1)
		usage(argv[0]);
	    speed = val;
	    break;
	}
	case 'c': { 			/* compressed buffer size. */
	    int val;
	    if (optarg == NULL)
		val = 1000000;
	    if (sscanf(optarg, "%d", &val) != 1)
		usage(argv[0]);
	    compressedBufferSize = val;
	    break;
	}
	case 'd': { 			/* Debug Level */
	    fprintf(stderr, "Debug set.\n");
	    debugLevel = 1;
	    break;
	}
	case 'p': { 			/* Print Level */
	    printLevel = 1;
	    break;
	}
	case '?':
	default:
	  usage(argv[0]);
	  break;
    }
    
    /* Open movie file */
    if (optind != argc-1) {
      fprintf(stderr, "%s: must name one and only one input file\n", argv[0]);
      usage(argv[0]);
    }
    if ((inFile = open(argv[optind],O_RDONLY)) == -1) {
	fprintf(stderr, "%s: can't open %s\n", argv[0], argv[optind]);
	exit(FAILURE);
    }    
    
    /* Setup GL to display images */
    width = 352;
    height = 240;
    prefsize(width * zoom, height * zoom);
    foreground();
    fprintf(stderr, "Decompressing ");
    grayscale = FALSE;
    yuv = FALSE;
    windowID = winopen("MPEG 352 by 240");
    fprintf(stderr, "%s\n", "MPEG 352 by 240");

    winset(windowID);
    reshapeviewport();
    prefsize(width*zoom,height*zoom);
    rectzoom(zoom, zoom);
    winconstraints();
    RGBmode();
    
    /* Open a Decompressor */
    decompressionScheme = CL_MPEG_VIDEO;
    
    clOpenDecompressor(decompressionScheme, &decompressorHdl);
    
    /* Determine the scheme from the frist 16 bytes of the header 
     * (from the beginning of video data) 
     */
    header = malloc(16);
    lseek(inFile, 0, SEEK_SET);
    read(inFile, header, 16);
    
    headerSize = clQueryMaxHeaderSize(decompressionScheme);
    if(headerSize > 0) {
	/* Read the header from the beginning of video data */
	header = malloc(headerSize);
	lseek(inFile, 0, SEEK_SET);
	read(inFile, header, headerSize);
	headerSize = clReadHeader(decompressorHdl, headerSize, header);
	free(header);
	lseek(inFile, 0, SEEK_SET);
	
	/* Query the parameters set up by reading the header */
	clGetParams(decompressorHdl, (int *)paramBufQuery, 
	    sizeof(paramBufQuery) / sizeof(int));
	printf("(%d x %d @ %f (%s at %f:1) %d frames. %s version %d\n", 
	    paramBufQuery[0][1], paramBufQuery[1][1], CL_TypeIsFloat(paramBufQuery[2][1]), 
	    CL_VideoFormatName(paramBufQuery[3][1]), CL_TypeIsFloat(paramBufQuery[4][1]), 
	    paramBufQuery[6][1], clGetAlgorithmName(clQueryScheme(header)), 
	    paramBufQuery[5][1]);
    }
    paramBuf[5][1] = CL_TypeIsInt(speed);
    clSetParams(decompressorHdl, (int *)paramBuf[5], 2);
    gconfig();
    /* gl draws bottom-up by default */
    clSetParam(decompressorHdl, CL_ORIENTATION, CL_BOTTOM_UP);

    frameSize = width * height * sizeof(int);
    
    if(compressedBufferSize == 0) {
	compressedBufferSize = clGetParam(decompressorHdl, CL_COMPRESSED_BUFFER_SIZE);
    }
    if(blockSize > compressedBufferSize)
	blockSize = compressedBufferSize;
    DEBUG(fprintf(stderr, "%s (block size is %d) (compressed buffer size is %d)\n", string, blockSize));
    
    frameHdl = clCreateBuf(decompressorHdl, CL_FRAME, 1, frameSize, NULL);
    data = malloc(compressedBufferSize);
    dataHdl = clCreateBuf(decompressorHdl, CL_DATA,
	compressedBufferSize, 1, &data);

    /* Start a shared data process that will read the data */
    pid = sproc(GetDataProc, PR_SADDR);

    k = 0;
    nextGOPAddress = data;
    sizeOfData = compressedBufferSize;
    do { 
	/* Decompress each frame and display it */
	/* Flavor 3 - Use buffer management routines */
	clDecompress(decompressorHdl, 1, 0, NULL, NULL);
	frameSize = clQueryValid(frameHdl, 1, (void **)&frameBuffer, &wrap);
	lrectwrite(0, 0, width-1, height-1, (unsigned long *)frameBuffer);
	clUpdateTail(frameHdl, 1);

	if(printLevel > 0) {
	    printf("Frame Number %d. ", k); 
	    clGetParams(decompressorHdl, (int *)paramBuf2, sizeof(paramBuf2) / sizeof(int));
	    if(paramBuf2[2][1] == CL_KEYFRAME) 
		printf("Keyframe. "); 
	    else if(paramBuf2[2][1] == CL_PREDICTED)
		printf("Predicted Frame. ");
	    else if(paramBuf2[2][1] == CL_BIDIRECTIONAL)
		printf("Bidirectional Frame. ");
	    else 
		printf("Unknown Frame Type (%d). ", paramBuf2[2][1]);
	    if(paramBuf2[1][1] != k) 
		printf("(Actual Frame is %d)\n", paramBuf2[1][1]);
	    else
		printf("\n");
	    }
	else
	    clGetParams(decompressorHdl, (int *)paramBuf2, 2);
	k++;
	if(outOfData == TRUE) {
	    /* GetDataProc() ran out of data, so we should stop when
	     * we run out of data in the buffer.
	     */
	    dataSize = clQueryValid(dataHdl, 1, (void **)&dummy, &wrap);
	    if(dataSize + wrap == 0)
		break;
	}
    } while(!clGetParam(decompressorHdl, CL_END_OF_SEQUENCE));

    if (pid != 0) 
	kill(pid, SIGKILL);
    clDestroyBuf(frameHdl);
    clDestroyBuf(dataHdl);
    
    /* Close Decompressor */
    clCloseDecompressor(decompressorHdl);
    
    exit(0);
}


