/**********************************************************************
Demonstrates vl -> memory -> cl  -> movie file

NOTES:
       - vl_mem_cl opens vl device 0 by default.  On Indy's this
	 is always vino.  This program has been verified to work
	 with vino and ev1, although ev1's memory interface is very slow.

       - vl_mem_cl uses the default port on the selected device.
         Use videopanel to select the desired default port.

**********************************************************************/

/* General Headers */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <getopt.h>
#include <sys/time.h>
#include <signal.h>
#include <ulocks.h>
#include <assert.h>

/* Movie headers */
#include <movie.h>
#include <moviefile.h>

/* GL headers */
#include <gl/gl.h>
#include <gl/device.h>
#include <gl/image.h>

/* VL headers */
#include <vl/vl.h>

/* CL headers */
#include <dmedia/cl.h>
#include <dmedia/cl_cosmo.h>


/* ------------------ Function protoypes ------------------ */
void		processGlEvent();
void		parseargs(int argc, char **argv);
int		gotErr();
void		cleanupExit();

/* Error macros */

#ifdef __ANSI_CPP__
#define PRINT_CALL(call)   fprintf( stderr, "Call: %s\n", # call );
#else
#define PRINT_CALL(call)   fprintf( stderr, "Call: %s\n", "call" );
#endif

/* CC - Compression Check  Wraps a CL call that returns <0 on error  */
#define CC(call)						\
   {								\
   if( (call) < 0 ) 						\
      {								\
      PRINT_CALL(call);						\
      fprintf( stderr,  "Compression library error.\n" );	\
      gotErr();							\
      }								\
   }								\


/* CNC - Compression NULL Check  Wraps a CL call that returns NULL on error */
#define CNC(call)							\
   {									\
   if ( (call) == NULL )						\
      {									\
      PRINT_CALL(call);							\
      fprintf( stderr,  "Compression library error.\n" );		\
      gotErr();								\
      }									\
   }									\


/* VC - Video Check  Wraps a VL call that returns <0 on error  */
#define VC(call)							\
   {									\
   if ( (call) < 0 )							\
      {									\
      fprintf( stderr, "%s: %s\n", _progName, vlStrError(vlErrno));	\
      PRINT_CALL(call);							\
      gotErr();								\
      }									\
   }									\


/* VNC - Video NULL Check  Wraps a VL call that returns NULL on error */
#define VNC(call)							\
   {									\
   if ( (call) == NULL )						\
      {									\
      fprintf( stderr, "%s: %s\n", _progName, vlStrError(vlErrno));	\
      PRINT_CALL(call);							\
      gotErr();								\
      }									\
   }									\


/* MC - Movie Check  Wraps movie lib call that returns DM_FAILURE on err  */
#define MC(call)							\
   {									\
   if ( (call) != DM_SUCCESS ) 						\
      {									\
      fprintf( stderr, "%s: %s\n", _progName,				\
	 mvGetErrorStr(mvGetErrno()) );					\
      PRINT_CALL(call);							\
      gotErr();								\
      }									\
   }									\


/* DC - DM Params Check  Wraps dmParams fn that returns DM_FAILURE on err  */
#define DC(call)							\
   {									\
   if ( (call) != DM_SUCCESS ) 						\
      {									\
      fprintf( stderr, "%s: Out of memory\n", _progName );		\
      PRINT_CALL(call);							\
      gotErr();								\
      }									\
   }									\


/* OC OS Check Wraps a Unix call that returns negative on err */
#define OC(call)							\
   {									\
   if ( (call) < 0 ) 							\
      {									\
      perror( _progName );						\
      PRINT_CALL(call);							\
      exit( EXIT_FAILURE );						\
      }									\
   }                                  					\


/* OCW - OS Check Warn  Like OC, but prints warning and continues */
#define OCW(call)							\
   {									\
   if ( (call) < 0 ) 							\
      {									\
      fprintf( stderr, " \nWarning: " );				\
      perror( _progName );						\
      PRINT_CALL(call);							\
      }									\
   }									\


/* ONC - OS Null Check  Wraps a Unix call that returns NULL on error  */
#define ONC(call)							\
   {									\
   if ( (call) == 0 ) 							\
      {									\
      perror( _progName );						\
      PRINT_CALL(call);							\
      exit( EXIT_FAILURE );						\
      }									\
   }									\



#define Verbose	if (verbose) printf
#define TIMEOUT	1000


/* --------------------  Globals -------------------- */
/* flags (command line parameters) */
int	fieldsPerBuffer= 4;         /* num fields raw video in rb's   */
int	verbose= 0;                 /* no verbose comments by default */
int	displayVideo= 1;            /* Display video as it arrives    */
int	xVidSize, yVidSize;         /* video field size               */
int	xCompSize, yCompSize;       /* field size to be compressed    */
int 	hardware_jpeg= 1;           /* use hw==cosmo vs software jpeg */
int 	capture= 1;                 /* defualt to movie file capture  */
int	cosmoQueuemode= 1;          /* use cosmo queue mode           */
int	double_buffer= 0;           /* default to non-double buffered */
int	compressVideo= 1;           /* default using compression      */
int	quality= 75;                /* jpeg quality factor            */
int	zoom_num= 1;                /* image zoom                     */
int	zoom_denom= 1; 

/* file/psuedo handles */
long glWindowHandle;
VLPath		vlPath;
VLServer	vlSvr;
VLNode		drn;
VLNode		src;
int 		shovelId=  0;

usptr_t		*semaHdl;
usema_t		*dropSema; /* couting semaphore to synch frame drops */

CL_CompressorHdl compressorHdl;
CL_BufferHdl	dataHdl=  NULL;
CL_BufferHdl	frameHdl= NULL;

MVid		sfd;	/* movie file handle */
MVid		outTrack;


/* buffers */
VLBuffer	transferBuf;
char 		*cbuffer, *cbuffer2;

struct 
   {
   int field1size;
   int field2size;
   }dummyFrame;


/* 32k guess at MAX jpeg data Size (4 of those) */
int QUEUE_BUFFER_SIZE= (32*1024*4); /* 4 worst case compressed fields */

char *compressedBuffer;
char *rawframeBuffer; /* this is the tmp buffer only needed for copy */

#define VNAMESIZE 10

/* misc */
char *		_progName;
int		totalCount= 10000000;  /* stop after totalCount frames */
int		framenumber= 0;
float		rate;	        /* videoframe rate */
DMinterlacing	interlaceMode;
char		vidSrcName[VNAMESIZE];
char 		*deviceName;
VLControlValue 	val;
char		fname[30];      /* movie file name */
int		frame= 0;
int		compressing= 0; /* set when the cl compressor is activated */
int		stopVLloop= 0;
int		stopShovelLoop= 0;
int		running= 0;
int		shovelFinished= 0; /* the shovel process has completed        */
int		processVLfinished= 0; /* the vl processing loop completed */




/**********************************************************************
shovelDataToMovie 

Only complete frames are stored to disk.

NOTE: This method is just an example to show users, how to minimize
      disk access/write latencies only.
      A simple alternative, is to write on a per frame basis.
**********************************************************************/
void
shovelDataToMovie()
{
CLimageInfo	imageInfo[2];
void		*frameAddr;
int		wrappedBytes, unwrappedBytes;
char		*wrapbuffer; /* dynamicaly allocated buffer, grows as needed */
int		wrapbufferlen= 0; /* keeps track of wrapbuffer's current size */
int		frameNumber= 0;
sigset_t        sigset;
sigset_t	currentSet;
int 		value;
int		fieldcount;
int		frameSize;
int		wrap;
int		dropCount;

/* wait for the compressor to be turned on! */
while((!compressing)&&(!stopShovelLoop)&&(!stopVLloop))
   sginap(1);

while(!stopShovelLoop) /* shovel data until the plug gets pulled */
   {
   /* handle the requested drops */
   if(frameNumber) /* we can not drop the 1st frame! */
      {
      dropCount= ustestsema(dropSema)/2; /* determine no. of frames to drop */
      while(dropCount--)
	 {
	 /* insert a droped frame marker into the movie file */
	 MC(mvInsertCompressedImage(outTrack, frameNumber++,
	    sizeof(dummyFrame), &dummyFrame) );
	 uspsema(dropSema); /* decrement the couting semaphore by one frame */
	 uspsema(dropSema); 
	 }
      }

   fieldcount= 0; /* reset fieldcounter */
   while(fieldcount<2)
      {
      /* loop waiting for a new comrpessed frame, or cl err */
      while(!stopShovelLoop && 
	 ((value= 
	    clGetNextImageInfo(compressorHdl, &imageInfo[fieldcount], 
	    sizeof(CLimageInfo))) 
				== CL_NEXT_NOT_AVAILABLE)
	 )
	 sginap(1); /* nap so we don't burn too many cycles polling! */ 
      if(value!=SUCCESS) /* did we stop looping due to a cl error?   */
	gotErr(); 

      if(stopShovelLoop)
	 break;

      fieldcount++; /* got another field */
      }

   if(stopShovelLoop)
      break;


   /* got 2 fields==a frame, (now party) */
   frameSize= imageInfo[0].size + imageInfo[1].size;

   CC((unwrappedBytes= clQueryValid(dataHdl, 0, (void **)&frameAddr,
      &wrappedBytes)) ); 

   dummyFrame.field1size= imageInfo[0].size; /* save info for next drop! */
   dummyFrame.field2size= imageInfo[1].size;

   if(unwrappedBytes >= frameSize)
      { /* easy, contiguous case */
      /* write data to movie file */
      if(capture)
	 MC(mvInsertCompressedImage(outTrack, frameNumber++, frameSize,
	    frameAddr) ); 

      /* done with these bytes, make space for data filled by cosmo driver  */
      CC(clUpdateTail(dataHdl, frameSize) ); 
      }
   else
      { /* handle wrap condition */
      if(wrapbufferlen < frameSize) 
	 {
	 Verbose("shovel: mallocing a larger buffer size(0x%X)\n", frameSize);
	 if(wrapbufferlen)
	    free(wrapbuffer);
	 wrapbuffer= malloc(frameSize);
	 wrapbufferlen= frameSize;
	 }
      wrappedBytes= frameSize - unwrappedBytes;
      bcopy(frameAddr, wrapbuffer, unwrappedBytes); /* stash data before wrap */
      CC( clUpdateTail(dataHdl, unwrappedBytes) );  /* advance cl's ptr */
      CC( clQueryValid(dataHdl, wrappedBytes, &frameAddr, &wrap) );
      bcopy(frameAddr, ((char*)wrapbuffer) + unwrappedBytes, wrappedBytes);
      CC( clUpdateTail(dataHdl, wrappedBytes) );

   Verbose("shovel: wrapping fsize= %d, unwrap= %d, wrapb= %d frmAddr= 0x%X\n",
	 frameSize, unwrappedBytes, wrappedBytes, frameAddr);

      /* write data to movie file */
      if(capture)
	 MC(mvInsertCompressedImage(outTrack, frameNumber++, frameSize,
	    wrapbuffer) ); 
      }

   Verbose("Recorded (%8d, %8d) bytes for frame %d\n", 
      imageInfo[0].size, imageInfo[1].size, frameNumber);
   }

Verbose("shovelDataToMovie exiting due to stopShovelLoop flag\n");
shovelFinished= 1;
}


/**********************************************************************
parseargs

Parse the command line arguments setting global mode flags.
**********************************************************************/
void
parseargs(int argc, char **argv)
{
int	c;
int	len, xlen, ylen;
char	*numbuf;
char	*xloc;
char	*cmdargs= "b:c:dDCfsnv:Qq:z:";
char	*car;

char *usage_m =
   "usage: %s %s moviefile.mv\n"
   "where arguments may be:\n"
   "	-b n	use n buffers\n"
   "	-c n	capture n frames\n"
   "	-d	turn off double buffer mode\n"
   "	-D	turn on verbose mode (displays more information)\n"
   "	-f	displayVideo mode (no display)\n"
   "	-n	no compression\n"
   "	-v	video source (depends on hardware; see release notes)\n"
   "	-C 	turn movie file capture off\n"
   "	-Q      don't cosmo queue-mode (use one shot instead)\n"
   "	-q      JPEG quality factor from 1-100 (default is 75)\n"
   "	-z      zoom factor represented as the fraction num/denom\n"
   "	-s	software (not cosmo hardware) jpeg\n";


while((c = getopt(argc, argv, cmdargs)) != EOF) 
   {
   switch(c) 
      {
      case 'b': fieldsPerBuffer= atoi(optarg);		break;
      case 'c': totalCount= atoi(optarg);		break;
      case 'D': verbose= 1;				break;
      case 'd': double_buffer= 1;			break;
      case 'f': displayVideo= 0;			break;
      case 'Q': cosmoQueuemode= 0;			break;
      case 'C': capture= 0;				break;
      case 's': hardware_jpeg= 0; cosmoQueuemode= 0;	break;
      case 'v': strncpy(vidSrcName, optarg, VNAMESIZE); break;

      case 'z': 
	 if(sscanf(optarg, "%d/%d",
	    &zoom_num, &zoom_denom) != 2 || !zoom_num || !zoom_denom)
	    {
	    fprintf(stderr, "%s: ERROR: zoom format <num>/<denom>", _progName);
	    exit(1);
	    }
	 Verbose("using zoom of: %d/%d\n", zoom_num, zoom_denom);
      break;

      case 'q':
	 quality= atoi(optarg);			
	 if((quality > 100) || (quality < 1))
	    {
	    fprintf(stderr,
	       "quality of %d is out of the legal range [1-100]\n");
	    exit(-1);
	    }
      break;

      case 'n': 
         compressVideo= 0;	
	 capture= 0;
      break;

      default:
	 fprintf(stderr, usage_m, _progName, cmdargs);
	 exit(0);
      break;
      }
   }

if(optind == argc) /* are there more arguments */
   { /* No!  Missing file name, print usage and exit. */
   fprintf(stderr, "Missing filename\n");
   fprintf(stderr, usage_m, _progName, cmdargs);
   exit(0);
   }
else 
   {
   strcpy(fname, argv[optind]);

   /* if no .mv extension, then add it! */
   for(car= fname; *car; car++);  /* examine end of the string           */
   if(*(car-1) != 'v')            /* is the last letter of string a 'v'? */
      strcat(fname, ".mv");       /* No! add .mv extension               */
   Verbose("movie file: [%s]\n", fname);
   }
}


/**********************************************************************
This routine sets the rate, x,y VidSize and x,y CompSize globals based 
on the vl's idea of what is attatched to the port!
**********************************************************************/
void
initVL()
{
int		devicenum;
VLDevList	devlist;
int		index;
int		vidsrc;
VLControlValue	value;

VNC(vlSvr= vlOpenVideo("") ); /* Connect to the video daemon */

VC(vlGetDeviceList(vlSvr, &devlist) ); /* Get list of devices */

if(!*vidSrcName)
   strcpy(vidSrcName, "vino"); /* default to vino */

/* search for our device */
index= devlist.numDevices;
while(1)
   {
   if(index== -1)
      {
      fprintf(stderr, "Video Device %s not found\n", vidSrcName);
      if(devlist.numDevices)
	 {
	 fprintf(stderr, "Try one of: \n");
	 for(index= 0; index < devlist.numDevices; index++)
	    fprintf(stderr, "    %s\n", devlist.devices[index].name);
	 }
      exit(0);
      }

   if(!strcmp(devlist.devices[index].name, vidSrcName))
      {
      vidsrc= index;
      break;
      }
   else
      index--;
   }


/* Set up source and drain nodes (source node on the default port) */
VC(src= vlGetNode(vlSvr, VL_SRC, VL_VIDEO, VL_ANY) );
VC(drn= vlGetNode(vlSvr, VL_DRN, VL_MEM, VL_ANY) );

/* Create a path from the selected video device, vidsrc, to memory  */
VC(vlPath= vlCreatePath(vlSvr, vidsrc, src, drn) );

VC(devicenum= vlGetDevice(vlSvr, vlPath) ); /* device number for future use */

/* Set up the hardware for and define the usage of the path */
VC(vlSetupPaths(vlSvr, (VLPathList)&vlPath, 1, VL_SHARE, VL_SHARE) );

/* Set the zoom ratio */
value.fractVal.numerator=   zoom_num;
value.fractVal.denominator= zoom_denom;
vlSetControl(vlSvr, vlPath, drn, VL_ZOOM, &value);

/* Get the name of the device we're using */
deviceName= devlist.devices[devicenum].name;
Verbose("deviceName= [%s]\n", deviceName);

/* determine the video rate */
VC(vlGetControl(vlSvr, vlPath, VL_ANY, VL_TIMING, &value) );
if(val.intVal == VL_TIMING_625_SQ_PIX)
   { /* PAL */
   rate= 25.0;
   interlaceMode= DM_IMAGE_INTERLACED_EVEN;
   }
else
   { /* NTSC */
   rate= 29.97;
   interlaceMode= DM_IMAGE_INTERLACED_ODD;
   }

/* determine the size */
VC(vlGetControl(vlSvr, vlPath, drn, VL_SIZE, &value) );
xVidSize=  value.xyVal.x;
yVidSize=  value.xyVal.y/2;

/* round up to nearest multiple of the jpeg block size (8) */
xCompSize= ((xVidSize+7)/8)*8;
yCompSize= ((yVidSize+7)/8)*8;

Verbose("video filed size: %d,%d,  compress field size: %d,%d\n",
    xVidSize, yVidSize, xCompSize, yCompSize);

/* Set up for non-interleaved frame capture */
val.intVal= VL_CAPTURE_NONINTERLEAVED;
VC(vlSetControl(vlSvr, vlPath, drn, VL_CAP_TYPE, &val) );

/* Specify what vlPath-related events we want to receive */
VC(vlSelectEvents(vlSvr, vlPath,
   VLTransferCompleteMask | VLStreamPreemptedMask | VLTransferFailedMask) );

/* Make sure non-interleaved capture is supported by the hardware */
VC(vlGetControl(vlSvr, vlPath, drn, VL_CAP_TYPE, &val) );

if(val.intVal != VL_CAPTURE_NONINTERLEAVED)
   fprintf(stderr,"Cptr type not set to NONINTERLEAVED: %d\n", val.intVal);
Verbose("Field (non-interleaved) mode\n");

/* Set the packing type for the video board to 32bit rgba */
val.intVal= VL_PACKING_RGB_8;
VC(vlSetControl(vlSvr, vlPath, drn, VL_PACKING, &val) );

/* Set up the ring buffer for data transfer */
VNC(transferBuf= vlCreateBuffer(vlSvr, vlPath, drn, fieldsPerBuffer) );

/* Associate the ring buffer with the path */
VC(vlRegisterBuffer(vlSvr, vlPath, drn, transferBuf) );
}


/**********************************************************************
initGL

   Setup an iris-gl window for the purpose of displaying video during
capture.
**********************************************************************/
void
initGL()
{
char window_name[128];

foreground();    /* Run in foreground only */

/* Set the window to the same geometry as the video data */
prefsize(xVidSize, yVidSize);

/* display video src in window title */
sprintf(window_name, "Capture from %s", deviceName); 

glWindowHandle= winopen(window_name); /* Create window */

if(double_buffer) /* double buffer video in window */
   doublebuffer();

RGBmode(); /* Set the graphics to RGB mode */

/* Allow these key presses, mouseclicks, etc to be entered in the event queue */
qdevice(DKEY);
qdevice(QKEY);
qdevice(WINSHUT);
qdevice(WINQUIT);

gconfig(); /* Set up the graphics subsystem */

clear();  /* clear the initial image       */
if(double_buffer)
   swapbuffers();

pixmode(PM_TTOB, 1); /* lrectwrite from top to bottom (match video) */
}


/**********************************************************************
initCL

Setup the cl for jpeg video compression, using Cosmo hardware or software
jpeg.


NOTE: This routine is affected by the state of the hardware_jpeg global flag.
**********************************************************************/
void
initCL()
{
int scheme;
int index= 0;
int maxCompressedBufferSize;
int paramBuf [20];

scheme= (hardware_jpeg)?CL_JPEG_COSMO:CL_JPEG_SOFTWARE;/*select compressor type*/
Verbose("using %s jpeg\n", hardware_jpeg?"COSMO":"Software");

CC(clOpenCompressor(scheme, &compressorHdl) );  /* open the compressor */

paramBuf[index++]= CL_IMAGE_WIDTH;
paramBuf[index++]= xCompSize;

paramBuf[index++]= CL_IMAGE_HEIGHT;
paramBuf[index++]= yCompSize;

paramBuf[index++]= CL_JPEG_QUALITY_FACTOR;
paramBuf[index++]= quality;

paramBuf[index++]= CL_ORIGINAL_FORMAT;
paramBuf[index++]= CL_RGBX;

paramBuf[index++]= CL_INTERNAL_FORMAT;
paramBuf[index++]= CL_YUV422;

if(hardware_jpeg)
   {
   /* this parameter must be set for non-queueing mode */
   paramBuf[index++]= CL_ENABLE_IMAGEINFO;
   paramBuf[index++]= 1;
   }

/* enable stream headers */
paramBuf[index++]= CL_STREAM_HEADERS;
paramBuf[index++]= TRUE;

CC(clSetParams(compressorHdl, paramBuf, index) ); 


CC(maxCompressedBufferSize=
      clGetParam(compressorHdl, CL_COMPRESSED_BUFFER_SIZE) ) ;

/* malloc enough space for 2 worst case compressed fields */
ONC(cbuffer= (char *)malloc(2*maxCompressedBufferSize) ); 

if(cosmoQueuemode)
   {
   compressedBuffer=  NULL;

   /* compressed jpeg buffer from cosmo */
   CNC((dataHdl= 
      clCreateBuf(compressorHdl, CL_DATA, QUEUE_BUFFER_SIZE, 1,
	 (void **)&compressedBuffer)) ); 

   ONC(rawframeBuffer= 
      (char *)malloc(fieldsPerBuffer * xCompSize * yCompSize * 4) );

   CNC((frameHdl= 
      clCreateBuf(compressorHdl, CL_FRAME, fieldsPerBuffer, 
	 xCompSize * yCompSize * 4, (void **)&rawframeBuffer)) );
   }
}


/**********************************************************************
initMovie

   Sets up the movie library to create a movie file with an image track. 
**********************************************************************/
void
initMovie()
{
DMparams	*movieParams, *imageTrackParams;

/* Create Movie Parameters and File */ 
MC(dmParamsCreate(&movieParams) ); 
MC(mvSetMovieDefaults(movieParams, MV_FORMAT_SGI_3) );
MC(mvCreateFile(fname,  movieParams, NULL, &sfd) ); 
dmParamsDestroy(movieParams);

/* Create Image Track Parameters */
DC(dmParamsCreate(&imageTrackParams) ); 
DC(dmSetImageDefaults(imageTrackParams, xCompSize, 2 * yCompSize, DM_PACKING_RGBX) );
DC(dmParamsSetString(imageTrackParams, DM_IMAGE_COMPRESSION, DM_IMAGE_JPEG) );
DC(dmParamsSetEnum(imageTrackParams, DM_IMAGE_INTERLACING, interlaceMode) );
DC(dmParamsSetEnum(imageTrackParams, DM_IMAGE_ORIENTATION, DM_TOP_TO_BOTTOM) );
DC(dmParamsSetFloat(imageTrackParams, DM_IMAGE_RATE, rate) );
MC(mvAddTrack(sfd, DM_IMAGE, imageTrackParams, NULL, &outTrack) ); 
dmParamsDestroy(imageTrackParams);
}


/**********************************************************************
initSema

Creates a us (shared memory arena) semaphore for use in the drop frame
mechanism.
**********************************************************************/
void
initSema()
{
char		*template= "vmcXXXXXX";

/* use an unique arena name */
mktemp(template);

/* modify usinit to create arena's suchthat the filename is unlinked  */
/* so that the file will not linger on the disk                       */
usconfig(CONF_ARENATYPE, US_SHAREDONLY); 

if((semaHdl= usinit(template)) == NULL) 
   {
   fprintf(stderr, "usinit() failed\n");
   exit(0);
   }

/* setup a semaphore, initialized to 0 */
if((dropSema= usnewsema(semaHdl, 0)) == NULL) 
   {
   fprintf (stderr, "usnewsema () failed.\n");
   exit (0);
   }
}

/**********************************************************************
activateVL

   Enables event callbacks, and begins the video transfers.
**********************************************************************/
void
activateVL()
{
VC(vlBeginTransfer(vlSvr, vlPath, 0, NULL) ); /* Begin the data transfer */
}


/**********************************************************************
cleanVL

   Cleanly shuts off video transfers, dissasociates buffers, then 
closes the video library
**********************************************************************/
void
cleanVL()
{
int loop, j;

Verbose("cleaning up the VL\n");
vlEndTransfer(vlSvr, vlPath);   /* End the data transfer */

/* Disassociate the ring buffer from the path */
vlDeregisterBuffer(vlSvr, vlPath, drn, transferBuf);

vlDestroyPath(vlSvr,vlPath); /* Destroy path, free memory it used */

vlDestroyBuffer(vlSvr, transferBuf); /* Destroy ringbuffer, free memory */

vlCloseVideo(vlSvr); /* Disconnect from the daemon */
}


/**********************************************************************
cleanCL

   Shutdown compressor and disassociate buffers.
**********************************************************************/
void
cleanCL()
{
CC(clDestroyBuf(dataHdl) );

Verbose("cleanCL: closing compressor\n");
clCloseCompressor(compressorHdl);
}


/**********************************************************************
cleanMV

   Close the movie library, which will complete pending operations to
open files.  Also searches movie file for dropped frame pointers, 
replacing those with copies of the last good frame.
**********************************************************************/
void
cleanMV()
{
int	numFrames;      /* number of frames in movie file */
int	frameIndex;     /* current frame index            */
int	dropCount= 0;
int	size;		/* size of last good frame        */
int	newSize;	/* size of field2 repeated frame  */

char 	*origBuffer;        /* dynamicaly sized buffer        */
int 	origBufferSize= 0;
char 	*newBuffer;        /* dynamicaly sized buffer        */
int 	newBufferSize= 0;
int	lastReplicated= -1; /* keep track of consecutive drops */

/* replicate dropped frames in movie file */ 
Verbose("cleanMV replicating dropped frames\n");

numFrames= mvGetTrackLength(outTrack);
for(frameIndex= 0; frameIndex < numFrames; frameIndex++)
   {
   if(mvGetCompressedImageSize(outTrack, frameIndex) == sizeof(dummyFrame))
      { /* we got another frame drop marker */
      if(lastReplicated==(frameIndex-1))
	 { /* OK, this is easy, just insert the already built frame */
	 MC(mvDeleteFrames(outTrack, frameIndex, 1) );
	 MC(mvInsertCompressedImage(outTrack, frameIndex, newSize, newBuffer) );
	 }
      else
	 { /* this is a new drop */
	 MC(mvReadCompressedImage(outTrack, frameIndex,
	    sizeof(dummyFrame), &dummyFrame) );
	 newSize= 2*dummyFrame.field2size; /* size of  newBuffer w/2 field2's */
 
	 size= mvGetCompressedImageSize(outTrack, frameIndex-1);

	 /* make sure the marker data corresponds to the same last goodframe */
	 assert(size==(dummyFrame.field1size+dummyFrame.field2size));

	 if(origBufferSize < size) /* dynamicaly size buffer as needed */
	    {
	    if(origBufferSize)
	       free(origBuffer);
	    origBuffer= malloc(size);
	    origBufferSize= size;
	    }

	 /* get the original good frame */
	 MC(mvReadCompressedImage(outTrack, frameIndex-1, size, origBuffer) );

	 /* create a new buffer w/field2 duplicated */
	 if(newBufferSize < newSize) /* dynamicaly size buffer */
	    {
	    if(newBufferSize)
	       free(newBuffer);
	    newBuffer= malloc(newSize);
	    newBufferSize= newSize;
	    }

	 /* copy field2 into field1 of new buffer */
	 bcopy(origBuffer + dummyFrame.field1size, /* from beg of field 2 */
	       newBuffer,                          /* to new field1       */
	       dummyFrame.field2size);             /* move all of field 2 */

	 /* copy field2 into field2 of new buffer */
	 bcopy(origBuffer + dummyFrame.field1size, /* from beg of field 2 */
	       newBuffer + dummyFrame.field2size,  /* to new field2       */
	       dummyFrame.field2size);             /* move all of field 2 */

	 MC(mvDeleteFrames(outTrack, frameIndex, 1) );
	 MC(mvInsertCompressedImage(outTrack, frameIndex, newSize, newBuffer) );
	 } /* new dupe loop */

      lastReplicated= frameIndex;
      dropCount++;
      } /* end of drop handler */
   } /* end of frame interation */
Verbose("%d frames dropped out of %d total\n", dropCount, numFrames);

Verbose("cleanMV: Closing movie file\n");
mvClose(sfd);
}


/**********************************************************************
cleanGL

   Close the window.
**********************************************************************/
void
cleanGL()
{
Verbose("cleanGL: Closing the window\n");
winclose(glWindowHandle);
}


/**********************************************************************
gotErr

   This routine is called from err macros, or when it is desired to
shut the program down.  If the running flag indicates that we are 
currently transfering data an orderly shutdown is initiated by setting
stopShovelLoop, waiting for shovel to exit, then setting stopVLloop
to stop the processVL loop.  processVL calls cleanupExit upon exiting.

If the running flag was not set, gotErr calls cleanup immediatly.
**********************************************************************/
int
gotErr()
{
int timeout= TIMEOUT;
static int enterCount= 0;

if(enterCount++)
   {
   Verbose("gotErr entered %d (returning)\n", enterCount);
   return(0);
   }

if(!running)
   cleanupExit();
else
   {
   /* shutdown 1st the shovel process, then the vl loop */
Verbose("gotErr: setting stopShovelLoop\n");
   stopShovelLoop= 1;
   while((!shovelFinished)&&(timeout--))
      sginap(1);
   if(!timeout)
      Verbose("gotErr: timed out waiting for shovel to exit\n");

Verbose("gotErr: setting stopVLloop\n");
   stopVLloop= 1;
   }

}


/**********************************************************************
cleanupExit

close and exit gracefully from all libraries and files, then exit.
**********************************************************************/
void
cleanupExit()
{
int timeout= TIMEOUT;

Verbose("cleanupExit entered\n");

/* rendezvous */
while(((!shovelFinished) || (!processVLfinished)) && timeout--)
   sginap(1);
if(!timeout)
   Verbose("cleanup: rendezvous timed out\n");

/* now shut the machine down */
sigignore(SIGCHLD);
if(shovelId) 
   kill(shovelId, SIGTERM);  /* kill it in case it's hanging around */

cleanVL();

if(compressVideo)
   cleanCL();

if(capture)
   cleanMV();

if(displayVideo)
   cleanGL();

exit(0);
}


/**********************************************************************
processGlEvent

Handle graphics library events
**********************************************************************/
void
processGlEvent()
{
static short val;

if(qtest())
   {
   switch(qread(&val))
      {
      /* Toggle double buffer mode */
      case DKEY:
	 if(val == 1) /* Get key downs only */
	    {
	    if(double_buffer= 1 - double_buffer) 
	       {
	       doublebuffer();
	       Verbose("double buffer mode\n");
	       }
	    else 
	       {
	       singlebuffer();
	       Verbose("single buffer mode\n");
	       }
	    /* Set up the graphics subsystem */
	    gconfig();
	    }
      break;

      /* Quit */
      case QKEY :
	 if(val != 1) /* Ignore key releases */
	    break;

      case WINSHUT:
      case WINQUIT:
	 gotErr();        /* window closed, exit cleanly */

      default: break;
      }
   }
}


/**********************************************************************
processVLevent

This routine loops, handling vl events (only transfer complete events 
in this example), until asked to stop.

processVLevent is also responsible for catching SIGINT signals, and
starting a polite shutdown in that case.
**********************************************************************/
void
processVLevents()
{
DMediaInfo	*dmInfo;
VLInfoPtr	info;
char		*dataPtr;
int		compressedDataSize1, compressedDataSize2;
sigset_t	sigset;
VLEvent		ev;
int		fieldcount= 0;
DMediaInfo	*fieldInfo;
int 		lastFieldId= 0;
int		count;
int		dropped= 0;

while(!stopVLloop)
   {
   /* check to see if a SIGINT (^C) has occurred */
   if(sigpending(&sigset))           /* get pending signal list */
      perror("sigpending");
   if(sigismember(&sigset, SIGINT))  /* check if SIGINT is in list */
      { /* Got a SIGINT begin orderly shutdown by calling gotErr */
      Verbose("processVLevents: found pending SIGINT\n");

      gotErr();
      }

   processGlEvent(); /* process a gl event if any */

   vlNextEvent(vlSvr, &ev); /* block until a vl event is available */

   /* empty all other outstanding events */
   while(!vlCheckEvent(vlSvr, VLAllEventsMask, &ev));

   switch(ev.reason)
      {
      case VLTransferComplete:
	 /* get info on the latest frame (and drop the rest!) */
	 info= vlGetLatestValid(vlSvr, transferBuf);
	 if(!info)
	    break;

	 /* investigate the field's timestamp! */
	 fieldInfo= vlGetDMediaInfo(vlSvr, transferBuf, info);

	 if(lastFieldId)
	    { /* lastFieldId!=0 (not 1st time) */
	    /* compute dropped fields */
	    dropped= fieldInfo->sequence - lastFieldId;

	    /* inc dropSema counter (XXX is there a better way?) */
	    for(count= 0; count < dropped; count++)
	       usvsema(dropSema);
	    }

	 lastFieldId= fieldInfo->sequence;
	 
	 framenumber++;

	 /* Get the valid video data from that frame */
	 VNC(dataPtr= vlGetActiveRegion(vlSvr, transferBuf, info) );

	 if(compressVideo)
	    {
	    if(cosmoQueuemode)
	       {
	       char *clPtr;
	       int wrap, free; /* set by clQueryFree call */

	       /* get the pointer to the next frame in the cl input buffer */
	       CC(free= clQueryFree(frameHdl, 0, (void **)&clPtr, &wrap) );
	       if(free > 0)
		  {
		  /* copy vid since the cl,vl don't use a common rb format */
		  bcopy(dataPtr, clPtr, xVidSize * yVidSize * 4);
		  CC(clUpdateHead(frameHdl, 1) ); /* notify cl of field */
		  }
	       }
	    else /* single frame mode */
	       {
	       if(fieldcount==0)
		  { /* stick 1st field at beginning of buffer */
		  CC(clCompress(compressorHdl, 1, dataPtr, 
		     &compressedDataSize1, cbuffer) ); 
		  fieldcount++;
		  }
	       else
		  {
		  /* set the 2nd buffer to where the 1st field left off */
		  cbuffer2= cbuffer + compressedDataSize1;

		  CC(clCompress(compressorHdl, 1, dataPtr, 
		     &compressedDataSize2, cbuffer2) ); 

		  /* insert the fields into the movie file */
		  if(capture)
		     MC(mvInsertCompressedImage(outTrack, frame++, 
			compressedDataSize1+compressedDataSize2, cbuffer) ); 

		  fieldcount= 0;   /* reset fieldcount */
		  }
	       }
	    }

	 if(displayVideo)    /* write raw frame to window */
	    {
	    lrectwrite(0, 0, xVidSize - 1, yVidSize - 1, (ulong *)dataPtr);

	    if(double_buffer)
	       swapbuffers();
	    }

	 /* Done with that frame, free the memory used by it */
	 VC(vlPutFree(vlSvr, transferBuf) );

	 if(framenumber >= totalCount)
	    stopVLloop= 1; /* got all the frames requested, exit cleanly */
      break;


      case VLTransferFailed:    gotErr(); break;

      case VLStreamPreempted: 
	 fprintf(stderr, "%s: Stream preempted by another Prog\n", _progName);
	 gotErr();
      break;

      default: printf("Got Event %d\n", ev.reason); break;
      }
   }
Verbose("processVLevents: FINISHED (setting processVLfinished)\n");
processVLfinished= 1;
}


void
int_handler(int sig)
{
Verbose("int_handler: pid %d entered handler due to signal %d\n",
   getpid(), sig);
}


/**********************************************************************
**********************************************************************/
main(int argc, char **argv)
{
char		*xloc;
int		c;
int		len, xlen, ylen;
VLControlValue	val;
int		compressedDataSize;
sigset_t        sigset;
struct		sigaction act;
int		timeout= TIMEOUT;  /* timeout to exit */

_progName= argv[0];    /* set the program name global               */

parseargs(argc, argv); /* Parse command line args to set mode flags */



initSema();

/* set up SIGINT handler */
act.sa_handler= (void (*)(int))int_handler;
sigemptyset(&act.sa_mask);
act.sa_flags= 0;
sigaction(SIGINT, &act, NULL);

/* block SIGINT from being delivered; some of the library
   routines don't seem to like it, even if it's caught. */
sigemptyset(&sigset);
sigaddset(&sigset, SIGINT);
if(sigprocmask(SIG_BLOCK, &sigset, NULL))
   perror("sigprocmask");





initVL();       /* setup video to memory transfer       */

if(compressVideo)
   initCL();    /* setup memory to memory compressor    */

if(displayVideo)
   initGL();    /* setup graphics window                */

if(capture)
   initMovie(); /* setup movie file with an image track */

activateVL();   /* start video transfers                */


if(cosmoQueuemode && compressVideo)
   {
   running= 1;    /* flag gotErr that the time for simple shutdown is over */
   Verbose("Starting clCompress continuous\n");
   shovelId= sproc((void (*)(void *)) shovelDataToMovie, PR_SADDR);

   clCompress(compressorHdl, CL_CONTINUOUS_NONBLOCK,
      NULL, &compressedDataSize, NULL);
   compressing= 1;  /* allow the reader to unblock */
   }

processVLevents(); /* loops, processing vlEvents unless it catched SIGINT */

if(stopVLloop)    /* if we got here due to stopVLloop being set           */
   {
   stopShovelLoop= 1; /* attempt to stop the shovel proccess */
   while(!shovelFinished && timeout--) /* wait for shovel to exit cleanly */
      sginap(1);
   }
if(!timeout)
   Verbose("main: timed-out waiting for shovel to exit, cleaning up anyway\n");

cleanupExit();     /* processVLevents returned, cleanup!                  */
}
