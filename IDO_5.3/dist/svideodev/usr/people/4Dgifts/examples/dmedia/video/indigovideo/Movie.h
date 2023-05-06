/*
 *	Movie.h -- Public include file for movies.
 */

typedef struct MovieHeader {
    long	movie_id;        /* magic number for movie  'MOVI' */
    short	version;         /* Currently 2 */
    short	frameWidth;      /* X size of the movie frames. */
    short	frameHeight;     /* Y size of the movie frames. */
    double	framesPerSec;    /* Frame rate of the movie. */
    short	movieType;       /* SOUND or SILENT - see #define below */
    short 	showType;        /* MOVIE or SLIDE_SHOW - see #define below */
    long	frameCount;      /* Number of frames in the movie */
    long 	image_format;	 /* See #defines below */ 
    long	imageXsize;      /* x size of libil image */
    long	imageYsize;      /* y size of libil image */
    long	imageCsize;      /* c size (number of channels) of libil image*/
    long	imageDatatype;   /* ilType of data in libil image */
    long	loop_mode;       /* For movieplayer use. See #defines below */
    long	audioSampleRate; /* Sample rate of the sound track */
    long	audioSampleSize; /* Size of audio samples in the sound track */
    long	audioSampleFormat;/* Format of the data in the audio samples */
    long	audioNumChannels; /* Number of channels in the sound track */
    long	audioCompression; /* Audio compression type used */
    long	default_volume;   /* For movieplayer use, but not used */
    char	title[128];       /* Title of the movie */
    char	comments[128];    /* Comments about the movie, or whatever */
    char	field_extensions[256]; /* Space for additional features. */
} MovieHeader ;

typedef struct DirectoryEntry {
    long	location;       /* absolute file position of start of audio  */
    long	asize;		/* byte size of audio frame */
    long	vsize;		/* bytes size of video frame */
    long        displayTime;    /* for slide shows seconds to display image */
    long	frame_flags;	/* DSM's idea. Currently unused. */
} DirectoryEntry ;

#define MOVIE_ID		'MOVI' 
#define MOVIE_ID_OLD		0x00010400	
#define MOVIE_VERSION_1		1
#define MOVIE_VERSION_2		2
#define MOVIE           	1   /* For MovieHeader.showType */ 
#define SLIDE_SHOW      	2   /* For MovieHeader.showType */
#define SOUND           	1   /* For MovieHeader.movieType */
#define SILENT          	2   /* For MovieHeader.movieType */
#define IMAGE_FORMAT_MVC1	1   /* CL_MVC1 compressed image data */
#define IMAGE_FORMAT_FIT	2   /* Uncompressed image data */
#define IMAGE_FORMAT_CMAP_RLE	3   /* Not implemented */
#define IMAGE_FORMAT_MPEG	4   /* MPEG compressed movie. */
#define IMAGE_FORMAT_8RGB       5   /* Indigo 8 bit RGB image data. */

#define LOOP_NONE		0   /* For MovieHeader.loop_mode */
#define LOOP_CONTINUOUSLY	1   /* For MovieHeader.loop_mode */
#define LOOP_SWINGING		2   /* For MovieHeader.loop_mode */

#define FRAME_FLAG_ISKEY	1
