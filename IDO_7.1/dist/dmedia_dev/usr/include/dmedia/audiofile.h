#ifndef __INC_AUDIOFILE_H__
#define __INC_AUDIOFILE_H__ 

/***************************************************************************
 * SGI Audio File Library
 *
 * audiofile.h
 *    header file for use with /usr/lib/libaudiofile.so
 ***************************************************************************
 * 
 * Copyright 1992, Silicon Graphics, Inc.
 * All Rights Reserved.
 *
 * This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
 * the contents of this file may not be disclosed to third parties, copied or
 * duplicated in any form, in whole or in part, without the prior written
 * permission of Silicon Graphics, Inc.
 *
 * RESTRICTED RIGHTS LEGEND:
 * Use, duplication or disclosure by the Government is subject to restrictions
 * as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
 * and Computer Software clause at DFARS 252.227-7013, and/or in similar or
 * successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
 * rights reserved under the Copyright Laws of the United States.
 *
 ****************************************************************************/

#include <dmedia/dm_audio.h>    /* depend on DM_AUDIO_ for SAMPFMT #defines */
#include <sys/types.h>		/* needed for off_t type */

#ident "$Revision: 1.47 $"

#define LIBAUDIOFILE_VERSION 2

#ifdef __cplusplus
extern "C" {
#endif

/* macros to allow backwards-compatibility */

#ifndef __AF_NO_BACKWARDS_COMPAT__

#define AFseterrorhandler		afSetErrorHandler
#define AFquery				afQuery
#define AFquerylong			afQueryLong
#define AFquerydouble			afQueryDouble
#define AFqueryptr			afQueryPointer
#define AFnewfilesetup			afNewFileSetup
#define AFfreefilesetup			afFreeFileSetup
#define AFidentifyfd			afIdentifyFD
#define AFopenfile			afOpenFile
#define AFopenfd			afOpenFD
#define AFgetfd				afGetFD
#define AFsavefilepos			afSaveFilePosition
#define AFrestorefilepos		afRestoreFilePosition
#define AFsyncfile			afSyncFile
#define AFclosefile			afCloseFile
#define AFinitfilefmt			afInitFileFormat
#define AFgetfilefmt			afGetFileFormat
#define AFinittrackids			afInitTrackIDs
#define AFgettrackids			afGetTrackIDs
#define AFreadframes			afReadFrames
#define AFwriteframes			afWriteFrames
#define AFseekframe			afSeekFrame
#define AFtellframe			afTellFrame
#define AFgetframesize			afGetFrameSize
#define AFgetvirtualframesize		afGetVirtualFrameSize
#define AFinitaeschanneldata		afInitAESChannelData
#define AFinitaeschanneldatato		afInitAESChannelDataTo
#define AFgetaeschanneldata		afGetAESChannelData
#define AFsetaeschanneldata		afSetAESChannelData
#define AFinitbyteorder			afInitByteOrder
#define AFgetbyteorder			afGetByteOrder
#define AFsetvirtualbyteorder		afSetVirtualByteOrder
#define AFgetvirtualbyteorder		afGetVirtualByteOrder
#define AFinitchannels			afInitChannels
#define AFgetchannels			afGetChannels
#define AFsetvirtualchannels		afSetVirtualChannels
#define AFgetvirtualchannels		afGetVirtualChannels
#define AFsetchannelmatrix	        afSetChannelMatrix
#define AFinitsampfmt			afInitSampleFormat
#define AFgetsampfmt			afGetSampleFormat
#define AFsetvirtualsampfmt		afSetVirtualSampleFormat
#define AFgetvirtualsampfmt		afGetVirtualSampleFormat
#define AFinitrate			afInitRate
#define AFgetrate			afGetRate
#define AFinitcompression		afInitCompression
#define AFinitcompressionparams		afInitCompressionParams
#define AFgetcompression		afGetCompression
#define AFgetcompressionparams		afGetCompressionParams
#define AFgetcompressionname		afGetCompressionName
#define AFinitpcmmapping		afInitPCMMapping
#define AFgetpcmmapping			afGetPCMMapping
#define AFsettrackpcmmapping		afSetTrackPCMMapping
#define AFsetvirtualpcmmapping		afSetVirtualPCMMapping
#define AFgetvirtualpcmmapping		afGetVirtualPCMMapping
#define AFinitdataoffset		afInitDataOffset
#define AFgetdataoffset			afGetDataOffset
#define AFinitframecnt			afInitFrameCount
#define AFgetframecnt			afGetFrameCount
#define AFinitinstids			afInitInstIDs
#define AFgetinstids			afGetInstIDs
#define AFsetinstparams			afSetInstParams
#define AFgetinstparams			afGetInstParams
#define AFsetinstparamlong		afSetInstParamLong
#define AFgetinstparamlong		afGetInstParamLong
#define AFinitloopids			afInitLoopIDs
#define AFgetloopids			afGetLoopIDs
#define AFsetloopmode			afSetLoopMode
#define AFgetloopmode			afGetLoopMode
#define AFsetloopstart			afSetLoopStart
#define AFgetloopstart			afGetLoopStart
#define AFsetloopend			afSetLoopEnd
#define AFgetloopend			afGetLoopEnd
#define AFsetlooptrack			afSetLoopTrack
#define AFgetlooptrack			afGetLoopTrack
#define AFinitmarkids			afInitMarkIDs
#define AFgetmarkids			afGetMarkIDs
#define AFsetmarkpos			afSetMarkPosition
#define AFgetmarkpos			afGetMarkPosition
#define AFinitmarkname			afInitMarkName
#define AFgetmarkname			afGetMarkName
#define AFinitmiscids			afInitMiscIDs
#define AFgetmiscids			afGetMiscIDs
#define AFinitmisctype			afInitMiscType
#define AFgetmisctype			afGetMiscType
#define AFinitmiscsize			afInitMiscSize
#define AFgetmiscsize			afGetMiscSize
#define AFwritemisc			afWriteMisc
#define AFreadmisc			afReadMisc
#define AFseekmisc			afSeekMisc

#endif

/*
 * basic data structures
 */

typedef struct _AFfilesetup  *AFfilesetup;  
typedef struct _AFfilehandle *AFfilehandle;
typedef void (*AFerrfunc)(long,const char*);

/*
 * types for frame count (in frames) and file offset (in bytes)
 */

typedef off_t AFframecount;
typedef off_t AFfileoffset;

#define AF_NULL_FILESETUP ((struct _AFfilesetup *)0)
#define AF_NULL_FILEHANDLE  ((struct _AFfilehandle *)0)

/*
 * Audio Utility Library parameter-value lists
 */
#include <audioutil.h>

/*
 * for now, use this as the track id in all AF library
 * calls - all audio file formats supported by the current library 
 * contain exactly one audio sample data chunk
 */
#define AF_DEFAULT_TRACK 1001
/*
 * for now, use this value wherever an instrument id is required
 *   as an argument to one of the library functions below
 */
#define AF_DEFAULT_INST 2001

/* global routines */
extern AFerrfunc  afSetErrorHandler(AFerrfunc efunc);
extern AUpvlist afQuery		(int querytype, 
				 int arg1, int arg2, int arg3, int arg4);
extern long     afQueryLong	(int querytype, 
				 int arg1, int arg2, int arg3, int arg4);
extern double   afQueryDouble	(int querytype, 
                                 int arg1, int arg2, int arg3, int arg4);
extern void *   afQueryPointer	(int querytype, 
				 int arg1, int arg2, int arg3, int arg4);
                           
/* basic operations on filehandle and filesetup */
extern AFfilesetup  afNewFileSetup(void);
extern void afFreeFileSetup(AFfilesetup);
extern int afIdentifyFD(int);
extern int afIdentifyNamedFD(int, const char *filename, int* implemented);

extern AFfilehandle afOpenFile    (const char *fileName, const char *mode, 
                                   AFfilesetup fileSetUp);
extern AFfilehandle afOpenFD      (int fd, const char *mode, 
                                   AFfilesetup fileSetUp);
extern AFfilehandle afOpenNamedFD (int fd, const char *mode, 
                                   AFfilesetup fileSetUp,
				   const char *filename);

extern int  afGetFD		    (AFfilehandle);
extern void afSaveFilePosition	    (AFfilehandle);
extern void afRestoreFilePosition   (AFfilehandle);
extern int afSyncFile		    (AFfilehandle);
extern int afCloseFile		    (AFfilehandle);                 

extern void afInitFileFormat        (AFfilesetup, int format);
extern int afGetFileFormat          (AFfilehandle, int *version);

/* track */
extern void afInitTrackIDs          (AFfilesetup,  int *trackids, int trackCount);
extern int afGetTrackIDs            (AFfilehandle, int *trackids);

/* track data: reading, writing, seeking, sizing frames */
extern int afReadFrames             (AFfilehandle, int track, void *buf, int frameCount);
extern int afWriteFrames            (AFfilehandle, int track, void *buf, int frameCount); 
extern AFframecount afSeekFrame	    (AFfilehandle, int track, AFframecount frame);
extern AFfileoffset afTellFrame	    (AFfilehandle, int track);
extern AFfileoffset afGetTrackBytes (AFfilehandle, int track);
extern float afGetFrameSize	    (AFfilehandle, int track, int stretch3to4);
extern float afGetVirtualFrameSize  (AFfilehandle, int track, int stretch3to4);

/* track data: AES data */

/* afInitAESChannelData obsolete -- use afInitAESChannelDataTo() */
extern void afInitAESChannelData    (AFfilesetup, int track); /*obsolete*/
extern void afInitAESChannelDataTo  (AFfilesetup, int track, int willBeData);
extern int afGetAESChannelData	    (AFfilehandle,int track,unsigned char buf[24]);
extern void afSetAESChannelData	    (AFfilehandle,int track,unsigned char buf[24]);

/* track data: byte order */
extern void afInitByteOrder	    (AFfilesetup, int track, int byteOrder);
extern int afGetByteOrder	    (AFfilehandle, int track);
extern int afSetVirtualByteOrder    (AFfilehandle, int track, int byteOrder);
extern int afGetVirtualByteOrder    (AFfilehandle, int track);

/* track data: # channels */
extern void afInitChannels	    (AFfilesetup, int track, int nchannels);
extern int afGetChannels	    (AFfilehandle, int track); 
extern int afSetVirtualChannels     (AFfilehandle, int track, int channelCount);
extern int afGetVirtualChannels     (AFfilehandle, int track);
extern void afSetChannelMatrix	    (AFfilehandle, int track, double *matrix);

/* track data: sample format and sample width */
extern void afInitSampleFormat	    (AFfilesetup, int track, 
				     int sampleFormat, int sampleWidth);
extern void afGetSampleFormat	    (AFfilehandle file, int track, 
				     int *sampleFormat, int *sampleWidth);
extern int afSetVirtualSampleFormat (AFfilehandle, int track, 
				     int sampleFormat, int sampleWidth);
extern void afGetVirtualSampleFormat(AFfilehandle, int track, 
				     int *sampleFormat, int *sampleWidth);

/* track data: sampling rate */
extern void   afInitRate(AFfilesetup, int track, double rate);
extern double afGetRate (AFfilehandle, int track);

/* track data: compression */
extern void afInitCompression		(AFfilesetup, int track, int compression);
extern void afInitCompressionParams	(AFfilesetup, int track, 
					    int compression, 
					    AUpvlist params, int parameterCount);
extern int afGetCompression		(AFfilehandle, int track);
extern void afGetCompressionParams	(AFfilehandle file, int track, 
					    int *compression,
					    AUpvlist params, int parameterCount);
/* this is obsolete--use afQuery() */
extern char *afGetCompressionName	(AFfilehandle, int track);/*obsolete*/

/* track data: pcm mapping */
extern void afInitPCMMapping	(AFfilesetup filesetup, int track,
                             double slope, double intercept, 
                             double minClip, double maxClip);
extern void afGetPCMMapping	(AFfilehandle file, int track,
                            double *slope, double *intercept, 
                            double *minClip, double *maxClip);
/* NOTE: afSetTrackPCMMapping() is special--it does not set the virtual     */
/* format; it changes what the AF thinks the track format is! Be careful.   */
extern int afSetTrackPCMMapping(AFfilehandle file, int track,
                                 double slope, double intercept,
                                 double minClip, double maxClip);
/* NOTE: afSetVirtualPCMMapping() is different than afSetTrackPCMMapping(): */
/* see comment for afSetTrackPCMMapping().                                  */
extern int afSetVirtualPCMMapping(AFfilehandle file, int track,
                                   double slope, double intercept,
                                   double minClip, double maxClip);
extern void afGetVirtualPCMMapping(AFfilehandle file, int track,
                                   double *slope, double *intercept, 
                                   double *minClip, double *maxClip);

/* track data: data offset within the file (init for raw reading only) */
extern void afInitDataOffset(AFfilesetup, int track, AFfileoffset offset); /*raw read only*/
extern AFfileoffset afGetDataOffset (AFfilehandle, int track);

/* track data: count of frames in file (init for raw only) */
extern void afInitFrameCount(AFfilesetup, int track, AFframecount frameCount); /*raw read only*/
extern AFframecount afGetFrameCount (AFfilehandle, int track);

/* AIFF instrument data chunk operations */
extern void afInitInstIDs   (AFfilesetup, int *ids, int nids);
extern int afGetInstIDs    (AFfilehandle, int *ids);
extern void afSetInstParams (AFfilehandle, int instrumentID, 
				AUpvlist params, int parameterCount);
extern void afGetInstParams (AFfilehandle, int instrumentID, 
				AUpvlist params, int parameterCount);

extern void afSetInstParamLong(AFfilehandle, int instrumentID, int paramater, long val);
extern long afGetInstParamLong(AFfilehandle, int instrumentID, int parameter);

/* AIFF loop operations */
extern void afInitLoopIDs (AFfilesetup, int instrumentID, int *ids, int nids);
extern int afGetLoopIDs  (AFfilehandle, int instrumentID, int *ids); 
extern void afSetLoopMode (AFfilehandle, int instrumentID, int loop, int mode);
extern int afGetLoopMode (AFfilehandle, int instrumentID, int loop);
extern void afSetLoopStart(AFfilehandle, int instrumentID, int loop, int markerID);
extern int afGetLoopStart(AFfilehandle, int instrumentID, int loop);
extern void afSetLoopEnd  (AFfilehandle, int instrumentID, int loop, int markerID);
extern int afGetLoopEnd  (AFfilehandle, int instrumentID, int loop);
extern void afSetLoopTrack(AFfilehandle, int instrumentID, int loop, int trackid);
extern int afGetLoopTrack(AFfilehandle, int instrumentID, int loop);

/* AIFF marker operations */
extern void afInitMarkIDs    (AFfilesetup, int trackID, int *ids, int nids);
extern int afGetMarkIDs     (AFfilehandle, int trackID, int *ids);
extern void afSetMarkPosition(AFfilehandle, int trackID, int marker, AFframecount pos);
extern AFframecount afGetMarkPosition(AFfilehandle, int trackID, int marker);
extern void afInitMarkName   (AFfilesetup, int trackID, int marker, const char *name);
extern char *afGetMarkName   (AFfilehandle, int trackID, int marker);

/* AIFF miscellaneous data chunk operations */
extern void afInitMiscIDs (AFfilesetup,  int *ids, int nids);
extern int afGetMiscIDs  (AFfilehandle, int *ids);
extern void afInitMiscType(AFfilesetup,  int miscellaneousID, int type);
extern int afGetMiscType (AFfilehandle, int miscellaneousID);
extern void afInitMiscSize(AFfilesetup,  int miscellaneousID, int size);
extern int afGetMiscSize (AFfilehandle, int miscellaneousID);
extern int afWriteMisc   (AFfilehandle, int miscellaneousID, void *buf, int bytes);
extern int afReadMisc    (AFfilehandle, int miscellaneousID, void *buf, int bytes);
extern int afSeekMisc    (AFfilehandle, int miscellaneousID, int offset);

/* sample formats */ 

/* Note that these values are pulled from <dmedia/dm_audio.h>. 
 * As new SAMPFMT's are supported, their names will be rolled into
 * DMaudioformat, as defined by dm_audio.h. 
 */
#define AF_SAMPFMT_TWOSCOMP    ((int)DM_AUDIO_TWOS_COMPLEMENT)  /* linear two's complement */
#define AF_SAMPFMT_UNSIGNED    ((int)DM_AUDIO_UNSIGNED)         /* unsigned integer */
#define AF_SAMPFMT_FLOAT       ((int)DM_AUDIO_FLOAT)            /* 32-bit IEEE float */
#define AF_SAMPFMT_DOUBLE      ((int)DM_AUDIO_DOUBLE)           /* 64-bit IEEE double */

/* byte orders */
#define AF_BYTEORDER_BIGENDIAN	    501  /* big endian, SGI MIPS / Motorola order */
#define AF_BYTEORDER_LITTLEENDIAN   502  /* little endian, Intel order */

/* file formats */
#define AF_FILE_UNKNOWN		-1
#define AF_FILE_UNSUPPORTED	-2 /*obsolete and unused*/
#define AF_FILE_RAWDATA		0
#define AF_FILE_AIFFC		1
#define AF_FILE_AIFF		2
#define AF_FILE_NEXTSND		3
#define AF_FILE_WAVE		4
#define AF_FILE_BICSF		5
#define AF_FILE_MPEG1BITSTREAM  6
#define AF_FILE_SOUNDESIGNER1	7
#define AF_FILE_SOUNDESIGNER2	8

/* for full compatibility */
#define AF_FILE_IRCAM		AF_FILE_BICSF

/* loop modes */
#define AF_LOOP_MODE_NOLOOP	0
#define AF_LOOP_MODE_FORW	1
#define AF_LOOP_MODE_FORWBAKW	2

/* instrument parameters */
#define AF_INST_MIDI_BASENOTE      301   
#define AF_INST_NUMCENTS_DETUNE    302   
#define AF_INST_MIDI_LONOTE        303
#define AF_INST_MIDI_HINOTE        304
#define AF_INST_MIDI_LOVELOCITY    305
#define AF_INST_MIDI_HIVELOCITY    306
#define AF_INST_NUMDBS_GAIN        307
#define AF_INST_SUSLOOPID          308 /* loop id for sustain loop */
#define AF_INST_RELLOOPID          309 /* loop id for release loop */

/* miscellaneous data sections */
/* AIFF-constants -- for backwards compatibility */
#define AF_MISC_AIFF_UNRECOGNIZED   0  /* unrec. data chunk */ /*obsolete*/
#define AF_MISC_AIFF_COPY         201  /* copyright string */ /*obsolete*/
#define AF_MISC_AIFF_AUTH         202  /* author string */ /*obsolete*/
#define AF_MISC_AIFF_NAME         203  /* name string */ /*obsolete*/
#define AF_MISC_AIFF_ANNO         204  /* annotation string */ /*obsolete*/
#define AF_MISC_AIFF_APPL         205  /* app-specific data */ /*obsolete*/
#define AF_MISC_AIFF_MIDI         206  /* MIDI exclusive data */ /*obsolete*/

#define AF_MISC_UNRECOGNIZED	  0    /* unrecognized data chunk */
#define AF_MISC_COPY		201  /* copyright string */
#define AF_MISC_AUTH		202  /* author string */
#define AF_MISC_NAME		203  /* name string */
#define AF_MISC_ANNO		204  /* annotation string */
#define AF_MISC_APPL		205  /* application-specific data */
#define AF_MISC_MIDI		206  /* MIDI exclusive data */

#define AF_MISC_PCMMAP		207  /* PCM mapping information (future use) */
#define AF_MISC_NeXT		208  /* misc binary data appended to NeXT hdr */
#define AF_MISC_IRCAM_PEAKAMP	209  /* peak amplitude information */
#define AF_MISC_IRCAM_COMMENT	210  /* text comment */

/* compression schemes supported by SGI Audio File Library */
#define AF_COMPRESSION_UNKNOWN          -1
#define AF_COMPRESSION_NONE             0
#define AF_COMPRESSION_G722             501
#define AF_COMPRESSION_G711_ULAW        502
#define AF_COMPRESSION_G711_ALAW        503

/* Apple proprietary AIFF-C compression schemes: not supported by SGI 
 *     Audio File Library */
#define AF_COMPRESSION_APPLE_ACE2       504
#define AF_COMPRESSION_APPLE_ACE8       505
#define AF_COMPRESSION_APPLE_MAC3       506
#define AF_COMPRESSION_APPLE_MAC6       507

#define AF_COMPRESSION_MPEG1            515
#define AF_COMPRESSION_AWARE_MULTIRATE  516

/* provided for backwards compatibility with old libaudiofile */
#define AF_COMPRESSION_AWARE_MPEG       AF_COMPRESSION_MPEG1

/* handy defaults for afInitCompression */
#define AF_COMPRESSION_DEFAULT_MPEG_I             508
#define AF_COMPRESSION_DEFAULT_MPEG1_LAYERI       AF_COMPRESSION_DEFAULT_MPEG_I
#define AF_COMPRESSION_DEFAULT_MPEG_II            509
#define AF_COMPRESSION_DEFAULT_MPEG1_LAYERII      AF_COMPRESSION_DEFAULT_MPEG_II
#define AF_COMPRESSION_AWARE_DEFAULT_MULTIRATE	  513
#define AF_COMPRESSION_AWARE_DEFAULT_LOSSLESS	  514


/* provided for backwards compatibility */
#define AF_COMPRESSION_AWARE_DEFAULT_MPEG_I	  AF_COMPRESSION_DEFAULT_MPEG_I
#define AF_COMPRESSION_AWARE_DEFAULT_MPEG1_LAYERI AF_COMPRESSION_DEFAULT_MPEG_I 
#define AF_COMPRESSION_AWARE_DEFAULT_MPEG_II	  AF_COMPRESSION_DEFAULT_MPEG_II
#define AF_COMPRESSION_AWARE_DEFAULT_MPEG1_LAYERII AF_COMPRESSION_DEFAULT_MPEG_II

/* Compression Parameters for MPEG Compression */

#define AF_MPEG_PARAM_LAYER                530000 /* algorithm layer */
#define     AF_MPEG_LAYER_I                530001 /* .. mpeg 1 layer 1 */
#define     AF_MPEG_LAYER_II               530002 /* .. mpeg 1 layer 2 */

#define AF_MPEG_PARAM_BITRATE_TARGET       511000 /* bitrate target */

#define AF_MPEG_PARAM_CONST_QUAL_NMR       514000 /* const qual NMR */

#define AF_MPEG_PARAM_BITRATE_POLICY       517000 /* bitrate policy */
#define     AF_MPEG_FIXED_RATE             517001 /* .. fixed rate */
#define     AF_MPEG_CONST_QUAL             517002 /* .. constant quality */

#define AF_MPEG_PARAM_CHANNEL_POLICY       510000 /* channel policy */
#define     AF_MPEG_JOINT_STEREO           510001 /* .. joint stereo */
#define     AF_MPEG_STEREO                 510002 /* .. stereo */
#define     AF_MPEG_INDEPENDENT            510003 /* .. independent */

/* Compression Parameters for Aware MultiRate */

#define AF_AWARE_PARAM_CHANNEL_POLICY      510000 /* bitrate policy */
#define     AF_AWARE_STEREO                510002 /* .. stereo */
#define     AF_AWARE_INDEPENDENT           510003 /* .. independent */

#define AF_AWARE_PARAM_BITRATE_POLICY      517000 /* bitrate policy */
#define     AF_AWARE_CONST_QUAL            517002 /* .. constant quality */
#define     AF_AWARE_LOSSLESS              517003 /* .. lossless */


/* Not legal for Aware MultiRate; here only for compatibility */
#define AF_AWARE_PARAM_LAYER           AF_MPEG_PARAM_LAYER
#define AF_AWARE_LAYER_I               AF_MPEG_LAYER_I
#define AF_AWARE_LAYER_II              AF_MPEG_LAYER_II
#define AF_AWARE_PARAM_CONST_QUAL_NMR  AF_MPEG_PARAM_CONST_QUAL_NMR
#define AF_AWARE_PARAM_BITRATE_TARGET  AF_MPEG_PARAM_BITRATE_TARGET
#define AF_AWARE_FIXED_RATE            AF_MPEG_FIXED_RATE
#define AF_AWARE_JOINT_STEREO          AF_MPEG_JOINT_STEREO

/* Not Yet Supported Compression Parameters for Aware MultiRate */
#define AF_AWARE_PARAM_VERSION         509000 /* version (long) */
#define AF_AWARE_PARAM_NOISE_FLOOR     512000 /* noise floor (double) */
#define AF_AWARE_PARAM_FILTER_ID       513000 /* filter id (long token) */
#define AF_AWARE_DEFAULT_FILTER        513001
#define AF_AWARE_PARAM_PSYCHO_MODEL    515000 /* psycho model (long token) */
#define AF_AWARE_PSYCHO_MODEL_1        515001
/* psycho model 1 alpha (double val) */
#define AF_AWARE_PARAM_PSYCHO_MODEL1_ALPHA	516000 
/* scale parameters (long) */
#define AF_AWARE_PARAM_SCALE_DECIMATION		518000 
#define AF_AWARE_PARAM_SCALE_FILTER_SHAPE	519000
#define AF_AWARE_SCALE_DEFAULT_FILTER_SHAPE	519001
#define AF_AWARE_SCALE_FILTER_SHAPE_1		519002
#define AF_AWARE_SCALE_FILTER_SHAPE_2		519003
#define AF_AWARE_PARAM_SCALE_COMBINE_CHANNELS	520000
#define AF_AWARE_PARAM_FRAMES_PER_BLOCK		521000 /* (long) */
#define AF_AWARE_PARAM_MAX_BYTES_PER_BLOCK	522000 /* (long) */
#define AF_AWARE_PARAM_DECODER_DELAY		523000 /* (long) */
#define AF_AWARE_PARAM_ENCODER_DELAY		524000 /* (long) */

/* tokens for afQuery() -- see the man page for instructions */
#define AF_QUERYTYPE_INSTPARAM		500
#define AF_QUERYTYPE_FILEFMT		501
#define AF_QUERYTYPE_COMPRESSION	502
#define AF_QUERYTYPE_COMPRESSIONPARAM	503
#define AF_QUERYTYPE_MISC		504

#define AF_QUERY_NAME		    600
#define AF_QUERY_DESC		    601
#define AF_QUERY_LABEL		    602
#define AF_QUERY_TYPE		    603
#define AF_QUERY_DEFAULT	    604
#define AF_QUERY_ID_COUNT	    605
#define AF_QUERY_IDS		    606
#define AF_QUERY_IMPLEMENTED        613
#define AF_QUERY_TYPE_COUNT	    607
#define AF_QUERY_TYPES		    608
#define AF_QUERY_NATIVE_SAMPFMT	    609
#define AF_QUERY_NATIVE_SAMPWIDTH   610
#define AF_QUERY_SQUISHFAC	    611 /* 1.0 means variable */
#define AF_QUERY_MAX_NUMBER	    612

/* audio file library error codes */
#define AF_BAD_NOT_IMPLEMENTED  0  /* not implemented yet */
#define AF_BAD_FILEHANDLE       1  /* tried to use invalid filehandle */
#define AF_BAD_OPEN             3  /* unix open failed */
#define AF_BAD_CLOSE            4  /* unix close failed */
#define AF_BAD_READ             5  /* unix read failed */
#define AF_BAD_WRITE            6  /* unix write failed */
#define AF_BAD_LSEEK            7  /* unix lseek failed */
#define AF_BAD_NO_FILEHANDLE    8  /* failed to allocate a filehandle struct */
#define AF_BAD_ACCMODE          10 /* unrecognized audio file access mode */
#define AF_BAD_NOWRITEACC       11 /* file not open for writing */
#define AF_BAD_NOREADACC        12 /* file not open for reading */
#define AF_BAD_FILEFMT          13 /* unrecognized audio file format */
#define AF_BAD_RATE             14 /* invalid sample rate */
#define AF_BAD_CHANNELS         15 /* invalid # channels*/
#define AF_BAD_SAMPCNT          16 /* invalid sample count */
#define AF_BAD_WIDTH            17 /* invalid sample width */
#define AF_BAD_SEEKMODE         18 /* invalid seek mode */
#define AF_BAD_NO_LOOPDATA	19 /* failed to allocate loop struct */
#define AF_BAD_MALLOC           20 /* malloc failed somewhere */
#define AF_BAD_LOOPID           21
#define AF_BAD_SAMPFMT          22 /* bad sample format */
#define AF_BAD_FILESETUP        23 /* bad file setup structure*/
#define AF_BAD_TRACKID          24 /* no track corresponding to id */
#define AF_BAD_NUMTRACKS        25 /*too many or too few tracks for file format */
#define AF_BAD_NO_FILESETUP     26 /* failed to allocate a filesetup struct*/
#define AF_BAD_LOOPMODE         27 /*unrecognized loop mode symbol*/
#define AF_BAD_INSTID           28 /* invalid instrument id */
#define AF_BAD_NUMLOOPS         29 /* bad # loops */
#define AF_BAD_NUMMARKS         30 /* bad # markers */
#define AF_BAD_MARKID           31 /* bad marker id */
#define AF_BAD_MARKPOS          32 /* invalid marker position value */
#define AF_BAD_NUMINSTS         33 /* invalid # instruments */
#define AF_BAD_NOAESDATA        34 
#define AF_BAD_MISCID           35
#define AF_BAD_NUMMISC          36
#define AF_BAD_MISCSIZE         37
#define AF_BAD_MISCTYPE         38
#define AF_BAD_MISCSEEK         39
#define AF_BAD_STRLEN           40 /* invalid string length */
#define AF_BAD_RATECONV         45
#define AF_BAD_SYNCFILE         46
#define AF_BAD_CODEC_CONFIG     47 /* improperly configured codec */
#define AF_BAD_CODEC_STATE      48 /* invalid codec state: can't recover */
#define AF_BAD_CODEC_LICENSE    49 /* no license available for codec */
#define AF_BAD_CODEC_TYPE       50 /* unsupported codec type */

#define AF_BAD_COMPRESSION      AF_BAD_CODEC_CONFIG /* for back compat */
#define AF_BAD_COMPTYPE         AF_BAD_CODEC_TYPE   /* for back compat */

#define AF_BAD_INSTPTYPE        51 /* invalid instrument parameter type */
#define AF_BAD_INSTPID          52 /* invalid instrument parameter id */

#define AF_BAD_BYTEORDER        53
#define AF_BAD_FILEFMT_PARAM    54 /* unrecognized file format parameter */
#define AF_BAD_COMP_PARAM       55 /* unrecognized compression parameter */
#define AF_BAD_DATAOFFSET       56 /* bad data offset */
#define AF_BAD_FRAMECNT         57 /* bad frame count */

#define AF_BAD_QUERYTYPE        58 /* bad query type */
#define AF_BAD_QUERY            59 /* bad argument to afQuery() */

#define AF_WARNING_CODEC_RATE   60 /* using 8k instead of codec rate 8012 */
#define AF_WARNING_RATECVT      61 /* warning about rate conversion used */

#define AF_BAD_HEADER           62 /* failed to parse header */

/* AIFF/AIFF-C specific errors when parsing file header */
#define AF_BAD_AIFF_HEADER     108  /* failed to parse chunk header */
#define AF_BAD_AIFF_FORM       109  /* failed to parse FORM chunk */
#define AF_BAD_AIFF_SSND       110  /* failed to parse SSND chunk */
#define AF_BAD_AIFF_CHUNKID    111  /* unrecognized AIFF/AIFF-C chunk id */
#define AF_BAD_AIFF_COMM       112  /* failed to parse COMM chunk */
#define AF_BAD_AIFF_INST       113  /* failed to parse INST chunk */
#define AF_BAD_AIFF_MARK       114  /* failed to parse MARK chunk */
#define AF_BAD_AIFF_SKIP       115  /* failed to skip unsupported chunk */
#define AF_BAD_AIFF_LOOPMODE   116  /* unrecognized loop mode (forw, etc)*/

#ifdef __cplusplus 
}
#endif

#endif /* ! __INC_AUDIOFILE_H__ */
