#ifndef __INC_AUDIOFILE_H__
#define __INC_AUDIOFILE_H__ 

/***************************************************************************
 * SGI Audio File Library
 *
 * audiofile.h
 *    header file for use with /usr/lib/libaudiofile.a
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

#ident "$Revision: 1.20 $"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _AFfilesetup *AFfilesetup;  
typedef struct _AFfilehandle * AFfilehandle;
typedef void (*AFerrfunc)(long,const char*);

#define AF_NULL_FILESETUP ((struct _AFfilesetup *)0)
#define AF_NULL_FILEHANDLE  ((struct _AFfilehandle *)0)

/*
 * for now, use this as value wherever an instrument id is required
 *    as an argument to one of the library functions below
 */
#define AF_DEFAULT_TRACK 1001

/*
 * for now, use this value wherever an instrument id is required
 *   as an argument to one of the library functions below
 */
#define AF_DEFAULT_INST 2001

/*
 * sample formats
 */ 
#define AF_SAMPFMT_TWOSCOMP    401  /* linear two's complement */

/*
 * file formats
 */
#define AF_FILE_UNSUPPORTED    -2 /* recognized audio file format,           */
                                  /* not supported by the library            */
#define AF_FILE_UNKNOWN        -1 /* not a recognized audio file format      */
#define AF_FILE_AIFFC           1 /* the default                             */
#define AF_FILE_AIFF            2 /* for backward compatability              */

/* 
 * basic operations on a filehandle
 */
extern AFfilehandle AFopenfile(const char *,const char *, AFfilesetup);
extern AFfilehandle AFopenfd(int,const char *, AFfilesetup);
extern long AFidentifyfd(int);
extern long AFclosefile(AFfilehandle);                 
extern long AFreadframes(AFfilehandle,long/*track id*/,void *,long);  
extern long AFwriteframes(AFfilehandle,long/*track id*/,void *,long); 
extern long AFgetframecnt(AFfilehandle, long/*track*/);
extern long AFseekframe(AFfilehandle,long/*track id*/,long);
extern long AFgettrackids(AFfilehandle, long */*track ids*/);
extern long AFgetfilefmt(AFfilehandle, long *);
extern long AFgetchannels(AFfilehandle, long/*track*/); 
extern double AFgetrate(AFfilehandle, long/*track*/);
extern void AFgetsampfmt(AFfilehandle, long/*track*/, long *, long *);
extern long AFgetloopids(AFfilehandle, long /*inst*/, long*); 
extern long AFgetloopmode(AFfilehandle, long/*inst*/, long /*loopid */);
extern long AFgetloopstart(AFfilehandle, long/*inst*/, long/*loopid */);
extern long AFgetloopend(AFfilehandle, long/*inst*/, long/*loopid*/);
extern AFfilesetup  AFnewfilesetup(void);
extern void AFfreefilesetup(AFfilesetup);
extern void AFinittrackids(AFfilesetup, long *, long);
extern void AFinitinstids(AFfilesetup, long *, long);
extern void AFinitfilefmt(AFfilesetup, long/*fmt*/);
extern void AFinitaeschanneldata(AFfilesetup, long);
extern long AFgetaeschanneldata(AFfilehandle,long,unsigned char buf[24]);
extern void AFsetaeschanneldata(AFfilehandle,long,unsigned char buf[24]);
extern void AFinitloopids(AFfilesetup, long/*inst*/, long *, long);
extern void AFinitmarkids(AFfilesetup,long/*trk*/,long *, long);

extern long AFgetinstids(AFfilehandle, long */*inst ids*/);
extern void AFinitchannels(AFfilesetup, long/*track*/, long);
extern void AFinitsampfmt(AFfilesetup, long/*track*/, long, long);
extern void AFinitrate(AFfilesetup, long/*track*/, double);
extern void AFsetloopmode(AFfilehandle, long/*inst*/, long/*loopid*/, long);
extern void AFsetloopstart(AFfilehandle, long/*inst*/, long/*loopid*/,
                                                               long/*markid*/);
extern void AFsetloopend(AFfilehandle, long/*inst*/, long/*loopid*/, 
                                                               long/*markid*/);
extern void AFsetlooptrack(AFfilehandle, long/*inst*/, long/*loopid*/, 
                                                               long/*trackid*/);
extern long AFgetlooptrack(AFfilehandle, long/*inst*/, long/*loopid*/);
extern void AFsetmarkpos(AFfilehandle, long/*track*/, long/*markid*/, long);
extern long AFgetmarkids(AFfilehandle, long/*track*/, long */*ids*/);
extern long AFgetmarkpos(AFfilehandle, long/*track*/, long/*markid*/);
extern int  AFgetfd(AFfilehandle);
extern void AFinitmiscids(AFfilesetup, long *, long);
extern void AFinitmisctype(AFfilesetup, long /*miscid*/, long /*type*/);
extern void AFinitmiscsize(AFfilesetup, long /*miscid*/, long /*size*/);
extern long AFgetmiscids(AFfilehandle, long * /*idlist*/);
extern long AFgetmisctype(AFfilehandle, long /*miscid*/);
extern long AFgetmiscsize(AFfilehandle, long /*miscid*/);
extern long AFwritemisc(AFfilehandle, long /*miscid*/, void *buf,long);
extern long AFreadmisc(AFfilehandle, long /*miscid*/, void *buf,long);
extern void AFseekmisc(AFfilehandle,long /*miscid*/, long /*offset*/);
extern AFerrfunc  AFseterrorhandler(AFerrfunc efunc);

extern void AFsetinstparamlong(AFfilehandle, long/*inst*/, 
                              long/*param*/, long/*value*/);
extern long AFgetinstparamlong(AFfilehandle, long/*inst*/, long /*param*/);

extern void AFinitmarkname(AFfilesetup , long , long , const char *);
extern char *AFgetmarkname(AFfilehandle, long, long);

extern void AFinitcompression(AFfilesetup, long, long);
extern long AFgetcompression(AFfilehandle, long);
extern char *AFgetcompressionname(AFfilehandle, long);

extern long AFsyncfile(AFfilehandle);

/*
 * init, get compression parameters
 */
struct _AUpvlist;

extern void AFinitcompressionparams(AFfilesetup, long, long,  
                                         struct _AUpvlist *, long);
extern void AFgetcompressionparams(AFfilehandle, long, long *, 
                                         struct _AUpvlist *, long);


/*
 * loop modes
 */
#define AF_LOOP_MODE_NOLOOP   0
#define AF_LOOP_MODE_FORW     1
#define AF_LOOP_MODE_FORWBAKW 2

/*
 * AIFF instrument parameters
 */
#define AF_INST_MIDI_BASENOTE      301   
#define AF_INST_NUMCENTS_DETUNE    302   
#define AF_INST_MIDI_LONOTE        303
#define AF_INST_MIDI_HINOTE        304
#define AF_INST_MIDI_LOVELOCITY    305
#define AF_INST_MIDI_HIVELOCITY    306
#define AF_INST_NUMDBS_GAIN        307
#define AF_INST_SUSLOOPID          308 /* loop id for sustain loop */
#define AF_INST_RELLOOPID          309 /* loop id for release loop */

/*
 * miscellaneous AIFF/AIFF-C data sections
 */
#define AF_MISC_AIFF_COPY         201  /* copyright string */
#define AF_MISC_AIFF_AUTH         202  /* author string */
#define AF_MISC_AIFF_NAME         203  /* name string */
#define AF_MISC_AIFF_ANNO         204  /* annotation string */
#define AF_MISC_AIFF_APPL         205  /* application-specific data */
#define AF_MISC_AIFF_MIDI         206  /* MIDI exclusive data */

#define AF_MISC_AIFF_UNRECOGNIZED 0    /* unrecognized data chunk */

/*
 * AIFF-C compression schemes supported by SGI Audio File Library
 */
#define AF_COMPRESSION_UNKNOWN          -1
#define AF_COMPRESSION_NONE             0
#define AF_COMPRESSION_G722             501
#define AF_COMPRESSION_G711_ULAW        502
#define AF_COMPRESSION_G711_ALAW        503
/*
 * Apple proprietary AIFF-C compression schemes: not supported by SGI 
 *     Audio File Library 
 */
#define AF_COMPRESSION_APPLE_ACE2       504
#define AF_COMPRESSION_APPLE_ACE8       505
#define AF_COMPRESSION_APPLE_MAC3       506
#define AF_COMPRESSION_APPLE_MAC6       507


/*
 * Aware Compression
 */
#define AF_COMPRESSION_AWARE_MPEG               515 
#define AF_COMPRESSION_AWARE_MULTIRATE          516

/*
 * some handy defaults for AFinitcompression
 */
#define AF_COMPRESSION_AWARE_DEFAULT_MPEG_I	508
#define AF_COMPRESSION_AWARE_DEFAULT_MPEG_II	509
#define AF_COMPRESSION_AWARE_DEFAULT_MULTIRATE	513
#define AF_COMPRESSION_AWARE_DEFAULT_LOSSLESS	514

/*
 * algorithm layer
 */
#define AF_AWARE_PARAM_LAYER           530000
#define AF_AWARE_LAYER_I               530001
#define AF_AWARE_LAYER_II              530002

/*
 * channel policy
 */
#define AF_AWARE_PARAM_CHANNEL_POLICY  510000
#define AF_AWARE_JOINT_STEREO          510001
#define AF_AWARE_STEREO                510002
#define AF_AWARE_INDEPENDENT           510003
/*
 * bitrate target
 */
#define AF_AWARE_PARAM_BITRATE_TARGET  511000
/* 
 * const qual NMR 
 */
#define AF_AWARE_PARAM_CONST_QUAL_NMR  514000
/* 
 * bitrate policy 
 */
#define AF_AWARE_PARAM_BITRATE_POLICY  517000
#define AF_AWARE_FIXED_RATE            517001
#define AF_AWARE_CONST_QUAL            517002
#define AF_AWARE_LOSSLESS              517003

/*
 * audio file library error codes
 */
#define AF_BAD_NOT_IMPLEMENTED 	0  /* not implemented yet */
#define AF_BAD_FILEHANDLE         	1  /* tried to use invalid filehandle */
#define AF_BAD_OPEN            	3  /* unix open failed */
#define AF_BAD_CLOSE    	4  /* unix close failed */
#define AF_BAD_READ     	5  /* unix read failed */
#define AF_BAD_WRITE    	6  /* unix write failed */
#define AF_BAD_LSEEK    	7  /* unix lseek failed */
#define AF_BAD_NO_FILEHANDLE   8  /* failed to allocate a filehandle struct */
#define AF_BAD_ACCMODE          10 /* unrecognized audio file access mode */
#define AF_BAD_NOWRITEACC       11 /* file not open for writing */
#define AF_BAD_NOREADACC        12 /* file not open for reading */
#define AF_BAD_FILEFMT          13 /* unrecognized audio file format */
#define AF_BAD_RATE             14 /* invalid sample rate */
#define AF_BAD_CHANNELS         15 /* invalid number of channels*/
#define AF_BAD_SAMPCNT          16 /* invalid sample count */
#define AF_BAD_WIDTH            17 /* invalid sample width */
#define AF_BAD_SEEKMODE         18 /* invalid seek mode */
#define AF_BAD_NO_LOOPDATA      19 /* failed to allocate loopdata struct */
#define AF_BAD_MALLOC           20 /* malloc failed somewhere */
#define AF_BAD_LOOPID           21
#define AF_BAD_SAMPFMT          22 /* bad sample format */
#define AF_BAD_FILESETUP        23 /* bad file setup structure*/
#define AF_BAD_TRACKID          24 /* no track corresponding to id */
#define AF_BAD_NUMTRACKS        25 /*too many or too few tracks for file fmt*/
#define AF_BAD_NO_FILESETUP     26 /* failed to allocate a filesetup struct*/
#define AF_BAD_LOOPMODE         27 /*unrecognized loop mode symbol*/
#define AF_BAD_INSTID           28 /* invalid instrument id */
#define AF_BAD_NUMLOOPS         29 /* bad number of loops */
#define AF_BAD_NUMMARKS         30 /* bad number of markers */
#define AF_BAD_MARKID           31 /* bad marker id */
#define AF_BAD_MARKPOS          32 /* invalid marker position value */
#define AF_BAD_NUMINSTS         33 /* invalid number of instruments */
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

/*
 * error parsing AIFF or AIFF-C file
 */
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
