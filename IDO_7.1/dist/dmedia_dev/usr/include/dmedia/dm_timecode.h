/*
 *  dm_timecode.h -- Header file for libdmedia timecode manipulation
 *     routines and structure definitions. 
 *
 * Copyright 1995 Silicon Graphics, Inc.
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
 */

#include <dmedia/dmedia.h>      /* for DMstatus, DMboolean, etc */

#ifndef _DM_TIMECODE_H_
#define _DM_TIMECODE_H_

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
   int hour;
   int minute;
   int second;
   int frame;
   int tc_type;                      /* FPS and drop type */
} DMtimecode;

/* tc_type definition */

#define DM_TC_DROP_MASK    0x00010000
#define DM_TC_FORMAT_MASK  0x0000FF00
#define DM_TC_RATE_MASK    0x000000FF

/* Formats */
#define DM_TC_FORMAT_NTSC  0x00000100
#define DM_TC_FORMAT_PAL   0x00000200
#define DM_TC_FORMAT_FILM  0x00000300

/* Rates */
#define DM_TC_RATE_2997    0x00000001   /* NTSC, Brazilian PAL, dropframe     */
#define DM_TC_RATE_30      0x00000002   /* NTSC, Brazilian PAL, non-dropframe */
#define DM_TC_RATE_24      0x00000003   /* film                               */
#define DM_TC_RATE_25      0x00000004   /* PAL                                */

/* Dropframe flag */
#define DM_TC_NODROP       0x00000000
#define DM_TC_DROPFRAME    0x00010000

/* common (fully specified) timecode types */
#define DM_TC_BAD          0
#define DM_TC_24_ND \
   DM_TC_NODROP | DM_TC_FORMAT_FILM | DM_TC_RATE_24
#define DM_TC_25_ND \
   DM_TC_NODROP | DM_TC_FORMAT_PAL | DM_TC_RATE_25
#define DM_TC_30_ND \
   DM_TC_NODROP | DM_TC_FORMAT_NTSC | DM_TC_RATE_30

/* Note that 29.97 4-field drop is what is used in NTSC */
/* 29.97 8-field drop is used in M-PAL (Brazil) */
#define DM_TC_2997_4FIELD_DROP \
   DM_TC_DROPFRAME | DM_TC_FORMAT_NTSC | DM_TC_RATE_2997
#define DM_TC_2997_8FIELD_DROP \
   DM_TC_DROPFRAME | DM_TC_FORMAT_PAL | DM_TC_RATE_2997

/* Function Prototypes */

extern DMstatus dmTCToString( char * string, const DMtimecode *smpteTimecode );

extern DMstatus dmTCFromString( DMtimecode * result, const char * string,
                                  int tc_type );

extern DMstatus dmTCFromSeconds( DMtimecode * result, const int tc_type,
                               const double seconds );

extern DMstatus dmTCToSeconds( const DMtimecode * a, double * seconds );

extern int  dmTCFramesPerDay( const int tc_type );

extern DMstatus dmTCAddTC(  DMtimecode * result, const DMtimecode *a,
                              const DMtimecode *b, int *overflow );

extern DMstatus dmTCAddFrames( DMtimecode * result, const DMtimecode * a, 
                                 int  b, int * overflowunderflow );

extern DMstatus dmTCFramesBetween( int  * result, const DMtimecode * a, 
                              const DMtimecode * b );

#ifdef __cplusplus
}
#endif
#endif /* _DM_TIMECODE_H_ */
