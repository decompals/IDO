/*****************************************************************************
 * Copyright 1991, 1992, 1993 Silicon Graphics, Inc.
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

#pragma once
/*
 * miditime.h
 */

#ifndef __MIDITIME
#define __MIDITIME
#include "sys/types.h"
#include "sys/time.h"
#include "values.h"

/*
// MMtime with conversions needed for midi.

// Time can be expressed in terms of Ppq or Smpte.

// Smpte is expressed in terms of:
//	smpte rate (24, 25, drop 30, or 30
//	pulses per frame
//	number of pulses

// Ppq (pulses per quarternote) is expressed in terms of:
//	tempo (either in usec per quarter note or in quarter notes per minute)
//	pulses per quarter note
//	number of pulses

// XXX Where should expression in terms of sample rate, number of samples go?
*/

enum SmpteFPS {
    NotSmpte	= -1,
    Smpte24	= 24,
    Smpte25	= 25,
    SmpteDrop30	= 29,
    Smpte30	= 30
};

enum MItimeTempoType { MItimeUsecPerQuarterNote, MItimeQuarterNotesPerMinute };

#ifdef __cplusplus

class MItime {
public:
    enum TempoType { UsecPerQuarterNote, QuarterNotesPerMinute };

    MItime(long hours, long minutes, long secs, long micro = 0);
    MItime(long secs, long micro);
    MItime(long micro = 0);
    MItime(const MItime &);

    MItime(SmpteFPS rate, long ppf, long puls);			// Smpte
    MItime(TempoType type, long tempo, long ppq, long puls);	// Ppq
    MItime(const struct timeval &tv);

    long gethours() const;
    long getminutes() const;
    long getsecs() const;
    long gettotalsecs() const;
    long getmicros() const;

    void sethours(long /*hours*/) {}
    void setminutes(long /*minutes*/) {}
    void setsecs(long s) { secs = s; }
    void setmicros(long u) { micros = u; }

    MItime operator-() const;

    long smpte(SmpteFPS rate, long ppf);
    long ppq(TempoType type, long tempo, long Ppq);

    MItime operator+(const MItime &) const;
    MItime operator-(const MItime &) const;
    MItime operator*(const float) const;
    MItime operator/(const float) const;
    MItime operator%(int) const;
    MItime &operator=(const MItime &);
    MItime &operator+=(const MItime &);
    MItime &operator-=(const MItime &);
    MItime &operator*=(const float);
    MItime &operator/=(const float);

    int operator<(const MItime &) const;
    int operator<=(const MItime &) const;
    int operator>(const MItime &) const;
    int operator>=(const MItime &) const;
    int operator==(const MItime &) const;
    int operator!=(const MItime &) const;

    operator int() const;
    operator long() const;
    operator double() const;

    MItime currentsystemtime();

  protected:
    long secs;
    long micros;
};

static const MItime MItimeUnknown = MItime(MAXLONG, 0);
static const MItime MItimePosInfinity = MItime(MAXLONG - 1, 0);
static const MItime MItimeNegInfinity = - MItimePosInfinity;
static const MItime MItimeMaxInt = MItime(0, MAXINT);
static const MItime MItimeMaxLong = MItime(0, MAXLONG);
static const MItime MItimeZero = MItime(0);
static const MItime MItime1sec = MItime(1, 0);
static const MItime MItime1Min = MItime(60, 0);
static const MItime MItime1Hour = MItime(3600, 0);

#else				/* __cplusplus */

typedef struct {
    long opaque1;
    long opaque2;
} MItime;


extern MItime MItimecreatesmpte(int, long, long);
extern MItime MItimecreateppq(int, long, long, long);
extern long MItimegetsmpte(MItime*, int, long);
extern long MItimegetppq(MItime*, int, long, long);

extern MItime MItimecreatehours(long, long, long, long);
extern MItime MItimecreatesecs(long, long);
extern MItime MItimecreatemicros(long);
extern MItime MItimecreate();

extern long MItimegethours(MItime*);
extern long MItimegetminutes(MItime*);
extern long MItimegetsecs(MItime*);
extern long MItimegettotalsecs(MItime*);
extern long MItimegetmicros(MItime*);

extern MItime MItimenegate(MItime*);

extern MItime MItimeadd(MItime*, MItime*);
extern MItime MItimesubtract(MItime*, MItime*);
extern MItime MItimemultiply(MItime*, float);
extern MItime MItimedivide(MItime*, float);
/* extern MItime MItimemod(MItime*, int); */
extern MItime MItimecopy(MItime*, MItime*);

extern int MItimecmp(MItime*, MItime*);

extern int MItimeint(MItime*);
extern long MItimelong(MItime*);
extern double MItimedouble(MItime*);

extern MItime MItimecurrentsystemtime(MItime*);

#endif				/* __cplusplus */
#endif				/* __MIDIIME */
