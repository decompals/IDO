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

/*
 * midiio.h -- definitions for MIDI I/O port and configurations.
 */
#ifndef __MIDIIO_H__
#define __MIDIIO_H__
#pragma once
#include "midi.h"
#include <sys/midi.h>

#define NMIDIS       8

#define MI_STAMPING        1
#define MI_TIMEOUT         2
#define MI_BLOCKING        3
#define MI_ACTIVE          4
#define MI_OUTPUTTIME      5
#define MI_SYNTH           6

#define MINOSTAMP    0
#define MIDELTASTAMP 1
#define MIRELSTAMP   2

#define MIBLOCKING      1
#define MINONBLOCKING   0

#define MISYNTH         1
#define MINOSYNTH       0

#define SYSEXSZ      0x200000

#ifdef __cplusplus
// MIDI I/O port, configuration
//

class MIioport;

#endif		/* __cplusplus */
#define STAMPED 1

#ifdef __cplusplus

// class MIconfig tells MIport how to open.  MIconfigs
// should be saveable and commicable between applications.

class MIconfig {
  public:
    MIconfig();

    u_int stamp() { return _stamp; }
    void setstamp(u_int stamping) { _stamp = stamping; }

    MItime *timeout() { return &_timeout; }
    void settimeout(MItime t) { _timeout = t; }

    MItime *outputtime() { return &_outputtime; }
    void setoutputtime(MItime t) { _outputtime = t; }
    
    u_int blocking() { return _blocking; }
    void setblocking(u_int b) { _blocking = b; }

    u_int activesense() { return _active; }
    void setactivesense(u_int a) { _active = a; }

    u_int synth() { return _synth; }
    void setsynth(u_int a) { _synth = a; }

    void setparams(u_int *params, u_int length);
    void getparams(u_int *params, u_int length);

  private:
    u_int _stamp:2;
    u_int _blocking:1;
    u_int _active:1;
    u_int _synth:1;
    MItime _timeout;   // timeout, for select within MIport
    MItime _outputtime;   // amout of time a program can get ahead of output
};

class sysex_s {
  public:
    int device;   // device sysex is occuring on
    MItime stamp; // timestamp of first chunk 
    char *buf;    // sysex buffer
    int idx;      // current index into buffer
};

class MIport {
  public:
    MIport() { open_ = 0; }
    virtual int open(const char *direction, MIconfig *config);
    virtual int close();

    virtual int send(MIevent* buf, int count);
    virtual int receive(MIevent* buf, int count);

    const MItime start() { return bishop_usher; }
    void setstart(struct timeval *t);
    
    int getfd() { return fd; }

    void setconfig(MIconfig* config);  // change configuration on the fly!
    MIconfig *config() { return conf; }

    MItime *timeleft();

  protected:
    int stampinput(int on);
    int fd;
    int open_:1;
    int stamp_:1;
    MIconfig *conf;
    MItime bishop_usher;
    MItime lasttime, runtime;
    MItime makestamp(struct timeval);

    
    sysex_s sysbufs[NMIDIS];
    sysex_s *findbuf(int device);
    void clrbuf(sysex_s *buf);
};

#else		/* __cplusplus */

typedef void MIport;
typedef void MIconfig;

extern MIconfig *MInewconfig();
extern void MIfreeconfig(MIconfig*);
extern void MIsetparams(MIconfig *, u_int *, u_int);
extern void MIgetparams(MIconfig *, u_int *, u_int);

extern void MICsetstamp(MIconfig*, u_int);
extern int MICstamp(MIconfig*);
extern int MICblocking(MIconfig*);
extern int MICactivesense(MIconfig*);
extern const MItime *MICtimeout(MIconfig*);
extern void MICsettimeout(MIconfig*, MItime*);
extern void MICsetblocking(MIconfig*, u_int);
extern void MICsetactivesense(MIconfig*, u_int);

extern MIport* MInewport();
extern void MIfreeport(MIport*);

extern int MIopen(MIport*, const char*, MIconfig*);
extern void MIclose(MIport*);

extern int MIsend(MIport*, MIevent*, int);
extern int MIreceive(MIport*, MIevent*, int);
extern void MIsetstart(MIport*, struct timeval *);
extern const MItime *MIstart(MIport*);
extern MItime *MItimeleft(MIport*);
extern int MIgetfd(MIport*);

#endif		/* __cplusplus */
#endif 		/* __MIDIIO_H__ */
