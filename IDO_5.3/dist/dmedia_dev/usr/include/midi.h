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

#ifndef __MIDILIB_H_
#define __MIDILIB_H_
#pragma once
/*
 * midi.h
 */

#include "miditime.h"
#include <sys/midi.h> 

extern int msglengths [];

enum MIDIstatus {
    /* For distinguishing channel number from status */
    MIDI_ChannelMask		= 0x0F,
    MIDI_StatusMask		= 0xF0,

    /* Channel voice messages */
    MIDI_ChannelVoice		= 0x80,
    MIDI_NoteOff		= 0x80,
    MIDI_NoteOn			= 0x90,
    MIDI_PolyKeyPressure	= 0xA0,
    MIDI_ControlChange		= 0xB0,
    MIDI_ChannelModeSelect	= 0xB0,
    MIDI_ProgramChange		= 0xC0,
    MIDI_ChannelPressure	= 0xD0,
    MIDI_PitchBendChange	= 0xE0,

    /* System messages */
    MIDI_SysEx			= 0xF0,		/* System Exclusive */

    /* System common */
    MIDI_SystemCommon		= 0xF1,
    MIDI_TimeCodeQuarterFrame	= 0xF1,
    MIDI_SongPositionPointer	= 0xF2,
    MIDI_SongSelect		= 0xF3,
    MIDI_Undefined1		= 0xF4,
    MIDI_Undefined2		= 0xF5,
    MIDI_TuneRequest		= 0xF6,
    MIDI_EOX			= 0xF7,		/* End of System Exclusive */

    /* System real time */
    MIDI_SystemRealTime		= 0xF8,
    MIDI_TimingClock		= 0xF8,
    MIDI_Undefined3		= 0xF9,
    MIDI_Start			= 0xFA,
    MIDI_Continue		= 0xFB,
    MIDI_Stop			= 0xFC,
    MIDI_Undefined4		= 0xFD,
    MIDI_ActiveSensing		= 0xFE,
    MIDI_SystemReset		= 0xFF,
    MIDI_Meta			= 0xFF		/* MIDI Files only */
};

enum MIDIcontrollers {
    MC_ModulationWheel		= 0x01,
    MC_BreathController		= 0x02,
    MC_FootController		= 0x04,
    MC_PortamentoTime		= 0x05,
    MC_DataEntryMSB		= 0x06,
    MC_MainVolume		= 0x07,
    MC_Balance			= 0x08,
    MC_Pan			= 0x0A,
    MC_ExpressionController	= 0x0B,
    MC_GeneralPurpose1		= 0x10,
    MC_GeneralPurpose2		= 0x11,
    MC_GeneralPurpose3		= 0x12,
    MC_GeneralPurpose4		= 0x13,

    MC_LSBfor00			= 0x20,
    MC_LSBfor1F			= 0x3F,

    MC_DamperPedal		= 0x40,
    MC_Portamento		= 0x41,
    MC_Sostenuto		= 0x42,
    MC_SoftPedal		= 0x43,
    MC_Hold2			= 0x45,
    MC_GeneralPurpose5		= 0x50,
    MC_GeneralPurpose6		= 0x51,
    MC_GeneralPurpose7		= 0x52,
    MC_GeneralPurpose8		= 0x53,
    MC_ExternalEffectsDepth	= 0x5B,
    MC_TremeloDepth		= 0x5C,
    MC_ChorusDepth		= 0x5D,
    MC_CelesteDepth		= 0x5E,
    MC_PhaserDepth		= 0x5F,
    MC_DataIncrement		= 0x60,
    MC_DataDecrement		= 0x61,
    MC_NRPN_LSB			= 0x62,	/* Non Registered Parameter Number */
    MC_NRPN_MSB			= 0x63,
    MC_RPN_LSB			= 0x64,	/* Registered Parameter Number */
    MC_RPN_MSB			= 0x65,

    /* Channel Mode Messages */
    MC_ChannelModeMessages	= 0x79,
    MC_ResetAllControllers	= 0x79,
    MC_LocalControl		= 0x7A,
    MC_AllNotesOff		= 0x7B,
    MC_OmniModeOff		= 0x7C,
    MC_OmniModeOn		= 0x7D,
    MC_MonoModeOn		= 0x7E,
    MC_PolyModeOn		= 0x7F
};

#ifdef __cplusplus

class MImessage {
  public:
    void makemessage(u_int status, u_int channel, char v1, char v2) { 
	type_ = 1;
	channel_ = channel % 16;
	device_ = channel >> 4;
	status_ = status >> 4;
	byte1_ = v1;
	byte2_ = v2;
	length_ = msglengths[status & 0x7f];
    }

    u_int status() { return ((status_ == 0xf) ? 
			  (status_ << 4 | channel_) : status_ << 4); }

    u_int channel() { return (status_ == 0xf ? 0 : (channel_+(device_<<4))); }


    u_char length() { return length_; }
    u_char device() { return device_; }
    u_char byte1() { return byte1_; }
    u_char byte2() { return byte2_; }

    void setdevice(int dev) { device_ = dev; }
    void setchannel(int channel) { channel_ = channel;
				   device_ = channel >> 4; }
    void setstatus(int stat) { status_ = stat >> 4; 
			       length_ = msglengths[stat & 0x7f]; }
    void setbyte1(int val) { byte1_ = val; }
    void setbyte2(int val) { byte2_ = val; }
    
  private:
    unsigned int type_:1;		/* always == 1 */
    unsigned int length_:2;
    unsigned int device_:5;
    unsigned int status_:4;
    unsigned int channel_:4;
    unsigned int byte1_:8;
    unsigned int byte2_:8;
};

#else	/* __cplusplus */

typedef long MImessage;		/* play with bits, you deserve what you get */

#define __statm(m) (m & 0xf00000)
#define MIstatus(m) ((__statm(m) != 0xf00000) ? (__statm(m) >> 16) : \
		     ((m & 0xff0000) >> 16))

#define MIdevice(m) ((m & 0x1f000000) >> 24)
#define MIchannel(m) (__statm(m) == 0xf ? 0 : ((m & 0xf0000) >> 16) + ((m & 0x1f000000) >> 20))

#define MIlength(m) (msglengths[__statm(m) & 0x7f])
#define MIbyte1(m) ((m & 0xff00) >> 8)
#define MIbyte2(m) (m & 0xff)

extern void MImakemessage(MImessage *msg, u_int, u_int, char, char);

#define __mp(m) (*(int *)(m))

#define MIsetdevice(m, d) (__mp(m) &= 0xe0ffffff, __mp(m) |= ((d & 0x1f) << 24))

#define MIsetchannel(m, c) (__mp(m) &= 0xfff0ffff, __mp(m) |= ((c & 0xf) << 16),MIsetdevice(m, c >> 4))

#define MIsetstatus(m,s) (__mp(m) &= 0xff0fffff, __mp(m) |= (s << 20))
#define MIsetbyte1(m,v) (__mp(m) &= 0xffff00ff, __mp(m) |= (v << 8))
#define MIsetbyte2(m,v) (__mp(m) &= 0xffffff00, __mp(m) |= v)

#endif	/* __cplusplus */

typedef enum { MIMESSAGE, MISYSEX, MIREALTIME, MISYSCOM } MItype;

typedef union {
    MImessage msgbuf;
    u_char *sysexbuf;
} MIbuf;

typedef struct {
    MItime	dt;
    MIbuf	mm;
    MItype      t;
    int count;    /* count & device used by sysex data */
    int device;
} MIevent;

/* accessor macros for MIevents */

#define MESSAGE(x)  ((x).mm.msgbuf)
#define SYSEX(x)    ((x).mm.sysexbuf)

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

typedef void (*MIerrfunc)(long,const char*,...);
extern MIerrfunc MIseterrorhandler(MIerrfunc);

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

/* error codes from libmidi.  they are negative so that they won't
 * conflict with errno's.  All errors are considered fatal.
 */

#define MIOUTOFMEMORY   -100
#define MIBADSTATUS     -101
#define MINOMIDIPROC    -102
#define MIBADFLAG       -103

#endif /* __MIDILIB_H_ */
