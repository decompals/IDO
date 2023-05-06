/**************************************************************************
 *									  *
 * Copyright (C) 1993 Silicon Graphics, Inc.   			          *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/
#ifndef __MD_H__
#define __MD_H__

#include <sgidefs.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/midi.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct __MDport *MDport;

typedef void (*mdErrfunc)(long,const char*,...);
extern mdErrfunc mdSeterrorhandler(mdErrfunc efunc);

MDport mdOpenInPort(char *name);   /* open an input port */
MDport mdOpenOutPort(char *name);  /* open an output port */
int mdClosePort(MDport);           /* close down any port */
int mdGetFd(MDport);		   /* retrieve a file descriptor */

int mdInit();                       /* initialize midi system */
char *mdGetName(int);               /* get the name of a lower device */

int mdSetStampMode(MDport, int);    /* set time stamp mode */
int mdGetStampMode(MDport);    /* get time stamp mode */
int mdSetOrigin(MDport, long long); /* set the timing base */
unsigned long long mdGetOrigin(MDport); /* set the timing base */

/* these routines deal with MIDI time.  They have no effect
 * unless MD_DELATTICKS or MD_RELATIVETICKS have been set in 
 * mdSetStampMode 
 */
int mdGetDivision(MDport);          /* sets the divisions per beat */
int mdSetDivision(MDport, int);     /* aka ppq */

int mdGetTempo(MDport);             /* {g,s}et tempo, in usec per beat */
int mdSetTempo(MDport, int);        

int mdSetTemposcale(MDport, double); /* set a tempo scalar */

/* conversions from nanosecond times to ticks based on 
   division and tempo for the MDport in question 
 */
unsigned long long mdTicksToNanos(MDport, unsigned long long);
unsigned long long mdNanosToTicks(MDport, unsigned long long);

/* timing modes */
#define MD_NOSTAMP           1
#define MD_DELTASTAMP        2
#define MD_RELATIVESTAMP     3
#define MD_DELTATICKS        4	    /* delta ticks, as in SMF 1.0 */
#define MD_RELATIVETICKS     5	    /* relative ticks */

#define MD_STAMPING_MASK     0xf
#define MD_PAUSED            0x10

extern int __mdmsglengths [];

#define mdMsgLen(x) ((__mdmsglengths[(x & 0x7f) >> 4]))

/* set/get the maximum time to drain MDport 
 * this implements "temporal buffering"
 */
int mdSetTemporalBuffering(MDport, unsigned int);
int mdGetTemporalBuffering(MDport);

typedef struct __mdevent {
	char msg[4];
	char *sysexmsg;
	unsigned long long stamp;   /* time stamp in ns */
	int msglen;                 /* length of data, sysex only */
} MDevent;

#define NSEC_PER_SEC 1000000000
#define NSEC_PER_MSEC 1000000

/* basic I/O routines */
int mdReceive(MDport, MDevent *, int);
int mdSend(MDport, MDevent *, int);
unsigned long long mdPause(MDport);
unsigned long long mdTell(MDport);
int mdPanic(MDport);

int mdPrintEvent(char *, MDevent *, int);

typedef void *(*mdMalloc_t)(size_t);
typedef void (*mdFree_t)(void *);

void mdSetAllocator(mdMalloc_t, mdFree_t, void *);
extern mdMalloc_t mdMalloc;
extern mdFree_t mdFree;

/* routines for dealing with midi messages */
int mdGetStatus(char *msg);
int mdGetChannel(char *msg);
int mdGetByte1(char *msg);
int mdGetByte2(char *msg);

void mdSetStatus(char *msg, int x);
void mdSetChannel(char *msg, int x);
void mdSetByte1(char *msg, int x);
void mdSetByte2(char *msg, int x);

#define MD_CHANNELMASK	 	 0x0F
#define MD_STATUSMASK	 	 0xF0

#define MD_CHANNELVOICE	 0x80
#define MD_NOTEOFF	 0x80
#define MD_NOTEON		 0x90
#define MD_POLYKEYPRESSURE 0xA0
#define MD_CONTROLCHANGE	 0xB0
#define MD_CHANNELMODESELECT 0xB0
#define MD_PROGRAMCHANGE	 0xC0
#define MD_CHANNELPRESSURE 0xD0
#define MD_PITCHBENDCHANGE 0xE0

#ifdef MD_SYSEX
#undef MD_SYSEX
#endif
#define MD_SYSEX		 0xF0		/* System Exclusive */

#define MD_SYSTEMCOMMON	 0xF1
#define MD_TIMECODEQUARTERFRAME 0xF1
#define MD_SONGPOSITIONPOINTER 0xF2
#define MD_SONGSELECT	 0xF3
#define MD_UNDEFINED1	 0xF4
#define MD_UNDEFINED2	 0xF5
#define MD_TUNEREQUEST	 0xF6
#define MD_EOX		 0xF7		/* End of System Exclusive */

#define MD_SYSTEMREALTIME	 0xF8
#define MD_TIMINGCLOCK	 0xF8
#define MD_UNDEFINED3	 0xF9
#define MD_START		 0xFA
#define MD_CONTINUE	 0xFB
#define MD_STOP		 0xFC
#define MD_UNDEFINED4	 0xFD
#define MD_ACTIVESENSING 0xFE
#define MD_SYSTEMRESET	 0xFF
#define MD_META		 0xFF		/* MIDI Files only */

#define MD_MODULATIONWHEEL	 0x01
#define MD_BREATHCONTROLLER	 0x02
#define MD_FOOTCONTROLLER	 0x04
#define MD_PORTAMENTOTIME	 0x05
#define MD_DATAENTRYMSB	 0x06
#define MD_MAINVOLUME	 0x07
#define MD_BALANCE		 0x08
#define MD_PAN		 0x0A
#define MD_EXPRESSIONCONTROLLER 0x0B
#define MD_GENERALPURPOSE1	 0x10
#define MD_GENERALPURPOSE2	 0x11
#define MD_GENERALPURPOSE3	 0x12
#define MD_GENERALPURPOSE4	 0x13

#define MD_LSBFOR00		 0x20
#define MD_LSBFOR1F		 0x3F

#define MD_DAMPERPEDAL	 0x40
#define MD_PORTAMENTO	 0x41
#define MD_SOSTENUTO	 0x42
#define MD_SOFTPEDAL	 0x43
#define MD_HOLD2		 0x45
#define MD_GENERALPURPOSE5	 0x50
#define MD_GENERALPURPOSE6	 0x51
#define MD_GENERALPURPOSE7	 0x52
#define MD_GENERALPURPOSE8	 0x53
#define MD_EXTERNALEFFECTSDEPTH 0x5B
#define MD_TREMELODEPTH	 0x5C
#define MD_CHORUSDEPTH	 0x5D
#define MD_CELESTEDEPTH	 0x5E
#define MD_PHASERDEPTH	 0x5F
#define MD_DATAINCREMENT	 0x60
#define MD_DATADECREMENT	 0x61
#define MD_NRPN_LSB		 0x62	/* Non Registered Parameter Number */
#define MD_NRPN_MSB		 0x63
#define MD_RPN_LSB		 0x64	/* Registered Parameter Number */
#define MD_RPN_MSB		 0x65

#define MD_CHANNELMODEMESSAGES 0x79
#define MD_RESETALLCONTROLLERS 0x79
#define MD_LOCALCONTROL	 0x7A
#define MD_ALLNOTESOFF	 0x7B
#define MD_OMNIMODEOFF	 0x7C
#define MD_OMNIMODEON	 0x7D
#define MD_MONOMODEON	 0x7E
#define MD_POLYMODEON	 0x7F

#ifdef __cplusplus
}
#endif

#endif /* !__MD_H__ */
