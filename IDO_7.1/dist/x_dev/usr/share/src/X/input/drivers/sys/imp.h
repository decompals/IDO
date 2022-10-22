#ifndef __SYS_IMP_H__
#define __SYS_IMP_H__

#define	IMP_NAME		"imp"
#define	IMP_TYPE		"MOUSE"
#define	IMP_NUM_BUTTONS		5

#define	IMP_NUM_VALUATORS		2
#define	IMP_VALUATOR_MIN		0
#define	IMP_VALUATOR_MAX		65000
#define	IMP_VALUATOR_RESOLUTION	200
#define	IMP_VALUATOR_MIN_RESOLUTION	200
#define	IMP_VALUATOR_MAX_RESOLUTION	200
#define IMP_BIAS_MAX			75
#define IMP_BIAS_DFLT			40

#define	IMP_NUM_LEDS		0
#define	IMP_NUM_STR_DPYS	0
#define	IMP_NUM_INT_DPYS	0
#define	IMP_NUM_BELLS		0
#define	IMP_FLAGS		0

/* Bits withing the reports */
#define	IMP_SYNC		0x40
#define IMP_VALBITS		0x3f
#define IMP_CENTERB		0x80
#define IMP_LFB			0x20
#define IMP_RFB			0x10
#define IMP_LRB			0x80
#define IMP_RRB			0x80

#define IMP_XMITTERID_CHANGED	0x80
#define IMP_BATTERY_LOW		0x10

/* States while parsing input */
#define	IMP_BUTTONS		0
#define	IMP_LOWX		1
#define	IMP_LOWY		2
#define IMP_NUM_STATES		3

/* bits from mouse indicating button pressed */
#define CNBUTTON    0
#define LFBUTTON    1
#define RFBUTTON    2
#define LRBUTTON    3
#define RRBUTTON    4

#define BUTTONZERO  0x01
#define BUTTONONE   0x02
#define BUTTONTWO   0x04
#define BUTTONTHREE 0x08
#define BUTTONFOUR  0x10
#define ALLBUTTONS ( BUTTONZERO | BUTTONONE | BUTTONTWO | BUTTONTHREE | BUTTONFOUR )

#ifdef _KERNEL
typedef struct imp_state {
	idevInfo		info;

	idevValuatorDesc	vdesc[IMP_NUM_VALUATORS];
	idevTransform		vtrans[IMP_NUM_VALUATORS];

	int			vstate[IMP_NUM_VALUATORS];
	unsigned char		vactive;

	unsigned char		bactive;
	unsigned char		bstate;

	unsigned char		flags;
	char			state;
	signed char		deltaX;
	signed char		deltaY;
	unsigned char		report[IMP_NUM_STATES];
	
	signed char		bias;
	char			btnmap[IMP_NUM_BUTTONS];
	
	/* Not sure what to do with these yet */
	char			xmitter_id_changed;
	char			low_battery;

	unsigned char dropsync; /* drop next char to try to sync back up */
	char			wantsignon;
} imp_state_t;

#endif /* _KERNEL */

#endif
