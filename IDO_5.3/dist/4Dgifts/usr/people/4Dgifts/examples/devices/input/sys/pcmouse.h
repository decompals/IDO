#ifndef __SYS_PCMOUSE_H__
#define __SYS_PCMOUSE_H__

#define	PCMOUSE_NAME		"mouse"
#define	PCMOUSE_TYPE		"MOUSE"
#define	PCMOUSE_NUM_BUTTONS	3

#define	PCMOUSE_NUM_VALUATORS		2
#define	PCMOUSE_VALUATOR_MIN		0
#define	PCMOUSE_VALUATOR_MAX		65000
#define	PCMOUSE_VALUATOR_RESOLUTION	200
#define	PCMOUSE_VALUATOR_MIN_RESOLUTION	200
#define	PCMOUSE_VALUATOR_MAX_RESOLUTION	200

#define	PCMOUSE_NUM_LEDS	0
#define	PCMOUSE_NUM_STR_DPYS	0
#define	PCMOUSE_NUM_INT_DPYS	0
#define	PCMOUSE_NUM_BELLS	0
#define	PCMOUSE_FLAGS		0

#define	PCMOUSE_FLAG		0x08
#define	PCMOUSE_BUTTONS		0
#define	PCMOUSE_X		1
#define	PCMOUSE_Y		2

#ifdef _KERNEL
typedef struct pcmouse_state {
	idevInfo		info;

	idevValuatorDesc	vdesc[PCMOUSE_NUM_VALUATORS];
	idevTransform		vtrans[PCMOUSE_NUM_VALUATORS];

	unsigned char		vactive;
	int			vstate[PCMOUSE_NUM_VALUATORS];

	unsigned char		bactive;
	unsigned char		bstate;

	unsigned char		flags;
	char			state;
	signed char		deltaX;
	signed char		deltaY;

	unsigned char dropsync; /* drop next char to try to sync back up */
} pcmouse_state_t;

#endif /* _KERNEL */

#endif
