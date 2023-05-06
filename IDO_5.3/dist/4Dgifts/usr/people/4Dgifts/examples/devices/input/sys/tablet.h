#ifndef TABLET_H
#define TABLET_H 1

#define	TABLET_NAME		"tablet"
#define	TABLET_TYPE		"TABLET"
#define	TABLET_NUM_BUTTONS	4
#define	TABLET_ALL_BUTTONS	0x0f

#define	TABLET_NUM_VALUATORS	2
#define	TABLET_VALUATOR_MIN	0
#define	TABLET_VALUATOR_MAX	2206
#define	TABLET_VALUATOR_RESOLUTION		200
#define	TABLET_VALUATOR_MIN_RESOLUTION	200
#define	TABLET_VALUATOR_MAX_RESOLUTION	200
#define	TABLET_ALL_VALUATORS	0x3

#define	TABLET_BUTTON_FLAG	0x40
#define	TABLET_NOT_SYNCHED	-1
#define	TABLET_LOW_X		0
#define	TABLET_HIGH_X		1
#define	TABLET_LOW_Y		2
#define	TABLET_HIGH_Y		3
#define	TABLET_BUTTONS		4

#ifdef _KERNEL
typedef struct tbt_state {
	idevInfo		info;

	idevValuatorDesc	vdesc[TABLET_NUM_VALUATORS];
	idevTransform		vtrans[TABLET_NUM_VALUATORS];

	unsigned char		vactive;
	int			vstate[TABLET_NUM_VALUATORS];

	unsigned char		bactive;
	unsigned char		bstate;

	int			state;
	idevValuatorState	vals;

	unsigned char		pad0;
	unsigned short		pad1;


	/* Misc. Std. Driver stuff */
	unsigned char initialized;/* true if data structures are set */
				/* and init sequence has been sent to */
				/* device */
	unsigned char ready;	/* true if we've received confirmation */
				/* of the init sequence from the device */
} tbt_state_t;

#endif /* _KERNEL */

#endif
