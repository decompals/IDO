#ifndef MS_H
#define MS_H 1

#define	MS_TYPE			"MOUSE"
#define	MS_NAME			"mouse"
#define	MS_NUM_BUTTONS		3

#define	MS_NUM_VALUATORS 	2
#define	MS_VALUATOR_MIN		0
#define	MS_VALUATOR_MAX		65000
#define	MS_VALUATOR_RESOLUTION		200
#define	MS_VALUATOR_MIN_RESOLUTION	200
#define	MS_VALUATOR_MAX_RESOLUTION	200

/* bits from mouse indicating button pressed */
#define RGTBUTTON 0x1
#define MIDBUTTON 0x2
#define LFTBUTTON 0x4
#define ALLBUTTONS ( LFTBUTTON | MIDBUTTON | RGTBUTTON )

/* State machine states */
#define MS_BUT_STATE 0
#define MS_DX1_STATE 1
#define MS_DY1_STATE 2
#define MS_DX2_STATE 3
#define MS_DY2_STATE 4
#define MS_LAST_STATE 5
#define	SZ_MOUSE_STATES	8
#define	MS_STATE_MASK 0x7

#ifdef _KERNEL
typedef struct ms_state {
	idevInfo		info;

	idevValuatorDesc	vdesc[MS_NUM_VALUATORS];
	idevTransform		vtrans[MS_NUM_VALUATORS];

	unsigned char		vactive;
	int			vstate[MS_NUM_VALUATORS];

	unsigned char		bactive;
	unsigned char		bstate;

	/* "Warp" Variable Accelerators */
	unsigned char		m1;
	unsigned char		cM;
	unsigned char		inflection;
	unsigned char		threshold;
	unsigned short int	acceleratorN;
	unsigned short int	acceleratorD;
	int			currentX;
	int			currentY;

	unsigned		state;
	signed char		report[SZ_MOUSE_STATES];

	/* Misc. Std. Driver stuff */
	unsigned char initialized;/* true if data structures are set */
				/* and init sequence has been sent to */
				/* device */
	unsigned char ready;	/* true if we've received confirmation */
				/* of the init sequence from the device */
} ms_state_t;
#endif /* _KERNEL */

#endif
