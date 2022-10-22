#ifndef XXX_H
#define XXX_H 1

#define	XXX_NAME		"XXX"
#define	XXX_TYPE		"XXX"
#define	XXX_NUM_BUTTONS		IDEV_MAX_BUTTONS

#define	XXX_NUM_VALUATORS		IDEV_MAX_VALUATORS
#define	XXX_VALUATOR_MIN		0
#define	XXX_VALUATOR_MAX		65000
#define	XXX_VALUATOR_RESOLUTION		200
#define	XXX_VALUATOR_MIN_RESOLUTION	200
#define	XXX_VALUATOR_MAX_RESOLUTION	200

#define	XXX_NUM_LEDS		IDEV_MAX_LEDS

#define	XXX_NUM_STR_DPYS	IDEV_MAX_STR_DPYS
#define	XXX_STR_DPY_MAX_LEN	128
#define	XXX_STR_DPY_SYMBOL_SET	"CAPS"
#define	XXX_STR_DPY_ALL_SAME	1

#define	XXX_NUM_INT_DPYS	IDEV_MAX_INT_DPYS
#define	XXX_INT_DPY_MIN		0
#define	XXX_INT_DPY_MAX		99
#define	XXX_INT_DPY_ALL_SAME	1
#define	XXX_INT_DPY_RESOLUTION	2

#define	XXX_NUM_BELLS		1

#define	XXX_FLAGS		IDEV_HAS_KEYMAP
#define	XXX_KEYMAP_NAME		"SGI Standard"

#ifdef _KERNEL
typedef struct xxx_state {
	idevInfo		info;

	idevValuatorDesc	vdesc[XXX_NUM_VALUATORS];
	idevTransform		vtrans[XXX_NUM_VALUATORS];

	unsigned char		vactive[idevSize(XXX_NUM_VALUATORS)];
	int			vstate[XXX_NUM_VALUATORS];

	unsigned char		bactive[idevSize(XXX_NUM_BUTTONS)];
	unsigned char		bstate[idevSize(XXX_NUM_BUTTONS)];

	unsigned char		ledstate[idevSize(XXX_NUM_LEDS)];

	unsigned char		sdpyon[idevSize(XXX_NUM_STR_DPYS)];
	char			sdpystate[XXX_NUM_STR_DPYS]
					   [XXX_STR_DPY_MAX_LEN+1];

	unsigned char		idpyon[idevSize(XXX_NUM_INT_DPYS)];
	short			idpystate[XXX_NUM_INT_DPYS];

	/* Misc. Std. Driver stuff */
	unsigned char initialized;/* true if data structures are set */
				/* and init sequence has been sent to */
				/* device */
	unsigned char ready1;	/* true if we've received confirmation */
				/* of the init sequence from the device */
	unsigned char errorCount;/* count of bad messages for recovery */
} xxx_state_t;

#endif /* _KERNEL */

#endif
