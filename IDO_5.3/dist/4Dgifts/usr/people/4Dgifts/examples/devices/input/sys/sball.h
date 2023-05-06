#ifndef SB_H
#define SB_H 1

#define	SB_NAME			"spaceball"
#define	SB_DIAL_EMULATION_NAME	"dial+buttons"
#define	SB_TYPE			"SPACEBALL"
#define	SB_NUM_BUTTONS		9
#define	SB_BUTTON_MASK		(0x1FF)

#define	SB_NUM_VALUATORS	7
#define	SB_VALUATOR_MAX		18000
#define	SB_VALUATOR_MIN		-18000
#define	SB_VALUATOR_RESOLUTION		200
#define	SB_VALUATOR_MIN_RESOLUTION	200
#define	SB_VALUATOR_MAX_RESOLUTION	200
#define	SB_VALUATOR_MASK	(0x7f)

#define	SB_NUM_BELLS		1

#define	SB_MAX_MSG_LEN		60
#define	SB_MAX_VERSION_LEN	8
#define	SB_MAX_DATE_LEN		12

#define	SB_NORMAL_MODE		0
#define	SB_DIAL_EMULATION_MODE	1

#define	SB_NO_PERIOD		0
#define	SB_GEN_PERIOD		1
#define	SB_PERIOD_PREMULT	2

#ifdef _KERNEL

typedef struct sb_state {
	idevInfo		info;

	unsigned		eventMode;

	idevValuatorDesc	vdesc[SB_NUM_VALUATORS];
	idevTransform		vtrans[SB_NUM_VALUATORS];

	unsigned char		vactive;
	int			vstate[SB_NUM_VALUATORS];

	unsigned char		bactive[idevSize(SB_NUM_BUTTONS)];
	unsigned char		bstate[idevSize(SB_NUM_BUTTONS)];

	char			transMode;
	char			rotMode;
	char			rotForm;
	char			spinExp;
	char			spinMantissa;
	char			pad;
	char			zero[32];
	int			(*parseData)(int,char *,int *,int *);
	void			(*genValEvent)(struct sb_state *,int,char *);

	/* Message we are currently parsing */
	char	error;	
	char	msgLen;
	char	msg[SB_MAX_MSG_LEN];

	char	version[SB_MAX_VERSION_LEN];
	char	date[SB_MAX_DATE_LEN];
	char	periodMode;

	/* Misc. Std. Driver stuff */
	unsigned char initialized;/* true if data structures are set */
				/* and init sequence has been sent to */
				/* device */
	unsigned char ready;	/* true if we've received confirmation */
				/* of the init sequence from the device */
} sb_state_t;

#endif /* _KERNEL */

#endif
