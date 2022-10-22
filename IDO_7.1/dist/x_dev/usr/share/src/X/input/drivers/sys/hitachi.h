#ifndef _HITACHI_H_
#define _HITACHI_H_ 1

	/*
	 * Driver for Hitachi HDG-J series tablet running in
	 * Hitachi III mode.
	 */

#define HITACHI_NAME              "hitachi"
#define HITACHI_TYPE              "TABLET"
#define HITACHI_NUM_BUTTONS       4
#define HITACHI_ALL_BUTTONS       0xf

/*
 * We operate the tablet in mm units.  
 */
#define HITACHI_ACTIVE_WIDTH	228.6
#define HITACHI_ACTIVE_HEIGHT	152.4

/*
 * We operate the tablet at this resolution.  User may change from
 * menu on tablet. The mode string define HTCHI_RESOLUTION in
 * hitachi.c must set the tablet to this defined resolution.
 */
#define HITACHI_RESOLUTION  0.01

/*
 * Maximum X and Y values depend on resolution selected.
 * These values are the maximum from a table of the given size.
 */
#define HITACHI_NUM_AXES	  2
#define HITACHI_NUM_VALUATORS     6
#define HITACHI_X_VALUATOR_MIN    0
#define HITACHI_X_VALUATOR_MAX    ((int)(HITACHI_ACTIVE_WIDTH / HITACHI_RESOLUTION))
#define HITACHI_Y_VALUATOR_MIN    0
#define HITACHI_Y_VALUATOR_MAX    ((int)(HITACHI_ACTIVE_HEIGHT / HITACHI_RESOLUTION))
#define HITACHI_PRESSURE_VALUATOR_MIN      0
#define HITACHI_PRESSURE_VALUATOR_MAX      127
/*
 * If the MIN and MAX values for height ever become different than those
 * for PRESSURE, the code in htchi_open must be changed to initialize
 * the valuator descriptor correctly.
 */
#define HITACHI_HEIGHT_VALUATOR_MIN	  0
#define HITACHI_HEIGHT_VALUATOR_MAX	  127
#define HITACHI_TILT_VALUATOR_MIN	-63
#define HITACHI_TILT_VALUATOR_MAX	+63

/*
 *Resolution of valuator in units/inch
 */
#define HITACHI_AXIS_VALUATOR_RESOLUTION     ((int)(25.4/HITACHI_RESOLUTION))
#define HITACHI_AXIS_VALUATOR_MIN_RESOLUTION 102
#define HITACHI_AXIS_VALUATOR_MAX_RESOLUTION 2540

/*
 * Which valuators to enable at initialization
 */
#define HITACHI_ALL_VALUATORS     0x03

#define HITACHI_FLAG_BIT        0x80
#define HITACHI_NOT_SYNCHED     -1
#define HITACHI_FLAGS           0
#define HITACHI_HIGH_X          1
#define HITACHI_LOW_X           2
#define HITACHI_KEYS		3
#define HITACHI_HIGH_Y          4
#define HITACHI_LOW_Y           5
#define HITACHI_PRESSURE        6
#define HITACHI_HEIGHT		7
#define HITACHI_XTILT		8
#define HITACHI_YTILT		9

#ifdef _KERNEL
typedef struct htchi_state {
        idevInfo                info;

        int                     state;
        idevValuatorState       vals;

        idevValuatorDesc        vdesc[HITACHI_NUM_VALUATORS];
        idevTransform           vtrans[HITACHI_NUM_VALUATORS];
        int                     vstate[HITACHI_NUM_VALUATORS];
        unsigned char           vactive;

        unsigned char           bactive;
        unsigned char           bstate;

	struct hflags {
	    unsigned char	sync:1, prox:1, menu:1, pick_up:2, fill:1, topx:2;
	} flags;
	unsigned char		keys;

        int                     xsign;
        int                     ysign;

        unsigned char           btn_pressure;
        unsigned char           pad0;
        unsigned short          pad1;

        char                    inside;

        /* Misc. Std. Driver stuff */
        unsigned char initialized;/* true if data structures are set */
                                /* and init sequence has been sent to */
                                /* device */
        unsigned char ready;    /* true if we've received confirmation */
                                /* of the init sequence from the device */
} htchi_state_t;

#endif /* _KERNEL */

#endif /* _HITACHI_H_ */

