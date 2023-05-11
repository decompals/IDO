/**************************************************************************
 *									  *
 * 		 Copyright (C) 1986, Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/
#ifndef BVO_H
#define BVO_H

/* Set bvo register tokens */
#define BVO_SUBCPHASE_REG	(VID_BVO |0x01)    /* value (0x000-0x1ff) */
#define BVO_HPHASE_REG		(VID_BVO |0x02)	   /* value (0x000-0x1ff) */
#define BVO_MODE_REG		(VID_BVO |0x03)	   /* mode/status register */
#define BVO_STAT_REG		(VID_BVO |0x05)	   /* bvo board status */


/* Masks for BVO_MODE_REG bit assignments */
#define BVO_ENCODER_ENABLE	0x01
#define BVO_SUBCARRIER_SUPR	0x08
#define BVO_BLEND_ENABLE	0x10
#define BVO_SETOUT_SET 		0x40
#define BVO_SETIN_SET 		0x80
#define BVO_CHROMA25_ATTEN	0x100
#define BVO_LUMA25_ATTEN	0x200
#define BVO_GENSYNC_SET		0x400
#define BVO_GSYNC_PAL_ID	0x800
#define BVO_DISABLE_VCO		0x1000
#define BVO_DISABLE_TXCO	0x2000
#define BVO_HSEL_SET		0x10000
#define BVO_HILEVEL_SET		0x1c0000
#define BVO_TTL_SYNCFIL_VAL	0x3
#define BVO_LORES_SYNCFIL_VAL	0x5
#define BVO_HIRES_SYNCFIL_VAL	0x6

#define	BVO_BOARD_STATUS	0xe000000
#define BVO_BOARD_STAT_0	0x2000000
#define BVO_BOARD_STAT_1	0x4000000
#define BVO_BOARD_STAT_2	0x8000000
#define	BVO_NO_BOARD    	0x7
#define	BVO_NTSC_BOARD  	0x0
#define	BVO_PAL_BOARD   	0x1

/* Masks for BVO_STAT_REG bit assignments */
#define BVO_HPLL_LOCK		0x200000
#define BVO_BPLL_LOCK		0x400000
#define BVO_VSYNC_DETECT	0x800000
#define BVO_HSYNC_DETECT	0x1000000

#endif
