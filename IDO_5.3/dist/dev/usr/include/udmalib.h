#ifndef __UDMALIB_H__
#define __UDMALIB_H__
#ifdef __cplusplus
extern "C" {
#endif

/*
 * libudma/udmalib.h
 *
 *
 * Copyright 1993, Silicon Graphics, Inc.
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
 */

#ident "$Revision: 1.3 $"

typedef void udmaid_t;
typedef void udmaprm_t;

#define DMA_VMEBUS	0

struct vmeparms_s {
	char	vp_block;		/* 0 - not block */
	char	vp_datumsz;		/* xfer datum size */
	char	vp_dir;			/* direction of xfer */
	char	vp_throt;		/* throttle value */
	char	vp_release;		/* RWD or ROR */
	char	vp_addrmod;		/* VME address modifier */
};
typedef struct vmeparms_s vmeparms_t;


#define VME_DS_BYTE	0
#define VME_DS_HALFWORD	1
#define VME_DS_WORD	2
#define VME_DS_DBLWORD	3

#define VME_THROT_2048	0
#define VME_THROT_256	1

#define VME_REL_RWD	0
#define VME_REL_ROR	1

#define VME_READ	0
#define VME_WRITE	1


/* DMA lib prototypes */
extern udmaid_t *dma_open(int bus, int adap);
extern int dma_close(udmaid_t *dp);
extern void *dma_allocbuf(udmaid_t *dp, int size);
extern int dma_freebuf(udmaid_t *dp, void *bp);
extern int dma_freeparms(udmaid_t *dp, udmaprm_t *dparms);
extern udmaprm_t *dma_mkparms(udmaid_t *dp, void *dinfo, void *iobuf, int size);
extern int dma_start(udmaid_t *dp, void *busaddr, udmaprm_t *dparms);

#ifdef __cplusplus
}
#endif
#endif /* __UDMALIB_H__ */
