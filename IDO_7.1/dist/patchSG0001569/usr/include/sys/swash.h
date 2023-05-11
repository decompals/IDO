/**************************************************************************
 *                                                                        *
 *               Copyright (C) 1996 Silicon Graphics, Inc.                *
 *                                                                        *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *                                                                        *
 **************************************************************************/

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

struct swash_cb_state {
	caddr_t pc;
	caddr_t swashp;
	unsigned long long custom;
};

typedef unsigned long swash;

#endif

/* Offsets in swash_cb_state, above */
#define SWASH_PC 	0
#define SWASH_SWASH	(SWASH_PC+4)
#define SWASH_CUSTOM	(SWASH_SWASH+4)


/* Constants for
 *    syssgi(SGI_SWASH_INIT, SWASH_VERSION, handler, &state, &flags) 
 */

/* Versions currently supported are 0 and 1 */
#define SWASH_VERSION 1

/* flags returned in the fifth param to syssgi() */
#define SWASH_VCE_AVOIDANCE	1


/* swash construction: these bits and shifts are defined to be equal to
 * PG_M and PG_VR on IP22, among others, to minimize instruction count
 * in swash_tlbservice().
 */
#define SWASH_RW    6
#define SWASH_RO    2
#define SWASH_INV   0
#define SWASH_PERMMASK 6

#define SWASH_USRSHIFT 0
#define SWASH_SUPSHIFT 2
#define SWASH_USRMASK  6
#define SWASH_SUPMASK  (6<<2)

/* swash make_swash(int smode, int umode, void *vaddr); */

#define make_swash(smode, umode, vaddr) \
	((0xff800000<<5) | (((unsigned long)(vaddr)>>PNUMSHFT)<<(2+5)) | \
	 ((smode)<<SWASH_SUPSHIFT)| ((umode)<<SWASH_USRSHIFT))


/* 1024 segments in a segment table, so a segment covers
 * 2**32 / 1024 (0x400000) bytes of each 32-bit 
 * addr space.
 */
#define SWASH_SEGSHIFT 22
#define SWASH_SEGSIZE  (1<<SWASH_SEGSHIFT)
#define SWASH_SEGMASK  (SWASH_SEGSIZE-1)

#define SWASH_SIZE	4
#define SWASH_SHIFT	2

/* High bit in the 8 bit ASID indicates supervisor mode */
#define SWASH_SUPASID	39
#define SWASH_MAXASID	255

#ifdef _KERNEL
#define KSWASHSEG_HIWORD 0xc0000000
#endif /* _KERNEL */

#ifndef _KERNEL
/* These defines are required to get sys/immu.h and sys/sbd.h to do the right
 * thing.  IOW, sys/swash.h should be #include'd before these files.
 */

#define _VCE_AVOIDANCE
#define R4000 1
#define _PAGESZ 4096
typedef struct pte {
	uint_t	opaque_pte;
} pte_t;

#define cachecolormask CACHECOLORMASK 

#endif /* !_KERNEL */
