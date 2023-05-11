#ifndef __CORE_OUT_H__
#define __CORE_OUT_H__
/**************************************************************************
 *									  *
 * 		 Copyright (C) 1986,1989 Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/

#ident	"$Revision: 1.5 $"

/*
 * Core file format
 *
 * The coreout struct lives at offset 0 in the core file.
 */
#define CORE_NIDESC	32		/* max # of info descriptors */
#define	CORE_NAMESIZE	80		/* maximum process name length */
#define	CORE_ARGSIZE	80		/* maximum process arguments length */

struct coreout {
	int		c_magic;	/* core magic number */
	int		c_version;	/* corefile version # */
	unsigned	c_vmapoffset;	/* byte offset to start of vmaps */
	int		c_nvmap;	/* # of vmaps */
	char		c_name[CORE_NAMESIZE];
					/* name of process (as in ps) */
	char		c_args[CORE_ARGSIZE];
					/* process arguments (as in ps) */
	int		c_sigcause;	/* signal that caused dump */

	struct idesc {
		unsigned i_offset;	/* byte offset to descriptor */
		unsigned i_len;		/* descriptor length in bytes */
		unsigned i_flags;	/* flags */
	} c_idesc[CORE_NIDESC];		/* information descriptors */
};

#define CORE_MAGIC	0xdeadadb0
#define	CORE_VERSION1	1

/* map of a virtual space in a process */
struct vmap {
	unsigned v_vaddr;		/* virtual address */
	unsigned v_len;			/* length in bytes */
	unsigned v_offset;		/* offset in bytes from start of file */
	ushort   v_flags;		/* flags */
	ushort   v_type;		/* type of space */
};

/* v_flags */
#define VDUMPED		0x1		/* space was dumped in core file */

/* v_type */
#define VTEXT		1		/* space is text */
#define VDATA		2		/* space is data/bss space */
#define VSTACK		3		/* space is stack */
#define VSHMEM		4		/* space is shared mem */
#define VLIBTEXT	5		/* space is shd lib text */
#define VLIBDATA	6		/* space is shd lib data */
#define	VGRAPHICS	7		/* space is graphics hardware */
#define	VMAPFILE	8		/* space is memory mapped file */

/* i_flags values */
#define IVALID		0x1		/* descriptor is valid */

/* indexes into idesc array */
#define	I_GPREGS	0		/* 32 general purpose registers */


#define	I_FPREGS	1		/* 32 floating point registers */

#define I_SPECREGS	2		/* special purpose control registers
					 * int EPC, CAUSE, BADVADDR, MDHI, MDLO
					 * int fpcsr, fpeir
					 */

#define I_SIGHANDLER	3		/* signal handlers
					 * int *signal[MAXSIG]
					 */

#define I_EXDATA	4		/* exec data
					 * int tsize, dsize, bsize
					 */
#endif /* ! __CORE_OUT_H__ */
