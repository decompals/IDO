/**************************************************************************
 *									  *
 * 		 Copyright (C) 1986-1993, Silicon Graphics, Inc.	  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifndef __SYS_REGION_H__
#define __SYS_REGION_H__

#ident	"$Revision: 3.84 $"

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/sema.h>
#include <sys/mman.h>
#include <sys/avl.h>

/*	Each process has a number of {p}regions which describe
 *	contiguous address spaces attached to the process.
 *	Process-specific attributes are kept in the pregion
 *	structure itself -- these include attach address,
 *	protections, (hooks to the) page table maps, etc.
 *
 *	Process-independent attributes are kept in the region
 *	structure.  This includes hooks to the anonymous
 *	memory manager for the region, region size and ref count,
 *	and a lock used to single-thread vfault/page access.
 *
 *	The region lock protects most of the region struct fields
 *	except for r_nofree, which is covered by the mreg_lock
 *	spin lock, and r_noshrink, which is updated using atomic
 *	ll/sc ops.  The region lock also protects the fields in
 *	the pregion (most notably, the attributes) and the entries
 *	in the pmap for the region.
 */

struct vhandl;
/*
 * Per region descriptor.
 */

typedef	struct	region	{
	mrlock_t	r_lock;		/* lock	for reg, attrs, pmap, & anon  */
	uchar_t		r_type;		/* type of region OBSOLETE	      */
	uchar_t		r_color;	/* virtual address coherence color    */
	ushort		r_refcnt; 	/* number of users pointing at region */
	ushort		r_nofree;	/* # of requests not to allow freereg */
	ushort		r_flags;	/* Various flags defined below.	      */
	int		r_noshrink;	/* # of requests not to allow shrink  */
	int		(*r_fault)(struct vhandl *, void *, caddr_t, int);
	union {
		void	*g_fltarg;	/* arg to (*r_fault)()		      */
		unsigned int g_gen;	/* for randomizing cachekey	      */
	} r_g;
	pgcnt_t	r_pgsz;			/* size in pages		      */
	off_t		r_fileoff;	/* File offset of this mapping.	      */
	off_t		r_maxfsize;	/* Max growable size of the file      */
					/* from which this region is loaded.  */
	off_t		r_mappedsize;	/* size in bytes of original mapping  */
	anon_hdl	r_anon;		/* Handle to anon memory manager.     */
	struct vnode   *r_vnode;	/* pointer to vnode where blocks are  */
	struct vnode   *r_loadvnode;	/* loadreg vnode (for PIOCOPENM)      */
#ifdef USE_LGPAGES
	void            *r_lpghdl;	/* handle to find hashed large pages  */
	uchar_t         r_lpgszshft;    /* (1 << r_lpgszshft) --> page size   */
	uchar_t         r_lpgwire;      /* boolean: wire page into tlb or not */
#endif
	void		*r_spans;	/* unreserved spans		*/
} reg_t;

#define r_fltarg	r_g.g_fltarg
#define r_gen		r_g.g_gen

/*
 * Region flags
 */
#define	RG_NOFREE	0x0001	/* Don't free region on last detach */
#define RG_USYNC	0x0002  /* call usync module before freeing */
#define RG_CW		0x0008	/* cw region */
#define	RG_DFILL	0x0010	/* Demand fill pages (not demand zero) */
#define	RG_AUTOGROW	0x0020	/* Pages become autogrow after ip shrinks */
#define	RG_PHYS		0x0040	/* Region describes physical memory */
#define	RG_ISOLATE	0x0080	/* Region is isolated for real-time, no aging */
#define	RG_ANON		0x0100	/* (some) pages mapped anonymously */
#define	RG_GROW		0x0200	/* growable region (data) */
#define RG_HASSANON	0x0400	/* region has SANON pages */
#define RG_TEXT		0x0800	/* region contains pure, read-only text */
				/* (unmodified with respect to a.out)   */
#define RG_AUTORESRV	0x1000	/* reserve availsmem on demand when page*/
				/* goes anonymous			*/
#define RG_LGPAGES      0x2000  /* region is using very large pages */
#define	RG_PHYSIO	0x4000	/* Region maps a physical device space	     */

#ifdef R10000_SPECULATION_WAR
#define	RG_LOCKED	0x8000	/* region is locked down */
#endif	/* R10000_SPECULATION_WAR */

/* RG_PHYSIO can be set only if RG_PHYS is set.  A Region should never 
 * have RG_PHYSIO Set  without RG_PHYS being set.
 * RG_PHYS -> Regions maps physical memory.  
 * RG_PHYS|RG_PHYSIO -> Region maps physical device space (e.g. VME Bus space)
 *
 * This provides a mechanism for separating  Regions that map 
 * physical memory space, and those that map physical device space. 
 * This separation is used ONLY while handling dbx attaching a process 
 * that maps one of these regions, and user tries to dump virtual address 
 * represented by these regions. If only RG_PHYS is set, there are
 * no restrictions. If RG_PHYS|RG_PHYSIO is set, accesses greater than
 * 8 bytes are disallowed. 
 */

#define NOCOLOR	128

/*
 * Region types
 *
 * XXX	These should all go away, replaced by flag values.
 */
#define	RT_UNUSED	0	/* Region not being used.	*/
#define	RT_MEM		3	/* Regular swappable region */
#define RT_MAPFILE	4	/* Mapped file region 		*/

typedef struct pattribute {
	uint	prot	:  3,	/* read, write, execute	*/
		cc	:  3,	/* cache coherency	*/
		watchpt	:  1,	/* page(s) being watched*/
			:  9,
		lockcnt	: 16;	/* page(s)' lock count	*/
} at_t;

typedef struct pageattr {
	struct pageattr	*attr_next;
	char		*attr_start;
	char	 	*attr_end;
	union {
		uint	attributes;
		at_t	at;
	} at;
} attr_t;

#define	attr_attrs	at.attributes
#define	attr_prot	at.at.prot
#define	attr_cc		at.at.cc
#define	attr_nc		at.at.nc
#define	attr_watchpt	at.at.watchpt
#define	attr_lockcnt	at.at.lockcnt

/*	Pregion protections.  These are kept the same as those in mman.h
 *	so we don't have to translate flags for mmap and mprotect.
 */

#define	PROT_R		PROT_READ	/* Read permissions.		*/
#define	PROT_W		PROT_WRITE	/* Write permissions.		*/
#define	PROT_X		PROT_EXEC	/* Execute permissions.		*/
#define PROT_RW		(PROT_R|PROT_W)	/* convenient shorthands	*/
#define PROT_RX		(PROT_R|PROT_X)
#define PROT_WX		(PROT_W|PROT_X)
#define PROT_RWX	(PROT_R|PROT_W|PROT_X)



typedef struct pregion {
	/***** avlnode has to be first in struct **** 			*/
	avlnode_t	 p_avlnode;	/* each pregion is an avl node  */
	/***** Do not move avlnode from begining of struct ****		*/
	attr_t		 p_attrs;	/* Pregion protections.		*/
	attr_t		*p_attrhint;	/* last attr_t accessed.	*/
	struct region	*p_reg;		/* Pointer to the region.	*/
	pgcnt_t		 p_offset;	/* Offset into reg where preg   */
					/* begins                       */
	pgcnt_t		 p_pglen;	/* Size of pregion in pages     */
	uchar_t		 p_maxprots; 	/* Max protections allowed.	*/
	uchar_t		 p_type;	/* Type.			*/
	ushort		 p_flags;	/* Flags.			*/
	int              p_stk_rglen;   /* r_pgsz at pregion attach     */
	pgcnt_t		 p_pghnd;	/* getpages index into region	*/
	pgcnt_t		 p_nvalid;	/* nbr of valid pages in region	*/
	struct pmap	*p_pmap;	/* pmap handle			*/
	struct pregion	*p_vchain;	/* Link on inode chain.		*/
} preg_t;

typedef 	avltree_desc_t 	preg_set_t;

/*
 * 	Macros to manipulate preg_set_t, or its elements.
 * PREG_FIRST:
 * 	Given a set of pregions (prset) [NOT a pointer to it], extract first.
 * PREG_NEXT:
 * 	Given an element (prp), extract next.
 *
 * Modifying functions:
 * -------------------
 *
 * PREG_INIT(prsetp):
 * 	Given a pointer to set of pregions (prsetp), initialize to null set .
 * 
 * PREG_DELETE(prsetp, prp):
 * 	Given a pointer to a set (prpsetp), and an element (prp),
 *		delete element from set; tree balanced if necessary.
 *
 * PREG_INSERT(prsetp, prp):
 * 	Given a pointer to a set (prpsetp), and an element (prp),
 *		insert element into set; tree balanced if necessary.
 *
 * Maintaining regions only as a list (no tree):
 * ---------------------------------------------
 *
 * PREG_SET_NEXT_LINK:
 * 	For efficiency, the elements in the set may be linked on a linear chain
 *	(esp. for the use of tsave, the list of regions saved on exec).
 *	This macro sets the next element in such a chain.
 * PREG_LINK_LVALUE:
 *	Gives the address of link used in a chain;
 *	used in conjunction with tsave.
 *
 * Search functions:
 * ----------------
 *
 * PREG_FINDRANGE(prsetp, value)
 *	given a value, find an element of set prsetp which contains value.
 *
 * PREG_FINDANYRANGE(prsetp, start, end, checklen)
 *	Return any element of set prsetp. Element should correspond to
 *	range contained in [start, end). If checklen is PREG_EXCLUDE_ZEROLEN,
 * 	the size of the range should be non-zero for a hit.
 *
 * PREG_FINDADJACENT(prsetp, value, dir):
 *	Find range r, such that 
 *		1. value is in range (similar to FINDRANGE)
 *	 (OR)	2. range immediately follows value (dir == PREG_SUCCEED) or
 *			range immediately preceeds value (dir == PREG_PRECEED).
 */

extern avlops_t preg_avlops;		/* ops vector for pregion avl trees */

#define PREG_INIT(prsetp)		avl_init_tree(prsetp, &preg_avlops)
#define PREG_DELETE(prsetp, prp)	avl_delete(prsetp, (avlnode_t *) prp) 
#define PREG_INSERT(prsetp, prp) 	avl_insert(prsetp, (avlnode_t *) prp)

#define PREG_LINK_LVALUE(prp)   	((preg_t **) &(prp->p_avlnode.avl_forw))
#define PREG_GET_NEXT_LINK(prp) 	((preg_t *) prp->p_avlnode.avl_forw)
#define PREG_SET_NEXT_LINK(prp, nextp) 	\
				prp->p_avlnode.avl_forw = (avlnode_t *) nextp

#define PREG_FIRST(prset)		((preg_t *) ((prset).avl_firstino))
#define	PREG_NEXT(prp)			((preg_t *) ((prp)->p_avlnode.avl_nextino))

#define PREG_INSERT_IMMEDIATE(prsetp, afterp, prp) 	\
	avl_insert_immediate(prsetp, (avlnode_t *) afterp, (avlnode_t *) prp) 

#define PREG_FINDRANGE(prsetp, value)			\
	(preg_t *) avl_findrange(prsetp, (__psunsigned_t) (value))

#define	PREG_FINDANYRANGE(prsetp, start, end, checklen)		\
	(preg_t *) avl_findanyrange(prsetp, start, end, checklen)

#define	PREG_FINDADJACENT(prsetp, value, dir)		\
	(preg_t *) avl_findadjacent(prsetp, value, dir)

/* SPT
 * If a pregion is using Shared Page Tables the p_nvalid count
 * can be inaccurate. To avoid negative values the value should
 * be tested before decrement.
 */
#define	PREG_NVALID_DEC(prp, n)	{ if ((prp)->p_nvalid >= (n)) \
					(prp)->p_nvalid -= (n); }

/*
 * checklen values for FINDANYRANGE - includes or exclues
 * zerolength regions for finding match.
 */
#define PREG_INCLUDE_ZEROLEN		AVL_INCLUDE_ZEROLEN
#define PREG_EXCLUDE_ZEROLEN		AVL_EXCLUDE_ZEROLEN

/*
 * dir values for FINDADJACENT
 */
#define PREG_SUCCEED			AVL_SUCCEED
#define PREG_PRECEED			AVL_PRECEED


#define	p_regva		p_attrs.attr_start

/*	Pregion types.
 *
 *	Note: do not change these without checking core() first.
 */

#define	PT_UNUSED	0x00		/* Unused region.		*/
#define	PT_TEXT		0x01		/* Text region.			*/
#define	PT_DATA		0x02		/* Data region.			*/
#define	PT_STACK	0x03		/* Stack region.		*/
#define	PT_SHMEM	0x04		/* Shared memory region.	*/
#define	PT_MEM		0x05		/* Generic memory region.	*/
					/* Space was Double mapped memory.*/
#define	PT_LIBTXT	0x06		/* Shared library text region.	*/
#define	PT_LIBDAT	0x07		/* Shared library data region.	*/
#define PT_GR 		0x08		/* Graphics region 		*/
#define PT_MAPFILE	0x09		/* Memory mapped file region 	*/
#define PT_PRDA		0x0a		/* PRDA region			*/

/*	Pregion flags.
 */

#define	PF_DUP		0x01		/* Dup on fork.			*/
#define	PF_NOPTRES	0x02		/* No pmap reservations made	*/
#define PF_NOSHARE	0x04		/* Do not share on sproc	*/
#define PF_SHARED	0x08		/* On shaddr pregion list	*/
#define PF_AUDITR	0x10		/* Read audit			*/
#define PF_AUDITW	0x20		/* Write audit			*/
#define	PF_FAULT	0x40		/* Vfault logging enabled	*/
#define	PF_TSAVE	0x80		/* if a tsave'd  pregion 	*/
#define PF_LGPAGES      0x100           /* pregion has large pages      */
#define PF_WIREDPGS     0x200           /* pregion has specially wired pages */
#define PF_KLOCKPRDA    0x400           /* prda was locked by the kernel */
#define PF_LCKNPROG     0x800           /* pregion lockdown in progress  */


#define PREG_MARK_TSAVE(prp)	prp->p_flags |= PF_TSAVE
#define PREG_UNMARK_TSAVE(prp)	prp->p_flags &= ~PF_TSAVE
#define PREG_IS_TSAVE(prp)	(prp->p_flags & PF_TSAVE)


/*	Several ``logical page numbers'' are needed by various routines:
 *	a monatomically increasing page number used to generate a key for
 *	the page allocator, to ensure that logically successive user pages
 *	get memory with logically successive d- and i-cache indices;
 *	a unique page identifier used by the page cache and anonymous
 *	memory manager.
 */
#define vtorpn(P,V)	(btoct((V) - (P)->p_regva) + (P)->p_offset +    \
				((P)->p_type != PT_STACK ? 0 :  \
					(P)->p_reg->r_pgsz-(P)->p_stk_rglen))

#define vtoapn(P,V)	((P)->p_type == PT_STACK ? \
				(P)->p_reg->r_pgsz - 1 - vtorpn(P,V) : \
				vtorpn(P,V))

#define rpntoapn(P,N)	((P)->p_type == PT_STACK ? \
				(P)->p_reg->r_pgsz - 1 - (N) : \
				(N))

/*	macro to find number of pages left in pregion starting at vaddr 
 */

#define pregpgs(P, V)	((P)->p_pglen - btoct((V) - (P)->p_regva))

/*	``Opaque handle'' on virtual space.
 */

typedef struct vhandl {
	struct pregion	*v_preg;	/* Pointer to the pregion.	*/
	caddr_t		v_addr;		/* Virtual address of region.	*/
} vhandl_t;

#define v_getpreg(VT)	(VT)->v_preg
#define v_getaddr(VT)	(VT)->v_addr
#define v_getlen(VT)	ctob((VT)->v_preg->p_pglen)
#define v_getvnode(VT)	(VT)->v_preg->p_reg->r_vnode
#define v_gethandle(VT)	(__psunsigned_t)(VT)->v_preg->p_reg

/*	Some generic flags for region routines.
 */
#define RF_FORCE	0x0002		/* force a private region */
#define RF_NOFLUSH	0x0004		/* do not flush tlbs (detachreg) */
#define RF_TSAVE	0x0008		/* preg is tsave'd (detachreg) */

extern sema_t	rlistlock;		/* Lock for the region list.	*/
extern lock_t	preg_lock;		/* Lock for the pregion list.	*/

extern preg_t	syspreg;	/* Pregion for dynamic system space */
extern reg_t	sysreg;		/* Region for dynamic system space */

struct vnode;
struct user;
struct proc;
struct pfdat;
/* region allocator */
reg_t *allocreg(struct vnode *, uchar_t, ushort);

/* region free routine */
void freereg(reg_t *);

/* Attach region to process. */
int attachreg(reg_t *, struct user *, caddr_t, pgno_t, pgno_t, uchar_t, 
		  uchar_t, uchar_t, ushort, preg_t **);

/* Detach region from process. */
int detachreg(preg_t *, struct user *, caddr_t, pgno_t, int);

/* Duplicate region (fork). */
preg_t *dupreg(preg_t *, struct user *, caddr_t, uchar_t, ushort, int);

/* Grow region. */
int growreg(preg_t *, struct user *, uchar_t, pgno_t);

/* Load region from file to be paged in. */
int mapreg(preg_t *, caddr_t, struct vnode *, off_t, uchar_t, size_t);

/* Load region from file. */
int loadreg(preg_t *, caddr_t, struct vnode *, off_t, size_t);

/* Replace current region with a private copy */
reg_t *replacereg(preg_t *);

/* mark a region as isolated, or not */
void isolatereg(struct proc *, cpuid_t);
void unisolatereg(struct proc *, cpuid_t);

/* routines for execself support */
void unattachpreg(struct proc *, preg_t *);
int reattachpreg(struct proc *, preg_t *);

/* v_* routines for mapped driver support */
int v_mapphys(vhandl_t *, void *, int);
int v_mapreg(vhandl_t *, int, int);
int v_enterpage(vhandl_t *, caddr_t, void *, pgno_t);
int v_initphys(vhandl_t *, int (*)(vhandl_t *, void *, caddr_t, int), int, void *);

/* Find pregion from virtual address. */
preg_t *findreg(struct proc *, caddr_t);

/* Find pregion from virtual address. */
preg_t *vtopreg(struct proc *, caddr_t);

/* Find first pregion within address range. */
preg_t *findfpreg(struct proc *, caddr_t, caddr_t);

/* Find pregion of given type. */
preg_t *findpreg(struct proc *, uchar_t);

/* Find attributes structure for an address within a pregion */
attr_t *findattr(preg_t *, char *);

/* Clip an attribute structure */
attr_t *attr_clip(attr_t *, char *, char *);

/* Find attributes structure that maps (exactly) [start, end) */
attr_t *findattr_range(preg_t *, char *, char *);

/* Same as findattr_range but don't clip or coalesce */
attr_t *findattr_range_noclip(preg_t *, char *, char *);

/* Change protection for region. */
void chgprot(preg_t *, char *, pgno_t, uchar_t);

/* Find apprpriate spot for a region within address space */
caddr_t allocaddr(int, uchar_t);

/* Returns size of specified region in pages */
unsigned getpregionsize(preg_t *);

/* Fault logging routine prototype */
void prfaultlog(struct proc *p, preg_t *prp, caddr_t vaddr, int flag);

/* Returns current size of proces */
pgno_t getpsize(struct proc *);

/* fill in page frame numbers */
int vtop(struct proc *, caddr_t, int, int *, int);
int vtopv(struct proc *, caddr_t, int, int *, int, int, int);

/* invalidate an address range */
void invalidateaddr(struct proc *, preg_t *, caddr_t, size_t);

/* generic PHYS region fault handler */
int genphysfault(struct vhandl *, void *, caddr_t, int);

/* change text region to a PRIVATE writable version */
int getprivatespace(struct proc *, caddr_t, preg_t **);
int ispcwriteable(struct proc *, caddr_t, reg_t *);

/* remove any saved text-style regions */
extern void remove_tsave(struct proc *, int);
extern void removetsave(void);

/* trim pmap */
extern int try_pmap_trim(struct proc *);

/* Test for COW page */
extern	int is_cow_page(struct pfdat *, reg_t *);

/* Initialize the region table. */
void reginit(void);
void reg2init(void);
void vrelvm(void);

#define	reglock(RP)	mrlock(&(RP)->r_lock, MR_UPDATE, PZERO)
#define reglock_rd(RP)	mrlock(&(RP)->r_lock, MR_ACCESS, PZERO)
#define	creglock(RP)	cmrlock(&(RP)->r_lock, MR_UPDATE)
#define	regrele(RP)	{ ASSERT(ismrlocked(&(RP)->r_lock, \
			MR_UPDATE|MR_ACCESS)); mrunlock(&(RP)->r_lock); }
#define reglocked(RP)	(ismrlocked(&(RP)->r_lock, MR_UPDATE|MR_ACCESS))

extern mutex_t	*preglocks;	/* array of pregion locks */
extern int	preglocksmask;	/* size of pregion locks array, - 1 */

#define	preglock(P)	mutex_lock(&preglocks[(P)->p_reglockindx], PZERO)

#define pregunlock(P) { ASSERT(mutex_owner(&preglocks[(P)->p_reglockindx]) == \
				curprocp); \
			mutex_unlock(&preglocks[(P)->p_reglockindx]); }

#ifdef USE_LGPAGES
union rval;
extern sema_t maplpg_sema;
extern int map_lpages(caddr_t, int, int, int, union rval *);
#endif

#ifdef __cplusplus
}
#endif

#endif /* __SYS_REGION_H__ */
