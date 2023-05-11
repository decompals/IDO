/**************************************************************************
 *									  *
 * 		 Copyright (C) 1992, Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/

#ifndef __SYS_PMAP_H__
#define __SYS_PMAP_H__

#ifdef __cplusplus
extern "C" {
#endif

#ident	"$Revision: 1.25 $"
#include <sys/region.h>

/*
 *	Page maps.
 *	The page map manages all valid address references in an
 *	address space, whether to pageable memory or to mapped
 *	device space.
 *	
 *	The pmap structure is a collection of mostly opaque counters
 *	and pointers, whose definition and use is supplied by the
 *	particular type of pmap.
 */
typedef struct pmap {
	void   		*pmap_ptr;	/* generic pointer */
	short		pmap_scount;	/* generic short counter */
	uchar_t		pmap_type;	/* pmap type	*/
	uchar_t		pmap_flags;	/* flags	*/
	preg_set_t	*pmap_preg;	/* pointer to address list */
} pmap_t;

/*	pmap_types
*/
#define PMAP_INACTIVE	0
#define PMAP_SEGMENT	1
#define PMAP_TRILEVEL	2
#define PMAP_FORWARD	3
#define PMAP_REVERSE	4
#define PMAP_LGPAGES    5

/* pmap flags */
#define PMAPFLAG_NOTRIM		1
#define PMAPFLAG_TLBWIRED	2
#define	PMAPFLAG_LOCAL		4
#define	PMAPFLAG_SPT		8

/* SPT macros */
#define pmap_spt(p)		((p) && ((p)->pmap_flags & PMAPFLAG_SPT))
#define	pmap_spt_set(p)		((p)->pmap_flags |= PMAPFLAG_SPT)
#define pmap_spt_unset(p)	((p)->pmap_flags &= ~PMAPFLAG_SPT)

#ifdef USE_LGPAGES
/*
 * Different overlay of pmap_t, one for the pmap
 * used for mapping of large pages.
 */
typedef struct pmap_lpg {
	pde_t           **pseg_segptr;  /* pointer to segment table(s) */
	short		plpg_flags:4,
			pseg_segcnt:12; /* count of page tables */
 	uchar_t         plpg_type;      /* pmap type    */
	uchar_t         plpg_shft;      /* shift factor to get size of pages */
      	preg_set_t      *plpg_preg;     /* ptr to address list */
} plpg_t;

#define get_lpgshft(X)  (((plpg_t *)(X))->plpg_shft)
#define get_lpgsize(X)  (1 << ((plpg_t *)(X))->plpg_shft)


#endif /* USE_LGPAGES */

/* PTPOOL: page table pooling */

typedef struct ptpool_s {
	lock_t  lock;
	void    *head;
	uint_t  cnt;
	uint_t  maxcnt;
} ptpool_t;

typedef	struct ptpool_stat_s {
	ulong_t hit;
	ulong_t miss;
	ulong_t	mem;
} ptpool_stat_t;


/*	Special pmap marker used within a page table entry
 *	to indicate that a shared page may exist for that
 *	virtual page and that the shared pmap should be
 *	used instead.  Used by utlbmiss (and vfault?).
 */

#define SHRD_SENTINEL	(PDEFER_SENTRY << PTE_PFNSHFT)

/*
 *	pmap locking:
 *
 *	Callers to pmap routines that return pointers to pmap entries
 *	must either have 1) p_aspacelock held (for, at least, access);
 *	or 2) the pmap_lock mutex.  Pmap entries can only be removed
 *	by a pmap_destroy call (which requires p_aspacelock for update,
 *	and is only called by an exitting process), or by pmap_trim,
 *	which requires both the address space lock and pmap_lock.
 *
 *	Note, then, that a process accessing pmap entries within its
 *	own address (pmap) space are guaranteed that (pointers to)
 *	entries will persist as long as the access lock is held.
 *	(Note, too, that region locks still control the _content_ of
 *	the pmap entries, the address space lock just ensures their
 *	existence.)  
 *
 *	A process that is accessing pmap entries mapped to a different
 *	address space must ensure that the other process doesn't exit
 *	(and, thus, call pmap_destroy).
 */

extern mutex_t	pmap_lock;
#define	pmaplock()	mutex_lock(&pmap_lock, PZERO)
#define	cpmaplock()	mutex_trylock(&pmap_lock)
#define	pmapunlock()	{ ASSERT(mutex_owner(&pmap_lock)); \
			mutex_unlock(&pmap_lock); }

/*
 * Shared Page Tables Descriptor.
 */

typedef void 	*pmap_sptid_t;

typedef struct pmap_sptdesc_s {
	struct pmap_sptdesc_s	*spt_next;
	struct pmap_sptdesc_s	*spt_prev;
	pmap_sptid_t		spt_id;
	uint_t			spt_type;
	addr_t			spt_addr;
	size_t			spt_size;
	pmap_t			*spt_pmap;
} pmap_sptdesc_t;

/*
 *	External pmap interface and function prototypes:
 */
struct proc;
union pde;

/*
 *	System pmap initialization routine.
 */
extern void	init_pmap(void);

/*
 *	Initialize, resume, zap segment handler.
 */
extern void	initsegtbl(struct proc *);
extern void	segresume(struct proc *);
extern void	zapsegtbl(struct proc *);


/*
 *	Initialize pmap -- called on creation of a new address space.
 */
extern pmap_t	*pmap_create(preg_set_t *, struct proc *, int, int);

/*
 *	Destroy pmap -- called on last reference to pmap.
 */
extern void	pmap_destroy(pmap_t *);

/*
 *	pmap_pte(pmap_t *pmap, char *vaddr, int vmflag)
 *
 *	Return a pointer to pmap entry representing vaddr.
 */
extern pde_t	*pmap_pte(pmap_t *, char *, int);

/*
 *	pmap_ptes(pmap_t *pmap, char *vaddr, pgno_t *size, int vmflag)
 *
 *	Return a pointer to pmap entries representing vaddr.
 *	Number of entries of interest passed in size;
 *	number of contiguous entries returned in size.
 */
extern pde_t	*pmap_ptes(pmap_t *, char *, pgno_t *, int);

/*
 *	pmap_probe(pmap_t *pmap, char **vaddr, pgno_t *sizein, pgno_t *sizeout)
 *
 *	Return a pointer to the first extant pmap entry that
 *	is mapped in the address range starting at vaddr.
 *	The number of pages of interest is passed in sizein;
 *	vaddr and sizein are updated to reflect first mapping;
 *	number of contiguous entries returned in sizeout.
 */
extern pde_t    *pmap_probe(pmap_t *, char **, pgno_t *, pgno_t *, int *);

/*
 *	pmap_reserve(pmap_t *pmap, char *vaddr, pgno_t size)
 *
 *	Reserve mapping rights for size pages starting at vaddr.
 *	Returns 0 on success, errno otherwise.
 */
extern int	pmap_reserve(pmap_t *, char *, pgno_t);

/*
 *	pmap_unreserve(pmap_t *pmap, char *vaddr, pgno_t size)
 *
 *	Uneserve mapping rights for size pages starting at vaddr.
 */
extern void	pmap_unreserve(pmap_t *, char *, pgno_t);

/*
 *	pmap_free(pmap_t *pmap, char *vaddr, pgno_t size, int free)
 *
 *	Free all memory mapped by pmap entries mapping size pages,
 *	starting at vaddr.  Returns number of pages freed.
 */
#define PMAPFREE_TOSS	0
#define PMAPFREE_FREE	1
#define PMAPFREE_SANON	2
#define PMAPFREE_UNHASH	4

extern int	pmap_free(pmap_t *, char *, pgno_t, int);

/*
 *	pmap_trim(pmap_t *pmap);
 *
 *	Trim pmap module of all unused data structures.
 *	Called by vhand when memory is tight.  Returns true
 *	if the pmap was actually trimmed.
 */
extern int	pmap_trim(pmap_t *);

/*
 *	pmap_split(struct proc *p, preg_set_t *prpp)
 *
 *	Specialized routine for sproc -- manages local and shared
 *	address space overlaps.
 */
extern pmap_t	*pmap_split(struct proc *, preg_set_t *);

/*
 *	pmap_lclsetup(struct proc *p, pmap_t *pmap, char *vaddr, pgno_t size)
 *
 *	Perform any initialization needed when attaching or growing a
 *	local mapping in a shared address process.
 */
extern void	pmap_lclsetup(struct proc *, pmap_t *, char *, pgno_t);

/*
 *	pmap_lclteardwn(pmap_t *pmap, char *vaddr, pgno_t size)
 *
 *	Perform any tear down needed when detaching or shrinking a
 * 	local mapping in a shared address process.
 */
extern void	pmap_lclteardwn(pmap_t *, char *, pgno_t);

/* Shared Page Tables */

/*
 *
 *	pmap_spt_get(pmap_sptid_t, pmap_t *, addr_t, size_t)
 *
 *	Allocation and initialization of pmap_sptdesc and
 *	associated pmap. Returns 0 on success errno on failure.
 */
extern int		pmap_spt_get(pmap_sptid_t, pmap_t *, addr_t, size_t);

/*
 *	pmap_spt_remove(pmap_sptid_t)
 *
 *	Removal of pmap_sptdesc and associated pmap.
 */
extern void		pmap_spt_remove(pmap_sptid_t);

/*
 * 	pmap_spt_attach(pmap_sptid_t, pmap_t *, addr_t)
 *
 *	Attach to shared PT. Returns 0 on success and errno on failure.
 */
extern int		pmap_spt_attach(pmap_sptid_t, pmap_t *, addr_t);

/*
 *	pmap_spt_check(pmap_t *, addr_t, size_t)
 *
 *	Check range [addr, addr + size] for shared PT. Returns B_TRUE
 *	if at least on PT was found.
 */
extern boolean_t	pmap_spt_check(pmap_t *, addr_t, size_t);

/*
 *	pmap_modify(struct proc *, pmap_t *, addr_t, size_t)
 *
 *	Replace shared PT in address range [addr, addr + size].
 */	
extern int		pmap_modify(struct proc *, pmap_t *, addr_t, size_t);

/*
 *	pmap_pt_is_shared(pde_t *pt)
 *
 *	Check range [addr, addr + size] for shared PT. Returns B_TRUE
 *	if at least on PT was found.
 */
extern boolean_t	pmap_pt_is_shared(pde_t *pt);

#ifdef __cplusplus
}
#endif

#endif /* __SYS_PMAP_H__ */
