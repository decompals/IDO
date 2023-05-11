/* Copyright (C) 1989 Silicon Graphics, Inc. All rights reserved.  */
/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1991, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Avenue                               |
 * |         Sunnyvale, California 94088-3650, USA             |
 * |-----------------------------------------------------------|
 */
/* $Header: /proj/irix5.1/isms/cmplrs/include/RCS/mdebug.h,v 1.6 1993/06/08 01:15:57 bettina Exp $ */

/* ATTENTION:  THIS FILE IS SUBJECT TO CHANGE ANYTIME */

#ifndef __MDEBUG_H__
#define __MDEBUG_H__

#ifdef __cplusplus
extern "C" {
#endif

/* (C) Copyright 1984 by Third Eye Software, Inc.
 *
 * Third Eye Software, Inc. grants reproduction and use rights to
 * all parties, PROVIDED that this comment is maintained in the copy.
 *
 * Third Eye makes no claims about the applicability of this
 * symbol table to a particular use.
 */

/* 
 * This file contains the definition of the Third Eye Symbol Table.
 *
 * Symbols are assumed to be in 'encounter order' - i.e. the order that
 * the things they represent were encountered by the compiler/assembler/loader.
 * EXCEPT for globals!	These are assumed to be bunched together,
 * probably right after the last 'normal' symbol.  Globals ARE sorted
 * in ascending order.
 *
 * -----------------------------------------------------------------------
 * A brief word about Third Eye naming/use conventions:
 *
 * All arrays and index's are 0 based.
 * All "ifooMax" values are the highest legal value PLUS ONE. This makes
 * them good for allocating arrays, etc. All checks are "ifoo < ifooMax".
 *
 * "isym"	Index into the SYMbol table.
 * "ipd"	Index into the Procedure Descriptor array.
 * "ifd"	Index into the File Descriptor array.
 * "iss"	Index into String Space.
 * "cb"		Count of Bytes.
 * "rgPd"	array whose domain is "0..ipdMax-1" and RanGe is PDR.
 * "rgFd"	array whose domain is "0..ifdMax-1" and RanGe is FDR.
 */


/* 
 * Symbolic Header (HDR) structure.
 * As long as all the pointers are set correctly,
 * we don't care WHAT order the various sections come out in!
 *
 * A file produced solely for the use of CDB will probably NOT have
 * any instructions or data areas in it, as these are available
 * in the original.
 */

#include "elf.h"

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))


typedef struct __sgi_hdrr_s32  {
	Elf32_Half	magic;		/* to verify validity of the table */
	Elf32_Half	vstamp;		/* version stamp */
	Elf32_Section	xFdSH;	/* File descriptor SH index */
	Elf32_Section	xDnSH;	/* Dense numbers SH index */
	Elf32_Section	xSsExtSH;	/* External strings SH index */
	Elf32_Section	xExtSymSH;	/* External symbols SH index */
	Elf32_Section	xLineSH;	/* line numbers SH index */
	Elf32_Section	xLocSymSH;	/* local symbols SH index */
	Elf32_Section	xOptSymSH;	/* optimization symbols SH index */
	Elf32_Section	xAuxSymSH;	/* auxiliary symbols SH index */
	Elf32_Section	xPdescSH;	/* procedure table SH index */
	Elf32_Section	xRfdSH;		/* relative file desc SH index */
	/* If you add machine dependent fields, add them here */
	} HDRR32, *pHDRR32; 
#define cbHDRR32 sizeof(HDRR32)
#define hdrNil32 ((pHDRR32)0)

/*
 * The FDR and PDR structures speed mapping of address <-> name.
 * They are sorted in ascending memory order and are kept in
 * memory by CDB at runtime.
 */

/* 
 * File Descriptor
 *
 * There is one of these for EVERY FILE, whether compiled with
 * full debugging symbols or not.  The name of a file should be
 * the path name given to the compiler.	 This allows the user
 * to simply specify the names of the directories where the COMPILES
 * were done, and we will be able to find their files.
 * A field whose comment starts with "R - " indicates that it will be
 * setup at runtime.
 */
typedef struct fdr32 {
	Elf32_Addr	adr;	/* memory address of beginning of file */
	Elf32_Word	rss;	/* String table index of filename */
	Elf32_Word iLocBase;
        Elf32_Word issBase;
        Elf32_Word cbSs;
	Elf32_Word cLocSym;
	Elf32_Word iLineBase;
	Elf32_Word cLine;
	Elf32_Word iOptSymBase;
	Elf32_Word cOptSym;
	Elf32_Word iPdescBase;
	Elf32_Word cPdesc;
	Elf32_Word iAuxSymBase;
	Elf32_Word cAuxSym;
	Elf32_Word iRfdBase;
	Elf32_Word cRfd;
	Elf32_Word lang : 5;	/* language for this file */
	Elf32_Word fMerge : 1;	/* whether this file can be merged */
	Elf32_Word fReadin : 1;	/* true if it was read in (not just created) */
	Elf32_Word fBigendian : 1;/* if set, was compiled on big endian machine */
				/*	aux's will be in compile host's sex */
	Elf32_Word glevel : 2;	/* level this file was compiled with */
	Elf32_Word reserved : 22;  /* reserved for future use */
	Elf32_Word timestamp; /* Modification timestamp of file */
	Elf32_Word cbLineOffset; /* Byte offset from start of section */
	} FDR32, *pFDR32;
#define cbFDR32 sizeof(FDR32)
#define fdNil32 ((pFDR32)0)
#define ifdNil32 -1
#define ifdTemp32 0
#define ilnNil32 -1


/* 
 * Procedure Descriptor
 *
 * There is one of these for EVERY TEXT LABEL.
 * If a procedure is in a file with full symbols, then isym
 * will point to the PROC symbols, else it will point to the
 * global symbol for the label.
 */

typedef struct pdr32 {
	Elf32_Word 	addrList;
	Elf32_Word	isym;		
	Elf32_Word	iline;		
	Elf32_Word	regmask;
	Elf32_Off	regOffsets;
	Elf32_Word	iopt;		
	Elf32_Word	fregmask;
	Elf32_Off	fregOffsets;
	Elf32_Off       frameoffset;
	Elf32_Half	framereg;	/* frame pointer register */
	Elf32_Half	pcreg;		/* offset or reg of return pc */
	Elf32_Word	lnLow;		/* lowest line in the procedure */
	Elf32_Word	lnHigh;		/* highest line in the procedure */
	Elf32_Off	cbLineOffset;	/* byte offset for this procedure from the fd base */
	} PDR32, *pPDR32;
#define cbPDR32 sizeof(PDR32)
#define pdNil32 ((pPDR32) 0)
#define ipdNil32	-1

/*
 * The structure of the runtime procedure descriptor created by the loader
 * for use by the static exception system.
 */
typedef struct runtime_pdr32 {
	Elf32_Word	adr;	/* memory address of start of procedure */
	Elf32_Sword	regmask;	/* save register mask */
	Elf32_Sword	regoffset;	/* save register offset */
	Elf32_Sword	fregmask;	/* save floating point register mask */
	Elf32_Sword	fregoffset;	/* save floating point register offset */
	Elf32_Sword	frameoffset;	/* frame size */
	Elf32_Half	framereg;	/* frame pointer register */
	Elf32_Half	pcreg;		/* offset or reg of return pc */
	Elf32_Sword	irpss;		/* index into the runtime string table */
	Elf32_Sword	reserved;
	struct exception_info *exception_info;/* pointer to exception array */
} RPDR32, *pRPDR32;
#define cbRPDR32 sizeof(RPDR32)
#define rpdNil32 ((pRPDR32) 0)
#define rsdNil32 ((pSYMR32) 0)

/*
 * Line Numbers
 *
 * Line Numbers are segregated from the normal symbols because they
 * are [1] smaller , [2] are of no interest to your
 * average loader, and [3] are never needed in the middle of normal
 * scanning and therefore slow things down.
 *
 * By definition, the first LINER for any given procedure will have
 * the first line of a procedure and represent the first address.
 */

typedef	Elf32_Sword LINER32, *pLINER32;
#define lineNil32 ((pLINER32)0)
#define cbLINER32 sizeof(LINER32)
#define ilineNil32	-1



/*
 * The Symbol Structure		(GFW, to those who Know!)
 */

typedef struct __sgi_symr_s32 {
	Elf32_Word	iss;		/* index into String Space of name */
	Elf32_Word	value;		/* value of symbol */
	Elf32_Word st : 8;	/* symbol type */
	Elf32_Word sc  : 8;	/* storage class - text, data, etc */
	Elf32_Word reserved : 16;	/* reserved */
	Elf32_Word index;	/* index into sym/aux table */
	} SYMR32, *pSYMR32;
#define symNil32 ((pSYMR32)0)
#define cbSYMR32 sizeof(SYMR32)
#define isymNil32 -1
#define indexNil32 0xfffff
#define issNil32 -1
#define issNull32 0


/* The following converts a memory resident string to an iss.
 * This hack is recognized in SbFIss, in sym.c of the debugger.
 */
#define IssFSb(sb) (0x80000000 | ((unsigned long)(sb)))

/* E X T E R N A L   S Y M B O L  R E C O R D
 *
 *	Same as the SYMR except it contains file context to determine where
 *	the index is.
 */
typedef struct __sgi_extr__32 {
	Elf32_Word jmptbl:1;	/* symbol is a jump table entry for shlibs */
	Elf32_Word cobol_main:1;	/* symbol is a cobol main procedure */
	Elf32_Word weakext:1;	/* symbol is weak external */
	Elf32_Word reserved:29;	/* reserved for future use */
	Elf32_Word	ifd;		/* where the iss and index fields point into */
	SYMR32	asym;		/* symbol for the external */
	} EXTR32, *pEXTR32;
#define extNil32 ((pEXTR32)0)
#define cbEXTR32 sizeof(EXTR32)


/* A U X I L L A R Y   T Y P E	 I N F O R M A T I O N */

/*
 * Type Information Record
 */
typedef struct {
	Elf32_Word fBitfield : 1; /* set if bit width is specified */
	Elf32_Word continued : 1; /* indicates additional TQ info in next AUX */
	Elf32_Word bt  : 6;	/* basic type */
	Elf32_Word tq4 : 4;
	Elf32_Word tq5 : 4;
	/* ---- 16 bit boundary ---- */
	Elf32_Word tq0 : 4;
	Elf32_Word tq1 : 4;	/* 6 type qualifiers - tqPtr, etc. */
	Elf32_Word tq2 : 4;
	Elf32_Word tq3 : 4;
	} TIR32, *pTIR32;
#define cbTIR32 sizeof(TIR32)
#define tiNil32 ((pTIR32)0)
#define itqMax32 6

/*
 * Relative symbol record
 *
 * If the rfd field is 4095, the index field indexes into the global symbol
 *	table.
 */

typedef struct {
	Elf32_Word	rfd : 12;    /* index into the file indirect table */
	Elf32_Word	index : 20; /* index int sym/aux/iss tables */
	} RNDXR32, *pRNDXR32;
#define cbRNDXR32 sizeof(RNDXR32)
#define rndxNil32 ((pRNDXR32)0)

/* dense numbers or sometimes called block numbers are stored in this type,
 *	a rfd of 0xffffffff is an index into the global table.
 */
typedef struct {
	Elf32_Word	rfd;    /* index into the file table */
	Elf32_Word	index; 	/* index int sym/aux/iss tables */
	} DNR32, *pDNR32;
#define cbDNR32 sizeof(DNR32)
#define dnNil32 ((pDNR32)0)



/*
 * Auxillary information occurs only if needed.
 * It ALWAYS occurs in this order when present.

	    isymMac		used by stProc only
	    TIR			type info
	    TIR			additional TQ info (if first TIR was not enough)
	    rndx		if (bt == btStruct,btUnion,btEnum,btSet,btRange,
				    btTypedef):
				    rsym.index == iaux for btSet or btRange
				    else rsym.index == isym
	    dimLow		btRange, btSet
	    dimMac		btRange, btSet
	    rndx0		As many as there are tq arrays
	    dimLow0
	    dimHigh0
	    ...
	    rndxMax-1
	    dimLowMax-1
	    dimHighMax-1
	    width in bits	if (bit field), width in bits.
 */
#define cAuxMax (6 + (idimMax*3))

/* a union of all possible info in the AUX universe */
typedef union __sgi_auxu_u32 {
	TIR32	ti;		/* type information record */
	RNDXR32	rndx;		/* relative index into symbol table */
	Elf32_Sword	dnLow;		/* low dimension */
	Elf32_Sword	dnHigh;		/* high dimension */
	Elf32_Word	isym;		/* symbol table index (end of proc) */
	Elf32_Word	iss;		/* index into string space (not used) */
	Elf32_Word	width;		/* width for non-default sized struc fields */
	Elf32_Word	count;		/* count of ranges for variant arm */
	} AUXU32, *pAUXU32;
#define cbAUXU32 sizeof(AUXU32)
#define auxNil32 ((pAUXU32)0)
#define iauxNil32 -1


/*
 * Optimization symbols
 *
 * Optimization symbols contain some overlap information with the normal
 * symbol table. In particular, the proc information
 * is somewhat redundant but necessary to easily find the other information
 * present. 
 *
 * All of the offsets are relative to the beginning of the last otProc
 */

typedef struct __sgi_optr_s32 {
	Elf32_Word ot: 8;		/* optimization type */
	Elf32_Word value: 24;	/* address where we are moving it to */
	RNDXR32	rndx;		/* points to a symbol or opt entry */
	Elf32_Word	offset;	/* relative offset this occured */
	} OPTR32, *pOPTR32;
#define optNil32	((pOPTR32) 0)
#define cbOPTR32 sizeof(OPTR32)
#define ioptNil32 -1

/*
 * File Indirect
 *
 * When a symbol is referenced across files the following procedure is used:
 *	1) use the file index to get the File indirect entry.
 *	2) use the file indirect entry to get the File descriptor.
 *	3) add the sym index to the base of that file's sym table
 *
 */

typedef Elf32_Word RFDT32, *pRFDT32;
#define cbRFDT32 sizeof(RFDT32)
#define rfdNil32	-1

/*
 * The file indirect table in the mips loader is known as an array of FITs.
 * This is done to keep the code in the loader readable in the area where
 * these tables are merged.  Note this is only a name change.
 */
typedef Elf32_Word FIT32, *pFIT32;
#define cbFIT32	sizeof(FIT32)
#define ifiNil32	-1
#define fiNil32	((pFIT32) 0)

/* 
 * The following are the declarations to access the runtime symbol 
 * table and its size generated by the loader.
 */

#if 0
extern SYMR _rt_symbol_table[];
#endif
extern char _rt_symbol_string_table[];
#define RTSIZE ((int) _rt_symbol_table_size)
extern char _rt_symbol_table_size[];

#endif /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

#ifdef _LANGUAGE_PASCAL
#define ifdNil -1
#define ilnNil -1
#define ipdNil -1
#define ilineNil -1
#define isymNil -1
#define indexNil 16#fffff
#define issNil -1
#define issNull 0
#define itqMax 6
#define iauxNil -1
#define ioptNil -1
#define rfdNil -1
#define ifiNil -1
#endif	/* _LANGUAGE_PASCAL */


/* Dense numbers
 *
 * Rather than use file index, symbol index pairs to represent symbols
 *	and globals, we use dense number so that they can be easily embeded
 *	in intermediate code and the programs that process them can
 *	use direct access tabls instead of hash table (which would be
 *	necesary otherwise because of the sparse name space caused by
 *	file index, symbol index pairs. Dense number are represented
 *	by RNDXRs.
 */

/*
 * The following table defines the meaning of each SYM field as
 * a function of the "st". (scD/B == scData OR scBss)
 *
 * Note: the value "isymMac" is used by symbols that have the concept
 * of enclosing a block of related information.	 This value is the
 * isym of the first symbol AFTER the end associated with the primary
 * symbol. For example if a procedure was at isym==90 and had an
 * isymMac==155, the associated end would be at isym==154, and the
 * symbol at 155 would probably (although not necessarily) be the
 * symbol for the next procedure.  This allows rapid skipping over
 * internal information of various sorts. "stEnd"s ALWAYS have the
 * isym of the primary symbol that started the block.
 * 

ST		SC	VALUE		INDEX
--------	------	--------	------
stFile		scText	address		isymMac
stLabel		scText	address		---
stGlobal	scD/B	address		iaux
stStatic	scD/B	address		iaux
stParam		scAbs	offset		iaux
stLocal		scAbs	offset		iaux
stProc		scText	address		iaux	(isymMac is first AUX)
stStaticProc	scText	address		iaux	(isymMac is first AUX)

stMember	scNil	ordinal		---	(if member of enum)
stMember	scNil	byte offset	iaux	(if member of struct/union)
stMember	scBits	bit offset	iaux	(bit field spec)

stBlock		scText	address		isymMac (text block)
stBlock		scNil	cb		isymMac (struct/union member define)
stBlock		scNil	cMembers	isymMac (enum member define)

stEnd		scText	address		isymStart
stEnd		scNil	-------		isymStart (struct/union/enum)

stTypedef	scNil	-------		iaux
stRegReloc	sc???	value		old register number
stForward	sc???	new address	isym to original symbol

stConstant	scInfo	value		--- (scalar)
stConstant	scInfo	iss		--- (complex, e.g. string)

 *
 */

typedef enum {
    DBG_SYMHDR, DBG_FD, DBG_EXTSYM, DBG_LOCSTR, DBG_DN,
    DBG_PD, DBG_LOCSYM, DBG_AUXSYM, DBG_OPTSYM, DBG_RFD, DBG_LINE, /* Line needs to be last since */
    /* size of line table is not known until later */
    MAX_DBG_SCN
} DBG_SCNS;

#ifdef __cplusplus
}
#endif

#endif /* !__MDEBUG_H__ */
