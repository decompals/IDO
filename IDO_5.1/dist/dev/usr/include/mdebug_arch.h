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
/* $Header: /proj/irix5.1/isms/cmplrs/include/RCS/mdebug_arch.h,v 1.3 1993/06/08 01:16:26 bettina Exp $ */
#ifndef __MDEBUG_ARCH_H__
#define __MDEBUG_ARCH_H__

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
typedef unsigned short Elf32_Section;

#define  HDRR 	HDRR32
#define pHDRR 	pHDRR32
#define cbHDRR 	cbHDRR32 
#define hdrNil 	hdrNil32 
#define FDR	FDR32
#define pFDR 	pFDR32
#define cbFDR	cbFDR32 
#define fdNil 	fdNil32 
#define ifdNil	ifdNil32
#define ifdTemp	ifdTemp32
#define ilnNil	ilnNil32
#define PDR 	PDR32
#define pPDR 	pPDR32
#define cbPDR 	cbPDR32 
#define pdNil	pdNil32 
#define ipdNil	ipdNil32
#define RPDR 	RPDR32
#define pRPDR 	pRPDR32
#define cbRPDR	cbRPDR32 
#define rpdNil	rpdNil32 
#define rsdNil	rsdNil32
#define LINER	LINER32
#define pLINER	pLINER32
#define lineNil	lineNil32 
#define cbLINER	cbLINER32 
#define ilineNil	ilineNil32
#define SYMR 	SYMR32
#define pSYMR 	pSYMR32
#define symNil	symNil32 
#define cbSYMR	cbSYMR32 
#define isymNil	isymNil32 
#define indexNil	indexNil32
#define issNil	issNil32
#define issNull	issNull32
#define EXTR 	EXTR32
#define pEXTR 	pEXTR32
#define extNil	extNil32 
#define cbEXTR	cbEXTR32 
#define TIR 	TIR32
#define pTIR 	pTIR32
#define cbTIR	cbTIR32 
#define tiNil	tiNil32 
#define itqMax	itqMax32
#define RNDXR   RNDXR32
#define pRNDXR  pRNDXR32
#define cbRNDXR	cbRNDXR32 
#define rndxNil	rndxNil32 
#define DNR 	DNR32
#define pDNR 	pDNR32
#define cbDNR	cbDNR32 
#define dnNil	dnNil32 
#define AUXU 	AUXU32
#define pAUXU 	pAUXU32
#define cbAUXU	cbAUXU32 
#define auxNil	auxNil32 
#define iauxNil	iauxNil32
#define OPTR	OPTR32
#define pOPTR	pOPTR32
#define optNil	optNil32        
#define cbOPTR	cbOPTR32 
#define ioptNil	ioptNil32
#define RFDT	RFDT32
#define pRFDT	pRFDT32
#define cbRFDT	cbRFDT32 
#define rfdNil	rfdNil32 
#define FIT	FIT32
#define pFIT	pFIT32

#ifdef __cplusplus
}
#endif

#endif /* __MDEBUG_ARCH_H__ */
