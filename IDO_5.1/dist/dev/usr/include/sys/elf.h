#ifndef __SYS_ELF_H__
#define __SYS_ELF_H__

/*	Copyright (c) 1990, 1991 UNIX System Laboratories, Inc.	*/
/*	Copyright (c) 1984, 1986, 1987, 1988, 1989, 1990 AT&T	*/
/*	  All Rights Reserved  	*/
/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF     	*/
/*	UNIX System Laboratories, Inc.                     	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/
/*
 * Copyright 1992 Silicon Graphics,  Inc.
 * ALL RIGHTS RESERVED
 * 
 * THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF SGI
 * The copyright notice above does not evidence any  actual  or
 * intended  publication of this source code and material is an
 * unpublished work by Silicon  Graphics,  Inc.  This  material
 * contains CONFIDENTIAL INFORMATION that is the property and a
 * trade secret of Silicon Graphics, Inc. Any use,  duplication
 * or  disclosure  not  specifically  authorized  in writing by
 * Silicon Graphics is  strictly  prohibited.  THE  RECEIPT  OR
 * POSSESSION  OF  THIS SOURCE CODE AND/OR INFORMATION DOES NOT
 * CONVEY ANY RIGHTS TO REPRODUCE, DISCLOSE OR  DISTRIBUTE  ITS
 * CONTENTS,  OR  TO MANUFACTURE, USE, OR SELL ANYTHING THAT IT
 * MAY DESCRIBE, IN WHOLE OR IN PART.
 * 
 * U.S. GOVERNMENT RESTRICTED RIGHTS LEGEND
 * Use, duplication or disclosure by the Government is  subject
 * to  restrictions  as  set  forth  in  FAR 52.227.19(c)(2) or
 * subparagraph (c)(1)(ii) of the Rights in Technical Data  and
 * Computer  Software  clause  at  DFARS 252.227-7013 and/or in
 * similar or successor clauses in the FAR, or the DOD or  NASA
 * FAR  Supplement.  Unpublished  --  rights reserved under the
 * Copyright Laws of the United States. Contractor/manufacturer
 * is Silicon Graphics, Inc., 2011 N. Shoreline Blvd., Mountain
 * View, CA 94039-7311
 */
/* $Header: /proj/irix5.1/isms/cmplrs/include/sys/RCS/elf.h,v 1.11 1993/07/21 23:04:20 ho Exp $ */

#include <sys/elftypes.h> 

/* 
 * Random constants
 */

#define _TEXT_ALIGN 0x10000
#define _DATA_ALIGN 0x10000
#define ELF_MIPS_MAXPGSZ (64*1024)


/*
 * e_flags
 */

#define EF_MIPS_NOREORDER	0x00000001
#define EF_MIPS_OPSEX		EF_MIPS_NOREORDER
#define EF_MIPS_PIC		0x00000002
#define EF_MIPS_CPIC		0x00000004
#define EF_MIPS_UGEN_ALLOC	0x00000008
#define EF_MIPS_UGEN_RESERVED	0x00000010 /* reserved for future use */
#define EF_MIPS_ARCH		0xf0000000
#define EF_MIPS_ARCH_1		0x00000000
#define EF_MIPS_ARCH_2          0x10000000
#define EF_MIPS_ARCH_3          0x20000000
#define EF_MIPS_ARCH_4          0x30000000


/* 
 * special Program header types
 */

#define PT_MIPS_REGINFO		(PT_LOPROC + 0)
#define PT_MIPS_RTPROC		(PT_LOPROC + 1)	/* runtime procedure table */


/* 
 * special p_flags
 */

#define PF_MIPS_LOCAL		0x10000000


/*
 * Special mips st_other
 */
#define STO_DEFAULT		0x0
#define STO_INTERNAL		0x1
#define STO_HIDDEN		0x2
#define STO_PROTECTED		0x3


/* 
 * Special mips section indices
 */

#define SHN_MIPS_ACOMMON	(SHN_LOPROC + 0)
#define SHN_MIPS_TEXT		(SHN_LOPROC + 1)
#define SHN_MIPS_DATA		(SHN_LOPROC + 2)
#define SHN_MIPS_SCOMMON	(SHN_LOPROC + 3)
#define SHN_MIPS_SUNDEFINED	(SHN_LOPROC + 4)


/*
 * sh_type
 */

#define SHT_MIPS_LIBLIST	(SHT_LOPROC + 0)
#define SHT_MIPS_MSYM		(SHT_LOPROC + 1)
#define SHT_MIPS_CONFLICT	(SHT_LOPROC + 2)
#define SHT_MIPS_GPTAB		(SHT_LOPROC + 3)
#define SHT_MIPS_UCODE		(SHT_LOPROC + 4)
#define SHT_MIPS_DEBUG          (SHT_LOPROC + 5)
#define SHT_MIPS_REGINFO        (SHT_LOPROC + 6)
#ifdef __osf__
#define	SHT_MIPS_PACKAGE	(SHT_LOPROC + 7)
#define	SHT_MIPS_PACKSYM	(SHT_LOPROC + 8)
#endif /* __osf__ */

#define SHT_MIPS_RELD		(SHT_LOPROC + 9)
#define SHT_MIPS_EVENTS		(SHT_LOPROC + 10)
#define SHT_MIPS_IFACE		(SHT_LOPROC + 11)
#define SHT_MIPS_CONTENT	(SHT_LOPROC + 12)
#define SHT_MIPS_OPTIONS	(SHT_LOPROC + 13)

#define SHT_MIPS_SHDR		(SHT_LOPROC + 16)
#define SHT_MIPS_FDESC		(SHT_LOPROC + 17)
#define SHT_MIPS_EXTSYM		(SHT_LOPROC + 18)
#define SHT_MIPS_DENSE		(SHT_LOPROC + 19)
#define SHT_MIPS_PDESC		(SHT_LOPROC + 20)
#define SHT_MIPS_LOCSYM		(SHT_LOPROC + 21)
#define SHT_MIPS_AUXSYM		(SHT_LOPROC + 22)
#define SHT_MIPS_OPTSYM		(SHT_LOPROC + 23)
#define SHT_MIPS_LOCSTR		(SHT_LOPROC + 24)
#define SHT_MIPS_LINE		(SHT_LOPROC + 25)
#define SHT_MIPS_RFDESC		(SHT_LOPROC + 26)

#define SHT_MIPS_DELTASYM	(SHT_LOPROC + 27)
#define SHT_MIPS_DELTAINST	(SHT_LOPROC + 28)
#define SHT_MIPS_DELTACLASS	(SHT_LOPROC + 29)

#define SHT_MIPS_DWARF		(SHT_LOPROC + 30)

#define SHT_MIPS_DELTADECL	(SHT_LOPROC + 31)
/*
 * sh_flags
 */

#define SHF_MIPS_GPREL	0x10000000
#define SHF_MIPS_MERGE	0x20000000
#define SHF_MIPS_ADDR32	0x40000000
#define SHF_MIPS_ADDR64	0x80000000
#define SHF_MIPS_NOSTRIP 0x08000000
#define SHF_MIPS_LOCAL	0x04000000


/*
 * special section names
 */

#define MIPS_SDATA		".sdata"
#define MIPS_REL_SDATA		".rel.sdata"
#define MIPS_SBSS		".sbss"
#define MIPS_LIT4		".lit4"
#define MIPS_LIT8		".lit8"
#define MIPS_REGINFO		".reginfo"
#define MIPS_LIBLIST		".liblist"
#define MIPS_MSYM		".msym"
#define MIPS_RHEADER		".rheader"
#define MIPS_CONFLICT		".conflict"
#define MIPS_GPTAB_SDATA	".gptab.sdata"
#define MIPS_GPTAB_DATA		".gptab.data"
#define MIPS_GPTAB_BSS		".gptab.bss"
#define MIPS_GPTAB_SBSS		".gptab.sbss"
#define MIPS_LBSS		".lbss"
#define MIPS_UCODE		".ucode"
#define MIPS_MDEBUG		".mdebug"
#define MIPS_COMPACT_RELOC	".compact_rel"
#ifdef __osf__
#define MIPS_PACKAGE		".package"
#define MIPS_PACKSYM		".packsym"
#endif /* __osf__ */
#define MIPS_CONTENT		".MIPS.content"
#define MIPS_DELTACLASS		".MIPS.dclass"
#define MIPS_DELTASYM		".MIPS.dsym"
#define MIPS_DELTAINST		".MIPS.dinst"
#define MIPS_DELTADECL		".MIPS.ddecl"
#define MIPS_REL_DELTA		".rel.delta"

/*
 * ".gptab" section
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef union
{
	struct
	{
		Elf32_Word	gt_current_g_value;
		Elf32_Word	gt_unused;
	} gt_header;
	struct
	{
		Elf32_Word	gt_g_value;
		Elf32_Word	gt_bytes;
	} gt_entry;
} Elf32_Gptab;
#endif


/*
 * ".reginfo" section
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct
{
	Elf32_Word	ri_gprmask;
	Elf32_Word	ri_cprmask[4];
	Elf32_Sword	ri_gp_value;
} Elf32_RegInfo;
#endif


/*
 * ".options" section
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct
{
	Elf32_Byte	kind;		/* determines interpretation of the */
					/* variable part of descriptor	    */
	Elf32_Byte	size;		/* size of descriptor, incl. header */
	Elf32_Section	section;	/* section header index of section  */
					/* affected, 0 for global options   */
	Elf32_Word	options;	/* Kind-specific information	    */
} Elf_Options;
#endif


/*
 * Options descriptor kinds
 */

#define ODK_NULL	0	/* Undefined */
#define ODK_REGINFO	1	/* Register usage information */
#define ODK_EXCEPTIONS	2	/* Exception processing options  */
#define ODK_PAD		3	/* Section padding options */


/*
 * r_info
 */

/*
 * relocation types
 */

#define R_MIPS_NONE		0
#define R_MIPS_16		1
#define R_MIPS_32		2
#define R_MIPS_REL32		3
#define R_MIPS_26		4
#define R_MIPS_HI16		5
#define R_MIPS_LO16		6
#define R_MIPS_GPREL		7
#define R_MIPS_GPREL16		R_MIPS_GPREL
#define R_MIPS_LITERAL		8
#define R_MIPS_GOT16		9
#define R_MIPS_PC16		10
#define R_MIPS_CALL16		11
#define R_MIPS_GPREL32		12

#define R_MIPS_SHIFT5		16
#define R_MIPS_SHIFT6		17
#define R_MIPS_64		18
#define R_MIPS_GOT_DISP		19
#define R_MIPS_GOT_PAGE		20
#define R_MIPS_GOT_OFST		21
#define R_MIPS_GOT_HI16		22
#define R_MIPS_GOT_LO16		23
#define R_MIPS_SUB		24
#define R_MIPS_INSERT_A		25
#define R_MIPS_INSERT_B		26
#define R_MIPS_DELETE		27
#define R_MIPS_HIGHER		28
#define R_MIPS_HIGHEST		29
#define R_MIPS_CALL_HI16	30
#define R_MIPS_CALL_LO16	31
#define _R_MIPS_COUNT_		32	/* Number of relocations */

/*
 * ".liblist" section
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct
{
	Elf32_Word	l_name;
	Elf32_Word	l_time_stamp;
	Elf32_Word	l_checksum;
	Elf32_Word	l_version;
	Elf32_Word	l_flags;
} Elf32_Lib;
#endif


/*
 * l_flags
 */

#define LL_NONE			0
#define LL_EXACT_MATCH		0x1
#define LL_IGNORE_INT_VER	0x2
#define LL_REQUIRE_MINOR	0x4

/*
 * ".msym" section
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct
{
	Elf32_Word	ms_hash_value;
	Elf32_Word	ms_info;
} Elf32_Msym;
#endif


/*
 * ms_info
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
#define ELF32_MS_REL_INDEX(i)	((i) >> 8)
#define ELF32_MS_FLAGS(i)	((i) & 0xff)
#define ELF32_MS_INFO(r,f)	(((r) << 8) + ((f) & 0xff))
#endif


/*
 * ".conflict" section
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef Elf32_Addr Elf32_Conflict;
#endif

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
extern Elf32_Conflict	_ConflictList [];
#endif

#define RLD_VERSION            1


/* ====================================================================
 *
 * .content Section
 *
 * sh_type:	SHT_MIPS_CONTENT
 * sh_link:	section header index of section classified
 * sh_info:	0
 * attributes:	SHF_ALLOC, SHF_MIPS_NOSTRIP
 *
 * ====================================================================
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef union {
    struct {				/* Normal descriptor */
	Elf32_Word	con_info;
	Elf32_Word	con_start;
    } con_y;
    Elf32_Word	con_xval;		/* Extension descriptor */
} Elf32_Content;

/* con_info masks: */
#define __CON32_EMASK	0x80000000
#define __CON32_ESHIFT	31
#define __CON32_KMASK	0x7f000000
#define __CON32_KSHIFT	24
#define __CON32_LMASK	0x00ffffff
#define __CON32_VMASK	0x7fffffff

/* Access macros: */
#define ELF32_CON_EXTN(c) \
	(((c).con_y.con_info & __CON32_EMASK)>>__CON32_ESHIFT)
#define ELF32_CON_KIND(c) \
	(((c).con_y.con_info & __CON32_KMASK)>>__CON32_KSHIFT)
#define Set_ELF32_CON_KIND(c,v) \
	((c).con_y.con_info = ((c).con_y.con_info & ~__CON32_KMASK) | (v<<__CON32_KSHIFT))
#define ELF32_CON_LENGTH(c)	((c).con_y.con_info & __CON32_LMASK)
#define Set_ELF32_CON_LENGTH(c,v) \
	((c).con_y.con_info = ((c).con_y.con_info & ~__CON32_LMASK) | (v&__CON32_LMASK))
#define ELF32_CON_XVAL(c)	((c).con_xval & __CON32_VMASK)

/* Content kind -- valid for ELF-32 and ELF-64: */
typedef enum {
    CK_NULL	= 0,	/* Invalid */
    CK_INSTR	= 1,	/* Instructions */
    CK_DATA	= 2,	/* Non-address data */
    CK_SADDR_32	= 3,	/* Simple 32-bit addresses */
    CK_GADDR_32	= 4,	/* GP-relative 32-bit addresses */
    CK_CADDR_32	= 5,	/* Complex 32-bit addresses */
    CK_SADDR_64	= 6,	/* Simple 64-bit addresses */
    CK_GADDR_64	= 7,	/* GP-relative 64-bit addresses */
    CK_CADDR_64	= 8,	/* Complex 64-bit addresses */
    CK_NO_XFORM	= 9,	/* No transformations allowed in this range */
    CK_NO_REORDER = 10	/* No reordering allowed in this range */
} Elf_Content_Kind;

#endif


/*
 * ".got" section
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct
{
	Elf32_Addr	g_index;
} Elf32_Got;
#endif


#ifdef __osf__

/*
 * .package section
 * Multiple package entries for the same package are allowed
 * in order to express out of order symbols in a package.
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct {
	Elf32_Word	pkg_name;	/* index into String Space of name */
	Elf32_Word	pkg_version;	/* index into String Space of version string */
	Elf32_Half	pkg_flags;	/* package flags */
} Elf32_Package;
#endif

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
extern Elf32_Package	_PackageList [];
#endif


/*
 * pkg_name --
 * index of a string that identifies the name of this package
 * implementation, which cannot be the null string; the offset is in
 * bytes of a zero terminated string from the start of the .dynstr section
 * pkg_version --
 * index of a string that identifies the version of this package
 * implementation, which may be the null string; the offset is in
 * bytes of a zero terminated string from the start of the .dynstr section
 * pkg_flags --
 * export flag means package is exported, import flag means package is imported,
 * both flags must be set if a package is exported and is also used by other
 * packages within the shared library.  continuance flag means that this
 * package entry defines additional symbols for a previously defined
 * package.  continuance entries must exactly match the original entry in each
 * field, except for the pkg_start, pkg_count, and continuance flag in the pkg_flags.
 * The conflict flag is a possibility for future support for symbol preemption.
 */

/*
 * pkg_flags
 */

#define PKGF_EXPORT	0x1
#define PKGF_IMPORT	0x2
/* #define PKGF_CONFLICT	0x8 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef Elf32_Word Elf32_Package_Symbol;
#endif

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
#define	PACKSYM_NULL_INDEX	((Elf32_Word) 0)
#endif

#endif /* __osf__ */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
extern Elf32_Got	_GlobalOffsetTable [];
#endif

#define MS_ALIAS        0x1

#define DT_MIPS_RLD_VERSION     0x70000001
#define DT_MIPS_TIME_STAMP      0x70000002
#define DT_MIPS_ICHECKSUM       0x70000003
#define DT_MIPS_IVERSION        0x70000004
#define DT_MIPS_FLAGS           0x70000005
#define DT_MIPS_BASE_ADDRESS    0x70000006
#define DT_MIPS_MSYM            0x70000007
#define DT_MIPS_CONFLICT        0x70000008
#define DT_MIPS_LIBLIST         0x70000009
#define DT_MIPS_LOCAL_GOTNO     0x7000000A
#define DT_MIPS_CONFLICTNO      0x7000000B
#define DT_MIPS_LIBLISTNO       0x70000010
#define DT_MIPS_SYMTABNO        0x70000011
#define DT_MIPS_UNREFEXTNO      0x70000012
#define DT_MIPS_GOTSYM          0x70000013
#ifndef __osf__
#define DT_MIPS_HIPAGENO        0x70000014
/* 0x70000015 is skipped */
#define DT_MIPS_RLD_MAP         0x70000016
#define DT_MIPS_DELTA_CLASS 	0x70000017	/* contains Delta C++ class definition */
#define DT_MIPS_DELTA_CLASS_NO 	0x70000018	/* the number of entries in DT_MIPS_DELTA_CLASS */
#define DT_MIPS_DELTA_INSTANCE	0x70000019	/* contains Delta C++ class instances */
#define DT_MIPS_DELTA_INSTANCE_NO	0x7000001A	/* the number of entries in DT_MIPS_DELTA_INSTANCE */
#define DT_MIPS_DELTA_RELOC	0x7000001B	/* contains Delta relocations */
#define DT_MIPS_DELTA_RELOC_NO	0x7000001C	/* the number of entries in DT_M
IPS_DELTA_RELOC */
#define DT_MIPS_DELTA_SYM	0x7000001D	/* contains Delta symbols that Delta relocations refer to */
#define DT_MIPS_DELTA_SYM_NO	0x7000001E	/* the number of entries in DT_M
IPS_DELTA_SYM */
#define DT_MIPS_DELTA_CLASSSYM	0x70000020	/* contains Delta symbols that hold the class declaration */
#define DT_MIPS_DELTA_CLASSSYM_NO	0x70000021	/* the number of entries in DT_MIPS_DELTA_CLASSSYM */
#else  /* __osf__ */
#define DT_MIPS_PACKAGE        	0x70000014
#define DT_MIPS_PACKAGENO       0x70000015
#define DT_MIPS_PACKSYM		0x70000016
#define DT_MIPS_PACKSYMNO	0x70000017
#define	DT_MIPS_IMPACKNO	0x70000018
#define	DT_MIPS_EXPACKNO	0x70000019
#define	DT_MIPS_IMPSYMNO	0x7000001A
#define	DT_MIPS_EXPSYMNO	0x7000001B
#define DT_MIPS_HIPAGENO        0x7000001C
#endif /* __osf__ */

#define RHF_NONE                    0x00000000
#define RHF_QUICKSTART              0x00000001
#define RHF_NOTPOT                  0x00000002
#define RHF_NO_LIBRARY_REPLACEMENT  0x00000004
#define RHF_NO_MOVE                 0x00000008
#define RHF_SGI_ONLY                0x00000010
#define RHF_GUARANTEE_INIT	    0x00000020
#define RHF_DELTA_C_PLUS_PLUS	    0x00000040
#define RHF_GUARANTEE_START_INIT    0x00000080

/* ************************  64-bit declarations  ********************* */


#define ELF64_FSZ_ADDR		8
#define ELF64_FSZ_HALF		2
#define ELF64_FSZ_OFF		8
#define ELF64_FSZ_SWORD		4
#define ELF64_FSZ_WORD		4
#define ELF64_FSZ_SXWORD	8
#define ELF64_FSZ_XWORD		8


/*	ELF header
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct {
	unsigned char	e_ident[EI_NIDENT];	/* ident bytes */
	Elf64_Half	e_type;			/* file type */
	Elf64_Half	e_machine;		/* target machine */
	Elf64_Word	e_version;		/* file version */
	Elf64_Addr	e_entry;		/* start address */
	Elf64_Off	e_phoff;		/* phdr file offset */
	Elf64_Off	e_shoff;		/* shdr file offset */
	Elf64_Word	e_flags;		/* file flags */
	Elf64_Half	e_ehsize;		/* sizeof ehdr */
	Elf64_Half	e_phentsize;		/* sizeof phdr */
	Elf64_Half	e_phnum;		/* number phdrs */
	Elf64_Half	e_shentsize;		/* sizeof shdr */
	Elf64_Half	e_shnum;		/* number shdrs */
	Elf64_Half	e_shstrndx;		/* shdr string index */
} Elf64_Ehdr;
#endif


/*	Program header
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct {
	Elf64_Word	p_type;		/* entry type */
	Elf64_Word	p_flags;	/* entry flags */
	Elf64_Off	p_offset;	/* file offset */
	Elf64_Addr	p_vaddr;	/* virtual address */
	Elf64_Addr	p_paddr;	/* physical address */
	Elf64_Xword	p_filesz;	/* file size */
	Elf64_Xword	p_memsz;	/* memory size */
	Elf64_Xword	p_align;	/* memory/file alignment */
} Elf64_Phdr;
#endif


/*	Section header
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct {
	Elf64_Word	sh_name;	/* section name */
	Elf64_Word	sh_type;	/* SHT_... */
	Elf64_Xword	sh_flags;	/* SHF_... */
	Elf64_Addr	sh_addr;	/* virtual address */
	Elf64_Off	sh_offset;	/* file offset */
	Elf64_Xword	sh_size;	/* section size */
	Elf64_Word	sh_link;	/* misc info */
	Elf64_Word	sh_info;	/* misc info */
	Elf64_Xword	sh_addralign;	/* memory alignment */
	Elf64_Xword	sh_entsize;	/* entry size if table */
} Elf64_Shdr;
#endif


/*	Symbol table
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct {
	Elf64_Word	st_name;
	unsigned char	st_info;	/* bind, type: ELF_64_ST_... */
	unsigned char	st_other;
	Elf64_Half	st_shndx;	/* SHN_... */
	Elf64_Addr	st_value;
	Elf64_Xword	st_size;
} Elf64_Sym;
#endif


/*	The macros compose and decompose values for S.st_info
 *
 *	bind = ELF64_ST_BIND(S.st_info)
 *	type = ELF64_ST_TYPE(S.st_info)
 *	S.st_info = ELF64_ST_INFO(bind, type)
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
#define ELF64_ST_BIND(info)		((info) >> 4)
#define ELF64_ST_TYPE(info)		((info) & 0xf)
#define ELF64_ST_INFO(bind,type)	(((bind)<<4)+((type)&0xf))
#endif


/*	Relocation
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct {
	Elf64_Addr	r_offset;
	Elf64_Xword	r_info;		/* sym, type: ELF64_R_... */
} Elf64_Rel;
#endif

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct {
	Elf64_Addr	r_offset;
	Elf64_Xword	r_info;		/* sym, type: ELF64_R_... */
	Elf64_Sxword	r_addend;
} Elf64_Rela;
#endif


/*	The macros compose and decompose values for Rel.r_info, Rela.f_info
 *
 *	sym = ELF64_R_SYM(R.r_info)
 *	type = ELF64_R_TYPE(R.r_info)
 *	R.r_info = ELF64_R_INFO(sym, type)
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
#define ELF64_R_SYM(info)	((info)>>8)
#define ELF64_R_TYPE(info)	((unsigned char)(info))
#define ELF64_R_INFO(sym,type)	(((sym)<<8)+(unsigned char)(type))
#endif


/*
 * ".conflict" section
 */
#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef Elf64_Addr Elf64_Conflict;

extern Elf64_Conflict   _ConflictList64 [];
#endif


/*	Dynamic Structure
 */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct {
	Elf64_Xword	d_tag;
	union {
		Elf64_Xword	d_val;
		Elf64_Addr	d_ptr;
	} d_un;
} Elf64_Dyn;
#endif


/*
 * ".reginfo" section
 */
#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct
{
        Elf64_Word      ri_gprmask;
        Elf64_Word      ri_cprmask[4];
        Elf64_Addr      ri_gp_value;
} Elf64_RegInfo;
#endif


/*
 * ".liblist" section
 */
#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
typedef struct
{
        Elf64_Word      l_name;
        Elf64_Word      l_time_stamp;
        Elf64_Word      l_checksum;
        Elf64_Word      l_version;
        Elf64_Word      l_flags;
} Elf64_Lib;
#endif

#endif /* __SYS_ELF_H__ */
