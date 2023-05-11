/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1991 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/*  $Header: /d1/cmplrs.src/v3.12/include/cmplrs/RCS/fio.h,v 7.5 1992/05/26 14:36:29 daveb Exp $ */

/* who	ref.		date		description			*/
/* AGC	#1008	13apr87		correct reference to svic->icierr to use*/
/*				CILISTERR macro 			*/
/* sjc  #1963	11Dec87		Dynamically allocate unit table		*/

#ifndef FIO_INCLUDED
#define FIO_INCLUDED
#include <stdio.h>
typedef long ftnint;
#ifdef _LONGLONG
typedef long long ftnll;
typedef unsigned long long ftnull;
#else
typedef long ftnll;
typedef unsigned long ftnull;
#endif
typedef ftnint flag;
typedef long ftnlen;
typedef union {
	char byte;
	short word;
	long longword;
} ftnintu;

typedef struct  {
	short e1;
	short e2;
	short dt;
} Keyspec;

/*external read, write*/
typedef struct
{	flag cierr;
	ftnint ciunit;
	flag ciend;
	char *cifmt;
	ftnint cirec;
	ftnint cimatch;
	ftnint cikeytype;
	union {
	   ftnint ciintval;
	   char *cicharval;
	} cikeyval;
	ftnint cikeyid;
	char *cinml;
/* fix bug 4779 */
	ftnint cikeyvallen;
} cilist;
/*internal read, write*/
typedef struct
{	flag icierr;
	char *iciunit;
	flag iciend;
	char *icifmt;
	ftnint icirlen;
	ftnint icirnum;
} icilist;
/*open*/
typedef struct
{	flag oerr;
	ftnint ounit;
	char *ofnm;
	ftnlen ofnmlen;
	char *osta;
	char *oacc;
	char *ofm;
	ftnint orl;
	char *oblnk;
	char *occ;
	char *oorg;
	flag oshared;
	flag oreadonly;
	ftnint onkeys;
	Keyspec *okeys;
	ftnint *oassocv;
	char *odfnm;
	ftnlen odfnmlen;
	char *odisp;
	ftnint omaxrec;
	char *orectype;
} olist;
/*close*/
typedef struct
{	flag cerr;
	ftnint cunit;
	char *csta;
} cllist;
/*rewind, backspace, endfile, delete, unlock*/
typedef struct
{	flag aerr;
	ftnint aunit;
} alist;
/*find*/
typedef struct
{	flag ferr;
	ftnint funit;
	ftnint frec;
} flist;
/*units*/
typedef struct UNIT
{
	FILE *ufd;
	int isfd;	/* C-ISAM file descriptor (for KEYED) */
	flag uconn;	/* 0=unconnected */
	char *ufnm;
	ftnint luno;	/* fortran logical unit number */
	long uinode;
	int url;	/*0=sequential*/
	flag useek;	/*true=can backspace, use dir, ...*/
	flag ufmt;
	flag uprnt;
	flag ublnk;
	flag uend;
	flag uwrt;	/*last io was write*/
	flag uscrtch;
	flag uerror;    /* last I/O op. has error */
	flag uistty;	/* the file is a terminal device */
	char ucchar;    /* carriage control character */
	char ucc;	/* carriage control code */
	int uacc;	/* SEQUENTIAL, DIRECT, KEYED, or APPEND */
	ftnint ukeyid;  /* current key of reference */
	ftnint unkeys;  /* # of keys */
	Keyspec *ukeys; /* pointer to table of key definitions */
	flag ushared;   /* for keyed files: 0=exclusive, 1=shared */
	ftnint *uassocv; /* associate variable */
	ftnint udisp;	/* disposition flag: 0=keep, 1=delete, 2=print,
				3=print/delete,4=submit, 5=submit/delete */
	ftnint umaxrec; /* maximum direct access records */
	flag ureadonly; /* readonly permission */
	struct UNIT *ualias;   /* alias pointer for carriage control */
	ftnint *umask;	/* type mask for return values */
	ftnint uirec;	/* current direct access record */
	ftnint	flen;	/* length of the file before BACKSPACE/REWIND */
	
} unit;
typedef struct
{	flag inerr;
	ftnint inunit;
	char *infile;
	ftnlen infilen;
	ftnintu	*inex;	/*parameters in standard's order*/
	ftnintu	*inopen;
	ftnintu	*innum;
	ftnintu	*innamed;
	char	*inname;
	ftnlen	innamlen;
	char	*inacc;
	ftnlen	inacclen;
	char	*inseq;
	ftnlen	inseqlen;
	char 	*indir;
	ftnlen	indirlen;
	char	*infmt;
	ftnlen	infmtlen;
	char	*inform;
	ftnlen	informlen;
	char	*inunf;
	ftnlen	inunflen;
	ftnintu	*inrecl;
	ftnintu	*innrec;
	char	*inblank;
	ftnlen	inblanklen;
	char	*indefaultfile;
	ftnlen	indefaultfilelen;
	char	*incc;
	ftnlen	incclen;
	char	*inkeyed;
	ftnlen	inkeyedlen;
	char	*inorg;
	ftnlen	inorglen;
	char	*inrecordtype;
	ftnlen	inrecordtypelen;
} inlist;

extern int errno;
extern ftnint errluno;
extern flag f77init;
extern cilist *f77elist;	/*active external io list*/
extern icilist *f77svic;	/* AGC #1008 4/13/87 */
extern flag f77reading,f77external,f77sequential,f77formatted;
extern int (*f77getn)(),(*f77gets)(),(*f77putn)();	/*for formatted io*/
extern int (*f77do_unf)();		/* unformatted I/O */
extern int (*_libisam_iscleanup)();
extern int (*_libisam_idxrd)();
extern int (*_libisam_idxwrt)();
extern int (*_libisam_idxcls)();
extern FILE *f77cf;	/*current file*/
extern unit *f77curunit;	/*current unit*/
extern unit *f77units;		/* logical unit map table */
				/* sjc #? 11Dec 87 */
unit *map_luno();
#define err(f,n,s) {if(f){errno=n;if(f77curunit) {errluno=f77curunit->luno; f77curunit->uerror = 1;}} else f77fatal(n,s); return(n);}
#define ierr(f,n,s) {if(f){errno=n;if(f77curunit)errluno=f77curunit->luno;} else f77fatal(n,s); return(n);}
					/* AGC #1008 4/13/87 */
#define	CILISTERR	(f77external ? f77elist->cierr : f77svic->icierr)

/*Initial size of f77 unit table*/
#define INIT_MXUNIT 32  /* sjc #? 11Dec 87 */
extern int mxunit;

extern int f77reclen;
extern int f77recpos;	/*position in current record*/

#define WRITE	1
#define READ	2
#define SEQ	3
#define DIR	4
#define FMT	5
#define UNF	6
#define EXT	7
#define INT	8

#define CC_LIST     	  0
#define CC_NONE     	  1
#define CC_FORTRAN  	  2

/*  File access types  */

#define SEQUENTIAL 1
#define DIRECT     2
#define KEYED      3
#define APPEND     4

/* File disposition codes */

#define KEEP	  0
#define DELETE    1
#define PRINT     2
#define SUBMIT    4


typedef struct {
   int (*PFI)();
   char * static_link;
} vfmt_struct;

#endif
