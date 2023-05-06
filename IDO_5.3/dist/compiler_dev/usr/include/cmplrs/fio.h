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
/*  $Header: /proj/irix5.3/isms/cmplrs/commonlib/include/cmplrs/RCS/fio.h,v 7.12 1994/09/08 21:24:12 calvin Exp $ */

#ifndef FIO_INCLUDED
#define FIO_INCLUDED
#include <stdio.h>
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
typedef int ftnint;
#else /* MIPSpro */
#include <sys/types.h>
typedef long ftnint;
#endif /* MIPSpro */
#ifdef _LONGLONG
typedef long long ftnll;
typedef unsigned long long ftnull;
#else
typedef long ftnll;
typedef unsigned long ftnull;
#endif
typedef ftnint flag;
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
typedef int ftnlen;
#else /* MIPSpro */
typedef long ftnlen;
#endif /* MIPSpro */
typedef union {
	char byte;
	short word;
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
	int longword;
#else /* MIPSpro */
	long longword;
#endif /* MIPSpro */
	long long longlongword;
} ftnintu;

typedef struct  {
	short e1;
	short e2;
	short dt;
} Keyspec;

/* external read, write */
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
	ftnint cikeyvallen;
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
				/* Fortran-90 extension */
	char *ciadvance;	/* ADVANCE=		*/
	ftnint ciadvancelen;
	flag cieor;		/* EOR=			*/
	ftnint *cisize;		/* SIZE=		*/
#endif /* MIPSpro */
} cilist;

#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
typedef struct
{
        ftnint cimatch;
        ftnint cikeytype;
        union {
           ftnint ciintval;
           char *cicharval;
        } cikeyval;
        ftnint cikeyid;
        char *cinml;
        ftnint cikeyvallen;
} idxlist;
#endif /* MIPSpro */

/* internal read, write */
typedef struct
{	flag icierr;
	char *iciunit;
	flag iciend;
	char *icifmt;
	ftnint icirlen;
	ftnint icirnum;
} icilist;

#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
typedef struct
{       
	char *iciunit;
	ftnint *cisize;		/* SIZE=		*/
	flag f77external;
	flag cierr;
        flag ciend;
        flag cieor;		/* F90 EOR */
        ftnint icirlen;
        ftnint icirnum;
} errlist;
#endif /* MIPSpro */

/* open */
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
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
				/* Fortran-90 extension */
	char *oaction;		/* ACTION=		*/
	ftnlen oactionlen;
	char *odelim;		/* DELIM=		*/
	ftnlen odelimlen;
	char *opad;		/* PAD=			*/
	ftnlen opadlen;
	char *oposition;	/* POSITION=		*/
	ftnlen opositionlen;
#endif /* MIPSpro */
} olist;

/* close */
typedef struct
{	flag cerr;
	ftnint cunit;
	char *csta;
} cllist;

/* rewind, backspace, endfile, delete, unlock */
typedef struct
{	flag aerr;
	ftnint aunit;
} alist;

/* find */
typedef struct
{	flag ferr;
	ftnint funit;
	ftnint frec;
} flist;

#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
struct f77syl
{       int op;
        long p1;
        long p2;
        int p3;
};

typedef union
{       signed char flbyte;
        short   flshort;
        ftnint  flint;
#ifdef _LONGLONG
        long long flll;
#else
        long flll;
#endif
        float   flreal;
        double  fldouble;
	long double flquad;
} flex;
#endif /* MIPSpro */

/* units */
typedef struct UNIT
{
	FILE *ufd;
	int isfd;	/* C-ISAM file descriptor (for KEYED) */
	flag uconn;	/* 0=unconnected */
	char *ufnm;
	ftnint luno;	/* fortran logical unit number */
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
	unsigned long uinode;
#else /* MIPSpro */
	long uinode;
#endif /* MIPSpro */
	int url;	/* 0=sequential */
	flag useek;	/* true=can backspace, use dir, ... */
	flag ufmt;
	flag uprnt;
	flag ublnk;
	flag uend;
	flag uwrt;	/* last io was write */
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
	ftnint umask;	/* type mask for return values */
	ftnint uirec;	/* current direct access record */
	ftnint flen;	/* length of the file before BACKSPACE/REWIND */
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
			/* Fortran-90 extension */

	ftnint uaction; /* action flag: 0=readwrite, 1=readonly, 2=writeonly */
	ftnint udelim;  /* delimit flag: 0=none, 1=quote, 2=apostrophe
                           delimits constants written with list_direct or namelist */
	flag unpad;      /* pad flag: 0=yes, 1=no
	                   determines whether to pad formattted input with blanks */
	int urecend;	/* record end in the IO buffer */

	/* structures */
	errlist f77errlist;	/* for error recovery */
        idxlist f77idxlist;

	/* 128-bit values */
	flex	lqx;	/* saved value of list-directed read */
	flex	lqy;	/* saved value of list-directed read */

	/* pointers */
	ftnint (*f77getn)(struct UNIT *);
	ftnint (*f77ungetn)(struct UNIT *, int);
	ftnint (*f77gets)(struct UNIT *, char *, int, char);
	ftnint (*f77putn)(struct UNIT *,int, char, char *); /* formatted io */
	ftnint (*f77do_unf)(struct UNIT *, ftnint *, char *, ftnlen); /* unformatted I/O */
	ftnint (*f77doed)(struct UNIT *, struct f77syl *, char *, ftnlen, ftnint);
	ftnint (*f77doned)(struct UNIT *, struct f77syl *);
	ftnint (*dowrite)(struct UNIT *);
	ftnint (*f77doend)(struct UNIT *);
	ftnint (*f77donewrec)(struct UNIT *);
	ftnint (*f77dorevert)(struct UNIT *);
	ftnint (*f77lioproc)(struct UNIT *, ftnint *, flex *, ftnlen, ftnint);
	char   *f77fio_buf;	/* I/O buffer */
	char   *f77fmtbuf;	/* Contents of the FORMAT specification */
	struct f77syl *f77syl;
	char   *startaddr;	/* For F90 namelist */
	long	lock_unit;

	/* 32-bit int */
	ftnint f77reclen;
	ftnint f77recpos;
	ftnint f77cursor;
	ftnint f77recend;
	ftnint f77fio_size;	/* size of the buffer f77fio_buf */
	ftnint lcount;
	ftnint ltype;
	ftnint f77syl_size;
	ftnint nextch;
	ftnint suboffset;	/* for namelist I/O */
	ftnint substr_lb;	/* for namelist I/O */
	ftnint substr_ub;	/* for namelist I/O */

	/* 16-bit integers */
	short int    f77scale;
	short int    parenlvl;
	short int    pc;
	short int    revloc;
	short int	cnt[10];
	short int	ret[10];
	short int	cp;
	short int	rp;

	/* Assorted flags */
	unsigned char f77cplus;
	unsigned char f77cblank;
	unsigned char f77workdone;
	unsigned char f77nonl;
	unsigned char lquit;
	unsigned char overflowed;
	unsigned char l_first;
	unsigned char f90eor;
	unsigned char f90nadv;
	unsigned char f90sw;
	unsigned char subscript;
	unsigned char array_section;
#endif /* MIPSpro */
} unit;


typedef struct
{	flag inerr;
	ftnint inunit;
	char *infile;
	ftnlen infilen;
	ftnintu	*inex;		/* parameters in standard's order */
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
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
					/* Fortran-90 extension */
	char	*inaction;		/* ACTION=		*/
	ftnlen	inactionlen;		
	char	*indelim;		/* DELIM=		*/
	ftnlen	indelimlen;
	char	*inpad;			/* PAD=			*/
	ftnlen	inpadlen;
	char	*inposition;		/* POSITION=		*/
	ftnlen	inpositionlen;
	char	*inread;		/* READ=		*/
	ftnlen	inreadlen;
	char	*inreadwrite;		/* READWRITE=		*/
	ftnlen	inreadwritelen;
	char	*inwrite;		/* WRITE=		*/
	ftnlen	inwritelen;
#endif /* MIPSpro */
} inlist;

#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
extern unit *f77curunit;	/* for backward compatibility */
#endif /* MIPSpro */
extern int errno;
extern ftnint errluno;
extern flag f77init;
extern cilist *f77elist;	/* active external io list */
extern icilist *f77svic;
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
extern int (*_libisam_iscleanup)(unit *);
extern int (*_libisam_idxrd)(unit *);
extern int (*_libisam_idxwrt)(unit *);
extern int (*_libisam_idxcls)(unit *, flag);
extern unit *f77units;          /* logical unit map table */
extern unit * Internal_File;
#else /* MIPSpro */
extern flag f77reading,f77external,f77sequential,f77formatted;
extern int (*f77getn)(),(*f77gets)(),(*f77putn)();	/* for formatted io */
extern int (*f77do_unf)();		/* unformatted I/O */
extern int (*_libisam_iscleanup)();
extern int (*_libisam_idxrd)();
extern int (*_libisam_idxwrt)();
extern int (*_libisam_idxcls)();
extern FILE *f77cf;		/* current fil e*/
extern unit *f77curunit;	/* current unit */
extern unit *f77units;		/* logical unit map table */
#endif /* MIPSpro */

#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
unit *map_luno(int);
#define err(f,n,s) {if(f){errno=n;if(ftnunit) {errluno=ftnunit->luno; ftnunit->uerror = n; ftnunit->lock_unit = 0;}} else f77fatal(ftnunit,n,s); return(n);}
#define ierr(f,n,s) {if(f){errno=n;if(ftnunit)errluno=ftnunit->luno;ftnunit->uerror = n;} else f77fatal(ftnunit,n,s); return(n);}
#else /* MIPSpro */
unit *map_luno();
#define err(f,n,s) {if(f){errno=n;if(f77curunit) {errluno=f77curunit->luno; f77curunit->uerror = errno;}} else f77fatal(n,s); return(n);}
#define ierr(f,n,s) {if(f){errno=n;if(f77curunit)errluno=f77curunit->luno;} else f77fatal(n,s); return(n);}
#endif /* MIPSpro */

#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
#define	CILISTERR	(ftnunit->f77errlist.cierr)
#else /* MIPSpro */
#define	CILISTERR	(f77external ? f77elist->cierr : f77svic->icierr)
#endif /* MIPSpro */

/* Initial size of f77 unit table */
#define INIT_MXUNIT 32
extern int mxunit;

#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
#define EOR     -2
#else /* MIPSpro */
extern int f77reclen;
extern int f77recpos;	/* position in current record */
#endif /* MIPSpro */

#define WRITE	1
#define READ	2
#define SEQ	3
#define DIR	4
#define FMT	5
#define UNF	6
#define EXT	7
#define INT	8

#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
#define CC_LIST     	  (char) 0
#define CC_NONE     	  (char) 1
#define CC_FORTRAN  	  (char) 2
#else /* MIPSpro */
#define CC_LIST     	  0
#define CC_NONE     	  1
#define CC_FORTRAN  	  2
#endif /* MIPSpro */

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

#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
/* File action types  */

#define READWRITE 0
#define READONLY  1
#define WRITEONLY 2

/* File delimeter types  */

#define DELIM_NONE 0
#define QUOTE      1
#define APOSTROPHE 2
#endif /* MIPSpro */

typedef struct {
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
   int (*PFI)(void *);
#else /* MIPSpro */
   int (*PFI)();
#endif /* MIPSpro */
   char * static_link;
} vfmt_struct;
#endif
