/*
 *  The following defines and structures are borrowed 
 *	from X11R5.  When R5 is available, they may be deleted
 *	and the real include files may be used.
 */

/*
 * $XConsortium: pcfread.c,v 1.7 91/07/22 22:58:57 keith Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, MIT X Consortium
 */


/*  The _real_ header files ...
#include    "fontfilest.h"
#include    "bitmap.h"
#include    "pcf.h"
*/

#include <stdio.h>
#include <X11/Xproto.h>

#define	TRUE	1
#define	FALSE	0
#define Successful 85

typedef FILE    *FontFilePtr;
typedef	int	Bool;
#define FontFileGetc(f)     getc(f)
#define xalloc( n )	malloc( n )
#define xfree( n )	free( n )
#define Xalloc( n )	malloc( n )
#define Xfree( n )	free( n )
#define LSBFirst        0
#define MSBFirst        1

typedef struct _PCFTable {
    unsigned long      type;
    unsigned long      format;
    unsigned long      size;
    unsigned long      offset;
}           PCFTableRec, *PCFTablePtr;

typedef struct _CharInfo {
    xCharInfo   metrics;        /* info preformatted for Queries */
    char       *bits;           /* pointer to glyph image */
}           CharInfoRec;
typedef struct _CharInfo *CharInfoPtr;

#define FontFileSkip(f,n)   (fseek(f,n,1) != -1)
#define FontFileRead(f,b,n) fread((char *) b, 1, n, f)

#define PCF_FILE_VERSION	(('p'<<24)|('c'<<16)|('f'<<8)|1)
#define	PCF_FORMAT_MASK		0xffffff00
#define PCF_DEFAULT_FORMAT	0x00000000
#define PCF_COMPRESSED_METRICS	0x00000100
#define PCF_FORMAT_MATCH(a,b) (((a)&PCF_FORMAT_MASK) == ((b)&PCF_FORMAT_MASK))
#define GLYPHPADOPTIONS 4     /* 1, 2, 4, or 8 */
#define PCF_GLYPH_PAD_MASK	(3<<0)
#define PCF_BYTE_MASK		(1<<2)
#define PCF_BYTE_ORDER(f)	(((f) & PCF_BYTE_MASK)?MSBFirst:LSBFirst)
#define PCF_GLYPH_PAD_INDEX(f)	((f) & PCF_GLYPH_PAD_MASK)
#define PCF_GLYPH_PAD(f)	(1<<PCF_GLYPH_PAD_INDEX(f))
#define PCF_SIZE_TO_INDEX(s)	((s) == 4 ? 2 : (s) == 2 ? 1 : 0)
#define PCF_FORMAT(bit,byte,glyph,scan) (\
    (PCF_SIZE_TO_INDEX(scan) << 4) | \
    (((bit) == MSBFirst ? 1 : 0) << 3) | \
    (((byte) == MSBFirst ? 1 : 0) << 2) | \
    (PCF_SIZE_TO_INDEX(glyph) << 0))
#define PCF_METRICS		    (1<<2)
#define PCF_BITMAPS		    (1<<3)
#define	PCF_BDF_ENCODINGS	    (1<<5)
#define BYTES_PER_ROW(bits, nbytes) \
        ((nbytes) == 1 ? (((bits)+7)>>3)        /* pad to 1 byte */ \
        :(nbytes) == 2 ? ((((bits)+15)>>3)&~1)  /* pad to 2 bytes */ \
        :(nbytes) == 4 ? ((((bits)+31)>>3)&~3)  /* pad to 4 bytes */ \
        :(nbytes) == 8 ? ((((bits)+63)>>3)&~7)  /* pad to 8 bytes */ \
        : 0)

typedef struct _FontProp {
    long        name;
    long        value;          /* assumes ATOM is not larger than INT32 */
}           FontPropRec;
typedef struct _FontProp *FontPropPtr;

typedef struct _FontInfo {
    unsigned short firstCol;
    unsigned short lastCol;
    unsigned short firstRow;
    unsigned short lastRow;
    unsigned short defaultCh;
    unsigned int noOverlap:1;
    unsigned int terminalFont:1;
    unsigned int constantMetrics:1;
    unsigned int constantWidth:1;
    unsigned int inkInside:1;
    unsigned int inkMetrics:1;
    unsigned int allExist:1;
    unsigned int drawDirection:2;
    unsigned int cachable:1;
    unsigned int anamorphic:1;
    short       maxOverlap;
    short       pad;
    xCharInfo   maxbounds;
    xCharInfo   minbounds;
    xCharInfo   ink_maxbounds;
    xCharInfo   ink_minbounds;
    short       fontAscent;
    short       fontDescent;
    int         nprops;
    FontPropPtr props;
    char       *isStringProp;
}           FontInfoRec;

typedef unsigned long  fsBitmapFormat;

typedef unsigned char   *pointer;

typedef struct _FontPathElement {
    int         name_length;
    char       *name;
    int         type;
    int         refcount;
    pointer     private;
}           FontPathElementRec;
typedef struct _FontPathElement *FontPathElementPtr;

typedef struct _Font {
    int         refcnt;
    FontInfoRec info;
    char        bit;
    char        byte;
    char        glyph;
    char        scan;
    fsBitmapFormat format;
    int         (*get_glyphs) ( /* font, count, chars, encoding, count, glyphs
*/ );
    int         (*get_metrics) ( /* font, count, chars, encoding, count, glyphs */ );
    int         (*get_bitmaps) (/* client, font, flags, ranges, nranges,
                                    nextents, extents */ );
    int         (*get_extents) (/* client, font, format, flags, ranges,
                                    nranges, nglyphs, offsets, glyphs */ );
    void        (*unload_font) ( /* font */ );
    FontPathElementPtr fpe;
    pointer     svrPrivate;
    pointer     fontPrivate;
    pointer     fpePrivate;
    int         maxPrivate;
    pointer     *devPrivates;
}           FontRec;
typedef struct _Font *FontPtr;


/* X11R5 header stuff ends here. */



/* X11R5 code begins here. */

/* Read PCF font files */

static int  position;

static int
pcfGetLSB32(file)
    FontFilePtr file;
{
    int         c;

    c = FontFileGetc(file);
    c |= FontFileGetc(file) << 8;
    c |= FontFileGetc(file) << 16;
    c |= FontFileGetc(file) << 24;
    position += 4;
    return c;
}

static int
pcfGetINT32(file, format)
    FontFilePtr file;
    CARD32      format;
{
    int         c;

    if (PCF_BYTE_ORDER(format) == MSBFirst) {
	c = FontFileGetc(file) << 24;
	c |= FontFileGetc(file) << 16;
	c |= FontFileGetc(file) << 8;
	c |= FontFileGetc(file);
    } else {
	c = FontFileGetc(file);
	c |= FontFileGetc(file) << 8;
	c |= FontFileGetc(file) << 16;
	c |= FontFileGetc(file) << 24;
    }
    position += 4;
    return c;
}

static int
pcfGetINT16(file, format)
    FontFilePtr file;
    CARD32      format;
{
    int         c;

    if (PCF_BYTE_ORDER(format) == MSBFirst) {
	c = FontFileGetc(file) << 8;
	c |= FontFileGetc(file);
    } else {
	c = FontFileGetc(file);
	c |= FontFileGetc(file) << 8;
    }
    position += 2;
    return c;
}

#define pcfGetINT8(file, format) (position++, FontFileGetc(file))

static      PCFTablePtr
pcfReadTOC(file, countp)
    FontFilePtr file;
    int        *countp;
{
    CARD32      version;
    PCFTablePtr tables;
    int         count;
    int         i;

    position = 0;
    version = pcfGetLSB32(file);
    if (version != PCF_FILE_VERSION)
	return (PCFTablePtr) NULL;
    count = pcfGetLSB32(file);
    tables = (PCFTablePtr) xalloc(count * sizeof(PCFTableRec));
    if (!tables)
	return (PCFTablePtr) NULL;
    for (i = 0; i < count; i++) {
	tables[i].type = pcfGetLSB32(file);
	tables[i].format = pcfGetLSB32(file);
	tables[i].size = pcfGetLSB32(file);
	tables[i].offset = pcfGetLSB32(file);
    }
    *countp = count;
    return tables;
}

/*
 * PCF supports two formats for metrics, both the regular
 * jumbo size, and 'lite' metrics, which are useful
 * for most fonts which have even vaguely reasonable
 * metrics
 */

static
pcfGetMetric(file, format, metric)
    FontFilePtr file;
    CARD32      format;
    xCharInfo  *metric;
{
    metric->leftSideBearing = pcfGetINT16(file, format);
    metric->rightSideBearing = pcfGetINT16(file, format);
    metric->characterWidth = pcfGetINT16(file, format);
    metric->ascent = pcfGetINT16(file, format);
    metric->descent = pcfGetINT16(file, format);
    metric->attributes = pcfGetINT16(file, format);
}

static
pcfGetCompressedMetric(file, format, metric)
    FontFilePtr file;
    CARD32      format;
    xCharInfo  *metric;
{
    metric->leftSideBearing = pcfGetINT8(file, format) - 0x80;
    metric->rightSideBearing = pcfGetINT8(file, format) - 0x80;
    metric->characterWidth = pcfGetINT8(file, format) - 0x80;
    metric->ascent = pcfGetINT8(file, format) - 0x80;
    metric->descent = pcfGetINT8(file, format) - 0x80;
    metric->attributes = 0;
}

/*
 * Position the file to the begining of the specified table
 * in the font file
 */
static Bool
pcfSeekToType(file, tables, ntables, type, formatp, sizep)
    FontFilePtr file;
    PCFTablePtr tables;
    int         ntables;
    CARD32      type;
    CARD32     *formatp;
    CARD32     *sizep;
{
    int         i;

    for (i = 0; i < ntables; i++)
	if (tables[i].type == type) {
	    if (position > tables[i].offset)
		abort ();
	    if (!FontFileSkip(file, tables[i].offset - position))
		return FALSE;
	    position = tables[i].offset;
	    *sizep = tables[i].size;
	    *formatp = tables[i].format;
	    return TRUE;
	}
    return FALSE;
}

/* X11R5 code ends here. */

/*
 *	Beginning of hpcf_*() hashed-pcf management routines.
 *
 *	These routines manage a pcf file reading in only
 *	those bitmaps requested, and register them with the GL
 *	using deflfont().
 *
 */

#include <gl/gl.h> 
#include <signal.h>

typedef struct _HashPcfFontRec 
{
        void *		next;		/* must match genericrec */
        unsigned long	name;		/* must match genericrec */
        FontFilePtr	fontf;
	PCFTablePtr     tables;
	unsigned short*	encoding;
	int             ntables;
        int		firstCol;
        int		lastCol;
        int		firstRow;
        int		lastRow;
        int		nencoding;
        int		scale;
        unsigned int	delta;
} HashPcfFontRec;


/*
 *  Miscellaneous pcf equations.
 */
#define n2dCols(font)   ((font)->lastCol - (font)->firstCol +1 )
#define EnctoppCI( p, v ) ((v/256)*n2dCols( p ) + (v % 256))
#define ppCItoEnc( p, i ) ((i) / n2dCols( p ) * 256 + (i) % n2dCols( p))


#define	ATTRIB_HASH_SIZE	128
#define ATTRIB_HASH(x)  ((x) & (ATTRIB_HASH_SIZE - 1))
/* 
 *  Static hash table for managing open fonts.
 */
static HashPcfFontRec*  hpcf_fonttab[ATTRIB_HASH_SIZE];

/* Functions */
static void *copyrasters(unsigned short *, Lfontchar *, unsigned char *, int );
static FontFilePtr getfontfile( char * );
static void copymetrics(Lfontchar *, CharInfoPtr, long );
static void hpcf_freeFont( HashPcfFontRec * );
static bmscale( unsigned short *, Lfontchar *lfc, unsigned short *, int );

/* Hash Functions */
typedef struct _Genericrec {
        void *          next;           
	unsigned long   name;
} Genericrec;
	
static Genericrec *hsh_remove( Genericrec *[], long );
static Genericrec *hsh_find( Genericrec *[], long );
static void hsh_insert( Genericrec **, Genericrec * );

void
hpcf_initialize( )
{
	bzero( hpcf_fonttab, ATTRIB_HASH_SIZE * sizeof( void *) );
}

int
hpcf_openFont( char *filename, int fontnum, unsigned int delta, int scale )
{
	FontFilePtr	fontf = 0;
	HashPcfFontRec*	fontrec = 0;
	Lfontchar	lfc;
	PCFTablePtr	tables = 0;
	int		nencoding, ntables;
	int		format, i, junk, r, size;
	unsigned short*	encoding = 0;
	unsigned short	encodingOffset;

	/* Try to open the file specified. */
	if( ( fontf = getfontfile( filename ) ) == NULL )
		goto Bail;

	position = 0;	/* This global variable was not my idea!  
			   It came with the X11R5 code in pcfread.c.
			   Why not use ftell()?? 	-- mlk */
	if( ! ( tables = pcfReadTOC( fontf, &ntables ) ) )
		goto Bail;

	/* Save this information. */
	fontrec = (HashPcfFontRec *) malloc( sizeof( HashPcfFontRec ) );
	bzero( fontrec, sizeof( HashPcfFontRec ) );

	fontrec->next    = NULL;
	fontrec->name    = fontnum;
	fontrec->scale   = scale;
	fontrec->fontf   = fontf;
	fontrec->tables  = tables;
	fontrec->ntables = ntables;
	fontrec->delta	 = delta;

	if (!pcfSeekToType(fontf, tables, ntables, PCF_BDF_ENCODINGS, 
		&format, &size))
		goto Bail;
	format = pcfGetLSB32(fontf);
	if (!PCF_FORMAT_MATCH(format, PCF_DEFAULT_FORMAT))
        	return -1;

	fontrec->firstCol = pcfGetINT16(fontf, format);
	fontrec->lastCol  = pcfGetINT16(fontf, format);
	fontrec->firstRow = pcfGetINT16(fontf, format);
	fontrec->lastRow  = pcfGetINT16(fontf, format);
	junk  		  = pcfGetINT16(fontf, format);	/* defCh */

	nencoding = (fontrec->lastCol - fontrec->firstCol + 1) *
		(fontrec->lastRow - fontrec->firstRow + 1);
	fontrec->nencoding = nencoding;

	encoding = (unsigned short *) xalloc(nencoding * sizeof(short));
	if (!encoding)
		return -1;
	fontrec->encoding = encoding;

	for (i = 0; i < nencoding; i++)
		encoding[i] = pcfGetINT16(fontf, format);

	hsh_insert( (Genericrec **)hpcf_fonttab, (Genericrec *)fontrec );

	/*  
	 *  Add a character to the font to avoid stupid bug
	 *  messages from font()
	 */
	bzero( &lfc, sizeof( lfc ) );
	deflfont( fontnum, 1, &lfc, 0, 0 );

	return 1;

Bail:
	if( fontrec )	free( fontrec );
	if( fontf )	fclose( fontf );
	if( encoding )	free( encoding );
	if( tables )	free( tables );
	return r;
}

/*
 *   I do not have an answer for what format a string is.
 *   Punt.
 */

/*	commented out ...
hpcf_checkString( int format, void *string, int fontnum )
{
	unsigned short *sh;
	int i;

	sh = (unsigned short *) string;
	for( i = 0; sh[ i ]; i++ )
		hpcf_checkGlyph( sh[ i ], fontnum );
}
*/

int
hpcf_checkGlyph( int glyphnum, int fontnum )
{
	CharInfoRec	chrec;
	HashPcfFontRec	*hpfr = NULL;
	unsigned char	*bitmaps = NULL;
	unsigned short	*sbitmaps = NULL;
	unsigned short	*xbitmaps = NULL;
	unsigned long	string[2];
	int 		format, i, nbitmaps, nmetrics, offsets_i, size, v;
	int		sizedest, sizebitmaps;
	int		fnttmp;
	Lfontchar	lfc;
	FILE 		*file = NULL;
	CARD32		bitmapSizes[GLYPHPADOPTIONS];

	/* Check to see if the glyph is already there. */
	string[0] = glyphnum;
	string[1] = 0;

	fnttmp = getfont( );	/* Save the current font. */
	font( fontnum );
	if( lstrwidth( STR_32, string ) != 0 )	/* Already there!! Hack! */
	{
		font( fnttmp );	/* Restore the current font. */
		return 2;  /* This return value is useful for testing. */
	}

	hpfr = (HashPcfFontRec*) hsh_find( (Genericrec **)hpcf_fonttab, fontnum );
	if( !hpfr )
		goto Bail;

	/* Array position for this glyph. */
	v =  EnctoppCI( hpfr, glyphnum );
	if( v > hpfr->nencoding )	/* Out of range? */
		goto Bail;

	v =  hpfr->encoding[ v ];	/* Use the same variable. */
	if( v == 0xffff )
		goto Bail;

	file = hpfr->fontf;
	/* get the metric data */
	position = 0;	/* This global variable was not my idea!  -- mlk */
	fseek( file, SEEK_SET, 0 );
	if( ! pcfSeekToType( file, hpfr->tables, hpfr->ntables, 
	    PCF_METRICS, &format, &size)) 
			goto Bail;

	format = pcfGetLSB32(file);
	if (!PCF_FORMAT_MATCH(format, PCF_DEFAULT_FORMAT) &&
	    !PCF_FORMAT_MATCH(format, PCF_COMPRESSED_METRICS))
		goto Bail;
	if (PCF_FORMAT_MATCH(format, PCF_DEFAULT_FORMAT))
		nmetrics = pcfGetINT32(file, format);
	else
		nmetrics = pcfGetINT16(file, format);

	/* Skip the metrics in which we are not interested. */
	if (PCF_FORMAT_MATCH(format, PCF_DEFAULT_FORMAT))
		fseek( file, 12 * v, SEEK_CUR ); /* Hard-coded constants. */
	else
		fseek( file, 5 * v, SEEK_CUR );

	if (PCF_FORMAT_MATCH(format, PCF_DEFAULT_FORMAT))
		pcfGetMetric(file, format, &(chrec.metrics) );
	else
		pcfGetCompressedMetric(file, format, &(chrec.metrics) );

	copymetrics( &lfc, &chrec, 0 );

	/* Get the bitmaps. */
	position = 0;	/* This global variable was not my idea!  -- mlk */
	fseek( file, SEEK_SET, 0 );

	if( ! pcfSeekToType( file, hpfr->tables, hpfr->ntables, 
		PCF_BITMAPS, &format, &size)) {
			goto Bail;
	}
	format = pcfGetLSB32(file);
	if (!PCF_FORMAT_MATCH(format, PCF_DEFAULT_FORMAT))
		goto Bail;

	nbitmaps = pcfGetINT32(file, format);

/* Get the offset we need, and skip the rest ... -- mlk */
	fseek( file, 4 * v, SEEK_CUR );
	offsets_i = pcfGetINT32(file, format);
	fseek( file, 4 * ((nbitmaps - v) - 1), SEEK_CUR );

	for (i = 0; i < GLYPHPADOPTIONS; i++)
		bitmapSizes[i] = pcfGetINT32(file, format);

/* Get the bitmap we need, and skip the rest ... -- mlk */
	fseek( file, offsets_i, SEEK_CUR );
	sizebitmaps = lfc.h * BYTES_PER_ROW(lfc.w, 2); /* 2 == sizeof short */

	bitmaps = (unsigned char *) xalloc(sizebitmaps);
	FontFileRead(file, bitmaps, sizebitmaps);
	sbitmaps = (unsigned short *) xalloc(sizebitmaps);
	copyrasters( sbitmaps, &lfc, bitmaps, 
		BYTES_PER_ROW( lfc.w, PCF_GLYPH_PAD( format ) ) );
	if( hpfr->scale != 1 )
	{
		sizedest = sizebitmaps * hpfr->scale * hpfr->scale;
		/* expanded bitmaps */
		xbitmaps = (unsigned short *) xalloc(sizedest);
		bmscale( xbitmaps, &lfc, sbitmaps, hpfr->scale );
	}

	lfc.value = glyphnum + hpfr->delta;

	/* Add this glyph to a GL font. */
	if( hpfr->scale == 1 )
		deflfont( fontnum, 1, &lfc, sizebitmaps/2, sbitmaps );
	else
		deflfont( fontnum, 1, &lfc, sizedest/2, xbitmaps );

	if( bitmaps ) xfree( bitmaps );
	if( sbitmaps ) xfree( sbitmaps );
	if( xbitmaps ) xfree( xbitmaps );
	
	font( fnttmp );
	return 1;	/* added successfully */
Bail:
	if( bitmaps ) xfree( bitmaps );
	if( sbitmaps ) xfree( sbitmaps );
	if( xbitmaps ) xfree( xbitmaps );
	
	font( fnttmp );
	return -1;
}


void
hpcf_closeFont( int fontnum )
{

        CharInfoRec     chrec;
        HashPcfFontRec  *hpfr;
        unsigned char   *bitmaps;
        unsigned short  *sbitmaps;
        int             format, i, nbitmaps, nmetrics, offsets_i, size, v;
        int             sizebitmaps;
        Lfontchar       lfc;
        FILE            *file;
        CARD32          bitmapSizes[GLYPHPADOPTIONS];
        int r;

        hpfr = (HashPcfFontRec*) hsh_remove( (Genericrec **)hpcf_fonttab, fontnum );
        if( !hpfr )
                return;
	hpcf_freeFont( hpfr );
}


static void
hpcf_freeFont( HashPcfFontRec* hpfr )
{
	if( !hpfr )
		return;

	if( hpfr->fontf )	fclose( hpfr->fontf );
	if( hpfr->tables )	free( hpfr->fontf );
	if( hpfr->encoding )	free( hpfr->encoding );

	deflfont( hpfr->name, 0, 0, 0, 0 );

	free( hpfr );
}


static FontFilePtr 
getfontfile( char *fname )
{
	static char file[ 256 ];
	char command[256];
	int i;
	char *tmpfilename;
	FontFilePtr fontfilep;
	void        (*childstat)();
	static char *places[ ] = {
		"/usr/lib/X11/fonts/misc",
		"/usr/lib/X11/fonts/100dpi",
		"/usr/lib/X11/fonts/75dpi",
		"/usr/lib/fmfonts",
		".",
		NULL 				};

	if( ( tmpfilename = (char *)getenv("TMPDIR") ) == 0 )
		tmpfilename = "/tmp";

	/* looked for uncompressed versions */
	for( i = 0; places[ i ]; i++ )
	{
		sprintf(file, "%s/%s", places[ i ], fname);
		if ((fontfilep = fopen(file, "r")) != NULL)
			return fontfilep;
	}

	/* looked for compressed versions */
	for( i = 0; places[ i ]; i++ )
	{
		sprintf(file, "%s/%s.Z", places[ i ], fname);
		if((fontfilep = fopen(file, "r")) != NULL) {
			fclose( fontfilep );
			childstat = signal(SIGCHLD, SIG_DFL);
			sprintf(command,"uncompress -c %s > %s/decode_%d",
			    file, tmpfilename, getpid());
			system(command);
			(void)signal(SIGCHLD, childstat);

			/* now "file" will be used for the new file name */
			sprintf(file, "%s/decode_%d", tmpfilename, getpid());
			fontfilep = fopen(file, "r");
			unlink( file );
			return fontfilep;
		}
	}

	return NULL;
}

static void
copymetrics( Lfontchar *chp, CharInfoPtr cip, long offset )
{
	xCharInfo	*mp;

	mp = &cip->metrics;
	chp->offset = offset;
	chp->w = mp->rightSideBearing - mp->leftSideBearing;
	chp->h = mp->ascent + mp->descent;
	chp->xoff = mp->leftSideBearing;
	chp->yoff = -mp->descent;
	chp->xmove = mp->characterWidth;
	chp->ymove = 0;
}

static void *
copyrasters(unsigned short *tr,Lfontchar *chp, unsigned char *cp,int pcfnbpr)
{
	int	nbpr, i, j;
	unsigned char *f, *t, bits; 

	nbpr = (chp->w + 7)/8;
	/*	t = (unsigned char *)&r[chp->offset]; */
	t = (unsigned char *)  tr;
	for (i = chp->h-1; i>=0; i--) {
		f = cp + i*pcfnbpr;
		if ((int)t&1) t++;
		for (j=0; j < nbpr; j++) {
			bits = *f++;
			*t++ = bits;
		}
	}
	return t;
}


int
hpcf_loadAll( char *filename, int fontid, unsigned int delta, int scale )
{
    unsigned	*encoding = 0;
    int         encodingOffset;
    int         i, j;
    int         nbitmaps;
    int         nencoding;
    int         nmetrics;
    int         ntables;
    int         sizebitmaps;
    char       *bitmaps = 0;
    unsigned short* xbitmaps = 0;
    CARD32      format;
    CARD32      size;
    CharInfoPtr metrics = 0;
    PCFTablePtr tables = 0;
    CARD32      bitmapSizes[GLYPHPADOPTIONS];
    CARD32	*offsets = 0;
    FILE	*file = 0;
    FontRec	fr;
    FontPtr	pFont = &fr;
    Lfontchar 	*lfc;
    unsigned short* s, *sbm;

    if( ( file = getfontfile( filename ) ) == NULL )
	goto Bail;

    if (!(tables = pcfReadTOC(file, &ntables)))
	goto Bail;

    /* metrics */
    if (!pcfSeekToType(file, tables, ntables, PCF_METRICS, &format, &size))
	goto Bail;
    format = pcfGetLSB32(file);
    if (!PCF_FORMAT_MATCH(format, PCF_DEFAULT_FORMAT) &&
	    !PCF_FORMAT_MATCH(format, PCF_COMPRESSED_METRICS)) 
	goto Bail;
    if (PCF_FORMAT_MATCH(format, PCF_DEFAULT_FORMAT))
	nmetrics = pcfGetINT32(file, format);
    else
	nmetrics = pcfGetINT16(file, format);
    metrics = (CharInfoPtr) xalloc(nmetrics * sizeof(CharInfoRec));
    if (!metrics) 
	goto Bail;
    for (i = 0; i < nmetrics; i++)
	if (PCF_FORMAT_MATCH(format, PCF_DEFAULT_FORMAT))
	    pcfGetMetric(file, format, &(metrics + i)->metrics);
	else
	    pcfGetCompressedMetric(file, format, &(metrics + i)->metrics);

    /* bitmaps */
    if (!pcfSeekToType(file, tables, ntables, PCF_BITMAPS, &format, &size))
	goto Bail;
    format = pcfGetLSB32(file);
    if (!PCF_FORMAT_MATCH(format, PCF_DEFAULT_FORMAT))
	goto Bail;

    nbitmaps = pcfGetINT32(file, format);
    if (nbitmaps != nmetrics) 
	goto Bail;

    offsets = (CARD32 *) xalloc(nbitmaps * sizeof(CARD32));
    if (!offsets) 
	goto Bail;

    for (i = 0; i < nbitmaps; i++)
	offsets[i] = pcfGetINT32(file, format);

    for (i = 0; i < GLYPHPADOPTIONS; i++)
	bitmapSizes[i] = pcfGetINT32(file, format);
    sizebitmaps = bitmapSizes[PCF_GLYPH_PAD_INDEX(format)];
    bitmaps = (char *) xalloc(sizebitmaps);
    if (!bitmaps)
	goto Bail;
    FontFileRead(file, bitmaps, sizebitmaps);
    position += sizebitmaps;

    /* encoding */
    if (!pcfSeekToType(file, tables, ntables, PCF_BDF_ENCODINGS, &format, &size))
	goto Bail;
    format = pcfGetLSB32(file);
    if (!PCF_FORMAT_MATCH(format, PCF_DEFAULT_FORMAT))
	goto Bail;

    pFont->info.firstCol = pcfGetINT16(file, format);
    pFont->info.lastCol = pcfGetINT16(file, format);
    pFont->info.firstRow = pcfGetINT16(file, format);
    pFont->info.lastRow = pcfGetINT16(file, format);
    pFont->info.defaultCh = pcfGetINT16(file, format);

    nencoding = (pFont->info.lastCol - pFont->info.firstCol + 1) *
	(pFont->info.lastRow - pFont->info.firstRow + 1);

    encoding = (unsigned int *) xalloc(nencoding * sizeof(int));
    if (!encoding)
	goto Bail;

    for (i = 0; i < nencoding; i++)
	encoding[i] = pcfGetINT16(file, format);

    lfc = (Lfontchar *) malloc( nbitmaps * sizeof( Lfontchar ) );

    /* Bitmap as array of shorts. */
    s = sbm = (unsigned short *) malloc( sizebitmaps );
    for( j = i = 0; i < nencoding; i++ )
    {
	if( encoding[ i ]  == 0xFFFF )
		continue;
	lfc[j].value = ppCItoEnc( &(pFont->info), i ) + delta;
	copymetrics( lfc + j, metrics + j, offsets[j]/2 );
	s = (unsigned short *) copyrasters( s, 
		lfc+j, 
		((unsigned char*)bitmaps) + offsets[j],
		BYTES_PER_ROW( (lfc+j)->w, PCF_GLYPH_PAD( format ) ) );
	j++;
    }

    if( scale != 1 )
    {
	xbitmaps = (unsigned short *) malloc( sizebitmaps * scale * scale );
	for( i = 0; i < nbitmaps; i++ )
		bmscale( &(xbitmaps[lfc[i].offset * scale * scale]),
			lfc+i, 
			&(sbm[lfc[i].offset]),
			scale );
    }

    if( scale == 1 )
	deflfont( fontid, nbitmaps, lfc, sizebitmaps/2, sbm );
    else
	deflfont( fontid, nbitmaps, lfc, sizebitmaps*scale*scale/2, xbitmaps );

    if( sbm ) xfree(sbm);
    if( encoding ) xfree(encoding);
    if( tables ) xfree(tables);
    if( offsets ) xfree(offsets);
    if( bitmaps ) xfree(bitmaps);
    if( xbitmaps ) xfree(bitmaps);
    if( lfc ) free(lfc);
    if( file ) fclose(file);
    return Successful;
Bail:
    if( sbm ) xfree(sbm);
    if( encoding ) xfree(encoding);
    if( tables ) xfree(tables);
    if( xbitmaps ) xfree(bitmaps);
    if( offsets ) xfree(offsets);
    if( bitmaps ) xfree(bitmaps);
    if( lfc ) free(lfc);
    if( file ) fclose(file);
    return -1;
}


static unsigned short dbltbl[256] = {
0x0000, 0x0003, 0x000c, 0x000f, 0x0030, 0x0033, 0x003c, 0x003f, 
0x00c0, 0x00c3, 0x00cc, 0x00cf, 0x00f0, 0x00f3, 0x00fc, 0x00ff, 
0x0300, 0x0303, 0x030c, 0x030f, 0x0330, 0x0333, 0x033c, 0x033f, 
0x03c0, 0x03c3, 0x03cc, 0x03cf, 0x03f0, 0x03f3, 0x03fc, 0x03ff, 
0x0c00, 0x0c03, 0x0c0c, 0x0c0f, 0x0c30, 0x0c33, 0x0c3c, 0x0c3f, 
0x0cc0, 0x0cc3, 0x0ccc, 0x0ccf, 0x0cf0, 0x0cf3, 0x0cfc, 0x0cff, 
0x0f00, 0x0f03, 0x0f0c, 0x0f0f, 0x0f30, 0x0f33, 0x0f3c, 0x0f3f, 
0x0fc0, 0x0fc3, 0x0fcc, 0x0fcf, 0x0ff0, 0x0ff3, 0x0ffc, 0x0fff, 
0x3000, 0x3003, 0x300c, 0x300f, 0x3030, 0x3033, 0x303c, 0x303f, 
0x30c0, 0x30c3, 0x30cc, 0x30cf, 0x30f0, 0x30f3, 0x30fc, 0x30ff, 
0x3300, 0x3303, 0x330c, 0x330f, 0x3330, 0x3333, 0x333c, 0x333f, 
0x33c0, 0x33c3, 0x33cc, 0x33cf, 0x33f0, 0x33f3, 0x33fc, 0x33ff, 
0x3c00, 0x3c03, 0x3c0c, 0x3c0f, 0x3c30, 0x3c33, 0x3c3c, 0x3c3f, 
0x3cc0, 0x3cc3, 0x3ccc, 0x3ccf, 0x3cf0, 0x3cf3, 0x3cfc, 0x3cff, 
0x3f00, 0x3f03, 0x3f0c, 0x3f0f, 0x3f30, 0x3f33, 0x3f3c, 0x3f3f, 
0x3fc0, 0x3fc3, 0x3fcc, 0x3fcf, 0x3ff0, 0x3ff3, 0x3ffc, 0x3fff, 
0xc000, 0xc003, 0xc00c, 0xc00f, 0xc030, 0xc033, 0xc03c, 0xc03f, 
0xc0c0, 0xc0c3, 0xc0cc, 0xc0cf, 0xc0f0, 0xc0f3, 0xc0fc, 0xc0ff, 
0xc300, 0xc303, 0xc30c, 0xc30f, 0xc330, 0xc333, 0xc33c, 0xc33f, 
0xc3c0, 0xc3c3, 0xc3cc, 0xc3cf, 0xc3f0, 0xc3f3, 0xc3fc, 0xc3ff, 
0xcc00, 0xcc03, 0xcc0c, 0xcc0f, 0xcc30, 0xcc33, 0xcc3c, 0xcc3f, 
0xccc0, 0xccc3, 0xcccc, 0xcccf, 0xccf0, 0xccf3, 0xccfc, 0xccff, 
0xcf00, 0xcf03, 0xcf0c, 0xcf0f, 0xcf30, 0xcf33, 0xcf3c, 0xcf3f, 
0xcfc0, 0xcfc3, 0xcfcc, 0xcfcf, 0xcff0, 0xcff3, 0xcffc, 0xcfff, 
0xf000, 0xf003, 0xf00c, 0xf00f, 0xf030, 0xf033, 0xf03c, 0xf03f, 
0xf0c0, 0xf0c3, 0xf0cc, 0xf0cf, 0xf0f0, 0xf0f3, 0xf0fc, 0xf0ff, 
0xf300, 0xf303, 0xf30c, 0xf30f, 0xf330, 0xf333, 0xf33c, 0xf33f, 
0xf3c0, 0xf3c3, 0xf3cc, 0xf3cf, 0xf3f0, 0xf3f3, 0xf3fc, 0xf3ff, 
0xfc00, 0xfc03, 0xfc0c, 0xfc0f, 0xfc30, 0xfc33, 0xfc3c, 0xfc3f, 
0xfcc0, 0xfcc3, 0xfccc, 0xfccf, 0xfcf0, 0xfcf3, 0xfcfc, 0xfcff, 
0xff00, 0xff03, 0xff0c, 0xff0f, 0xff30, 0xff33, 0xff3c, 0xff3f, 
0xffc0, 0xffc3, 0xffcc, 0xffcf, 0xfff0, 0xfff3, 0xfffc, 0xffff, 
};

static
bmscale( unsigned short *sraster, Lfontchar *lfc, unsigned short *raster,
	int scale )
{
	short w, h, bwid;	/*char bit width & height, byte width*/
	short bytx, byty;	/*pointers into char raster*/
	long sbit;		/*pointers into schar sraster*/
	long rptr, srptr;
	long sbwid;                 /*byte width of schar*/
	unsigned char *bx;

	if( scale != 2 )	/* only 2 is supported now */
		return;

	w     = lfc->w;     /*char width  (bits) */
	h     = lfc->h;     /*char height (bits) */
	rptr  = 0;
	bwid  = (short) (15 + w) >> 4;

	srptr = 0;	/*raster byte pointer*/
	sbit = 0;	/*bit ptr, sraster*/
	sbwid  = (short) (15 + lfc->w * scale) >> 4;

	for (byty = 0; byty < h; byty++) {
		bx = (unsigned char *)&(raster[rptr]);
		for (bytx = 0; bytx < sbwid; bytx++) {
			sraster[srptr] = sraster[srptr+sbwid] = dbltbl[*bx++];
			srptr++;
		}
		srptr += sbwid;
		rptr += bwid;
	}

	lfc->w		*= scale;
	lfc->h		*= scale;
	lfc->xoff	*= scale;
	lfc->yoff	*= scale;
	lfc->xmove	*= scale;
	lfc->ymove	*= scale;
	lfc->offset	*= scale * scale;
}

/*
 * hsh_find --- find a record in table with name == n
 *	Move found record to front of table so next search is faster.
 *	return pointer to record.
 */
static Genericrec *
hsh_find(table, name)
Genericrec *table[];
register long name;
{
    register Genericrec *tptr, *otptr, **stptr;

    stptr = &table[ATTRIB_HASH(name)];
    for(otptr = tptr = *stptr; tptr != 0; otptr = tptr, tptr = tptr->next) {
	if(tptr->name == name) {
	    if(tptr != otptr) {/* if not there, move to front of list */
		otptr->next = tptr->next;
		tptr->next = *stptr;
		*stptr = tptr;
	    }
	    return(tptr);
	}
    }
    return(NULL);
}

/*
 * hsh_insert --- add an entry to the hash table
 */
static void
hsh_insert(table, entry)
Genericrec *table[];
Genericrec *entry;
{
    register Genericrec **tabptr;

    tabptr = &(table[ATTRIB_HASH(entry->name)]);
    entry->next = *tabptr;
    *tabptr = entry;
}

/* remove an entry from the hash table and return it, dont free it */
static Genericrec *
hsh_remove(table, name)
Genericrec *table[];
long name;
{
    register Genericrec *tptr, **stptr;

    if(tptr = hsh_find(table,name)) {
	stptr = &table[ATTRIB_HASH(name)];
	*stptr = tptr->next;
	return(tptr);
    }
    else return (NULL);
}

