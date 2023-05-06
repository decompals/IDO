
/* 
 * Indexed access to a .pcf file.
 * Public interface, and documentation.
 */

extern void	hpcf_initialize( );
extern int	hpcf_openFont( char *, int, unsigned int, int );
extern int	hpcf_checkGlyph( int , int );
extern void	hpcf_closeFont( int );
extern int	hpcf_loadAll( char *, int, unsigned int, int );

/*
 * void	hpcf_initialize( );
 *
 *	Call before any other calls.  Call only once.  Not needed 
 *	for hpcf_loadAll(), however.
 *
 *
 * int hpcf_openFont( char *filename, int fontid, unsigned int delta, int s );
 *
 *	Open font file on the specified font id.  Add delta to
 *	any characters before registering them.  Delta is usually
 *	zero.  No bitmap data is actually read in to memory.
 *	"filename" should be the basename of the file with
 *	the .pcf extension.  If the file exists in compress'ed 
 *	format, it will be uncompressed automatically. 
 *
 *	"s" is a scaling factor for bitmaps.  Currently, only "2"
 *	is supported.
 *
 *	For better performance, store font files in uncompressed
 *	format (at the expense of some disk space).
 *
 *	Example:
 *	
 *		 hpcf_openFont( "jiskan.pcf",  100, 0, 0 );
 *
 *
 * int	hpcf_checkGlyph( int glyphnum , int fontnum );
 *
 *	Assert that the specified glyphnum is loaded into
 *	GL font "fontnum".  If the glyph has been loaded
 *	once, it will not be loaded again.
 *
 *
 * void	hpcf_closeFont( int fontid );
 *
 *	Closes this font and deletes the corresponding GL font.
 *
 *
 * int	hpcf_loadAll( char *filename, int fontid, unsigned int delta, int s );
 *
 *	Load all glyphs from "filename" and load at GL font "fontid".
 *	All bitmap data is read in.
 *
 *	s is a scaling factor.
 *
 *
 */
