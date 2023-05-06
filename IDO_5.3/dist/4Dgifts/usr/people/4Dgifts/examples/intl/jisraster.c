/*
 *   jisraster.c:
 *
 *    Draws a rotating triangle with vertices labeled in Japanese.
 *    Press ESCAPE to toggle between English and Japanese labels.
 *
 *    Adapted from 4Dgifts/examples/glpg/ch03/rasterchars2.c.
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <stdio.h>
#include "hpcf.h"

float p[3][2] = {
	{ 0.0,   0.5},
	{-0.35, -0.35},
	{ 0.35, -0.35}
};

extern void jischarstr();

char *v1[] = { "Vertex 1", "頂点一番" };
char *v2[] = { "Vertex 2", "頂点二番" };
char *v3[] = { "Vertex 3", "頂点三番" };
char *lbl[] ={ "Press ESC for translation", "Press ESC for translation" };

main( )
{
	short val;
	int angle;
	int lang;

	prefposition( 50, 500, 200, 700 );
	foreground( );

	winopen( "jisraster" );
	doublebuffer( );
	gconfig( );

	qdevice( ESCKEY );
	color( BLACK );
	clear( );
	swapbuffers();

	/* Try to convert a Kanji font file to GL rasterfont. */
	if( hpcf_loadAll( "k14.pcf", 100, 0, 1 ) < 0 )
	{
		fprintf( stderr, "Could not find font file k14.pcf.Z.\n" );
		fprintf( stderr, 
			"You must install the eoe2.sw.Xasianfonts package.\n" );
		exit( 1 );
	}

	/* Add this ASCII font to the _same_ font id, 100 */
	if( hpcf_loadAll( "ScrB16.pcf", 100, 0, 1 ) < 0 )
	{
		fprintf( stderr, "Could not find font file ScrB16.pcf.Z.\n" );
		fprintf( stderr, 
			"You must install the eoe2.sw.Xfonts package.\n" );
		exit( 1 );
	}

	font( 100 );
	ortho2( -1.0, 1.0, -1.0, 1.0 );

	angle = 0;
	lang = 1;	/* Start with Japanese labels. */

	while( 1 )
	{
		color(BLACK);
		clear();

		/* draw the triangle */
		pushmatrix( );
		 rotate( angle = (angle + 1) % 3600, 'z' );
		 color(RED);
		 bgnpolygon();
			v2f(p[0]);
			v2f(p[1]);
			v2f(p[2]);
		 endpolygon();

		 /* add the labels */
		 color(GREEN);
		 cmov2(p[0][0], p[0][1]);
		 jischarstr( v1[ lang ] );
		 cmov2(p[1][0], p[1][1]);
		 jischarstr( v2[ lang ] );
		 cmov2(p[2][0], p[2][1]);
		 jischarstr( v3[ lang ] );
		popmatrix( );

		/* title at bottom */
		cmov2( -0.4, -0.75 );
		jischarstr( lbl[ lang ] );

		swapbuffers();
		/* press ESCAPE for translation */
		if( qtest( ) )
			if( qread( &val ) == ESCKEY && val )
				lang = ! lang;
	}
}


