/*
 *   jistools.c:
 *
 *    jischarstr() displays a string encoded in the JIS encoding
 *    surrounded by the standard three-byte ESCAPE sequences for
 *    Kanji-shift-in and Kanji-shift out.
 *    
 *    If the string does not start with an ESCAPE character,
 *    it is assumed to be ascii.
 */


#include <gl/gl.h>
#include <string.h>


jischarstr( unsigned char *str )
{
	unsigned char c1, c2;
	unsigned int  len;

	if( ! str )	return;

	if( str[ 0 ] == '\033' )
	{
		len = strlen( str );

		/* 
		 * Put two consecutive bytes of NULL at the end of the string,
		 * but save the contents of those characters.
		 */
		c1 = str[ len - 2 ];
		str[ len - 2 ] = 0;

		c2 = str[ len - 3 ];
		str[ len - 3 ] = 0;

		/* Display the string */
		lcharstr( STR_2B, str + 3 );

		/* Restore the string to the way we found it. */
		str[ len - 2 ] = c1;
		str[ len - 3 ] = c2;
	}
	else
		charstr( str );
}

