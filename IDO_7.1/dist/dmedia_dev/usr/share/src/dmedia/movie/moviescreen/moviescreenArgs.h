/****************************************************************************
 *
 * File:        moviescreenArgs.h
 *
 * Description: Functions for processing the command line arguments, 
 *              and accessing variables set therefrom, including movie names.
 * 
 ****************************************************************************/

#include <dmedia/moviefile.h>
#include <dmedia/movieplay.h>

#ifndef __EXTENSIONS__        /* To pick up declarations for random() and */
#define __EXTENSIONS__        /* srandom() from math.h. */
#endif
#include <math.h>

extern void 	 processCmdArgs( int argc, char **argv );

extern void 	 usage( void );

extern char 	 *pickMovieAtRandom( void );

extern void 	 setMovieID( MVid newMovieID );
extern MVid 	 getMovieID( void );

extern int 	 getMovieCount( void );

extern DMboolean isFullScreen( void );

extern DMboolean playSound( void );

extern int 	 getVolume( void );

extern DMboolean isVolumeSet( void );

extern int 	 getZoom( void );

extern int 	 getLoopmode( void );

extern char	 *getProgramName( void );
