/******************************************************************************
 *
 * File:        moviescreenGl.h
 *
 * Description: Functions controlling positioning and erasing (via GL drawing)
 *              of the movie window.
 * 
 ******************************************************************************/

extern void initPositionAndDirection( int zoom, int width, int height );
                                     
extern int  getXOffset( void );

extern int  getYOffset ( void );

extern void moveSaverPicture( void );

extern void undrawSaverPicture( void );
