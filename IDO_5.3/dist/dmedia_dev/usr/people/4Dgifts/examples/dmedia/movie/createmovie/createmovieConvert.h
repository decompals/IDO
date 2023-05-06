/***************************************************************************
 *
 * File:	createmovieConvert.h
 *
 * Description: Public interface to craetemovieqtConvert.c++, a module
 *		which contains functions for converting between Apple's
 *		QuickTime image format and the SGI rgb image format.
 *
 *****************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

extern void Apple16ToRGBX( int width, int height, void* from, void* to);

extern void RGBXToApple16( int width, int height, void* from, void* to);

extern void Apple32ToRGBX( int width, int height, void* from, void* to);

extern void RGBXToApple32( int width, int height, void* from, void* to);

#ifdef __cplusplus
}
#endif
