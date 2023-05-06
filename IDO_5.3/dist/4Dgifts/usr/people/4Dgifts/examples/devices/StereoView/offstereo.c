/*
 *                                offstereo.c
 *
 *       Unconditionally turn off stereo mode, go back into 60 HZ mode.
 *
 *     The libstereo.a routine off_stereo() isn't used here because the
 *   routine on_stereo() stores away the previous monitor mode and
 *   restores it in off_stereo(); we want to just turn off stereo mode
 *   (don't care what mode the monitor was in when offstereo was called).
 */

#include <gl/gl.h>
#include <gl/get.h>
#include <gl/device.h>

main()
{
	int mousey;

    noport();
    winopen("offstereo");
    setmonitor(HZ60);

	/* Allow mouse to go anywhere on screen */
	mousey = getvaluator(MOUSEY);
	setvaluator(MOUSEY, mousey, 0, getgdesc(GD_YPMAX));
}
