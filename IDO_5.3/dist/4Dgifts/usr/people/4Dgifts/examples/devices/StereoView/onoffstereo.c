#include <gl/gl.h>
#include <gl/get.h>
#include <gl/device.h>

#include "stereo.h"

/*
 * Re-defined in on_stereo; in the future, should be initialized by a
 * call to getgdesc().
 */
int YMAXSTEREO = (-1);
int YOFFSET = (-1);

static int old_monitor_mode = (-1);

void
stereo_on()
{
	YMAXSTEREO = 491;
	YOFFSET = 532;

	/* If stereo is supported... */
	if (getgdesc(GD_STEREO))
	{
		/* save old monitor mode (normally HZ60) */
		old_monitor_mode = getmonitor();

		if (old_monitor_mode != STR_RECT)
		{
			setmonitor(STR_RECT);

     /* Constrain mouse to lower half of screen while in stereo mode */
			setvaluator(MOUSEY, YMAXSTEREO/2, 0, YMAXSTEREO);
		}
	}
}

void
stereo_off()
{
	int mousey;

	if (old_monitor_mode != (-1) && old_monitor_mode != STR_RECT)
	{
		setmonitor(old_monitor_mode);
		mousey = getvaluator(MOUSEY);
		setvaluator(MOUSEY, mousey, 0, getgdesc(GD_YPMAX));
	}
}
