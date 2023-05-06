/* help.c
 * ------
 *
 * $Revision: 1.6 $
 *
 */

#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <fmclient.h>
#include <stdlib.h>

#include "panel.h"

static long HelpYsize, HelpXsize;

extern fmfonthandle FontScreen, FontScreen5, FontScreen8, FontScreen10;
extern fmfontinfo FontScreenInfo, FontScreen5Info, FontScreen8Info, 
    FontScreen10Info;

long HelpWid = -1;

static char **Lines = 0;
static char Text[] = 
"This program uses the sphere primitives to render molecular models.\n\n\
The following describes the functionality of each mouse button.\n\n\
Middle            - Rotate the model objects about its center.\n\
Left              - Scale the model objects.\n\
Left+Middle+Right - Translate the model objects in the z plane.\n\
Left+Middle       - Zoom in or out.\n\n\
The following describes the functionality of each button in the control panel.\n\n\
Help              - Click the help button to exit this window.\n\
Model             - Scroll through the different available molecular models.\n\
Reset             - Reset the model objects to their original size, and put \n\
                    the point of view back in its original position.  If the \n\
                    objects are rotating, pressing the Reset button will stop\n\
\
		    them.\n\
Quit              - Quit the program.\n\
Sphere Depth      - Click the left mouse to decrease the tessellation depth\n\
                    level.  Click the middle mouse to increase the tessellation\n\
                    depth level.  The depth range is 1 through 30 inclusively.\n\
Sphere Prim       - Scroll through the different display primitive options.\n\
                    The options are: mesh, polygon, line, and point.\n\
Sphere Type       - Scroll through the different sphere tessellation types.  \n\
                    The options are: oct, icos, bary, cube, and bilin.\n\
Orient            - Toggle orient mode on/off.  When on, the sphere is\n\
                    displayed in a fixed orientation independent of world\n\
                    rotations.\n\
Hemi Sphere       - Toggle hemi sphere mode on/off.\n\
Draw Quick        - Toggle Quick drawing on/off.  In quick-draw mode no \n\
		    antialiasing or accumulation processes are done.\n\
Display Bonds     - Toggle the display of the bonds on/off.\n\
Display Atoms     - Toggle the display of the atoms on/off.\n\
Accumulate Atoms  - Toggle the accumulation of the atoms on/off.  In this\n\
                    mode, successive whole-scene antialiasing is done.  When\n\
                    the objects stop moving, the static image is accumulated\n\
                    in the accumulation buffer with slight perturbations.  The\n\
                    way the perturbations of the atoms are done is controlled\n\
                    by roll mode and spin mode.  The final image is\n\
                    antialiased.\n\
Smooth Bonds      - Scroll through the different line smooth levels.  The\n\
                    different levels are: off, on,  and smoother.\n\
End Correct Bonds - Toggle the End Correct mode on/off.  In end correct mode\n\
                    the end point of the bonds are antialiased.  This is\n\
                    visible only if the atoms are not displayed.\n\
Accumulate Bonds  - Toggle the accumulation of the bonds on/off.  This is an\n\
                    alternative way for displaying smooth lines.\n\
Radius            - Use the left mouse button to change the relative size of\n\
                    the atom radii while dragging the vertical bar.  Notice that\n\
                    the bonds are invisible with atoms size 1.0 \n\
Multisample       - This will toggle full scene multisampled antialiasing  \n\
                    on a suitable RealityEngine configuration. \n\
Stereo            - This will toggle in-the-window stereo on \n\
                    a suitable RealityEngine configuration with the proper \n\
                    monitor configuration (setmon 960x680_108s). \n\n\
The Following Keys have useful functionality: \n\n\
O/L               - Will increase/decrease, respectively, \n\
                    the separation for each eye when in stereo.";

static int NumLines = 0;

void InitHelpWindow();

void InitHelp();
void DrawHelp();
void DoEvents();
void DoExit();

/****************************************************************************/

void InitHelpWindow()
{
    foreground();
    prefsize(HelpXsize, HelpYsize);
    HelpWid = winopen("help");
    if (HelpWid == -1) {
	fprintf(stderr, "no additional graphics windows are available\n");
	DoExit(-1);
    }
    gconfig();
}

void CloseHelpWindow()
{
    winclose(HelpWid);
    HelpWid = -1;
}

void InitHelp()
{
    char *c;
    int line;
    int num_chars, count;
    int file;
    FILE *fptr;
    int length;
    static initialized = 0;

    if (initialized)
	return;
    for(num_chars = 0; Text[num_chars] != 0; num_chars++);

    for (c = Text; *c != 0; c++) {
	if (*c == '\n') {
	    NumLines++;
	}
    }
    NumLines++;

    Lines = (char **) malloc(NumLines * sizeof(char *));

    line = 0;
    Lines[line++] = Text;

    for (c = Text; *c != 0; c++) {
	if (*c == '\n') {
	    *c = 0;
	    Lines[line++] = c + 1;
	}
    }
    HelpYsize = NumLines * FontScreen10Info.height + 4;
    HelpXsize = 0;

    for (line = 0; line < NumLines; line++) {
	length = fmgetstrwidth(FontScreen10, Lines[line]);
	if (HelpXsize < length)
	    HelpXsize = length;
    }
    HelpXsize += 4;

    initialized = 1;
}

void DrawHelp()
{
    int line;
    long x, y, space;

    color(34);
    clear();
    color(WHITE);

    fmsetfont(FontScreen10);
    space = FontScreen10Info.height;
    x = 2;
    y = HelpYsize - space;

    for (line = 0; line < NumLines; line++) {
	cmov2i(x, y);
	fmprstr(Lines[line]);
	y -= space;
    }
}

void DoExitHelp(rc)
    int rc;
{
    if (Lines)
	free(Lines);
}

void DoEventsHelp(dev, val)
    long dev;
    short val;
{
    winset(HelpWid);

    switch(dev) {
      case REDRAW:
	reshapeviewport();
	DrawHelp();
	break;

      case ESCKEY:
	CloseHelpWindow();
	Items[ITEM_HELP].status = SET_PUSHED(0, Items[ITEM_HELP].status);
	break;

      default: break;
    }
}

