/* $Id: svtest7.c,v 1.4 1995/03/31 01:42:13 edc Exp $ */

#define ENABLE_FILTERING

/*** Continuous transfer of frames from vino to galileo;
 *** Use discrete transfers, rgb images;
 ***/

#include <stdio.h>
#include <unistd.h>
#include <vl/vl.h>
#include <sv/sv.h>

void
setupPathCallback(VLServer svr, VLPath path, VLNode node1, VLNode node2)
{

#ifdef ENABLE_FILTERING
    printf("setup path callback invoked!\n");

    vlConversionAdviseFlag(svr, path, VL_PACKING_RGB_8,  VL_PACKING_YVYU_422_8, 1);
    vlConversionAdviseFlag(svr, path, VL_PACKING_RGBA_8, VL_PACKING_YVYU_422_8, 1);
#endif
}

int
main(void)
{
    int ret;
    svImage *imageInfo;
    svContext context1, context2;

    context1 = svCurrentContext();
    svSelectInput(vnINPUT_INDYCAM);
    svSetFrameCount(2);

    context2 = svNewContext();
    svSelectOutput(gvOUTPUT_ANALOG);
    svSetFrameCount(2);

    svSetupPathCallback(setupPathCallback);

    imageInfo = svNewImage();

    while (1) {
	svSetContext(context1);
	if (ret = svGetFrame(imageInfo))
	    printf("svGetFrame returns %d\n", ret);

	svSetContext(context2);
	if (ret = svPutFrame(imageInfo))
	    printf("svPutFrame returns %d\n", ret);
    }

    return 0;
}
