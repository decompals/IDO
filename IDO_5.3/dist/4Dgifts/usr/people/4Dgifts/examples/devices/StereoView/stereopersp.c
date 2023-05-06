/* stereopersp.c */

#include <malloc.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/get.h>
#include <gl/device.h>

#include "stereo.h"

void stereopersp(int fovy, float aspect,
	float near, float far, float conv, float eye)
{
    float left, right, top, bottom;
    float gltan;

    gltan = tan(fovy/2.0/10.0*M_PI/180.0);

    top = gltan * near;

    bottom = -top;

    gltan = tan(fovy*aspect/2.0/10.0*M_PI/180.0);

    left = -gltan*near - eye/conv*near;
    right = gltan*near - eye/conv*near;

    window(left, right, bottom, top, near, far);

    translate(-eye, 0.0, 0.0);
}

