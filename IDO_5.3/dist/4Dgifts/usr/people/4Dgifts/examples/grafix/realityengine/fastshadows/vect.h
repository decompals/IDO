#ifndef VECTDEF
#define VECTDEF

#include "math.h"

typedef struct vect {
    float x, y, z, w;
} vect;

typedef struct dvect {
    double x, y, z, w;
} dvect;

float vlength();
float vdot();
float flerp();
vect *vnew();
vect *vclone();

double dlerp();
double dvdot();

#endif
