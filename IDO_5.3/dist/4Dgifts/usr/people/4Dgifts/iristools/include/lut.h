#ifndef LUTDEF
#define LUTDEF

typedef struct lut {
    int insteps;
    int outsteps;
    int stoclut;
    unsigned short *stab;
    float *flow;
    float *fhigh;
} lut;

lut *makelut();

#endif
