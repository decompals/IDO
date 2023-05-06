#ifndef SETSCREENDEF
#define SETSCREENDEF

typedef struct halftone {
    int totsize, dx, dy, w2;
    int octant, base, mint, maxt;
    unsigned char *bm;
    int *nexti;
    float angle;
} halftone;

halftone *setscreen();

#endif
