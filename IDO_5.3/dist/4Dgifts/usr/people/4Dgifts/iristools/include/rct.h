#ifndef RCTDEF
#define RCTDEF

typedef struct rct {
    int xmin, xmax;
    int ymin, ymax;
} rct;

rct *rctnew();
rct *rctclone();

#endif
