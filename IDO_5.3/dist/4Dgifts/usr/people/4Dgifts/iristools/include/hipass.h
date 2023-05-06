#ifndef HIPASSDEF
#define HIPASSDEF

typedef struct highpass {
    int active, y;
    int xsize, ysize;
    int extrapval;
    short *blurrows[3];
    short *pastrows[2];
    short *acc;
    int (*getfunc)();
} highpass;

highpass *newhp();

#endif
