#ifndef HISTDEF
#define HISTDEF

typedef struct histogram {
    int *bucket;
    int nbuckets;
    int min, max;
} histogram;

histogram *newhist();

#endif
