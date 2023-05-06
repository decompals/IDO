typedef struct attribs {
	int r, g, b;
	float nx, ny, nz;
} attribs;

typedef struct point {
	struct point 	*next;
	int 		type;
	attribs		*attr;
	float x, y, z;
} point;

typedef struct object {
   	struct object 	*next;
	int 		type;
	attribs		*attr;
	point 		*points;
} object;

#define FASTMAGIC	0x5423

typedef struct fastobj {
    int npoints;
    int colors;
    int type;
    int material;
    int display;
    int *data;
} fastobj;

fastobj *readfastobj();
fastobj *clonefastobj();

