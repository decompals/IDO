#ifndef SGIOBJDEF
#define SGIOBJDEF

#define SOMAGIC		0x5424

/* types of objects */
#define OBJ_QUADLIST	1
#define OBJ_TRILIST	2
#define OBJ_TRIMESH	3
#define OBJ_END		4

/* offsets to data */
#define OFFSET_NORMAL	0
#define OFFSET_UVS	3
#define OFFSET_COLOR	3
#define OFFSET_POINT	6
#define PNTLONGS	9

/* how to draw an object */
#define DRAW_POINTS	0
#define DRAW_NORMALS	1
#define DRAW_UVS	2
#define DRAW_COLORS	4
#define DRAW_LINES	8

/* tmesh opcodes */

#define OP_BGNTMESH	1
#define OP_SWAPTMESH	2
#define OP_ENDBGNTMESH	3
#define OP_ENDTMESH	4

typedef struct sgiobj {
    struct sgiobj *next;
    long objtype;
    long nlongs;
    long *data;
    long xnlongs;
    long *xdata;
} sgiobj;

sgiobj *readsgiobj();
sgiobj *clonesgiobj();
sgiobj *newtriobj();
sgiobj *newquadobj();
sgiobj *catsgiobj();
sgiobj *tmeshobj();

#endif
