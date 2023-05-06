/* 
 *   sgiflag.h
 *
 */

/* useful for lmdef, nurbssurface, nurbscurve, and more */
#define ELEMENTS(x)    (sizeof(x)/sizeof(x[0]))

/* Define nurbs surface properties */
#define S_NUMPOINTS    4
#define S_ORDER        4   /* cubic, degree 3 */
#define S_NUMKNOTS    (S_NUMPOINTS + S_ORDER)
#define S_NUMCOORDS    3

#define T_NUMPOINTS 4
#define T_ORDER        4 
#define T_NUMKNOTS    (T_NUMPOINTS + T_ORDER)
#define T_NUMCOORDS    3

typedef double Knot;
typedef double Point[3];
typedef double TrimPoint[2];

/* Trimming curves are either piecewise linear or nurbscurve */
enum TrimType {PWL, CURVE};


/* A trimming curve is made up of one or more trimming pieces.
 * A trimming piece may be of PWL or CURVE. If a trim piece is PWL,
 * it has at least two trim points, with each trim point composing
 * the endpoints of the line segments. If a trim piece is CURVE, it
 * has four trim points defining the cubic bezier trim.
 */

#define MAX_PIECES 20

struct TrimPieceStruct {
    enum TrimType type;             /* type of the trim              */
    int points;                     /* # of points in the trim piece */
    TrimPoint point[MAX_PIECES];    /* pointer to first trim point   */
};
typedef struct TrimPieceStruct TrimPiece;

struct TrimCurveStruct {
    int pieces;
    TrimPiece *trim;
};
typedef struct TrimCurveStruct TrimCurve;


struct teststruct {
    int a, b, c[2];
};
typedef struct teststruct Test;

/* function prototypes */
void interp(TrimPoint a, TrimPoint b, double d, TrimPoint result);
void join_trims(TrimPiece *trim1, TrimPiece *trim2, double radius);
void translate_trim(TrimPiece *trim, double tx, double ty);
void scale_trim(TrimPiece *trim, double sx, double sy);
void rotate_trim(TrimPiece *trim, double angle);
void copy_path(TrimCurve *src, TrimCurve **dst);
void init_trims(void);
void initialize(void);
void draw_nurb(Boolean);
void draw_hull(Point cpoints[S_NUMPOINTS][T_NUMPOINTS]);
void dotrim(TrimCurve *curve);

