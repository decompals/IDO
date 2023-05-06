/* geom.h
 * ------
 *
 * $Revision: 1.10 $
 *
 */

#define GLPROF_TAG(_name) {\
    cmov(-99999., -99999., -99999.); \
    glprof_object(_name); \
}


typedef struct atomtype {
	int id;
	float org_rad;
	float x, y, z, rad;		/* Do not change this order!!!	    */
	float r, g, b;
} atom_t;

typedef struct atomtabtype {
	int natoms;
	int atomcol;
	atom_t *atomlist;
} atomtab_t;


typedef struct bondtype {
	int id;
	float sx,sy,sz; 
	float ex,ey,ez; 
	float r, g, b;
} bond_t;

typedef struct bondtabtype {
	int nbonds;
	int bondcol;
	bond_t *bondlist;
} bondtab_t;

extern atomtab_t AtomTab[8];
extern bondtab_t BondTab[8];
extern int Natoms, Nbonds;
extern long Menu;

extern int Alreadyaccumulated;


extern float Left, Right, Bottom, Top;
extern float zNear, zFar;
extern long ZMin, ZMax;
extern long FogType;
extern int GeomLeftDown, GeomRightDown, GeomMiddleDown;

extern long GeomWid;
extern long GeomXorigin, GeomYorigin;
extern long GeomXsize, GeomYsize; 
extern int RotMode;


/****************************************************************************/

extern void DoReset(void);
extern void SetLineSmooth(void);
extern void ChangeRadius(void);
extern int ReadKernel(char *filename);
extern void InitData(void);
extern void OpenGeomWindow(void);
extern void SetGeomTitle(void);
extern void InitAccBuf(void);
extern void InitSphere(void);
extern void DefineLight(void);
extern void DisplayAccScene(void);
extern void DisplayScene(void);
extern void DoEventsGeom(long dev, short val);
extern void DoGeom(void);
extern void DoExitGeom(void);
extern void CalcInfo(void);
extern void SetBackface(int flag);
extern void DoMultisample(void);
extern void DoBitmapSpheres(void);
extern void DoStereo(void);
extern void DoDispAtoms(void);
extern void DoDispBonds(void);
extern void DoFastDraw(void);
extern void DoAccRollMode(void);
extern void DoSpinMode(void);
extern void DoHemi(void);
extern void DoOrient(void);
extern void DoProjection(void);
