/* geom.c
 * ------
 *
 * $Revision: 1.24 $
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>
#include <fmclient.h>
#include <gl/sphere.h>

#include "mview.h"
#include "geom.h"
#include "light.h"

#define ABS(x) (((x) > 0) ? (x) : (0 - (x)))


#define DIST 80.0
#define ANGLE_SHOCK_ABSORBER 0.4
#define COORD_SHOCK_ABSORBER 0.04
#define AUTO_ROT_TOL 5

/*
 * stereo defines - for units in inches
 */

#define PIXELS	50.0		/* pixels per inch in stereo mode */
#define SCREEN_WIDTH  15.       /* width of screen in inches*/
#define SCREEN_HEIGHT 10.625    /* height of screen in inches*/
#define DISTANCE_TO_SCREEN 30.  /* distance from eye to screen (nearclip)*/

#define CLIP_FACTOR (1./8.00) /* to move near clip in */

#define NEAR_CLIP_DISTANCE	(DISTANCE_TO_SCREEN*CLIP_FACTOR)
#define FAR_CLIP_DISTANCE	(DISTANCE_TO_SCREEN*500.)  

#define DESIRED_SIZE_ON_SCREEN	2.
#define WORLD_SPACE_SIZE	2.

#define IOD 2.3			/* eye offset distance */

static float viewOffset = IOD;
static float projOffset = 0.;

/****************************************************************************/

static float centerx=0.0, centery=0.0, centerz=0.0;
static int AtomsInitialized = 0;
static int BondsInitialized = 0;
static int ModelRead = 0;
static atom_t *Atoms = 0;
static bond_t *Bonds = 0;

static int LightModel;
static unsigned AAmode = SML_OFF;

long FogType = FG_PIX_LIN;
static float fogprops[5] = {1.0,40.0,0.0,0.0,0.0};

/* 
 * atom types
 */
#define AT_CARBON	1
#define AT_HYDROGEN	2
#define AT_OXYGEN	3
#define AT_NITROGEN	4
#define AT_SULFER	5
#define AT_Z		6
#define AT_U		7
#define AT_OTHER	8

/* ambient/diffuse color data */
float atom_colors[][4] = {
/* AT_CARBON - grey */   {0.15, 0.15, 0.15, 1.0}, 
/* AT_HYDROGEN - white */  {0.55, 0.55, 0.55, 1.0}, 
/* AT_OXYGEN - red */    {0.5, 0.0, 0.0, 1.0}, 
/* AT_NITROGEN - blue */   {0.12, 0.12, 0.35, 1.0},
/* AT_SULFER - yellow */ {0.5, 0.5, 0.0, 1.0},
/* AT_Z - green */ {0.0, 0.5, 0.0, 1.0},
/* AT_U  - cyan */ {0.0, 0.5, 0.5, 1.0},
/* AT_OTHER - pink */   {120.0/255.0, 0.0, 50.0/255.0, 1.0} ,
};

float *atom_props[] = 
{
/* AT_CARBON - grey */  MatGreyprops, 
/* AT_HYDROGEN - white */  MatWhiteprops, 
/* AT_OXYGEN - red */    MatRedprops, 
/* AT_NITROGEN - blue */   MatBlueprops,
/* AT_SULFER - yellow */ MatYellowprops,
/* AT_Z - green */   MatGreenprops, 
/* AT_U - cyan */   MatCyanprops, 
/* AT_OTHER - pink */   MatPinkprops, 
};

float FOVY = 400.;
float Left = -24.0, Right = 24.0, Bottom = -20.0, Top = 20.0;
float CenterZ = -DISTANCE_TO_SCREEN;
float Near = NEAR_CLIP_DISTANCE, Far = FAR_CLIP_DISTANCE;
long ZMin, ZMax;

static KERNEL Kernel = {
    18,
    {{0.439362, 0.198547, 0.},
    {0.209265, 0.518289, 0.},
    {0.810847, 0.489433, 0.},
    {0.818721, 0.089856, 0.},
    {0.170028, 0.911008, 0.},
    {0.551777, 0.792868, 0.},
    {0.339362, 0.198547, 0.},
    {0.509265, 0.518289, 0.},
    {0.710847, 0.489433, 0.},
    {0.918721, 0.089856, 0.},
    {0.070028, 0.911008, 0.},
    {0.251777, 0.592868, 0.},
    {0.439362, 0.198547, 0.},
    {0.609265, 0.318289, 0.},
    {0.710847, 0.589433, 0.},
    {0.918721, 0.189856, 0.},
    {0.170028, 0.911008, 0.},
    {0.351777, 0.792868, 0.}}
};

#define SPH_KERNAL_SIZE 18
static float Zkernel[SPH_KERNAL_SIZE] = {
    9., 26., 30., 44., 51., 66., 
    11., 15., 33., 66., 91., 101., 
    3., 13., 18., 37., 86., 95.
};

static float Xkernel[SPH_KERNAL_SIZE] = {
    -3., 3., -1., 1., -5., 5., 
    -2., 2., -4., 4., -1.5, 1.5, 
    -3.5, 3.5, -2.5, 2.5, -4.5, 4.5
};
		    
static Matrix 
    IdentityMat = {
	1.0, 0.0, 0.0, 0.0,
	0.0, 1.0, 0.0, 0.0,
	0.0, 0.0, 1.0, 0.0,
	0.0, 0.0, 0.0, 1.0
    }, 
    Mat = {
	1.0, 0.0, 0.0, 0.0, 
	0.0, 1.0, 0.0, 0.0, 
	0.0, 0.0, 1.0, 0.0, 
	0.0, 0.0, 0.0, 1.0, 
    };

static float Mx, My;
static long Xpos, Ypos;
static long NewXpos, NewYpos;
static float TransX = 0.0f, TransY = 0.0f, TransZ = 0.0f;
static float ScaleFactor = 1.0f;

static int NumPolysAtom;
static int NumPolys;

long Menu = -1;
int
    GeomLeftDown = 0, 
    GeomRightDown = 0, 
    GeomMiddleDown = 0;
int RotMode = 0;

long GeomWid = -1;
long GeomXorigin = 0, GeomYorigin = 0;
long GeomXsize = 1280, GeomYsize = 1024; 
float GeomAspect = 1280./1024.;
float GeomXInches;

atomtab_t AtomTab[8];
bondtab_t BondTab[8];
int Natoms = 0, Nbonds = 0;

int Alreadyaccumulated = 0;

extern fmfonthandle FontScreen, FontScreen8;
extern fmfontinfo FontScreenInfo, FontScreen8Info;

/****************************************************************************/

static void PrintMat(Matrix mat);
static void ConfigureWindow(void);
static float** SphRotMatrix(float rx, float ry, float rz);
static void InitAtoms(void);
static void InitBonds(void);
static void SetupAtomTab(int natoms, atom_t *atoms);
static void SetupBondTab(int nbonds, bond_t *bonds);
static void ClearScreen(void);
static void ResetLight(void);
static void DrawAtoms(void);
static void DrawBonds(void);
static void TranslateXY(void);
static void Scale(void);
static void RotateXY(int flag);
static void Zoom(void);
static void SetMatter(int m);
static void AccWindow(float pixdx, float pixdy, float eyedx, float eyedy, 
			float focus);
static void ResetMat(Matrix m);
static void TessDown(void);
static void TessUp(void);
static void DrawInfo(void);
static void DrawScene(void);
static void stereopersp(int fovy, float aspect,
        float near, float far, float conv, float eye);

/****************************************************************************/

void InitData()
{
    bzero((char *)AtomTab, sizeof(AtomTab));
    bzero((char *)BondTab, sizeof(BondTab));
    AtomsInitialized = 0;
    if (Atoms) free(Atoms);
    InitAtoms();
    BondsInitialized = 0;
    if (Bonds) free(Bonds);
    InitBonds();
    ModelRead = 1;
}

static void InitAtoms()
{
    if (!AtomsInitialized) {
	Natoms = readatoms(Models[ModelId].atom, &Atoms, 
	    &centerx, &centery, &centerz);
	SetupAtomTab(Natoms, Atoms);
	AtomsInitialized = 1;
    }
}

static void InitBonds()
{
    if (!BondsInitialized) {
	Nbonds = readbonds(Models[ModelId].bond, &Bonds, 
	    &centerx, &centery, &centerz);
	SetupBondTab(Nbonds, Bonds);
	BondsInitialized = 1;
    }
}

static void SetupAtomTab(int natoms, atom_t *atoms)
{
    int i;
    atom_t *tptr[8];
    atom_t *aptr;

    /* malloc AtomTab */
    for (i = 0; i < 8; i++) {
	if (AtomTab[i].natoms > 0) {
           AtomTab[i].atomlist = (atom_t *)
				 malloc (AtomTab[i].natoms * sizeof(atom_t));
	   AtomTab[i].atomcol = i;
	}
    }
    for (i=0; i < 8; i++) {
	tptr[i] =  AtomTab[i].atomlist;
    }
    /* put atoms in  AtomTab */
    aptr = atoms;
    for (i = 0; i < natoms; i++) {
	tptr[aptr->id]->org_rad  = aptr->org_rad;
	tptr[aptr->id]->rad  = aptr->rad;
        tptr[aptr->id]->id  = aptr->id;
        tptr[aptr->id]->x  = aptr->x;
        tptr[aptr->id]->y  = aptr->y;
        tptr[aptr->id]->z  = aptr->z;
        tptr[aptr->id]->r  = aptr->r;
        tptr[aptr->id]->g  = aptr->g;
        tptr[aptr->id]->b  = aptr->b;
	tptr[aptr->id] += 1;
	aptr++;
    }
}

static void SetupBondTab(int nbonds, bond_t *bonds)
{
    int i;
    bond_t *tptr[8];
    bond_t *bptr;

    /* malloc BondTab */
    for (i = 0; i < 8; i++) {
	if (BondTab[i].nbonds > 0) {
           BondTab[i].bondlist = (bond_t *)
				 malloc (BondTab[i].nbonds * sizeof(bond_t));
	}
    }
    for (i=0; i < 8; i++) {
	tptr[i] =  BondTab[i].bondlist;
    }
    /* put bonds in  BondTab */
    bptr = bonds;
    for (i = 0; i < nbonds; i++) {
	tptr[bptr->id]->id  = bptr->id;
        tptr[bptr->id]->sx  = bptr->sx;
        tptr[bptr->id]->sy  = bptr->sy;
        tptr[bptr->id]->sz  = bptr->sz;
        tptr[bptr->id]->ex  = bptr->ex;
        tptr[bptr->id]->ey  = bptr->ey;
        tptr[bptr->id]->ez  = bptr->ez;
        tptr[bptr->id]->r  = bptr->r;
        tptr[bptr->id]->g  = bptr->g;
        tptr[bptr->id]->b  = bptr->b;
	tptr[bptr->id] += 1;
	bptr++;
    }
}

static void ConfigureWindow(void)
{
    getorigin(&GeomXorigin, &GeomYorigin);
    getsize(&GeomXsize, &GeomYsize);
    GeomAspect = ((float)GeomXsize / (float)GeomYsize);
    Top = Right / GeomAspect;
    Bottom = -Top;
    GeomXInches = ((float)GeomXsize/PIXELS);
}

void OpenGeomWindow()
{
    if (UsePrefsize) prefsize(GeomXsize, GeomYsize);
    else if (UsePrefposition)
	prefposition(GeomXorigin, GeomXorigin + GeomXsize - 1, 
	    GeomYorigin, GeomYorigin + GeomYsize - 1);
    else keepaspect(5,4);
    if (Debug)
	foreground();
    GeomWid = winopen(ProgName);
    SetGeomTitle();
    if (GeomWid == -1) {
	fprintf(stderr, "%s: no additional graphics windows are available\n", 
	    ProgName);
	DoExit(-1);
    }

    RGBmode();
    	doublebuffer();
    DoMultisample();
    gconfig();
    ConfigureWindow();
    zbuffer(1);
    ClearScreen();
    swapbuffers();
    clear();
    subpixel(1);
    lsetdepth(0, ZMax);

    fogvertex(FogType, fogprops);
}

static void ClearScreen()
{
    /*cpack(BackgroundColor);*/
    /*clear(); zclear();*/
    czclear(BackgroundColor, ZMax);
}

void InitSphere()
{
    sphmode(SPH_DEPTH, SphereDepth);
    sphmode(SPH_TESS, SphereType);
    sphmode(SPH_PRIM, SpherePrim);
    if (!RollMode)
    {
	if (Hemi)
	    sphmode(SPH_HEMI, Hemi);
	else
	    SetBackface(1);
    }
    sphmode(SPH_ORIENT, Orient);
}


void InitAccBuf(void)
{
    static int initialized = 0;

    winset(GeomWid);
    if (!initialized) {
	if (!HwAccBuf()) {
	    AtomAccBuf = 0;
	    BondAccBuf = 0;
	    printf("%s: Accumulation Buffer turned off - no hw support\n", 
		ProgName);
	    return;
	}
	initialized = 1;
    }
    if (AtomAccBuf || BondAccBuf)
    {
	acsize(hwAccbuf);
    } else {
	acsize(0);
    } 
    gconfig();
    Alreadyaccumulated = 0;
}

void DoReset()
{
    DoProjection();
    ResetMat(Mat);
    loadmatrix(IdentityMat);
    translate(0.0, 0.0, CenterZ);
    scale(ZoomFactor, ZoomFactor, ZoomFactor);
    RotMode = 0;
    ResetLight();
}

static void ResetLight()
{
    LightModel = MODEL_INFINITE;
    SetMaterial(MAT_WHITEPLASTIC);
    SetLight(LIGHT_DEFAULT);
    SetLightModel(MODEL_INFINITE);
}

static void DrawBonds()
{
    int bond;
    int bondtype;
    bond_t *bptr;

#if 1
    SetLightModel(0);
    if (AAmode != SML_OFF) {
	blendfunction(BF_SA, BF_ONE);
	if (!DispAtoms) {
	    zfunction (ZF_ALWAYS);
	    /*zbuffer(0);*/
	}
    }
#endif

    for (bondtype = 0; bondtype < 8; bondtype++) {
	if (BondTab[bondtype].nbonds > 0) {
	    bptr = BondTab[bondtype].bondlist;
	    c3f(&(bptr->r));
	    for (bond = 0; bond < BondTab[bondtype].nbonds; bond++) { 
		bgnline();
		    v3f(&bptr->sx);
		    v3f(&bptr->ex);
		endline();
		bptr++;
	    }
	}
    }

#if 1
    if (AAmode != SML_OFF) {
	blendfunction(BF_ONE, BF_ZERO);
	if (!DispAtoms) {
	    zfunction (ZF_LEQUAL);
	    /*zbuffer(1);*/
	}
    }
    SetLightModel(LightModel);
#endif
}

static void DrawAtoms()
{
    int atomtype;
    atom_t *aptr;
    int atom;

    if ((SpherePrim == SPH_LINE) && (BondSmooth != SML_OFF))
	blendfunction(BF_SA,BF_ONE);

    if (BitmapSpheres)
    {
	shademodel(FLAT);
	sphbgnbitmap();
	afunction(128,AF_GREATER);

    }
    for (atomtype = 0; atomtype < 8; atomtype++) {
	if (AtomTab[atomtype].natoms > 0) {
	    SetMatter(atomtype);
	    aptr = AtomTab[atomtype].atomlist;
	    for (atom = 0; atom < AtomTab[atomtype].natoms; atom++) { 
		if ((aptr->id < 1) || (aptr->id > 7)) {
		    printf("drawatom: uknown atom id=%d\n",aptr->id);
		}
		sphdraw(&(aptr->x));
		aptr++;
	    }
	}
    }

    if (BitmapSpheres)
    {
	sphendbitmap();
	shademodel(GOURAUD);
	afunction(0,AF_ALWAYS);
    }

    if ((SpherePrim == SPH_LINE) && (BondSmooth != SML_OFF))
	blendfunction(BF_ONE, BF_ZERO);
}

void DisplayAccScene()
{
    float total_weight;
    int i;

    if (!ModelRead) return;
    winset(GeomWid);

    acbuf(AC_CLEAR, 0.);
    total_weight = 0.;
    for (i = 0; i < Kernel.numsamples; i++) {
	pushmatrix();
	AccWindow(Kernel.samples[i].x, Kernel.samples[i].y, 0.0, 0.0, 1.0);
	if (RollMode && SpinMode)
	    sphrotmatrix(SphRotMatrix(Xkernel[i], Xkernel[i], Zkernel[i]));
	else if (RollMode)
	    sphrotmatrix(SphRotMatrix(Xkernel[i], Xkernel[i], 0.0));
	else if (SpinMode)
	    sphrotmatrix(SphRotMatrix(0.0, 0.0, Zkernel[i]));
	DrawScene();
	acbuf(AC_ACCUMULATE, 1.);
	popmatrix();

	if (qtest()) {
	    acbuf(AC_RETURN, 1. / (i + 1));
	    DrawInfo();
	    swapbuffers();
	    ClearScreen();
	    Alreadyaccumulated = 1;
	    return;
	} else if (((i + 1) % 3) == 0) {
	    acbuf(AC_RETURN, 1. / (i+1));
	    DrawInfo();
	    swapbuffers();
	}
	ClearScreen();
    }
    Alreadyaccumulated = 1;
}

void DoEventsGeom(long dev, short val)
{
    Matrix tmat;
    winset(GeomWid);

    switch(dev) {

      case RIGHTMOUSE:
	if (val) {
	    if (!GeomLeftDown && !GeomMiddleDown) {
		BuildMainMenu();
		dopup(Menu);
	    } else
		GeomRightDown = 1;
	} else {
	    GeomRightDown = 0;
	    Xpos = getvaluator(MOUSEX) - GeomXorigin;
	    Ypos = getvaluator(MOUSEY) - GeomYorigin;
	}
	break;

      case LEFTMOUSE:
	if (val) {
	    Xpos = getvaluator(MOUSEX) - GeomXorigin;
	    Ypos = getvaluator(MOUSEY) - GeomYorigin;
	    GeomLeftDown = 1;
	} else {
	    GeomLeftDown = 0;
	}
	break;

      case MIDDLEMOUSE:
	if (val) {
	    Xpos = getvaluator(MOUSEX) - GeomXorigin;
	    Ypos = getvaluator(MOUSEY) - GeomYorigin;
	    GeomMiddleDown = 1;
	} else
	    GeomMiddleDown = 0;
	break;
      case KEYBD:
	switch(val)
	{
	  case 'b':
		BitmapSpheres ^= 1;
		fprintf(stderr,"BitmapSpheres = %d\n", BitmapSpheres);
		DoBitmapSpheres();
		DisplayScene();
		break;
	  case 'o':
		viewOffset += 0.1;
		DisplayScene();
		break;
	  case 'l':
		viewOffset -= 0.1;
		DisplayScene();
		break;
#if 0
	  case 'm':
		mmode(MPROJECTION);
		getmatrix(tmat);
		fprintf(stderr,"Projection \n");
		PrintMat(tmat);
		mmode(MVIEWING);
		getmatrix(tmat);
		fprintf(stderr,"Viewing \n");
		PrintMat(tmat);
#endif
	}
      case REDRAW:
	{
	reshapeviewport();
	ConfigureWindow();
	GeomLeftDown = GeomRightDown = GeomMiddleDown = 0;
	ClearScreen();
	swapbuffers();
	ClearScreen();

	if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
	!Alreadyaccumulated)
	    DisplayAccScene();
	else
	    DisplayScene();
	}
	break;

      case UPARROWKEY:
	if (!val) break;
	TessUp();
	DisplayScene();
        Alreadyaccumulated = 1;
	break;

      case DOWNARROWKEY:
	if (!val) break;
	TessDown();
	DisplayScene();
	Alreadyaccumulated = 1;
	break;

      default: break;
    }
}

void DoGeom(void)
{
    int moved = 0;

    winset(GeomWid);

    NewXpos = getvaluator(MOUSEX) - GeomXorigin;
    NewYpos = getvaluator(MOUSEY) - GeomYorigin;

    if (GeomLeftDown && GeomMiddleDown && GeomRightDown) {
	moved = 1;
	TranslateXY();
    } else if (GeomLeftDown && GeomMiddleDown && !GeomRightDown) {
	moved = 1;
	Zoom();
    } else if (GeomLeftDown && !GeomMiddleDown && !GeomRightDown) {
	moved = 1;
        if (ABS(NewXpos - Xpos) > AUTO_ROT_TOL)
	    Scale();
    } else if (!GeomLeftDown && GeomMiddleDown && !GeomRightDown) {
	moved = 1;
	RotateXY(1);
    } else if (RotMode) {
	moved = 1;
	RotateXY(0);
    }

    if (moved) {
	DisplayScene();
	moved = 0;
    } else if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated) {
	DisplayAccScene();
    }

    Xpos = NewXpos;
    Ypos = NewYpos;

}

/*
 * SphRotMatrix()
 *    calculates the sphere rotation matrix to send to sphrotmatrix()
 *    to use in SPH_ORIENT mode.
 *    this routine always calcs the rotations as rz*ry*rx = M.
 */

static float** SphRotMatrix(float rx, float ry, float rz)
{
    static Matrix mat;

    pushmatrix();
    loadmatrix(IdentityMat);

    if (rx)    
	rot(rx, 'x');
    if (ry)
	rot(ry, 'y');
    if (rz)
	rot(rz, 'z');
    getmatrix(mat);
    popmatrix();
    return((float **)mat);
}

static void RotateXY(int flag)
{
    float rz, rx;

    pushmatrix();
    loadmatrix(IdentityMat);

    if (flag) {
	if ((NewYpos != Ypos) || (NewXpos != Xpos))
	    RotMode = 1;
	else
	    RotMode = 0;

	My = (float) (NewYpos - Ypos);
	Mx = (float) (NewXpos - Xpos);
    }

    rx = 0.0 - My * ANGLE_SHOCK_ABSORBER;
    rz = Mx * ANGLE_SHOCK_ABSORBER;

    rot(rx, 'x');
    rot(rz, 'y');

    multmatrix(Mat);
    getmatrix(Mat);

    popmatrix();

    Xpos = NewXpos;
    Ypos = NewYpos;
}

static void Zoom()
{
    float mx;

    mx = (float) (NewXpos - Xpos);
    TransZ += - mx * COORD_SHOCK_ABSORBER;
    Xpos = NewXpos;
}

static void TranslateXY()
{
    float mx, my;

    mx = (float) (NewXpos - Xpos);
    my = (float) (NewYpos - Ypos);
    TransX += mx * COORD_SHOCK_ABSORBER; 
    TransY += my * COORD_SHOCK_ABSORBER;
    Xpos = NewXpos;
    Ypos = NewYpos;
}

static void Scale()
{
    float mx;
    float factor;

    mx = (float) (NewXpos - Xpos);
    ScaleFactor *=  (1.0 + mx * ZoomFactor / (float) GeomXsize);
}

void DoExitGeom()
{
    if (Menu != -1)
	freepup(Menu);
}

static void SetMatter(int m)
{
    register int i = m - 1;

    if (BitmapSpheres)
	sphcolor(atom_colors[i]);
    else 
	ChangeMaterial(atom_props[i]);
}

static void TessDown()
{
    if (SphereDepth > 1) SphereDepth--;
    printf("depth down to %d \n", SphereDepth);
    sphmode(SPH_DEPTH, SphereDepth);
    CalcInfo();
}

static void TessUp()
{
    if (SphereDepth < SPH_MAXDEPTH) SphereDepth++;
    printf("depth up to %d \n", SphereDepth);
    sphmode(SPH_DEPTH, SphereDepth);
    CalcInfo();
}
static void ResetMat(Matrix m)
{
    m[0][0] = m[1][1] = m[2][2] = m[3][3] = 1.0;
    m[0][1] = m[0][2] = m[0][3] = 0.0;
    m[1][0] = m[1][2] = m[1][3] = 0.0;
    m[2][0] = m[2][1] = m[2][3] = 0.0;
    m[3][0] = m[3][1] = m[3][2] = 0.0;
}

static void PrintMat(Matrix mat)
{
    int i;

    printf("matrix:\n");
    for (i = 0; i < 4; i++) 
	printf("\t%5.5f %5.5f %5.5f %5.5f\n",
		mat[i][0], mat[i][1], mat[i][2], mat[i][3]);
    printf("\n");
}

void DoLineSmooth()
{
    winset(GeomWid);
    if (FastDraw) {
	BondSmooth = SML_OFF;
	BondEndCorrect = SML_OFF;
    }

    AAmode = BondSmooth | BondEndCorrect;
    linesmooth(AAmode);
}

void DoBitmapSpheres(void)
{
    if (!hwBitmapSpheres)
	return;
    winset(GeomWid);
    if (BitmapSpheres)
    {
	if (Multisample)
	{
	    Multisample = 0;
	    DoMultisample();
	}
	RGBsize(12);
	if (AtomAccBuf)
	{
	    AtomAccBuf = 0;
	}
	if (BondAccBuf)
	{
	    BondAccBuf = 0;
	}
	acsize(0);
	gconfig();
    }
    CalcInfo();
    DrawPanel();
}

void DoMultisample(void)
{
    if (!hwMultisample)
	return;
    winset(GeomWid);
    if (Multisample) 
    {
	stensize(0);
	acsize(0);
	zbsize(0);
	mssize(8, 24, 1);
	multisample(1);
	gconfig();
    } else
    {
	mssize(0, 0, 0);
	multisample(0);
	zbsize(32);
	gconfig();
    }
}

void DoStereo(void)
{
    long bits;
    if (!hwStereo)
	return;
    winset(GeomWid);

    if (Stereo) 
    {
	/* set to 8 bits color so can allocate stereo buffers 
	 * when have small pixel depth
	 * Note: this is incompatible with hw accumulation buffer !!!
	 * Note: bitmap spheres require RGBsize(12) !!!
	 */
	if (!BitmapSpheres)
		RGBsize(8);
	acsize(0);
	stereobuffer();
	doublebuffer();
	gconfig();
    } else
    {
	RGBsize(12);
	monobuffer();
	doublebuffer();
	gconfig();
	DoProjection();
    }
}

void DoProjection()
{
    winset(GeomWid);
    mmode(MPROJECTION);
    if (!Perspective)
    {
	if (Stereo)
	{
	    float offset = projOffset * .1;
	    ortho(Left + offset, Right + offset, Bottom, Top, Near, Far);    
	}
	else
	    ortho(Left, Right, Bottom, Top, Near, Far);    
    } else 
    {
	if (Stereo)
	    stereopersp(FOVY, GeomAspect, Near, Far, 
			DISTANCE_TO_SCREEN, projOffset);
	else
	    perspective(FOVY, GeomAspect, Near, Far);
    }
    mmode(MVIEWING);
}

static void stereopersp(int fovy, float aspect,
        float near, float far, float conv, float eye)
{
    float left, right, top, bottom;
    float gltan;
    float offset=0.;

    gltan = ftan(fovy/2.0/10.0*M_PI/180.0);

    top = gltan * near;
    bottom = -top;

    gltan = ftan(fovy*aspect/2.0/10.0*M_PI/180.0);
    left = -gltan*near + eye/conv*near;
    right = gltan*near + eye/conv*near;

    window(left, right, bottom, top, near, far);

    translate(eye, 0.0, 0.0);

}

/*
 * Shift around window for accumulation buffer sampling
 */
static void AccWindow(float pixdx, float pixdy, float eyedx, float eyedy, 
			float focus)
{
    float hsize, vsize; 
    float dx, dy;

    if(focus<0.0)
	focus = -focus;

    if (!Perspective)
    {
	hsize = Right-Left;
	vsize = Top-Bottom;
	dx = -(pixdx*hsize/GeomXsize+eyedx*Near/focus);
	dy = -(pixdy*vsize/GeomYsize+eyedy*Near/focus);

	ortho(Left+dx,Right+dx,Bottom+dy,Top+dy,Near,Far);
    }
    else
    {
	float left, right, top, bottom;
	float gltan;

	gltan = ftan(FOVY/2.0/10.0*M_PI/180.0);

	top = gltan * Near; 
	bottom = -top;
	gltan = ftan(FOVY*GeomAspect/2.0/10.0*M_PI/180.0);
	left = -gltan*Near;
	right = gltan*Near;

	hsize = right-left;
	vsize = top-bottom;

	dx = -(pixdx*hsize/GeomXsize+eyedx*Near/focus);
	dy = -(pixdy*vsize/GeomYsize+eyedy*Near/focus);
	window(left+dx,right+dx,bottom+dy,top+dy,Near,Far);
    }
    translate(-eyedx,-eyedy,0.0);
}


void DoDispAtoms(void)
{
    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
		!Alreadyaccumulated)
	DisplayAccScene();
    else 
	DisplayScene();
}


void DoDispBonds(void)
{
    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else 
	DisplayScene();
}

void DoFastDraw(void)
{
    BondSmooth = SML_OFF;
    BondEndCorrect = SML_OFF;
    DoLineSmooth();
    BondAccBuf = 0;
    AtomAccBuf = 0;

    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else 
	DisplayScene();
}


void DoSpinMode(void)
{
    if (SpinMode && !Orient)
    {
	Orient = 1;
	DoOrient();
    }
    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else 
	DisplayScene();
}

void DoAccRollMode(void)
{
    if (RollMode && !Orient)
    {
	Orient = 1;
	DoOrient();
    }

    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else DisplayScene();
}

void DoHemi(void)
{
    if (!Orient || (Orient && !Hemi)) SetBackface(1);
    sphmode(SPH_HEMI, Hemi);
    CalcInfo();
    DisplayScene();
}

void DoOrient(void)
{
    sphmode(SPH_ORIENT, Orient);
    CalcInfo();
    if (!Orient || (Orient && !Hemi)) SetBackface(1);
    else SetBackface(0);
    DisplayScene();
}

void DefineLight(void)
{
    DefineMaterials();
    DefineLights();
    DefineLightModels();
}

void CalcInfo(void)
{
    NumPolysAtom = sphgnpolys();
    NumPolys = NumPolysAtom * Natoms;
}

static void DrawInfo(void)
{
    char str[100];

#ifdef USEOVERLAY
    drawmode(OVERDRAW);
    color(WHITE);
#else
    zfunction(ZF_ALWAYS);
    cpack(0xffffff);
#endif /* USEOVERLAY */

    SetLightModel(0);
    ortho2(-0.5, GeomXsize - 0.5, -0.5, GeomYsize - 0.5);
    pushmatrix();
    loadmatrix(IdentityMat);

    if (DispAtoms) {

	cmov2i(20, 20);
	sprintf(str, "Atoms: %d", Natoms);
	charstr(str);

	if (!BitmapSpheres)
	{
	    cmov2i(20, 35);
	    sprintf(str, "Polygons per Atom: %d", NumPolysAtom);
	    charstr(str);

	    cmov2i(20, 50);
	    sprintf(str, "Total number of Polygons: %d", NumPolys);
	    charstr(str);
	}
    }
    if (DispBonds) {
	cmov2i(20, 65);
	sprintf(str, "Bonds: %d", Nbonds);
	charstr(str);
    }

    SetLightModel(LightModel);
    popmatrix();
    if (!Stereo)
	DoProjection();

#ifdef USEOVERLAY
    drawmode(NORMALDRAW);
#else
    zfunction(ZF_LEQUAL);
#endif /* USEOVERLAY */
}

static void DrawScene(void)
{
    if (Depthcue)
	fogvertex(FG_ON, NULL);

    pushmatrix();
    
    translate(TransX, TransY, TransZ);
    scale(ScaleFactor, ScaleFactor, ScaleFactor);

    multmatrix(Mat);
    translate(-centerx, -centery, -centerz);

    if (DispAtoms)
	DrawAtoms();

    if (DispBonds)
	DrawBonds();

    popmatrix();
    
    if (Depthcue)
	fogvertex(FG_OFF, NULL);
}

void DisplayScene(void)
{
    if (!ModelRead) 
	return;
    winset(GeomWid);
    
    if (Stereo)
    {
	leftbuffer(1);
	rightbuffer(0);
	projOffset = viewOffset;
	DoProjection();
    }
    ClearScreen();
    DrawScene();
    DrawInfo();
    if (Stereo)
    {
	leftbuffer(0);
	rightbuffer(1);
	projOffset = -viewOffset;
	DoProjection();
	ClearScreen();
	DrawScene();
	DrawInfo();
    }
    swapbuffers();
    Alreadyaccumulated = 0;
}

void ChangeRadius(void)
{
    int atomtype;
    atom_t *aptr;
    int atom;

    for (atomtype = 0; atomtype < 8; atomtype++) {
	if (AtomTab[atomtype].natoms > 0) {
	    aptr = AtomTab[atomtype].atomlist;
	    for (atom = 0; atom < AtomTab[atomtype].natoms; atom++) { 
		if ((aptr->id < 1) || (aptr->id > 7)) {
		    printf("drawatom: uknown atom id=%d\n",aptr->id);
		}
		aptr->rad = aptr->org_rad * RadScaleFactor;
		aptr++;
	    }
	}
    } 
    DisplayScene();   
}

void SetGeomTitle(void)
{
    if (ModelId == -1) return;
    winset(GeomWid);
    wintitle(Models[ModelId].name);
}

void SetBackface(flag)
    int flag;
{
    winset(GeomWid);
    backface(flag);
}

int ReadKernel(char *filename)
{
    FILE *fpt = 0;
    int i;
    float x, y;

    if (!(fpt = fopen(filename, "r"))) {
	fprintf(stderr, "%s: cannot read kernel from file %s\n", ProgName, 
	    filename);
	return -1;
    }

    (void) fscanf(fpt, "%d", &Kernel.numsamples);
    for (i = 0; i < Kernel.numsamples; i++) {
	fscanf(fpt, "%f %f", &x, &y);
	Kernel.samples[i].x = x;
	Kernel.samples[i].y = y;
	Kernel.samples[i].weight = 1. - sqrt(x*x + y*y);
    }
    return 0;
}

#if 0
/* libsphere bitmap stubs */

void sphbgnbitmap(void) { }
void sphendbitmap(void) { }
void sphcolor(float clr[4]) {}

#endif

#if 0
/*
 * fast RE sphere stubs
 */
#endif
