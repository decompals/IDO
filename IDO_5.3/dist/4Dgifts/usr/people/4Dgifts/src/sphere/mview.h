/* mview.h
 * -------
 *
 * $Revision: 1.16 $
 *
 */

/*
 * A sample is x,y fractional offsets from pixel center and a weight.
 */
typedef struct {
    float x, y, weight;
} SAMPLE;

/* 
 * A kernel is an array of samples 
 */
#define MAXSAMPLES  64
typedef struct {
    int numsamples;
    SAMPLE samples[MAXSAMPLES];
} KERNEL;

typedef struct model {
    char *name;
    char *bond, *atom;
} MODEL;

#define DATA_DIR "/usr/demos/data/atom/"

#define RE 1

extern char *ProgName;

extern int UsePrefposition;
extern int UsePrefsize;
extern int UsePanel;

extern int Debug;

extern unsigned int BackgroundColor;
extern char *BackgroundImageName;

extern int hwGfxType;
extern int hwAccbuf;
extern int hwMultisample;
extern int hwStereo;
extern int hwBitmapSpheres;

extern int SphereType;
extern int SphereDepth;
extern int SpherePrim;
extern int Hemi;
extern int Orient;
extern int Multisample;
extern int Stereo;
extern int Perspective;
extern int Depthcue;

extern int AtomAccBuf;
extern int BondAccBuf;
extern int InfoAccBuf;
extern int BondSmooth;
extern int BondEndCorrect;
extern int SpinMode;
extern int RollMode;
extern int BitmapSpheres;


extern int DispAtoms;
extern int DispBonds;
extern int DispInfo;

extern int FastDraw;

extern int ModelId;
extern MODEL *Models;
extern int NumModels;
extern char **ModelNames;

extern int KernelId;
extern char **Kernels;
extern int NumKernels;

extern char *KernelFile;

extern float ZoomFactor;
extern float RadScaleFactor;
extern float MaxRadScaleFactor;
extern float MinRadScaleFactor;

/****************************************************************************/

extern void DoHelp(long);
extern void DoExit(int);
extern char *get_basename(char *);
extern void InitKernels(void);
extern int HwAccBuf(void);
extern char *get_basename(char *);

