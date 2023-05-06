/* mview.c
 * -------
 *
 * $Revision: 1.58 $
 *
 * How to Use:
 * -a				use accumulation buffer
 * -b color			use background color
 * -d SphereDepth		use tesselation depth SphereDepth
 * -f				enter fast draw
 * -h				on line help
 * -m display			0 - atoms, 1 - bonds, 2 -both
 * -n ModelName			use molecula model, ModelName
 * -p SpherePrim		use primitive SpherePrim for rendering
 * -r RadiusScaleFactor		scale all atom radii with RadiusScaleFactor
 * -t SphereType		use tesselation type SphereType
 * -z ZoomFactor		ZoomFactor
 * -A AntialiasMode		set antialias mode AntiAliasMode
 *				    0 - Off
 *				    1 - on
 *				    2 - smoother
 *				    3 - end correct
 *				    4 - smoother + end correct
 * -D				enter debug mode
 * -H program			execute program when help is requested
 * -I dir			directory to search for model files.
 * -K KernelFileName		Read kernel file KernelFileName
 * -P				use panel
 * -R				enter roll mode
 * -W x0,y0[,x1,y1]		set screen position and size
 */


#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>
#include <fmclient.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <sys/types.h>
#include <dirent.h>
#include <gl/sphere.h>

#include "mview.h"
#include "geom.h"
#include "panel.h"
#include "fileio.h"

#define Min(a, b) (((a) > (b)) ? (b) : (a))

/*~~~~~~~~~~~~~~~~~~~~~~~~~ global variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

char *ProgName;
char *HelpCmd = 0;

unsigned int BackgroundColor = 0xffcc1001;
char *BackgroundImageName = 0;

int hwGfxType=0;
int hwMultisample = 0;
int hwAccbuf = 0;
int hwStereo = 0;
int hwBitmapSpheres = 1;
int Debug = 0;
int DispAtoms = 1;
int DispBonds = 0;
int DispInfo = 1;
int AtomAccBuf = 0;
int SpinMode = 0;
int RollMode = 0;
int InfoAccBuf = 0;
int BondAccBuf = 0;
int BondSmooth = SML_OFF;
int BondEndCorrect = SML_OFF;
int UsePrefposition = 0;
int UsePrefsize = 0;
int UsePanel = 0;
int UseHelp = 0;
int FastDraw = 0;
float ZoomFactor = 1.0;
int Multisample = 0;
int Stereo = 0;
int Perspective = 0;
int Depthcue = 0;
int BitmapSpheres = 0;

int SphereType = SPH_OCT;
int SphereDepth = 8;
int SpherePrim = SPH_MESH;
int Hemi = 0;
int Orient = 0;

fmfonthandle FontScreen, FontScreen5, FontScreen8, FontScreen10;
fmfontinfo FontScreenInfo, FontScreen5Info, FontScreen8Info, FontScreen10Info;

float RadScaleFactor = 1.0;
float MaxRadScaleFactor = 1.0;
float MinRadScaleFactor = 0.1;

int NumAtomModels = 0;
int NumBondModels = 0;
int NumModels = 0;
char **AtomModels = 0;
char **BondModels = 0;
MODEL *Models = 0;
char **ModelNames = 0;
int ModelId = -1;

char *ModelName = 0;

int AtomModelId = -1;
int BondModelId = -1;

int KernelId = -1;
char **Kernels = 0;
int NumKernels = 0;

char *KernelFile = 0;

/*~~~~~~~~~~~~~~~~~~~~~~~~~ static variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

static char StrOption[] = "A:BDH:I:K:N:PRTW:ab:d:fhi:k:ln:p:r:st:w:z:?";
static char StrUsage[] = "[-DPRhf] [-n ModelName] [-r RadiusFactor] [-b color] [-z ZoomFactor] [-W x0,y0[,x1,y1]] [-t SphereType] [-d SphereDepth] [-p SpherePrim] [-K KernelFileName] [-I directory]";

#define NUM_DIRS 10
static char *Dirs[NUM_DIRS] = {
    ".", 
    "/usr/demos/data/atom"
};
static int NumDirs = 2;

static int GraphicsInitialized = 0;

extern long HelpWid;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

static void ParseArgs(int argc, char **argv);
static void Usage(void);
static void InitGraphics(void);
static void InitFonts(void);
static void InitQueue(void);
static void DoEvents(void);
static void InitModels(void);
static void SetModelId(void);
static int GetNumFiles(char *dir, char *s);
static int GetFileNames(char *dir, char *s, char **file);
static void AddDir(char *dir);
static void SetKernelId(void);
static unsigned int atox(char *);
int strcmpr(char *s1, char *s2);

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

void main(int argc, char **argv)
{
    ProgName = strdup(argv[0]);		/* program name			    */
    ParseArgs(argc, argv);		/* parse command line		    */
    InitModels();			/* initialize model names array	    */
    SetModelId();			/* according to the model name	    */
    InitKernels();			/* initialize kernel names array    */
    SetKernelId();			/* according to the kernel file	    */
    if (KernelId != -1)			/* if a specific kernel is provided */
	ReadKernel(Kernels[KernelId]);	/* ...read the kernel file	    */
    if (ModelId != -1)			/* if a specific model is provided  */
	InitData();			/* ...initialize the data	    */
    InitGraphics();			/* open the windows and initialize  */
    InitQueue();			/* initialize the queue		    */
    InitFonts();			/* initialize the fonts used	    */
    InitPanel();
    if (UsePanel) OpenPanelWindow();
    if (UsePanel) DrawPanel();		/* draw the panel		    */
    if (UseHelp) DoHelp(0);
    OpenGeomWindow();
    DefineLight();			/* define lights properties	    */
    DoLineSmooth();
    InitSphere();			/* initialize the sphere parameters */
    CalcInfo();				/* calculate teh sphere info	    */
    if (AtomAccBuf || BondAccBuf)	/* if specified,		    */
	InitAccBuf();			/* initialize the accumulation buf  */
    DoReset();				/* Reset transformation matrix	    */
    if ((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf))
	DisplayAccScene();
    else DisplayScene();
    DoEvents();
}

static void ParseArgs(int argc, char **argv)
{
    int opt;
    int i;
    char *tmp;

    opterr = 0;				    /* disable getopt error msgs    */
    while ((opt = getopt(argc, argv, StrOption)) != EOF) {
	switch(opt) {
	  case '?':
	    Usage();
	    DoExit(0);
	  case 'a':
	    AtomAccBuf = 1 - AtomAccBuf;
	    printf("AtomAccBuf=%d\n", AtomAccBuf);
	    break;
	  case 'b':
	    BackgroundColor = atox(optarg);
	    printf("BackgroundColor=%x\n", BackgroundColor);
	    break;
	  case 'd':
	    SphereDepth = atoi(optarg);
	    printf("SphereDepth=%d\n",SphereDepth);
	    break;
	  case 'f': 
	    FastDraw = 1 - FastDraw;
	    printf("FastDraw=%d\n", FastDraw);
	    break;
	  case 'h':
	    UseHelp = 1;
	    break;
	  case 'i': 
	    BackgroundImageName = strdup(optarg);
	    printf("BackgroundImageName=%d\n", BackgroundImageName);
	    break;
	  case 'n':
	    ModelName = strdup(optarg);
	    break;
	  case 'p':
	    printf("SpherePrim=%d\n", SpherePrim);
	    break;
	  case 'r':
	    RadScaleFactor = atof(optarg);
	    printf("RadScaleFactor=%f\n",RadScaleFactor);
	    break;
	  case 't':
	    SphereType = atoi(optarg);
	    printf("spheretesstype=%d\n",SphereType);
	    break;
	  case 'z':
	    ZoomFactor =  atof(optarg);
	    printf("ZoomFactor=%f\n",ZoomFactor);
	    break;
	  case 'A':
	    switch (atoi(optarg)) {
	      case 0:
		BondSmooth = SML_OFF;
		BondEndCorrect = SML_OFF;
		break;
	      case 1:
		BondSmooth = SML_ON;
		BondEndCorrect = SML_OFF;
		break;
	      case 2:
		BondSmooth = SML_SMOOTHER;
		BondEndCorrect = SML_OFF;
		break;
	      case 3:
		BondSmooth = SML_OFF;
		BondEndCorrect = SML_END_CORRECT;
		break;
	      case 4:
		BondSmooth = SML_SMOOTHER;
		BondEndCorrect = SML_END_CORRECT;
		break;
	    }
	    break;
	  case 'B': 
		BitmapSpheres ^= 1;
		break;
	  case 'D': 
	    Debug = 1 - Debug;
	    printf("in debug mode\n");
	    break;

	  case 'H':
	    HelpCmd = (char *) malloc((unsigned) (strlen(optarg) + 3));
	    strcpy(HelpCmd, optarg);
	    strcat(HelpCmd, " &");
	    break;

	  case 'I':
	    AddDir(optarg);
	    break;
	  case 'K':
	    if ( (tmp = strrchr(optarg, '/')) != NULL) {
		KernelFile = strdup(tmp + 1);
		*tmp = '\0';
		AddDir(optarg);
		*tmp = '/';
	    } else {
		KernelFile = strdup(optarg);
		AddDir(".");
	    }
	    printf("Kernel file %s\n", optarg);
	    break;
	  case 'R':
	    RollMode = 1 - RollMode;
	    break;
	  case 'P':
	    UsePanel = 1;
	    break;
	  case 'W': /* provide x1,y1,x2,y2  or just xsize, ysize */
	    if (4 == sscanf(optarg, "%d,%d,%d,%d", &GeomXorigin, &GeomYorigin, 
	    &GeomXsize, &GeomYsize)) {
		UsePrefposition = 1;
	    } else if (2 == sscanf(optarg, "%d,%d", &GeomXsize, &GeomYsize)) {
		UsePrefsize = 1;
	    } else  {
		Usage();
		DoExit(-1);
	    }
	    break;
	  default:
	    Usage();
	    DoExit(-1);
	}
    }
    if (BitmapSpheres)
	Multisample = 0;
}

static void Usage(void)
{
    fprintf(stderr, "usage: %s %s\n", ProgName, StrUsage);
    fprintf(stderr, "\n\
Use the -n option to specify a model name (i.e. advil).  The corresponding \n\
data files are searched for in the default directories, namely '.' and \n\
'/usr/demos/data/atom', and in the directories specified by the -I \n\
option.\n");
}

static void DoEvents(void)
{
    int done = 0;
    long dev;
    short val;
    short geom_attached = 0;
    short panel_attached = 0;
    short help_attached = 0;
    int help_window_iconified = 0;
    int panel_window_iconified = 0;

    while(!done) {
	if ((!GeomLeftDown && !GeomMiddleDown && !GeomRightDown && !RotMode &&
	!PanelButtonDown) || qtest()) {
	    switch(dev = qread(&val)) {

	      case INPUTCHANGE:
		geom_attached = (val == GeomWid);
		panel_attached = (val == PanelWid);
		help_attached = (val == HelpWid);
		break;

	      case ESCKEY:
		if (geom_attached) done = 1;
		else if (panel_attached) done = 1;
		else if (help_attached) DoEventsHelp(dev, val);
		break;

	      case REDRAW:
		if (val == GeomWid)
		    DoEventsGeom(dev, val);
		else if (val == PanelWid)
		    DoEventsPanel(dev, val);
		else if (val == HelpWid)
		    DoEventsHelp(dev, val);
		break;

	      case WINTHAW:
		if (val == GeomWid) {
		    if (help_window_iconified) {
			InitHelpWindow();
			DrawHelp();
		    }
		    if (panel_window_iconified) {
			OpenPanelWindow();
			DrawPanel();
		    }
		}
		break;

	      case WINFREEZE:
		if (val == GeomWid) {
		    if (PanelWid != -1) {
			ClosePanelWindow();
			panel_window_iconified = 1;
		    } else panel_window_iconified = 0;
		    if (HelpWid != -1) {
			CloseHelpWindow();
			help_window_iconified = 1;
		    } else help_window_iconified = 0;
		}
		break;

	      default:
		if (geom_attached)
		    DoEventsGeom(dev, val);
		else if (UsePanel && panel_attached)
		    DoEventsPanel(dev, val);
	    }
	}
	if (UsePanel && panel_attached) {
	    DoPanel(dev, val);
	    if (RotMode)
		DoGeom();
	} else if (geom_attached)
	    DoGeom();
    }
}

static void InitFonts(void)
{
    fminit();

    FontScreen = fmfindfont("Screen");
    FontScreen5 = fmscalefont(FontScreen, 5.0);
    FontScreen8 = fmscalefont(FontScreen, 8.0);
    FontScreen10 = fmscalefont(FontScreen, 10.0);

    fmgetfontinfo(FontScreen, &FontScreenInfo);
    fmgetfontinfo(FontScreen5, &FontScreen5Info);
    fmgetfontinfo(FontScreen8, &FontScreen8Info);
    fmgetfontinfo(FontScreen10, &FontScreen10Info);
}

static void InitGraphics(void)
{
    char gfxstr[12];
    int c;
    
    if (Debug) foreground();
    
    gversion(gfxstr);
    if (strstr(gfxstr, "RE"))
    {
	hwGfxType = RE;
	Hemi = 0;
    }

    if ((c = getgdesc(GD_BITS_NORM_DBL_BLUE)) <= 0) 
    {
	fprintf(stderr, "%s: Not enough bitplanes available.\n", ProgName);
	DoExit(-1);
    }
    if (!(hwMultisample = getgdesc(GD_MULTISAMPLE)))
    {
	fprintf(stderr, "%s: No multisampling on this machine.\n", ProgName);
	Multisample = 0;
    }

    /* check for very low end */
    if ((c <= 4) || (!(getgdesc(GD_BITS_NORM_ZBUFFER))))
	SphereDepth = 4;
    else if (!hwMultisample) /* mid range */
	SphereDepth = 6;

    if (hwMultisample)
    {
	if (!BitmapSpheres)
	    Multisample = 1; /* come up in multisample */
	AtomAccBuf = 0;
    }

    if (!getgdesc(GD_FOGPIXEL))
	FogType = FG_VTX_LIN;


    if (!(hwAccbuf = (getgdesc(GD_BITS_ACBUF) >= 16)))
    {
	fprintf(stderr, "%s: No accumulation buffer.\n", ProgName);
    } else
    {
	if (!Multisample)
		hwAccbuf = 12;
	else
		hwAccbuf = 16;
    }

   if (!(hwStereo = getgdesc(GD_STEREO_IN_WINDOW)))
   {
	fprintf(stderr, "%s: No in-the-window stereo on this machine.\n", ProgName);
	fprintf(stderr, "%s: No bitmap spheres on this machine.\n", ProgName);
	hwBitmapSpheres = 0;
    }

    ZMin = getgdesc(GD_ZMIN);
    ZMax = getgdesc(GD_ZMAX);

    noport();
    winopen("mview");
    GraphicsInitialized = 1;
}

static void InitQueue(void)
{
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(RIGHTMOUSE);
    qdevice(ESCKEY);
    qdevice(INPUTCHANGE);
    qdevice(REDRAW);
    qdevice(LEFTARROWKEY);
    qdevice(RIGHTARROWKEY);
    qdevice(UPARROWKEY);
    qdevice(DOWNARROWKEY);
    qdevice(KEYBD);
    qdevice(WINTHAW);
    qdevice(WINFREEZE);

    qreset();
}

void DoExit(int rc)
{
    int i;

    DoExitGeom();
    if (GraphicsInitialized) {
#ifdef USEOVERLAY
	drawmode(OVERDRAW);
	color(0);
	clear();
	drawmode(NORMALDRAW);
#endif /* USEOVERLAY */
	gexit();
    }
    if (AtomModels) {
	for (i = 0; i < NumAtomModels; i++)
	    free(AtomModels[i]);
	free(AtomModels);
    }
    if (BondModels) {
	for (i = 0; i < NumBondModels; i++)
	    free(BondModels[i]);
	free(BondModels);
    }
    if (Models) {
	for (i = 0; i < NumModels; i++)
	    free(Models[i].name);
	free(Models);
    }
    if (ModelNames)
	free(ModelNames);
    if (ModelName)
	free(ModelName);

    exit(rc);
}

/* utiliy functions */

int HwAccBuf(void)
{
    return (hwAccbuf);
}

static unsigned int atox(char *s)
{
    unsigned int x;

    if ((isalpha(s[0])) || ((strncmp(s,"0x",2) == 0) && (s += 2))) {
        sscanf(s,"%x",&x);
    } else
	sscanf(s,"%u",&x);
    return x;
}

char *get_basename(char *file)
{
    char *bname;

    bname = strrchr(file, '/');
    if (bname) return (bname + 1);
    else return (file);
}

void DoHelp(long index)
{
    char *str;

    if (HelpCmd) {
	system(HelpCmd);
	return;
    }
    if (HelpWid == -1) {
	InitHelp();
	InitHelpWindow();
	DrawHelp();
    } else CloseHelpWindow();
}

static void InitModels(void)
{
    char **dir_ptr;
    char **model_ptr;
    int i, j;
    char *tmp, atom_model[100], bond_model[100];
    int length;

    if (NumDirs == 0) {
	fprintf(stderr, "%s: no directories to search for models found.\n", 
	    ProgName);
	DoExit(0);
    }

    for (dir_ptr = Dirs; dir_ptr < &Dirs[NumDirs]; dir_ptr++) {
	NumAtomModels += GetNumFiles(*dir_ptr, "*.atoms");
    }

    if (NumAtomModels == 0) {
	fprintf(stderr, 
	    "%s: no models found in the specified directories: ", 
	    ProgName);
	for (dir_ptr = Dirs; dir_ptr < &Dirs[NumDirs]; dir_ptr++)
	    fprintf(stderr, "%s, ", *dir_ptr);
	fprintf(stderr, "\n");
	DoExit(0);
    }

    AtomModels = (char **) malloc (NumAtomModels * sizeof(char *));
    model_ptr = AtomModels;
    for (dir_ptr = Dirs; dir_ptr < &Dirs[NumDirs]; dir_ptr++) {
	model_ptr = &model_ptr[GetFileNames(*dir_ptr, "*.atoms", model_ptr)];
    }

    for (dir_ptr = Dirs; dir_ptr < &Dirs[NumDirs]; dir_ptr++) {
	NumBondModels += GetNumFiles(*dir_ptr, "*.bonds");
    }

    BondModels = (char **) malloc (NumBondModels * sizeof(char *));
    model_ptr = BondModels;
    for (dir_ptr = Dirs; dir_ptr < &Dirs[NumDirs]; dir_ptr++) {
	model_ptr = &model_ptr[GetFileNames(*dir_ptr, "*.bonds", model_ptr)];
    }

    Models = 
	(MODEL *) malloc(Min(NumBondModels, NumAtomModels) * sizeof(MODEL));
    for (i = 0; i < NumAtomModels; i++) {
	tmp = get_basename(AtomModels[i]);
	length = strlen(tmp) - 6;
	strncpy(atom_model, tmp, length);
	atom_model[length] = '\0';
	for (j = 0; j < NumBondModels; j++) {
	    tmp = get_basename(BondModels[j]);
	    length = strlen(tmp) - 6;
	    strncpy(bond_model, tmp, length);
	    bond_model[length] = '\0';
	    if (strcmp(atom_model, bond_model) == 0) {
		Models[NumModels].name = strdup(atom_model);
		Models[NumModels].atom = AtomModels[i];
		Models[NumModels++].bond = BondModels[j];
		break;
	    }
	}
    }

    ModelNames = (char **) malloc (NumModels * sizeof(char *));
    for (i = 0; i < NumModels; i++)
	ModelNames[i] = Models[i].name;
}

static void SetModelId(void)
{
    int i;

    if (ModelName) {
	for (i = 0; i < NumModels; i++) {
	    if (strcmp(Models[i].name, ModelName) == 0) {
		ModelId = i;
		break;
	    }
	}
	if (i == NumModels) {
	    fprintf(stderr, "%s: Model %s is not available\n", 
	    ProgName, ModelName);
	}
    }
}

void InitKernels(void)
{
    char **dir_ptr;
    char **kernel_ptr;

    if (Kernels) {
	free (Kernels);
	NumKernels = 0;
    }

    for (dir_ptr = Dirs; dir_ptr < &Dirs[NumDirs]; dir_ptr++) {
	NumKernels += GetNumFiles(*dir_ptr, "*.kl");
    }

    Kernels = (char **) malloc (NumKernels * sizeof(char *));
    kernel_ptr = Kernels;
    for (dir_ptr = Dirs; dir_ptr < &Dirs[NumDirs]; dir_ptr++) {
	kernel_ptr = &kernel_ptr[GetFileNames(*dir_ptr, "*.kl", kernel_ptr)];
    }
}

static void SetKernelId(void)
{
    int i;

    if (KernelFile) {
	for (i = 0; i < NumKernels; i++) {
	    if (strcmp(get_basename(Kernels[i]), KernelFile) == 0) {
		KernelId = i;
		break;
	    }
	}
	if (i == NumKernels) {
	    fprintf(stderr, "%s: Kernel %s is not available\n", 
	    ProgName, KernelFile);
	}
    }
}

static int GetNumFiles(char *dir, char *s)
{
    DIR *dd;
    struct dirent *dirent_p;
    int i = 0;

    if (!(dd = opendir(dir))) {
	fprintf(stderr, "%s: fail to open directory %s\n", ProgName, dir);
	return 0;
    }
    while (dirent_p = readdir(dd)) {
	if (strcmpr(dirent_p->d_name, s))
	    i++;
    }
    closedir(dd);
    return (i);
}

static int GetFileNames(char *dir, char *s, char **file)
{
    char *f;
    DIR *dd;
    struct dirent *dirent_p;
    int i = 0;

    if (!(dd = opendir(dir))) {
	fprintf(stderr, "Fail to open directory %s\n", dir);
	return 0;
    }
    while (dirent_p = readdir(dd)) {
	f = dirent_p->d_name;
	if (strcmpr(f, s)) {
	    if (dir[strlen(dir)-1] != '/') {
		file[i] = (char *)
			malloc((strlen(f) + strlen(dir) + 2) * sizeof(char));
		strcpy(file[i], dir);
		strcat(file[i], "/");
	    } else {
		file[i] = (char *)
			malloc((strlen(f) + strlen(dir) + 1) * sizeof(char));
		strcpy(file[i], dir);
	    }
	    strcat(file[i++], f);
	}
    }
    closedir(dd);
    return (i);
}

/* strcmpr() takes two expressions as arguments. It compares the given
expressions and returns 0 or 1 as they are different or equivalent
respectively. The two expressions are strings. The '*' is a special
character that expands to any string.
 */
int strcmpr(char *s1, char *s2)
{
    if ((s1[0] == '\0') && (s2[0] == '\0'))
	return (1);

    if (s1[0] == '\0') {
	if (s2[0] != '*')
	    return (0);
	else
	    return (strcmpr(s1, &s2[1]));
    }

    if (s2[0] == '\0') {
	if (s1[0] != '*')
	    return (0);
	else
	    return (strcmpr(&s1[1], s2));
    }

    if ((s1[0] == s2[0]) && (s1[0] != '*') && (s2[0] != '*'))
	return (strcmpr(&s1[1], &s2[1]));

    if ((s1[0] == '*') || (s2[0] == '*'))
	return (
	    strcmpr(&s1[1], s2) ||	/* match to the empty string	    */
	    strcmpr(&s1[1], &s2[1]) ||	/* match to one caracter	    */
	    strcmpr(s1, &s2[1])		/* match to more than one caracter  */
	);

    return (0);
}

static void AddDir(char *dir)
{
    int i;

    if (NumDirs == NUM_DIRS) {
	fprintf(stderr, "%s: no more directories are available\n", ProgName);
	return;
    }

    for (i = 0;  i < NumDirs;  i++)
	if (strcmp(Dirs[i], dir) == 0)
	    return;

    Dirs[NumDirs++] = strdup(dir);
}

