/* menu.c
 * ------
 *
 * $Revision: 1.8 $
 *
 */

#include <string.h>
#include <gl/gl.h>
#include <string.h>
#include <gl/sphere.h>

#include "mview.h"
#include "geom.h"

#define MAX_ENTRIES 15
#define MAX_MENU_ENTRY 100

/****************************************************************************/

static int BuildModelMenu();
static int BuildAtomMenu();
static int BuildBondMenu();
static int BuildSphereTypeMenu();
static int BuildSpherePrimMenu();
static int BuildBondSmoothMenu();
static int BuildKernelMenu();

static void SetFastDraw();
static void SetDispAtoms();
static void SetDispBonds();
static void SetSphereType();
static void SetSpherePrim();
static void SetAtomAccBuf();
static void SetBondAccBuf();
static void SetBondEndCorrect();
static void SetSpinMode();
static void SetRollMode();
static void SetBondSmooth();
static void SetPanel();
static void Reset();
static void Exit();
static void SetModel();
static void SetKernel();

/****************************************************************************/

void addtopupx(menu, entry, index, current)
    int menu;
    char *entry;
    int index;
    int current;
{
    char new[MAX_MENU_ENTRY];

    if (index == current)
	sprintf(new, "*%s %%x%d", entry, index);
    else
	sprintf(new, "%s %%x%d", entry, index);
    addtopup(menu, new);
}

void build_long_menu(names, size, current, main_menu, index, basename_flag)
    char **names;
    int size, current;
    int main_menu;
    long index;
    int basename_flag;
{
    int	sub_menu;
    int	i;
    char entry[MAX_MENU_ENTRY], *s;

    for (i = 0; i < size; i++) {
	if ((i != 0) && ((i % MAX_ENTRIES) == 0)) main_menu = sub_menu;
	if (((size - i) > MAX_ENTRIES) &&
	    ((i % MAX_ENTRIES) == 0)) {
	    sub_menu = defpup("Items %t");
	    addtopup(main_menu, "More %m", sub_menu);
	}
        if (basename_flag) {
	    if (s = strrchr(names[i], '/'))
		s++;
	    else
		s = names[i];
	} else
	    s = names[i];

        addtopupx(main_menu, s, index++, current);
    }
}

void BuildMainMenu()
{
    char str[200];
    long sub_menu;

    if (Menu != -1)
	freepup(Menu);

    sprintf(str, "%s %%t", ProgName);
    Menu = defpup(str);
    if (UsePanel)
	addtopup(Menu, "Close Panel %f", SetPanel);
    else
	addtopup(Menu, "Open Panel %f", SetPanel);
    addtopup(Menu, "Models %m", BuildModelMenu());
    addtopup(Menu, "Kernels %m", BuildKernelMenu());
    addtopup(Menu, "Reset %f", Reset);
    if (FastDraw)
	addtopup(Menu, "Draw Quick off %f", SetFastDraw);
    else
	addtopup(Menu, "Draw Quick on %f", SetFastDraw);
    addtopup(Menu, "Atom %m", BuildAtomMenu());
    /* ZoomFactor */
    addtopup(Menu, "Bond %m", BuildBondMenu());
    addtopup(Menu, "Help %f", DoHelp);
    addtopup(Menu, "Exit %f", Exit);
}

static int BuildModelMenu()
{
    static long menu = -1;

    if (menu != -1)
	freepup(menu);
    menu = defpup("Models %t %F", SetModel);

    build_long_menu(ModelNames, NumModels, ModelId, menu, 0, 0);
    return menu;
}

static int BuildKernelMenu()
{
    static long menu = -1;

    if (menu != -1)
	freepup(menu);
    menu = defpup("Kernels %t %F", SetKernel);
    InitKernels();
    build_long_menu(Kernels, NumKernels, KernelId, menu, 0, 0);
    return menu;
}

static int BuildAtomMenu()
{
    static long menu = -1;

    if (menu != -1)
	freepup(menu);
    menu = defpup("Atom %t");
    if (DispAtoms)
	addtopup(menu, "Display off %f", SetDispAtoms);
    else
	addtopup(menu, "Display on %f", SetDispAtoms);
    addtopup(menu, "Sphere Type %m", BuildSphereTypeMenu());
    addtopup(menu, "Sphere Primitive %m", BuildSpherePrimMenu());
    /* XXX SphereDepth */
    if (AtomAccBuf)
	addtopup(menu, "AccBuf off %f", SetAtomAccBuf);
    else
	addtopup(menu, "AccBuf on %f", SetAtomAccBuf);
    if (SpinMode)
	addtopup(menu, "SpinMode off %f", SetSpinMode);
    else
	addtopup(menu, "SpinMode on %f", SetSpinMode);
    if (RollMode)
	addtopup(menu, "RollMode off %f", SetRollMode);
    else
	addtopup(menu, "RollMode on %f", SetRollMode);
    /* XXX RadScaleFactor */
    return menu;
}

static int BuildSphereTypeMenu()
{
    static long menu = -1;

    if (menu != -1)
	freepup(menu);
    menu = defpup("Sphere Type %t %F", SetSphereType);
    addtopupx(menu, "oct", SPH_OCT, SphereType);
    addtopupx(menu, "icos", SPH_ICOS, SphereType);
    addtopupx(menu, "cube", SPH_CUBE, SphereType);
    addtopupx(menu, "bary", SPH_BARY, SphereType);
    addtopupx(menu, "bilin", SPH_BILIN, SphereType);
    return menu;
}

static int BuildSpherePrimMenu()
{
    static long menu = -1;

    if (menu != -1)
	freepup(menu);
    menu = defpup("Sphere Prim %t %F", SetSpherePrim);
    addtopupx(menu, "mesh", SPH_MESH, SpherePrim);
    addtopupx(menu, "poly", SPH_POLY, SpherePrim);
    addtopupx(menu, "line", SPH_LINE, SpherePrim);
    addtopupx(menu, "point", SPH_POINT, SpherePrim);
    return menu;
}

static int BuildBondMenu()
{
    static long menu = -1;

    if (menu != -1)
	freepup(menu);
    menu = defpup("Bond %t");
    if (DispBonds)
	addtopup(menu, "Display off %f", SetDispBonds);
    else
	addtopup(menu, "Display on %f", SetDispBonds);
    if (BondAccBuf)
	addtopup(menu, "AccBuf off %f", SetBondAccBuf);
    else
	addtopup(menu, "AccBuf on %f", SetBondAccBuf);
    addtopup(menu, "Smooth %m", BuildBondSmoothMenu());
    if (BondEndCorrect)
	addtopup(menu, "EndCorrect off %f", SetBondEndCorrect);
    else
	addtopup(menu, "EndCorrect on %f", SetBondEndCorrect);
    return menu;
}

static int BuildBondSmoothMenu()
{
    static long menu = -1;

    if (menu != -1)
	freepup(menu);
    menu = defpup("Smooth %t %F", SetBondSmooth);
    addtopupx(menu, "Off", SML_OFF, BondSmooth);
    addtopupx(menu, "On", SML_ON, BondSmooth);
    addtopupx(menu, "Smoother", SML_SMOOTHER, BondSmooth);
    return menu;
}

static void SetFastDraw()
{
    FastDraw = 1 - FastDraw;

    if (FastDraw) {
	if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
	!Alreadyaccumulated)
	    DisplayAccScene();
	else
	    DisplayScene();
    } else {
	BondSmooth = SML_OFF;
	BondEndCorrect = SML_OFF;
	DoLineSmooth();
	BondAccBuf = 0;
	AtomAccBuf = 0;
	DisplayScene();
    }

    if (UsePanel)
	DrawPanel();
}

static void SetDispAtoms()
{
    DispAtoms = 1 - DispAtoms;

    if ((AtomAccBuf || (DispBonds && BondAccBuf)) && !Alreadyaccumulated)
	DisplayAccScene();
    else
	DisplayScene();
    if (UsePanel)
	DrawPanel();
}

static void SetDispBonds()
{
    DispBonds = 1 - DispBonds;

    if (((DispAtoms && AtomAccBuf) || BondAccBuf) && !Alreadyaccumulated)
	DisplayAccScene();
    else
	DisplayScene();
    if (UsePanel)
	DrawPanel();
}

static void SetSphereType(index)
    long index;
{
    SphereType = index;
    sphmode(SPH_TESS, SphereType);
    CalcInfo();

    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else
	DisplayScene();
    if (UsePanel)
	DrawPanel();
}

static void SetSpherePrim(index)
    long index;
{
    SpherePrim = index;
    sphmode(SPH_PRIM, SpherePrim);
    CalcInfo();

    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else
	DisplayScene();
    if (UsePanel)
	DrawPanel();
}

static void SetAtomAccBuf(index)
    long index;
{
    AtomAccBuf = 1 - AtomAccBuf;
    if (AtomAccBuf) {
	InitAccBuf();
	if ((DispAtoms || (DispBonds && BondAccBuf)) && !Alreadyaccumulated)
	    DisplayAccScene();
	else
	    DisplayScene();
    } else {
    if ((DispBonds && BondAccBuf) && !Alreadyaccumulated)
	DisplayAccScene();
    else
	DisplayScene();
    }

    if (UsePanel)
	DrawPanel();
}

static void SetSpinMode(index)
    long index;
{
    SpinMode = 1 - SpinMode;

    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else
	DisplayScene();
    if (UsePanel)
	DrawPanel();
}

static void SetRollMode(index)
    long index;
{
    RollMode = 1 - RollMode;

    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else
	DisplayScene();
    if (UsePanel)
	DrawPanel();
}

static void Exit(index)
    long index;
{
    DoExit(0);
}

static void Reset(index)
    long index;
{
    DoReset();
    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else
	DisplayScene();
    if (UsePanel)
	DrawPanel();
}

static void SetBondAccBuf(index)
    long index;
{
    BondAccBuf = 1 - BondAccBuf;

    if (BondAccBuf) {
	InitAccBuf();

	if (((DispAtoms && AtomAccBuf) || BondAccBuf) && !Alreadyaccumulated)
	    DisplayAccScene();
	else
	    DisplayScene();
    } else {
	if ((DispAtoms && AtomAccBuf) && !Alreadyaccumulated)
	    DisplayAccScene();
	else
	    DisplayScene();
    }
    if (UsePanel)
	DrawPanel();
}

static void SetBondSmooth(index)
    long index;
{
    BondSmooth = index;
    DoLineSmooth();

    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else
	DisplayScene();

    if (UsePanel)
	DrawPanel();
}

static void SetBondEndCorrect(index)
    long index;
{
    BondEndCorrect = 1 - BondEndCorrect;
    DoLineSmooth();

    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else
	DisplayScene();

    if (UsePanel)
	DrawPanel();
}

static void SetPanel(index)
    int index;
{
    UsePanel = 1 - UsePanel;
    if (UsePanel) {
	OpenPanelWindow();
	DrawPanel();
    } else {
	ClosePanelWindow();
    }
}

static void SetModel(index)
    int index;
{
    ModelId = index;
    SetGeomTitle();
    InitData();
    CalcInfo();

    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else
	DisplayScene();

    if (UsePanel)
	DrawPanel();
}

static void SetKernel(index)
    int index;
{
    KernelId = index;
    if (KernelFile)
	free (KernelFile);
    KernelFile = strdup(get_basename(Kernels[KernelId]));
    if (ReadKernel(Kernels[KernelId]) == -1)
	return;

    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else
	DisplayScene();

    if (UsePanel)
	DrawPanel();
}

