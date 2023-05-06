/* panel.c
 * -------
 *
 * $Revision: 1.22 $
 *
 */

#include <stdio.h>
#include <fmclient.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <gl/sphere.h>

#include "panel.h"
#include "mview.h"
#include "geom.h"

#define COLOR_RAMP_SIZE 8

extern fmfonthandle FontScreen, FontScreen5, FontScreen8;
extern fmfontinfo FontScreenInfo, FontScreen5Info, FontScreen8Info;

long PanelWid = -1;
int PanelButtonDown = 0;

static float RangeRadScaleFactor;

static unsigned long WhiteRamp[COLOR_RAMP_SIZE];
static unsigned long RedRamp[COLOR_RAMP_SIZE];

static long PanelXorigin, PanelYorigin;
static int PanelPrefposition = 0;

static float Value(float m1, float m2, float hue);
static unsigned long HLStoRGB(float h, float l, float s);
static void DrawBack(ITEM *item);
static void DrawText(char text[], ITEM *item);
static void BevelRect(RECT *rect, int pushed, unsigned long *c, int width);
static void DrawName(char *name, RECT *rect);

static void DspEvent(ITEM *item);
static void SelectQuit(ITEM *item, long dev);
static void SelectHelp(ITEM *item, long dev);
static void SelectReset(ITEM *item, long dev);
static void DspSphereType(ITEM *item);
static void SelectSphereType(ITEM *item, long dev);
static void DspSpherePrim(ITEM *item);
static void SelectSpherePrim(ITEM *item, long dev);
static void DspSphereDepth(ITEM *item);
static void SelectSphereDepth(ITEM *item, long dev);
static void DspAtomAccBuf(ITEM *item);
static void SelectAtomAccBuf(ITEM *item, long dev);
static void DspBondAccBuf(ITEM *item);
static void SelectBondAccBuf(ITEM *item, long dev);
static void DspBondSmooth(ITEM *item);
static void SelectBondSmooth(ITEM *item, long dev);
static void DspBondEndCorrect(ITEM *item);
static void SelectBondEndCorrect(ITEM *item, long dev);
static void DspRadius(ITEM *item);
static void SelectRadius(ITEM *item, long dev);
static void DspModel(ITEM *item);
static void SelectModel(ITEM *item, long dev);
static void SelectMultisample(ITEM *item, long dev);
static void SelectStereo(ITEM *item, long dev);
static void SelectPerspective(ITEM *item, long dev);
static void DspProjection(ITEM *item);
static void SelectBitmapSpheres(ITEM *item, long dev);

static void DspOnOff(ITEM *item);
static void SelectOnOff(ITEM *item, long dev);

ITEM *PrevItem = 0;
ITEM Items[NUM_ITEMS] = {
    {"Quit",		    {  10, 110,  10,  40}, DspEvent, SelectQuit, 
	NULL, NULL, STATUS_DEFAULT, WhiteRamp}, 
    {"Help",		    {  10, 110,  60,  90}, DspEvent, SelectHelp, 
	NULL, NULL,  STATUS_DEFAULT, WhiteRamp}, 
    {"Reset",		    {  10, 110, 110, 140}, DspEvent, SelectReset, 
	NULL, NULL, STATUS_DEFAULT, WhiteRamp}, 
    {"Models",		    {  10, 110, 160, 190}, DspModel, SelectModel, 
	NULL, &ModelId, STATUS_DEFAULT, WhiteRamp}, 
    {"Display Atoms",	    { 120, 230,  10,  35}, DspOnOff, 
	SelectOnOff, DoDispAtoms, &DispAtoms, STATUS_DEFAULT, WhiteRamp}, 
    {"Display Bonds",	    { 120, 230,  60,  85}, DspOnOff, 
	SelectOnOff, DoDispBonds, &DispBonds, STATUS_DEFAULT, WhiteRamp}, 
    {"Bitmap Spheres",	    { 120, 230, 110, 135}, DspOnOff, 
	SelectBitmapSpheres, DoBitmapSpheres, &BitmapSpheres, 
	STATUS_DEFAULT, WhiteRamp}, 
    {"Sphere Type",	    { 240, 350,  10,  35}, DspSphereType, 
	SelectSphereType, NULL, &SphereType, STATUS_DEFAULT, WhiteRamp}, 
    {"Sphere Primitive",    { 240, 350,  60,  85}, DspSpherePrim, 
	SelectSpherePrim, NULL, &SpherePrim, STATUS_DEFAULT, WhiteRamp}, 
    {"Sphere Depth",	    { 240, 350, 110, 135}, DspSphereDepth, 
	SelectSphereDepth, NULL, &SphereDepth, STATUS_DEFAULT, WhiteRamp},
    {"Hemi Spheres",        { 360, 470,  10,  35}, DspOnOff, SelectOnOff, 
	DoHemi, &Hemi, STATUS_DEFAULT, WhiteRamp},
    {"Sph Orient",          { 360, 470,  60,  85}, DspOnOff, SelectOnOff, 
	DoOrient, &Orient, STATUS_DEFAULT, WhiteRamp},
    {"Accumulate Atoms",    { 480, 590,  10,  35}, DspOnOff, 
	SelectAtomAccBuf, InitAccBuf, &AtomAccBuf, STATUS_DEFAULT, WhiteRamp}, 
    {"SpinMode",	    { 480, 590,  60,  85}, DspOnOff, 
	SelectOnOff, DoSpinMode, &SpinMode, STATUS_DEFAULT, WhiteRamp},
    {"RollMode",	    { 480, 590, 110, 135}, DspOnOff, 
	SelectOnOff, DoAccRollMode, &RollMode, STATUS_DEFAULT, WhiteRamp},
    {"Accumulate Bonds",    { 600, 710,  10,  35}, DspOnOff, 
	SelectBondAccBuf, InitAccBuf, &BondAccBuf, STATUS_DEFAULT, WhiteRamp}, 
    {"Smooth Bonds",	    { 600, 710,  60,  85}, DspBondSmooth, 
	SelectBondSmooth, NULL, &BondSmooth, STATUS_DEFAULT, WhiteRamp}, 
    {"End Correct Bonds",   { 600, 710, 110, 135}, DspBondEndCorrect,
	SelectBondEndCorrect, NULL, &BondSmooth, STATUS_DEFAULT, WhiteRamp}, 
    {"Multisample",	    { 480, 590, 160, 185}, DspOnOff,
	SelectMultisample, NULL, &Multisample, STATUS_DEFAULT, WhiteRamp}, 
    {"Stereo",		    { 600, 710, 160, 185}, DspOnOff,
	SelectStereo, DoStereo, &Stereo, STATUS_DEFAULT, WhiteRamp}, 
    {"Projection",	    { 360, 470,  110,  135}, DspProjection,
	SelectPerspective, DoProjection, &Perspective, STATUS_DEFAULT, WhiteRamp}, 
    {"Depthcue",	    { 360, 470,  160,  185}, DspOnOff,
	SelectOnOff, NULL, &Depthcue, STATUS_DEFAULT, WhiteRamp}, 
};

ITEM *orientItem =  &Items[11];
ITEM *msItem =  &Items[18];
ITEM *accAtomItem =  &Items[12];
ITEM *accBondItem =  &Items[15];
ITEM *stereoItem =  &Items[19];
ITEM *projItem =  &Items[20];

ITEM RadiusItem = {
    "Radius",		    { 120, 350, 160, 200}, DspRadius, SelectRadius, 
				    ChangeRadius, (int *)&RadScaleFactor, 
};

/* Initialization stuff */

void InitPanel()
{
    int i;

    for (i = 1; i <= COLOR_RAMP_SIZE; i++) {
	WhiteRamp[i-1] = HLStoRGB(0.0, (float) i / 8.0, 0.0);
	RedRamp[i-1] = HLStoRGB(0.0, (float) i / 8.0, 0.3);
    }
}

static float Value(float m1, float m2, float hue)
{
    if (hue > 360.0) hue -= 360.0;
    else if (hue < 0.0) hue += 360.0;

    if (hue < 60.0) return (m1 + (m2 - m1) * hue / 60.0);
    else if (hue < 180.0) return m2;
    else if (hue < 240.0) return (m1 + (m2 - m1) * (240.0 - hue) / 60.0);
    else return m1;
}

static unsigned long HLStoRGB(float h, float l, float s)
{
    float r, g, b;
    float m1, m2;

    if (l < 0.5) m2 = l * (1 + s);
    else m2 = l + s - l * s;
    m1 = 2 * l - m2;
    if (s = 0.0) r = g = b = l;
    else {
	r = Value(m1, m2, h + 120);
	g = Value(m1, m2, h);
	b = Value(m1, m2, h - 120);
    }
    return ((((char) (b * 255.0)) << 16) | (((char) (g * 255.0)) << 8) |
	((char) (r * 255.0)));
}

/* Window Stuff */

void OpenPanelWindow()
{
    if (PanelPrefposition)
	prefposition(PanelXorigin, PanelXorigin + PANEL_WIDTH, 
	    PanelYorigin, PanelYorigin + PANEL_HEIGHT);
    else prefsize(PANEL_WIDTH, PANEL_HEIGHT);
    PanelWid = winopen("panel");
    RGBmode();
    if (PanelWid == -1) {
	fprintf(stderr, "no additional graphics windows are available\n");
	DoExit(-1);
    }
#ifdef DEBUG
    if (!Debug)
	doublebuffer();
#else
    doublebuffer();
#endif
    gconfig();
    getorigin(&PanelXorigin, &PanelYorigin);
    PanelPrefposition = 1;
    cpack(WhiteRamp[6]);
    clear();
#ifdef DEBUG
    if (!Debug) {
	swapbuffers();
	clear();
    }
#endif
    swapbuffers();
    clear();
    RangeRadScaleFactor = MaxRadScaleFactor - MinRadScaleFactor;
}


static void DspOnOff(ITEM *item)
{
    DrawBack(item);    
    if (*(item->val)) DrawText("On", item);
    else DrawText("Off", item);
    DrawName(item->name, &(item->rect));
}

static void SelectOnOff(ITEM *item, long dev)
{
    *(item->val) = 1 - *(item->val);
    if (item->action)
	item->action();
    DisplayScene();
    winset(PanelWid);
    item->status = SET_PUSHED(0, item->status);
    frontbuffer(1);
    item->dsp(item);
    frontbuffer(0);
}


void ClosePanelWindow()
{
    winclose(PanelWid);
    PanelWid = -1;
}

void DrawPanel()
{
    ITEM *item;

    winset(PanelWid);
#ifdef DEBUG
    if (Debug) {
	cpack(WhiteRamp[6]);
	clear();
    }
#endif
    for (item = Items; item < &Items[NUM_ITEMS]; item++) {
	item->dsp (item);
    }
    DspRadius(&RadiusItem);
#ifdef DEBUG
    if (!Debug) {
	swapbuffers();
	cpack(WhiteRamp[6]);
	clear();
    }
#else
    swapbuffers();
    cpack(WhiteRamp[6]);
    clear();
#endif
}

void DoEventsPanel(long dev, short val)
{
    ITEM *item;

    winset(PanelWid);
    switch(dev) {

      case LEFTMOUSE:
      case MIDDLEMOUSE:
	if (val) {
	    PanelButtonDown = 1;
	    if (inrect(getvaluator(MOUSEX) - PanelXorigin,
		getvaluator(MOUSEY) - PanelYorigin, RadiusItem.rect)) {
		SelectRadius(&RadiusItem, dev);
		return;
	    }
	} else {
	    PanelButtonDown = 0;
	    PrevItem = 0;
	    for (item = Items; item < &Items[NUM_ITEMS]; item++) {
		if (inrect(getvaluator(MOUSEX) - PanelXorigin,
		getvaluator(MOUSEY) - PanelYorigin, item->rect)) {
		    item->select (item, dev);
		    return;
		}
	    }
	}
	return;

      case RIGHTMOUSE:
	return;

      case REDRAW:
	reshapeviewport();
	getorigin(&PanelXorigin, &PanelYorigin);
	cpack(WhiteRamp[6]);
	clear();
	DrawPanel();
	return;

      default:
	return;
    }
}

void DoPanel(long dev, short val)
{
    ITEM *item;
    long xpos, ypos;

    if (!PanelButtonDown) return;
    winset(PanelWid);

    xpos = getvaluator(MOUSEX) - PanelXorigin;
    ypos = getvaluator(MOUSEY) - PanelYorigin;

    if (PrevItem) {
	if (inrect(xpos, ypos, PrevItem->rect))
	    return;
	PrevItem->status = (GET_PUSHED(PrevItem->status)) ?
	    SET_PUSHED(0, PrevItem->status) : SET_PUSHED(1, PrevItem->status);
	frontbuffer(1);
	PrevItem->dsp(PrevItem);
	frontbuffer(0);
	PrevItem = 0;
    }
    for (item = Items; item < &Items[NUM_ITEMS]; item++) {
	if (inrect(xpos, ypos, item->rect)) {
	    item->status = (GET_PUSHED(item->status)) ?
		SET_PUSHED(0, item->status) : SET_PUSHED(1, item->status);
	    frontbuffer(1);
	    item->dsp(item);
	    frontbuffer(0);
	    PrevItem = item;
	    break;
	}
    }
}

static void BevelRect(RECT *rect, int pushed, unsigned long *c, int width)
{
    int i;
      
    linewidth(1);
    for(i = 0; i < width; i++) {
	if (pushed) cpack(c[i]);
	else cpack(c[7-i]);
        move2i(rect->left + i, rect->bottom + i);
        draw2i(rect->left + i, rect->top - i);
        draw2i(rect->right - i, rect->top - i);
	if (pushed) cpack(c[7-i]);
	else cpack(c[i]);
        move2i(rect->right - i, rect->top - i);
        draw2i(rect->right - i, rect->bottom + i);
        draw2i(rect->left + i, rect->bottom + i);
    }   
}           

static void DrawBack(ITEM *item)
{
    unsigned long status = item->status;
    unsigned long *color_ramp = item->color_ramp;
    int frame_width;

    cpack((GET_PUSHED(status)) ? 
	((GET_HIGHLITED(status)) ? color_ramp[4] : color_ramp[3]) : 
	((GET_HIGHLITED(status)) ? color_ramp[4] : color_ramp[3]));

    rectfi(item->rect.left, item->rect.bottom, 
	item->rect.right, item->rect.top);

    if (GET_PUSHED(status)) BevelRect(&item->rect, 1, color_ramp, 4);
    else BevelRect(&item->rect, 0, color_ramp, 4);
}

static void DrawText(char text[], ITEM *item)
{
    unsigned long status = item->status;
    unsigned long *color_ramp = item->color_ramp;
    long x, y;

    if (GET_PUSHED(status)) cpack(color_ramp[7]);
    else cpack(color_ramp[0]);
    x = (item->rect.right - item->rect.left - fmgetstrwidth(FontScreen8, 
	text)) / 2 + item->rect.left;
    y = (item->rect.top - item->rect.bottom - FontScreen8Info.height) / 2 + 
	item->rect.bottom;
    fmsetfont(FontScreen8);
    cmov2i(x, y);
    fmprstr(text);
}

static void DrawName(name, rect)
    char name[];
    RECT *rect;
{
    long x, y;

    cpack(0x0);
    x = (rect->right - rect->left - fmgetstrwidth(FontScreen8, name)) / 2 + 
	rect->left;
    y = rect->top + FontScreen8Info.height / 2;
    fmsetfont(FontScreen8);
    cmov2i(x, y);
    fmprstr(name);
}

static void DspEvent(ITEM *item)
{
    DrawBack(item);
    DrawText(item->name, item);
}

static void DspSphereType(ITEM *item)
{
    DrawBack(item);    
    switch (SphereType) {
      case SPH_OCT: DrawText("oct", item); break;
      case SPH_ICOS: DrawText("icos", item); break;
      case SPH_CUBE: DrawText("cube", item); break;
      case SPH_BARY: DrawText("bary", item); break;
      case SPH_BILIN: DrawText("bilin", item); break;
      default: break;
    }
    DrawName(item->name, &(item->rect));
}

static void SelectSphereType(ITEM *item, long dev)
{
    if (dev == MIDDLEMOUSE) {
	SphereType++;
	SphereType %= NUMTESSTYPES;
    } else {
	if (SphereType == 0) SphereType = NUMTESSTYPES - 1;
	else SphereType--;
    }
    sphmode(SPH_TESS, SphereType);
    sphmode(SPH_TESS, SphereType);
    CalcInfo();

    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else 
	DisplayScene();

    winset(PanelWid);
    item->status = SET_PUSHED(0, item->status);
    frontbuffer(1);
    item->dsp(item);
    frontbuffer(0);
}

static void DspSpherePrim(ITEM *item)
{
    DrawBack(item);    
    switch (SpherePrim) {
      case SPH_MESH: DrawText("mesh", item); break;
      case SPH_POLY: DrawText("polygon", item); break;
      case SPH_LINE: DrawText("line", item); break;
      case SPH_POINT: DrawText("point", item); break;
    }
    DrawName(item->name, &(item->rect));
}

static void SelectSpherePrim(ITEM *item, long dev)
{
    if (dev == MIDDLEMOUSE) {
	SpherePrim++;
	SpherePrim %= NUMPRIMTYPES;
    } else {
	if (SpherePrim == 0)
	    SpherePrim = NUMPRIMTYPES - 1;
	else SpherePrim--;
    }
    sphmode(SPH_PRIM, SpherePrim);
    CalcInfo();
    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else 
	DisplayScene();
    winset(PanelWid);
    item->status = SET_PUSHED(0, item->status);
    frontbuffer(1);
    item->dsp(item);
    frontbuffer(0);
}

static void DspSphereDepth(ITEM *item)
{
    char str[10];

    DrawBack(item);    
    sprintf(str, "%d", SphereDepth);
    DrawText(str, item);
    DrawName(item->name, &(item->rect));
}

static void SelectSphereDepth(ITEM *item, long dev)
{
    if (dev == MIDDLEMOUSE) {
	if (SphereDepth < SPH_MAXDEPTH) SphereDepth++;
    } else if (dev == LEFTMOUSE) {
	if (SphereDepth > 1) SphereDepth--;
    }
    sphmode(SPH_DEPTH, SphereDepth);
    CalcInfo();
    DisplayScene();
    Alreadyaccumulated = 1;
    winset(PanelWid);
    item->status = SET_PUSHED(0, item->status);
    frontbuffer(1);
    item->dsp(item);
    frontbuffer(0);
}

static void SelectAtomAccBuf(ITEM *item, long dev)
{
    if (!hwAccbuf || BitmapSpheres)
    {
	winset(PanelWid);
	item->status = SET_PUSHED(0, item->status);
	frontbuffer(1);
	item->dsp(item);
	return;
    }
    AtomAccBuf = 1 - AtomAccBuf;
    if (AtomAccBuf) 
    {
	InitAccBuf();
	/* must force off Multisample */
	if (Multisample)
	{
	    Multisample = 0;
	    DoMultisample();
	}
	/* must force off Stereo */
	if (Stereo)
	{
	    Stereo = 0;
	    DoStereo();
	}
    }
    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else 
	DisplayScene();
    winset(PanelWid);
    item->status = SET_PUSHED(0, item->status);
    frontbuffer(1);
    item->dsp(item);
    msItem->dsp(msItem);
    stereoItem->dsp(stereoItem);
    frontbuffer(0);
}

static void SelectBondAccBuf(ITEM *item, long dev)
{
    if (!hwAccbuf || BitmapSpheres)
    {
	winset(PanelWid);
	item->status = SET_PUSHED(0, item->status);
	frontbuffer(1);
	item->dsp(item);
	return;
    }
    BondAccBuf = 1 - BondAccBuf;
    if (BondAccBuf)
    {
	InitAccBuf();
	/* must force off Multisample */
	if (Multisample)
	{
	    Multisample = 0;
	    DoMultisample();
	}
	/* must force off Stereo */
	if (Stereo)
	{
	    Stereo = 0;
	    DoStereo();
	}
    }
    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else 
	DisplayScene();
    winset(PanelWid);
    item->status = SET_PUSHED(0, item->status);
    frontbuffer(1);
    item->dsp(item);
    msItem->dsp(msItem);
    stereoItem->dsp(stereoItem);
    frontbuffer(0);
}

static void DspBondSmooth(ITEM *item)
{
    DrawBack(item);    
    switch (BondSmooth) {
      case SML_OFF: DrawText("Off", item); break;
      case SML_ON: DrawText("On", item); break;
      case SML_SMOOTHER: DrawText("Smoother", item); break;
      default: break;
    }
    DrawName(item->name, &(item->rect));
}

static void SelectBondSmooth(ITEM *item, long dev)
{
    static long options[] = {SML_OFF, SML_ON, SML_SMOOTHER};
    static long size = 3;
    static long index;
    static initialized = 0;

    if (!initialized) {
	int i;

	for (i = 0; i < size; i++) {
	    if (options[i] == BondSmooth) {
		index = i;
		break;
	    }
	}
	initialized = 1;
    }
    index++;
    index %= size;
    BondSmooth = options[index];
    DoLineSmooth();
    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else 
	DisplayScene();
    winset(PanelWid);
    item->status = SET_PUSHED(0, item->status);
    frontbuffer(1);
    item->dsp(item);
    frontbuffer(0);
}

static void DspBondEndCorrect(ITEM *item)
{
    DrawBack(item);    
    switch (BondEndCorrect) {
      case SML_OFF: DrawText("Off", item); break;
      case SML_END_CORRECT: DrawText("On", item); break;
      default: break;
    }
    DrawName(item->name, &(item->rect));
}

static void SelectBondEndCorrect(ITEM *item, long dev)
{
    static long options[] = {SML_OFF, SML_END_CORRECT};
    static long index;
    static initialized = 0;

    if (!initialized) {
	int i;

	if (options[0] == BondSmooth) index = 0;
	else index = 1;

	initialized = 1;
    }
    index++;
    index %= 2;
    BondEndCorrect = options[index];
    DoLineSmooth();
    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else DisplayScene();
    winset(PanelWid);
    item->status = SET_PUSHED(0, item->status);
    frontbuffer(1);
    item->dsp(item);
    frontbuffer(0);
}

static void SelectQuit(ITEM *item, long dev)
{
    DoExit(0);
}

static void SelectHelp(ITEM *item, long dev)
{
    DoHelp(0);
    winset(PanelWid);
    frontbuffer(1);
    item->dsp(item);
    frontbuffer(0);
}

static void SelectReset(ITEM *item, long dev)
{
    DoReset();
    if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
    !Alreadyaccumulated)
	DisplayAccScene();
    else DisplayScene();
    winset(PanelWid);
    item->status = SET_PUSHED(0, item->status);
    frontbuffer(1);
    item->dsp(item);
    frontbuffer(0);
}

static void DspRadius(ITEM *item)
{
    int gap, height, width;
    long vector[2];
    char str[40];
    long xpos_rad;

    cpack(0x0);
    rectfi(item->rect.left, item->rect.bottom, item->rect.right, 
	item->rect.top);
    height = item->rect.top - item->rect.bottom;
    gap = height / 4;
    width = item->rect.right - item->rect.left - 2 * gap;
    xpos_rad = item->rect.left + gap + 
	(int) ((float) width * (RadScaleFactor - MinRadScaleFactor) / 
	RangeRadScaleFactor);

    cpack(0xffffff);

    bgnline();
    vector[0] = item->rect.left + gap;
    vector[1] = item->rect.bottom + gap * 2;
    v2i(vector);
    vector[0] = item->rect.right - gap;
    v2i(vector);
    endline();

    bgnline();
    vector[1] = item->rect.bottom + gap;
    v2i(vector);
    vector[1] = item->rect.top - gap;
    v2i(vector);
    endline();

    bgnline();
    vector[0] = item->rect.left + gap;
    v2i(vector);
    vector[1] = item->rect.bottom + gap;
    v2i(vector);
    endline();

    bgnline();
    vector[0] = xpos_rad;
    v2i(vector);
    vector[1] = item->rect.top - gap;
    v2i(vector);
    endline();

    fmsetfont(FontScreen5);

    sprintf(str, "%.2f", MinRadScaleFactor);
    cmov2i(item->rect.left + 2, item->rect.bottom + 2);
    fmprstr(str);

    sprintf(str, "%.2f", MaxRadScaleFactor);
    cmov2i(item->rect.right - 2 - fmgetstrwidth(FontScreen5, str), 
	item->rect.bottom + 2);
    fmprstr(str);

    sprintf(str, "%.2f", RadScaleFactor);
    cmov2i(xpos_rad - fmgetstrwidth(FontScreen5, str) / 2, 
	item->rect.bottom + gap * 3 + 1);
    fmprstr(str);

    DrawName(item->name, &(item->rect));
}

static void SelectRadius(ITEM *item, long dev)
{
    int gap, height, width;
    long xpos;
    float fval;

    height = item->rect.top - item->rect.bottom;
    gap = height / 4;
    width = item->rect.right - item->rect.left - 2 * gap;
    while (getbutton(LEFTMOUSE)) {
	xpos = getvaluator(MOUSEX) - PanelXorigin - item->rect.left - gap;
	if (xpos < 0) xpos = 0;
	else if (xpos > width) xpos = width;
	(*(float *)(item->val)) = (MaxRadScaleFactor - MinRadScaleFactor) * 
	    (float) xpos / (float) width + MinRadScaleFactor;
	DrawPanel();
	if (item->action)
	    item->action();
    }
}

static void DspProjection(ITEM *item)
{
    DrawBack(item);    
    if (Perspective) DrawText("Perspective", item);
    else DrawText("Ortho", item);
    DrawName(item->name, &(item->rect));
}

static void DspModel(ITEM *item)
{
    DrawBack(item);    
    if (ModelId == -1) DrawText("No Model", item);
    else DrawText(Models[ModelId].name, item);
    DrawName(item->name, &(item->rect));
}

static void SelectModel(ITEM *item, long dev)
{
    if (NumModels) {
	if (dev == MIDDLEMOUSE) {
	    ModelId++;
	    ModelId %= NumModels;
	} else {
	    if (ModelId <= 0) ModelId = NumModels - 1;
	    else ModelId--;
	}
	SetGeomTitle();
	InitData();
	CalcInfo();
	if (((DispAtoms && AtomAccBuf) || (DispBonds && BondAccBuf)) &&
	!Alreadyaccumulated)
	    DisplayAccScene();
	else DisplayScene();
    }
    winset(PanelWid);
    item->status = SET_PUSHED(0, item->status);
    frontbuffer(1);
    item->dsp(item);
    frontbuffer(0);
}

static void SelectMultisample(ITEM *item, long dev)
{
    if (!hwMultisample || BitmapSpheres)
    {
	winset(PanelWid);
	item->status = SET_PUSHED(0, item->status);
	frontbuffer(1);
	item->dsp(item);
	return;
    }
    Multisample = 1 - Multisample;
    if (Multisample)
    { /* Must turn off acc buff */
	AtomAccBuf = 0;
	BondAccBuf = 0;
	/* must force off Stereo */
	if (Stereo)
	{
	    Stereo = 0;
	    DoStereo();
	}
    }
    if (item->action)
	item->action();
    DisplayScene();
    winset(PanelWid);
    item->status = SET_PUSHED(0, item->status);
    frontbuffer(1);
    item->dsp(item);
    accAtomItem->dsp(accAtomItem);
    accBondItem->dsp(accBondItem);
    stereoItem->dsp(stereoItem);
    frontbuffer(0);
}

static void SelectStereo(ITEM *item, long dev)
{
    if (!hwStereo)
    {
	winset(PanelWid);
	item->status = SET_PUSHED(0, item->status);
	frontbuffer(1);
	item->dsp(item);
	return;
    }
    Stereo = 1 - Stereo;
    if (Stereo)
    {
	{ /* Must turn off acc buff */
	    AtomAccBuf = 0;
	    BondAccBuf = 0;
	}
	/* force off Multisample - may not have enough memory */
	if (Multisample)
	{
	    Multisample = 0;
	    DoMultisample();
	}
    }
    if (item->action)
	item->action();
    DisplayScene();
    winset(PanelWid);
    item->status = SET_PUSHED(0, item->status);
    frontbuffer(1);
    item->dsp(item);
    accAtomItem->dsp(accAtomItem);
    accBondItem->dsp(accBondItem);
    msItem->dsp(msItem);
    frontbuffer(0);
}

static void SelectPerspective(ITEM *item, long dev)
{
    SelectOnOff(item, dev);
}

static void SelectBitmapSpheres(ITEM *item, long dev)
{
    if (!hwBitmapSpheres)
    {
	winset(PanelWid);
	item->status = SET_PUSHED(0, item->status);
	frontbuffer(1);
	item->dsp(item);
	return;
    }
    BitmapSpheres = 1 - BitmapSpheres;
    if (BitmapSpheres)
    {
	{ /* Must turn off acc buff */
	    AtomAccBuf = 0;
	    BondAccBuf = 0;
	}
	/* force off Multisample - may not have enough memory */
	if (Multisample)
	{
	    Multisample = 0;
	    DoMultisample();
	}
    }
    if (item->action)
	item->action();
    DisplayScene();
    winset(PanelWid);
    item->status = SET_PUSHED(0, item->status);
    frontbuffer(1);
    item->dsp(item);
    accAtomItem->dsp(accAtomItem);
    accBondItem->dsp(accBondItem);
    msItem->dsp(msItem);
    projItem->dsp(projItem);
    frontbuffer(0);
}

