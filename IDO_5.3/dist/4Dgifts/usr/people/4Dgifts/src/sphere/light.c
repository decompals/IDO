/* light.c
 * -------
 *
 * $Revision: 1.16 $
 *
 */

#include <gl/gl.h>

#include "light.h"

/* material stuff */

static int CurMaterial = 0;
static float *CurMatProp = 0;

float MatBrass[] = {
    AMBIENT, 0.35, 0.25,  0.1,
    DIFFUSE, 0.65, 0.5, 0.35,
    SPECULAR, 0.0, 0.0, 0.0,
    SHININESS, 5.0,
    ALPHA, 0.2,
    LMNULL
};

float MatShinybrass[] = {
    AMBIENT, 0.25, 0.15, 0.0,
    DIFFUSE, 0.65, 0.5, 0.35,
    SPECULAR, 0.9, 0.6, 0.0,
    SHININESS, 10.0,
    ALPHA, 0.2,
    LMNULL
};

float MatPewter[] = {
    AMBIENT, 0.1, 0.1,  0.1,
    DIFFUSE, 0.6, 0.55 , 0.65,
    SPECULAR, 0.9, 0.9, 0.95,
    SHININESS, 10.0,
    ALPHA, 0.2,
    LMNULL
};

float MatSilver[] = {
    AMBIENT, 0.4, 0.4,  0.4,
    DIFFUSE, 0.3, 0.3, 0.3,
    SPECULAR, 0.9, 0.9, 0.95,
    SHININESS, 30.0,
    ALPHA, 1.0,
    LMNULL
};

float MatGold[] = {
    AMBIENT, 0.4, 0.2, 0.0,
    DIFFUSE, 0.9, 0.5, 0.0,
    SPECULAR, 0.7, 0.7, 0.0,
    SHININESS, 10.0,
    ALPHA, 0.2,
    LMNULL
};

float MatShinygold[] = {
    AMBIENT, 0.4, 0.2,  0.0,
    DIFFUSE, 0.9, 0.5, 0.0,
    SPECULAR, 0.9, 0.9, 0.0,
    SHININESS, 20.0,
    ALPHA, 0.2,
    LMNULL
};

float MatPlaster[] = {
    AMBIENT, 0.2, 0.2,  0.2,
    DIFFUSE, 0.95, 0.95, 0.95,
    SPECULAR, 0.0, 0.0, 0.0,
    SHININESS, 1.0,
    ALPHA, 0.2,
    LMNULL
};

float MatCyanplastic[] = {
    AMBIENT, 0.0, 0.1, 0.06,
    DIFFUSE, 0.0, 130.0/255, 130.0/255.0,
    SPECULAR, 128.0/255.0, 128.0/255.0, 128.0/255.0,
    SHININESS, 40.0,
    ALPHA, 1.0,
    LMNULL
};

float MatWhiteplastic[] = {
    AMBIENT, 0.0, 0.0, 0.0,
    DIFFUSE, 0.55, 0.55, 0.55,
    SPECULAR, 0.70, 0.70, 0.70,
    SHININESS, 30.0,
    ALPHA, 1.0,
    LMNULL
};

float MatGreyplastic[] = {
    AMBIENT, 0.0, 0.0, 0.0,
    DIFFUSE, 0.15, 0.15, 0.15,
    SPECULAR, 0.60, 0.60, 0.60,
    SHININESS, 30.0,
    ALPHA, 1.0,
    LMNULL
};

float MatYellowplastic[] = {
    AMBIENT, 0.0, 0.0, 0.0,
    DIFFUSE,  0.5, 0.5, 0.0, 
    SPECULAR, 0.6, 0.6, 0.5, 
    SHININESS, 30.0,
    ALPHA, 1.0,
    LMNULL
};

float MatRedplastic[] = {
    AMBIENT, 0.0, 0.0, 0.0,
    DIFFUSE, 0.5, 0.0, 0.0,
    SPECULAR, 0.70, 0.60, 0.60,
    SHININESS, 30.0,
    ALPHA, 1.0,
    LMNULL
};

float MatGreenplastic[] = {
    AMBIENT, 0.0, 0.0, 0.0,
    DIFFUSE, 0.1, 0.35, 0.1,
    SPECULAR, 0.45, 0.55, 0.45,
    SHININESS, 30.0,
    ALPHA, 1.0,
    LMNULL
};

float MatBlueplastic[] = {
    AMBIENT, 0.0, 0.0, 0.0,
    DIFFUSE, 0.12, 0.12, 0.35,
    SPECULAR, 0.45, 0.45, 0.45,
    SHININESS, 30.0,
    ALPHA, 1.0,
    LMNULL
};

float MatPinkplastic[] = {
    AMBIENT, 0.1, 0.0, 0.06, 
    DIFFUSE, 120.0/255.0, 0.0, 50.0/255.0,
    SPECULAR, 128.0/255.0, 128.0/255.0, 128.0/255.0,
    SHININESS, 40.0,
    ALPHA, 1.0,
    LMNULL
};

float MatLavpolstone[] = {
    AMBIENT, 0.1,  0.0,  0.12, 
    SPECULAR, 0.97, 0.59, 0.94, 
    DIFFUSE,  0.33, 0.04, 0.37, 
    EMISSION, 0.0, 0.0, 0.0, 
    SHININESS, 54.65, 
    ALPHA, 0.0, 
    LMNULL
};
			   
float MatBrownpolstone[] = {
    AMBIENT, 0.1, 0.1, 0.0, 
    SPECULAR, 0.627, 0.627, 0.627, 
    DIFFUSE,  0.208, 0.04, 0.0, 
    EMISSION, 0.0, 0.0, 0.0, 
    SHININESS, 60.0, 
    ALPHA, 0.0, 
    LMNULL
};
			   
float MatLapis[] = {
    AMBIENT, 0.0,  0.0, 0.9, 
    SPECULAR, 0.56, 0.54, 0.73, 
    DIFFUSE,  0.01, 0.01, 0.18, 
    EMISSION, 0.0, 0.0, 0.0, 
    SHININESS, 44.06, 
    ALPHA, 0.0, 
    LMNULL
};

float MatShinybronze[] = {
    AMBIENT, 0.1,  0.0,  0.02, 
    SPECULAR, 1.0, 0.545, 0.275, 
    DIFFUSE,  0.30, 0.12, 0.06, 
    EMISSION, 0.0, 0.0, 0.0, 
    SHININESS, 70.0, 
    ALPHA, 0.0, 
    LMNULL
};

float MatRedrubber[] = {
    AMBIENT, 0.0,  0.0,  0.05, 
    SPECULAR, 0.5, 0.5, 0.5, 
    DIFFUSE,  0.7, 0.04, 0.05, 
    EMISSION, 0.0, 0.0, 0.0, 
    SHININESS, 10.0, 
    ALPHA, 0.0, 
    LMNULL
};

float MatWall[] = {
    AMBIENT, 0.4,  0.4,  0.4, 
    SPECULAR, 0.1, 0.1, 0.1, 
    DIFFUSE,  0.6, 0.6, 0.6, 
    EMISSION, 0.0, 0.0, 0.0, 
    SHININESS, 30.0, 
    ALPHA, 0.0, 
    LMNULL
};

float MatGreenprops[] = {
    DIFFUSE, 0.10, 0.35, 0.10,
    LMNULL
};

float MatRedprops[] = {
    DIFFUSE, 0.5, 0.0, 0.0,
    LMNULL
};

float MatYellowprops[] = {
    DIFFUSE, 0.5, 0.5, 0.0,
    LMNULL
};

float MatBlueprops[] = {
    DIFFUSE, 0.12, 0.12, 0.35,
    LMNULL
};

float MatPinkprops[] = {
    DIFFUSE, 120.0/255.0, 0.0, 50.0/255.0,
    LMNULL
};

float MatCyanprops[] = {
    DIFFUSE, 0.0, 130.0/255, 130.0/255.0,
    LMNULL
};

float MatWhiteprops[] = {
    DIFFUSE, 0.55, 0.55, 0.55,
    LMNULL
};

float MatGreyprops[] = {
    DIFFUSE, 0.15, 0.15, 0.15,
    LMNULL
};

/* DefineMaterials() define material properties.
 */			   
void DefineMaterials()
{
    lmdef(DEFMATERIAL, MAT_BRASS, 0, MatBrass);
    lmdef(DEFMATERIAL, MAT_SHINYBRASS, 0, MatShinybrass);
    lmdef(DEFMATERIAL, MAT_PEWTER, 0, MatPewter);
    lmdef(DEFMATERIAL, MAT_SILVER, 0, MatSilver);
    lmdef(DEFMATERIAL, MAT_GOLD, 0, MatGold);
    lmdef(DEFMATERIAL, MAT_SHINYGOLD, 0, MatShinygold);
    lmdef(DEFMATERIAL, MAT_PLASTER, 0, MatPlaster);
    lmdef(DEFMATERIAL, MAT_REDPLASTIC, 0, MatRedplastic);
    lmdef(DEFMATERIAL, MAT_GREENPLASTIC, 0, MatGreenplastic);
    lmdef(DEFMATERIAL, MAT_BLUEPLASTIC, 0, MatBlueplastic);
    lmdef(DEFMATERIAL, MAT_PINKPLASTIC, 0, MatPinkplastic);

    lmdef(DEFMATERIAL, MAT_GREYPLASTIC, 0, MatGreyplastic);
    lmdef(DEFMATERIAL, MAT_WHITEPLASTIC, 0, MatWhiteplastic);
    lmdef(DEFMATERIAL, MAT_YELLOWPLASTIC, 0, MatYellowplastic);
    lmdef(DEFMATERIAL, MAT_CYANPLASTIC, 0, MatCyanplastic);

    lmdef(DEFMATERIAL, MAT_LAVPOLSTONE, 0, MatLavpolstone);
    lmdef(DEFMATERIAL, MAT_BROWNPOLSTONE, 0, MatBrownpolstone);
    lmdef(DEFMATERIAL, MAT_LAPIS, 0, MatLapis);
    lmdef(DEFMATERIAL, MAT_SHINYBRONZE, 0, MatShinybronze);
    lmdef(DEFMATERIAL, MAT_REDRUBBER, 0, MatRedrubber);
    lmdef(DEFMATERIAL, MAT_WALL, 0, MatWall);
}

void SetMaterial(new_mat)
    int new_mat;
{
    if (new_mat == CurMaterial)		/* if material already picked	    */
	return;
    CurMaterial = new_mat;
    lmbind(MATERIAL, CurMaterial);
}

void ChangeMaterial(new_property)
    float *new_property;
{
    if (new_property == CurMatProp)
	return;
    CurMatProp = new_property;
    lmdef(DEFMATERIAL, CurMaterial, 0, new_property);
}

/* light source stuff */

typedef struct light_map {
    int index;
    int num;
} LIGHT_MAP;

#define NUM_LIGHTS 6
static LIGHT_MAP CurLight[NUM_LIGHTS] = {
    {0, LIGHT1}, 
    {0, LIGHT2}, 
    {0, LIGHT3}, 
    {0, LIGHT4}, 
    {0, LIGHT5}, 
    {0, LIGHT6}, 
};

/* Initial lighting parameters */
float DefaultLight[] = {
    AMBIENT, 0.0, 0.0, 0.0, 
    LCOLOR, 1.0, 1.0, 1.0, 
    POSITION, 0.0, 0.0, 1.0, 0.0,
    LMNULL
};
		    
float WhiteInfLight[] = {
    AMBIENT, 0.0, 0.0, 0.0, 
    LCOLOR,   0.70, 0.70, 0.70, 
    POSITION, 50.0, -50.0, 50.0, 0.0, 
    LMNULL
};

float RedInfLight[] = {
    AMBIENT, 0.0, 0.0, 0.0, 
    LCOLOR,   0.5, 0.1, 0.1, 
    POSITION, -100.0, 0.0, 50.0, 0.0, 
    LMNULL
};

float BlueInfLight[] = {
    AMBIENT, 0.0, 0.0, 0.0, 
    LCOLOR,   0.1, 0.1, 0.5, 
    POSITION, 0.0, 0.0, 50.0, 0.0, 
    LMNULL
};

float GreenInfLight[] = {
    AMBIENT, 0.0, 0.0, 0.0, 
    LCOLOR,   0.1, 0.5, 0.1, 
    POSITION, 100.0, 0.0, 50.0, 0.0, 
    LMNULL
};

float WhiteLocalLight[] = {
    AMBIENT, 0.0, 0.0, 0.0, 
    LCOLOR,   0.75, 0.75, 0.75, 
    POSITION, 0.0, -50.0, 50.0, 1.0, 
    LMNULL
};

/* DefineLights() define lighting source.
 */
void DefineLights()
{
    lmdef(DEFLIGHT, LIGHT_DEFAULT, 0, DefaultLight);
    lmdef(DEFLIGHT, LIGHT_WHITE_INF, 0, WhiteInfLight);
    lmdef(DEFLIGHT, LIGHT_RED_INF, 0, RedInfLight);
    lmdef(DEFLIGHT, LIGHT_BLUE_INF, 0, BlueInfLight);
    lmdef(DEFLIGHT, LIGHT_GREEN_INF, 0, GreenInfLight);
    lmdef(DEFLIGHT, LIGHT_WHITE_LOCAL, 0, WhiteLocalLight);
}

void SetLight(new_light)
    int new_light;
{
    LIGHT_MAP *light;

    for (light = CurLight; light < &CurLight[NUM_LIGHTS]; light++) {
	if (new_light == light->index)	/* if light already picked	    */
	return;	
    }
    for (light = CurLight; light < &CurLight[NUM_LIGHTS]; light++) {
	if (!light->index) {
	    light->index = new_light;
	    lmbind(light->num, light->index);
	    return;
	}
    }
}

/* light model stuff */

static int CurLightModel = 0;

/* Initial lighting parameters */

float InfiniteModel[] = {
    AMBIENT, 0.3,  0.3, 0.3, 
    LOCALVIEWER, 0.0, 
    LMNULL
};

float LocalModel[] = {
    AMBIENT, 0.3,  0.3, 0.3, 
    LOCALVIEWER, 1.0, 
    ATTENUATION, 1.0, 0.0, 
    LMNULL
};

/* DefineLightModels() define lighting model.
 */
void DefineLightModels()
{
    lmdef(DEFLMODEL, MODEL_INFINITE, 0, InfiniteModel);
    lmdef(DEFLMODEL, MODEL_LOCAL, 0, LocalModel);
}

void SetLightModel(new_model)
    int new_model;
{
    if (new_model == CurLightModel)	/* if model already picked	    */
	return;
    CurLightModel = new_model;
    lmbind(LMODEL, CurLightModel);
}

