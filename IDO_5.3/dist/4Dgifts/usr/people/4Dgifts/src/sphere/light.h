/* light.h
 * -------
 *
 * $Revision: 1.5 $
 *
 */

#define MAT_GREYPLASTIC	    1
#define MAT_CYANPLASTIC	    2
#define MAT_REDPLASTIC	    3
#define MAT_BLUEPLASTIC	    4
#define MAT_YELLOWPLASTIC   5
#define MAT_PINKPLASTIC	    6
#define MAT_GREENPLASTIC    7
#define MAT_WHITEPLASTIC    8
#define MAT_BRASS	    9
#define MAT_SHINYBRASS	    10
#define MAT_PEWTER	    11
#define MAT_SILVER	    12
#define MAT_GOLD	    13
#define MAT_SHINYGOLD	    14
#define MAT_PLASTER	    15
#define MAT_LAVPOLSTONE	    16
#define MAT_BROWNPOLSTONE   17
#define MAT_LAPIS	    18
#define MAT_SHINYBRONZE	    19
#define MAT_REDRUBBER	    20
#define MAT_WALL	    21

#define MODEL_INFINITE	    1
#define MODEL_LOCAL	    2

#define LIGHT_DEFAULT	    1
#define LIGHT_WHITE_INF	    2
#define LIGHT_RED_INF	    3
#define LIGHT_BLUE_INF	    4
#define LIGHT_GREEN_INF	    5
#define LIGHT_WHITE_LOCAL   6

void DefineMaterials();
void DefineLightModels();
void DefineLights();

void SetMaterial();
void ChangeMaterial();
void SetLightModel();
void SetLight();

extern float MatGreenprops[], MatRedprops[], MatYellowprops[], 
    MatBlueprops[], MatPinkprops[], MatCyanprops[], MatWhiteprops[], 
    MatGreyprops[];

