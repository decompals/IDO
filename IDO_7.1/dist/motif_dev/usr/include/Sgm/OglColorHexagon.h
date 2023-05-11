/* 
 * (c) Copyright 1993 Silcon Graphics, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/*   $RCSfile: OglColorHexagon.h,v $ $Revision: 1.2 $ $Date: 1994/10/24 19:18:41 $ */
/*
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS. */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */
#ifndef _OglColorHexagon_h
#define _OglColorHexagon_h

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Class record constants */

typedef struct _ColorHexagonObj *ColorHexagon;


/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern ColorHexagon _SgOglColorHexagonCreate();
extern void         _SgOglColorHexagonDestroy();
extern void         _SgOglColorHexagonSetColor();
extern void         _SgOglColorHexagonGetColor();
extern void         _SgOglColorHexagonSetHSV();
extern void         _SgOglColorHexagonSetWysiwyg();

#else

extern ColorHexagon _SgOglColorHexagonCreate(Widget w, Boolean use_popup, Widget overlay_w);

extern void         _SgOglColorHexagonDestroy(ColorHexagon hexagon);
extern void         _SgOglColorHexagonSetColor(ColorHexagon hexagon, short r, 
					   short g, short b);
extern void         _SgOglColorHexagonGetColor(ColorHexagon hexagon, short *r, 
					   short *g, short *b);
extern void         _SgOglColorHexagonSetHSV(ColorHexagon hexagon, 
					 float h, float s, float v); 
extern void         _SgOglColorHexagonSetWysiwyg(ColorHexagon hexagon, 
					     Boolean wysiwyg);

#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _OglColorHexagon_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
