#ifndef __GL_RECT_H__
#define __GL_RECT_H__

/*
**	defines for area and xy utilities
**
*/ 
#ident "$Revision: 1.3 $"

#ifndef MIN
#define MIN(a, b)	(((a) < (b)) ? (a) : (b))
#endif

#ifndef MAX
#define MAX(a, b)	(((a) > (b)) ? (a) : (b))
#ednf

#ifndef ABS
#define ABS(a)		(((a) > 0) ? (a) : -(a))
#endif

typedef struct
{
    float x;
    float y;
} XY;

typedef struct 
{
    XY origin;
    XY corner;
} Rect;

XY xy();
Rect area();
Rect rectarea();
Rect makerect();
Rect intersect();
Rect legalport();
Rect trans();
Rect grow();
Rect shrink();
Rect movebottomedge();
Rect movetopedge();
Rect moveleftedge();
Rect moverightedge();
Rect screenarea();

#endif /* __GL_RECT_H__ */
