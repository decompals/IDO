/* $XConsortium: XKBstr.h,v 1.4 93/09/28 20:16:45 rws Exp $ */
/************************************************************
Copyright (c) 1993 by Silicon Graphics Computer Systems, Inc.

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of Silicon Graphics not be 
used in advertising or publicity pertaining to distribution 
of the software without specific prior written permission.
Silicon Graphics makes no representation about the suitability 
of this software for any purpose. It is provided "as is"
without any express or implied warranty.

SILICON GRAPHICS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS 
SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY 
AND FITNESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SILICON
GRAPHICS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, 
DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE 
OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
THE USE OR PERFORMANCE OF THIS SOFTWARE.

********************************************************/

#ifndef _XKBGEOM_H_
#define	_XKBGEOM_H_

typedef	struct _XkbProperty {
	char	*name;
	char	*value;
} XkbPropertyRec,*XkbPropertyPtr;

typedef struct _XkbRGBColor {
	unsigned char	type;
	unsigned char	index;
	unsigned short	red;
	unsigned short	green;
	unsigned short	blue;
} XkbRGBColorRec,*XkbRGBColorPtr;

typedef struct _XkbNamedColor {
	unsigned char 	type;
	unsigned char	index;
	char *		name;
} XkbNamedColorRec,*XkbNamedColorPtr;

typedef union _XkbColor {
	unsigned char		type;
	struct {
	    unsigned char	type;
	    unsigned char	index;
	} any;
	XkbNamedColorRec	named;
	XkbRGBColorRec		rgb;
} XkbColorRec,*XkbColorPtr;

typedef	struct _XkbPoint {
	unsigned short	x;
	unsigned short	y;
} XkbPointRec, *XkbPointPtr;

typedef struct _XkbOutline {
	unsigned short	num_points;
	unsigned short	sz_points;
	unsigned short	corner_radius;
	XkbPointPtr	points;
} XkbOutlineRec, *XkbOutlinePtr;

typedef struct _XkbShape {
	XkbKeyNameRec	 name;
	unsigned short	 num_outlines;
	unsigned short	 sz_outlines;
	XkbOutlinePtr	 outlines;
	XkbOutlinePtr	 approximation;
	XkbOutlinePtr	 primary;
} XkbShapeRec, *XkbShapePtr;

typedef struct _XkbShapeDoodad {
	unsigned char	 type;
	unsigned char	 priority;
	unsigned short	 top;
	unsigned short	 left;
	unsigned short	 angle;
	XkbColorPtr	 color;
	XkbShapePtr	 shape;
} XkbShapeDoodadRec, *XkbShapeDoodadPtr;

typedef struct _XkbImageDoodad {
	unsigned char	 type;
	unsigned char	 priority;
	unsigned short	 top;
	unsigned short	 left;
	unsigned short	 width;
	unsigned short	 height;
	unsigned short	 pixel_height;
	unsigned short	 pixel_width;
	unsigned char	*image;
} XkbImageDoodadRec, *XkbImageDoodadPtr;

typedef struct _XkbTextDoodad {
	unsigned char	 type;
	unsigned char	 priority;
	unsigned short	 baseline;
	unsigned short	 left;
	unsigned short	 angle;
	XkbColorPtr	 color;
	char *		 text;
	char *		 font;
} XkbTextDoodadRec, *XkbTextDoodadPtr;

typedef struct _XkbIndicatorDoodad {
	unsigned char	 type;
	unsigned char	 priority;
	unsigned short	 top;
	unsigned short	 left;
	unsigned char	 width;
	unsigned char	 height;
	XkbColorPtr	 on_color;
	XkbColorPtr	 off_color;
} XkbIndicatorDoodadRec, *XkbIndicatorDoodadPtr;

typedef struct _XkbAnyDoodad {
	unsigned char	 type;
	unsigned char	 priority;
	unsigned short	 top;
	unsigned short	 left;
} XkbAnyDoodadRec, *XkbAnyDoodadPtr;

typedef union _XkbDoodad {
	XkbAnyDoodadRec		any;
	XkbShapeDoodadRec	shape;
	XkbImageDoodadRec	image;
	XkbTextDoodadRec	text;
	XkbIndicatorDoodadRec	indicator;
} XkbDoodadRec, *XkbDoodadPtr;

#define	XkbOutlineDoodad	0
#define	XkbFilledDoodad		1
#define	XkbImageDoodad		2
#define	XkbTextDoodad		3
#define	XkbIndicatorDoodad	4

typedef struct _XkbKey {
	unsigned char	 name[XkbKeyNameLength];
	short		 gap;
	XkbShapePtr	 shape;
	XkbColorPtr	 color;
} XkbKeyRec, *XkbKeyPtr;

typedef struct _XkbRow {
	unsigned short	 top;
	unsigned short	 left;
	unsigned short	 num_keys;
	unsigned short	 sz_keys;
	int		 vertical;
	XkbKeyPtr	 keys;
} XkbRowRec, *XkbRowPtr;

typedef struct _XkbSection {
	Atom		 name;
	unsigned short	 top;
	unsigned short	 left;
	unsigned short	 width;
	unsigned short	 height;
	unsigned short	 angle;
	unsigned char	 num_rows;
	unsigned char	 num_doodads;
	unsigned char	 sz_rows;
	unsigned char	 sz_doodads;
	XkbRowPtr	 rows;
	XkbDoodadPtr	 doodads;
} XkbSectionRec, *XkbSectionPtr;

typedef struct _XkbGeometry {
	Atom		 name;
	unsigned short	 width;
	unsigned short	 height;
	unsigned short	 sz_properties;
	unsigned short	 sz_colors;
	unsigned short	 sz_shapes;
	unsigned short   sz_sections;
	unsigned short	 sz_doodads;
	unsigned short	 num_properties;
	unsigned short	 num_colors;
	unsigned short	 num_shapes;
	unsigned short	 num_sections;
	unsigned short	 num_doodads;
	XkbPropertyPtr	 properties;
	XkbColorPtr	 all_colors;
	XkbShapePtr	 all_shapes;
	XkbSectionPtr	 all_sections;
	XkbDoodadPtr	 all_doodads;
} XkbGeometryRec;

#define	XkbGeomPropertiesMask	(1<<0)
#define	XkbGeomPointsMask	(1<<1)
#define	XkbGeomColorsMask	(1<<2)
#define	XkbGeomOutlinesMask	(1<<3)
#define	XkbGeomShapesMask	(1<<4)
#define	XkbGeomSectionsMask	(1<<5)
#define	XkbGeomDoodadsMask	(1<<6)
#define	XkbGeomAllMask		(0x7f)

typedef struct _XkbGeometrySizes {
	unsigned	which;
	unsigned short	num_properties;
	unsigned short	num_colors;
	unsigned short	num_shapes;
	unsigned short	num_sections;
	unsigned short	num_doodads;
} XkbGeometrySizesRec,*XkbGeometrySizesPtr;

_XFUNCPROTOBEGIN

extern	XkbPropertyPtr
XkbAddGeomProperty(
#if NeedFunctionPrototypes
    XkbGeometryPtr	/* geom */,
    char *		/* name */,
    char *		/* value */
#endif
);

extern	XkbColorPtr
XkbAddNamedColor(
#if NeedFunctionPrototypes
    XkbGeometryPtr	/* geom */,
    char *		/* color */
#endif
);

extern XkbColorPtr
XkbAddRGBColor(
#if NeedFunctionPrototypes
    XkbGeometryPtr	/* geom */,
    int			/* red */,
    int			/* green */,
    int			/* blue */
#endif
);

extern	XkbOutlinePtr
XkbAddGeomOutline(
#if NeedFunctionPrototypes
    XkbGeometryPtr	/* geom */,
    XkbShapePtr		/* shape */,
    int			/* sz_points */
#endif
);

extern XkbShapePtr
XkbAddGeomShape(
#if NeedFunctionPrototypes
    XkbGeometryPtr	/* geom */,
    XkbKeyNamePtr	/* name */,
    int			/* sz_outlines */
#endif
);

extern XkbKeyPtr
XkbAddGeomKey(
#if NeedFunctionPrototypes
    XkbGeometryPtr	/* geom */,
    XkbRowPtr		/* row */
#endif
);

extern XkbRowPtr
XkbAddGeomRow(
#if NeedFunctionPrototypes
    XkbGeometryPtr	/* geom */,
    XkbSectionPtr	/* section */,
    int			/* sz_keys */
#endif
);

extern XkbSectionPtr
XkbAddGeomSection(
#if NeedFunctionPrototypes
    XkbGeometryPtr	/* geom */,
    Atom		/* name */,
    int			/* sz_rows */
#endif
);

extern void
XkbFreeGeomProperties(
#if NeedFunctionPrototypes
    XkbGeometryPtr	/* geom */,
    int			/* first */,
    int			/* count */
#endif
);

extern void
XkbFreeGeomShapes(
#if NeedFunctionPrototypes
    XkbGeometryPtr	/* geom */,
    int			/* first */,
    int			/* count */
#endif
);

extern void
XkbFreeGeomSections(
#if NeedFunctionPrototypes
    XkbGeometryPtr	/* geom */,
    int			/* first */,
    int			/* count */
#endif
);

extern void
XkbFreeGeometry(
#if NeedFunctionPrototypes
    XkbDescPtr		/* xkb */,
    XkbGeometrySizesPtr	/* sizes */
#endif
);

extern	Bool
XkbAllocGeometry(
#if NeedFunctionPrototypes
	XkbDescPtr		/* xkb */,
	XkbGeometrySizesPtr	/* sizes */
#endif
);

_XFUNCPROTOEND

#endif /* _XKBSTR_H_ */
