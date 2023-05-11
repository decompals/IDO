/*
 * $XConsortium: TextI.h,v 1.1 90/04/30 17:46:56 converse Exp $
 */


/***********************************************************
Copyright 1990 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#ifndef _XawTextI_h
#define _XawTextI_h

typedef long XawTextPosition;

typedef enum { XawtextScrollNever,
	       XawtextScrollWhenNeeded, XawtextScrollAlways} XawTextScrollMode;

typedef enum { XawtextWrapNever, 
	       XawtextWrapLine, XawtextWrapWord} XawTextWrapMode;

typedef enum { XawtextResizeNever, XawtextResizeWidth,
	       XawtextResizeHeight, XawtextResizeBoth} XawTextResizeMode;

typedef enum {XawsdLeft, XawsdRight} XawTextScanDirection;
typedef enum {XawtextRead, XawtextAppend, XawtextEdit} XawTextEditType;
typedef enum {XawselectNull, XawselectPosition, XawselectChar, XawselectWord,
    XawselectLine, XawselectParagraph, XawselectAll} XawTextSelectType;

typedef struct {
    int  firstPos;
    int  length;
    char *ptr;
    Atom format;
    } XawTextBlock, *XawTextBlockPtr; 

#endif /* _XawTextI_h */
