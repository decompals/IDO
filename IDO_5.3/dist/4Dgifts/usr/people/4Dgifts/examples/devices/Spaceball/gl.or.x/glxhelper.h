/*
 * List of drawing modes supported by GLXCreateWindow.  More than this
 * are possible with mixed model, but this is just an example.  you can
 * either expand this list (and the corresponding code in GLXCreateWindow)
 * or call the mixed model calls yourself, using GLXCreateWindow as an
 * example.
 */

typedef enum {
    GLXcolorIndexSingleBuffer,
    GLXcolorIndexDoubleBuffer,
    GLXrgbSingleBuffer,
    GLXrgbDoubleBuffer
} GLXWindowType;

extern Window GLXCreateWindow(Display*,Window,int,int,int,int,int,GLXWindowType);
