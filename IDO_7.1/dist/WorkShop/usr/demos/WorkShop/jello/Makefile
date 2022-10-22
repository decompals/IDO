GL_LIBS = -lgl
OGL_LIBS = -lGLw -lGL -lGLU -lXm -lXt -lXext
LLDLIBS = -lX11 -lm -lc
LCDEFS=-g -DEIGHTPLANE

default all: jello jello_u ogl_jello

jello: jello.c
	cc $(LCDEFS) -o jello jello.c $(GL_LIBS) $(LLDLIBS)

jello_u: jello.c
	cc $(LCDEFS) -o jello_u jello.c $(GL_LIBS) $(LLDLIBS)

ogl_jello: ogl_jello.c
	cc $(LCDEFS) -o ogl_jello ogl_jello.c $(OGL_LIBS) $(LLDLIBS)

clean:
	rm -f jello jello_u jello.o ogl_jello ogl_jello.o

clobber:
	rm -f jello jello_u jello.o ogl_jello ogl_jello.o
