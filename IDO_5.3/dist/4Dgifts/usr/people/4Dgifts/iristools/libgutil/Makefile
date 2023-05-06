#!smake

include $(ROOT)/usr/include/make/commondefs

LCOPTS	= -xansi -D__STDC__
LCDEFS	= -O
LCINCS	= -I../include -I/usr/include/gl
CFILES	= abekas.c brect.c cmyk.c colormod.c dispimg.c file.c gamma.c gendit.c \
	getapixel.c getset.c glshade.c glstuff.c hipass.c hist.c hypcurve.c \
	imgread.c izoom.c lut.c matrix.c misc.c miscgl.c percent.c psimg.c \
	pstri.c rand.c rct.c row.c setscreen.c shadow.c \
	texture.c tomesh.c track.c turtle.c unmesh.c vcolor.c vect.c
CVERSION= 

TARGETS	= libgutil.a

default: $(TARGETS)

include $(COMMONRULES)

$(TARGETS): $(OBJECTS) 
	$(AR) scr $(TARGETS) $(OBJECTS)

