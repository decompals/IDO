#!make 
#

include	$(ROOT)/usr/include/make/commondefs

DEPTH=..

LC++INCS = -I$(DEPTH)/libmidifile/include

LC++DEFS = -float
C++FILES = 	main.c++ \
		mainWindow.c++ \
		file.c++ \
		controls.c++ \
		player.c++

OBJS	= $(C++FILES:.c++=.o) 

LLDOPTS = -L$(DEPTH)/libmidifile/src
LLDLIBS	= -lfpe -lgen -lvk -lvkhelp -lXpm -lSgw -lXm -lXt -lX11 -lmidifile -lmd

OPTIMIZER = -O2

TARGETS =   mfp

install: default

default: mfp

include	$(COMMONRULES)

mfp: $(OBJS)
	$(C++F) -o mfp $(OBJS) $(LDOPTS) $(LDFLAGS) 

