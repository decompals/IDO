#!smake
#
# Makefile for Audio Control Panel (audiopanel)
#

include /usr/include/make/commondefs

.SUFFIXES:	.yuk
.c++.yuk:
	$(C++C) $(C++FLAGS) -F $< > $*.yuk

MOTIFLIBS = -lSgm -lXm -lXt -lX11
VKLIBS	  = -lvk -lXpm
AUDIOLIBS = -laudio
LLDLIBS   = $(VKLIBS) $(MOTIFLIBS) $(AUDIOLIBS) -lhelpmsg -lm -lmpc
LLDLIBS2  = -laudio
LC++FLAGS = -O2

OFILES  = main.o \
          InputControls.o \
          OutputControls.o \
          ApanelWindow.o \
          Corona.o \
          utility.o

OFILES2 = audiostate.o utility.o

LC++OPTS = +a1 +w +p -float -Wf,-XNl8192 -Wo,-loopunroll,1 -woff 726,813,824,826,831,835 -fullwarn -wlint,-zaiv


ALL = apanel audiostate
TARGETS = $(ALL)

default: $(TARGETS)
include $(COMMONRULES)

all: $(ALL)

default $(COMMONTAGS): $(ALL)

apanel: $(OFILES)
	$(C++F) $(OFILES) -o $@ $(LDFLAGS) $(LLDLIBS)

audiostate: $(OFILES2)
	$(C++F) $(OFILES2) -o $@ $(LDFLAGS) $(LLDLIBS2)


InputControls.o: InputControls.C 
	$(C++F) -c InputControls.C 

OutputControls.o: OutputControls.C 
	$(C++F) -c OutputControls.C 

main.o: main.C 
	$(C++F) -c main.C 

ApanelWindow.o: ApanelWindow.C 
	$(C++F) -c ApanelWindow.C 

Corona.o: Corona.C 
	$(C++F) -c Corona.C 

utility.o: utility.C 
	$(C++F) -c utility.C 
