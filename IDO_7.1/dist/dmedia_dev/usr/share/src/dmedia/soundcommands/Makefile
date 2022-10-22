
OPTIMIZER = -O2 -g3 -USF_DEBUG -UDEBUG -DNDEBUG
CFLAGS = $(OPTIMIZER) \
	 -I \
         -I$(ROOT)/usr/include \
         -float -woff 726,813,824,826,831,835 -fullwarn -wlint,-zaiv 

LDFLAGS = -L -L$(ROOT)/usr/lib

LINKTARGETS = playaifc \
              playaiff \
              recordaiff \
              aiff2aifc \
              aifc2aiff \
              aifccompress \
              aifcdecompress \
              $(NULL)

TARGETS = sfplay \
          recordaifc \
          aifcinfo \
          aifcconvert \
          $(LINKTARGETS)

SF_OBJECTS         = sf.o
SFPLAY_OBJECTS     = $(SF_OBJECTS) sfplay.o play.o al.o
RECORDAIFC_OBJECTS   = $(SF_OBJECTS) sfrecord.o al.o

SF_LLDLIBS = -laudiofile -laudioutil -ldmedia -lm -lmalloc 
SFPLAY_LLDLIBS     = $(SF_LLDLIBS) -laudio
RECORDAIFC_LLDLIBS   = $(SF_LLDLIBS) -laudio

default all: $(TARGETS)

.c.o:
	cc $(CFLAGS) -c $< -o $@

sfplay: $(SFPLAY_OBJECTS)
	cc $(LDFLAGS) -o $@ $(SFPLAY_OBJECTS) $(SFPLAY_LLDLIBS)

recordaifc: $(RECORDAIFC_OBJECTS)
	cc $(LDFLAGS) -o $@ $(RECORDAIFC_OBJECTS) $(RECORDAIFC_LLDLIBS)


playaifc: sfplay
	ln -s sfplay playaifc
playaiff: sfplay
	ln -s sfplay playaiff

recordaiff: recordaifc
	ln -s recordaifc recordaiff

aifcinfo: aifcinfo.o
	cc $(CFLAGS) -o $@ aifcinfo.o -laudiofile 

aifcconvert: aifcconvert.o
	cc $(CFLAGS) -o $@ aifcconvert.o -laudiofile 

aiff2aifc aifc2aiff aifccompress aifcdecompress: aifcconvert
	ln -s aifconvert $@

clean:
	-rm -f *.o 

clobber: clean
	-rm -f $(TARGETS)
