#ident 4Dgifts/src/librmt/Makefile.full: $Revision: 1.1 $

include $(ROOT)/usr/include/make/commondefs

LCDEFS=-DUSG5 -UBSD -UBSD4_2

TARGETS	= librmt.a

CFILES= rmtaccess.c rmtclose.c rmtcreat.c \
	rmtfstat.c rmtioctl.c rmtisatty.c rmtlseek.c rmtread.c \
	rmtwrite.c isrmt.c rmtopen.c rmtcommand.c rmtdev.c \
	rmtstatus.c rmtabort.c

default: $(TARGETS)

include $(COMMONRULES)

$(TARGETS): $(OBJECTS)
	$(AR) cruv $@ $(OBJECTS)

