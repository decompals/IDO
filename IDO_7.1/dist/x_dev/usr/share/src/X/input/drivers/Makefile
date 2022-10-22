#!smake
#
#  Sample Makefile for input device drivers
#

STATIC_OBJS = idev.o mouse.o keyboard.o pcmouse.o pckbd.o
DYNAMIC_OBJS = tablet.o dial.o sball.o hitachi.o imp.o

#
#  Set COMPILATION_MODEL=64 in Makefile, environment, or on command line
#  to make drivers for IRIX64 systems
#
#  See the file /var/sysgen/Makefile.kernio for more information about
#  defining symbols for different architectures
#
#if defined(COMPILATION_MODEL) && $(COMPILATION_MODEL) == "64"
MODEL_CDEFS=-D_K64U64
MODEL_COPTS=-64 -mips3 -TENV:kernel -TENV:misalignment=1 -OPT:space
MODEL_DYNAMIC_COPTS=
#else
MODEL_CDEFS=-D_K32U64
MODEL_COPTS=-32 -mips2 -Wc,-pic0
MODEL_DYNAMIC_COPTS=-r -d
#endif

CDEFS= $(MODEL_CDEFS) -D_KERNEL -DSTATIC=static #-DDEBUG
COPTS= $(MODEL_COPTS) -non_shared -elf -O -I.
LDOPTS=-non_shared -elf

#
#  See the mload(4) man page for more information about building
#  dynamically loadable drivers
#
DYNAMIC_COPTS= $(COPTS) $(MODEL_DYNAMIC_COPTS) -G 0 -jalr
DYNAMIC_LDOPTS= $(LDOPTS) -G 0 -r -d

STATIC_COPTS= $(COPTS) -G 8
STATIC_LDOPTS= $(LDOPTS) -G 8


all: $(STATIC_OBJS) $(DYNAMIC_OBJS)

$(STATIC_OBJS):  $$(@:.o=.c)
	$(CC) $(CDEFS) $(STATIC_COPTS) -c $?

$(DYNAMIC_OBJS):  $$(@:.o=.c)
	$(CC) $(CDEFS) $(DYNAMIC_COPTS) -c $?

clean:
	/bin/rm -f $(STATIC_OBJS) $(DYNAMIC_OBJS)
