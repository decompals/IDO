#
# Pmake system makefile for IRIX
#
# $Revision: 1.11 $
#
# The following variables specify the nature of the system on which Pmake 
# is running. These names may later be used in #if expressions for conditional 
# reading of the enclosed portion of the makefile.

IRIX		= IRIS-4D operating system
SYSV		= System V
SVR4		= System V Release 4
unix		= It runs UNIX
sgi		= Silicon Graphics system
mips		= Machine has a MIPS CPU
mipseb		= Machine has a big-endian MIPS CPU

.SUFFIXES : .out .a .b .o .u .c .c++ .C .p .f .e .r .pl1 .pli \
		.y .Y .l .L .s .sh .h .i \
		.c,v .c++,v .f,v .p,v .pl1,v .pli,v .y,v .l,v .s,v .sh,v .h,v \
		.Y,v .L,v .C,v
.INCLUDES : .h
.LIBS: .a .b
.NULL: .out

CC		= cc
CFLAGS		= -O
C++		= CC
C++C		= CC
C++FLAGS	= -O
AS		= as
ASFLAGS		=
PC		= pc
PFLAGS		= -O
PL1		= pl1
PL1FLAGS	= -O
FC		= f77
FFLAGS		= -O
RFLAGS		=
EFLAGS		=
MKF2C		= mkf2c
F2CFLAGS	=
YACC		= yacc
YFLAGS		=
LEX		= lex
LFLAGS		=
LD		= ld
LDFLAGS		=
LOADLIBES	=
CO		= co
COFLAGS		=
CI		= ci
CIFLAGS		=
AR		= ar
ARFLAGS		= crl
INSTALL		= install
EXTCENTRY	= extcentry


.c,v.c .c++,v.c++ .y,v.y .l,v.l .s,v.s .f,v.f .p,v.p .pl1,v.pl1 .pli,v.pli \
 .sh,v.sh .h,v.h .C,v.C .L,v.L .Y,v.Y :
	$(CO) $(COFLAGS) $(.IMPSRC) $(.TARGET)

.s.out .c.out .o.out :
	$(CC) $(CFLAGS) $(.IMPSRC) $(LDFLAGS) -o $(.TARGET)

.s.o :
	$(AS) $(ASFLAGS) -o $(.TARGET) $(.IMPSRC)


.c.o :
	$(CC) $(CFLAGS) -c $(.IMPSRC)
.c.s :
	$(CC) $(CFLAGS) -S $(.IMPSRC)
.c.i :
	$(CC) $(CFLAGS) -E $(.IMPSRC) > $(.PREFIX).i
.c.u :
	$(CC) $(CFLAGS) -j $(.IMPSRC)


.C.out .c++.out:
	$(C++) $(C++FLAGS) $(.IMPSRC) $(LDFLAGS) -o $(.TARGET)
.C.out .c++.o :
	$(C++) $(C++FLAGS) -c $(.IMPSRC)
.C.out .c++.s :
	$(C++) $(C++FLAGS) -S $(.IMPSRC)
.C.out .c++.i :
	$(C++) $(C++FLAGS) -E $(.IMPSRC) > $(.PREFIX).i


.p.o :
	$(PC) $(PFLAGS) -c $(.IMPSRC)
.p.u :
	$(PC) $(PFLAGS) -j $(.IMPSRC)
.p.out :
	$(PC) $(PFLAGS) $(.IMPSRC) $(LDFLAGS) -o $(.TARGET)


.pl1.o .pli.o :
	$(PL1) $(PL1FLAGS) -c $(.IMPSRC)
.pl1.u .pli.u :
	$(PL1) $(PL1FLAGS) -j $(.IMPSRC)
.pl1.out .pli.out :
	$(PL1) $(PL1FLAGS) $(.IMPSRC) $(LDFLAGS) -o $(.TARGET)


.f.o :
	$(FC) $(FFLAGS) -c $(.IMPSRC)
.f.u :
	$(FC) $(FFLAGS) -j $(.IMPSRC)
.e.o .r.o :
	$(FC) $(RFLAGS) $(EFLAGS) $(FFLAGS) -c $(.IMPSRC)
.f.out .r.out .e.out :
	$(FC) $(EFLAGS) $(RFLAGS) $(FFLAGS) $(.IMPSRC) $(LDFLAGS) -o $(.TARGET)
	-rm -f $(.PREFIX).o


.Y.C .y.c :
	$(YACC) $(YFLAGS) $(.IMPSRC)
	mv -f y.tab.c $(.TARGET)
.y.o :
	$(YACC) $(YFLAGS) $(.IMPSRC)
	$(CC) $(YACCMKDEPFLAGS) $(CFLAGS) -c y.tab.c
	rm y.tab.c
	mv -f y.tab.o $(.TARGET)
.y.out :
	$(YACC) $(YFLAGS) $(.IMPSRC)
	$(CC) $(CFLAGS) y.tab.c $(LDFLAGS) -ly -o $(.TARGET)
	rm y.tab.c
.Y.o :
	$(YACC) $(YFLAGS) $(.IMPSRC)
	$(C++C) $(YACCMKDEPFLAGS) $(C++FLAGS) -c y.tab.c
	rm y.tab.c
	mv -f y.tab.o $(.TARGET)


.L.C .l.c :
	$(LEX) $(LFLAGS) $(.IMPSRC)
	mv -f lex.yy.c $(.TARGET)
.l.o :
	$(LEX) $(LFLAGS) $(.IMPSRC)
	$(CC) $(LEXMKDEPFLAGS) $(CFLAGS) -c lex.yy.c
	rm lex.yy.c
	mv -f lex.yy.o $(.TARGET)
.l.out :
	$(LEX) $(LFLAGS) $(.IMPSRC)
	$(CC) $(CFLAGS) lex.yy.c $(LDFLAGS) -ll -o $(.TARGET)
	rm lex.yy.c
.L.o :
	$(LEX) $(LFLAGS) $(.IMPSRC)
	$(C++C) $(LEXMKDEPFLAGS) $(C++FLAGS) -c lex.yy.c
	rm lex.yy.c
	mv -f lex.yy.o $(.TARGET)


.sh.out :
	rm -f $(.TARGET)
	cp $(.IMPSRC) $(.TARGET); chmod a+x,u+w  $(.TARGET)
