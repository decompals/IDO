include $(ROOT)/usr/include/make/commondefs

CC = CC
DEBUG = -O


C++FLAGS = $(DEBUG) 
LIBS =   -lvk -lvkhelp -lXpm -lSgm -lXm -lXt -lX11 -lPW

all: alignmentTest

targets: all

alignmentTest: driver.o AlignmentTestUI.o AlignmentTest.o 
	$(CC) -o alignmentTest  driver.o AlignmentTestUI.o AlignmentTest.o $(LIBS)


AlignmentTest.o: AlignmentTest.c++  AlignmentTest.h  AlignmentTestUI.h  
alignmentTest.o: AlignmentTest.h  AlignmentTestUI.h  

clobber:
	rm -f *.o alignmentTest


