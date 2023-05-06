CC = CC
DEBUG = -g
C++FLAGS = $(DEBUG)  -I$(ROOT)/usr/include/
LIBS =   -L$(ROOT)/usr/lib  -lvk -lvkhelp -lXpm -lSgm -lXm -lXt -lX11 -lPW 

all: radioDemo

targets: all


radioDemo: RadioDemoDriver.o RadioGroupTest.o 
	$(CC) -o radioDemo  RadioDemoDriver.o RadioGroupTest.o $(LIBS)
radioDemo.o: RadioGroupTest.h  
clobber:
	rm -f *.o radioDemo

