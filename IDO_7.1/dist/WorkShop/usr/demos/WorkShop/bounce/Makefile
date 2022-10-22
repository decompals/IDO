
.SUFFIXES: .o .C .c 

CC = CC
DEBUG = -g
C++FLAGS = $(DEBUG) -DFUNCPROTO 
LIBS =  -lXm -lXt -lX11 -lPW -lm

.C.o:
	$(CC) $(C++FLAGS) -c $*.C

all: bounce

OBJECTS= Actor.o\
	AddBallCmd.o\
	AddEngineCmd.o\
	Application.o\
	AskFirstCmd.o\
	BasicComponent.o\
	BounceApp.o\
	BounceClock.o\
	BounceWindow.o\
	BouncingBall.o\
	ButtonInterface.o\
	Clock.o\
	Cmd.o\
	CmdInterface.o\
	CmdList.o\
	ColorChooser.o\
	ColorModel.o\
	ColorView.o\
	ControlPanel.o\
	DialogManager.o\
	Engine.C\
	HSVView.o\
	InfoDialogManager.o\
	Main.o\
	MainWindow.o\
	MenuBar.o\
	MenuWindow.o\
	NoUndoCmd.o\
	QuestionDialogManager.o\
	QuitCmd.o\
	RGBController.o\
	RGBView.o\
	RunCmd.o\
	Stage.o\
	StepCmd.o\
	StopCmd.o\
	SwatchView.o\
	TextView.o\
	UIComponent.o\
	UndoCmd.o\
	WarnNoUndoCmd.o

bounce: $(OBJECTS)
	$(CC) -o bounce $(OBJECTS) $(LIBS)

clobber:
	rm -f *.o *~*
	rm -f bounce


