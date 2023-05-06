/*
 * controls.c++
 */

#include "MainWindow.h" 
#include "Player.h"

#include <Xm/MessageB.h>
#include <X11/Xlib.h>

void MainWindow::playCallback(Widget /*w*/, 
				XtPointer clientData,
				XtPointer /*callData*/)
{
    MainWindow *obj = (MainWindow*)clientData;
    obj->thePlayer->play();
}

void MainWindow::stopCallback(Widget /*w*/, 
			    XtPointer clientData,
			    XtPointer /*callData*/)
{
    MainWindow *obj = (MainWindow*)clientData;
    obj->thePlayer->stop();
}

void MainWindow::forwardBeginCallback(Widget /*w*/, 
			    XtPointer clientData,
			    XtPointer /*callData*/)
{
    MainWindow *obj = (MainWindow*)clientData;
    obj->thePlayer->forwardBegin();
}
				
void MainWindow::forwardEndCallback(Widget /*w*/, 
			    XtPointer clientData,
			    XtPointer /*callData*/)
{
    MainWindow *obj = (MainWindow*)clientData;
    obj->thePlayer->forwardEnd();
}

void MainWindow::rewindBeginCallback(Widget /*w*/, 
			    XtPointer clientData,
			    XtPointer /*callData*/)
{
    MainWindow *obj = (MainWindow*)clientData;
    obj->thePlayer->rewindBegin();
}

void MainWindow::rewindEndCallback(Widget /*w*/, 
			    XtPointer clientData,
			    XtPointer /*callData*/)
{
    MainWindow *obj = (MainWindow*)clientData;
    obj->thePlayer->rewindEnd();
}

void MainWindow::songPosCallback(Widget /*w*/, 
			    XtPointer clientData,
			    XtPointer callData)
{
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct*)callData;
    
    MainWindow *obj = (MainWindow*)clientData;
    obj->thePlayer->setSongPos(cbs->value);
}

void MainWindow::speedCallback(Widget /*w*/, 
			    XtPointer clientData,
			    XtPointer callData)
{
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct*)callData;
    
    MainWindow *obj = (MainWindow*)clientData;
    obj->thePlayer->setSpeed(cbs->value);
}
				    


// COMMANDS from PLAYER OBJECT

void MainWindow::setSpeedDisplay(int speed)
{
    XtVaSetValues(_speedScale, 
		    XmNvalue, speed, 
		    NULL);    
}

void MainWindow::setSongPosDisplay(int position)
{
    XtVaSetValues(_songPosScale, 
		    XmNvalue, position, 
		    NULL);    
    XFlush(XtDisplay(_baseWidget));
}

void MainWindow::setFilenameDisplay(const char *name)
{
    char text[1024];
    int len;
            
    len = sprintf(text, "File:");
    if(name != NULL && *name != '\0')
	sprintf(text + len, " %s", name);
	     
    XtVaSetValues(_filenameLabel, 
		    XtVaTypedArg, XmNlabelString, XmRString,
                    text, strlen(text)+1,  
		    NULL);    
}

void MainWindow::setTextBoxString(const char* string)
{
    XmTextSetString(_textBox, (char*)string);    
}



void MainWindow::messageDialog(const char *message)
{
    Arg args[10];
    int n;
    Widget widget;
    XmString xm_str,motif_title;

    xm_str = XmStringCreateLtoR((char*)message,
			XmSTRING_DEFAULT_CHARSET);
    motif_title = XmStringCreateLtoR(_appName,
			XmSTRING_DEFAULT_CHARSET);

    n = 0;
    XtSetArg(args[n],XmNdialogTitle,motif_title);n++;
    XtSetArg(args[n],XmNmessageString,xm_str); n++;

    widget = XmCreateInformationDialog(_w,"dialog",args,n);
    XtUnmanageChild(XmMessageBoxGetChild(widget, XmDIALOG_CANCEL_BUTTON));
    XtUnmanageChild(XmMessageBoxGetChild(widget, XmDIALOG_HELP_BUTTON));

    XtManageChild(widget);
    
    XmStringFree(xm_str);
    XmStringFree(motif_title);
}
