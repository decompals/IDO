/*

    MainWindow.h  -  MainWindow Class Header

*/

#ifndef _MAINWINDOW_H_
#define _MAINWINDOW_H_

#include <iostream.h>

// ViewKit include files
#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkMenu.h>
#include <Vk/VkOutline.h>

// Motif include files
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/ArrowB.h>
#include <Xm/PushB.h>
#include <Xm/Scale.h>
#include <Xm/Text.h>

// our own special defs include file
#include "defs.h"
#include "Player.h"

class Player;

class MainWindow: public VkWindow {

protected:
    Widget setUpInterface(Widget parent);

private:

    Player*	thePlayer;
    char	*_appName;
        
    static String _defaultResources[];
    static void sampleCallback(Widget,XtPointer,XtPointer);
    static void quitCallback(Widget,XtPointer,XtPointer);
    static void fileOpenCallback(Widget, XtPointer, XtPointer);  

    // 
    void sample();
    void quit();
    void doFileSelectionDialog();

    // transport/player control callbacks
    static void rewindBeginCallback(Widget,XtPointer,XtPointer);
    static void rewindEndCallback(Widget,XtPointer,XtPointer);
    static void forwardBeginCallback(Widget,XtPointer,XtPointer);
    static void forwardEndCallback(Widget,XtPointer,XtPointer);
    static void playCallback(Widget,XtPointer,XtPointer);
    static void stopCallback(Widget,XtPointer,XtPointer);
    static void songPosCallback(Widget,XtPointer,XtPointer);
    static void speedCallback(Widget,XtPointer,XtPointer);
        
    static VkMenuDesc subMenu[];
    static VkMenuDesc sampleMenuPane[];
    static VkMenuDesc fileMenuPane[];
    static VkMenuDesc mainMenuPane[];

// UI Widgets
    Widget	_speedScale;
    Widget	_songPosScale;
    Widget	_textBox;
    Widget	_filenameLabel;
    
public:
    MainWindow(const char *name);
    ~MainWindow();
    virtual const char *className();

    // calls from Player object to UI

    void setFilenameDisplay(const char *name);
    void setSpeedDisplay(int speed);
    void setSongPosDisplay(int position);
    void setTextBoxString(const char *string);
    void messageDialog(const char *message);
};

#endif
