/*
 * file.c++
 */
 
#include "MainWindow.h"
#include "Vk/VkFileSelectionDialog.h"

// File Selection/Loading Support


void MainWindow::doFileSelectionDialog()
{
    cout << "fileSelection()\n" << flush;

    if(theFileSelectionDialog->postAndWait( ) == VkDialogManager::OK)
    {
	// user filename available here;
	// use/copy immediately...
	thePlayer->open(theFileSelectionDialog->fileName());

//	cout << "File name: " << theFileSelectionDialog->fileName() <<
//		flush;
    }        
}

void MainWindow::fileOpenCallback(Widget, XtPointer clientData, XtPointer)
{
    MainWindow *obj = (MainWindow *)clientData;
    obj->doFileSelectionDialog();
}

