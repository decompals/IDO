//////////////////////////////////////////////////
// fileSBdialog.c++ -- 
//////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkFileSelectionDialog.h>

#include <iostream.h>

void main( int argc, char** argv )
{
    VkApp myApp( "MyApp", &argc, argv );

    if(theFileSelectionDialog->postAndWait( ) == VkDialogManager::OK)
	cout << "File name: " << theFileSelectionDialog->fileName() << '\n' << flush;
    else
	cout << "User canceled.\n" << flush;

    if(theFileSelectionDialog->postAndWait(NULL, "helpText") == VkDialogManager::OK)
	cout << "File name: " << theFileSelectionDialog->fileName() << '\n' << flush;
    else
	cout << "User canceled.\n" << flush;
}

