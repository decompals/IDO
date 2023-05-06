/*
 * main.c++	-   main application module
 */
 
 
#include <Vk/VkApp.h>
#include "MainWindow.h"

void main(int argc, char **argv)
{
    VkApp	*app = new VkApp("Mfp", &argc, argv);
    MainWindow	*win = new MainWindow("MIDI File Player 1.0");
    
    // Application specific code
	;
	;
	


    // display the main window and 
    // run the application event loop
    	
    win->show(); 
    
#ifdef CONTROLS_DEMO    
    win->setFilenameDisplay("flaky.smf");
    win->setSongPosDisplay(500);
    win->setSpeedDisplay(120);
    win->setTextBoxString("Some text in the\ntext box.....");
    win->messageDialog("Hello World!");
#endif
       
    app->run();
}
