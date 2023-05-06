/////////////////////////////////////////////////////////////////////
// completion. Demo the VkCompletionFiled class
////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkResource.h>
#include <Vk/VkCompletionField.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>


class SampleWindow: public VkSimpleWindow {

  public:

    SampleWindow ( const char *name );
    ~SampleWindow ();
    virtual const char* className();
};

SampleWindow::~SampleWindow ()
{
    // Empty
}

const char* SampleWindow::className() { return "SampleWindow"; }

SampleWindow::SampleWindow ( const char *name ) : VkSimpleWindow ( name )
{
    VkCompletionField *cf = new VkCompletionField("completion", mainWindowWidget());

    char *dir;

    if(theApplication->argc() >= 2)
	dir = theApplication->argv(1);    
    else
	dir = getcwd(NULL, BUFSIZ);
 

    // Read the current directory and add each file to the competion field

    DIR* dirp = opendir (dir);
    struct dirent* entry = readdir (dirp);

     while ((entry = readdir(dirp)) != NULL) 
     {
	 if (entry->d_name[0] != '.')
	     cf->add(entry->d_name);
    }
    closedir (dirp);
    
    addView(cf);
}

void main ( int argc, char **argv )
{
    VkApp        *app = new VkApp("Completion", &argc, argv);
    SampleWindow  *win = new SampleWindow("completion");

    win->show();
    app->run();
}

