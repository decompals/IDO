
//
// headers for your application
//
#include <Xm/PushB.h>

#include <Vk/VkApp.h>
#include <Vk/VkFatalErrorDialog.h>
#include <Vk/VkSimpleWindow.h>

#include <stdio.h>

//
// header for licensing via vk
//
#include <Vk/VkFLM.h>

//
// main window class is just a simple vk app which puts up a push button
// in a window.  replace this with your actual application
//

//
// beginning of main window class
//

class MainWindow : public VkSimpleWindow {

public:

    MainWindow  (const char *);
    ~MainWindow (void);
    static void quit (Widget, XtPointer, XtPointer);
};

MainWindow::MainWindow (const char *name) : VkSimpleWindow (name) {

    XmString label = XmStringCreateLocalized("Push button to quit");
    Widget quitB = XtVaCreateManagedWidget("quit",
					   xmPushButtonWidgetClass,
					   mainWindowWidget(),
					   XmNlabelString, label,
					   NULL);
    XmStringFree(label);
    XtAddCallback(quitB, XmNactivateCallback, (XtCallbackProc) quit, this);
    addView(quitB);
}

MainWindow::~MainWindow (void) {
}

void
MainWindow::quit (Widget, XtPointer, XtPointer) {
    theApplication->quitYourself();
}

//
// end of main window class
//

void
main (int argc, char **argv)
{
    //
    // you must instantiate a VkAppobject before you instantiate a VkFLM object
    //
    VkApp*    app = new VkApp("Vktest", &argc, argv);

    //
    // instantiating a VkFLM object will provide you with licensing
    // for your application
    //
    VkFLM* flexlm = new VkFLM(&VkFLM_licenseCode, // leave as is
			      "sgifd",  // replace with your vendor daemon name
			      "dummy",  // replace with your feature name
			      "7.0",    // replace with your version number
			      10	// optional parameter;
			                // interval in minutes that you want
			                // your application to check the
			                // connection to the license server;
					// only used for network licenses
			      );

    flexlm->getLicense();		// check out the license

    //
    // an example of how you can set licensing attributes
    //
    // in this one, we are setting the LM_A_RETRY_COUNT to 5
    // (see the FlexLM Programmer's Guide for an explaination)
    //
    if (license_set_attr(LM_A_RETRY_COUNT, (LM_A_VAL_TYPE) 5)) {
	char message[512];
	sprintf(message, "set attr: %s\n", license_errstr());
	theErrorDialog->post(message);
    }

    //
    // rest of the application goes here
    //
    MainWindow *main = new MainWindow("vktest");
    main->show();
    app->run();
}
