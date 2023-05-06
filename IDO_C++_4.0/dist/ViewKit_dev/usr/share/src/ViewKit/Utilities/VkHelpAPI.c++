#include <Vk/VkHelpAPI.h>
#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkResource.h>
#include <Vk/VkFormat.h>
#include <Xm/MessageB.h>


static void postDialog(char *in_key, char *in_book)
{
    static Widget dialog = NULL;    
    XmString helpmsg;

    if (in_key && in_book)
    {
	const char *req = VkFormat("%s.%s.helpText", in_key, in_book);
	helpmsg = (XmString) VkGetResource(theApplication->baseWidget(), req, req,
					   XmRXmString,
					   "Sorry, no help available on this topic"); 	   
    }
    else if (in_key)
    {
	const char *req = VkFormat("%s.helpText", in_key);
	helpmsg = (XmString) VkGetResource(theApplication->baseWidget(), req, req,
					   XmRXmString,
					   "Sorry, no help available on this topic");      
    }
  else
  {
      helpmsg = (XmString) VkGetResource(theApplication->baseWidget(),
					 "helpText",
					 "HelpText",
					 XmRXmString,
					 "Sorry, no help available on this topic");      
  }
    
  if (!dialog)
  {
      Widget parent;

      if (theApplication->mainWindow() &&
	  theApplication->mainWindow()->baseWidget())
	  parent = theApplication->mainWindow()->baseWidget();
      else 
	  parent = theApplication->baseWidget();

      dialog = XmCreateInformationDialog(parent, "Help", NULL, 0);
      XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
      XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));      
  }

    int helpMapMode = (int) VkGetResource(theApplication->baseWidget(),
					  "helpAuthorMode",
					  "HelpAuthorMode",
					  XmRBoolean, FALSE);
    if(helpMapMode)
    {

	XmString req;

	if (in_key && in_book)	
	    req = XmStringCreateLtoR((String)VkFormat("Request = <application>.%s.%s\n\n", in_key, in_book), XmFONTLIST_DEFAULT_TAG);
	else if (in_key)	
	    req = XmStringCreateLtoR((String)VkFormat("Request = <application>.%s\n\n", in_key), XmFONTLIST_DEFAULT_TAG);
	else 
	    req = XmStringCreateLtoR((String)VkFormat("Request = <application>.helpText\n\n"), XmFONTLIST_DEFAULT_TAG);

	helpmsg = XmStringConcat(req, helpmsg);
	XmStringFree(req);
    }
  
    XtVaSetValues(dialog, XmNmessageString, helpmsg, NULL);  
    XmStringFree(helpmsg);        
    XtManageChild(dialog);
}

extern "C" {
    
int SGIHelpInit(Display *, char *, char *)
{
    return 1;
}

int SGIHelpMsg(char *in_key, char *in_book, char *)
{
    postDialog(in_key, in_book);
    return 1;
}

int SGIHelpIndexMsg(char *in_key, char *in_book)
{
    postDialog(in_key, in_book);
    return 1;
}

}




































































