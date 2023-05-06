////////////////////////////////////////////////////////////////////////////////
///////   Copyright 1992, Silicon Graphics, Inc.  All Rights Reserved.   ///////
//                                                                            //
// This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;     //
// the contents of this file may not be disclosed to third parties, copied    //
// or duplicated in any form, in whole or in part, without the prior written  //
// permission of Silicon Graphics, Inc.                                       //
//                                                                            //
// RESTRICTED RIGHTS LEGEND:                                                  //
// Use,duplication or disclosure by the Government is subject to restrictions //
// as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data     //
// and Computer Software clause at DFARS 252.227-7013, and/or in similar or   //
// successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -    //
// rights reserved under the Copyright Laws of the United States.             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
#include <stdio.h>
#include <Vk/VkWindow.h>
#include <Vk/VkApp.h>
#include <Vk/VkFetchMenu.h>
#include <Vk/VkMenuBar.h>

#include <Xm/Form.h>

class SampleWindow : public VkWindow {
public:
  SampleWindow(const char *name);
  ~SampleWindow();

protected:
  virtual Widget setUpInterface(Widget parent);

};

void
execute_stub(Widget, XtPointer client_data, XtPointer)
{
  VkFetchMenuData *record = (VkFetchMenuData *) client_data;

  fprintf(stderr, "Execute %s for object 0x%x\n", record->str,
	  (SampleWindow *) record->data);
}

SampleWindow::SampleWindow(const char *name)
: VkWindow(name)
{
}

SampleWindow::~SampleWindow()
{
}

Widget
SampleWindow::setUpInterface(Widget parent)
{
  Arg args[10];
  int count;
  VkFetchMenu *fm;
  VkMenuDesc *vmd;
  VkSubMenu *m;
  Widget form;

  setMenuBar(new VkMenuBar());
  fm = new VkFetchMenu(_baseWidget);
  vmd = fm->fetchMenu("ViewCommands",
		      execute_stub,
		      (XtPointer) this);
  delete fm;
  m = menu()->addSubmenu("viewsMenu", vmd);

  count = 0;
  XtSetArg(args[count], XmNwidth, 400);  count++;
  XtSetArg(args[count], XmNheight, 200);  count++;
  form = XmCreateForm(parent, "form", args, count);
  XtManageChild(form);

  return form;
}

void
main(int argc, char **argv)
{
  VkApp *app = new VkApp("Fetchmenu", &argc, argv);

  SampleWindow *sw = new SampleWindow("fetchmenu");

  sw->show();

  app->run();
}
