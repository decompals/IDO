#include <stdio.h>
#include "CBrowser.h"
#include "List.h"
#include "Area.h"
#include "Overview.h"
#include <Vk/VkApp.h>
#include <Vk/VkMenuBar.h>
#include <Vk/VkSubMenu.h>

#include <Xm/Form.h>
#include <Xm/Frame.h>

static int count;
static Arg args[10];

Cbrowser::Cbrowser(const char *docName)
: VkWindow(docName)
{
  coverview = NULL;
}

Cbrowser::~Cbrowser()
{
}

const char *
Cbrowser::className()
{
  return "CBrowser";
}

/**********************************************************************/

Widget
Cbrowser::setUpInterface(Widget parent)
{
  Widget dummy, form;

  setMenuBar(new VkMenuBar());
  fileMenu(menu());

  count = 0;
  dummy = XmCreateForm(parent, "dummy", args, count);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  form = XmCreateForm(dummy, "form", args, count);

  clist = new Clist("clist", form);
  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  XtSetValues(clist->baseWidget(), args, count);
  clist->show();

  carea = new Carea("carea", form);
  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNleftWidget, clist->baseWidget());  count++;
  XtSetValues(carea->baseWidget(), args, count);
  carea->show();

  clist->setArea(carea);

  XtManageChild(form);
  XtManageChild(dummy);
  return dummy;
}

void
Cbrowser::handleWmDeleteMessage()
{
  theApplication->quitYourself();
}

void
Cbrowser::fileMenu(VkMenu *parent)
{
  VkSubMenu *menu =   parent->addSubmenu("fileMenu");
  
  menu->addAction("showOverviewItem",
		  Cbrowser::show_overview,
		  (XtPointer) this);
  menu->addSeparator();
  menu->addAction("quitMenuItem",
		  Cbrowser::quit_menu,
		  (XtPointer) this);
}

void
Cbrowser::showOverview()
{
  theApplication->busy();
  if (!coverview) {
    coverview = new Coverview("coverview");
  }
  coverview->show();
  coverview->open();
  coverview->raise();
  theApplication->notBusy();
}

/**********************************************************************/

void
Cbrowser::show_overview(Widget, XtPointer client_data, XtPointer)
{
  Cbrowser *obj = (Cbrowser *) client_data;

  obj->showOverview();
}

void
Cbrowser::quit_menu(Widget, XtPointer, XtPointer)
{
  theApplication->quitYourself();
}
