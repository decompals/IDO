#include <stdio.h>
#include <malloc.h>
#include "List.h"
#include "Area.h"
#include "Data.h"
#include <Vk/VkApp.h>

#include <Xm/Form.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>

static int count;
static Arg args[10];

Clist::Clist(const char *name, Widget parent)
: VkComponent(name)
{
  carea = NULL;
  lastComponent = NULL;

  count = 0;
  _baseWidget = XmCreateForm(parent, (char *) name, args, count);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  label = XmCreateLabelGadget(_baseWidget, "label", args, count);
  XtManageChild(label);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNtopWidget, label);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  list = XmCreateScrolledList(_baseWidget, "list", args, count);
  XtManageChild(list);
  XtAddCallback(list, XmNdefaultActionCallback,
		Clist::list_select, (XtPointer) this);
  XtAddCallback(list, XmNsingleSelectionCallback,
		Clist::list_select, (XtPointer) this);
  XtAddCallback(list, XmNbrowseSelectionCallback,
		Clist::list_select, (XtPointer) this);

  displayComponents(list);

  installDestroyHandler();
}

Clist::~Clist()
{
  if (lastComponent) {
    delete lastComponent;
  }
}

const char *
Clist::className()
{
  return "CList";
}

/**********************************************************************/

void
Clist::displayComponents(Widget list)
{
  XmString *strings;
  int num, each;

  num = componentInfoSize;
  strings = new XmString[num];
  for (each=0; each<num; each++) {
    strings[each] = XmStringCreateSimple(componentInfo[each].name);
  }
  count = 0;
  XtSetArg(args[count], XmNitems, strings);  count++;
  XtSetArg(args[count], XmNitemCount, num);  count++;
  XtSetValues(list, args, count);
  for (each=0; each<num; each++) {
    XmStringFree(strings[each]);
  }
  delete strings;
}

void
Clist::listSelect(int which)
{
  char str[256];

  theApplication->busy();
  if (lastComponent) {
    delete lastComponent;
  }
  sprintf(str, "%s:", componentInfo[which].name);
  carea->setLabel(str);
  lastComponent = (*(componentInfo[which].proc))("component",
						 carea->getDisplayWidget());
  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetValues(lastComponent->baseWidget(), args, count);
  lastComponent->show();
  theApplication->notBusy();
}

/**********************************************************************/

void
Clist::list_select(Widget, XtPointer clientData, XtPointer callData)
{
  Clist *obj = (Clist *) clientData;
  XmListCallbackStruct *cb = (XmListCallbackStruct *) callData;

  obj->listSelect(cb->item_position-1);
}
