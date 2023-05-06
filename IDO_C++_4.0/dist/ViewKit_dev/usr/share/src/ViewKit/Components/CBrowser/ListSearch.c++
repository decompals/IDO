#include <stdio.h>
#include <Vk/VkListSearch.h>
#include <Vk/VkComponent.h>
#include <Xm/Form.h>
#include <Xm/List.h>
#include <Xm/LabelG.h>
#include <Xm/Text.h>

static int count;
static Arg args[10];

class ListSearch : public VkComponent {
public:
  ListSearch(const char *name, Widget parent);
  ~ListSearch();

protected:
  void fillList();

  Widget list, label, text;
  VkListSearch *listSearch;
};

ListSearch::ListSearch(const char *name, Widget parent)
: VkComponent(name)
{
  Dimension height;

  count = 0;
  _baseWidget = XmCreateForm(parent, (char *) name, args, count);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNvisibleItemCount, 5);  count++;
  XtSetArg(args[count], XmNscrollBarDisplayPolicy, XmSTATIC);  count++;
  list = XmCreateScrolledList(_baseWidget, "list", args, count);
  XtManageChild(list);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNtopWidget, XtParent(list));  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  label = XmCreateLabelGadget(_baseWidget, "Search: ", args, count);
  XtManageChild(label);
  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNtopWidget, XtParent(list));  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNleftWidget, label);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  text = XmCreateText(_baseWidget, "text", args, count);
  XtManageChild(text);

  count = 0;
  XtSetArg(args[count], XmNheight, &height);  count++;
  XtGetValues(text, args, count);
  count = 0;
  XtSetArg(args[count], XmNheight, height);  count++;
  XtSetValues(label, args, count);

  listSearch = new VkListSearch(list, text);

  fillList();

  installDestroyHandler();
}

ListSearch::~ListSearch()
{
  delete listSearch;
}

void
ListSearch::fillList()
{
  static String strings[] = {
    "One",
    "Two",
    "Three",
    "Four",
    "Five",
    "Six",
    "Seven",
    "Eight",
    "Nine",
    "Ten",
  };
  int each, num;
  XmString *items;

  num = XtNumber(strings)*2;
  items = new XmString[num];
  for (each=0; each<num; each++) {
    items[each] = XmStringCreateSimple(strings[each % XtNumber(strings)]);
  }
  count = 0;
  XtSetArg(args[count], XmNitems, items);  count++;
  XtSetArg(args[count], XmNitemCount, num);  count++;
  XtSetValues(list, args, count);
  for (each=0; each<num; each++) {
    XmStringFree(items[each]);
  }
  delete items;
}

VkComponent *
createListSearch(char *name, Widget parent)
{
  return new ListSearch(name, parent);
}
