#include <stdio.h>
#include <Vk/VkDeck.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>

static int count;
static Arg args[10];

extern VkComponent* createTextIO(char *, Widget);
extern VkComponent* createScroll(char *, Widget);
extern VkComponent* createListSearch(char *, Widget);

class Deck : public VkComponent {
public:
  Deck(const char *name, Widget parent);
  ~Deck();

protected:
  void popActivate();

  static void pop_activate(Widget w, XtPointer clientData,
			   XtPointer callData);

  Widget pop;
  VkComponent *textio, *scroll, *listsearch;
  VkDeck *deck;
};

Deck::Deck(const char *name, Widget parent)
: VkComponent(name)
{
  count = 0;
  _baseWidget = XmCreateForm(parent, (char *) name, args, count);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  pop = XmCreatePushButton(_baseWidget, "Pop", args, count);
  XtManageChild(pop);
  XtAddCallback(pop, XmNactivateCallback, Deck::pop_activate,
		(XtPointer) this);

  deck = new VkDeck("deck", _baseWidget);
  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNtopWidget, pop);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  XtSetValues(deck->baseWidget(), args, count);

  textio = createTextIO("textio", deck->baseWidget());
  textio->show();
  deck->addView(textio);
  scroll = createScroll("scroll", deck->baseWidget());
  scroll->show();
  deck->addView(scroll);
  listsearch = createListSearch("listSearch", deck->baseWidget());
  listsearch->show();
  deck->addView(listsearch);

  deck->pop(textio);
  deck->show();

  installDestroyHandler();
}

Deck::~Deck()
{
  delete textio;
  delete scroll;
  delete listsearch;
  delete deck;
}

void
Deck::popActivate()
{
  deck->pop();
}

void
Deck::pop_activate(Widget, XtPointer clientData, XtPointer)
{
  Deck *obj = (Deck *) clientData;

  obj->popActivate();
}

VkComponent *
createDeck(char *name, Widget parent)
{
  return new Deck(name, parent);
}
