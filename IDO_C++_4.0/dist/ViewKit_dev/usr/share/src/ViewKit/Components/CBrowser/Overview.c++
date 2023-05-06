#include <stdio.h>
#include <math.h>
#include "Overview.h"
#include "Element.h"
#include "Data.h"
#include <Sgm/Grid.h>

static int count;
static Arg args[10];

typedef Celement *CElementPtr;

Coverview::Coverview(const char *name)
: VkSimpleWindow(name)
{
  elements = NULL;
}

Coverview::~Coverview()
{
  int each;

  if (elements) {
    for (each=0; each<numElements; each++) {
      delete elements[each];
    }
    delete elements;
  }
}

/**********************************************************************/

Widget
Coverview::setUpInterface(Widget parent)
{
  int each, rows, cols, index;

  numElements = componentInfoSize;
  for (each=0; each<componentInfoSize; each++) {
    if (componentInfo[each].omitFromOverview) {
      numElements--;
    }
  }

  rows = (int) sqrt(numElements);
  cols = (numElements+rows-1)/rows;

  count = 0;
  XtSetArg(args[count], XmNnumRows, rows);  count++;
  XtSetArg(args[count], XmNnumColumns, cols);  count++;
  XtSetArg(args[count], XmNautoLayout, False);  count++;
  XtSetArg(args[count], XmNresizePolicy, XmRESIZE_NONE);  count++;
  grid = SgCreateGrid(parent, "grid", args, count);

  elements = new CElementPtr[numElements];
  index = 0;
  for (each=0; each<componentInfoSize; each++) {
    if (!componentInfo[each].omitFromOverview) {
      elements[index] = new Celement("element", grid, each);
      elements[index]->show();
      index++;
    }
  }
  
  XtManageChild(grid);
  return grid;
}

void
Coverview::handleWmDeleteMessage()
{
  hide();
}
