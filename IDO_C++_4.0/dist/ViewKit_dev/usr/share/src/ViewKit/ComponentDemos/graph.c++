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

/////////////////////////////////////////////////////////////////
// Program to demo VkGraph component. Program reads a tree description
// from stdin. Format is as follows:
//     parent child parentLabel childLabel
//     parent child parentLabel childLabel
/////////////////////////////////////////////////////////////////////


#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkNode.h>
#include <Vk/VkGraph.h>
#include <Sgm/IconG.h>
#include <Sgm/Graph.h>
#include <Vk/VkMenu.h>
#include <Xm/ToggleB.h>
#include <stdio.h>
#include <stdlib.h>

class GraphWindow: public VkWindow {

  private:

    static void editCallback ( Widget,  XtPointer , XtPointer );
    static void printCallback( Widget,  XtPointer , XtPointer );
    static void quitCallback ( Widget,  XtPointer , XtPointer );
    static void newNodeCallback ( Widget,  XtPointer , XtPointer );
    static void newArcCallback ( Widget,  XtPointer , XtPointer );

    void newNode ( Widget,  XtPointer );
    void newArc ( Widget,  XtPointer );
    void print(Widget, XtPointer);
    void edit(Widget, XtPointer);

    static VkMenuDesc appMenuPane[];

  protected:

    VkGraph  *_graph;

  public:
 
    GraphWindow( const char *name);
    ~GraphWindow();
    virtual const char* className();

};

const char* GraphWindow::className() { return "GraphWindow";}

GraphWindow::~GraphWindow()
{
    // Empty
}

GraphWindow::GraphWindow( const char *name) : VkWindow( name)
{
    char    parentName[1000];
    char    childName[1000];
    char    childLabel[1000];
    char    parentLabel[1000];
    int     nitems;

    addMenuPane("Application", appMenuPane);
    
    _graph = new VkGraph("graph", mainWindowWidget());
    
    XtVaSetValues(_graph->graphWidget(),   
		  XmNeditable,         TRUE,
		  XmNeditableArcs,     FALSE,
		  XmNokToAdd,          FALSE, 
		  NULL);

    XtAddCallback(_graph->graphWidget(),  XmNnewNodeCallback, 
		  &GraphWindow::newNodeCallback, (XtPointer) this);

    XtAddCallback(_graph->graphWidget(),  XmNnewArcCallback, 
		  &GraphWindow::newArcCallback, (XtPointer) this);
    
    while((nitems = scanf("%s %s %s %s", parentName, childName, parentLabel, childLabel)) != EOF &&
	  nitems == 4)
    {
	VkNode *parent, *child;
	
	parent = child = NULL;
	
	if(parentName && (parent = _graph->find(parentName)) == NULL)
	    parent = new VkNode(parentName, parentLabel);
	
	if(childName && (child = _graph->find(childName)) == NULL)
	    child = new VkNode(childName, childLabel);
	
	_graph->add(parent, child);
    }
    
    _graph->displayAll();
    _graph->doLayout();
    
    addView(_graph);
}


VkMenuDesc GraphWindow::appMenuPane[] = {
  { TOGGLE,   "Edit Mode", &GraphWindow::editCallback},
  { ACTION,   "Print",     &GraphWindow::printCallback},
  { SEPARATOR },
  { ACTION,   "Quit",      &GraphWindow::quitCallback},
  { END},
};


void GraphWindow::editCallback( Widget w,  XtPointer clientData, XtPointer callData)
{
    GraphWindow *obj = (GraphWindow *) clientData;

    obj->edit(w, callData);
}


void GraphWindow::edit(Widget, XtPointer callData)
{
    XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *) callData;

    if(cbs->set)
	XtVaSetValues(_graph->graphWidget(),   
		      XmNeditableArcs,     TRUE,
		      XmNokToAdd,          TRUE, 
		      NULL);
    else
	XtVaSetValues(_graph->graphWidget(),   
		      XmNeditableArcs,     FALSE,
		      XmNokToAdd,          FALSE, 
		      NULL);

}

void GraphWindow::newNodeCallback( Widget w,  XtPointer clientData  , XtPointer callData)
{
    GraphWindow *obj = (GraphWindow *) clientData;

    obj->newNode(w, callData);
}

void GraphWindow::newNode(Widget, XtPointer callData)
{
    SgGraphCallbackStruct *cbs = (SgGraphCallbackStruct *) callData;

    cbs->doit = FALSE;

    _graph->add(new VkNode("newNode", "New Node"));

    _graph->displayAll();
    _graph->buildGraph();
    _graph->doLayout();
}


void GraphWindow::newArcCallback( Widget w,  XtPointer clientData  , XtPointer callData)
{
    GraphWindow *obj = (GraphWindow *) clientData;

    obj->newArc(w, callData);
}

void GraphWindow::newArc(Widget, XtPointer callData)
{
    SgGraphCallbackStruct *cbs = (SgGraphCallbackStruct *) callData;
    VkNode *toNode, *fromNode;

    cbs->doit = FALSE;

    Widget  to = cbs->new_to;
    Widget  from = cbs->new_from;

    XtVaGetValues(to, XmNuserData, &toNode, NULL);
    XtVaGetValues(from, XmNuserData, &fromNode, NULL);

    _graph->add(fromNode, toNode);

    _graph->displayAll();
    _graph->buildGraph();
    _graph->doLayout();
}


void GraphWindow::print(Widget, XtPointer)
{
    _graph->saveToFile();
}

void GraphWindow::printCallback( Widget w,  XtPointer clientData  , XtPointer callData)
{
    GraphWindow *obj = (GraphWindow *) clientData;
    obj->print(w, callData);
}


void GraphWindow::quitCallback ( Widget, XtPointer, XtPointer )
{
    theApplication->quitYourself();
}


void main(int argc, char **argv)
{
  VkApp        *myApp    = new VkApp("GraphViewer",  &argc,  argv);
  GraphWindow  *graphWin  = new GraphWindow("GraphViewer");

  graphWin->show();

  myApp->run();
}

