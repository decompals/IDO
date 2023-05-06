#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkNode.h>
#include <Vk/VkGraph.h>
#include <Vk/VkMenu.h>

class GraphWindow: public VkWindow {

  public:
    GraphWindow( const char *);
    ~GraphWindow();
    virtual const char* className();

  protected:
    VkGraph *graph;
    VkNode *p_node, *c1_node, *c2_node, *gc1_node, *gc2_node;

  private:
    static void quitCallback (Widget, XtPointer, XtPointer);

    static VkMenuDesc appMenuPane[];
};


VkMenuDesc GraphWindow::appMenuPane[] = {
  { ACTION,   "Quit",      &GraphWindow::quitCallback },
  { END }
};

GraphWindow::GraphWindow(const char *name) : VkWindow( name )
{
    // Create nodes

    p_node   = new VkNode("parentNode", "Parent");
    c1_node  = new VkNode("childNode1", "Child 1");
    c2_node  = new VkNode("childNode2", "Child 2");
    gc1_node = new VkNode("grandChildNode1", "Grandchild 1");
    gc2_node = new VkNode("grandChildNode2", "Grandchild 2");
    
    // Create graph

    graph   = new VkGraph( "graph", mainWindowWidget() );

    // Add nodes to graph

    graph->add(p_node, c1_node);     // p_node is parent to c1_node
    graph->add(p_node, c2_node);     // p_node is parent to c2_node
    graph->add(c1_node, gc1_node);   // c1_node is parent to gc1_node
    graph->add(c1_node, gc2_node);   // c1_node is parent to gc2_node

    graph->displayAll();             // Display all nodes in graph
    graph->doLayout();               // Layout the graph
    
    addView(graph);                  // Set graph to be window's view
    addMenuPane("Application", appMenuPane);         // Create menu bar
}

GraphWindow::~GraphWindow()
{
    delete graph;
    delete p_node;
    delete c1_node;
    delete c2_node;
    delete gc1_node;
    delete gc2_node;
}

const char* GraphWindow::className()
{
    return "GraphWindow";
}

void GraphWindow::quitCallback ( Widget, XtPointer, XtPointer )
{
    theApplication->quitYourself();
}

void main(int argc, char **argv)
{
  VkApp       *myApp    = new VkApp("GraphViewer", &argc, argv);
  GraphWindow *graphWin = new GraphWindow("GraphViewer");

  graphWin->show();
  myApp->run();
}
