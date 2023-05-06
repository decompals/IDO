#include <Vk/VkGraph.h>
#include <Vk/VkNode.h>

VkComponent *
createGraph(char *name, Widget parent)
{
  VkGraph *graph;
  VkNode *root, *a, *b, *c, *d;

  graph = new VkGraph(name, parent);

  root = new VkNode("root", "Root");
  a = new VkNode("a", "A");
  b = new VkNode("b", "B");
  c = new VkNode("c", "C");
  d = new VkNode("d", "D");
  graph->add(root, NULL);
  graph->add(root, a);
  graph->add(root, b);
  graph->add(b, c);
  graph->add(b, d);
  graph->displayAll();
  graph->doLayout();

  return graph;
}
