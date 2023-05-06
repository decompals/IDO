///////////////////////////////////////////////////////
// LabeledText.h: A simple C++ component class
//////////////////////////////////////////////////////// 

#ifndef LABELEDTEXT_H
#define LABELEDTEXT_H

#include <Vk/VkComponent.h>

class LabeledText  : public VkComponent {
    
  protected:

    Widget _text;        // Input area
    Widget _label;       // The label

  public:
    
    LabeledText ( Widget, char * ); // Requires a parent and a name
    ~LabeledText ( );
    char *getString();
    void setString(char *);
};
#endif


