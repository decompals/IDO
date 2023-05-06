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
#ifndef VKNODE_H
#define VKNODE_H

#include <Vk/VkComponent.h>

class VkNode;

typedef int (*VkNodeSortFunction)(VkNode *, VkNode *);

class VkNode : public VkComponent {

    friend class VkGraph;

 public:

    // Constructors

    VkNode();
    VkNode(const char *name, const char *label = NULL);
    VkNode(const char *name, VkNode *parent, const char *label = NULL);

    static void setSortFunction(VkNodeSortFunction f) { VkNode::_sortFunction = f; }

    // Destructor

    virtual ~VkNode();

    // The only public info that should be needed is
    // is the name, the widgets,  and access to the 
    // children and parents of a node.
    
    char *name() const {return _name;}
    virtual char *label();

    int nChildren() const { return _nChildren;}
    int nParents() const { return _nParents;}

    VkNode *findChild(char * name);
    VkNode *findParent(char * name);

    VkNode *child(int index) const { return _children[index];}
    VkNode *parent(int index) const { return _parents[index];}

    void sortChildren();   

    // "private" functions for internal use. Do not use.

    void markAsConnected(int index) { _buildList[index] = TRUE;}
    void markAsDisconnected(int index) { _buildList[index] = FALSE;}
    Boolean connected(int index) ;
    Atom attribute(int index) ;
    String attributeName(int index) ;

  protected:

    int   _index;


    char          *_label;
    void clear();
    class VkNode **_children;
    int            _nChildren;
    int            _maxChildren;

    Boolean        *_buildList;
    Atom           *_attributeList;
    VkNode         **_parents;
    int            _nParents;
    int            _maxParents;
    Boolean        _hasChildren;
    Boolean        _hasVisibleChildren;
    Boolean        _hasVisibleParents;
    Boolean        _hasParents;

    Boolean hasChildren() { return _hasChildren; };
    Boolean hasVisibleChildren() { return _hasVisibleChildren; };
    Boolean hasParents() { return _hasParents; };
    Boolean hasVisibleParents() { return _hasVisibleParents; };
    
    void display(Widget parent);
    void undisplay();

    void addParent(VkNode *, char *attribute = NULL);

    void addChild(VkNode *);

    void removeChild(char *name);
    void removeChild(VkNode *child);

    void removeParent(char *name);
    void removeParent(VkNode *child);

    virtual void updateVisualState();

    virtual void build(Widget);

    void widgetDestroyed();
    
    
  private:

    static VkNodeSortFunction _sortFunction;
    static int sortStub(const void *, const void *);

    
};

#endif


