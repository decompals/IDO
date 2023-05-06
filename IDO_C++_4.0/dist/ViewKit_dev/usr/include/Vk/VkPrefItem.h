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
#ifndef VKPREFITEM_H
#define VKPREFITEM_H

#include <Vk/VkComponent.h>

enum VkPrefItemType { PI_none, PI_group, PI_list, PI_radio, PI_text, PI_toggle,
		      PI_option, PI_custom, PI_empty, PI_label, PI_separator };

class VkPrefItem : public VkComponent {

   friend class VkPrefGroup;
   friend class VkPrefList;
   friend class VkPrefRadio;
   friend class VkPrefDialog;
  
  public:

     ~VkPrefItem();
     virtual const char *className();
     virtual VkPrefItemType type();

     void activate()    { setActivated(TRUE); }
     void deactivate()  { setActivated(FALSE); }

     virtual Boolean changed();
     virtual Boolean isContainer();
     Dimension labelHeight();
     Dimension baseHeight();
     void setLabelHeight(Dimension h);
     void setBaseHeight(Dimension h);
     Widget labelWidget();

  protected:

    void buildLabel(Widget parent);
    Dimension getHeight(Widget w);
    void setHeight(Widget w, Dimension h);

    Widget   _labelWidget;
    char    *_baseName, *_labelName;
    Boolean  _activated, _homogeneous;
   
    VkPrefItem(const char *name);
    virtual void build(Widget parent);
    virtual void build(Widget labelParent, Widget baseParent) = 0;
    void installDestroyHandlers();
    virtual void updateValue() = 0;
    virtual void revertValue() = 0;
    virtual void setActivated(Boolean);
    virtual void deleteChildren();
    void    setHomogeneous(Boolean flag);

  private:

    static void labelDestroyedCallback(Widget, XtPointer, XtPointer);
};


class VkPrefGroup : public VkPrefItem {

  public:

            VkPrefGroup(const char *name, 
       		       Boolean horizOrientation = False,
		       Boolean noLabel = False);
            ~VkPrefGroup();

    virtual void build(Widget parent);
    virtual void show();
    virtual void deleteChildren();
            void addItem(VkPrefItem *item);
    virtual Boolean okToQuit();
    virtual Boolean isContainer();
    virtual const char *className();
    virtual VkPrefItemType type();
    virtual Boolean changed();
    virtual Dimension baseHeight();
            VkPrefItem *item(int which);
           int size();
 
    virtual void updateValue();
    virtual void revertValue();

  protected:

    virtual void setActivated(Boolean v);


    virtual void build(Widget labelParent, Widget baseParent);

    void checkHomogeneous();
    virtual void buildVertical(Widget baseParent);
    virtual void buildHorizontal(Widget baseParent);

    Widget _leftStuff, _rightStuff;
    VkPrefItem **_itemList;
    int _itemCount, _itemListSize;
    Boolean _horiz, _noLabel;

  private:

    static void mapHandler(Widget w, XtPointer client_data, XEvent *event);
};


class VkPrefList : public VkPrefGroup {

  public:

    VkPrefList(const char *name);
    ~VkPrefList();

    virtual void build(Widget parent);

    virtual const char *className();
    virtual VkPrefItemType type();

  protected:

     virtual void build(Widget labelParent, Widget baseParent);
};


class VkPrefRadio : public VkPrefGroup {

  public:

    VkPrefRadio(const char *name, 
		Boolean horizOrientation = False,
   	        Boolean noLabel = False);
    ~VkPrefRadio();

    virtual void build(Widget parent);
    virtual const char *className();
    virtual VkPrefItemType type();

  protected:

     virtual void build(Widget labelParent, Widget baseParent);
};


class VkPrefText : public VkPrefItem {


  public:

     VkPrefText(const char *name, int columns = 5);
     ~VkPrefText();
      virtual const char *className();

      virtual VkPrefItemType type();

      virtual Boolean changed();

      void setValue(const char *str);
      char *getValue();

  protected:

      int   _columns;
      char *_value;

      virtual void build(Widget labelParent, Widget baseParent);
      virtual void build(Widget parent);
      virtual void updateValue();
      virtual void revertValue();
    
};


class VkPrefToggle : public VkPrefItem {

  public:

     VkPrefToggle(const char *name, Boolean forceLabelFormat = False);
     ~VkPrefToggle();
      virtual const char *className();
      virtual VkPrefItemType type();
      virtual Boolean changed();

      void setValue(Boolean v);
      Boolean getValue();

  protected:

     Boolean _value, _forceLabel;
     virtual void updateValue();
     virtual void revertValue();
     virtual void build(Widget labelParent, Widget baseParent);
     virtual void build(Widget parent);
};


class VkPrefOption : public VkPrefItem {

  public:
    
    VkPrefOption(const char *name, int numEntries);
    ~VkPrefOption();
    
    void setValue(int index);
    int getValue();
    
    int size() { return _num; }
    void setSize(int numEntries);
    
    void setLabel(int index, const char *label);
    char *getLabel(int index);
    Widget getButton(int index) { return _buttons[index]; }
    
    virtual const char *className();
    virtual VkPrefItemType type();
    virtual Boolean changed();
    
  protected:
    
    Widget _pulldown, *_buttons;
    int _num, _size, _value;
    char **_labels;
    
    virtual void build(Widget labelParent, Widget baseParent);
    virtual void build(Widget parent);
    virtual void updateValue();
    virtual void revertValue();
    
};


class VkPrefCustom : public VkPrefItem {

  public:

     VkPrefCustom(const char *name);
    ~VkPrefCustom();
     virtual const char *className();
      virtual VkPrefItemType type();
      virtual Boolean changed();

  protected:

    virtual void build(Widget labelParent, Widget baseParent);
    virtual void build(Widget parent);

    virtual void updateValue();
    virtual void revertValue();
};


class VkPrefEmpty : public VkPrefCustom {

  public:

    VkPrefEmpty();
    ~VkPrefEmpty();
     virtual const char *className();

    virtual VkPrefItemType type();
    virtual Boolean changed();

  protected:

    virtual void build(Widget labelParent, Widget baseParent);
    virtual void build(Widget parent);
};


class VkPrefLabel : public VkPrefItem {

  public:

    VkPrefLabel(const char *name);
    ~VkPrefLabel();
    virtual const char *className();

    virtual VkPrefItemType type();
    virtual Boolean changed();

  protected:
   
     virtual void build(Widget labelParent, Widget baseParent);
     virtual void build(Widget parent);
     virtual void updateValue();
     virtual void revertValue();
};


class VkPrefSeparator : public VkPrefItem {
  
   public:

     VkPrefSeparator(const char *name);
     ~VkPrefSeparator();
     virtual const char *className();
     virtual VkPrefItemType type();
     virtual Boolean changed();

   protected:

     virtual void build(Widget labelParent, Widget baseParent);
     virtual void build(Widget parent);
     virtual void updateValue();
     virtual void revertValue();
};


#endif
