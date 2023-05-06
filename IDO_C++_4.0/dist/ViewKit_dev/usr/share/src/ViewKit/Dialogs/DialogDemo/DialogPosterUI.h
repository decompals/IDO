//////////////////////////////////////////////////////////////
//
// Header file for DialogPosterUI
//
//////////////////////////////////////////////////////////////
#ifndef _DialogPosterUI_H
#define _DialogPosterUI_H
#include <Vk/VkComponent.h>


class DialogPosterUI : public VkComponent
{ 

  private: 

    // Array that describes interactions with Xt resource manager

    static String      _defaultDialogPosterUIResources[];

    // Callbacks to interface with Motif

    static void changeModalCallback (Widget, XtPointer, XtPointer);
    static void changeToErrorCallback (Widget, XtPointer, XtPointer);
    static void changeToQuestionCallback (Widget, XtPointer, XtPointer);
    static void changeToWarningCallback (Widget, XtPointer, XtPointer);
    static void postCallback (Widget, XtPointer, XtPointer);
    static void unpostAllCallback (Widget, XtPointer, XtPointer);
    static void unpostLastCallback (Widget, XtPointer, XtPointer);

  protected: 

    // Widgets created by this class

    Widget  _error;
    Widget  _grid;
    Widget  _modal;
    Widget  _post;
    Widget  _question;
    Widget  _radioBox;
    Widget  _radioBox1;
    Widget  _rowColumn;
    Widget  _separator;
    Widget  _separator1;
    Widget  _unpostall;
    Widget  _unpostlast;
    Widget  _warning;

    // These virtual functions are called from the private callbacks (above)
    // Intended to be overriden in derived classes to define actions

    virtual void changeModal ( Widget, XtPointer );
    virtual void changeToError ( Widget, XtPointer );
    virtual void changeToQuestion ( Widget, XtPointer );
    virtual void changeToWarning ( Widget, XtPointer );
    virtual void post ( Widget, XtPointer );
    virtual void unpostAll ( Widget, XtPointer );
    virtual void unpostLast ( Widget, XtPointer );

  public:

    DialogPosterUI(const char *, Widget);
    ~DialogPosterUI();
    const char* className();
};
#endif

