//////////////////////////////////////////////////////////////
//
// Header file for DialogPoster a subclass template
//
//////////////////////////////////////////////////////////////
#ifndef _DialogPoster_H
#define _DialogPoster_H
#include "DialogPosterUI.h"


class VkDialogManager;

class DialogPoster : public DialogPosterUI
{ 

  protected:

    VkDialogManager *_current;
    Boolean  _modal;

    // These functions will be called as a result of callbacks
    // registered in DialogPosterUI

    virtual void changeModal(Widget, XtPointer);
    virtual void changeToError(Widget, XtPointer);
    virtual void changeToQuestion(Widget, XtPointer);
    virtual void changeToWarning(Widget, XtPointer);
    virtual void post(Widget, XtPointer);
    virtual void unpostAll(Widget, XtPointer);
    virtual void unpostLast(Widget, XtPointer);


  public:

    DialogPoster(const char *, Widget);
    ~DialogPoster();
    const char* className();
};
#endif

