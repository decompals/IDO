///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//         This example code is from the book:
//
//           Object-Oriented Programming with C++ and OSF/Motif
//         by
//           Douglas Young
//           Prentice Hall, 1992
//           ISBN 0-13-630252-1	
//
//         Copyright 1991 by Prentice Hall
//         All Rights Reserved
//
//  Permission to use, copy, modify, and distribute this software for 
//  any purpose except publication and without fee is hereby granted, provided 
//  that the above copyright notice appear in all copies of the software.
///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////
// This code is from the book:
//
//   "Object-Oriented Programming with C++ and OSF/Motif"
//	by Douglas Young	
//
// Copyright 1991 by Prentice Hall
// All Rights Reserved
//
// Permission to use, copy, modify, and distribute this software without fee
// is hereby granted, provided that the above copyright notice appear in all
// copies and that both the copyright notice and this permission notice appear
// in supporting documentation.
/////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
// ColorChooser.C
////////////////////////////////////////////////////////////////////////////

#include "ColorModel.h"
#include "HSVView.h"
#include "SwatchView.h"
#include "RGBController.h"
#include "RGBView.h"
#include "ColorChooser.h"
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/BulletinB.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>

static String colorChooserResources[] = {
    "*rgbView.x:                        150",
    "*rgbView.y:                         20",
    "*rgbView*label1*labelString:       Red:",
    "*rgbView*label2*labelString:       Green:",
    "*rgbView*label3*labelString:       Blue:",
    "*hsvView.x:                        300",
    "*hsvView.y:                         20",
    "*hsvView*label1*labelString:       Hue:",
    "*hsvView*label2*labelString:       Saturation:",
    "*hsvView*label3*labelString:       Value:",
    "*rgbController.x:                   50",
    "*rgbController.y:                  150",
    "*rgbController*scaleWidth:         375",
    "*rgbController*scaleHeight:         22",
    //  Debatable use of color
    "*rgbController*red*troughColor:    red",
    "*rgbController*green*troughColor:  green",
    "*rgbController*blue*troughColor:   blue",
    "*colorView.x:                       20",
    "*colorView.y:                       20",
    "*swatch.width:                     100",
    "*swatch.height:                    100",
    "*colorView.shadowType:             shadow_in",
    "*ok.x:                              20",
    "*ok.y:                             240",
    "*ok*labelString:                   OK",
    "*cancel.x:                         400",
    "*cancel.y:                         240",
    "*cancel*labelString:               Cancel",
    NULL,
};

ColorChooser::ColorChooser(Widget parent, char *name) : UIComponent(name)
{
    
    setDefaultResources ( parent , colorChooserResources);
    
    _w = XmCreateBulletinBoardDialog ( parent, _name, NULL, 0 );
    
    _okButton     = XmCreatePushButton ( _w, "ok", NULL, 0 );
    _cancelButton = XmCreatePushButton ( _w, "cancel", NULL, 0 );
    
    XtVaSetValues(_w, XmNdefaultButton, _okButton,
                  XmNcancelButton,  _cancelButton, 
                  (char*)NULL );
    
    XtAddCallback ( _okButton, 
                   XmNactivateCallback, 
                   &ColorChooser::okCallback,
                   (XtPointer) this );

    XtAddCallback ( _cancelButton, 
                   XmNactivateCallback, 
                   &ColorChooser::canceledCallback,
                   (XtPointer) this );
    
    _model      = new ColorModel();
    _rgbSliders = new RGBController ( _w, _model, "rgbController");
    _swatch     = new SwatchView ( _w, "colorView");
    _rgbView    = new RGBView ( _w, "rgbView" );
    _hsvView    = new HSVView ( _w, "hsvView" );
    
    _model->attachView(_swatch);
    _model->attachView(_rgbView);
    _model->attachView(_hsvView);
    _model->attachView(_rgbSliders);

    _clientData = NULL;

    XtManageChild ( _okButton );
    XtManageChild ( _cancelButton );
    
    _rgbSliders->manage();
    _swatch->manage();
    _rgbView->manage();
    _hsvView->manage();
}

ColorChooser::~ColorChooser()
{
    delete _model;
    delete _rgbSliders;
    delete _swatch;
    delete _rgbView;
    delete _hsvView;
}

void ColorChooser::pickColor( ColorSelectedCallback okCallback , CanceledCallback canceledCallback, void *clientData)
{
    _okCallback = okCallback;
    _canceledCallback = canceledCallback;

    _clientData = clientData;    

    manage();
}

void ColorChooser::ok()
{
    if(_okCallback)
        (*_okCallback)( _model->red(), _model->green(), _model->blue(), _clientData);
}

void ColorChooser::okCallback(Widget, XtPointer clientData, XtPointer)
{
    ColorChooser *obj = (ColorChooser *) clientData;
    
    obj->ok();
}


void ColorChooser::canceled()
{
    if(_canceledCallback)
        (*_canceledCallback)(  _clientData);
}

void ColorChooser::canceledCallback(Widget, XtPointer clientData, XtPointer)
{
    ColorChooser *obj = (ColorChooser *) clientData;
    
    obj->canceled();
}


