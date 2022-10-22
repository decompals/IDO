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


#include "ColorModel.h"
#include "ColorView.h"

ColorModel::ColorModel()
{
    _numViews = 0;
    _views    = NULL;
    _red      = 0;
    _green    = 0;
    _blue     = 0;
}

void ColorModel::attachView( ColorView *view )
{
    int i;
    
    ColorView **newViewList;
    
    newViewList = new ColorView*[_numViews + 1];
    
    for( i = 0; i < _numViews; i++)
        newViewList[i] = _views[i];
    
    delete _views;
    
    _views =  newViewList;
    
    _views[_numViews] = view;
    
    _numViews++;
    
    view->update(this);
}


void ColorModel::detachView( ColorView *view )
{
    
    
}

void ColorModel::updateViews()
{
    int i;
    for(i = 0; i < _numViews; i++ )
        _views[i]->update( this );
}

void ColorModel::setRgb(int r, int g, int b)
{
    _red   = r;
    _blue  = b;
    _green = g;
    updateViews();
}

