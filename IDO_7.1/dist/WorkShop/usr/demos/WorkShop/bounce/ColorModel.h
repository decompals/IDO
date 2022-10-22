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


#ifndef COLORMODEL_H
#define COLORMODEL_H

class ColorView;

class ColorModel {
    
private:
    
    int         _numViews;
    ColorView **_views;    
    
    int      _red;       // rgb values
    int      _green;
    int      _blue;
    void     updateViews();
    
public:
    
    ColorModel();
    virtual ~ColorModel() {};
    
    void attachView( ColorView * );
    void detachView( ColorView * );
    
    void setRgb ( int, int, int );   
    void setRed ( int r )   { setRgb ( r,    _green, _blue); }
    void setGreen ( int g ) { setRgb ( _red, g,      _blue);  }
    void setBlue ( int b )  { setRgb ( _red, _green,  b); }
    
    int      red()      { return _red; }
    int      green()    { return _green; }
    int      blue()     { return _blue; }
};
#endif



