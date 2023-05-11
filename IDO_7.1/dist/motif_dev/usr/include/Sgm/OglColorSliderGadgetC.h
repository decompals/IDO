#ifndef __OglColorSliderGadgetC_h_
#define __OglColorSliderGadgetC_h_

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif 

#ifndef __OglColorSliderManagerC_h_
typedef struct _SgOglColorSliderGadget *_SgOglColorSliderGadgetP;
#endif

/* The range of values the slider visually represents in its background. */

typedef enum {
    Slider_None, 
    Slider_Red,
    Slider_Green, 
    Slider_Blue, 
    Slider_Hue, 
    Slider_Sat,
    Slider_Value
} SliderType;

extern _SgOglColorSliderGadgetP 
  _SgOglColorSliderGadgetCreate(SliderType type, Widget parent);
extern void   _SgOglColorSliderGadgetSetColor(_SgOglColorSliderGadgetP gadget, 
					    short r, short g, short b);
extern void   _SgOglColorSliderGadgetSetHSVColor(_SgOglColorSliderGadgetP gadget, 
					      float hue, float sat, float val);
extern void   _SgOglColorSliderGadgetSetValue(_SgOglColorSliderGadgetP gadget, 
					    float v);
extern float  _SgOglColorSliderGadgetGetValue(_SgOglColorSliderGadgetP gadget);
extern void   _SgOglColorSliderGadgetAddCallback(_SgOglColorSliderGadgetP gadget, 
					       XtCallbackProc callback,
					       void *client_data_arg);
extern void   _SgOglColorSliderGadgetSetMarginHeight(_SgOglColorSliderGadgetP gadget,
						   Dimension height);

#ifdef __cplusplus
}
#endif 


#endif /* __OglColorSliderGadgetC_h_ */

