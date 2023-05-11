#ifndef __OglColorSliderManagerC_h_
#define __OglColorSliderManagerC_h_

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif 

#ifndef __OglColorSliderGadgetC_h_
typedef struct _SgOglColorSliderGadget *_SgOglColorSliderGadgetP;
#endif

extern Widget _SgOglColorSliderManagerCreate(Widget parent, char *name, 
					   Arg *args, Cardinal num_args);
/***
extern Widget _SgOglColorSliderManagerCreate(Widget parent);
***/

extern void   _SgOglColorSliderManagerAddSliders(Widget w, 
					       _SgOglColorSliderGadgetP after,
					       _SgOglColorSliderGadgetP *sliders, 
					       int num_sliders);
extern void   _SgOglColorSliderManagerDeleteSliders(Widget w, 
						 _SgOglColorSliderGadgetP *sliders,
						  int num_sliders);

extern void   _SgOglColorSliderManagerClearArea(Widget w);

#ifdef __cplusplus
}
#endif 


#endif /* __OglColorSliderManagerC_h_ */
