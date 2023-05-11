/* 
 * Private header file for Oscilloscope widget
 */
#ifndef OSCP_H
#define OSCP_H

#include <Xm/PrimitiveP.h>
#include "Osc.h"


typedef struct _SgOscClassPart {
  XtPointer	 	extension;
} SgOscClassPart;

typedef struct _SgOscClassRec {
  CoreClassPart		core_class;
  XmPrimitiveClassPart	primitive_class;
  SgOscClassPart	osc_class;
} SgOscClassRec;

extern SgOscClassRec sgOscClassRec;

typedef struct _SgOscPart {
  int			time_scale;    		/* dist between grids		*/
  float			real_time_scale;	/* time_scale in sec.  		*/
  int			grid_type;		/* grid/axis display: 0-8	*/

  int			sample_rate;		/* samples per second		*/
  
  long			minimum;		/* minimum sample value 	*/
  long			maximum;		/* maximum sample value 	*/
  
  enum SgOscDisplayMode	display_mode;		/* left, right, or both (stereo)*/

  Boolean		live;			/* Live or prerecorded data? 	*/
  Boolean		repeat;			/* Loop or end playback? 	*/
  Boolean		done;			/* Reached the end?   		*/

  enum SgOscDataFormat	sample_format;	       	/* Format of audio data		*/
  enum SgOscBufferFormat buffer_format;		/* Mono or Stereo data buffer?	*/
  XtPointer		 samples;		/* pointer to sample buffer	*/
  unsigned int		num_samps;		/* # samps displayed at a time	*/
  unsigned int		total_samps;		/* total # samps in data buffer */
  unsigned int		num_samps_drawn;	/* actual # of points to draw	*/
  
  XPoint		*l_channel_pts;		/* point array			*/
  XPoint		*r_channel_pts;		/* point array			*/

  int			l_channel_offset;	/* Vert offset			*/
  int			r_channel_offset;	/* Vert offset			*/
 
  int			place;			/* current position in buffer	*/
  int			start;			/* first sample to display	*/
  int			end;			/* last sample to display	*/
  
  
  Pixel			l_channel_color;	/* left channel color		*/
  Pixel			r_channel_color;	/* right channel color		*/
  Pixel			grid_color;		/* grid/axis color		*/
  Pixel			range_color;		/* range-marker color		*/

  GC			l_channel_GC;		/* GC for left channel		*/
  GC			r_channel_GC;		/* GC for right channel		*/
  GC			osc_GC;			/* GC for grid/axis & background*/
  GC			range_GC;		/* GC for range markers		*/
  GC			inverse_GC;		/* GC for erasing 		*/

  XtCallbackList	value_changed;		/* callback			*/
} SgOscPart;

typedef struct _SgOscRec {
  CorePart		core;
  XmPrimitivePart	primitive;
  SgOscPart		osc;
} SgOscRec;

#endif /* OSCP_H */


