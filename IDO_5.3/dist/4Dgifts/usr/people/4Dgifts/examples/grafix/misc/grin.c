/*			
 *   grin.c:
 *	
 *      GRaphics INventory probe.
 *
 *	Using GETGDESC(3G), this program probes all of the graphics hardware 
 *   and software capabilities.  There are over 65 hardware and software 
 *   features that may be listed through the use of GETGDESC(3G).  The 
 *   features range from finding out how many hardware bits are in the 
 *   Z-buffer to whether color map anti-aliasing of lines is supported on the
 *   machine the code is running on. This program will run on any 4D machine.
 *
 *   Note that getgdesc and gversion calls do not require a call to "winopen".
 */

#include <gl.h>

main()
{
  String v[20];
  printf("\n\n");
  gversion(v);
  printf("Machine Version : \t%s\n\n",v);
  printf("This machine has the following features :\n\n");
  printf("%d\t  Horizontal Width (pixels)\n",getgdesc(GD_XPMAX));
  printf("%d\t  Horizontal Width (mm)\n",getgdesc(GD_XMMAX));
  printf("%d\t  Vertical Width (pixels)\n",getgdesc(GD_YPMAX));
  printf("%d\t  Vertical Width (mm)\n",getgdesc(GD_YMMAX));
  printf("%x\t  Max Z-buffer depth \n",getgdesc(GD_ZMAX));
  printf("%x  Min Z-buffer depth \n",getgdesc(GD_ZMIN));
  printf("%d\t  Single Buffered Red Bits\n",getgdesc(GD_BITS_NORM_SNG_RED));
  printf("%d\t  Single Buffered Green Bits\n",getgdesc(GD_BITS_NORM_SNG_GREEN));
  printf("%d\t  Single Buffered Blue Bits\n",getgdesc(GD_BITS_NORM_SNG_BLUE));
  printf("%d\t  Double Buffered Red Bits\n",getgdesc(GD_BITS_NORM_DBL_RED));
  printf("%d\t  Double Buffered Green Bits\n",getgdesc(GD_BITS_NORM_DBL_GREEN));
  printf("%d\t  Double Buffered Blue Bits\n",getgdesc(GD_BITS_NORM_DBL_BLUE));
  printf("%d\t  Single Buffered Color Map Bits\n",getgdesc(GD_BITS_NORM_SNG_CMODE));
  printf("%d\t  Double Buffered Color Map Bits\n",getgdesc(GD_BITS_NORM_DBL_CMODE));
  printf("%d\t  Single Buffered Multi Map Bits\n",getgdesc(GD_BITS_NORM_SNG_MMAP));
  printf("%d\t  Double Buffered Multi Map Bits\n",getgdesc(GD_BITS_NORM_DBL_MMAP));
  printf("%d\t  Z Buffer Bits\n",getgdesc(GD_BITS_NORM_ZBUFFER));
  printf("%d\t  Overlay Planes \n",getgdesc(GD_BITS_OVER_SNG_CMODE));
  printf("%d\t  Underlay Planes \n",getgdesc(GD_BITS_UNDR_SNG_CMODE));
  printf("%d\t  Popup Planes \n",getgdesc(GD_BITS_PUP_SNG_CMODE));
  printf("%d\t  Single Buffered Alpha Planes \n",getgdesc(GD_BITS_NORM_SNG_ALPHA));
  printf("%d\t  Double Buffered Alpha Planes \n",getgdesc(GD_BITS_NORM_DBL_ALPHA));
  printf("%d\t  Cursor Bits \n",getgdesc(GD_BITS_CURSOR));
  printf("%d\t  Shared Over/Underlay Planes \n",getgdesc(GD_OVERUNDER_SHARED));
  printf("%d\t  Blend Functions Supported \n",getgdesc(GD_BLEND));
  printf("%d\t  Fractional Precision Color Iteration Supported \n",getgdesc(GD_CIFRACT));
  printf("%d\t  Crosshair Color index \n",getgdesc(GD_CROSSHAIR_CINDEX));
  printf("%d\t  RGB Dithering\n",getgdesc(GD_DITHER));
  printf("%d\t  CMap Antialiased Lines Supported \n",getgdesc(GD_LINESMOOTH_CMODE));
  printf("%d\t  RGB Antialiased Lines Supported \n",getgdesc(GD_LINESMOOTH_RGB));
  printf("%d\t  Logicops Supported \n",getgdesc(GD_LOGICOP));
  printf("%d\t  Available Screens \n",getgdesc(GD_NSCRNS));
  printf("%d\t  Max Order of NuRBs Surface \n",getgdesc(GD_NURBS_ORDER));
  printf("%d\t  Max Number of Blinkin CMap Entries \n",getgdesc(GD_NBLINKS));
  printf("%d\t  Max Number of Vertices per Polygon \n",getgdesc(GD_NVERTEX_POLY));
  printf("%d\t  64x64 Patterns Supported \n",getgdesc(GD_PATSIZE_64));
  printf("%d\t  CMap Antialiased Points Supported \n",getgdesc(GD_PNTSMOOTH_CMODE));
  printf("%d\t  RGB Antialiased Points Supported \n",getgdesc(GD_PNTSMOOTH_RGB));
  printf("%d\t  Over/Underlay Can Use Popup Planes \n",getgdesc(GD_PUP_TO_OVERUNDER));
  printf("%d\t  Readsource of Auto,Front,Back Supported \n",getgdesc(GD_READSOURCE));
  printf("%d\t  Readsource of Z-buffer Supported \n",getgdesc(GD_READSOURCE_ZBUFFER));
  printf("%d\t  setmonitor(STR_RECT) Supported \n",getgdesc(GD_STEREO));
  printf("%d\t  Subpixel Positioned Lines Supporte\n",getgdesc(GD_SUBPIXEL_LINE));
  printf("%d\t  Subpixel Positioned Points Supporte\n",getgdesc(GD_SUBPIXEL_PNT));
  printf("%d\t  Subpixel Positioned Polygons Supporte\n",getgdesc(GD_SUBPIXEL_POLY));
  printf("%d\t  Max Order of Trimming Curve  \n",getgdesc(GD_TRIMCURVE_ORDER));
  printf("%d\t  Window Manager Is Currently Running \n",getgdesc(GD_WSYS));
  printf("%d\t  Rendering Geometry to Z-buffer Supported \n",getgdesc(GD_ZDRAW_GEOM));
  printf("%d\t  Rendering Pixels to Z-buffer Supported \n",getgdesc(GD_ZDRAW_PIXELS));
  printf("%d\t  Screen Type \n",getgdesc(GD_SCRNTYPE));
  printf("%d\t  Textports Supported \n",getgdesc(GD_TEXTPORT));
  printf("%d\t  Number Of Smaller Cmaps Available \n",getgdesc(GD_NMMAPS));
  printf("%d\t  Readsource of Framegrabber Supporte\n",getgdesc(GD_FRAMEGRABBER));
  printf("%d\t  Timer Event Frequency \n",getgdesc(GD_TIMERHZ));
  printf("%d\t  Dial & Button Routines Function  \n",getgdesc(GD_DBBOX));
  printf("%d\t  Afunction Is Supported \n",getgdesc(GD_AFUNCTION));
  printf("%d\t  Over/Underlay Can Use Alpha Planes \n",getgdesc(GD_ALPHA_OVERUNDER));
  printf("%d\t  Accumulation Buffer Bits\n",getgdesc(GD_BITS_ACBUF));
  printf("%d\t  Accumulation Buffer Hardware Bits\n",getgdesc(GD_BITS_ACBUF_HW));
  printf("%d\t  Stencil Planes Bits\n",getgdesc(GD_BITS_STENCIL));
  printf("%d\t  Arbitrary Clip Planes Available \n",getgdesc(GD_CLIPPLANES));
  printf("%d\t  Fogging Available \n",getgdesc(GD_FOGVERTEX));
  printf("%d\t  Two Sided Lighting Available  \n",getgdesc(GD_LIGHTING_TWOSIDE));
  printf("%d\t  polymode Available  \n",getgdesc(GD_POLYMODE));
  printf("%d\t  polysmooth Available  \n",getgdesc(GD_POLYSMOOTH));
  printf("%d\t  scrbox Available \n",getgdesc(GD_SCRBOX));
  printf("%d\t  Texturing Available \n",getgdesc(GD_TEXTURE));
  printf("%d\t  Per Pixel Foggin Available \n",getgdesc(GD_FOGPIXEL));
  printf("%d\t  Texturing w/ Perspective Correction Available \n",getgdesc(GD_TEXTURE_PERSP));
  printf("%d\t  Number Of Pipes Available For Video Multiplexing \n",getgdesc(GD_MUXPIPES));

}
