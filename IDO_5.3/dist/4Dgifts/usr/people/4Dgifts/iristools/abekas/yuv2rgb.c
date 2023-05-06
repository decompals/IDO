/* yuv2rgb.c      1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* This program converts an Abekas YUV file to an RGB file. */
/* If syncs and line numbers needed for SCSI transfer are   */
/* in the YUV file, they must be stripped off before using  */
/* torgb.  Degoop is used for this function.                */
/*                                                          */
/*        cc -g yuv2rgb.c -o yuv2rgb                        */

#include <stdio.h> 

#define LINE_LENGTH 720 
#define FRAME_LENGTH 486 

main(argc, argv) 
int argc; char **argv; 
{ 
int i, line, pixel; 
FILE *rgbfile, *yuvfile; 
long r, g, b, y1, y2, u, v;

if(argc != 3)
    { 
    fprintf(stderr, "Usage yuv2rgb <yuvfile> <rgbfile>\n"); 
    exit(1); 
    } 

if((yuvfile = fopen(argv[1], "rb")) == NULL) 
    { 
    fprintf(stderr, "Unable to open file %s\n", argv[1]); 
    exit(1); 
    } 

if((rgbfile = fopen(argv[2], "wb")) == NULL) 
    { 
    fprintf(stderr, "Unable to open file %s\n", argv[2]); 
    exit(1); 
    } 

/* deal with an U Y V Y sequence each time round */ 

for(line = FRAME_LENGTH; line>0; line--) 
    {
    for(pixel = LINE_LENGTH / 2; pixel>0; pixel--) 
        { 
	u = fgetc(yuvfile)-128; 
	y1 = fgetc(yuvfile)-16; 
	if(y1 < 0) y1 = 0;

	v = fgetc(yuvfile)-128; 
	y2 = fgetc(yuvfile)-16; 
	if(y2 < 0) y2 = 0;

	    r  =    76310*y1 +            104635*v;
	    if(r > 0xFFFFFF) r = 0xFFFFFF;
	    if(r <= 0xFFFF) r = 0;

	    g  =    76310*y1 + -25690*u + -53294*v;
	    if(g > 0xFFFFFF) g = 0xFFFFFF;
	    if(g <= 0xFFFF) g = 0;

	    b  =    76310*y1 + 132278*u;
	    if(b > 0xFFFFFF) b = 0xFFFFFF;
	    if(b <= 0xFFFF) b = 0;

	    putc(r>>16, rgbfile); 
	    putc(g>>16, rgbfile); 
	    putc(b>>16, rgbfile); 

	    r  =    76310*y2 +            104635*v;
	    if(r > 0xFFFFFF) r = 0xFFFFFF;
	    if(r <= 0xFFFF) r = 0;

	    g  =    76310*y2 + -25690*u + -53294*v;
	    if(g > 0xFFFFFF) g = 0xFFFFFF;
	    if(g <= 0xFFFF) g = 0;

	    b  =    76310*y2 + 132278*u;
	    if(b > 0xFFFFFF) b = 0xFFFFFF;
	    if(b <= 0xFFFF) b = 0;

	    putc(r>>16, rgbfile); 
	    putc(g>>16, rgbfile); 
	    putc(b>>16, rgbfile); 
        } 
    } 
} 

