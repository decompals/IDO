/* rgb2yuv.c      1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* This program converts an RGB file to an Abekas YUV file. */
/* The syncs and line numbers needed for SCSI transfer must */
/* be added separately.  Engoop is used for this function.  */
/* this function.                                           */
/* (NOTE: This program is useful for speeding up Ethernet   */
/*  transfers since the internal A60 RGB->YUV conversion is */
/*  slower than running this program on an Iris.)           */
/*                                                          */
/*           cc -g rgb2yuv.c -o rgb2yuv                     */

#include <stdio.h> 

#define LINE_LENGTH 720 
#define FRAME_LENGTH 486 
#define FIELD_LENGTH 243 

main(argc, argv) 
int argc; char **argv; 
{ 
int r, g, b;
int tmp, n, line, pixel; 
FILE *rgbfile, *yuvfile; 
long y1, y2, u, v, u0, u1, u2, v0, v1, v2;
unsigned char yuvline[2*LINE_LENGTH], rgbline[3*LINE_LENGTH];
unsigned char *rgbptr, *yuvptr;

if(argc != 3)
    { 
    fprintf(stderr, "Usage toyuv <rgbfile> <yuvfile>\n"); 
    exit(1); 
    } 

if((rgbfile = fopen(argv[1], "r")) == NULL) 
    { 
    fprintf(stderr, "Unable to open file %s\n", argv[1]); 
    exit(1); 
    } 

if((yuvfile = fopen(argv[2], "w")) == NULL) 
    { 
    fprintf(stderr, "Unable to open file %s\n", argv[2]); 
    exit(1); 
    } 

    /* deal with an U Y V Y sequence each time round */ 

for(line = FRAME_LENGTH; line>0; line--) 
    {
    fread(rgbline, 1, 3*LINE_LENGTH, rgbfile);
    rgbptr = rgbline;
    yuvptr = yuvline;
    for(pixel = LINE_LENGTH / 2; pixel>0; pixel--) 
	{ 
	/* first pixel gives Y and 0.5 of chroma */ 

	r = *rgbptr++;
	g = *rgbptr++;
	b = *rgbptr++;

	y1  =    16829*r +  33039*g +  6416*b + (0xFFFF & y2);

	u1  =    -4853*r +  -9530*g + 14383*b;

	v1  =    14386*r + -12046*g + -2340*b;


	/* second pixel just yields a Y and 0.25 U, 0.25 V */ 

	r = *rgbptr++;
	g = *rgbptr++;
	b = *rgbptr++;

	y2  =    16829*r +  33039*g +  6416*b + (0xFFFF & y1);

	u2  =    -2426*r +  -4765*g + 7191*b;

	v2  =    7193*r + -6023*g + -1170*b;

	/* Filter the chroma */

	u = u0 + u1 + u2 + (0xFFFF & u);
	v = v0 + v1 + v2 + (0xFFFF & v);

	u0 = u2;
	v0 = v2;

	*yuvptr++ = (u>>16) +128;
	*yuvptr++ = (y1>>16) + 16;
	*yuvptr++ = (v>>16) +128;
	*yuvptr++ = (y2>>16) + 16;
	} 
    fwrite(yuvline, 1, 1440, yuvfile);
    } 
} 

