/* 
 *  maketorus.c:
 *
 *
 *     This is an example program for creating an object file in the spin 
 *  format.  Spin objects contain only independent quadrilaterals.  There
 *  are two formats, one for objects with a normal at each vertex and one 
 *  for objects with a color at each vertex.
 *
 *     The first thing in the file is an int that contains a magic number
 *  (see FASTMAGIC below).
 *
 *     The second is an int containing the number of points in the object.
 *  Every four points specifies a polygon.  The number of polygons is always 
 *  one fourth the number of points.
 *
 *     The third is an int flag that tells spin if the object contains 
 *  colors or normals.  If the color flag is true, the rest of the data is:
 *
 *    r, g, b,	(first vertex color)
 *    x, y, z,	(first vertex coordinate)
 * 
 *    r, g, b,	(second vertex color)
 *    x, y, z,	(second vertex coordinate)
 * 
 *    ...
 *
 *  where r, g, and b are ints between 0 and 255.  There is always a color 
 *  for each vertex.
 *
 *     If the color flag is false then the rest of the data is:
 *
 *    nx, ny, nz,	(first vertex normal)
 *    px, py, pz,	(first vertex coordinate)
 *
 *    nx, ny, nz,	(second vertex normal)
 *    px, py, pz,	(second vertex coordinate)
 *
 *    ...
 *
 *  where nx, ny, nz, px, py, and pz are floats.  There is always a normal 
 *  for each vertex.
 *
 *     There is presently no way to specify triangle meshes or polygons 
 *  with other than four sides.
 *
 *     This example program creates a torus with normal information.
 *
 *     After creating the outfile ("maketorus torus.bin"), you need to 
 *  become root user, and move the data file into the directory
 *  "/usr/demos/lib/models"
 *
 *                       thant tessman - October '88
 */


#include <stdio.h>
#include <math.h>

#define RAD 3.0	        /* ratio of major radius to minor radius */

#define SCALE 0.2	/* shrink object to fit in spin's view */

#define LATRES 16	/* latitude resolution */
#define LONRES 36	/* longitude resolution */

#define X 0
#define Y 1
#define Z 2

#define FASTMAGIC 0x5423

#define DEG 	*M_PI/180.0

float torus_points[LATRES+1][LONRES+1][3];
float torus_normals[LATRES+1][LONRES+1][3];

FILE *fp;

main(argc, argv)
int	argc;
char	*argv[];
{

    int magic = FASTMAGIC;
    int npoints = LATRES * LONRES * 4;
    int colors = 0;

    if (argc!=2) {
	fprintf(stderr, "\nusage: %s outfile\n", argv[0]);
	exit(1);
    }

    if ( !(fp = fopen(argv[1], "w"))) {
	fprintf(stderr, "%s: can't open %s:", argv[0], argv[1]);
	perror("");
	exit(1);
    }

    build_torus();

    fwrite(&magic, sizeof(int), 1, fp);
    fwrite(&npoints, sizeof(int), 1, fp);
    fwrite(&colors, sizeof(int), 1, fp);

    write_torus();

    fclose(fp);
}


build_torus() {

    float a, b, xp, yp, zp, xn, yn, zn;
    int i, j;


    for (j=0, b=0.0; b<=360.; b+=360.0/LATRES, j++) {

	for (i=0, a=0.0; a<=360.0; a+=360.0/LONRES, i++) {

	    torus_points[j][i][Z] = cos(b DEG) * SCALE;
	    torus_points[j][i][X] = (sin(b DEG) + RAD) * cos(a DEG) * SCALE;
	    torus_points[j][i][Y] = -(sin(b DEG) + RAD) * sin(a DEG) * SCALE;

	    torus_normals[j][i][Z] = cos(b DEG);
	    torus_normals[j][i][X] = sin(b DEG) * cos(a DEG);
	    torus_normals[j][i][Y] = -sin(b DEG) * sin(a DEG);

	}

    }

}



write_torus() {

    int i, j;

    for (j=0; j<LATRES; j++) {

	for (i=0; i<LONRES; i++) {

	    fwrite(torus_normals[j][i], sizeof(float), 3, fp);
	    fwrite(torus_points[j][i], sizeof(float), 3, fp);

	    fwrite(torus_normals[j+1][i], sizeof(float), 3, fp);
	    fwrite(torus_points[j+1][i], sizeof(float), 3, fp);

	    fwrite(torus_normals[j+1][i+1], sizeof(float), 3, fp);
	    fwrite(torus_points[j+1][i+1], sizeof(float), 3, fp);

	    fwrite(torus_normals[j][i+1], sizeof(float), 3, fp);
	    fwrite(torus_points[j][i+1], sizeof(float), 3, fp);
	}

    }

}
