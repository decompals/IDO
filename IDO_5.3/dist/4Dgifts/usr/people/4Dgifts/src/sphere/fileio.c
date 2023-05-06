/* fileio.c
 * --------
 *
 * $Revision: 1.13 $
 */


#include <stdio.h>
#include <string.h>
#include <values.h>

#include "mview.h"
#include "geom.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~ global variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

FILE *infileptr;

/*
 * readatoms()
 */
int readatoms(char *filename, atom_t **atoms, float *cx, float *cy, float *cz)
{
    int natoms=0, atomcount=0;
    atom_t *aptr;
    float tr=0.0, tg=0.0, tb=0.0;
    float
	maxx = MINFLOAT,
	maxy = MINFLOAT,
	maxz = MINFLOAT, 
	minx = MAXFLOAT,
	miny = MAXFLOAT,
	minz = MAXFLOAT;

    if (!(infileptr = fopen(filename, "r"))) {
	fprintf(stderr,"%s: cannot read atom file %s\n", ProgName, filename);
	return 0;
	/*DoExit(-1);*/
    }
    fscanf(infileptr,"natoms %d ", &natoms);

    aptr = *atoms = (atom_t *) malloc (natoms * sizeof(atom_t));

    while ((fscanf(infileptr," atom ") != -1) && (atomcount < natoms)) {
	fscanf(infileptr," %f %f %f %f %f %f %f %d \n",
		    &(aptr->org_rad), &(aptr->x), &(aptr->y), &(aptr->z), 
		    &(aptr->r), &(aptr->g), &(aptr->b),
		    &(aptr->id));
	aptr->rad = aptr->org_rad * RadScaleFactor;
	/* update count for atom type */
	AtomTab[aptr->id].natoms += 1;
	/* update atom bounding box */
	if (aptr->x < minx) minx = aptr->x;
	if (aptr->y < miny) miny = aptr->y;
	if (aptr->z < miny) minz = aptr->z;
	if (aptr->x > maxx) maxx = aptr->x;
	if (aptr->y > maxy) maxy = aptr->y;
	if (aptr->z > maxy) maxz = aptr->z;
	aptr++;
	atomcount++;
    }

    fclose(infileptr);

    /* set center of atom */
    *cx = (minx + maxx) / 2;
    *cy = (miny + maxy) / 2;
    *cz = (minz + maxz) / 2;

    printf("total atoms: %d\n", atomcount);
    return(atomcount);
}

/*
 * readbonds()
 */
int readbonds(char *filename, bond_t **bonds, float *cx, float *cy, float *cz)
{
    int nbonds=0, i=0;
    bond_t *bptr;
    float tr, tg, tb;
    float
	maxx = MINFLOAT,
	maxy = MINFLOAT,
	maxz = MINFLOAT, 
	minx = MAXFLOAT,
	miny = MAXFLOAT,
	minz = MAXFLOAT;

    if (!(infileptr = fopen(filename, "r"))) {
	fprintf(stderr,"%s: can't read bonds file %s\n", ProgName, filename);
	return 0;
	/*DoExit(-1);*/
    }

    fscanf(infileptr,"nbonds %d ", &nbonds);

    bptr = *bonds = (bond_t *) malloc (2 * nbonds * sizeof(bond_t));

    while (fscanf(infileptr," line ") != -1) {
	fscanf(infileptr," %f %f %f %f %f %f %f %f %f %d \n",
		    &(bptr->sx), &(bptr->sy), &(bptr->sz), 
		    &(bptr->ex), &(bptr->ey), &(bptr->ez), 
		    &(bptr->r), &(bptr->g), &(bptr->b),
		    &(bptr->id));
	/* update count for bond type */
	BondTab[bptr->id].nbonds += 1;
	/* update atom bounding box */
	if (bptr->sx < minx) minx = bptr->sx;
	if (bptr->sy < miny) miny = bptr->sy;
	if (bptr->sz < minz) minz = bptr->sz;
	if (bptr->sx > maxx) maxx = bptr->sx;
	if (bptr->sy > maxy) maxy = bptr->sy;
	if (bptr->sz > maxz) maxz = bptr->sz;
	if (bptr->ex < minx) minx = bptr->ex;
	if (bptr->ey < miny) miny = bptr->ey;
	if (bptr->ez < minz) minz = bptr->ez;
	if (bptr->ex > maxx) maxx = bptr->ex;
	if (bptr->ey > maxy) maxy = bptr->ey;
	if (bptr->ez > maxz) maxz = bptr->ez;
	bptr++;
	i++;
	if (i >= 2*nbonds) {
	    fprintf(stderr,"didn't malloc for enough bonds\n");
	    break;
	}
    }

    nbonds = i;


    fclose(infileptr);

    /* set center of atom */
    *cx = (minx + maxx) / 2;
    *cy = (miny + maxy) / 2;
    *cz = (minz + maxz) / 2;

    printf("total bonds: %d\n", nbonds);

    return (nbonds);
}
