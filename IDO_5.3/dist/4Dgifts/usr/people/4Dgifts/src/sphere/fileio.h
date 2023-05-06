/*
 * filio.h
 */

extern int readatoms(char *filename, atom_t **atoms, 
			float *cx, float *cy, float *cz);
extern int readbonds(char *filename, bond_t **bonds, 
			float *cx, float *cy, float *cz);
