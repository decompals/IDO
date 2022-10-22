/****************************************************************
 *  purpose --
 *	This program is going to reorder the input number and
 *	print them out in the order of min to max.
 *
 *  input --
 *	an array of numbers separated by space
 *****************************************************************
 */

#include <stdio.h>

main (int argc, char **argv)
{
    int *intArray, a, i, j;

    if(argc < 2) {
	printf ("Error: no number to compare.\n");
	exit(1);
    }

    /* create the list of integers */
    intArray = (int *) malloc (sizeof(int)*(argc-1));
    for (i=1;i<argc;i++) {
	if(sscanf (argv[i], "%d", &intArray[i-1]) != 1) {
	    printf ("Error: invalid input.\n");
	    exit(1);
	}
    }

    /* reorder */
    for (i=0;i<argc-1;i++) {
	for(j=i+1;j<argc-1;j++) {
	    if(intArray[j] < intArray[i]) {
		a = intArray[i];
		intArray[i] = intArray[j];
		intArray[j] = a;
	    }
  	}
	printf ("%d\n", intArray[i]);
    }
    free (intArray);
}

