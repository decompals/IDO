#include	<string.h>

void main (int argc, char **argv)
{
    char *str;
    int **array, *bogus, value;

    /* Let us malloc 3 bytes */
    str = (char *) malloc(strlen("bad"));

    /* The following statement writes 0 to the 4th byte */
    strcpy(str, "bad");

    free (str);

    /* Let us malloc 100 bytes */
    str = (char *) malloc(100);
    array = (int **) str;

    /* Get an uninitialized value */
    bogus = array[0];

    free (str);
    /* The following is a double free */
    free (str);

    /* The following statement uses the uninitialized value as a pointer */
    value = *bogus;
}
