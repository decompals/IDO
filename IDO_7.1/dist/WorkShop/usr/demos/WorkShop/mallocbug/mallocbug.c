void foo (int i)
{
    char *c = (char *) malloc (i * 10);
    char *d = (char *) malloc (i * 20);

    free (c);
    free (c);
}

void main (int argc, char **argv)
{
    int i;

    for (i = 1; i < 100; i++) {
	foo (i);
    }
}
