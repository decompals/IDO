/*
 *	rand -
 *		Generate high quality randow numbers.  This wrapper
 *	uses Berkeley random.
 *
 *				Paul Haeberli - 1987
 */
float frand()
{
    return (random() % 10000)/10000.0;
}
