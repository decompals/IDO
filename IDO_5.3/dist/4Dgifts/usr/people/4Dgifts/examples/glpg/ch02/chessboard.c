#include <gl/gl.h>

#define ODD(n)	((n) % 2)

main()
{
    int i, j;

    prefsize(400, 400);
    winopen("chessboard");
    color(GREEN);
    clear();
    for (i = 0; i < 8; i++) {
	for (j = 0; j < 8; j++) {
	    if (ODD(i + j))
		color(WHITE);
	    else
		color(BLACK);
	    sboxfi(100 + i*25, 100 + j*25, 124 + i*25, 124 + j*25);
	}
    }
    color(RED);
    recti(97, 97, 302, 302);
    sleep(10);
    gexit();
    return 0;
}

