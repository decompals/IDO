#include <gl/gl.h>

#define MAXBLINKS	20	/* maximum number of blinking entries */
#define FIRSTBLINKCI	64	/* avoid the first 64 colors */

main()
{
    int i;
    short red[MAXBLINKS], blue[MAXBLINKS], green[MAXBLINKS];

    prefsize(400, 400);
    winopen("blinker");
    ortho2(-0.5, 20.0*MAXBLINKS + 9.5, -0.5, 500.5);
    color(BLACK);
    clear();

    for (i = MAXBLINKS - 1; i >= 0 ; i--) /* save current color information */
	getmcolor( i, &red[i], &blue[i], &green[i]);

    for (i = MAXBLINKS - 1; i >= 0 ; i--) {
	color(i + FIRSTBLINKCI);
	sboxfi(i*20 + 10, 10, i*20 + 20, 490);
	blink(i + 1, i + FIRSTBLINKCI, 255, 0, 0);
    }
    sleep(10);
    blink(-1, 0, 0, 0, 0);	/* stop all blinking */

    for (i = MAXBLINKS - 1; i >= 0 ; i--)
	mapcolor(i, red[i], blue[i], green[i]); /* restore colormap */

    gflush(); /* flush color map requests to the server */
    gexit();
    return 0;
}
