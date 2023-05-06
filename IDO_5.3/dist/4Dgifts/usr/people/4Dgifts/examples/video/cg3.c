/* 
 *   cg3.c:
 *
 * set the de3 to RS170, genlock
 * set the cg2 to mode 3, lock to incoming composite video
 * Program will switch to the alternate video mode, the screen will go blank
 * and program will wait for any character to be pressed before switching back
 * to the 60hz console.
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <gl/cg2vme.h>
#include <gl/addrs.h>

main()
{
    noport();
    foreground();   /* need this else getchar() at end will go to la la land */
    winopen("cg3");
    setvideo(DE_R1,(long) 0);      /* reset DE3 or RV1 or GR1 graphics board */
    setvideo(DE_R1,DER1_G_170 | DER1_UNBLANK);  /* send 0xa8, genlocked RS170,
                                                   no blanking */
    setvideo(CG_MODE,0x3);                              /* set cg2 to mode 3 */
    printf("mode %x\n",(long) getvideo(CG_MODE));
    getchar();
    setvideo(DE_R1,DER1_60HZ | DER1_SYNCG | DER1_UNBLANK); /* return to 60HZ */

}
