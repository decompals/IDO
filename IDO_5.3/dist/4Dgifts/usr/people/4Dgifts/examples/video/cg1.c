/* 
 *   cg1.c:
 *
 * set the graphics control register to RS170, genlocked
 * set the cg2 to mode 1, lock to composite sync or composite monochrome.
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
    winopen("cg1");
    setvideo(DE_R1,(long) 0);      /* reset de3  or RV1 or GR1 graphics board*/
    setvideo(DE_R1,DER1_G_170 | DER1_UNBLANK);  /* send 0xA8, genlocked RS170,
                                                   no blanking */
    setvideo(CG_MODE,0x1);                            /* set cg2 into mode 1 */
    printf("mode %x\n",(long) getvideo(CG_MODE));
    getchar();
    setvideo(DE_R1,DER1_60HZ | DER1_SYNCG | DER1_UNBLANK); /* return to 60HZ */
}
