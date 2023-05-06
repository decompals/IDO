/* 
 *   cg2.c:
 *
 * set the de3 to RS 170, genlock
 * set the cg2 to mode 2, standalone master
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <gl/cg2vme.h>
#include <gl/addrs.h>

main()
{
    noport();
    foreground();   /* need this else getchar() at end will go to la la land */
    winopen("cg2");
    setvideo(DE_R1,(long) 0);                               /* reset the de3 */
    setvideo(DE_R1,DER1_G_170 | DER1_UNBLANK);  /* send 0xa8, genlocked RS170,
                                                   no blanking */
    setvideo(CG_MODE,0x2);                          /* set the cg2 to mode 2 */
    printf("mode %x\n",(long) getvideo(CG_MODE));
    getchar();
    setvideo(DE_R1,DER1_60HZ | DER1_SYNCG | DER1_UNBLANK); /* return to 60HZ */
}
