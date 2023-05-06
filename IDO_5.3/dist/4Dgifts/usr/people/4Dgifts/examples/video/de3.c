/* 
 *   de3.c:
 *
 * set the DE3 or RV1 or GR1 to RS170, RGB output, sync on green
 * do not set the cg2 (a 0x7 can be sent to the cg2 to turn it off)
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <gl/cg2vme.h>
#include <gl/addrs.h>

main()
{
    noport();
    foreground();         /* used so getchar() does not fall into la la land */
    winopen("DE3 or RV1 or GR1");
    setvideo(DE_R1,(long) 0);                 /* reset the DE3 or RV1 or GR1 */

  /* send 0x2a, RS170 RGB sync on green 
   * The cg2 is not involved in this operation 
   * Included here is an example of using the bit-wise OR operator and
   * the defined variables in the include files #include'd above
   * DER1_170 = 0x20        00100000
   * DER1_SYNCG = 0x02      00000010  sync on green output and sync
   *                                  output 
   * DER1_UNBLANK = 0x08    00001000
   *                        --------
   *                        00101010 = 0x2a (0x28 should work for 
   *                                         externally sync'd devices)
   */
    setvideo(DE_R1,DER1_170|DER1_SYNCG|DER1_UNBLANK);    /* send 0x2a to de3 */
    printf("DE3 mode %x\n",(long) getvideo(DE_R1));
    getchar();
    setvideo(DE_R1,DER1_60HZ | DER1_SYNCG | DER1_UNBLANK); /* return to 60HZ */
}
