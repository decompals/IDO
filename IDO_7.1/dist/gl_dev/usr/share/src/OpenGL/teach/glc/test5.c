#include <stdio.h>
#include <GL/glc.h>


int main(void)
{
    GLint i, j, masterCount, charCount;

    glcContext(glcGenContext());

    masterCount = glcGeti(GLC_MASTER_COUNT);
    for (i = 0; i < masterCount; i++) {
        printf("GLC_FAMILY = \"%s\" ", glcGetMasterc(i, GLC_FAMILY));
        printf("GLC_VENDOR = \"%s\"\n", glcGetMasterc(i, GLC_VENDOR));
        charCount = glcGetMasteri(i, GLC_CHAR_COUNT);
        printf("  char count = %d\n", charCount);
        for (j = 0; j < charCount; j++) {
            printf("    %d, \"%s\"\n", j+1,
		   glcGetMasterListc(i, GLC_CHAR_LIST, j));
        }
        printf("\n");
    }
    return 0;
}
