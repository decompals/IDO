#include <GL/glc.h>
#include <stdio.h>

int main(void) {
    GLint i, masterCount;

    glcContext(glcGenContext());
    masterCount = glcGeti(GLC_MASTER_COUNT);
    for (i = 0 ; i < masterCount ; ++i) {
        GLint charCount, j;

        glcNewFontFromMaster(1, i);
        printf("GLC_FAMILY = \"%s\" ", glcGetFontc(1, GLC_FAMILY));
        printf("GLC_VENDOR = \"%s\"\n", glcGetFontc(1, GLC_VENDOR));
        charCount = glcGetFonti(1, GLC_CHAR_COUNT);
        printf("  char count = %d\n", charCount);
        for (j = 0 ; j < charCount ; ++j) {
            printf(
                "    %d, %s\n", j+1, glcGetFontListc(1, GLC_CHAR_LIST, j)
            );
        }
        printf("\n");
        glcDeleteFont(1);
    }
    printf("Error = %d\n", glcGetError());
    return 0;
}
