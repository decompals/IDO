#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <GL/gl.h>
#include <GL/glc.h>
#include <GL/glut.h>


GLuint contextID;
GLuint fontID, fontIDs[100];
GLboolean dl = GL_FALSE;
GLenum drawStyle = GLC_BITMAP;
GLint faceIndex, faceTotal, facesIndex[100], facesTotal[100];
GLfloat rotX = 0.0, rotY = 0.0, rotZ = 0.0, scale = 30.0;
GLint fontIndex, totalFont;
GLint charIndex;


void Init(void)
{
    int i, index;
    char buf[80];
    const char *ptr;

    if ((contextID = glcGenContext()) == 0) {
	printf("Died at glcGenContext\n");
	exit(1);
    }

    if (glcIsContext(contextID) == GL_FALSE) {
	printf("Died at glcIsContext\n");
	exit(1);
    }

    glcContext(contextID);
    if (glcGetError() != GL_NO_ERROR) {
	printf("Died at glcContext\n");
	exit(1);
    }

    if (glcGetCurrentContext() != contextID) {
	printf("Died at glcGetCurrentContext\n");
	exit(1);
    }

    glcPrependCatalog("/usr/lib/X11/fonts/100dpi/fonts.dir");
    if (glcGetError() != GL_NO_ERROR) {
	printf("Died at glcPrependCatalog\n");
	exit(1);
    }

    glcAppendCatalog("/usr/lib/X11/fonts/75dpi/fonts.dir");
    if (glcGetError() != GL_NO_ERROR) {
	printf("Died at glcAppendCatalog\n");
	exit(1);
    }

    glcRemoveCatalog(2);
    if (glcGetError() != GL_NO_ERROR) {
	printf("Died at glcRemoveCatalog\n");
	exit(1);
    }

    glcRemoveCatalog(0);
    if (glcGetError() != GL_NO_ERROR) {
	printf("Died at glcRemoveCatalog\n");
	exit(1);
    }

    fontID = glcGenFontID();
    if (glcGetError() != GL_NO_ERROR) {
	printf("Died at glcGenFontID\n");
	exit(1);
    }

    if (glcNewFontFromFamily(fontID, "Courier") != fontID) {
	printf("Died at glcNewFontFromFamily\n");
	exit(1);
    }
    faceIndex = 0;
    faceTotal = glcGetFonti(fontID, GLC_FACE_COUNT);

    totalFont = glcGeti(GLC_MASTER_COUNT);
    if (glcGetError() != GL_NO_ERROR) {
	printf("Died at glcGeti\n");
	exit(1);
    }
    if (totalFont > 100) {
	totalFont = 100;
    }

    index = 0;
    for (i = 0; i < totalFont; i++) {
	printf("%s\n", glcGetMasterc(i, GLC_FAMILY));
	fontIDs[index] = glcGenFontID();
	if (glcNewFontFromMaster(fontIDs[index], i) != fontIDs[index]) {
	    printf("Died at glcNewFontFromMaster\n");
	    exit(1);
	}
	facesIndex[index] = 0;
	facesTotal[index] = glcGetFonti(fontIDs[index], GLC_FACE_COUNT);
	index++;
    }

    totalFont--;

    fontIndex = 0;
    charIndex = 0;

    glClearColor(0.2, 0.2, 0.2, 0.0);
}

void Reshape(int width, int height)
{

    glViewport(0, 0, width, height);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0.0, (double)width, 0.0, (double)height, -10000.0, 10000.0);
    glMatrixMode(GL_MODELVIEW);
}

void Key(unsigned char key, int x, int y)
{
    int i, j;
    const char *face;

    switch (key) {
	case 'a':
	    rotY -= 1.0;
	    if (drawStyle == GLC_BITMAP) {
		glcDeleteGLObjects();
	    }
	    glutPostRedisplay();
	    break;
	case 'd':
	    rotY += 1.0;
	    if (drawStyle == GLC_BITMAP) {
		glcDeleteGLObjects();
	    }
	    glutPostRedisplay();
	    break;
	case 'w':
	    rotX -= 1.0;
	    if (drawStyle == GLC_BITMAP) {
		glcDeleteGLObjects();
	    }
	    glutPostRedisplay();
	    break;
	case 'x':
	    rotX += 1.0;
	    if (drawStyle == GLC_BITMAP) {
		glcDeleteGLObjects();
	    }
	    glutPostRedisplay();
	    break;
	case 'q':
	    rotZ -= 1.0;
	    if (drawStyle == GLC_BITMAP) {
		glcDeleteGLObjects();
	    }
	    glutPostRedisplay();
	    break;
	case 'z':
	    rotZ += 1.0;
	    if (drawStyle == GLC_BITMAP) {
		glcDeleteGLObjects();
	    }
	    glutPostRedisplay();
	    break;
	case '=':
	    scale += 1.0;
	    if (drawStyle == GLC_BITMAP) {
		glcDeleteGLObjects();
	    }
	    glutPostRedisplay();
	    break;
	case '-':
	    scale -= 1.0;
	    if (drawStyle == GLC_BITMAP) {
		glcDeleteGLObjects();
	    }
	    glutPostRedisplay();
	    break;
	case '1':
	    dl = (dl == GL_FALSE) ? GL_TRUE : GL_FALSE;
	    if (dl == GL_TRUE) {
		glcEnable(GLC_GL_OBJECTS);
	    } else {
		glcDisable(GLC_GL_OBJECTS);
		glcDeleteGLObjects();
	    }
	    glutPostRedisplay();
	    break;
	case '2':
	    if (drawStyle == GLC_BITMAP) {
		drawStyle = GLC_LINE;
	    } else if (drawStyle == GLC_LINE) {
		drawStyle = GLC_TRIANGLE;
	    } else if (drawStyle == GLC_TRIANGLE) {
		drawStyle = GLC_BITMAP;
	    }
	    glcRenderStyle(drawStyle);
	    glcDeleteGLObjects();
	    glutPostRedisplay();
	    break;
	case '3':
	    if (++facesIndex[fontIndex] == facesTotal[fontIndex]) {
		facesIndex[fontIndex] = 0;
	    }
	    face = glcGetFontListc(fontIDs[fontIndex], GLC_FACE_LIST,
			           facesIndex[fontIndex]);
	    glcFontFace(0, face);
	    glutPostRedisplay();
	    break;
	case '4':
	    if (++faceIndex == faceTotal) {
		faceIndex = 0;
	    }
	    face = glcGetFontListc(fontID, GLC_FACE_LIST, faceIndex);
	    glcFontFace(fontID, face);
	    glutPostRedisplay();
	    break;
	case '5':
	    if (glcIsEnabled(GLC_AUTO_FONT) == GL_TRUE) {
		glcDisable(GLC_AUTO_FONT);
	    } else {
		glcEnable(GLC_AUTO_FONT);
	    }
	    glutPostRedisplay();
	    break;
	case '7':
	    if (--charIndex < 0) {
		charIndex = 26 - 1;
	    }
	    glutPostRedisplay();
	    break;
	case '8':
	    if (++charIndex == 26) {
		charIndex = 0;
	    }
	    glutPostRedisplay();
	    break;
	case '9':
	    if (--fontIndex < 0) {
		fontIndex = totalFont - 1;
	    }
	    glutPostRedisplay();
	    break;
	case '0':
	    if (++fontIndex == totalFont) {
		fontIndex = 0;
	    }
	    glutPostRedisplay();
	    break;
	case 27:
	    glcDeleteFont(fontID);
	    glcDeleteContext(contextID);
	    exit(1);
    }
}

void Display(void)
{
    GLfloat v[8];
    GLubyte *ptr, str[128], buf[40];

    glClear(GL_COLOR_BUFFER_BIT);

    if (dl == GL_TRUE) {
	strcpy(str, "Display list, ");
    } else {
	strcpy(str, "Immediate, ");
    }
    if (drawStyle == GLC_BITMAP) {
	strcat(str, "Bitmap, ");
    } else if (drawStyle == GLC_LINE) {
	strcat(str, "Outline, ");
    } else if (drawStyle == GLC_TRIANGLE) {
	strcat(str, "Filled, ");
    }
    strcat(str, glcGetFontFace(fontIDs[fontIndex]));
    sprintf(buf, " (%d/%d), ", facesIndex[fontIndex]+1, facesTotal[fontIndex]);
    strcat(str, buf);
    strcat(str, glcGetFontc(fontIDs[fontIndex], GLC_FAMILY));

    glcFont(fontID);
    if (drawStyle == GLC_BITMAP) {
	glcLoadIdentity();
	glcScale(20.0, 20.0);
	glColor3f(1.0, 1.0, 0.0);
	glRasterPos3f(5.0, 20.0, 0.0);
	glcRenderString(str);
    } else {
	glPushMatrix();
	glTranslatef(5.0, 20.0, 0.0);
	glScalef(20.0, 20.0, 1.0);
	glColor3f(1.0, 1.0, 0.0);
	glcRenderString(str);
	glPopMatrix();
    }

    glcFont(fontIDs[fontIndex]);
    if (drawStyle == GLC_BITMAP) {
	glcLoadIdentity();
	glcRotate(rotZ);
	glcScale(scale, scale);
	glColor3f(1.0, 0.0, 0.0);
	glRasterPos3f(10.0, 100.0, 0.0);
	glcRenderString("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
	glColor3f(1.0, 0.0, 0.0);
	glRasterPos3f(10.0, 200.0, 0.0);
	glcRenderString("abcdefghijklmnopqrstuvwxyz");
	glPushMatrix();
	glTranslatef(10.0, 200.0, 0.0);
	if (glcMeasureString(GL_TRUE, "abcdefghijklmnopqrstuvwxyz") != 0) {
	    glColor3f(0.0, 1.0, 1.0);
	    glcGetStringCharMetric(charIndex, GLC_BOUNDS, v);
	    glBegin(GL_LINE_STRIP);
	    glVertex3f(v[0], v[1], 0.0);
	    glVertex3f(v[2], v[3], 0.0);
	    glVertex3f(v[4], v[5], 0.0);
	    glVertex3f(v[6], v[7], 0.0);
	    glVertex3f(v[0], v[1], 0.0);
	    glEnd();
	}
	glPopMatrix();
	glColor3f(1.0, 0.0, 0.0);
	glRasterPos3f(10.0, 300.0, 0.0);
	glcRenderString("0123456789");
	glPushMatrix();
	glTranslatef(10.0, 300.0, 0.0);
	if (glcMeasureString(GL_TRUE, "0123456789") != 0) {
	    glColor3f(0.0, 1.0, 1.0);
	    glcGetStringMetric(GLC_BOUNDS, v);
	    glBegin(GL_LINE_STRIP);
	    glVertex3f(v[0], v[1], 0.0);
	    glVertex3f(v[2], v[3], 0.0);
	    glVertex3f(v[4], v[5], 0.0);
	    glVertex3f(v[6], v[7], 0.0);
	    glVertex3f(v[0], v[1], 0.0);
	    glEnd();
	}
	glPopMatrix();
    } else {
	glPushMatrix();
	glRotatef(rotX, 1.0, 0.0, 0.0);
	glRotatef(rotY, 0.0, 1.0, 0.0);
	glRotatef(rotZ, 0.0, 0.0, 1.0);
	glPushMatrix();
	glTranslatef(10.0, 100.0, 0.0);
	glScalef(scale, scale, 1.0);
	if (drawStyle == GLC_LINE) {
	    glColor3f(0.0, 1.0, 0.0);
	} else if (drawStyle == GLC_TRIANGLE) {
	    glColor3f(0.0, 0.0, 1.0);
	}
	glcRenderString("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
	glPopMatrix();
	glPushMatrix();
	glTranslatef(10.0, 200.0, 0.0);
	glScalef(scale, scale, 1.0);
	if (drawStyle == GLC_LINE) {
	    glColor3f(0.0, 1.0, 0.0);
	} else if (drawStyle == GLC_TRIANGLE) {
	    glColor3f(0.0, 0.0, 1.0);
	}
	glcRenderString("abcdefghijklmnopqrstuvwxyz");
	glPopMatrix();
	glPushMatrix();
	glTranslatef(10.0, 200.0, 0.0);
	glScalef(scale, scale, 1.0);
	if (glcMeasureString(GL_TRUE, "abcdefghijklmnopqrstuvwxyz") != 0) {
	    glcGetStringCharMetric(charIndex, GLC_BOUNDS, v);
	    glColor3f(0.0, 1.0, 1.0);
	    glBegin(GL_LINE_STRIP);
	    glVertex3f(v[0], v[1], 0.0);
	    glVertex3f(v[2], v[3], 0.0);
	    glVertex3f(v[4], v[5], 0.0);
	    glVertex3f(v[6], v[7], 0.0);
	    glVertex3f(v[0], v[1], 0.0);
	    glEnd();
	}
	glPopMatrix();
	glPushMatrix();
	glTranslatef(10.0, 300.0, 0.0);
	glScalef(scale, scale, 1.0);
	if (drawStyle == GLC_LINE) {
	    glColor3f(0.0, 1.0, 0.0);
	} else if (drawStyle == GLC_TRIANGLE) {
	    glColor3f(0.0, 0.0, 1.0);
	}
	glcRenderString("0123456789");
	glPopMatrix();
	glPushMatrix();
	glTranslatef(10.0, 300.0, 0.0);
	glScalef(scale, scale, 1.0);
	if (glcMeasureString(GL_TRUE, "0123456789") != 0) {
	    glcGetStringMetric(GLC_BOUNDS, v);
	    glColor3f(0.0, 1.0, 1.0);
	    glBegin(GL_LINE_STRIP);
	    glVertex3f(v[0], v[1], 0.0);
	    glVertex3f(v[2], v[3], 0.0);
	    glVertex3f(v[4], v[5], 0.0);
	    glVertex3f(v[6], v[7], 0.0);
	    glVertex3f(v[0], v[1], 0.0);
	    glEnd();
	}
	glPopMatrix();
	glPopMatrix();
    }

    if (glcGetError() != GL_NO_ERROR) {
	printf("Got a GLC error\n");
    }

    glutSwapBuffers();
}

void main(int argc, char **argv)
{

    glutInitWindowSize(700, 500);
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE);
    glutCreateWindow("glc test");
    Init();
    glutDisplayFunc(Display);
    glutReshapeFunc(Reshape);
    glutKeyboardFunc(Key);
    glutMainLoop();
}
