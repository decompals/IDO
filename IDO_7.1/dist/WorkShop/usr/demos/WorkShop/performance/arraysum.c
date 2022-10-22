#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#define DEFAULT_SIZE 2000

int**
getArray(char* filename, int* _size)
{
        FILE* fd;
        int** p;
        int size = DEFAULT_SIZE;
        struct stat buf;
        int i;
        int nInts;

        /* find the size of the file */
        if (stat(filename, &buf) == -1) {
                *_size = 0;
                return NULL;
        }
        nInts = buf.st_size/sizeof(int);
        if (nInts < size) {
                size = nInts;
        }

        fprintf(stderr, "size %d\n", size);
        p = (int**) malloc(sizeof(int*)*size);
        
        for (i = 0; i < size; i++) {

                p[i] = (int*) malloc(sizeof(int)*size);

                /* open the file */
                fd = fopen(filename, "r");
                if (!fd) {
                        fprintf(stderr, "Internal error: can't open file \"%s\": %s\n", filename, sys_errlist[errno]);
                        *_size = 0;
                        return NULL;
                }


                /* read the integer array in */
                if(!fread(p[i], sizeof(int), size, fd)) {
                        fprintf(stderr, "Internal error: Error in reading first record of graph file %s\n", filename);
                        *_size = 0;
                        return NULL;
                }
                fclose(fd);

        }

        fprintf(stderr, "\nRead in array of size %d by %d\n", size, size);
        *_size = size;
        return p;
}

void
sum1(int**p, int size)
{
        int i, j;
        unsigned int total = 0;
        for (i = 0; i < size; i++) {
                for (j = 0; j < size; j++) {
                        total += p[i][j] * p[j][i];
                }
        }
        fprintf(stderr, "Sum of products of i th row and i th column() is %u\n", total);
}

void
sum2(int**p, int size)
{
        int i, j;
        unsigned int total = 0;
        for (i = 0; i < size; i++) {
                for (j = 0; j < size; j++) {
                        total += p[i][j] * p[j][0];
                }
        }
        fprintf(stderr, "Sum of products of i th row and 0 th column() is %u\n", total);

}

main(int argc, char** argv)
{
        int** p;
        int size;

        p = getArray("TestVector", &size);
        sum1(p, size);
        sum2(p, size);
}



