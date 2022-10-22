#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define OPEN_ERR         1
#define NOT_ENOUGH_BYTES 2
#define SIZE_0 	         3

int copy_file();

main (int argc, char *argv[])
{
    int bytes, status;

    if( argc < 4){
        printf("copyn: Insufficient arguments.\n");
        printf("Usage: copyn f1 f2 bytes\n");
        exit(1);
    }
    if( argc > 4 ) {
        printf("Error: Too many arguments\n");
        printf("Usage: copyn f1 f2 bytes\n");
        exit(1);
    }
    bytes = atoi(argv[3]);
    if(( status = copy_file(argv[1], argv[2], bytes)) >0){
       switch ( status) {
           case SIZE_0:
               printf("Nothing to copy\n");
               break;
           case NOT_ENOUGH_BYTES:
               printf("Not enough bytes\n");
               break;
           case OPEN_ERR:
               printf("File open error\n");
               break;
       }
       exit(1);
    }
}

int copy_file( source, destn, size)
char *source, *destn;
int size;
{
    char *buf;
    int fd1, fd2;
    struct stat fstat;
    if( (fd1 = open( source, O_RDONLY)) <= 0){
        return OPEN_ERR;
    }
    stat( source, &fstat);
    if( size <= 0){
        return SIZE_0;
    }
    if( fstat.st_size < size){
        return NOT_ENOUGH_BYTES;
    }
    if( (fd2 = creat( destn, 00777)) <= 0){
        return OPEN_ERR;
    }
    buf = (char *)malloc(size);
    
   read( fd1, buf, size);
   write( fd2, buf, size);
   return 0;
}
