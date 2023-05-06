/* serial.c       1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* Serial.c is one of two files which form the basis of an  */
/* RS-232 serial control interface for the A60 or A64 from  */
/* an SGI 4D.                                               */
/*                                                          */
/* BUGS                                                     */
/* For reasons which I haven't yet investigated the current */
/* response is always for the previous command. (Presumably */
/* read doesn't wait for very long before returning.)       */
/*            - a sleep might fix it!                       */

#include <fcntl.h>
#include <termio.h>
#include <errno.h>

extern int decodeReply();

int fd;
char buffer[128];
char *pointer;

/* serial open
 * takes path of serial port to use eg "/dev/ttyd2"
 */

int serialOpen(path)
char *path;
{
struct termio termio;

fd = open(path, O_RDWR);
if(fd < 0)
    {
    perror("ser_open");
    return 0;
    }

/* set tty to 9600 baud 8 bits even parity two stop bits */

termio.c_iflag = 0;
termio.c_oflag = 0;
termio.c_cflag = B9600 | CS8 | CSTOPB | CREAD | PARENB | CLOCAL;
termio.c_lflag = 0;
termio.c_line = 0;
termio.c_cc[0] = 0;

ioctl(fd, TCSETA, &termio);

pointer = &buffer[1];			/* initialise the buffer */
return 1;
}

/* serial command routines
 * build up a command packet and take care of byte swapping
 * serial command is most commmon - command byte followed by
 * long int of which the lower three bytes are sent 
 */

serialCommand(cmd, arg)
unsigned char cmd;
int arg;
{
*pointer++ = cmd;
*pointer++ = arg & 0xFF;
*pointer++ = arg >> 8 & 0xFF;
*pointer++ = arg >> 16 & 0xFF;
}
	
serialSend()
{
*pointer++ =  '\r';			/* carriage return to terminate */
*buffer = (char) (pointer - buffer);	/* the packet length */
write(fd, buffer, buffer[0]);		/* send it */
pointer = &buffer[1];			/* leave space for the count */
}

serialRecv()
{
char input[128], *ptr;
int count, i;

ptr = input;
input[0] = 0;
count = 0;
do  { /* in some cases need to do multiple reads if \r occurs mid packet */
    count += read(fd, &input[count], 128);
    } while(count < input[0]);

for(i=0; i<count; i++)
    printf("%02X ", *ptr++);
printf("\n");
decodeReply(input);
}

stop()
{
serialCommand(0x10, 0);
serialSend();
serialRecv();
}

play(speed)
int speed;
{
serialCommand(0x15, speed);
serialSend();
serialRecv();
}

freeze()
{
serialCommand(0x30, 0x12);
serialSend();
serialRecv();
}

unfreeze()
{
serialCommand(0x49, 0);
serialSend();
serialRecv();
}

video_in(inp)
int inp;
{
serialCommand(0x2C, inp<<7);
serialCommand(0x30, 1);
serialSend();
serialRecv();
}

pattern(patt)
int patt;
{
serialCommand(0x71, 0x11 | (patt << 8));
serialSend();
serialRecv();
}

