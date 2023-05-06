/* mycp.c         1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* Quick lash up to test homebrew rcp using sockets.        */
/*                                                          */
/* 'r' commands protocol is reverse engineered by watching  */
/*     what real systems do.                                */
/*                                                          */
/* Socket stuff comes from the IPC Primer "Networking on    */
/*   the Sun Workstation".                                  */
/* Don't expect this to connect to a real Unix shell or     */
/*   login service.  They are more security concious and    */
/*   require you to be bound to a priveleged port on the    */
/*   local machine.                                         */
/*                                                          */
/*           cc -g mycp.c -lds -o mycp                      */
 
#include <stdio.h>
#include <netdb.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#define CHANNELS 2	/* 3 for RGB */
#define PIXELS	720
#define LINES	486	/* make this 576 for 625 */

#define DEBUG

char login_str[] = "\0simon\0simon\0vt100";	/* rlogin (no echo) */
char shell_str[] = "\0simon\0simon";		/* rsh */

openSession(char *service, char *host)
{
struct sockaddr_in skt;
struct servent *rlogin_service;
struct hostent *a60;
int sd, n;
char tmp_str[80];

if((rlogin_service = getservbyname(service, "tcp")) == NULL) 
    {
    fprintf(stderr, "demo : tcp: unknown service\n");
    exit(1);
    }
if((a60 = gethostbyname(host)) == NULL) 
    {
    fprintf(stderr, "demo : a60 : unknown host\n");
    exit(1);
    }
bzero((char *)&skt, sizeof(skt));
bcopy(a60->h_addr, (char *)&skt.sin_addr, a60->h_length);
skt.sin_addr.s_addr = INADDR_ANY;

if((sd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
    perror("demo : socket");
    exit(3);
    }

bzero((char *)&skt, sizeof(skt));
bcopy(a60->h_addr, (char *)&skt.sin_addr, a60->h_length);
skt.sin_family = a60->h_addrtype;
skt.sin_port = rlogin_service->s_port;
if((connect(sd, (char *)&skt, sizeof(skt))) < 0)
    {
    perror("demo : connect");
    exit(3);
    }

#ifdef DEBUG 
printf("OK so far\n");
#endif

if(!strcmp(service, "login"))  /* sizeof() includes the embedded nulls */
    write(sd, login_str, sizeof(login_str)); 
else write(sd, shell_str, sizeof(shell_str)); 

n = read(sd, tmp_str, 20);

#ifdef DEBUG
printf("read %d\n", n);
#endif

return sd;
}

rcpTo(FILE *piccy, int sd, char *name, int frame)
{
char line[PIXELS*CHANNELS];
char str[80], tmp_str[20];
int i, n;

sprintf(str, "rcp -t %d\n", frame);
write(sd, str, strlen(str));
n = read(sd, tmp_str, 20);

#ifdef DEBUG
printf("read %d\n", n);
#endif

sprintf(str, "C0666 %d %s\n", CHANNELS*PIXELS*LINES, name);
write(sd, str, strlen(str));
n = read(sd, tmp_str, 20);

#ifdef DEBUG
printf("read %d\n", n);
#endif
for(i=0; i<LINES; i++)
    {
    fread(line, PIXELS*CHANNELS, 1, piccy);
    write(sd, line, PIXELS*CHANNELS);
    }
write(sd, "", 1); 			/* send the final null */
n = read(sd, tmp_str, 20);

#ifdef DEBUG
printf("read %d\n", n);
#endif
}

rcpFrom(int sd, FILE *piccy, int frame)
{
char line[PIXELS*CHANNELS];
char str[80], tmp_str[30];
int i, n, chunk, length;

sprintf(str, "rcp -f %d\n", frame);
write(sd, str, strlen(str));
write(sd, "", 1); 			/* send a null */
n = read(sd, tmp_str, 30);		/* get the file info */

#ifdef DEBUG
printf("read %d\n", n);
#endif

write(sd, "", 1); 			/* send a null to start transfer */

i = 0;
length = LINES*PIXELS*CHANNELS; /* should really get length from file info */
while(i<length) 
    {
    if((length - i) < PIXELS*CHANNELS)
    	chunk = read(sd, line, (length - i));/* last chunklet */
    else chunk = read(sd, line, PIXELS*CHANNELS); /* ..otherwise read a line */
    fwrite(line, chunk, 1, piccy);
    i += chunk;
    }

n = read(sd, tmp_str, 20);		/* read the null */

#ifdef DEBUG
printf("read %d\n", n);
#endif
write(sd, "", 1); 			/* .. acknowlege it */
}

main(argc, argv)
int argc;
char **argv;
{
int socket, n, count, frame;
FILE *file;
char line[PIXELS*CHANNELS];

/* copy to a60
socket = openSession("shell", "a60");
if(!(file = fopen(argv[1], "r")))
    {
    printf("Unable to open %s\n", argv[1]);
    exit(1);
    }
rcpTo(file, socket, argv[1], atoi(argv[2]));
*/

/* copy from a60
socket = openSession("shell", "a60");
if(!(file = fopen(argv[2], "w")))
    {
    printf("Unable to open %s\n", argv[1]);
    exit(1);
    }
rcpFrom(socket, file, atoi(argv[1]));
*/

/* copy multiple to a60 */

count = atoi(argv[3]);
frame = atoi(argv[2]);
socket = openSession("login", "a60");

if(!(file = fopen(argv[1], "r")))
    {
    printf("Unable to open %s\n", argv[1]);
    exit(1);
    }
rewind(file);

for(n=0;n<count;n++)
    {
    rcpTo(file, socket, argv[1], frame++);
    }

}
