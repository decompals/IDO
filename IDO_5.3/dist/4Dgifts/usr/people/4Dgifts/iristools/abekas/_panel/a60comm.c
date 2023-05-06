/*
 ****************************************************************
 * VERSION 2.0							*
 * September 1990						*
 ****************************************************************
 *
 * a60comm.c
 * copyright 1991 Rhonda Graphics Inc.
 *		  2235 W. Alice
 *		  Phoenix, AZ 85021
 *		  602-371-8880
 *
 * A60 communications module for Abekas Control Panel.
 * This module contains the functions that communicate
 * with the A60 over the network. Most (if not all) of
 * this code was ripped directly from the Abekas
 * A60 Ethernet Manual. 
 *
 * NOTE: to compile the -I/usr/include/bsd flag must
 * be used.
 *
 * VERSION 2.0 adds a time limit to the connect system call.
 *
 */

#include <stdio.h>
#include <netdb.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <signal.h>


int sd;
char login[] = "\0simon\0simon\0sun\0";



/****************************************************************
 * open_connection - establish a remote connection via TCP	*
 *	returns 1 if successful; 0 if not.			*
 ****************************************************************/

open_connection()
{
  struct sockaddr_in skt;
  struct servent *rlogin_service;
  struct hostent *a60;
  int iport;
  char c,tmp_str[80],str[80];
  extern int errno;

  if ((rlogin_service = getservbyname("login", "tcp")) == NULL)
	{
	fprintf(stderr,"ERROR: tcp: unknown service\n");
	exit(1);
	}

  if ((a60 = gethostbyname("a60")) == NULL)
	{
	fprintf(stderr,"ERROR: a60: unknown host\n");
	exit(1);
	}

  bzero((char *)&skt, sizeof(skt));
  bcopy(a60->h_addr, (char *)&skt.sin_addr, a60->h_length);
  skt.sin_addr.s_addr = INADDR_ANY;

  if ((sd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	{
	perror("ERROR: socket");
	exit(3);
	}

  bzero((char *)&skt, sizeof(skt));
  bcopy(a60->h_addr, (char *)&skt.sin_addr, a60->h_length);
  skt.sin_family = a60->h_addrtype;
  skt.sin_port = rlogin_service->s_port;

  catchsig();
  alarm(3);

  if ((connect(sd, (char *)&skt, sizeof(skt))) < 0)
	{
	if (errno == EINTR)
		{
		alarm(0);
		return(0);
		}
	else
		{
		perror("ERROR: connect");
		exit(3);
		}
	}

  alarm(0);

  write(sd, login, sizeof(login));
  return(1);
}



/****************************************************************
 * catchsig - catch the alarm signal				*
 ****************************************************************/

static int catchsig()
{
  if (signal(SIGALRM,catchsig) == SIG_ERR)
	fprintf(stderr,"ERROR: signal\n");
}



/****************************************************************
 * close_connection - close a remote connection 		*
 ****************************************************************/

close_connection()
{
  close(sd);
}



/****************************************************************
 * send_command - send a command to a remote connection		*
 ****************************************************************/

send_command(str)
char *str;
{
  write(sd, str, strlen(str));
}



/****************************************************************
 * get_command - get a command from a remote connection		*
 ****************************************************************/

get_command(str)
char *str;
{
  int i;
  int n;
  int val;
  static char tmp_str[80];
  unsigned char v100,v10,v1;

  write(sd, str, strlen(str));
/*
  fprintf(stderr,"where? ");
*/

  n = read(sd, tmp_str, 80);

/*
  fprintf(stderr,"n= %d  ",n);
  for (i = 0; i < n; i++)
	fprintf(stderr,"%x ",tmp_str[i]);
  fprintf(stderr,"\n");
*/
  if (n == 5)
	{
	v100 = tmp_str[0] & 0x0f;
	v10 = tmp_str[1] & 0x0f;
	v1 = tmp_str[2] & 0x0f;
	}
  else if (n == 4)
	{
	v100 = 0;
	v10 = tmp_str[0] & 0x0f;
	v1 = tmp_str[1] & 0x0f;
	}
  else if (n == 3)
	{
	v100 = 0;
	v10 = 0;
	v1 = tmp_str[0] & 0x0f;
	}
  else
	{
	v100 = 0;
	v10 = 0;
	v1 = 0;
	}
  val = v100 * 100 + v10 * 10 + v1;
/*
  fprintf(stderr,"val= %d ",val);
*/
  return(val);
}


