/* 
 *   pipe2way.c:
 *
 *    pipe2way employs bi-directional pipes between a parent process and 
 *  its child.  This example is NOT a replacement or an equal to SCRIPT(1).
 *  It will fail with many programs that are interactive and know about
 *  terminal modes.
 *
 *  References:  PIPE(2), FOPEN(3S), DUP(2), EXECL(2), GETENV(3C), TIME(1),
 *               CTIME(3C), PERROR(3C) 
 *
 *                                     Grant Dorman - 1990
 */

#include <stdio.h>

FILE *fd;
char *getenv(), *ctime(), buf[BUFSIZ], *shell, *fname;
int  aflg, qflg, nread, pd[2];
long tloc = 1, time();

main (argc, argv)
int    argc;
char **argv;
{
    parse(argc, argv);
    switch (fork()) { 
	case -1:
	    perror("fork"); 
	    exit(1);
	    break;
        case  0: 
	    do_shell();
        default: 
	    do_stdout(); 
	    break;
    }
}


parse (argc, argv)
int    argc;
char **argv;
{
    if (pipe(pd) == -1) { 
	perror("pipe"); 
	exit(2); 
    }
    while (argc--, argv++, argc > 0 && argv[0][0] == '-') { 
	switch (argv[0][1]) { 
	    case 'a': 
		aflg++; 
		break;
            case 'q': 
		qflg++; 
		break;
            default:  
		fprintf(stderr, "pipe2way [-a] [-q] [filenm]\n"); 
		exit(3); 
		break;
        }
    }
    if ((shell = getenv("SHELL")) == 0) 
	shell = "/bin/sh";
    fname = (argc > 0) ? argv[0] : "pipescript";
    if ((fd = fopen(fname, aflg ? "a" : "w")) == NULL) { 
	perror("open"); 
	exit(4); 
    }
}


do_stdout ()
{
    close(0); 
    dup(pd[0]); 
    close(pd[1]); 
    close(pd[0]);
    if (!qflg) { 
	time(&tloc); 
        fprintf(stderr,"Pipe2way started, file is %s\n", fname);
        fprintf(fd,"Pipe2way started on %s", ctime(&tloc)); 
    }
    while ((nread = read(0, buf, sizeof buf)) != 0) { 
	write(1, buf, nread); 
	fwrite(buf, 1, nread, fd); 
    }
    if (!qflg) { 
	time(&tloc);
        fprintf(stderr,"Pipe2way done, file is %s\n", fname);
        fprintf(fd,"\nPipe2way done on %s", ctime(&tloc)); 
    }
}


do_shell ()
{
    close(1); 
    dup(pd[1]);
    close(2); 
    dup(pd[1]);
    close(pd[0]); 
    close(pd[1]); 
    fclose(fd);
    execl(shell, shell, "-xi", NULL);
    perror("execl"); 
    exit(5);
} 
