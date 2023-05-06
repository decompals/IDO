#include <stdio.h>
#include <sys/syssgi.h>

char *usage = "usage: [-n N]";
extern int    optind;
extern char  *optarg;

main (argc, argv)
int    argc;
char **argv;
{ 
    int    c, setflag, trim;
    char  *pname;

    pname = *(argv+0);
    while ((c = getopt(argc, argv, "n:")) != EOF)
       switch (c) {
            case 'n': 
		trim = strtol(optarg, (char **) 0, 0); 
	        setflag++; 
		break;
            case '?': 
	        fprintf(stderr, "%s: %s\n", pname, usage); 
		exit(2); 
        }
    if (setflag)
      if (syssgi(SGI_SETTIMETRIM, trim) < 0) { 
	perror("syssgi"); 
        exit(3); 
      }
    if (syssgi(SGI_GETTIMETRIM, &trim) < 0) { 
	perror("syssgi"); 
	exit(4); 
    }
    fprintf(stderr, "timetrim is: %d\n", trim); 
    exit(0); 
}
