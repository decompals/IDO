/*ident	"@(#)ctrans:incl-master/proto-headers/libc.h	1.7" */
#ifndef __LIBC_H
#define __LIBC_H

#ifndef __PWD_H
#include <pwd.h>
#endif

#ifndef __RAND48_H
#include <rand48.h>
#endif

#ifndef __REGCMP_H
#include <regcmp.h>
#endif


#ifndef __ERRNO_H
#include <errno.h>
#endif

extern "C" {
	double atof(const char*);
	int atoi(const char*);
	long atol(const char*);
	const char* crypt(const char*, const char*);
	char *ecvt(double, int, int*, int*);
	void encrypt(char*, int);
	void exit(int);
	double fabs(double);
	const char* fcvt(double, int, int*, int*);
	double frexp(double, int*);
	char *gcvt(double, int, char*);
	char *getenv(const char*);
	char *getlogin();
	char *getpass(const char*);
	int getpw(int, char*);
	int isatty(int);
	double ldexp(double, int);
	double modf(double, double*);
	char *mktemp(char*);
	void perror(const char*);
	void qsort(void*, unsigned, unsigned, int(*)(const void*, const void*));
	void setkey(const char*);
	void swab(const char*, char*, int);
	int system(const char*);
	long time(long*);
	char *tmpnam(char*);
	char *ttyname(int);
	int ttyslot();
	char *ctermid(char*);
	char *cuserid(char*);
	char *tempnam(const char*, const char*);
	int getopt(int, char**, char*);
	long a64l(const char*);
	char *getcwd(char*, int);
	void l3tol(long*, const char*, int);
	char *l64a(long);
	void ltol3(char*, const long*, int);
	int putenv(const char*);
	int crypt_close(int[]);
	char *des_crypt(char *, char *);
	void des_encrypt(char *, int);
	void des_setkey(char *);
	int getmsg(int,struct strbuf *, struct strbuf *, int);
	int run_crypt(long, char *, unsigned int, int *);
	int run_setkey(int *, char *) ;
	unsigned sleep(unsigned);
	void srand(unsigned);
	void abort();
	long sgetl(char*);
	void sputl(long, char*);
	int rand();
	double strtod(const char*, char**);
	long strtol(const char*, char**, int);
	void bzero(void *, int);
	long bcopy(const void *, void *, unsigned long );
	int bcmp(const void *, const void *, unsigned long );

extern char *optarg;
extern int optind, opterr;

}

#endif

