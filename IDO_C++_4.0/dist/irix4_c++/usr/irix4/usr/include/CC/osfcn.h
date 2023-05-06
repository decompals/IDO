#ifndef __OSFCN_H
#define __OSFCN_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __TYPES_H
#include <sys/types.h>
#endif

#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>

struct fd_set;
struct timeval;

    void sginap(int);
   	void abort(void);
   	int accept(int, void *, int *);
	int acct(const char*);
	int bind(int, const void *, int);
	int chdir(const char*);
	int chroot(const char*);
	int connect(int, const void *, int);
	void exit(int);
    int gethostname(char *, int);
	int ioctl(int, int, ...);
	int kill(int, int);
	int listen(int, int);
	int nice(int);
	void sync(void);
	long ulimit(int, long);
	int rmount(const char *,const char *, const char *, int);
	int mount(const char*, const char*, int);
	void profil(const char*, int, int, int);
	int ptrace(int, int, int, int);
    int readlink(char*, char*, int);
	int openpl(void);
	int closepl(void);
	void sys3b(int, int, ...);
	int mknod(const char*, int, int);
	int brk(char*);
	int plock(int);
	int setsockopt(int, int, int, const void*, int);
	int stime(const long*);
	int umount(const char*);
	int wait(int*);
	char *sbrk(int);
	int sigpause(int);
	int select(int, fd_set*, fd_set*, fd_set*, struct timeval*);
    int sethostname(const char *, int);
	int socket(int, int, int);
	mode_t umask(mode_t);
	int mkdir(const char*, mode_t);
	int chmod(const char*, mode_t);
#ifdef __cplusplus
}
#endif

#endif





