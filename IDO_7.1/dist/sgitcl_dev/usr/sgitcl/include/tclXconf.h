/* src/tclXconf.h.  Generated automatically by configure.  */
/* src/tclXconf.h.in.  Generated automatically from configure.in by autoheader.  */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef gid_t */

/* Define if you have the strftime function.  */
#define HAVE_STRFTIME 1

/* Define if your struct tm has tm_zone.  */
/* #undef HAVE_TM_ZONE */

/* Define if you don't have tm_zone but do have the external array
   tzname.  */
#define HAVE_TZNAME 1

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef mode_t */

/* Define to `long' if <sys/types.h> doesn't define.  */
/* #undef off_t */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef pid_t */

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
/* #undef size_t */

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
#define TIME_WITH_SYS_TIME 1

/* Define if your <sys/time.h> declares struct tm.  */
/* #undef TM_IN_SYS_TIME */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef uid_t */

/* Define if you don't have sys/socket.h */
/* #undef NO_SYS_SOCKET_H */

/* Define if you have and need sys/select.h */
/* #undef HAVE_SYS_SELECT_H */

/* Define if don't you have values.h */
/* #undef NO_VALUES_H */

/* Define if your struct tm has tm_zone.  */
/* #undef HAVE_TM_ZONE */

/* Define if you have the timezone variable.  */
#define HAVE_TIMEZONE_VAR 1

/* Define if your struct tm has tm_tzadj.  */
/* #undef HAVE_TM_TZADJ */

/* Define if your struct tm has tm_gmtoff.  */
/* #undef HAVE_TM_GMTOFF */

/* Define if you don't have tm_zone but do have the external array
   tzname.  */
#define HAVE_TZNAME 1

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if your sys/time.h declares struct tm.  */
/* #undef TM_IN_SYS_TIME */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef uid_t */
/* #undef gid_t */
/* #undef pid_t */
/* #undef mode_t */

/* Define to `long' if <sys/types.h> doesn't define.  */
/* #undef clock_t */
/* #undef time_t */

/* Define if you do not have dirent.h */
/* #undef NO_DIRENT_H */

/* Define if you do not have readdir but you do have V7-style directories */
/* #undef USE_DIRENT2_H */

/* Define if you do not have float.h */
/* #undef NO_FLOAT_H */

/* Define if you do not have limits.h */
/* #undef NO_LIMITS_H */

/* Define if you do not have stdlib.h */
/* #undef NO_STDLIB_H */

/* Define if you do not have string.h */
/* #undef NO_STRING_H */

/* Define if you do not have sys/time.h */
/* #undef NO_SYS_TIME_H */

/* Define if you do not have sys/wait.h */
/* #undef NO_SYS_WAIT_H */

/* Define if time.h can be included with sys/time.h */
#define TIME_WITH_SYS_TIME 1

/* Define if wait does not return a union */
/* #undef NO_UNION_WAIT */

/* Define if "times" returns the elasped real time */
#define TIMES_RETS_REAL_TIME 1

/* Define if gettimeofday is not available */
/* #undef NO_GETTOD */

/* Define if errno.h is not available */
/* #undef NO_ERRNO_H */

/* Define if fd_set is not defined. */
/* #undef NO_FD_SET */

/* Define if stdlib.h defines random */
#define STDLIB_DEFS_RANDOM 1

/* Define if we must use our own version of random */
/* #undef NO_RANDOM */

/* Define if we have catgets and friends */
#define HAVE_CATGETS 1

/* Define if catclose is type void instead of int */
/* #undef BAD_CATCLOSE */

/* Define struct msghdr contains the msg_accrights field */
#define HAVE_MSG_ACCRIGHTS 1

/* Define if stdio FILE has _flags field */
/* #undef HAVE_STDIO_FLAGS */

/* Define if stdio FILE has __flags field */
/* #undef HAVE_STDIO__FLAGS */

/* Define if stdio FILE has _flag field */
#define HAVE_STDIO_FLAG 1

/* Define if stdio FILE has __flag field */
/* #undef HAVE_STDIO__FLAG */

/* Define if stdio FILE has _cnt field */
#define HAVE_STDIO_CNT 1

/* Define if stdio FILE has __cnt field */
/* #undef HAVE_STDIO__CNT */

/* Define if stdio FILE has _r field */
/* #undef HAVE_STDIO_R */

/* Define if stdio FILE has readCount field */
/* #undef HAVE_STDIO_READCOUNT */

/* Define if stdio FILE has the _gptr/_egptr fields */
/* #undef HAVE_STDIO_GPTR */

/* Define if stdio FILE has the _IO_read_ptr/_IO_read_end fields */
/* #undef HAVE_STDIO_IO_READ_PTR */

/* Define if have the function BSDgettimeofday (SGI) */
#define HAVE_BSDGETTIMEOFDAY 1

/* Define if have the function truncate */
#define HAVE_TRUNCATE 1

/* Define if have the function SCO function truncate in chsize */
/* #undef HAVE_CHSIZE */

/* Define if you have the bcopy function.  */
#define HAVE_BCOPY 1

/* Define if you have the bzero function.  */
#define HAVE_BZERO 1

/* Define if you have the fchmod function.  */
#define HAVE_FCHMOD 1

/* Define if you have the fchown function.  */
#define HAVE_FCHOWN 1

/* Define if you have the fsync function.  */
#define HAVE_FSYNC 1

/* Define if you have the ftruncate function.  */
#define HAVE_FTRUNCATE 1

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the getgroups function.  */
#define HAVE_GETGROUPS 1

/* Define if you have the gethostname function.  */
#define HAVE_GETHOSTNAME 1

/* Define if you have the getpriority function.  */
#define HAVE_GETPRIORITY 1

/* Define if you have the gettimeofday function.  */
#define HAVE_GETTIMEOFDAY 1

/* Define if you have the inet_aton function.  */
#define HAVE_INET_ATON 1

/* Define if you have the select function.  */
#define HAVE_SELECT 1

/* Define if you have the sendmsg function.  */
#define HAVE_SENDMSG 1

/* Define if you have the setitimer function.  */
#define HAVE_SETITIMER 1

/* Define if you have the setpgid function.  */
#define HAVE_SETPGID 1

/* Define if you have the setvbuf function.  */
#define HAVE_SETVBUF 1

/* Define if you have the sigaction function.  */
#define HAVE_SIGACTION 1

/* Define if you have the socket function.  */
#define HAVE_SOCKET 1

/* Define if you have the strcoll function.  */
#define HAVE_STRCOLL 1

/* Define if you have the tzset function.  */
#define HAVE_TZSET 1

/* Define if you have the <limits.h> header file.  */
#define HAVE_LIMITS_H 1

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1
