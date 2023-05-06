/*	@(#)sm_inter.h	1.2 88/07/11 4.0NFSSRC SMI	*/

/* 
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 * @(#) from SUN 1.5
 */

#define SM_PROG 100024
#define SM_VERS 1
#define SM_STAT 1
#define SM_MON 2
#define SM_UNMON 3
#define SM_UNMON_ALL 4
#define SM_SIMU_CRASH 5

#define SM_MAXSTRLEN 1024

struct sm_name {
	char *mon_name;
};
typedef struct sm_name sm_name;
extern bool_t xdr_sm_name(XDR *, sm_name *);


struct my_id {
	char *my_name;
	int my_prog;
	int my_vers;
	int my_proc;
};
typedef struct my_id my_id;
extern bool_t xdr_my_id(XDR *, my_id *);


struct mon_id {
	char *mon_name;
	struct my_id my_id;
};
typedef struct mon_id mon_id;
extern bool_t xdr_mon_id(XDR *, mon_id *);

#define SM_PRIVLEN 16

struct mon {
	struct mon_id mon_id;
	char priv[SM_PRIVLEN];
};
typedef struct mon mon;
extern bool_t xdr_mon(XDR *, mon *);


struct sm_stat {
	int state;
};
typedef struct sm_stat sm_stat;
extern bool_t xdr_sm_stat(XDR *, sm_stat *);


enum res {
	stat_succ = 0,
	stat_fail = 1
};
typedef enum res res;
extern bool_t xdr_res(XDR *, res *);


struct sm_stat_res {
	res res_stat;
	int state;
};
typedef struct sm_stat_res sm_stat_res;
extern bool_t xdr_sm_stat_res(XDR *, sm_stat_res *);


struct status {
	char *mon_name;
	int state;
	char priv[SM_PRIVLEN];
};
typedef struct status status;
extern bool_t xdr_status(XDR *, status *);

