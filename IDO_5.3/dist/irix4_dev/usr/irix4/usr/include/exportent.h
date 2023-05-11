/*	@(#)exportent.h	1.1 88/03/15 4.0NFSSRC SMI
 *
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 *  1.5 88/02/07 (C) 1986 SMI
 */

/*
 * Exported file system table, see exportent(3)
 */ 

#define TABFILE "/etc/xtab"		/* where the table is kept */

/*
 * Options keywords
 */
#define ACCESS_OPT	"access"	/* machines that can mount fs */
#define ROOT_OPT	"root"		/* machines with root access of fs */
#define RO_OPT		"ro"		/* export read-only */
#define RW_OPT		"rw"		/* export read-mostly */
#define ANON_OPT	"anon"		/* uid for anonymous requests */
#define	NOHIDE_OPT	"nohide"	/* visible from upper exported fs */
#define	WSYNC_OPT	"wsync"		/* write synchronously to disk */

struct exportent {
	char *xent_dirname;	/* directory (or file) to export */
	char *xent_options;	/* options, as above */
};

#ifdef __cplusplus
extern "C" {
#endif

extern struct __file_s *setexportent(void);
extern void endexportent(struct __file_s *);
extern int remexportent(struct __file_s *, char *);
extern int addexportent(struct __file_s *, char *, char *);
extern char *getexportopt(struct exportent *, char *);
extern struct exportent *getexportent(struct __file_s *);

#ifdef __cplusplus
}
#endif
