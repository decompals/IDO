/* Copyright (C) 1989 Silicon Graphics, Inc. All rights reserved.  */
/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1991, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Avenue                               |
 * |         Sunnyvale, California 94088-3650, USA             |
 * |-----------------------------------------------------------|
 */
/* $Header: /hosts/bonnie/proj/irix6.4-ssg/isms/cmplrs/include/RCS/exception.h,v 7.11 1995/12/11 22:05:15 bean Exp $ */
#ifndef _EXCEPTION_H
#define _EXCEPTION_H


/*

This file contains information, data structures and constants for
the Mips exception handling facility

First, let's examine the requirements:
	- minimize cost at runtime until an exception is encountered.
	- handle exception from Ada, PL1, IEEE and UNIX signals.

These requirements led us to providing a facility in the loader to
collect per-procedure exception information and frame information so
that we could easily unwind the stack at the point of exception.
Details follow.

The data structures involved are the runtime procedure table (runtime_pdr)
and the exception info structure (exception_info). A runtime_pdr table is
generated by the loader if the user accesses a variable called 
"_procedure_table" (akin to accessing _stext). "_procedure_table_size"
will contain the number of "_procedure_table" entries if it is used.
The loader will sort the runtime_pdr by the adr field.

Note that the adr field is NOT updated by rld(1) when
rld moves a dso (for a procedure table applying to a dso).

See sym.h for a definition of the runtime_pdr.

The "exception_info" field is filled in by the loader for external procedures
with address of a global data variable with the same name as the procedure
and a suffix "_exception_info" (e.g. foo() and foo_exception_info). By
convention this address will contain an array of "exception_info" structures.

*/
#ifdef __cplusplus
extern "C" {
#endif

#include	<sgidefs.h>

/*
 The following is usable as a test by applications to indicate that
 unwind_name and find_rpd_name entries exist in this header file.
*/
#define _EXCEPTION_H_HAS_NAME_ENTRIES 1

#if (_MIPS_SIM == _MIPS_SIM_ABI32)

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
struct sigcontext;
typedef struct exception_info {
	long exception;		/* exception identifier */
	void (*handler) (long, long, struct sigcontext *, 
		struct sigcontext *, struct runtime_pdr *, long); /* handler 
		for exception */
	long data;		/* compile time data to pass to handler */
} EXIR, *pEXIR;
#endif /* (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)) */

/*

The "exception" field will contain either a signal number or one of the
following constants (to be added to):

*/

#define EXC_END		0
#define EXC_BASE	1000000
#define EXC_ALL		(EXC_BASE+0)
#define EXC_ADA_USER	(EXC_BASE+1)
#define EXC_PL1_USER	(EXC_BASE+2)


/*

The array is terminated with an entry where the "exception field is either
EXI_END or EXI_ALL. EXI_ALL will say that "handler" will handle all
exceptions.

The data field will depend on the procedure. For example, in ADA the data
field will point at the frame table which the ADA handler will use to
further subdivide a procedure into exception scopes.

At runtime the following procedures will be provided in libexc.a:

*/

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
void
set_unhandled_exception(void (*  /*handler*/ ) (void (*)(long, long, struct sigcontext *, struct sigcontext *, struct runtime_pdr *, long)));

struct runtime_pdr * find_rpd(long /*pc*/);
struct runtime_pdr * find_rpd_name(long /*pc*/,char ** /*name*/);

void unwind(struct sigcontext * , struct runtime_pdr * );
void unwind_name(struct sigcontext *, struct runtime_pdr *, char** /*name*/);

void exception_dispatcher(long /*exception*/, long /*code*/, 
                          struct sigcontext *);

int  exc_resume(struct sigcontext *);

void exc_setjmp(struct sigcontext *);

/*

Exception_default allows the user to set up a default handler in case
no one handles an exception sent to the exception_dispatcher. By
default this will be set up to be a routine that unsets the signal
(where applicable) and returns to initial spot of the exception. If
it is a signal, then the process should die with unhandled exception.

Find_rpd will do a binary search on the table pointed to by "_procedure_table"
and return a pointer to entry which it's pc argument falls into.

Unwind will take the sigcontext scp argument and virtually unwind by
changing the fields to reflect the next frame on the stack. A sigreturn
call can be called with this scp upon return and you'll return to
the frame unwind set up. The prpd argument is used as the the
procedure table entry, if it is null unwind will call find_rpd on
scp->sc_pc to get it.

Exception_dispatcher will be a generic exception handler which will
get the runtime_pdr and check through it's exception_info array for an
entry which matches it's exception argument. If an entry is found which
matches or contains EXI_ALL, then the entry's handler is called as
follows:
		handler(exception, code, ocp, scp, prpd, data)
		long	exception;
		long	code;
		struct	sigcontext *oscp;-- original sigcontext
		struct	sigcontext *scp; -- current sigcontext for this frame
		pRPDR	prpd;
		long	data;		 -- from exception record

[NOTE: If exception is SIGFPE, the scp argument has IEEE exception bits masked 
out of it sc_fpc_scr so that a handler that uses the scp to sigreturn won't 
get an immediate SIGFPE-- of course oscp has the correct sc_fpc_csr]

If no match is found or the handler returns, exception_dispatch will
unwind the frame and try the previous frame's exception array. If the
bottom of the stack is reached (pc == 0), then the default handler is called.

The handler can either return to exception_dispatcher (who'll treat that action
as the handler not really handling the error and try to find someone else
to handle it up the stack) or it can sigreturn (longjump) to wherever it
deems appropriate.

If a language support user exceptions, it can use the exception dispatcher
as well (assume ADA code):

user_code:
	...
	if whatever
		raise(my_exception)
	...

raise(exception)
{
	struct sigcontext sc;

	setjmp(&sc);		-- currently 4.3's setjmp
	unwind(&sc, 0);		-- get back to user_code's frame
	sc.sc_pc -= 8;		-- make it point at the call
	exception_dispatcher(EXI_ADA_USER, exception, &sc);
}

*/

/* the following are the declarations to access the procedure table and
 *	its size generated by the loader.
 */
extern char _procedure_table_size[];
extern char _procedure_string_table[];
#define PSIZE ((int) _procedure_table_size)
extern struct runtime_pdr _procedure_table[];

#define EXCEPTION_SUFFIX "_exception_info"
#endif

#else /* if (_MIPS_SIM == _MIPS_SIM_ABI64 || _MIPS_SIM == _MIPS_SIM_NABI32) */

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))

#include <dwarf.h>
#include <libelf.h>
#include <libdwarf.h>

struct sigcontext;

Dwarf_Fde
find_fde(Elf64_Addr pc);

void
exc_unwind(struct sigcontext *scp, Dwarf_Fde fde);

int
exc_resume(struct sigcontext *scp);

void
exc_setjmp(struct sigcontext *scp);

#endif

#endif	

#ifdef __cplusplus
}
#endif

#endif /* !_EXCEPTION_H */