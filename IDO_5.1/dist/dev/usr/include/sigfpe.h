#ifdef __cplusplus
extern "C" {
#endif
/*

	sigfpe.h - constants and data areas for the floating-point
	exception-handling package when C is the native language. 

	The MIPS floating-point accelerator may raise 
	floating-point exceptions due to five conditions:
	_OVERFL (overflow), _UNDERFL (underflow), 
	_DIVZERO (divide-by-zero), _INEXACT (inexact result), 
	or _INVALID (invalid operand, e.g., infinity). 
	Usually these conditions are masked and do not cause a 
	floating-point exception; instead,
	a default value is substituted for the result of the operation
	and the program continues silently.  This event may be intercepted so
	by causing an exception to be raised.  Once an exception is raised,
	the specific conditions which caused the exception may be 
	determined, and more appropriate action taken.
	This package provides a mechanism for 'handling' such
	exceptions, classifying them by the conditions which caused them,
	and substituting a value, either a default value or one supplied 
	by the user.  This package also includes mechanisms useful in
	debugging code that causes traps : stack trace, trap counts, 
	abort and exit functions.  This package is provided 
	for all of the above conditions except _INEXACT.
	
	Two options exist to initiate trap handling: the programmable
	interface, and the environment variable (runtime) interface.

        *********************************************************************
       The Runtime interface:

       Link time switch -lfpe:
	In order to acquire the trap handler runtime interface, link the program
	with the switch -lfpe.   Traps will then be handled according to the
        runtime environment variable TRAP_FPE.

      TRAP_FPE:
	This runtime environment variable specifies how the enabled trap 
	handler shall behave.   If TRAP_FPE is undefined, the program will
	execute with sgi defaults (as found in the default values of 
    	sigfpe(3f) : array sigfpe_repls).  To execute user routine as 
	if it was not compiled -lfpe, use TRAP_FPE=OFF

      Syntax:

	The string "TRAP_FPE" will be checked as an environment variable, in
	upper case letters only.  The string defining the 
 	actions of the trap handler can be in either upper case, or lower 
	case for the user's convenience.

	The TRAP_FPE variable can take one of two forms:  either a "global"
	value, or a list of individual items.  The two forms cannot be combined.
	The global form is given by:

	setenv TRAP_FPE global_value

	where global_value takes on one of the following values:

		"" or OFF
					Execute the program with no
					Trap handling enabled. 
					Same as linking WITHOUT -lfpe.
		ON
					Same as TRAP_FPE="ALL=DEFAULT", and the
					same as TRAP_ENV undefined.

	Alternately, replacement values and actions may be specified for
	each of the possible trap types individually. This is accomplished by:

	setenv TRAP_FPE  "item;item;item...."

					TRAP_ENV is set to a list
					of items.  The length of this
					limited by csh.

		an item can be one of the following:
			traptype=statuslist
					where traptype defines the specific
					floating point exception to enable,
					and statuslist defines the list of
					actions upon encountering the trap.
			DEBUG
					confirm the parsing of the environment
					variable and the trap settings.


		trap type can be one of the following literal strings:
			UNDERFL
					underflow
			OVERFL
					overflow
			DIVZERO
					divide by zero	
			INVALID
					invalid operand
			ALL
					all of the above

		statuslist is a list separated by commas.  It contains an 
		optional symbolic replacement value and an optional list of
		actions.

		replacement values:

			DEFAULT
					Use the predefined SGI default
					(from fsigfpe(3f), sigfpe_[].repls)
					as the return result, and continue
					execution.  If no other replacement
					value is specified, this is the
					default.
			
			IEEE
					Use IEEE standard results (from 
					fsigfpe(3f), array repls(traptype)=
					_APPROPRIATE ) as the return
					result, and continue execution.
			ZERO		
					substitute _ZERO as the return result.
			MIN
					substitute _MIN as the return result.
			MAX
					substitute _MIN as the return result.
			INF		
					substitute _INF as the return result.
			NAN
					substitute _NAN as the return result.
			FLUSH_ZERO	
					set the flush to zero bit in the R4000
					Control Status Register.  Causes
					flush to zero without invoking the
					trap handler.  Works only
					for underflow traps on the R4000.
					Works like ZERO for the R3000.
				
		actions:
			COUNT(n)
					A count of the trap type will be 
					printed to stderr at the end of 
					execution of the program, and every
					nth trap. 
			
		these actions take an optional integer in parentheses
			ABORT(n)
					Core dump and abort program upon 
					encountering the nth trap, default 
					is 1.
			EXIT(n)
					Exit program without coredump upon 
					encountering the nth trap, default 
					is 1.
			TRACE(n)
					If trap is encountered, print a 
					stack trace to stderr. Print this
					stack trace up to n times.  Default
					is 10.

       Example:

		setenv TRAP_FPE "ALL=COUNT;UNDERFLOW=ZERO;OVERFLOW=IEEE,
		TRACE(5),ABORT(100); DIVZERO=ABORT"

	count all traps, print final results (even on abort) to stderr,
	underflow to zero, overflow to IEEE, trace first five overflows, if
	100 overflows occur then abort, abort on first zero divide.

        *********************************************************************
	The programmable interface:
	Control is initiated (and canceled) by calling the function 
	handle_sigfpes.  The function takes four arguments:

		handle_sigfpes(onoff,en_mask,user_routine,
			abort_action,abort_routine)

		   onoff - a flag indicating whether handling is 
			being turned on (onoff == _ON) or 
			debug (onoff = _DEBUG)
			off (onoff != _ON && onoff != _DEBUG)
	
		   The remaining parameters are only valid 
		   if onoff == _ON or _DEBUG.

		   en_mask - indicates which of the four conditions should
			raise a floating-point exception.  This parameter
			is only valid if onoff == _ON or _DEBUG, and is the 
			bit-or of _EN_UNDERFL, _EN_OVERFL, 
			_EN_DIVZERO, and _EN_INVALID

		   user_routine - The address of a user's routine which
			is called to set the value.  This routine is called
			if the replacement value code for the condition-at-hand
			is _USER_DETERMINED (see below). It
			is invoked with the following parameters:

			user_routine(exception, val);
			unsigned exception[5];
			int val[2];

			The exception array contains useful information
			concerning the exception.  Its elements
			are _EXCEPTION_TYPE (0), _INVALID_ACTION (1),
			_INVALID_TYPE (2), _VALUE_TYPE (3), _VALUE_SIGN (4):
	
			_EXCEPTION_TYPE : _OVERFL, _UNDERFL, etc.
			_INVALID_ACTION : _SET_RESULT if result 
					is being set, otherwise operand 
					is being replaced
			_INVALID_TYPE: if exception_type is _INVALID, this
					indicates the type of _INVALID
					exception which occurred: 
					_CVT_OVERFL, 
					_ZERO_TIMES_INF, etc.
			_VALUE_TYPE: _SINGLE, _DOUBLE, or _WORD
			_VALUE_SIGN: The suggested sign for the value, 
				based on an analysis of the instruction:
				_NEGATIVE or _POSITIVE
			val: an array in which the modified value should
				be placed.  If an operand is being replaced,
				val has a copy of the current operand.
	
		   abort_action - If the handler encounters an unexpected 
			condition, an inconsistency, or begins looping, 
			this flag indicates what action should be taken.  The 
			possible values are:
	
				_TURN_OFF_HANDLER_ON_ERROR - instruct the 
					floating-point-accelerator to cease
					causing exceptions and continue.
					(i.e., disable handling)
				_ABORT_ON_ERROR - kill the process after 
					giving an error message and possibly 
					calling a user-supplied cleanup routine.
				_REPLACE_HANDLER_ON_ERROR - install the 
					indicated user routine as the handler
					when such an error is encountered.
					Future floating-point exceptions will
					branch to the user-routine. 
					(see signal(2))
	
		    abort_routine - When a fatal error (i.e., one described
			under 'abort_action' above) is encountered, this
			argument is used as the address of a user routine.  
			If abort_action is _ABORT_ON_ERROR, and this argument
			is non-zero, it is used as the address of a routine
			to call before aborting.  The routine is invoked
			with a single argument - the pc of the exception.
			If abort_action is _REPLACE_HANDLER_ON_ERROR, 
			and abort_routine is non-zero, it will be installed 
			as the new handler.  The instruction which caused 
			the unexpected exception will be re-executed, 
			causing a new exception, and abort_routine entered.
			(see signal(2))
	
*/

#define _ON  1
#define _OFF  0
#define _DEBUG 2
	
#define _TURN_OFF_HANDLER_ON_ERROR  1
#define _ABORT_ON_ERROR  0
#define _REPLACE_HANDLER_ON_ERROR  2
	
 /* 	exceptions 	*/

#define _UNDERFL  1
#define _OVERFL   2
#define _DIVZERO  3
#define _INVALID  4
#define _N_EXCEPTION_TYPES  4
	
#define _EN_UNDERFL  2
#define _EN_OVERFL   4
#define _EN_DIVZERO  8
#define _EN_INVALID  16

 /* types */

#define _SINGLE  0
#define _DOUBLE  1
#define _WORD     2
#define _FIRST_TYPE  _SINGLE
#define _LAST_TYPE  _WORD

 /* signs */

#define _POSITIVE  0
#define _NEGATIVE  1
	
 /* actions  */

#define _SET_RESULT  0
#define _REPL_RS  1
#define _REPL_RT  2

 /* elements of exception array */

#define _EXCEPTION_TYPE  0
#define _INVALID_ACTION  1
#define _INVALID_TYPE    2
#define _VALUE_TYPE      3
#define _VALUE_SIGN      4

	
/*
	When an exception is encountered, the handler examines the 
	instruction causing the exception and the state of the 
	floating-point accelerator to determine the correct action
	to take, and the program is continued.  In most cases of 
	floating-point exceptions, e.g.  _UNDERFL, _OVERFL, 
	_DIVZERO, and some instances of _INVALID,
	an appropriate value is substituted for the result of 
	the operation, and the instruction which caused the exception 
	is skipped.  For most exceptions arising due to an invalid operand
	(_INVALID exceptions), more meaningful behavior may be obtained
	by replacing an erroneous operand.
	In this case, the operand is replaced, and the instruction re-issued.
	
	For exceptions which always warrant the setting of the result, 
	the value used is determined by the exception type (_UNDERFL, 
	_OVERFL, or _DIVZERO).

	These may be overridden by initializing a global array of structures
        named 'sigfpe_'.  A declaration for this array of structures 
        is below.  
	
	Each element in the sigfpe_[].repls is interpreted
	as an integer code used to select one of a set of replacement
	values, or the code _USER_DETERMINED, which indicates that 
	the user's routine should be invoked to provide the replacement value.
	If the code is not _USER_DETERMINED, the 
	appropriately-typed (single- or double- precision) 
	and appropriately-signed replacement value is then substituted 
	in the operation causing the exception.  The integer codes, 
	and the corresponding replacement values they select, are listed below:

		_ZERO 	- use zero as the replacement value.
		_MIN  	- use the appropriately-typed minimum value as 
				the replacement. (i.e., the smallest number
				which is representable in that format)
		_MAX  	- use the appropriately-typed maximum value as 
				the replacement.
		_INF  	- use the appropriately-typed value for infinity 
				as the replacement.
		_NAN  	- use the appropriately-typed value for not-a-number 
				as the replacement.
		_APPROPRIATE- use a handler-supplied appropriate value 
				as the replacement.  These are 
				  _UNDERFL - use _ZERO
				  _OVERFL - use _MAX
				  _DIVZERO - use _INF
		_USER_DETERMINED - call the user's routine to set the
			value.
		_FLUSH_ZERO 	
			- set the flush to zero bit in the R4000
			  Control Status Register.  Causes
			  flush to zero without invoking the
			  trap handler.  Works only
			  for underflow traps on the R4000.
			  Works like ZERO for the R3000.
				

	
		If the replacement-value code for _INVALID exceptions is 
		_USER_DETERMINED, the replacement value for all 
		_INVALID exceptions will be obtained from the user_routine.  
		Otherwise, the code for _INVALID is ignored, as this 
		exception has cases which warrant the replacement of an 
		operand as well as cases which warrant the setting of 
		the result (see below).
	
	
	The elements of this array are interpreted as follows:

		sigfpe_[0] is ignored.
		sigfpe_[1] is the structure for _UNDERFL
		sigfpe_[2] is the structure for _OVERFL
		sigfpe_[3] is the structure for _DIVZERO
		sigfpe_[4] is the structure for _INVALID -

	Each structure contains the following:

		struct sigfpe_template
			{
			int repls;	The replacement value
			int count;	The count limit
			int trace;	The trace limit
			int abort;	The abort limit
			int exit ;	The exit limit
			};
	Each limit defines the number of traps that will be executes
	before the defined action occurs:

	count : print a count of all traps that have count enabled.
	trace : print a dbx stack trace.
	abort : abort with a core dump.
	exit  : exit program, no core dump.

	
	If no sigfpe_ structure is initialized no count, trace, exit or
        abort ections will be performed. The default values for
	sigfpe_[].repls are used as if the following initialization 
	had been performed:
	 
		int sigfpe_[_N_EXCEPTION_TYPES+1].repls = {
			0, _MIN, _MAX, _MAX, _APPROPRIATE } ;

	Thus, the replacement code for _UNDERFL is _MIN, for _DIVZERO 
	_MAX, etc.
	
*/
	
 /* the possible replacement values: _ZERO=0, _MIN=minimum valid number, etc. */
#define _ZERO  1
#define _MIN  2
#define _MAX  3
#define _INF  4
#define _NAN  5
#define _FLUSH_ZERO	  6
#define _APPROPRIATE  7
#define _USER_DETERMINED  8
#define _MIN_REPL  _ZERO
#define _MAX_REPL  _USER_DETERMINED


struct sigfpe_template
	{
	int repls;
	int count;
	int trace;
	int abort;
	int exit ;
	};
extern struct sigfpe_template sigfpe_[_N_EXCEPTION_TYPES + 1];

/*
	For _INVALID exceptions, the correct action may be either to
	set the result and skip the instruction, or to replace an
	operand and retry the instruction.  

	There are four cases in which the result is set.  The array
	named 'invalidop_results_' is consulted for user-initialized
	codes for these cases.  A declaration for invalidop_results_
	is below.  Each element governs the following cases:
	
	---index---
	#  mnemonic			exception condition
	--------------------------------------------------------------
	0  (none)			(ignored)
	1  _MAGNITUDE_INF_SUBTRACTION   subtraction of infinities: 
	2  _ZERO_TIMES_INF	        multiplication 0 * infinity
	3  _ZERO_DIV_ZERO	        0/0
	4  _INF_DIV_INF		        infinity / infinity

		if the corresponding code in the array is _APPROPRIATE,
		uninitialized, or not a legal value, the result used is
	
		for _MAGNITUDE_INF_SUBTRACTION 	- _INF
		for _ZERO_TIMES_INF		- _ZERO
		for _ZERO_DIV_ZERO		- _ZERO
		for _INF_DIV_INF		  	- _INF
	
*/
	
 /* invalid ops for which the result is set  */
#define _MAGNITUDE_INF_SUBTRACTION  1
#define _ZERO_TIMES_INF 	 2
#define _ZERO_DIV_ZERO  	 3
#define _INF_DIV_INF    	 4
#define _N_INVALIDOP_RESULTS  4

extern int invalidop_results_[_N_INVALIDOP_RESULTS+1];

	
/*

	There are six cases in which an offending operand is replaced.  
	An array named 'invalidop_operands_' is consulted for 
	user-initialized codes for these cases.  A declaration for 
	invalidop_results_ is below.  Each element governs the 
	following cases:

	---index---
	#  mnemonic		exception condition
	--------------------------------------------------------------
	0  (none)	    (ignored)
	1  _SQRT_NEG_X 	    sqrt(-x) (currently not supported)
	2  _CVT_OVERFL	    conversion to floating pt caused target to overflow
	3  _TRUNK_OVERFL    conversion to integer caused target to overflow
	4  _CVT_NAN	    conversion of NaN
	5  _CVT_INF	    conversion of infinity
	6  _UNORDERED_CMP   comparison to NaN
	7  _SNAN_OP	    operand was Signaling Nan
	
		if the corresponding code in the array is _APPROPRIATE,
		the element is uninitialized or is not a legal value, 
		the replacement operand becomes:
	
		for _SQRT_NEG_X       - (currently unsupported)
		for _CVT_OVERFL	      - _MAX
		for _TRUNK_OVERFL     - _MAX
		for _CVT_NAN	      - _MAX
		for _CVT_INF	      - _MAX
		for _UNORDERED_CMP    - _MAX
		for _SNAN_OP	      - _MAX
	
*/
	
 /* invalid ops for which the offending operand is replaced  */
#define _SQRT_NEG_X     	 1
#define _CVT_OVERFL     	 2
#define _TRUNK_OVERFL     	 3
#define _CVT_NAN		 4
#define _CVT_INF		 5
#define _UNORDERED_CMP	 	 6
#define _SNAN_OP		 7
#define _N_INVALIDOP_OPERANDS    7
	

extern int invalidop_operands_[_N_INVALIDOP_OPERANDS+1];

	
/*
	Once handle_sigfpes has been called, the values for the
	various conditions are set.  Subsequent assignments to these
	arrays will have no effect, unless handling is suspended
	and resumed.
*/

extern void	handle_sigfpes( int, int, void (*) (unsigned [5], int [2]),
				int, void (*) (unsigned long) );

#ifdef __cplusplus
}
#endif
