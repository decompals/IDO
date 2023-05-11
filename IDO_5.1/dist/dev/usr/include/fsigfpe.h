C	fsigfpe.h - constants and data areas for the floating-point
C	exception-handling package when FORTRAN is the native language. 

C	The MIPS floating-point accelerator may raise 
C	floating-point exceptions due to five conditions:
C	 FPE_OVERFL  (overflow),  FPE_UNDERFL  (underflow), 
C	 FPE_DIVZERO  (divide-by-zero),  FPE_INEXACT  (inexact result), 
C	or  FPE_INVALID  (invalid operand, e.g., infinity). 
C	Usually these conditions are masked and do not cause a 
C	floating-point exception; instead,
C	a default value is substituted for the result of the operation
C	and the program continues silently.  This event may be intercepted so
C	by causing an exception to be raised.  Once an exception is raised,
C	the specific conditions which caused the exception may be 
C	determined, and more appropriate action taken.
C	This package provides a mechanism for 'handling' such
C	exceptions, classifying them by the conditions which caused them,
C	and substituting a value, either a default value or one supplied 
C	by the user.    This package also includes mechanisms usefil in
C	debugging code that causes traps: stack trace, trap counts,
C 	abort and exit functions. This package is provided 
C
C	for all of the above conditions except  FPE_INEXACT .
C
C	Two options exist to initiate trap handling: the programmable 
C	interface, and the environment variable (runtime) interface.
C      ************************************************************************

C      The runtime interface:

C      Link time switch -lfpe:
C      	In order to acquire the trap handler runtime interface, link the program
C	with the switch -lfpe.   Traps will then be handled according to the
C        runtime environment variable TRAP_FPE.

C     TRAP_FPE:
C	This runtime environment variable specifies how the enabled trap 
C	handler shall behave.   If TRAP_FPE is undefined, the program will
C	execute with sgi defaults (as found in the default values of 
C    	fsigfpe(3f) : array sigfpe_.repls).  To execute user routine as 
C	if it was not compiled -lfpe, use TRAP_FPE=OFF
C
C      Syntax:
C
C	The string "TRAP_FPE" will be checked as an environment variable, in
C	upper case letters only.  The string defining the 
C 	actions of the trap handler can be in either upper case, or lower 
C	case for the user's convenience.
C
C	The TRAP_FPE variable can take one of two forms:  either a "global"
C	value, or a list of individual items.  The two forms cannot be combined.
C	The global form is given by:
C
C	setenv TRAP_FPE global_value
C
C	where global_value takes on one of the following values:
C
C		"" or OFF
C					Execute the program with no
C					Trap handling enabled. 
C					Same as linking WITHOUT -lfpe.
C		ON
C					Same as TRAP_FPE="ALL=DEFAULT", and the
C					same as TRAP_ENV undefined.
C
C	Alternately, replacement values and actions may be specified for
C	each of the possible trap types individually. This is accomplished by:
C
C	setenv TRAP_FPE  "item;item;item...."
C
C					TRAP_ENV is set to a list
C					of items.  The length of this
C					limited by csh.
C
C		an item can be one of the following:
C			traptype=statuslist
C					where traptype defines the specific
C					floating point exception to enable,
C					and statuslist defines the list of
C					actions upon encountering the trap.
C			DEBUG
C					confirm the parsing of the environment
C					variable and the trap settings.
C
C
C		trap type can be one of the following literal strings:
C			UNDERFL
C					underflow
C			OVERFL
C					overflow
C			DIVZERO
C					divide by zero	
C			INVALID
C					invalid operand
C			ALL
C					all of the above
C
C		statuslist is a list separated by commas.  It contains an 
C		optional symbolic replacement value and an optional list of
C		actions.
C
C		replacement values:
C
C			DEFAULT
C					Use the predefined SGI default
C					(from fsigfpe(3f),array sigfpe().repls)
C					as the return result, and continue
C					execution.  If no other replacement
C					value is specified, this is the
C					default.
C			
C			IEEE
C					substitute FPE_APPROPRIATE
C				        as the return result.
C			ZERO		
C					substitute FPE_ZERO as the return result
C			MIN
C					substitute FPE_MIN as the return result.
C			MAX
C					substitute FPE_MAX as the return result.
C			INF		
C					substitute FPE_INF as the return result.
C
C			NAN
C					substitute FPE_NAN as the return result.
C                       FLUSH_ZERO
C                                       set the flush to zero bit in the R4000
C                                       Control Status Register.  Causes
C                                       flush to zero without invoking the
C                                       trap handler.  Works only
C                                       for underflow traps on the R4000.
C                                       Works like ZERO for the R3000.
C
C				
C		actions:
C			COUNT(n)
C					A count of the trap type will be 
C					printed to stderr at the end of 
C					execution of the program, and every
C					nth trap. 
C			
C		these actions take an optional integer in parentheses
C			ABORT(n)
C					Core dump and abort program upon 
C					encountering the nth trap, default 
C					is 1.
C			EXIT(n)
C					Exit program without coredump upon 
C					encountering the nth trap, default 
C					is 1.
C			TRACE(n)
C					If trap is encountered, print a 
C					stack trace to stderr. Print this
C					stack trace up to n times.  Default
C					is 10.
C
C      Example:
C
C		setenv TRAP_FPE "ALL=COUNT;UNDERFLOW=ZERO;OVERFLOW=IEEE,
C		TRACE(5),ABORT(100); DIVZERO=ABORT"
C
C	count all traps, print final results (even on abort) to stderr,
C	underflow to zero, overflow to IEEE, trace first five overflows, if
C	100 overflows occur then abort, abort on first zero divide.
C
C
C      ************************************************************************
C	The programmable interface:
	
C	Control is initiated (and canceled) by calling the function 
C	handle_sigfpes.  The function takes four arguments:

C		call handle_sigfpes(onoff,en_mask,user_routine,
C			abort_action,abort_routine)

C		   onoff - a flag indicating whether handling is 
C			being turned on (onoff ==  FPE_ON ) or
C			debug (onoff ==FPE_DEBUG ) or  
C			off (onoff !=  FPE_ON )
	
C		   The remaining parameters are only valid 
C		   if onoff ==  FPE_ON  or FPE_DEBUG.

C		   en_mask - indicates which of the four conditions should
C			raise a floating-point exception.  This parameter
C			is only valid if onoff ==  FPE_ON or FPE_DEBUG, 
C			and is the sum
C			of  FPE_EN_UNDERFL ,  FPE_EN_OVERFL , 
C			 FPE_EN_DIVZERO , and  FPE_EN_INVALID 

C		   user_routine - The address of a user's routine which
C			is called to set the value.  This routine is called
C			if the replacement value code for the condition-at-hand
C			is  FPE_USER_DETERMINED  (see below). It
C			is invoked with the following parameters:

C			subroutine user_routine(exception, val);
C			integer*4 exception(0:4)
C			integer val(2)

C			The exception array contains useful information
C			concerning the exception.  Its elements
C			are  FPE_EXCEPTION_TYPE  (0), 
C			 FPE_INVALID_ACTION  (1),
C			 FPE_INVALID_TYPE  (2), 
C			 FPE_VALUE_TYPE  (3), 
C			 FPE_VALUE_SIGN  (4):
	
C			 FPE_EXCEPTION_TYPE  :  FPE_OVERFL ,  FPE_UNDERFL , etc.
C			 FPE_INVALID_ACTION  :  FPE_SET_RESULT  if result 
C					is being set, otherwise operand 
C					is being replaced
C			 FPE_INVALID_TYPE : if exception_type is  FPE_INVALID,
C					this indicates the type of  FPE_INVALID 
C					exception which occurred: 
C					 FPE_CVT_OVERFL , 
C					 FPE_ZERO_TIMES_INF , etc.
C			 FPE_VALUE_TYPE :  FPE_SINGLE , 
C					 FPE_DOUBLE , or  FPE_WORD 
C			 FPE_VALUE_SIGN : The suggested sign for the value, 
C				based on an analysis of the instruction:
C				 FPE_NEGATIVE  or  FPE_POSITIVE 
C			val: an array in which the modified value should
C				be placed.  If an operand is being replaced,
C				val has a copy of the current operand.
	
C		   abort_action - If the handler encounters an unexpected 
C			condition, an inconsistency, or begins looping, 
C			this flag indicates what action should be taken.  The 
C			possible values are:
	
C				 FPE_TURN_OFF_HANDLER_ON_ERROR  - instruct the 
C					floating-point-accelerator to cease
C					causing exceptions and continue.
C					(i.e., disable handling)
C				 FPE_ABORT_ON_ERROR  - kill the process after 
C					giving an error message and possibly 
C					calling a user-supplied cleanup routine.
C				 FPE_REPLACE_HANDLER_ON_ERROR  - install the 
C					indicated user routine as the handler
C					when such an error is encountered.
C					Future floating-point exceptions will
C					branch to the user-routine. 
C					(see signal(2))
	
C		    abort_routine - When a fatal error (i.e., one described
C			under 'abort_action' above) is encountered, this
C			argument is used as the address of a user routine.  
C			If abort_action is  FPE_ABORT_ON_ERROR , and this 
C			argument is non-zero, it is used as the address of a 
C			routine to call before aborting.  The routine is invoked
C			with a single argument - the pc of the exception.
C			If abort_action is  FPE_REPLACE_HANDLER_ON_ERROR , 
C			and abort_routine is non-zero, it will be installed 
C			as the new handler.  The instruction which caused 
C			the unexpected exception will be re-executed, 
C			causing a new exception, and abort_routine entered.
C			(see signal(2))
	



 	integer*4  FPE_ON   
	parameter ( FPE_ON  =1)
 	integer*4  FPE_DEBUG
	parameter ( FPE_DEBUG  = 2)
 	integer*4  FPE_OFF   
	parameter ( FPE_OFF  =0)
	
 	integer*4  FPE_TURN_OFF_HANDLER_ON_ERROR   
	parameter ( FPE_TURN_OFF_HANDLER_ON_ERROR  =1)
 	integer*4  FPE_ABORT_ON_ERROR   
	parameter ( FPE_ABORT_ON_ERROR  =0)
 	integer*4  FPE_REPLACE_HANDLER_ON_ERROR   
	parameter ( FPE_REPLACE_HANDLER_ON_ERROR  =2)
	
C  	exceptions 	

 	integer*4  FPE_UNDERFL   
	parameter ( FPE_UNDERFL  =1)
 	integer*4  FPE_OVERFL    
	parameter ( FPE_OVERFL   =2)
 	integer*4  FPE_DIVZERO   
	parameter ( FPE_DIVZERO  =3)
 	integer*4  FPE_INVALID   
	parameter ( FPE_INVALID  =4)
 	integer*4  FPE_N_EXCEPTION_TYPES   
	parameter ( FPE_N_EXCEPTION_TYPES  =4)
	
 	integer*4  FPE_EN_UNDERFL   
	parameter ( FPE_EN_UNDERFL  =2)
 	integer*4  FPE_EN_OVERFL    
	parameter ( FPE_EN_OVERFL   =4)
 	integer*4  FPE_EN_DIVZERO   
	parameter ( FPE_EN_DIVZERO  =8)
 	integer*4  FPE_EN_INVALID   
	parameter ( FPE_EN_INVALID  =16)

C  types 

 	integer*4  FPE_SINGLE   
	parameter ( FPE_SINGLE  =0)
 	integer*4  FPE_DOUBLE   
	parameter ( FPE_DOUBLE  =1)
 	integer*4  FPE_WORD      
	parameter ( FPE_WORD     =2)
 	integer*4  FPE_FIRST_TYPE   
	parameter ( FPE_FIRST_TYPE  = FPE_SINGLE )
 	integer*4  FPE_LAST_TYPE   
	parameter ( FPE_LAST_TYPE  = FPE_WORD )

	
C  signs 

 	integer*4  FPE_POSITIVE   
	parameter ( FPE_POSITIVE  =0)
 	integer*4  FPE_NEGATIVE   
	parameter ( FPE_NEGATIVE  =1)

	
C  actions  

 	integer*4  FPE_SET_RESULT   
	parameter ( FPE_SET_RESULT  =0)
 	integer*4  FPE_REPL_RS   
	parameter ( FPE_REPL_RS  =1)
 	integer*4  FPE_REPL_RT   
	parameter ( FPE_REPL_RT  =2)

	
C  elements of exception array 

 	integer*4  FPE_EXCEPTION_TYPE   
	parameter ( FPE_EXCEPTION_TYPE  =0)
 	integer*4  FPE_INVALID_ACTION   
	parameter ( FPE_INVALID_ACTION  =1)
 	integer*4  FPE_INVALID_TYPE     
	parameter ( FPE_INVALID_TYPE    =2)
 	integer*4  FPE_VALUE_TYPE       
	parameter ( FPE_VALUE_TYPE      =3)
 	integer*4  FPE_VALUE_SIGN       
	parameter ( FPE_VALUE_SIGN      =4)

	

C	When an exception is encountered, the handler examines the 
C	instruction causing the exception and the state of the 
C	floating-point accelerator to determine the correct action
C	to take, and the program is continued.  In most cases of 
C	floating-point exceptions, e.g.   FPE_UNDERFL ,  FPE_OVERFL , 
C	 FPE_DIVZERO , and some instances of  FPE_INVALID ,
C	an appropriate value is substituted for the result of 
C	the operation, and the instruction which caused the exception 
C	is skipped.  For most exceptions arising due to an invalid operand
C	( FPE_INVALID  exceptions), more meaningful behavior may be obtained
C	by replacing an erroneous operand.
C	In this case, the operand is replaced, and the instruction re-issued.
	
C	For exceptions which always warrant the setting of the result, 
C	the value used is determined by the exception type ( FPE_UNDERFL , 
C	 FPE_OVERFL , or  FPE_DIVZERO ).

C	These default values may be overridden by 
C	initializing a common block named 
C	'sigfpe_repls'.  A declaration for this common block is below.  
C	It has a single member,
C	the integer array 'repls'.  
C	Each element in this array is interpreted
C	as an integer code used to select one of a set of replacement
C	values, or the code  FPE_USER_DETERMINED , which indicates that 
C	the user's routine should be
C	invoked to provide the replacement value.
C	If the code is not  FPE_USER_DETERMINED , the 
C	appropriately-typed (single- or double- precision) 
C	replacement value is then substituted in the operation causing the
C	exception.  The integer codes, and the corresponding replacement
C	values they select, are listed below:

C		 FPE_ZERO  	- use zero as the replacement value.
C		 FPE_MIN   	- use the appropriately-typed minimum value as 
C				  the replacement. (i.e., the smallest number
C				  which is representable in that format)
C		 FPE_MAX   	- use the appropriately-typed maximum value as 
C				  the replacement.
C		 FPE_INF   	- use the appropriately-typed value for infinity 
C				  as the replacement.
C		 FPE_NAN   	- use the appropriately-typed value for 
C				  not-a-number as the replacement.
C		 FPE_APPROPRIATE 
C				- use a handler-supplied appropriate value 
C				  as the replacement.  These are 
C				  FPE_UNDERFL  - use  FPE_ZERO 
C				  FPE_OVERFL  - use  FPE_MAX 
C				  FPE_DIVZERO  - use  FPE_INF 
C		 FPE_USER_DETERMINED  
C 				- call the user's routine to set 
C				  the value.
C                FPE_FLUSH_ZERO - set the flush to zero bit in the R4000
C                                 Control Status Register.  Causes
C                                 flush to zero without invoking the
C                                 trap handler.  Works only
C                                 for underflow traps on the R4000.
C                                 Works like ZERO for the R3000.
C
C		If the replacement-value code for  FPE_INVALID  exceptions is 
C		 FPE_USER_DETERMINED , the replacement value for all 
C		 FPE_INVALID  exceptions will be obtained from the user_routine.  
C		Otherwise, the code for  FPE_INVALID  is ignored, as this 
C		exception has cases which warrant the replacement of an 
C		operand as well as cases which warrant the setting of 
C		the result (see below).
	
	
C	The elements of this array of structures are interpreted as follows:

C		fsigfpe(0) is ignored.
C		fsigfpe(1) is the structure for  FPE_UNDERFL 
C		fsigfpe(2) is the structure for  FPE_OVERFL 
C		fsigfpe(3) is the structure for  FPE_DIVZERO 
C		fsigfpe(4) is the structure for  FPE_INVALID  
C
C	Each structure contains the following:

C		structure /sigfpe_template/
C		      integer repls			The replacement code
C		      integer count			The count limit
C		      integer trace			The trace limit
C		      integer abort			The abort limit
C		      integer exit 			The exit limit
C		end structure
C
C	Each limit defines the number of traps that will be executed 
C 	before the defined action occurs:

C	count : print a count of all the traps that have count enabled.
C	trace : print a dbx stack trace. 
C	abort : abort with a core dump.
C	exit  : exit program, no core dump.
	
	
C	If no sigfpe common block is initialized, no count, trace, exit or 
C	abort actions will be performed.
C	The default values are used as if the following initialization
C	had been performed:
	
C		data fsigfpe.repls / 0,  FPE_MIN ,  FPE_MAX ,  FPE_MAX ,  FPE_APPROPRIATE  /

C	Thus, the replacement code for  FPE_UNDERFL  is  FPE_MIN , for  FPE_DIVZERO  
C	 FPE_MAX , etc.
	

	
C  the possible replacement values:  FPE_ZERO =0,  FPE_MIN =minimum valid number, etc. 
 	integer*4  FPE_ZERO   
	parameter ( FPE_ZERO  =1)
 	integer*4  FPE_MIN   
	parameter ( FPE_MIN  =2)
 	integer*4  FPE_MAX   
	parameter ( FPE_MAX  =3)
 	integer*4  FPE_INF   
	parameter ( FPE_INF  =4)
 	integer*4  FPE_NAN   
	parameter ( FPE_NAN  =5)
 	integer*4  FPE_FLUSH_ZERO
	parameter ( FPE_FLUSH_ZERO  =6)
 	integer*4  FPE_APPROPRIATE   
	parameter ( FPE_APPROPRIATE  =7)
 	integer*4  FPE_USER_DETERMINED   
	parameter ( FPE_USER_DETERMINED  =8)
 	integer*4  FPE_MIN_REPL   
	parameter ( FPE_MIN_REPL  = FPE_ZERO )
 	integer*4  FPE_MAX_REPL   
	parameter ( FPE_MAX_REPL  = FPE_USER_DETERMINED )


	structure /sigfpe_template/
	      integer repls
	      integer count
	      integer trace
	      integer abort
	      integer exit 
	end structure
	record /sigfpe_template/ fsigfpe (0: FPE_N_EXCEPTION_TYPES )
	common /sigfpe / fsigfpe

C	For  FPE_INVALID  exceptions, the correct action may be either to
C	set the result and skip the instruction, or to replace an
C	operand and retry the instruction.  

C	There are four cases in which the result is set.  The common 
C	block named 'invalidop_results' is consulted for user-initialized
C	codes for these cases.  A declaration for this common block,
C	which contains only the array 'invres', is below.  Each element 
C	governs the following cases:
	
C	---index---
C	#  mnemonic			exception condition
C	--------------------------------------------------------------
C	0  (none)			(ignored)
C	1   FPE_MAGNITUDE_INF_SUBTRACTION subtraction of infinities: 
C	2   FPE_ZERO_TIMES_INF 	        multiplication 0 * infinity
C	3   FPE_ZERO_DIV_ZERO 	        0/0
C	4   FPE_INF_DIV_INF 		infinity / infinity

C		if the corresponding code in the array is  FPE_APPROPRIATE ,
C		uninitialized, or not a legal value, the result used is
	
C		for  FPE_MAGNITUDE_INF_SUBTRACTION  -  FPE_INF 
C		for  FPE_ZERO_TIMES_INF 	    -  FPE_ZERO 
C		for  FPE_ZERO_DIV_ZERO 		    -  FPE_ZERO 
C		for  FPE_INF_DIV_INF 		    -  FPE_INF 
	
C  invalid ops for which the result is set  
 	integer*4  FPE_MAGNITUDE_INF_SUBTRACTION   
	parameter ( FPE_MAGNITUDE_INF_SUBTRACTION  =1)
 	integer*4  FPE_ZERO_TIMES_INF  	 
	parameter ( FPE_ZERO_TIMES_INF  	=2)
 	integer*4  FPE_ZERO_DIV_ZERO   	 
	parameter ( FPE_ZERO_DIV_ZERO   	=3)
 	integer*4  FPE_INF_DIV_INF     	 
	parameter ( FPE_INF_DIV_INF     	=4)
 	integer*4  FPE_N_INVALIDOP_RESULTS   
	parameter ( FPE_N_INVALIDOP_RESULTS  =4)

	integer invres(0: FPE_N_INVALIDOP_RESULTS )
	common / invalidop_results / invres


C	There are six cases in which an offending operand is replaced.  
C	The common block named 'invalidop_operands' is consulted for 
C	user-initialized codes for these cases.  A declaration for 
C	this common block, which contains only the array invop, 
C	is below.  Each element governs the following cases:

C	---index---
C	#  mnemonic		exception condition
C	--------------------------------------------------------------
C	0  (none)		(ignored)
C	1   FPE_SQRT_NEG_X  	sqrt(-x) (currently not supported)
C	2   FPE_CVT_OVERFL 	conversion to floating pt caused target to 
C									overflow
C	3   FPE_TRUNK_OVERFL 	conversion to integer caused target to overflow
C	4   FPE_CVT_NAN 	conversion of NaN
C	5   FPE_CVT_INF 	conversion of infinity
C	6   FPE_UNORDERED_CMP 	comparison to NaN
C	7   FPE_SNAN_OP 	operand was Signaling Nan
	
C		if the corresponding code in the array is  FPE_APPROPRIATE ,
C		the element is uninitialized or is not a legal value, 
C		the replacement operand becomes:
	
C		for  FPE_SQRT_NEG_X  	- (currently unsupported)
C		for  FPE_CVT_OVERFL 	-  FPE_MAX 
C		for  FPE_TRUNK_OVERFL 	-  FPE_MAX 
C		for  FPE_CVT_NAN 	-  FPE_MAX 
C		for  FPE_CVT_INF 	-  FPE_MAX 
C		for  FPE_UNORDERED_CMP  -  FPE_MAX 
C		for  FPE_SNAN_OP 	-  FPE_MAX 
	
C  invalid ops for which the offending operand is replaced  

 	integer*4  FPE_SQRT_NEG_X      	 
	parameter ( FPE_SQRT_NEG_X      	=1)
 	integer*4  FPE_CVT_OVERFL      	 
	parameter ( FPE_CVT_OVERFL      	=2)
 	integer*4  FPE_TRUNK_OVERFL      	 
	parameter ( FPE_TRUNK_OVERFL      	=3)
 	integer*4  FPE_CVT_NAN 		 
	parameter ( FPE_CVT_NAN 		=4)
 	integer*4  FPE_CVT_INF 		 
	parameter ( FPE_CVT_INF 		=5)
 	integer*4  FPE_UNORDERED_CMP 	 
	parameter ( FPE_UNORDERED_CMP 	        =6)
 	integer*4  FPE_SNAN_OP 		 
	parameter ( FPE_SNAN_OP 		=7)
 	integer*4  FPE_N_INVALIDOP_OPERANDS   
	parameter ( FPE_N_INVALIDOP_OPERANDS    =7)
	

	integer invop(0: FPE_N_INVALIDOP_OPERANDS )
	common / invalidop_operands / invop

C	Once handle_sigfpes has been called, the values for the
C	various conditions are set.  Subsequent assignments to these
C	arrays will have no effect, unless handling is suspended
C	and resumed.


