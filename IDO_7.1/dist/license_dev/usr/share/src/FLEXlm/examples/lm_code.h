/******************************************************************************

	    COPYRIGHT (c) 1990, 1993 by Globetrotter Software Inc.
	This software has been provided pursuant to a License Agreement
	containing restrictions on its use.  This software contains
	valuable trade secrets and proprietary information of 
	Globetrotter Software Inc and is protected by law.  It may 
	not be copied or distributed in any form or medium, disclosed 
	to third parties, reverse engineered or used in any manner not 
	provided for in said License Agreement except with the prior 
	written authorization from Globetrotter Software Inc.

 *****************************************************************************/
/*	
 *
 *	Module:	lm_code.h v3.20.1.1
 *	Last changed:  07 Apr 1995
 *	Description: 	Encryption codes to be used in a VENDORCODE macro 
 *			for FLEXlm daemons, create_license, lm_init(),
 *			and lm_checkout() call - modify these values 
 *			for your own use.  (The VENDOR_KEYx values
 *			are assigned by Globetrotter Software).
 *
 *	example LM_CODE() macro:
 *
 *		LM_CODE(var_name, ENCRYPTION_CODE_1, ENCRYPTION_CODE_2,
 *					VENDOR_KEY1, VENDOR_KEY2, 
 *					VENDOR_KEY3, VENDOR_KEY4);
 *
 */

/*
 *	ENCRYPTION_CODE_1 and 2
 *	VENDOR's private encryption seed
 *		These are 0x87654321 and 0x12345678 by default.
 *		Each vendor must ensure that you replace these with
 *		numbers which are unique to your company, and keep these
 *		numbers secret.  Only someone with access to these
 *		numbers can generate license files that will work with
 *		your application.
 *		MAKE SURE the numbers are not left to the defaults.
 */

#define ENCRYPTION_CODE_1 0x0 
#define ENCRYPTION_CODE_2 0x0

/*
 *	FLEXlm vendor keys
 */


#define VENDOR_KEY1     0x0
#define VENDOR_KEY2     0x0
#define VENDOR_KEY3     0x0
#define VENDOR_KEY4     0x0
#define VENDOR_KEY5     0x0


/*
 *	FLEXlm vendor name
 */

#define VENDOR_NAME     "demo"

