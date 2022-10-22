
//
// api header file
//
#include <lmsgi.h>


//
// other headers for your application
//
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


//
// macro which defines the code variable
// *do not* change these two lines
//
LM_CODE(code, ENCRYPTION_CODE_1, ENCRYPTION_CODE_2,
	VENDOR_KEY1, VENDOR_KEY2, VENDOR_KEY3, VENDOR_KEY4, VENDOR_KEY5);


void
application_code (void) {

    //
    // application code goes here
    //
    // you need to sprinkle calls to license_timer() to ensure that you
    // are still connected to the license server
    //

    // some code
    license_timer();

    // more code
    license_timer();

    // ...

}


void
main (int argc, char *argv[]) {


    //
    // initializes the api
    //
    if (license_init(&code,     // leave as is
		     "sgifd",   // replace with your vendor daemon name
		     B_TRUE     // read man page before changing this
		     ) < 0) {
	printf ("init error: %s\n", license_errstr());
	exit(-1);
    }


    //
    // an example of how you can set licensing attributes
    //
    // in this one, we are setting the LM_A_RETRY_COUNT to 5
    // (see the FlexLM Programmer's Guide for an explaination)
    //
    if (license_set_attr(LM_A_RETRY_COUNT, (LM_A_VAL_TYPE)5)) {
	printf ("set attr: %s\n", license_errstr());
	exit(-1);
    }


    //
    // attempt to check out a license
    //
    // leave the first argument as is.  you only need to fill in
    // the feature name and version number.
    //
    if (license_chk_out(&code,     // leave as is
			"dummy",   // replace with your feature name
			"1.0"      // replace with your version number
			)) {
	printf ("check out error: %s\n", license_errstr());
	exit(-1);
    }


    //
    // application code goes here, please read the comments
    // for that function.
    //
    application_code();


    //
    // at the end of the application, you need to return the license
    // that was checked out
    //
    if (license_chk_in("dummy",   // replace with your feature name
		       0          // read man page before changing this
		       )) {
	printf ("check in error: %s\n", license_errstr());
	exit(-1);
    }

    exit(0);
}
