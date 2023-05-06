////////////////////////////////////////////////////////////////////////////////
///////   Copyright 1992, Silicon Graphics, Inc.  All Rights Reserved.   ///////
//                                                                            //
// This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;     //
// the contents of this file may not be disclosed to third parties, copied    //
// or duplicated in any form, in whole or in part, without the prior written  //
// permission of Silicon Graphics, Inc.                                       //
//                                                                            //
// RESTRICTED RIGHTS LEGEND:                                                  //
// Use,duplication or disclosure by the Government is subject to restrictions //
// as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data     //
// and Computer Software clause at DFARS 252.227-7013, and/or in similar or   //
// successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -    //
// rights reserved under the Copyright Laws of the United States.             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//
// This file contains two utility functions for dealing with pathnames.
// Pathdirname will return the directory portion of a path, and
// Pathexpandtilde will deal with "~" in the pathname.
//

#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#include <pwd.h>
#include "path.h"

char*
pathdirname(const char* path)
{
    register char*	c;
    char*		lastslash = 0;
    char*		retstr;

    for (c = (char *) path; *c; c++) {
	if (*c == '/') {
	    char* tmplastslash = c;
	    for (c++; *c == '/'; c++) {;}
	    			/* ignore redundant slashes */
	    if (*c) {		/* not superfluous trailing slash */
		lastslash = tmplastslash;
	    } else {		/* was superfluous trailing slash */
		break;
	    }
	}
    }

    if (lastslash == 0) {
	if (path[0] == '/') { 	/* path is "/" or has repeated "/" */
	    retstr = strdup("/");
	    return retstr;
	} else {		/* basename only implies current dir "." */
	    retstr = strdup(".");
	    return retstr;
	}
    }


    retstr = (char *) malloc(lastslash-path+1);
    strncpy(retstr, path, lastslash-path);
				/* copy only dir sans trailing /'s */
    retstr[lastslash-path] = '\0';

    return retstr;
}


char*
pathexpandtilde(const char* path)
{
    register char*	c;
    char		user[255];
    char*		homedir;
    char*		retstr;

    if (path[0] != '~') {
	retstr = strdup(path);
	return retstr;
    }
    
    if (path[1] == '/' || path[1] == '\0') {	/* ~/... or ~ */
	homedir = getenv("HOME");
	c = (char *) &(path[1]);
    } else {					/* ~<user>/... */
	struct passwd*	pwent;
	int i;

	c = (char *) &(path[1]);
	for (i=0; *c != '/' && *c != '\0'; c++, i++) {
	    user[i] = *c;
	}
	user[i] = '\0';

	pwent = getpwnam(user);
	if (pwent == 0) {			/* no such user */
	    retstr = strdup(path);
	    return retstr;
	}

	homedir = pwent->pw_dir;
    }

    if (homedir == 0) {
	homedir = "";				/* consistency with csh & ksh */
    }
    retstr = (char *) malloc(strlen(homedir) + strlen(c) + 1);
    strcpy(retstr, homedir);
    strcat(retstr, c);
    return retstr;
}

