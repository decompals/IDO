#ifndef __STRINGS_H__
#define __STRINGS_H__
#ident "$Revision: 1.9 $"
/*
 * Copyright (C) 1989 Silicon Graphics, Inc. All rights reserved.
 */


#ifdef __cplusplus
extern "C" {
#endif

#include <string.h>

/*
 * 4.3BSD versions of strchr and strrchr.
 */

extern char *index(const char *, int);
extern char *rindex(const char *, int);

#ifdef __cplusplus
}
#endif


#endif /* !__STRINGS_H__ */
