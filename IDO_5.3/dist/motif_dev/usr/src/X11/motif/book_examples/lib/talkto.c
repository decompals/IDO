/********************************************************************************
 *  xs_talkto(): connect two processes suchj that stdin of each reads from 
 *  stdout of the other
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  ??
  *
  *
  *  Copyright 1989 by Prentice Hall
  *  All Rights Reserved
  *
  * This code is based on the OSF/Motif widget set and the X Window System
  *
  * Permission to use, copy, modify, and distribute this software for 
  * any purpose and without fee is hereby granted, provided that the above
  * copyright notice appear in all copies and that both the copyright notice
  * and this permission notice appear in supporting documentation.
  *
  * Prentice Hall and the author disclaim all warranties with regard to 
  * this software, including all implied warranties of merchantability and fitness.
  * In no event shall Prentice Hall or the author be liable for any special,
  * indirect or cosequential damages or any damages whatsoever resulting from 
  * loss of use, data or profits, whether in an action of contract, negligence 
  * or other tortious action, arising out of or in connection with the use 
  * or performance of this software.
  *
  * Open Software Foundation is a trademark of The Open Software Foundation, Inc.
  * OSF is a trademark of Open Software Foundation, Inc.
  * OSF/Motif is a trademark of Open Software Foundation, Inc.
  * Motif is a trademark of Open Software Foundation, Inc.
  * DEC is a registered trademark of Digital Equipment Corporation
  * HP is a registered trademark of the Hewlett Packard Company
  * DIGITAL is a registered trademark of Digital Equipment Corporation
  * X Window System is a trademark of the Massachusetts Institute of Technology
  **********************************************************************************/
#include 	<stdio.h>
void xs_talkto(cmd)
   char   *cmd;
{
  int   to_child[2], /* pipe descriptors from parent->child */
        to_parent[2];/* pipe descriptors from child->parent */
  int   pid;
  pipe(to_child);
  pipe(to_parent);
  if (pid = fork(), pid == 0){    /* in the child   */
     close(0);                    /* redirect stdin */
     dup(to_child[0]);
     close(1);                    /* redirect stdout*/
     dup(to_parent[1]);
     close(to_child[0]);          /* close pipes    */
     close(to_child[1]);
     close(to_parent[0]);
     close(to_parent[1]);
     execlp(cmd, cmd, NULL);      /* exec the new cmd */
   }
   else if (pid > 0){             /* in the parent  */
      close(0);                   /* redirect stdin */
      dup(to_parent[0]);
      close(1);                   /* redirect stdout  */
      dup(to_child[1]);
      setbuf(stdout, NULL);       /* no buffered output */
      close(to_child[0]);         /* close pipes */
      close(to_child[1]);
      close(to_parent[0]);
      close(to_parent[1]);
    }
    else {                        /* error!       */
      fprintf(stderr,"Couldn't fork process %s\n", cmd);
      exit(1);
    }
}