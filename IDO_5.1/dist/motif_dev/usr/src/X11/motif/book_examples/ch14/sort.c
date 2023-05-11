/**********************************************************************************
  * sort.c: Display a binary sort tree using the Tree widget.
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages: 319-423
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


#include <stdio.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h> 
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/ScrolledW.h>
#include "Tree.h"

/*
 * Define the structure for a node in the binary sort tree.
 */
typedef struct _node {
  int            key;
  struct _node  *left;
  struct _node  *right;
} node;

extern node *insert_node();
extern node *make_node();
main(argc, argv)
    int argc;
    char **argv;
{
  Widget     toplevel, sw, tree;
  int        i;
  node      *head = NULL;
  int        digit;   
  Arg        wargs[10];
  toplevel = XtInitialize(argv[0], "Sort", NULL, 0, 
                          &argc, argv);
  /*
   * Put the tree in a scrolled window, to handle 
   * large trees.
   */
   XtSetArg(wargs[0], XmNscrollingPolicy, XmAUTOMATIC);
   sw = XtCreateManagedWidget("swindow",
                              xmScrolledWindowWidgetClass,
                              toplevel, wargs, 1);
   /*
    * Create the tree widget.
    */
   tree = XtCreateManagedWidget("tree", XstreeWidgetClass, 
                                sw, NULL, 0);
   /*
    * Create a binary sort tree from data read from stdin.
    */
   while(scanf("%d", &digit) != EOF)
      head = insert_node(digit, head);
   /*
    * Create the widgets representing the tree.
    */
   show_tree(tree, head, NULL);

   XtRealizeWidget(toplevel);
   XtMainLoop();
}

node *insert_node(key, head)
     int   key;
     node *head;
{
  node *prev, *ptr  = head;
  /*
   * If the tree doesn't exist, just create and 
   * return a new node.
   */
  if(!head)
    return (make_node(key));
  /*
   * Otherwise, find a leaf node, always following the 
   * left branches if the key is less than the value in each 
   * node, and the right branch otherwise.
   */
  while(ptr != NULL){ 
    prev = ptr; 
    ptr = (key < ptr->key) ? ptr->left : ptr->right;
  } 
  /*
   * Make a new node and attach it to the appropriate branch.
   */
  if (key < prev->key)
    prev->left = make_node(key);
  else 
    prev->right = make_node(key);
   return (head);
}

node *make_node(key)
     int   key;
{
  node  *ptr = (node *) malloc(sizeof(node));

  ptr->key  = key;
  ptr->left = ptr->right = NULL;

  return (ptr);
}

show_tree(parent, branch, super_node)
     Widget   parent;     
     node    *branch;     
     Widget   super_node; 
{
  Widget   w;
  Arg      wargs[3];
  int      n = 0;
  /*
   * If we've hit a leaf, return.
   */
  if(!branch) return;
  /*
   * Create a widget for the node, specifying the
   * given super_node constraint.
   */
  n = 0;
  XtSetArg(wargs[n], XtNsuperNode, super_node); n++;
  w  =  XtCreateManagedWidget("node", xmLabelWidgetClass, 
                              parent, wargs, n);
  xs_wprintf(w, "%d", branch->key);
  /*
   * Recursively create the subnodes, giving this node's 
   * widget as the super_node.
   */
  show_tree(parent, branch->left,  w);
  show_tree(parent, branch->right, w);
}
