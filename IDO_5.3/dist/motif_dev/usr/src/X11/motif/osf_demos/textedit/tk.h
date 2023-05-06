/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
/*   $RCSfile: tk.h,v $ $Revision: 1.1 $ $Date: 1992/05/08 18:13:53 $ */
/************************************************************
 *     tk.h -- toolkit-specific dialogue layer
 ************************************************************/

#include "tkdef.h"

extern void TkBeep();
extern void TkExit();
extern void TkUpdateStatus();

extern TkTextChanged();
extern void TkTextActUnchangedSince();
extern TkTextChangedSince();

extern void TkTextClear();
extern void TkTextStore( );
extern char *TkTextRetrieve();

extern void TkAskFileToOpen();
extern void TkAskFileToSave();
extern void TkAskFileToCopy();
extern void TkAskFileToMove();
extern void TkDoneAskingFile();
extern void TkArrangeToOpen();

extern void TkAskSave();
extern void TkDoneAskingSave();

extern void TkWarn();
extern void TkWarnAndAskFileToSave();
extern void TkQuestionRemove();







