/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
/*   $RCSfile: dlg.h,v $ $Revision: 1.1 $ $Date: 1992/05/08 18:13:52 $ */

/************************************************************
 *     dlg.h -- toolkit-independent dialogue layer
 ************************************************************/

extern void DlgKeepFileDialogueCB();
extern void DlgRevertToOpenCB();
extern void DlgNoteJustChangedCB();
extern void DlgNoteJustChangedSinceCB();

extern void DlgSelectOpenCB();
extern void DlgSelectSaveCB();
extern void DlgSelectCopyCB();
extern void DlgSelectMoveCB();

extern void DlgSelectCancelCB();

extern void DlgSaveYesCB();
extern void DlgSaveNoCB();
extern void DlgSaveCancelCB();
extern void DlgWarnCancelCB();
extern void DlgQuestionYesCB();

extern void DlgWantClearCB();
extern void DlgWantOpenCB();
extern void DlgWantSaveAsCB();
extern void DlgWantSaveCB();
extern void DlgWantCopyCB();
extern void DlgWantMoveCB();
extern void DlgWantRemoveCB();

extern void DlgExitCB();

