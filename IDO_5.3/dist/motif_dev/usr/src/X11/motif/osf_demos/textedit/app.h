/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
/*   $RCSfile: app.h,v $ $Revision: 1.1 $ $Date: 1992/05/08 18:13:52 $ */

/************************************************************
 *     app.h -- toolkit-independent code
 ************************************************************/

extern char *AppBufferName();

extern char *AppReadFile( );
extern void AppSaveFile( );
extern void AppTransferFile( );

extern void AppNewFile( );
extern AppRemoveFile();

extern AppOpenReadFile( );
extern AppOpenSaveFile( );
extern AppOpenTransferFile( );

extern void AppCompleteSaveAsFile( );
extern void AppCompleteCopyFile( );
extern AppCompleteMoveFile();


