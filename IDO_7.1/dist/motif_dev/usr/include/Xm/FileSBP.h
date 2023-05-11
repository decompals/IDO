/* 
 * (c) Copyright 1989, 1990, 1991, 1992, 1993 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2.3
*/ 
/*   $RCSfile: FileSBP.h,v $ $Revision: 0.13 $ $Date: 1995/12/02 01:38:00 $ */
/*
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS. */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */
#ifndef _XmFSelectP_h
#define _XmFSelectP_h

#include <Xm/SelectioBP.h>
#include <Xm/FileSB.h>


#if defined(__sgi) && defined(NEED_SGM)
/* SGI_SGM_VERSION makes simpler #define's everywhere else */
#define SGI_SGM_VERSION
#endif /* __sgi */

#ifdef __cplusplus
extern "C" {
#endif

/* Defines for use in allocation geometry matrix. */

#define XmFSB_MAX_WIDGETS_VERT   10 

/* Bit locations for the state_flags bit field.
*/
#define XmFS_NO_MATCH		(1 << 0)
#define XmFS_IN_FILE_SEARCH	(1 << 1)
#define XmFS_DIR_SEARCH_PROC    (1 << 2)

/* Constraint part record for FileSelectionBox widget */

typedef struct _XmFileSelectionBoxConstraintPart
{
   char unused;
} XmFileSelectionBoxConstraintPart, * XmFileSelectionBoxConstraint;

/*  New fields for the FileSelectionBox widget class record  */

typedef struct
{
    XtPointer           extension;      /* Pointer to extension record */
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
    caddr_t _SG_vendorExtension;
#endif /* __sgi */
} XmFileSelectionBoxClassPart;


/* Full class record declaration */

typedef struct _XmFileSelectionBoxClassRec
{
   CoreClassPart       core_class;
   CompositeClassPart  composite_class;
   ConstraintClassPart constraint_class;
   XmManagerClassPart  manager_class;
   XmBulletinBoardClassPart    bulletin_board_class;
   XmSelectionBoxClassPart    selection_box_class;
   XmFileSelectionBoxClassPart    file_selection_box_class;
} XmFileSelectionBoxClassRec;

externalref XmFileSelectionBoxClassRec xmFileSelectionBoxClassRec;

#ifdef __sgi
# ifdef RESOLVE_RLD_CONFLICTS
    extern Widget (*SgcreateDialogFuncPtr)(Widget, String,ArgList,Cardinal);
    extern Widget (*SggetChildFuncPtr)(Widget,unsigned char);
# endif
# endif /* __sgi */



#if defined(__sgi) && defined(NEED_SGM) /* Specify SGI Manager instance record extension */
/**********************************************************************/
/**********************************************************************/
/* typedefs for Sg private structures */
#include <limits.h>

typedef enum  { RxDIR, RxEXE, RxOTHER, RxERROR }     FileType;

typedef struct __SG_XmFileSelectionBoxExtPart
{
  Widget finder;
  Widget filterDialog;
  Cursor waitCursor;
  Cursor normalCursor;
  Boolean use_enhanced_fsb;
  Boolean use_enhanced_filter;
  unsigned char browser_file_mask;
  Boolean viewer_mode;
  Widget viewer;
  Widget pane;
  Dimension viewer_width;
  XmString viewer_filter;
  XmString viewer_file;
  XtIntervalId viewer_interval_id;
  XtIntervalId completion_interval_id;
  int completion_delay;
  int viewer_update_delay;
  char *current_viewer_file;
  XtTranslations text_translations;
} _SG_XmFileSelectionBoxExtPart;

typedef struct __SG_XmFileSelectionBoxExt
{
  _SgInstanceExtensionRec	common;   /* Stuff all instance rec's have */
  _SG_XmFileSelectionBoxExtPart	rsrc;	  /* Resources & instance var's */
} _SG_XmFileSelectionBoxExtRec, *_SG_XmFileSelectionBoxExt;


#define _SG_FileSelectionBoxPtr(w) \
  ((_SG_XmFileSelectionBoxExt)(((XmFileSelectionBoxWidget)(w)) \
                               ->file_selection_box._SG_vendorExtension))

/* Access Macros */
#define FSB_Finder(w) (_SG_FileSelectionBoxPtr(w)->rsrc.finder)
#define FSB_FilterDialog(w) (_SG_FileSelectionBoxPtr(w)->rsrc.filterDialog)
#define FSB_WaitCursor(w) (_SG_FileSelectionBoxPtr(w)->rsrc.waitCursor)
#define FSB_NormalCursor(w) (_SG_FileSelectionBoxPtr(w)->rsrc.normalCursor)
#define FSB_useEnhanced(w) (_SG_FileSelectionBoxPtr(w)->rsrc.use_enhanced_fsb)
#define FSB_UseEnhancedFilter(w) \
        (_SG_FileSelectionBoxPtr(w)->rsrc.use_enhanced_filter)
#define FSB_ViewerMode(w) (_SG_FileSelectionBoxPtr(w)->rsrc.viewer_mode)
#define FSB_Viewer(w) (_SG_FileSelectionBoxPtr(w)->rsrc.viewer)
#define FSB_ViewerWidth(w) (_SG_FileSelectionBoxPtr(w)->rsrc.viewer_width)
#define FSB_ViewerFile(w) (_SG_FileSelectionBoxPtr(w)->rsrc.viewer_file)
#define FSB_ViewerFilter(w) (_SG_FileSelectionBoxPtr(w)->rsrc.viewer_filter)
#define FSB_Pane(w) (_SG_FileSelectionBoxPtr(w)->rsrc.pane)
#define FSB_ViewerIntervalId(w) \
        (_SG_FileSelectionBoxPtr(w)->rsrc.viewer_interval_id)
#define FSB_CompletionIntervalId(w) \
        (_SG_FileSelectionBoxPtr(w)->rsrc.completion_interval_id)
#define FSB_BrowserFileMask(w) \
        (_SG_FileSelectionBoxPtr(w)->rsrc.browser_file_mask)
#define FSB_CompletionDelay(w) \
        (_SG_FileSelectionBoxPtr(w)->rsrc.completion_delay)
#define FSB_ViewerUpdateDelay(w) \
        (_SG_FileSelectionBoxPtr(w)->rsrc.viewer_update_delay)
#define FSB_CurrentViewerFile(w) \
        (_SG_FileSelectionBoxPtr(w)->rsrc.current_viewer_file)
#define FSB_TextTranslations(w) \
        (_SG_FileSelectionBoxPtr(w)->rsrc.text_translations)

/**********************************************************************/
/**********************************************************************/
#endif /* __sgi */



/* New fields for the FileSelectionBox widget record */

typedef struct
{
    XmString        directory;        /* directory specification */
    XmString        pattern;          /* file search pattern */
    Widget          dir_list_label;   /* directory list Label */
    XmString        dir_list_label_string;/* directory list label text */
    Widget          dir_list;         /* directory List */
    XmString *      dir_list_items;   /* items in directory List */
    int             dir_list_item_count;/* number of items in directory List */
    int             dir_list_selected_item_position;
    Widget          filter_label;     /* file search filter label */
    XmString        filter_label_string;/* filter label text */
    Widget          filter_text;      /* filter text entry field */
    XmString        dir_mask;         /* string in filter text entry field */
    XmString        no_match_string;  /* string in list when no file match */
    XmQualifyProc   qualify_search_data_proc; /* directory and mask routine */
    XmSearchProc    dir_search_proc;  /* change directory routine */
    XmSearchProc    file_search_proc; /* file search routine */
    unsigned char   file_type_mask;   /* mask for type of files in file list */
    Boolean         list_updated;     /* flag to indicate file list update   */
    Boolean         directory_valid ; /* flag to indicate valid new directory*/
    unsigned char   state_flags ;     /* internal flags to indicate state.   */

#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
    caddr_t _SG_vendorExtension;
#endif /* __sgi */
} XmFileSelectionBoxPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _XmFileSelectionBoxRec
{
    CorePart	            core;
    CompositePart           composite;
    ConstraintPart          constraint;
    XmManagerPart           manager;
    XmBulletinBoardPart     bulletin_board;
    XmSelectionBoxPart      selection_box;
    XmFileSelectionBoxPart  file_selection_box;
} XmFileSelectionBoxRec;


/* Access macros */

#define FS_Directory( w) \
                (((XmFileSelectionBoxWidget)(w))->file_selection_box.directory)
#define FS_DirMask( w) \
                 (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_mask)
#define FS_DirListLabel( w) \
           (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_list_label)
#define FS_DirListLabelString( w) \
    (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_list_label_string)
#define FS_DirList( w) \
                 (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_list)
#define FS_DirListItems( w) \
           (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_list_items)
#define FS_DirListItemCount( w) \
      (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_list_item_count)
#define FS_FilterLabel( w) \
             (((XmFileSelectionBoxWidget)(w))->file_selection_box.filter_label)
#define FS_FilterLabelString( w) \
      (((XmFileSelectionBoxWidget)(w))->file_selection_box.filter_label_string)
#define FS_FilterText( w) \
              (((XmFileSelectionBoxWidget)(w))->file_selection_box.filter_text)
#define FS_Pattern( w) \
                  (((XmFileSelectionBoxWidget)(w))->file_selection_box.pattern)
#define FS_NoMatchString( w) \
          (((XmFileSelectionBoxWidget)(w))->file_selection_box.no_match_string)
#define FS_QualifySearchDataProc( w) (((XmFileSelectionBoxWidget) \
                             (w))->file_selection_box.qualify_search_data_proc)
#define FS_DirSearchProc( w) \
          (((XmFileSelectionBoxWidget)(w))->file_selection_box.dir_search_proc)
#define FS_FileSearchProc( w) \
         (((XmFileSelectionBoxWidget)(w))->file_selection_box.file_search_proc)
#define FS_RealDefaultButton( w) \
      (((XmFileSelectionBoxWidget)(w))->file_selection_box.real_default_button)
#define FS_FileTypeMask( w) \
           (((XmFileSelectionBoxWidget)(w))->file_selection_box.file_type_mask)
#define FS_ListUpdated( w) \
             (((XmFileSelectionBoxWidget)(w))->file_selection_box.list_updated)
#define FS_DirectoryValid( w) \
          (((XmFileSelectionBoxWidget)(w))->file_selection_box.directory_valid)
#define FS_StateFlags( w) \
              (((XmFileSelectionBoxWidget)(w))->file_selection_box.state_flags)
#define FS_DirListSelectedItemPosition( w) (((XmFileSelectionBoxWidget) w) \
                          ->file_selection_box.dir_list_selected_item_position)


/********    Private Function Declarations    ********/
#ifdef _NO_PROTO


#else


#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmFSelectP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
