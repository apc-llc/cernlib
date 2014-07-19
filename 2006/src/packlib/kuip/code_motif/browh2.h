/*
 * $Id: browh2.h,v 1.1.1.1 1996/03/08 15:33:11 mclareni Exp $
 *
 * $Log: browh2.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:11  mclareni
 * Kuip
 *
 */
#ifndef _brow_h2
#define _brow_h2

/* Fortran routines and common blocks called from C and/or vice versa */
#ifdef QX_SC
#define List_addp_item F77_NAME(list_addp_item,LIST_ADDP_ITEM)
#define List_addc_item F77_NAME(list_addc_item,LIST_ADDC_ITEM)
#define list_delete_item     list_delete_item_
#define list_refresh         list_refresh_
#endif /* QX_SC */


#define FILE_LIST          1
#define DIR_LIST           2
#define MAXITEMS          50   /* max number of items in a popup menu */
#define MAXLEVEL           5   /* max number of levels in a popup menu */
#define MAX_FILE_BROWSER   5


/* type definitions */

typedef struct {
   Widget    widget;               /* file browser widget itself */
   Widget    dirlist;              /* the file browser's dirlist */
   Widget    file_popup;           /* popup menu for file list */
   Widget    dir_popup;            /* popup menu for dir list */
   Widget    pref_dialog;          /* preferences dialog */
   MenuCbStruct *pref_menu;        /* menu callback struct used by prefDialog */
   char      cd[MAXSTR];           /* CWD of file browser */
   int       hili_left;            /* left highlight position of pathText */
   int       hili_right;           /* right highlight position of pathText */
   int       titles;               /* list titles on/off */
   int       order;                /* ordering (grouping) of items on/off */
   char      filter[MAX_stringl];  /* filtering of items */
   Boolean   focus;                /* focus flag */
   Boolean   reset;                /* reset browser (update) flag */
   Boolean   path_edit;            /* true if pathText is being edited */
} BrowserStruct;

typedef struct {
        int  command_num;
        int  flag;
        Widget browser;
        int  panel;
        char pathname[MAX_string];
        char item[MAX_string];
        char command[MAX_string];
        BrAction *bract;
        BrAction *bract_next; /* for next item (toggle case) */
        KmObject *kmobj;
               } poptag_str;  /* tag structure for km_build_popup */


/* global data */
/* #define Extern must be in one routine (done in mkuip) to allocate space */
#ifndef Extern
#  define Extern extern
#endif
Extern BrowserStruct fbrowser[MAX_FILE_BROWSER];
Extern Widget        dirList;
Extern Widget        pref_dialog;
Extern MenuCbStruct *pref_menu;
Extern char          browser_dir[MAXSTR];
Extern int  titles;
Extern int     order;
Extern char    browser_filter[MAX_stringl];
Extern Boolean browser_focus, browser_reset;

#endif /* _brow_h2 */

