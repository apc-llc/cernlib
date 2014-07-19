/*
 * $Id: kuip.h,v 1.1.1.1 1996/03/01 11:39:12 mclareni Exp $
 *
 * $Log: kuip.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:12  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.35.53  by  Fons Rademakers*/
/*-- Author :    Fons Rademakers   23/11/92*/
/***********************************************************************
 *
 * Declarations and definitions of KUIP user routines.
 *
 **********************************************************************/
#ifndef _kuip_h
#define _kuip_h


#define KM_MAIN_CURSOR    0
#define KM_WAIT_CURSOR    1


typedef struct _BrClientdata {
  int        tag;
  char       *brobj;
  char       *brcls;
  char       *path;
  char       *kmobj;
  char       *kmcls;
  char       *stext;
  char       *ltext;
  char       *mtext;
} BrClientdata;

typedef void (*KxtermActionProc)(
    char**,             /* params */
    int                 /* num_params */
);

typedef struct _KxtermActionsRec{
    char               *string;
    KxtermActionProc    proc;
} KxtermActionsRec;

typedef KxtermActionsRec  *KxtermActionList;


#ifdef _NO_PROTO
extern void      km_all_cursor();
extern Boolean   km_double_click();
extern void      km_reset_allbrowser();
extern void      km_store_appShell();
extern void      km_reset_appShell();
extern void      km_warn_user();
extern void      km_inform_user();
extern void      km_file_type();
extern char    **km_scan_dir();
extern void      kxterm_add_actions();
extern void      send_kxterm_cmd();
#else   /* _NO_PROTO */
extern void      km_all_cursor(int);
extern Boolean   km_double_click(int);
extern void      km_reset_allbrowser(Boolean);
extern void      km_store_appShell(Widget, char *);
extern void      km_reset_appShell(Widget);
extern void      km_warn_user(Widget, char *, char *);
extern void      km_inform_user(Widget, char *, char *);
extern void      km_file_type(char *, char *, char *, int);
extern char    **km_scan_dir(char *, char *, int, int);
extern void      kxterm_add_actions(KxtermActionList);
extern void      send_kxterm_cmd(char**);
#endif  /* _NO_PROTO */

#endif  /* _kuip_h */
