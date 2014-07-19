/*
 * $Id: scan.c,v 1.4 1996/03/28 16:13:31 cremel Exp $
 *
 * $Log: scan.c,v $
 * Revision 1.4  1996/03/28 16:13:31  cremel
 * Suppress the button "Previous" in Paw++ "Ntuple Scanner" window: it cannot
 * work with the new QP.
 *
 * Revision 1.3  1996/03/22 14:07:34  cremel
 * nt/scan: change default value (see cdf) to 99999999 (intead of 9999999).
 *
 * Revision 1.2  1996/03/19 12:51:15  cremel
 * BUG correction in Paw++ cut editor (happened for a long cut expression when
 * the number of cuts was > 11). The problem was that cut_expression had
 * dimension MAX_CMD_LENGTH (512) whereas local variable cut was MAX_STRING (80).
 *
 * Revision 1.1.1.1  1996/03/01 11:38:56  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.05/10 27/06/94  14.56.30  by  Alfred Nathaniel*/
/*-- Author :    Fons Rademakers   26/11/92*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/ToggleB.h>

#include "hmotif/pawm.h"
#include "hmotif/scan.h"


/* ntuple scan window icon */
#define scan_width 50
#define scan_height 50
static char scan_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x38, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1c, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x1e, 0x00, 0x00, 0x3e, 0x60, 0x00, 0x00, 0x1e,
   0x00, 0xc0, 0xff, 0x79, 0x3c, 0x00, 0x0e, 0x00, 0xc0, 0xc1, 0xf9, 0x78,
   0x00, 0x0e, 0x00, 0xc0, 0xc0, 0xd9, 0x60, 0x00, 0x0e, 0x00, 0xc0, 0xc0,
   0xd9, 0x60, 0x00, 0x0e, 0x00, 0xc0, 0xe0, 0xd9, 0x40, 0x00, 0x0e, 0x00,
   0xc0, 0xe0, 0xdd, 0xc1, 0x00, 0x0e, 0x00, 0xc0, 0x61, 0x9c, 0xc1, 0x00,
   0x0e, 0x00, 0xc0, 0x3b, 0x9c, 0xc1, 0x1c, 0x0e, 0x00, 0xc0, 0x1f, 0x8e,
   0xc1, 0x1d, 0x0e, 0x00, 0xc0, 0x00, 0x0e, 0x83, 0x3d, 0x0e, 0x00, 0xc0,
   0x00, 0x8f, 0x83, 0x3d, 0x07, 0x00, 0xc0, 0x00, 0xff, 0x87, 0xef, 0x03,
   0x00, 0xc0, 0x80, 0x7f, 0x07, 0xef, 0x01, 0x00, 0xcc, 0x01, 0x06, 0x0e,
   0xe7, 0x01, 0x00, 0xfc, 0x01, 0x06, 0x1e, 0xc7, 0x00, 0x00, 0xf8, 0x01,
   0x07, 0x1c, 0xc6, 0x00, 0x00, 0xf0, 0x83, 0x07, 0x3c, 0x86, 0x00, 0x00,
   0x80, 0x87, 0x07, 0x7c, 0x04, 0x00, 0x00, 0x00, 0x07, 0x07, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x07, 0x00,
   0x00, 0x00, 0x00, 0x00, 0xe0, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x70,
   0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0xc0, 0x03, 0x00, 0x00, 0x00, 0x1e, 0x00, 0x80, 0x0f,
   0x70, 0xc0, 0x64, 0x7e, 0x00, 0x00, 0x1e, 0xf8, 0xe0, 0xc5, 0x73, 0x00,
   0x00, 0x38, 0x0c, 0x31, 0xc7, 0x60, 0x00, 0x00, 0x30, 0x0c, 0x11, 0xc6,
   0x60, 0x00, 0x00, 0x70, 0x0e, 0x18, 0xc6, 0x60, 0x00, 0x00, 0x60, 0x06,
   0x18, 0xc6, 0x60, 0x00, 0x00, 0x70, 0x0e, 0x18, 0xc6, 0xc1, 0x00, 0x18,
   0x30, 0x0c, 0x18, 0x86, 0xc1, 0x00, 0x18, 0x1c, 0x0c, 0x19, 0x86, 0xc1,
   0x00, 0xb0, 0x0f, 0x98, 0x71, 0x8f, 0xc1, 0x00, 0xf0, 0x07, 0xf0, 0xc0,
   0x0d, 0x00, 0x00, 0xe0, 0x01, 0x60, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00};


/* extern functions */
extern Widget create_ntupleScan(void);
extern Widget create_columnSelection(Widget);
extern void enter_ntuple_viewer(Widget, XtPointer, XEnterWindowEvent *);

/* extern widgets */
extern Widget ntupleBrowser;   /* from Ntuple Browser dialog */
extern Widget titleNtLabel;
extern Widget firstNtText;
extern Widget nrowNtText;
extern Widget varNtList;
extern Widget ignoreCutNtToggle;

extern Widget var1NtSList;     /* from columnSelection dialog */
extern Widget var2NtSList;
extern Widget exprNtSText;
extern Widget copyNtSButton;
extern Widget copyAllNtSButton;
extern Widget deleteNtSButton;
extern Widget resetNtSButton;
extern Widget scanNtSButton;
extern Widget cancelNtSButton;

/* global data */
extern char  cut_expression[];

/* local functions */
static void   copy_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void   reset_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void   show_ntupleScanner(Widget, XtPointer, XmAnyCallbackStruct *);
static int    get_scan_context(Widget);
static void   save_scan_context(Widget);
static void   popdown_cb(Widget, Widget, XmAnyCallbackStruct *);
static void   next_cb(Widget, Widget, XmAnyCallbackStruct *);
static void   execute_scan_cmd(Widget, Widget);
static Widget find_scanner(Widget child);
static void   matrix_enter_cb(Widget, XtPointer,
                              XbaeMatrixEnterCellCallbackStruct *);
static void   matrix_select_cb(Widget, XtPointer,
                               XbaeMatrixSelectCellCallbackStruct *);

/* translation table for matrix widget */
static char *translation_table =
   "<Btn1Down>:     SelectCell()\n\
    <Btn1Motion>:   SelectCell()";

/* local data */
static Widget               selector_dialog;
static Boolean              no_scan;
static Boolean              reverse_order;
static int                  max_rows;
static int                  n_scanners = 0;
static NtupleScanStruct     scanner[MAX_SCANNERS];
static int                  ntuple_id;
static char                 ntuple_dir[MAX_STRING];
static int                  ntuple_nrows;
static Widget               matrix;
static int                  nrows;
static int                  ncols;
static int                  last_event;
static int                  no_vars;
static char                 var_list[MAX_CMD_LENGTH];


/***********************************************************************
 *                                                                     *
 *   Show (manage) the column selector dialog.                         *
 *                                                                     *
 ***********************************************************************/
void show_columnSelector(Widget w, XtPointer cd,
                         XmAnyCallbackStruct *call_data)
{
   MenuCbStruct   *scan_button;
   int             icnt;
   XmString       *items;

   if (!selector_dialog) {
      selector_dialog = create_columnSelection(w);

      /*
       *   Install callbacks
       */
      scan_button = make_menuStruct(w, selector_dialog);
      InstallMwmCloseCallback(XtParent(selector_dialog),
                              (XtCallbackProc)cancel_cb,
                              (XtPointer)scan_button);

      /*
       *   Install event handler on EnterNotify events
       */
      XtAddEventHandler(XtParent(selector_dialog), EnterWindowMask, False,
                        (XtEventHandler)enter_ntuple_viewer, NULL);

      XtAddCallback(exprNtSText, XmNactivateCallback,
                    (XtCallbackProc)copy_cb, NULL);
      XtAddCallback(copyNtSButton, XmNactivateCallback,
                    (XtCallbackProc)copy_cb, NULL);
      XtAddCallback(copyAllNtSButton, XmNactivateCallback,
                    (XtCallbackProc)copy_cb, NULL);
      XtAddCallback(var1NtSList, XmNdefaultActionCallback,
                    (XtCallbackProc)copy_cb, NULL);
      XtAddCallback(scanNtSButton, XmNactivateCallback,
                    (XtCallbackProc)show_ntupleScanner, NULL);
      XtAddCallback(deleteNtSButton, XmNactivateCallback,
                    (XtCallbackProc)reset_cb, NULL);
      XtAddCallback(resetNtSButton, XmNactivateCallback,
                    (XtCallbackProc)reset_cb, NULL);
      XtAddCallback(cancelNtSButton, XmNactivateCallback,
                    (XtCallbackProc)cancel_cb, scan_button);

      /* store MenuCbSruct in userData */
      XtVaSetValues(selector_dialog, XmNuserData, scan_button, NULL);
   }

   /* clear left list */
   XmListSetPos(var1NtSList, 1);
   XmListDeselectAllItems(var1NtSList);
   XmListDeleteAllItems(var1NtSList);

   /*
    * Copy var list from ntupleBrowser to left list of columnSelection
    * dialog. In case there are no variables in the ntupleBrowser return.
    */
   XtVaGetValues(varNtList, XmNitemCount, &icnt,
                            XmNitems,     &items, NULL);
   if (!icnt) return;

   XmListAddItems(var1NtSList, items, icnt, 1);
   XmListSelectPos(var1NtSList, 1, False);

   /* Make scan button on ntupleBrowser insensitive and popup dialog */
   XtSetSensitive(w, False);
   XtManageChild(selector_dialog);
}

/***********************************************************************
 *                                                                     *
 *   Unmanage dialog. This routine should be called by the             *
 *   popdown_cb routine of the parent.                                 *
 *                                                                     *
 ***********************************************************************/
void unmanage_selector_dialog(void)
{
   if (selector_dialog) {
      MenuCbStruct  *item;
      XtVaGetValues(selector_dialog, XmNuserData, &item, NULL);
      popdown_argument_dialog(item);
   }
}

/***********************************************************************
 *                                                                     *
 *   Clear the selector columns.                                       *
 *                                                                     *
 ***********************************************************************/
void clear_selector_columns(int option)
{
   if (selector_dialog) {
      if (option == 0 || option == 1) {
         XmListSetPos(var1NtSList, 1);
         XmListDeselectAllItems(var1NtSList);
         XmListDeleteAllItems(var1NtSList);
         XmListSelectPos(var1NtSList, 1, False);
      }

      if (option == 0 || option == 2) {
         XmListSetPos(var2NtSList, 1);
         XmListDeselectAllItems(var2NtSList);
         XmListDeleteAllItems(var2NtSList);
      }
   }
}

/***********************************************************************
 *                                                                     *
 *   Set the left selector column.                                     *
 *                                                                     *
 ***********************************************************************/
void set_selector_column(XmString xmstr, int pos)
{
   if (selector_dialog)
      XmListAddItem(var1NtSList, xmstr, pos);
}

/***********************************************************************
 *                                                                     *
 *   Copy selected items from the left list to the right list.         *
 *                                                                     *
 ***********************************************************************/
static void copy_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   int         icnt, pos, pcnt, *posl;
   XmString   *items;

   if (XmListGetSelectedPos(var2NtSList, &posl, &pcnt)) {
      pos = posl[pcnt-1] + 1;
      XtFree((char *)posl);
   } else
      pos = 0;
   XmListDeselectAllItems(var2NtSList);

   if (w == exprNtSText) {

      char     *expr = XmTextGetString(w);

      if (!strempty(expr)) {
         XmString xms = XmStringCreateSimple(expr);

         XmListAddItem(var2NtSList, xms, pos);

         XmStringFree(xms);
      }
      XtFree(expr);

      /*
       * ugly... I don't want to show the scanner after hitting the
       * KActivate in the text widget
       */
      no_scan = True;

   } else if (w == copyNtSButton) {

      XtVaGetValues(var1NtSList, XmNselectedItemCount, &icnt,
                                 XmNselectedItems,     &items,
                                 NULL);

      XmListAddItems(var2NtSList, items, icnt, pos);

   } else if (w == copyAllNtSButton) {

      XtVaGetValues(var1NtSList, XmNitemCount, &icnt,
                                 XmNitems,     &items,
                                 NULL);

      clear_selector_columns(2);
      XmListAddItems(var2NtSList, items, icnt, pos);

   } else if (w == var1NtSList) {

      XmListCallbackStruct *cbsl = (XmListCallbackStruct *) cbs;
      if (cbsl->event->type == ButtonRelease)
         XmListAddItem(var2NtSList, cbsl->item, pos);

   }
   XmListSelectPos(var2NtSList, pos, False);
}

/***********************************************************************
 *                                                                     *
 *   Reset the right list.                                             *
 *                                                                     *
 ***********************************************************************/
static void reset_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   int         icnt;
   XmString   *items;

   if (w == deleteNtSButton) {
      XtVaGetValues(var2NtSList, XmNselectedItemCount, &icnt,
                                 XmNselectedItems,     &items,
                                 NULL);

      XmListDeleteItems(var2NtSList, items, icnt);

   } else if (w == resetNtSButton) {
      XmListSetPos(var2NtSList, 1);
      XmListDeselectAllItems(var2NtSList);
      XmListDeleteAllItems(var2NtSList);
   }
}

/***********************************************************************
 *                                                                     *
 *   Show (manage) the ntuple scanner.                                 *
 *                                                                     *
 ***********************************************************************/
static void show_ntupleScanner(Widget w, XtPointer cd,
                        XmAnyCallbackStruct *call_data)
{
   Widget        widget;
   Pixmap        pixmap;
   Display      *display;
   char         *title, *first, *nrow;
   XmString      xmtit;
   int           i, ifirst, nevent;

   if (ntuple.id == 0) return;

   if (no_scan) {
      no_scan = False;
      return;
   }

   /*
    * Copy items from the ntuple structure for local use.
    * These items are necessary for being able to scan an Ntuple that
    * is no longer in the Ntuple viewer.
    */
   ntuple_id    = ntuple.id;
   ntuple_nrows = ntuple.nrows;
   strcpy(ntuple_dir, ntuple.dir);

   if (n_scanners >= MAX_SCANNERS) {
      km_inform_user(w, "Cannot open more Ntuple scanners.", "");
      return;
   }

   widget = create_ntupleScan();

   (void)get_scan_context(widget);

   /* set icon for scan window */
   display = XtDisplay(widget);
   pixmap = XCreateBitmapFromData(display, DefaultRootWindow(display),
                                  scan_bits, scan_width,
                                  scan_height);

   for (i = 0; i < MAX_SCANNERS; i++) {
      if (!scanner[i].widget) {
         title = XtCalloc(MAX_STRING, 1);
         sprintf(title, "Ntuple Scanner %d", i+1);
         XtVaSetValues(XtParent(widget), XmNtitle, title,
                                         XmNiconName, title,
                                         XmNiconPixmap, pixmap,
                                         XmNdeleteResponse, XmUNMAP,
                                         NULL);

         /* make scanner known to KUIP's Exec window */
         km_store_appShell(widget, title);

         XtFree(title);
         break;
      }
   }

   /*
    *   Make sure that if the window is popped down, the scanner is
    *   properly destroyed.
    */
   XtAddCallback(XtParent(widget), XmNpopdownCallback,
                 (XtCallbackProc)popdown_cb, widget);

   /*
    *   Install close callback
    */
   XtAddCallback(closeScButton, XmNactivateCallback,
                 (XtCallbackProc)close_cb, widget);

   /*
    *   Install callbacks for text, re-scan, next and previous buttons
    */
   XtAddCallback(firstScText, XmNactivateCallback,
                 (XtCallbackProc)next_cb, widget);
   XtAddCallback(nrowScText, XmNactivateCallback,
                 (XtCallbackProc)next_cb, widget);
   XtAddCallback(scanScButton, XmNactivateCallback,
                 (XtCallbackProc)next_cb, widget);
   XtAddCallback(nextScButton, XmNactivateCallback,
                 (XtCallbackProc)next_cb, widget);
   XtAddCallback(previousScButton, XmNactivateCallback,
                 (XtCallbackProc)next_cb, widget);
   XtSetSensitive(previousScButton, False);
   /* "Previous" does not work with new QP */
   XtVaSetValues(previousScButton, XmNmappedWhenManaged,  False, NULL);

   /*
    *   Fill in the first and number of row fields as well as the
    *   Ntuple title label
    */
   first = XmTextGetString(firstNtText);
   nrow  = XmTextGetString(nrowNtText);
   ifirst = atoi(first);
   nevent = atoi(nrow);
   if (nevent > MAX_ROWS) nevent = MAX_ROWS;
   if (ifirst+nevent-1 >= ntuple_nrows)
      XtSetSensitive(nextScButton, False);
   XtFree(nrow);
   nrow = XtCalloc(20, 1);
   sprintf(nrow, "%d", nevent);
   XmTextSetString(firstScText, first);
   XmTextSetString(nrowScText, nrow);
   XtFree(first);
   XtFree(nrow);

   XtVaGetValues(titleNtLabel, XmNlabelString, &xmtit, NULL);
   XtVaSetValues(titleScLabel, XmNlabelString,  xmtit, NULL);

   /* execute the scan command */
   matrix  = (Widget) NULL;
   no_vars = -1;
   execute_scan_cmd(w, widget);

   /* finally popup the scanner */
   UxPopupInterface(widget, XtGrabNone);

   n_scanners++;
   for (i = 0; i < MAX_SCANNERS; i++) {
      if (!scanner[i].widget) {
         scanner[i].widget = widget;
         break;
      }
   }
   save_scan_context(widget);
}

/***********************************************************************
 *                                                                     *
 *   Get the context for scanner widget.                               *
 *                                                                     *
 ***********************************************************************/
static int get_scan_context(Widget w)
{
   int i;

   if (!w) return -1;

   /*
    *   find and restore the context
    *   (to get to the widgets belonging to this dialog)
    */
   UxNtupleScanContext = (_UxCntupleScan *)UxGetContext(w);

   /* restore the context not handled by the Architect context */
   for (i = 0; i < MAX_SCANNERS; i++) {
      if (scanner[i].widget == w) {
         matrix       = scanner[i].matrix;
         nrows        = scanner[i].nrows;
         ncols        = scanner[i].ncols;
         last_event   = scanner[i].last_event;
         ntuple_id    = scanner[i].ntuple_id;
         ntuple_nrows = scanner[i].ntuple_nrows;
         no_vars      = scanner[i].no_vars;
         strcpy(var_list,   scanner[i].var_list);
         strcpy(ntuple_dir, scanner[i].ntuple_dir);
         return i;
      }
   }
   return -1;
}

/***********************************************************************
 *                                                                     *
 *   Save the context for scanner widget.                              *
 *                                                                     *
 ***********************************************************************/
static void save_scan_context(Widget w)
{
   int i;

   for (i = 0; i < MAX_SCANNERS; i++) {
      if (scanner[i].widget == w) {
         scanner[i].matrix       = matrix;
         scanner[i].nrows        = nrows;
         scanner[i].ncols        = ncols;
         scanner[i].last_event   = last_event;
         scanner[i].ntuple_id    = ntuple_id;
         scanner[i].ntuple_nrows = ntuple_nrows;
         scanner[i].no_vars      = no_vars;
         strcpy(scanner[i].var_list,   var_list);
         strcpy(scanner[i].ntuple_dir, ntuple_dir);
         break;
      }
   }
}

/***********************************************************************
 *                                                                     *
 *   Destroy the scanner widget.                                       *
 *                                                                     *
 ***********************************************************************/
static void popdown_cb(Widget w, Widget scan,
                XmAnyCallbackStruct *call_data)
{
   int   i;

   n_scanners--;
   for (i = 0; i < MAX_SCANNERS; i++) {
      if (scanner[i].widget == scan) {
         scanner[i].widget        = (Widget) NULL;
         scanner[i].matrix        = (Widget) NULL;
         scanner[i].nrows         = 0;
         scanner[i].ncols         = 0;
         scanner[i].last_event    = 0;
         scanner[i].ntuple_id     = 0;
         scanner[i].ntuple_dir[0] = '\0';
         scanner[i].ntuple_nrows  = 0;
         scanner[i].no_vars       = 0;
         scanner[i].var_list[0]   = '\0';
         break;
      }
   }

   /* remove from KUIP's exec window */
   km_reset_appShell(scan);

   UxDestroyInterface(scan);
}

/***********************************************************************
 *                                                                     *
 *   Find the scanner widget.                                          *
 *                                                                     *
 ***********************************************************************/
static Widget find_scanner(Widget child)
{
   String  name;
   Widget  scan;

   /*
    * starting from a child widget find back the
    * parent widget to set the right context.
    */
   scan = child;
   do {
      scan = XtParent(scan);
      name = XtName(scan);
   } while (scan && strcmp(name, "ntupleScan"));

   return scan;
}

/***********************************************************************
 *                                                                     *
 *   Scan the next or previous "number of row" rows.                   *
 *                                                                     *
 ***********************************************************************/
static void next_cb(Widget w, Widget scan, XmAnyCallbackStruct *call_data)
{
   char  *first, *nrow;
   int    ifirst, nevent;

   (void)get_scan_context(scan);

   if (w != scanScButton) {

      first = XmTextGetString(firstScText);
      nrow  = XmTextGetString(nrowScText);
      ifirst = atoi(first);
      nevent = atoi(nrow);

      if (w == nextScButton) {
         ifirst = last_event + 1;
         if (ifirst+nevent-1 >= ntuple_nrows)
            XtSetSensitive(nextScButton, False);
         XtSetSensitive(previousScButton, True);
      }

      if (w == previousScButton) {
         /* ifirst range check is done in matrix_row */
         XtSetSensitive(nextScButton, True);
      }

      if (w == firstScText) {
         XtSetSensitive(nextScButton,     True);
         XtSetSensitive(previousScButton, True);
         if (ifirst <= 1) {
            ifirst = 1;
            XtSetSensitive(previousScButton, False);
         }
         if (ifirst+nevent-1 >= ntuple_nrows) {
            if (ifirst > ntuple_nrows)
               ifirst = ntuple_nrows;
            XtSetSensitive(nextScButton, False);
         }
      }

      if (w == nrowScText) {
         if (nevent < 1)        nevent = 1;
         if (nevent > MAX_ROWS) nevent = MAX_ROWS;
         if (ifirst+nevent-1 >= ntuple_nrows)
            XtSetSensitive(nextScButton, False);
         else
            XtSetSensitive(nextScButton, True);
      }

      XtFree(first);
      first = XtCalloc(20, 1);
      sprintf(first, "%d", ifirst);
      XmTextSetString(firstScText, first);
      sprintf(first, "%d", nevent);
      XmTextSetString(nrowScText, first);

      XtFree(first);
      XtFree(nrow);
   }

   /* execute the scan command */
   execute_scan_cmd(w, scan);

   save_scan_context(scan);

}

/***********************************************************************
 *                                                                     *
 *   Execute the scan command for scanner widget.                      *
 *                                                                     *
 ***********************************************************************/
static void execute_scan_cmd(Widget button, Widget scan)
{
   char      *cmd, *cut, *first, *nrow, *vlist, *var, *s;
   int        ifirst, nevent, nvar, cnt, i, update;
   XmString  *xmlst, xmstr;

   cmd   = XtCalloc(MAX_CMD_LENGTH, 1);
   vlist = XtCalloc(MAX_CMD_LENGTH, 1);
   /* cut   = XtCalloc(MAX_STRING, 1); */

   if (no_vars == -1) {

      /*
       * get the variables to be scanned from the right-hand list of the
       * column selection dialog
       */
      XtVaGetValues(var2NtSList, XmNitemCount, &cnt,
                                 XmNitems,     &xmlst,
                                 NULL);

      nvar = 0;
      for (i = 0; i < cnt; i++) {
         nvar++;
         if (nvar > MAX_COLS) {
            nvar--;
            km_inform_user(button, "Cannot scan more than 512 columns.", "");
            break;
         }
         var = extract_normal_string(xmlst[i]);
         /* get rid of extended info */
         if ((s = strchr(var, '(')) || (s = strchr(var, '[')) ||
             (s = strchr(var, ':')))
            *s = '\0';
         strcat(vlist, var); strcat(vlist, " ");
         XtFree(var);
      }

      no_vars = nvar;
      strcpy(var_list, vlist);

   } else {

      nvar = no_vars;
      strcpy(vlist, var_list);

   }

   /* get the cuts */
   if (strempty(cut_expression) || XmToggleButtonGetState(ignoreCutNtToggle))
      /* strcpy(cut, "!"); */
      cut = strdup ("!");
   else
      /* strcpy(cut, cut_expression); */
      cut = strdup (cut_expression);

   first    = XmTextGetString(firstScText);
   nrow     = XmTextGetString(nrowScText);
   ifirst   = atoi(first);
   max_rows = atoi(nrow);

   /* check range values */
   update = 0;
   if (ifirst < 1) {
      ifirst = 1;
      update++;
   }
   if (ifirst > ntuple_nrows) {
      ifirst = ntuple_nrows;
      update++;
   }
   if (max_rows < 1) {
      max_rows = 1;
      update++;
   }
   if (max_rows > MAX_ROWS) {
      max_rows = MAX_ROWS;
      update++;
   }
   if (update) {
      XtFree(first);
      first = XtCalloc(15, 1);
      sprintf(first, "%d", ifirst);
      XmTextSetString(firstScText, first);
      sprintf(first, "%d", max_rows);
      XmTextSetString(nrowScText, first);
   }

   if (button == previousScButton) {
      ifirst--;
      nevent = -99999999;
      reverse_order = True;
   } else {
      nevent = 99999999;
      reverse_order = False;
   }

   /* construct and execute Paw command */
   if (nvar)
      sprintf(cmd, "Ntuple/Scan %s/%d %s %d %d %d %s", ntuple_dir, ntuple_id,
              cut, nevent, -ifirst, nvar, vlist);
   else
      sprintf(cmd, "Ntuple/Scan %s/%d %s %d %d", ntuple_dir, ntuple_id,
              cut, nevent, -ifirst);

   km_all_cursor(KM_WAIT_CURSOR);

   execute_kuip_cmd(cmd);

   /* show number of rows and columns displayed in scanner */
   cut = malloc( MAX_STRING );
   sprintf(cut, "%d R  %d C", nrows, ncols);
   xmstr = XmStringCreate(cut, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(columnsScLabel, XmNlabelString, xmstr, NULL);
   XmStringFree(xmstr);

   km_all_cursor(KM_MAIN_CURSOR);

   XtFree(cmd);
   /* XtFree(cut); */
   free (cut);
   XtFree(vlist);
   XtFree(first);
   XtFree(nrow);
}

/***********************************************************************
 *                                                                     *
 *   Add the labels to the matrix widget. This routine is called by    *
 *   fortran routine PASCA2. It also creates the matrix widget.        *
 *                                                                     *
 ***********************************************************************/
#ifdef vms
void matrix_label(struct dsc$descriptor_s *label_dsc, int *nlab)
{
   char *label = label_dsc->dsc$a_pointer;
#else
void matrix_label(char *label, int *nlab)
{
#endif
   Widget   clip;
   int      i, old_nrows;
   char   **labels, **rlab, *s, lab[MAXLAB+1];
   unsigned char *ca;
   short   *cw;

   if (ntuple_id == 0) return;

   ncols     = *nlab;
   old_nrows = nrows;
   nrows     = 0;
   s         = label;

   labels = (char **) XtMalloc(ncols * sizeof(char *));
   rlab   = (char **) XtMalloc(1 * sizeof(char *));
   cw     = (short *) XtMalloc(ncols * sizeof(short));
   ca     = (unsigned char *) XtMalloc(ncols * sizeof(unsigned char));

   for (i = 0; i < ncols; i++) {
      strncpy(lab, s, MAXLAB); lab[MAXLAB] = '\0';
      labels[i] = km_strip(lab);
      cw[i]     = 15;
      ca[i]     = XmALIGNMENT_CENTER;
      s        += MAXLAB;
   }

   /* empty row label */
   rlab[0] = XtMalloc(2);
   strcpy(rlab[0], " ");

   if (!matrix) {
      /*
       * Create a Matrix widget
       */
      matrix = XtVaCreateManagedWidget("matrix",
                                xbaeMatrixWidgetClass,    ntupleScan,
                                XmNleftOffset,            5,
                                XmNleftAttachment,        XmATTACH_FORM,
                                XmNrightOffset,           5,
                                XmNrightAttachment,       XmATTACH_FORM,
                                XmNtopWidget,             separatorGadget5,
                                XmNtopOffset,             5,
                                XmNtopAttachment,         XmATTACH_WIDGET,
                                XmNbottomWidget,          separatorGadget4,
                                XmNbottomOffset,          5,
                                XmNbottomAttachment,      XmATTACH_WIDGET,
                                XmNcolumnLabels,          labels,
                                XmNcolumnWidths,          cw,
                                XmNcolumnLabelAlignments, ca,
                                XmNcolumns,               ncols,
                                XmNrowLabels,             rlab,
                                XmNrowLabelWidth,         (short) 6,
                                XmNrows,                  1,
                                NULL);

      /* overridebtranslation table, also for clip widget */
      XtOverrideTranslations(matrix,
                             XtParseTranslationTable(translation_table));
      clip = XtNameToWidget(matrix, "clip");
      if (clip)
         XtOverrideTranslations(clip,
                                XtParseTranslationTable(translation_table));

      /* install some callbacks */
      XtAddCallback(matrix, XmNenterCellCallback,
                 (XtCallbackProc)matrix_enter_cb, NULL);
      XtAddCallback(matrix, XmNselectCellCallback,
                 (XtCallbackProc)matrix_select_cb, NULL);
   } else {

      XbaeMatrixDeleteRows(matrix, 1, old_nrows-1);
      for (i = 0; i < ncols; i++)
         XbaeMatrixSetCell(matrix, 0, i, " ");

      XtVaSetValues(matrix, XmNrowLabels, rlab, NULL);

   }

   for (i = 0; i < ncols; i++)
      XtFree(labels[i]);
   XtFree(rlab[0]);

   XtFree((char *) labels);
   XtFree((char *) rlab);
   XtFree((char *) cw);
   XtFree((char *) ca);
}

/***********************************************************************
 *                                                                     *
 *   Add the a row to the matrix widget. This routine is called by     *
 *   fortran routine PASCA2.                                           *
 *                                                                     *
 ***********************************************************************/
#ifdef vms
void matrix_row(int *ievt, struct dsc$descriptor_s *row_dsc)
{
   char *row = row_dsc->dsc$a_pointer;
#else
void matrix_row(int *ievt, char *row)
{
#endif
   int      i;
   char   **rows, **evt, *s, lab[MAXLAB+1];
   static int cur_event;

   if (!matrix) return;

   /* when scan command finished it calls this routine with *ievt = -1 */
   if (*ievt == -1) {
      if (nrows == 0)
         km_inform_user(matrix, "No events satisfied the imposed cuts.", "");
      if (reverse_order) {
         s = XtCalloc(MAX_STRING, 1);
         sprintf(s, "%d", cur_event);
         XmTextSetString(firstScText, s);
         if (cur_event == 1)
            XtSetSensitive(previousScButton, False);
         XtFree(s);
      }
      return;
   }

   if (nrows >= max_rows) {
      *ievt = -1;
      return;
   }

   s  = row;

   rows = (char **) XtMalloc(ncols * sizeof(char *));
   evt  = (char **) XtMalloc(1 * sizeof(char *));

   for (i = 0; i < ncols; i++) {
      strncpy(lab, s, MAXLAB); lab[MAXLAB] = '\0';
      rows[i] = km_strip(lab);
      s      += MAXLAB;
   }

   cur_event = *ievt;
   evt[0] = XtMalloc(10);
   sprintf(evt[0], "%d", *ievt);

   if (!nrows) {

      for (i = 0; i < ncols; i++)
         XbaeMatrixSetCell(matrix, 0, i, rows[i]);

      XtVaSetValues(matrix, XmNrowLabels, evt, NULL);
      last_event = *ievt;

   } else {

      if (reverse_order)
         /* Scan the events in reverse order and add matrix element */
         /* from bottom to top */
         XbaeMatrixAddRows(matrix, 0, rows, evt, NULL, 1);
      else {
         /* Scan the events in normal order and add matrix element */
         /* from top to bottom */
         XbaeMatrixAddRows(matrix, nrows, rows, evt, NULL, 1);
         last_event = *ievt;
      }

   }

   nrows++;

   for (i = 0; i < ncols; i++)
      XtFree(rows[i]);
   XtFree(evt[0]);

   XtFree((char *) rows);
   XtFree((char *) evt);
}

/***********************************************************************
 *                                                                     *
 *   Matrix cells are not editable so tell the matrix that they may    *
 *   not be changed.                                                   *
 *                                                                     *
 ***********************************************************************/
static void matrix_enter_cb(Widget w, XtPointer cd,
                            XbaeMatrixEnterCellCallbackStruct *cbs)
{
   cbs->doit = False;
}

/***********************************************************************
 *                                                                     *
 *   When selecting a cell highlight the complete row.                 *
 *                                                                     *
 ***********************************************************************/
static void matrix_select_cb(Widget w, XtPointer cd,
                             XbaeMatrixSelectCellCallbackStruct *cbs)
{
   static int prev_row = 0;

   if (cbs->row != prev_row) {
      XbaeMatrixDeselectAll(w);
      XbaeMatrixSelectRow(w, cbs->row);

      prev_row = cbs->row;
   }
}

