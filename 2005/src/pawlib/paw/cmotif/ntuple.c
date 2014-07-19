/*
 * $Id: ntuple.c,v 1.3 1996/04/26 12:32:44 cremel Exp $
 *
 * $Log: ntuple.c,v $
 * Revision 1.3  1996/04/26 12:32:44  cremel
 * Do not clear the X, Y, Z fields in the ntuple viewer (Paw++) but
 * show the former variables instead.
 *
 * Revision 1.2  1996/03/19 12:51:14  cremel
 * BUG correction in Paw++ cut editor (happened for a long cut expression when
 * the number of cuts was > 11). The problem was that cut_expression had
 * dimension MAX_CMD_LENGTH (512) whereas local variable cut was MAX_STRING (80).
 *
 * Revision 1.1.1.1  1996/03/01 11:38:56  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.05/14 07/07/94  15.51.07  by  Fons Rademakers*/
/*-- Author :    Fons Rademakers   26/11/92*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <Xm/List.h>

#include "hmotif/pawm.h"

/* histo style panel icon */
#define ntuple_width 50
#define ntuple_height 50
static char ntuple_bits[] = {
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
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x18,
   0xe0, 0x01, 0x00, 0x00, 0x00, 0x00, 0x38, 0x60, 0x38, 0x00, 0x00, 0x00,
   0x00, 0x38, 0x60, 0x18, 0x00, 0x00, 0x00, 0x00, 0x78, 0x60, 0x18, 0x00,
   0x00, 0x00, 0x00, 0x78, 0x60, 0x18, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x20,
   0x18, 0x00, 0x00, 0x06, 0x00, 0xb8, 0x20, 0x18, 0x00, 0x00, 0x06, 0x00,
   0x98, 0x31, 0xf8, 0x00, 0x00, 0x06, 0x00, 0x9c, 0x31, 0xf8, 0x00, 0x38,
   0xe6, 0x00, 0x1c, 0x31, 0x18, 0xcc, 0x7c, 0xf6, 0x01, 0x1c, 0x33, 0x18,
   0xcc, 0xcc, 0x16, 0x01, 0x1c, 0x37, 0x18, 0xcc, 0x84, 0x96, 0x01, 0x0c,
   0x36, 0x18, 0xcc, 0xc4, 0xf6, 0x00, 0x0c, 0x3e, 0x18, 0xcc, 0x7c, 0x36,
   0x00, 0x0c, 0x3e, 0x18, 0xec, 0x3c, 0xf6, 0x00, 0x0e, 0x3e, 0xb8, 0xfd,
   0x0d, 0xe6, 0x01, 0x0e, 0x3c, 0xf8, 0x01, 0x0c, 0x04, 0x00, 0x0e, 0x38,
   0xf0, 0x00, 0x0c, 0x00, 0x00, 0x06, 0x08, 0x00, 0x00, 0x0c, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00};

/* extern functions */
extern Widget create_ntupleBrowser(void);
extern void show_cutEditor(Widget, XtPointer, XmAnyCallbackStruct *);
extern void show_columnSelector(Widget, XtPointer, XmAnyCallbackStruct *);
extern void unmanage_selector_dialog(void);
extern void clear_selector_columns(int);
extern void set_selector_column(XmString, int);
extern Widget find_button(char *, char *);

/* extern widgets */
extern Widget ntupleBrowser;
extern Widget closeNtButton;
extern Widget plotNtButton;
extern Widget scanNtButton;
extern Widget loopNtButton;
extern Widget projectNtButton;
extern Widget rebinNtButton;
extern Widget xNtToggle;
extern Widget yNtToggle;
extern Widget zNtToggle;
extern Widget xNtText;
extern Widget yNtText;
extern Widget zNtText;
extern Widget cutEditorNtButton;
extern Widget ignoreCutNtToggle;
extern Widget overlayNtToggle;
extern Widget extendedNtToggle;
extern Widget optionForm;        /* form containing boxes and profile toggle */
extern Widget boxesNtToggle;
extern Widget profileNtToggle;
extern Widget varNtList;
extern Widget titleNtLabel;
extern Widget columnsNtLabel;
extern Widget firstNtText;
extern Widget nrowNtText;
extern Widget histoNtText;

/* global data */
extern char  cut_expression[];
Widget       ntuplePanel  = (Widget) NULL;
NtupleStruct ntuple       = { 0 };
static int   old_id       = 0;

/* global functions */
void copy_variable(Widget, XtPointer, XButtonPressedEvent *);
void enter_ntuple_viewer(Widget, XtPointer, XEnterWindowEvent *);

/* local functions */
static void popdown_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void exec_ntuple_cmd(Widget, XtPointer, XmAnyCallbackStruct *);
static void update_toggle(Widget, XtPointer, XmTextVerifyCallbackStruct *);
static void handle_toggle(Widget, XtPointer, XmToggleButtonCallbackStruct *);
static void handle_extended(Widget, XtPointer, XmToggleButtonCallbackStruct *);
static void clear_ntuple_panel(void);

/***********************************************************************
 *                                                                     *
 *   Show (manage) the Ntuple viewer.                                  *
 *                                                                     *
 ***********************************************************************/
void show_ntupleBrowser(void)
{
   Pixmap        pixmap;
   Display      *display;

   if (!ntuplePanel) {
      ntuplePanel = create_ntupleBrowser();

      /*
       *   Set icon for Ntuple viewer popup
       */
      display = XtDisplay(ntuplePanel);
      pixmap = XCreateBitmapFromData(display, DefaultRootWindow(display),
                                     ntuple_bits, ntuple_width, ntuple_height);
      XtVaSetValues(XtParent(ntuplePanel), XmNtitle, "Ntuple Viewer",
                    XmNiconName, "Ntuple Viewer",
                    XmNiconPixmap, pixmap,
                    XmNdeleteResponse, XmUNMAP,
                    NULL);

      /* make ntuple panel known to KUIP's Exec window */
      km_store_appShell(ntuplePanel, "Ntuple Viewer");

      /*
       *   Make sure that if the window is popped down, all the
       *   local popups are unmanaged.
       */
      XtAddCallback(XtParent(ntuplePanel), XmNpopdownCallback,
                    (XtCallbackProc)popdown_cb, NULL);

      /*
       *   Install event handler on EnterNotify events
       */
      XtAddEventHandler(XtParent(ntuplePanel), EnterWindowMask, False,
                        (XtEventHandler)enter_ntuple_viewer, NULL);

      /*
       *   Install close callback
       */
      XtAddCallback(closeNtButton, XmNactivateCallback,
                    (XtCallbackProc)close_cb, ntuplePanel);

      /*
       *   Install callbacks on variable text widgets
       */
      XtAddCallback(xNtText, XmNmodifyVerifyCallback,
                    (XtCallbackProc)update_toggle, NULL);
      XtAddCallback(yNtText, XmNmodifyVerifyCallback,
                    (XtCallbackProc)update_toggle, NULL);
      XtAddCallback(zNtText, XmNmodifyVerifyCallback,
                    (XtCallbackProc)update_toggle, NULL);
      XtAddEventHandler(xNtText, ButtonPressMask, False,
                        (XtEventHandler)copy_variable, NULL);
      XtAddEventHandler(yNtText, ButtonPressMask, False,
                        (XtEventHandler)copy_variable, NULL);
      XtAddEventHandler(zNtText, ButtonPressMask, False,
                        (XtEventHandler)copy_variable, NULL);

      /*
       * Install callback on toggle buttons
       */
      XtAddCallback(xNtToggle, XmNvalueChangedCallback,
                    (XtCallbackProc)handle_toggle, NULL);
      XtAddCallback(yNtToggle, XmNvalueChangedCallback,
                    (XtCallbackProc)handle_toggle, NULL);
      XtAddCallback(zNtToggle, XmNvalueChangedCallback,
                    (XtCallbackProc)handle_toggle, NULL);

      XtAddCallback(extendedNtToggle, XmNvalueChangedCallback,
                    (XtCallbackProc)handle_extended, NULL);

      /*
       * Install default action callback on varNtList
       */
      XtAddCallback(varNtList, XmNdefaultActionCallback,
                    (XtCallbackProc)exec_ntuple_cmd, NULL);

      /*
       *   Install callback on buttons
       */
      XtAddCallback(cutEditorNtButton, XmNactivateCallback,
                    (XtCallbackProc)show_cutEditor, NULL);
      XtAddCallback(plotNtButton, XmNactivateCallback,
                    (XtCallbackProc)exec_ntuple_cmd, NULL);
      XtAddCallback(projectNtButton, XmNactivateCallback,
                    (XtCallbackProc)exec_ntuple_cmd, NULL);
      XtAddCallback(loopNtButton, XmNactivateCallback,
                    (XtCallbackProc)exec_ntuple_cmd, NULL);
      XtAddCallback(scanNtButton, XmNactivateCallback,
                    (XtCallbackProc)show_columnSelector, NULL);

      XtSetSensitive(optionForm, False);
      XtSetSensitive(rebinNtButton, False);
   }

   UxPopupInterface(ntuplePanel, XtGrabNone);

   /*
    * If the dialog was already open, UxPopupInterface does nothing.  In
    * this case, at least make sure the window is raised to the top
    * of the window tree (or as high as it can get).
    */
   XMapRaised(XtDisplay(ntuplePanel), XtWindow(XtParent(ntuplePanel)));
}

/***********************************************************************
 *                                                                     *
 *   Popdown callback. This callback is called when the ntuplePanel    *
 *   widget is popped down.                                            *
 *                                                                     *
 ***********************************************************************/
static void popdown_cb(Widget w, XtPointer call_data,
                       XmAnyCallbackStruct *cbs)
{
   unmanage_selector_dialog();
}

/***********************************************************************
 *                                                                     *
 *   In case the Ntuple does not exist anymore clear the panel.        *
 *                                                                     *
 ***********************************************************************/
static void update_panel(void)
{
   char         *title;
   int           nvars, nevt, bits, ntup;
   float         xmin, xmax, ymin, ymax;

   if (ntuple.id == 0) return;

   title = XtCalloc(MAX_STRING, 1);
   ntup  = 0;   /* do not fill the Ntuple variable list */
   Mhlattr(ntuple.dir, strlen(ntuple.dir), &ntuple.id, title, MAX_STRING,
         &nvars, &xmin, &xmax, &nevt, &ymin, &ymax, &bits, &ntup);

   if (nevt < 0)
      clear_ntuple_panel();

   XtFree(title);
}

/***********************************************************************
 *                                                                     *
 *   Handle the Ntuple viewer enter events.                            *
 *                                                                     *
 ***********************************************************************/
void enter_ntuple_viewer(Widget w, XtPointer cd, XEnterWindowEvent *ev)
{
   update_panel();
}

/***********************************************************************
 *                                                                     *
 *   Handle motion verify events in variable text widgets.             *
 *                                                                     *
 ***********************************************************************/
static void update_toggle(Widget w, XtPointer cd,
                          XmTextVerifyCallbackStruct *cbs)
{
   if (w == xNtText) {
      if (cbs->text->length) XmToggleButtonSetState(xNtToggle, True, True);
   }
   if (w == yNtText) {
      if (cbs->text->length) XmToggleButtonSetState(yNtToggle, True, True);
   }
   if (w == zNtText) {
      if (cbs->text->length) XmToggleButtonSetState(zNtToggle, True, True);
   }
}

/***********************************************************************
 *                                                                     *
 *   Handle variable toggle button changes.                            *
 *                                                                     *
 ***********************************************************************/
static void handle_toggle(Widget w, XtPointer cd,
                          XmToggleButtonCallbackStruct *cbs)
{
   int  dim = 0;

   if (XmToggleButtonGetState(xNtToggle)) dim++;
   if (XmToggleButtonGetState(yNtToggle)) dim++;
   if (XmToggleButtonGetState(zNtToggle)) dim++;

   if (dim == 2)
      XtSetSensitive(optionForm, True);
   else
      XtSetSensitive(optionForm, False);
}

/***********************************************************************
 *                                                                     *
 *   Handle extended toggle button changes.                            *
 *                                                                     *
 ***********************************************************************/
static void handle_extended(Widget w, XtPointer cd,
                            XmToggleButtonCallbackStruct *cbs)
{
   char  *title;
   int    nvars, nevt, bits, ntup;
   float  xmin, xmax, ymin, ymax;

   /* clear Ntuple variable list */
   XmListSetPos(varNtList, 1);
   XmListDeselectAllItems(varNtList);
   XmListDeleteAllItems(varNtList);
   if (!XtIsSensitive(scanNtButton))
     clear_selector_columns(1);

   /* Mhlattr will fill in the varNtList */
   title = XtCalloc(MAX_STRING, 1);
   ntup = ~0;   /* .TRUE., fill the Ntuple variable list */
   Mhlattr(ntuple.dir, strlen(ntuple.dir), &ntuple.id, title, MAX_STRING,
           &nvars, &xmin, &xmax, &nevt, &ymin, &ymax, &bits, &ntup);

   XtFree(title);
}

/***********************************************************************
 *                                                                     *
 *   Copy highlighted list item to variable text widget.               *
 *                                                                     *
 ***********************************************************************/
void copy_variable(Widget w, XtPointer cd, XButtonPressedEvent *event)
{
   XmString  *xvsel;
   char      *var, *varc, *s;
   int        vitem, vcnt;

   /* only handle 1st mouse button */
   if (event->button != 1)
      return;

   /*
    * if double-click occured then copy first highlighted variable list
    * item to variable text widget
    */
   if (km_double_click(event->time)) {
      /* get selected item in the file list */
      XtVaGetValues(varNtList, XmNitemCount, &vitem,
                    XmNselectedItemCount,    &vcnt,
                    XmNselectedItems,        &xvsel, NULL);

      if (vcnt) {
         var = extract_normal_string(xvsel[0]);
         varc = compress(var);
         if ((s = strchr(varc, '(')) || (s = strchr(varc, '[')) ||
             (s = strchr(varc, ':')))
            *s = '\0';
         XmTextSetString(w, NULL);
         XmTextSetString(w, varc);
         XtFree(var);
         XtFree(varc);
      }
   }
}

/***********************************************************************
 *                                                                     *
 *   Execute an Ntuple command (like plot, scan ,etc.).                *
 *                                                                     *
 ***********************************************************************/
static void exec_ntuple_cmd(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   char   *cmd, *xyz, *xyzc, *x, *y, *z, *cut;
   char   *hist, *first, *nrows, *same, *option;
   int     ihist, ifirst, nevent, nvars, nupd;

   if (ntuple.id == 0) return;

   x = y = z = NULL;

   cmd    = XtCalloc(MAX_CMD_LENGTH, 1);
   xyz    = XtCalloc(MAX_STRING, 1);
   /* cut    = XtCalloc(MAX_STRING, 1); */
   option = XtCalloc(MAX_STRING, 1);
   nvars  = 0;

   strcat(xyz, ".");

   if (w == varNtList) {

      char  *s;
      XmListCallbackStruct *cbsl = (XmListCallbackStruct *)cbs;

      x = extract_normal_string(cbsl->item);
      if ((s = strchr(x, '(')) || (s = strchr(x, '[')) ||
          (s = strchr(x, ':')))
         *s = '\0';
      strcat(xyz, x);
      nvars++;

   } else {

      if (XmToggleButtonGetState(zNtToggle)) {
         z = XmTextGetString(zNtText);
         if (!strempty(z)) {
            strcat(xyz, z);
            nvars++;
         }
      }
      if (XmToggleButtonGetState(yNtToggle)) {
         y = XmTextGetString(yNtText);
         if (!strempty(y)) {
            if (strlen(xyz) > 1) strcat(xyz, "%");
            strcat(xyz, y);
            nvars++;
         }
      }
      if (XmToggleButtonGetState(xNtToggle)) {
         x = XmTextGetString(xNtText);
         if (!strempty(x)) {
            if (strlen(xyz) > 1) strcat(xyz, "%");
            strcat(xyz, x);
            nvars++;
         }
      }
   }

   if (strlen(xyz) == 1) *xyz = '\0';
   xyzc = compress(xyz);

   /* get the cuts */
   if (strempty(cut_expression) || XmToggleButtonGetState(ignoreCutNtToggle))
      /* strcpy(cut, "!"); */
      cut = strdup ("!");
   else
      /* strcpy(cut, cut_expression); */
      cut = strdup (cut_expression);

   /* handle overlay mode */
   if (appres.same_zone || XmToggleButtonGetState(overlayNtToggle))
      same = "S";
   else
      same = " ";

   /* histogram ID, first event and number of events to process */
   first  = XmTextGetString(firstNtText);
   nrows  = XmTextGetString(nrowNtText);
   hist   = XmTextGetString(histoNtText);
   ifirst = atoi(first);
   nevent = atoi(nrows);
   ihist  = atoi(hist);

   if (ifirst < 1) {
      ifirst = 1;
      XmTextSetString(firstNtText, "1");
   }
   if (ifirst > ntuple.nrows) {
      char s[32];
      ifirst = ntuple.nrows;
      sprintf(s, "%d", ifirst);
      XmTextSetString(firstNtText, s);
   }

   if (nevent < 1) {
      nevent = 1;
      XmTextSetString(nrowNtText, "1");
   }
   if (nevent > ntuple.nrows) {
      char s[32];
      nevent = ntuple.nrows;
      sprintf(s, "%d", nevent);
      XmTextSetString(nrowNtText, s);
   }

   if (ihist <= 0) {
      ihist = 1000000;
      XmTextSetString(histoNtText, "1000000");
   }

   nupd   = nevent + 1;

   /* get the plotting option according to the number of variables */
   switch (nvars) {
      case 1:
         histogram.id   = ihist;
         histogram.type = HISTO_1D;
         strcpy(histogram.dir, "//PAWC");
         strcpy(option, histogram.option_1d);
         break;

      case 2:
         histogram.id   = ihist;
         histogram.type = HISTO_2D;
         strcpy(histogram.dir, "//PAWC");
         if (XmToggleButtonGetState(boxesNtToggle))
            strcpy(option, "BOX ");
         else if (XmToggleButtonGetState(profileNtToggle)) {
            strcpy(option, "PROF ");
            histogram.type = HISTO_1D;
         } else
            strcpy(option, histogram.option_2d);
         break;

      case 3:
         strcpy(option, " ");
         histogram.type = HISTO_3D;
         break;

      default:
         histogram.id   = ihist;
         histogram.type = HISTO_1D;
         strcpy(histogram.dir, "//PAWC");
         strcpy(option, histogram.option_1d);
   }

   set_environment();

   /* take care of same zone as in plot_current_histo */
   if (w == plotNtButton || w == varNtList) {
      sprintf(cmd, "Ntuple/Plot %s/%d%s %s %d %d %d '%s%s%s' %d",
              ntuple.dir, ntuple.id, xyzc, cut, nevent, ifirst,
              nupd, option, histogram.coordinate, same, ihist);
   } else if (w == projectNtButton) {
      sprintf(cmd, "Ntuple/Project %d %s/%d%s %s %d %d",
              ihist, ntuple.dir, ntuple.id, xyzc, cut, nevent, ifirst);
   } else if (w == loopNtButton) {
      sprintf(cmd, "Ntuple/Loop %s/%d %s %d %d",
              ntuple.dir, ntuple.id, cut, nevent, ifirst);
   }

   strcpy(ntuple.xyz, xyzc);
   strcpy(ntuple.cut, cut);
   ntuple.nvars  = nvars;
   ntuple.first  = ifirst;
   ntuple.nevent = nevent;

   km_all_cursor(KM_WAIT_CURSOR);

   execute_kuip_cmd(cmd);

   if (w != loopNtButton) set_histo_id(True);

   km_all_cursor(KM_MAIN_CURSOR);

   XtFree(cmd);
   XtFree(xyz);
   XtFree(xyzc);
   /* XtFree(cut); */
   free( cut );
   XtFree(x);
   XtFree(y);
   XtFree(z);
   XtFree(hist);
   XtFree(first);
   XtFree(nrows);
   XtFree(option);
}

/***********************************************************************
 *                                                                     *
 *   Add a variable in the Ntuple list.                                *
 *                                                                     *
 ***********************************************************************/
#ifdef vms
void list_add_ntuple_var(struct dsc$descriptor_s *item_dsc, int *litem,
                         int *pos)
{
   char *item = item_dsc->dsc$a_pointer;
#else
void list_add_ntuple_var(char *item, int *litem, int *pos)
{
#endif
   char      *str, *s;
   XmString   xmstr;

   if (!topWidget)   return;
   if (!ntuplePanel) return;
   if (*litem <= 0)  return;

   str = XtCalloc(MAX_STRING, 1);
   strncpy(str, item, *litem);
   if (!XmToggleButtonGetState(extendedNtToggle)) {
      if ((s = strchr(str, '(')) || (s = strchr(str, '[')) ||
          (s = strchr(str, ':')))
         *s = '\0';
   }
   xmstr = XmStringCreate(str, XmSTRING_DEFAULT_CHARSET);

   XmListAddItem(varNtList, xmstr, *pos);

   if (!XtIsSensitive(scanNtButton))
     set_selector_column(xmstr, *pos);

   XmStringFree(xmstr);
   XtFree(str);
}

/***********************************************************************
 *                                                                     *
 *   Fill in the Ntuple viewer fields.                                 *
 *                                                                     *
 ***********************************************************************/
void set_ntuple_id()
{
   char         *title, *tit, *str;
   int           ltit, nvars, nevt, bits, ntup;
   float         xmin, xmax, ymin, ymax;
   XmString      xmtit;
   static char   old_dir[MAX_STRING] = "\0";

   /* popup Ntuple viewer when Ntuple has been selected */
   show_ntupleBrowser();

   /* check if not the same Ntuple */
   title = XtCalloc(MAX_STRING, 1);
   ntup  = 0;   /* do not fill the Ntuple variable list */
   Mhlattr(ntuple.dir, strlen(ntuple.dir), &ntuple.id, title, MAX_STRING,
         &nvars, &xmin, &xmax, &nevt, &ymin, &ymax, &bits, &ntup);

   if (ntuple.id == old_id && !strcmp(ntuple.dir, old_dir) &&
       ntuple.ncols == nvars && ntuple.nrows == nevt) {
      XtFree(title);
      return;
   }

   old_id = ntuple.id;
   strcpy(old_dir, ntuple.dir);

   /* clear Ntuple variable list */
   XmListSetPos(varNtList, 1);
   XmListDeselectAllItems(varNtList);
   XmListDeleteAllItems(varNtList);
   if (!XtIsSensitive(scanNtButton))
     clear_selector_columns(0);

   /* reset variable text and toggle buttons */
   /* NO! keep last value (otherwise one has to re-type them every time 
   xmTextSetString(xNtText, "");
   XmTextSetString(yNtText, "");
   XmTextSetString(zNtText, "");
   XmToggleButtonSetState(xNtToggle, False, True);
   XmToggleButtonSetState(yNtToggle, False, True);
   XmToggleButtonSetState(zNtToggle, False, True);
   ... end NO! ... */

   ntup = ~0;   /* .TRUE., fill the Ntuple variable list */
   Mhlattr(ntuple.dir, strlen(ntuple.dir), &ntuple.id, title, MAX_STRING,
           &nvars, &xmin, &xmax, &nevt, &ymin, &ymax, &bits, &ntup);
   for (ltit = MAX_STRING; ltit > 0 && title[ltit-1] == ' '; )
     ltit--;

   if (nvars < 0) {
      ntuple.chain = True;
      nvars = -nvars;
   } else
      ntuple.chain = False;

   ntuple.bits  = bits;
   ntuple.ncols = nvars;
   ntuple.nrows = nevt;
   strcpy(ntuple.xyz, " ");
   strcpy(ntuple.cut, " ");
   ntuple.nvars  = 0;
   ntuple.first  = 0;
   ntuple.nevent = 0;

   /* fill in title and nvars labels */
   tit = XtCalloc(ltit+1, 1);
   str = XtCalloc(MAX_CMD_LENGTH, 1);
   strncpy(tit, title, ltit);
   if (ntuple.chain)
      sprintf(str, "%s/%d:  %s   (Chain)", ntuple.dir, ntuple.id, tit);
   else
      sprintf(str, "%s/%d:  %s", ntuple.dir, ntuple.id, tit);
   xmtit = XmStringCreate(str, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(titleNtLabel, XmNlabelString, xmtit, NULL);
   XmStringFree(xmtit);

   sprintf(title, "%d R  %d C", nevt, nvars);
   xmtit = XmStringCreate(title, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(columnsNtLabel, XmNlabelString, xmtit, NULL);
   XmStringFree(xmtit);

   /* set first and last event in text widget */
   XmTextSetString(firstNtText, "1");
   sprintf(title, "%d", nevt);
   XmTextSetString(nrowNtText, title);
   XmTextSetString(histoNtText, "1000000");

   /* clear the cut editor */


   XtFree(title);
   XtFree(tit);
   XtFree(str);
}

/***********************************************************************
 *                                                                     *
 *   Clear the Ntuple viewer fields.                                   *
 *                                                                     *
 ***********************************************************************/
static void clear_ntuple_panel(void)
{
   char       *str;
   XmString    xmtit;

   if (!ntuplePanel || ntuple.id == 0) return;

   ntuple.id    = 0;
   ntuple.chain = False;
   old_id       = 0;

   /* clear Ntuple variable list */
   XmListSetPos(varNtList, 1);
   XmListDeselectAllItems(varNtList);
   XmListDeleteAllItems(varNtList);
   if (!XtIsSensitive(scanNtButton))
     clear_selector_columns(0);

   /* reset variable text and toggle buttons */
   XmTextSetString(xNtText, "");
   XmTextSetString(yNtText, "");
   XmTextSetString(zNtText, "");
   XmToggleButtonSetState(xNtToggle, False, True);
   XmToggleButtonSetState(yNtToggle, False, True);
   XmToggleButtonSetState(zNtToggle, False, True);

   /* fill in title and nvars labels */
   str = XtCalloc(MAX_CMD_LENGTH, 1);
   sprintf(str, "No Ntuple Loaded");
   xmtit = XmStringCreate(str, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(titleNtLabel, XmNlabelString, xmtit, NULL);
   XmStringFree(xmtit);

   sprintf(str, " ");
   xmtit = XmStringCreate(str, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(columnsNtLabel, XmNlabelString, xmtit, NULL);
   XmStringFree(xmtit);

   /* set first and last event in text widget */
   XmTextSetString(firstNtText, "");
   XmTextSetString(nrowNtText,  "");
   XmTextSetString(histoNtText, "1000000");

   /* clear the cut editor */


   XtFree(str);
}
