/*
 * $Id: cuts.c,v 1.3 1996/05/03 13:01:02 cremel Exp $
 *
 * $Log: cuts.c,v $
 * Revision 1.3  1996/05/03 13:01:02  cremel
 * cuts editor: add operator '*' + bug correction (get_cut_context).
 *
 * Revision 1.2  1996/03/19 12:51:12  cremel
 * BUG correction in Paw++ cut editor (happened for a long cut expression when
 * the number of cuts was > 11). The problem was that cut_expression had
 * dimension MAX_CMD_LENGTH (512) whereas local variable cut was MAX_STRING (80).
 *
 * Revision 1.1.1.1  1996/03/01 11:38:54  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.06/06 13/12/94  19.42.28  by  Fons Rademakers*/
/*-- Author :*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <Xm/SelectioB.h>
#include <Xm/FileSB.h>

#include "hmotif/pawm.h"
#include "hmotif/cuts.h"

/* cut editor icon */
#define cuts_width 50
#define cuts_height 50
static char cuts_bits[] = {
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
   0x00, 0x00, 0x00, 0x00, 0x00, 0x1c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x67, 0x00, 0xc0, 0x01, 0x1e,
   0x00, 0xc0, 0x07, 0x00, 0xc0, 0x00, 0x3f, 0x00, 0xc0, 0x01, 0x18, 0xce,
   0x00, 0x73, 0x00, 0xe0, 0x00, 0x18, 0xc6, 0x07, 0x21, 0x00, 0xe0, 0x00,
   0x18, 0xc7, 0x07, 0x03, 0x00, 0xe0, 0x00, 0x18, 0xc3, 0x00, 0x0f, 0x00,
   0xe0, 0x00, 0x38, 0xc3, 0x00, 0x1c, 0x00, 0xe0, 0x00, 0x38, 0xc3, 0x00,
   0x38, 0x00, 0xc0, 0x80, 0x38, 0xc3, 0x00, 0x30, 0x00, 0xc0, 0xc1, 0x38,
   0xc3, 0x00, 0x30, 0x00, 0x80, 0x67, 0xf0, 0x87, 0x31, 0x31, 0x00, 0x00,
   0x7f, 0xf0, 0x8d, 0x9f, 0x39, 0x00, 0x00, 0x3e, 0x00, 0x00, 0x0f, 0x1d,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00};

#define OUT(a) (strempty(a) ? "ND" : a)

/* extern functions */
extern Widget create_cutEditor(void);
extern Widget create_cutExpr(Widget);
extern Widget create_indentCeDialog(Widget);
extern Widget create_openCeDialog(Widget);
extern Widget create_saveAsCeDialog(Widget);
extern Widget create_dynamicCeDialog(Widget);
extern void copy_variable(Widget, XtPointer, XButtonPressedEvent *);

/* extern widgets */
extern Widget cutEditorNtButton;
extern Widget plotNtButton;

/* global data */
CutEditorStruct   cut_editor[MAX_CUT_EDITORS];
char              cut_expression[MAX_CMD_LENGTH] = "";

/* global functions */
void show_cutEditor(Widget, XtPointer, XmAnyCallbackStruct *);

/* local functions */
static int get_ce_context(Widget);
static int get_cut_context(Widget);
static void popdown_cb(Widget, CutEditorStruct *, XmAnyCallbackStruct *);
static void show_openDialog(Widget, CutEditorStruct *, XmAnyCallbackStruct *);
static void show_saveAsDialog(Widget, CutEditorStruct *, XmAnyCallbackStruct *);
static void save_cuts(Widget, CutEditorStruct *, XmAnyCallbackStruct *);
static void save_cuts_file(CutEditorStruct *, char *);
static void read_cuts_file(CutEditorStruct *, char *);
static void make_weight(Cut *, Boolean);
static Cut *add_cut(CutEditorStruct *, enum cut_t, int);
static void delete_cut(CutEditorStruct *, int);
static void post_menu(Widget, Cut *, XButtonPressedEvent *);
static void select_cut(Cut *);
static void add_cut_expression(Widget, CutEditorStruct *,
                               XmAnyCallbackStruct *);
static void delete_cut_expression(Widget, CutEditorStruct *,
                                  XmAnyCallbackStruct *);
static int get_selected_cut_pos(CutEditorStruct *);
static Cut *get_cut_at_pos(CutEditorStruct *, int);
static Cut *get_selected_cut(CutEditorStruct *);
static int brackets(Cut *, enum cut_t, char **);
static Boolean balanced_brackets(CutEditorStruct *);
static void indent_cuts(CutEditorStruct *);
static void show_logical_menu(CutEditorStruct *);
static void set_cut_expression(CutEditorStruct *);
static void apply_cb(Widget, CutEditorStruct *, XmAnyCallbackStruct *);
static void activate(Widget, CutEditorStruct *, XmAnyCallbackStruct *);
static void activate_expression(Widget, Cut *, XmTextVerifyCallbackStruct *);
static void activate_toggle_cb(Widget, Cut *, XmToggleButtonCallbackStruct *);
static void operator_cb(Widget, Cut *, XmAnyCallbackStruct *);
static void toggle_not_button(Widget,  Cut *, XmAnyCallbackStruct *);
static void show_indentDialog(Widget, CutEditorStruct *,
                              XmAnyCallbackStruct *);
static void show_dynamicDialog(Widget, CutEditorStruct *,
                               XmAnyCallbackStruct *);
static void scale_dynamic(Widget, Cut *, XmScaleCallbackStruct *);
static void set_scale_range(Widget, Cut *, XmAnyCallbackStruct *);
static void apply_dynamic(Widget, Cut *, XmAnyCallbackStruct *);
static void reset_dynamic(Widget, Cut *, XmAnyCallbackStruct *);
static void toggle_dynamic(Widget, Cut *, XmToggleButtonCallbackStruct *);

/* local data */
static int n_cut_editors   = 0;


/***********************************************************************
 *                                                                     *
 *   Show (manage) the cut editor.                                     *
 *                                                                     *
 ***********************************************************************/
void show_cutEditor(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   Widget                widget;
   Pixmap                pixmap;
   Display              *display;

   if (!cut_editor[0].widget) {
      widget  = create_cutEditor();

      (void)get_ce_context(widget);

      /*
       *   Set icon for cut editor popup
       */
      display = XtDisplay(widget);
      pixmap = XCreateBitmapFromData(display, DefaultRootWindow(display),
                                     cuts_bits, cuts_width, cuts_height);
      XtVaSetValues(XtParent(widget), XmNtitle, "Cut Editor",
                    XmNiconName, "Cut Editor",
                    XmNiconPixmap, pixmap,
                    XmNdeleteResponse, XmUNMAP,
                    NULL);

      /* make cut editor known to KUIP's Exec window */
      km_store_appShell(widget, "Cut Editor");

      /*
       *   Make sure that if the window is popped down, the cut editor button
       *   on the ntuple viewer is made sensitive again and that all
       *   local popups are unmanaged.
       */
      XtAddCallback(XtParent(widget), XmNpopdownCallback,
                    (XtCallbackProc)popdown_cb, &cut_editor[0]);

      /*
       *  Install Apply callbacks
       */
      XtAddCallback(applyCeButton, XmNactivateCallback,
                    (XtCallbackProc)apply_cb, &cut_editor[0]);
      XtAddCallback(applyPlotCeButton, XmNactivateCallback,
                    (XtCallbackProc)apply_cb, &cut_editor[0]);

      /*
       *  Install callbacks on file menu items
       */
      XtAddCallback(openCeButton, XmNactivateCallback,
                    (XtCallbackProc)show_openDialog, &cut_editor[0]);
      XtAddCallback(saveCeButton, XmNactivateCallback,
                    (XtCallbackProc)save_cuts, &cut_editor[0]);
      XtAddCallback(saveasCeButton, XmNactivateCallback,
                    (XtCallbackProc)show_saveAsDialog, &cut_editor[0]);
      XtAddCallback(closeCeButton, XmNactivateCallback,
                    (XtCallbackProc)close_cb, widget);
      XtAddCallback(close2CeButton, XmNactivateCallback,
                    (XtCallbackProc)close_cb, widget);


      /*
       *  Install callbacks on edit menu items
       */
      XtAddCallback(addBeforeCeButton, XmNactivateCallback,
                    (XtCallbackProc)add_cut_expression, &cut_editor[0]);
      XtAddCallback(addAfterCeButton, XmNactivateCallback,
                    (XtCallbackProc)add_cut_expression, &cut_editor[0]);
      XtAddCallback(addBeforeOpenCeButton, XmNactivateCallback,
                    (XtCallbackProc)add_cut_expression, &cut_editor[0]);
      XtAddCallback(addAfterOpenCeButton, XmNactivateCallback,
                    (XtCallbackProc)add_cut_expression, &cut_editor[0]);
      XtAddCallback(addBeforeCloseCeButton, XmNactivateCallback,
                    (XtCallbackProc)add_cut_expression, &cut_editor[0]);
      XtAddCallback(addAfterCloseCeButton, XmNactivateCallback,
                    (XtCallbackProc)add_cut_expression, &cut_editor[0]);
      XtAddCallback(deleteCeButton, XmNactivateCallback,
                    (XtCallbackProc)delete_cut_expression, &cut_editor[0]);
      XtAddCallback(deleteAllCeButton, XmNactivateCallback,
                    (XtCallbackProc)delete_cut_expression, &cut_editor[0]);

      /*
       * Install callbacks on option menu items
       */
      XtSetSensitive(treeCeButton, False);
      XtAddCallback(activateCeButton, XmNactivateCallback,
                    (XtCallbackProc)activate, &cut_editor[0]);
      XtAddCallback(deactivateCeButton, XmNactivateCallback,
                    (XtCallbackProc)activate, &cut_editor[0]);
      XtAddCallback(indentCeButton, XmNactivateCallback,
                    (XtCallbackProc)show_indentDialog, &cut_editor[0]);
      XtAddCallback(dynCeButton, XmNactivateCallback,
                    (XtCallbackProc)show_dynamicDialog, &cut_editor[0]);


      n_cut_editors++;

      cut_editor[0].widget = widget;
      cut_editor[0].indentation = INDENTATION;
      (void)add_cut(&cut_editor[0], CUT, 0);
   }

   /* display the cut editor */
   UxPopupInterface(cut_editor[0].widget, XtGrabNone);

   /*
    * If the dialog was already open, UxPopupInterface does nothing.  In
    * this case, at least make sure the window is raised to the top
    * of the window tree (or as high as it can get).
    */
   XMapRaised(XtDisplay(cut_editor[0].widget),
              XtWindow(XtParent(cut_editor[0].widget)));
}

/***********************************************************************
 *                                                                     *
 *   Popdown callback. This callback is called when the cut_editor     *
 *   widget is popped down.                                            *
 *                                                                     *
 ***********************************************************************/
static void popdown_cb(Widget w, CutEditorStruct *editor,
                       XmAnyCallbackStruct *call_data)
{
   Cut           *cut;
   MenuCbStruct  *item;

   if (editor->indentDialog) {
      XtVaGetValues(editor->indentDialog, XmNuserData, &item, NULL);
      popdown_argument_dialog(item);
   }
   if (editor->openDialog) {
      XtVaGetValues(editor->openDialog, XmNuserData, &item, NULL);
      popdown_argument_dialog(item);
   }
   if (editor->saveAsDialog) {
      XtVaGetValues(editor->saveAsDialog, XmNuserData, &item, NULL);
      popdown_argument_dialog(item);
   }

   cut = editor->cuts;
   while (cut) {
      if (cut->dynamicDialog)
         XtUnmanageChild(cut->dynamicDialog);
      cut = cut->link;
   }
}

/***********************************************************************
 *                                                                     *
 *   Get the context for the cut editor.                               *
 *                                                                     *
 ***********************************************************************/
static int get_ce_context(Widget w)
{
   if (!w) return -1;

   /*
    *   find and restore the context
    *   (to get to the widgets belonging to this dialog)
    */
   UxCutEditorContext = (_UxCcutEditor *)UxGetContext(w);

   return 1;
}

/***********************************************************************
 *                                                                     *
 *   Get the context for the cut editor.                               *
 *                                                                     *
 ***********************************************************************/
static int get_cut_context(Widget w)
{
   if (!w) return -1;

   /*
    *   find and restore the context
    *   (to get to the widgets belonging to this dialog)
    */
   UxCutExprContext = (_UxCcutExpr *)UxGetContext(w);

   return 1;
}

#if 0
/***********************************************************************
 *                                                                     *
 *   Iconify/De-iconfy cut editor dialog.                              *
 *                                                                     *
 ***********************************************************************/
void iconify_cutEditor(Boolean map)
{
   int i;

   for (i = 0; i < MAX_CUT_EDITORS; i++) {
      if (cut_editor[i].widget)
         if (map) {
            if (IsShellIconic(XtParent(cut_editor[i].widget)))
               DeIconifyShell(XtParent(cut_editor[i].widget));
         } else
            IconifyShell(XtParent(cut_editor[i].widget));
   }
}
#endif

/***********************************************************************
 *                                                                     *
 *   Set apply buttons sensitive or insensitive.                       *
 *   Only apply buttons that cause a time consuming oparation (like    *
 *   redrawing), i.e. the apply button in the dynamic cut windows.     *
 *                                                                     *
 ***********************************************************************/
void cut_apply_sensitivity(Boolean set)
{
   Cut  *cut;
   int   i;

   for (i = 0; i < MAX_CUT_EDITORS; i++) {

      if (cut_editor[i].widget) {

         /* set cut editor context */
         (void)get_ce_context(cut_editor[i].widget);

         cut = cut_editor[i].cuts;
         while (cut) {
            (void)get_cut_context(cut->widget);

            if (cut->dynamicDialog) {
               /* set context of dynamicDialog */
               UxDynamicCeDialogContext =
                  (_UxCdynamicCeDialog *)UxGetContext(cut->dynamicDialog);

               XtSetSensitive(applyDyButton, set);
            }
            cut = cut->link;
         }
      }
   }
}

/***********************************************************************
 *                                                                     *
 *   Read a cuts file and store the cuts in the cut structure.         *
 *                                                                     *
 ***********************************************************************/
static void read_cuts_file(CutEditorStruct *editor, char *file)
{
   FILE       *fcuts;
   Cut        *cut;
   int         act, i;
   enum cut_t  type;
   char       *not, *lval, *op, *rval, *lop, *s;
   XmString    xmstr;
   XmAnyCallbackStruct   cbs1;

   if ((fcuts = fopen(file, "r")) != NULL) {

      not  = XtCalloc(MAX_STRING, 1);
      lval = XtCalloc(MAX_STRING, 1);
      op   = XtCalloc(MAX_STRING, 1);
      rval = XtCalloc(MAX_STRING, 1);
      lop  = XtCalloc(MAX_STRING, 1);

      /* unmanage cut editor form that contains all cut expressions */
      XtUnmanageChild(cutsCeForm);

      /* first delete all exiting cuts */
      delete_cut(editor, 0);

      while ((i = getc(fcuts)) != EOF) {

         type = (enum cut_t) ((char)i - '0');

         switch (type) {

            case CUT:

               fscanf(fcuts, "%s %s %s %s %s %d\n",
                      not, lval, op, rval, lop, &act);

               cut = add_cut(editor, CUT, 0);

               if (strcmp(not, "ND"))
                  strcpy(cut->not, not);
               if (strcmp(lval, "ND"))
                  strcpy(cut->lvalue, lval);
               if (strcmp(op, "ND"))
                  strcpy(cut->operator, op);
               if (strcmp(rval, "ND"))
                  strcpy(cut->rvalue, rval);
               if (strcmp(lop, "ND"))
                  strcpy(cut->loperator, lop);
               cut->active = (Boolean)act;

               /* set the cut expression widgets accordingly */
               (void)get_cut_context(cut->widget);

               if (cut->active)
                  XmToggleButtonSetState(activeCeToggle, True, False);
               else
                  XmToggleButtonSetState(activeCeToggle, False, False);

               if (!strcmp(cut->not, ".NOT.")) {
                  xmstr = XmStringCreateSimple("!");
                  XtVaSetValues(notCeButton, XmNlabelString, xmstr, NULL);
                  XmStringFree(xmstr);
                  TogglePushButton(notCeButton, True);
               }

               if (!strempty(cut->lvalue))
                  XmTextSetString(expr1CeText, cut->lvalue);

               if (!strcmp(cut->operator, "="))
                  XtVaSetValues(operatorMenu, XmNmenuHistory, operatorEQ, NULL);
               else if (!strcmp(cut->operator, "<>"))
                  XtVaSetValues(operatorMenu, XmNmenuHistory, operatorNE, NULL);
               else if (!strcmp(cut->operator, ">"))
                  XtVaSetValues(operatorMenu, XmNmenuHistory, operatorGT, NULL);
               else if (!strcmp(cut->operator, "<"))
                  XtVaSetValues(operatorMenu, XmNmenuHistory, operatorLT, NULL);
               else if (!strcmp(cut->operator, ">="))
                  XtVaSetValues(operatorMenu, XmNmenuHistory, operatorGE, NULL);
               else if (!strcmp(cut->operator, "<="))
                  XtVaSetValues(operatorMenu, XmNmenuHistory, operatorLE, NULL);
               else if (!strcmp(cut->operator, "#"))
                  XtVaSetValues(operatorMenu, XmNmenuHistory, operatorCT, NULL);

               if (!strempty(cut->rvalue))
                  XmTextSetString(expr2CeText, cut->rvalue);

               if (!strcmp(cut->loperator, ".AND."))
                  XtVaSetValues(logicalMenu, XmNmenuHistory, logicalAND, NULL);
               else if (!strcmp(cut->loperator, ".OR."))
                  XtVaSetValues(logicalMenu, XmNmenuHistory, logicalOR, NULL);
               else if (!strcmp(cut->loperator, "*"))
                  XtVaSetValues(logicalMenu, XmNmenuHistory, logicalMULT, NULL);

               break;

            case WEIGHT:

               fscanf(fcuts, "%s %s %s %d\n", lval, op, lop, &act);

               cut = add_cut(editor, CUT, 0);

               if (strcmp(lval, "ND"))
                  strcpy(cut->lvalue, lval);
               if (strcmp(op, "ND"))
                  strcpy(cut->operator, op);
               if (strcmp(lop, "ND"))
                  strcpy(cut->loperator, lop);
               cut->active = (Boolean)act;

               /* set the cut expression widgets accordingly */
               (void)get_cut_context(cut->widget);

               if (cut->active)
                  XmToggleButtonSetState(activeCeToggle, True, False);
               else
                  XmToggleButtonSetState(activeCeToggle, False, False);

               if (!strempty(cut->lvalue))
                  XmTextSetString(expr1CeText, cut->lvalue);

               if (!strcmp(cut->operator, "W")) {
                  XtVaSetValues(operatorMenu, XmNmenuHistory, operatorW, NULL);
                  make_weight(cut, True);
               }

               if (!strcmp(cut->loperator, ".AND."))
                  XtVaSetValues(logicalMenu, XmNmenuHistory, logicalAND, NULL);
               else if (!strcmp(cut->loperator, ".OR."))
                  XtVaSetValues(logicalMenu, XmNmenuHistory, logicalOR, NULL);
               else if (!strcmp(cut->loperator, "*"))
                  XtVaSetValues(logicalMenu, XmNmenuHistory, logicalMULT, NULL);

               break;

            case OPEN_BRACKET:

               fscanf(fcuts, "%s %s\n", not, lval);

               for (i = 0; i < strlen(lval); i++)
                  cut = add_cut(editor, OPEN_BRACKET, 0);

               if (strcmp(not, "ND"))
                  strcpy(cut->not, not);

               (void)get_cut_context(cut->widget);

               if (!strcmp(cut->not, ".NOT.")) {
                  xmstr = XmStringCreateSimple("!");
                  XtVaSetValues(notCeButton, XmNlabelString, xmstr, NULL);
                  XmStringFree(xmstr);
                  TogglePushButton(notCeButton, True);
               }

               break;

            case CLOSE_BRACKET:

               fscanf(fcuts, "%s %s\n", lval, lop);

               for (i = 0; i < strlen(lval); i++)
                  cut = add_cut(editor, CLOSE_BRACKET, 0);

               if (strcmp(lop, "ND"))
                  strcpy(cut->loperator, lop);

               (void)get_cut_context(cut->widget);

               if (!strcmp(cut->loperator, ".AND."))
                  XtVaSetValues(logicalMenu, XmNmenuHistory, logicalAND, NULL);
               else if (!strcmp(cut->loperator, ".OR."))
                  XtVaSetValues(logicalMenu, XmNmenuHistory, logicalOR, NULL);
               else if (!strcmp(cut->loperator, "*"))
                  XtVaSetValues(logicalMenu, XmNmenuHistory, logicalMULT, NULL);

               break;
         }
      }

      XtFree(lval);
      XtFree(op);
      XtFree(rval);
      XtFree(lop);

      fclose(fcuts);

      indent_cuts(editor);

      /* manage again cut editor form that contains all cut expressions */
      XtManageChild(cutsCeForm);

      /* set cut editor title to the saved file */
      strcpy(editor->file, file);
      strcpy(editor->title, "Cut Editor: ");
#ifdef vms
      s = strrchr(file, ']');
      if (!s)
         s = strrchr(file, ':');
#else
      s = strrchr(file, '/');
#endif
      if (s)
         strcat(editor->title, s+1);
      else
         strcat(editor->title, file);
      XtVaSetValues(XtParent(editor->widget), XmNtitle, editor->title,
                    XmNiconName, editor->title,
                    NULL);

      cbs1.reason = 0;
      cbs1.event  = (XEvent *) NULL;
      XtCallCallbacks(applyCeButton, XmNactivateCallback, &cbs1);

   } else {
     km_inform_user(editor->widget, "Can't read cuts file.", "");
     return;
   }
}

/***********************************************************************
 *                                                                     *
 *   Save a cuts file.                                                 *
 *                                                                     *
 ***********************************************************************/
static void save_cuts_file(CutEditorStruct *editor, char *file)
{
   FILE  *fcuts;
   Cut   *cut;
   char  *s;

   if (strempty(cut_expression)) {
      km_inform_user(editor->widget, "No cut expression defined.\n\
Define some cuts and/or press the Apply button.", "");
      return;
   }

   if ((fcuts = fopen(file, "w")) != NULL) {

      cut = editor->cuts;
      while (cut) {
         switch (cut->type) {
            case CUT:
               fprintf(fcuts, "%d %s %s %s %s %s %d\n", cut->type,
                       OUT(cut->not), OUT(cut->lvalue), OUT(cut->operator),
                       OUT(cut->rvalue), OUT(cut->loperator), cut->active);
               break;
            case WEIGHT:
               fprintf(fcuts, "%d %s %s %s %d\n", cut->type,
                       OUT(cut->lvalue), OUT(cut->operator),
                       OUT(cut->loperator), cut->active);
               break;
            case OPEN_BRACKET:
               fprintf(fcuts, "%d %s %s\n", cut->type,
                       OUT(cut->not), cut->lvalue);
               break;
            case CLOSE_BRACKET:
               fprintf(fcuts, "%d %s %s\n", cut->type,
                       cut->lvalue, OUT(cut->loperator));
               break;
         }
         cut = cut->link;
      }
      fclose(fcuts);

      /* set cut editor title to the saved file */
      strcpy(editor->file, file);
      strcpy(editor->title, "Cut Editor: ");
#ifdef vms
      s = strrchr(file, ']');
      if (!s)
         s = strrchr(file, ':');
#else
      s = strrchr(file, '/');
#endif
      if (s)
         strcat(editor->title, s+1);
      else
         strcat(editor->title, file);
      XtVaSetValues(XtParent(editor->widget), XmNtitle, editor->title,
                    XmNiconName, editor->title,
                    NULL);

   } else {
     km_inform_user(editor->widget, "Can't create cuts file.", "");
     return;
   }
}

/***********************************************************************
 *                                                                     *
 *   Get the arguments for the OPEN command and execute the            *
 *   command.                                                          *
 *                                                                     *
 ***********************************************************************/
static void get_openArg(Widget w, CutEditorStruct *editor,
                        XmFileSelectionBoxCallbackStruct *call_data)
{
   MenuCbStruct *item;
   char         *file;

   file = extract_normal_string(call_data->value);
   if (strempty(file)) {
      km_warn_user(w, "You must give a file name.", "");
      XtFree(file);
      return;
   }
#ifdef vms
   if ( strchr( file, ':' ) == NULL && strchr( file, ']' ) == NULL ) {
#else
   if (*file != '/' && *file != '~') {
#endif
      /*
       * if it is not a directory, determine the full pathname
       * of the selection by concatenating it to the "dir" part
       */
      char *dir, *newfile;
      dir = extract_normal_string(call_data->dir);
      newfile = XtMalloc(strlen(dir) + strlen(file) + 1);
      sprintf(newfile, "%s%s", dir, file);
      XtFree(dir);
      XtFree(file);
      file = newfile;
   }

   XtVaGetValues(w, XmNuserData, &item, NULL);
   popdown_argument_dialog(item);

   read_cuts_file(editor, file);

   XtFree(file);
}

/***********************************************************************
 *                                                                     *
 *   Show (manage) Open argument popup.                                *
 *                                                                     *
 ***********************************************************************/
static void show_openDialog(Widget w, CutEditorStruct *editor,
                            XmAnyCallbackStruct *cbs)
{
   Widget        dialog;
   MenuCbStruct *menu_item;
   XmString      xmmask;

   if (!editor->openDialog) {
      dialog = create_openCeDialog(editor->widget);

      /* unmanage the Help button */
      XtUnmanageChild(XmFileSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
      /*
       *   install callbacks
       */
      menu_item = make_menuStruct(w, dialog);
      InstallMwmCloseCallback(XtParent(dialog),
                              (XtCallbackProc)cancel_cb,
                              (XtPointer)menu_item);

      XtAddCallback(dialog, XmNokCallback,
                    (XtCallbackProc)get_openArg, editor);
      XtAddCallback(dialog, XmNcancelCallback,
                    (XtCallbackProc)cancel_cb, menu_item);

      /* store MenuCbSruct in userData */
      XtVaSetValues(dialog, XmNuserData, menu_item, NULL);

      editor->openDialog = dialog;
   }

   XtVaGetValues(editor->openDialog, XmNdirMask, &xmmask, NULL);
   XmFileSelectionDoSearch(editor->openDialog, xmmask);

   /* make menu item insensitive and popup dialog */
   XtSetSensitive(w, False);
   XtManageChild(editor->openDialog);
}

/***********************************************************************
 *                                                                     *
 *   Save cuts in current file.                                        *
 *                                                                     *
 ***********************************************************************/
static void save_cuts(Widget w, CutEditorStruct *editor,
                      XmAnyCallbackStruct *cbs)
{
   XmAnyCallbackStruct   cbs1;

   if (!strempty(editor->file))
      save_cuts_file(editor, editor->file);
   else {
      cbs1.reason = 0;
      cbs1.event  = (XEvent *) NULL;
      XtCallCallbacks(saveasCeButton, XmNactivateCallback, &cbs1);
   }
}

/***********************************************************************
 *                                                                     *
 *   Get the arguments for the SAVE AS command and execute the         *
 *   command.                                                          *
 *                                                                     *
 ***********************************************************************/
static void get_saveAsArg(Widget w, CutEditorStruct *editor,
                          XmFileSelectionBoxCallbackStruct *call_data)
{
   MenuCbStruct *item;
   char         *file;

   file = extract_normal_string(call_data->value);
   if (strempty(file)) {
      km_warn_user(w, "You must give a file name.", "");
      XtFree(file);
      return;
   }
#ifdef vms
   if ( strchr( file, ':' ) == NULL && strchr( file, ']' ) == NULL ) {
#else
   if (*file != '/') {
#endif
      /*
       * if it is not a directory, determine the full pathname
       * of the selection by concatenating it to the "dir" part
       */
      char *dir, *newfile;
      dir = extract_normal_string(call_data->dir);
      newfile = XtMalloc(strlen(dir) + strlen(file) + 1);
      sprintf(newfile, "%s%s", dir, file);
      XtFree(dir);
      XtFree(file);
      file = newfile;
   }

   XtVaGetValues(w, XmNuserData, &item, NULL);
   popdown_argument_dialog(item);

   save_cuts_file(editor, file);

   XtFree(file);
}

/***********************************************************************
 *                                                                     *
 *   Show (manage) Save As argument popup.                             *
 *                                                                     *
 ***********************************************************************/
static void show_saveAsDialog(Widget w, CutEditorStruct *editor,
                              XmAnyCallbackStruct *cbs)
{
   Widget        dialog;
   MenuCbStruct *menu_item;
   XmString      xmmask;

   if (!editor->saveAsDialog) {
      dialog = create_saveAsCeDialog(editor->widget);

      /* unmanage the Help button */
      XtUnmanageChild(XmFileSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
      /*
       *   install callbacks
       */
      menu_item = make_menuStruct(w, dialog);
      InstallMwmCloseCallback(XtParent(dialog),
                              (XtCallbackProc)cancel_cb,
                              (XtPointer)menu_item);

      XtAddCallback(dialog, XmNokCallback,
                    (XtCallbackProc)get_saveAsArg, editor);
      XtAddCallback(dialog, XmNcancelCallback,
                    (XtCallbackProc)cancel_cb, menu_item);

      /* store MenuCbSruct in userData */
      XtVaSetValues(dialog, XmNuserData, menu_item, NULL);

      editor->saveAsDialog = dialog;
   }

   XtVaGetValues(editor->saveAsDialog, XmNdirMask, &xmmask, NULL);
   XmFileSelectionDoSearch(editor->saveAsDialog, xmmask);

   /* make menu item insensitive and popup dialog */
   XtSetSensitive(w, False);
   XtManageChild(editor->saveAsDialog);
}

/***********************************************************************
 *                                                                     *
 *   Add bracket to bracket cut.                                       *
 *                                                                     *
 ***********************************************************************/
static void make_weight(Cut *cut, Boolean weight)
{

   (void)get_cut_context(cut->widget);

   if (weight) {
      cut->type = WEIGHT;
      XtVaSetValues(expr2CeText, XmNmappedWhenManaged, False,
                                 XmNcolumns, 1, NULL);
      XtVaSetValues(expr1CeText, XmNcolumns, 39, NULL);
      XtVaSetValues(notCeButton, XmNmappedWhenManaged, False, NULL);

      if (cut->selected) XtSetSensitive(dynCeButton, False);
   } else {
      cut->type = CUT;
      XtVaSetValues(expr1CeText, XmNcolumns, 20, NULL);
      XtVaSetValues(expr2CeText, XmNmappedWhenManaged, True,
                                 XmNcolumns, 20, NULL);
      XtVaSetValues(notCeButton, XmNmappedWhenManaged, True, NULL);

      if (cut->selected) XtSetSensitive(dynCeButton, True);
   }
}

/***********************************************************************
 *                                                                     *
 *   Add bracket to bracket cut.                                       *
 *                                                                     *
 ***********************************************************************/
static void add_bracket(Cut *cut)
{
   char     *str, *label;
   XmString  xmstr;

   (void)brackets(cut, cut->type, &label);
   str = XtCalloc(strlen(label)+2, 1);

   if (cut->type == OPEN_BRACKET)
      sprintf(str, "%s(", label);
   else
      sprintf(str, "%s)", label);

   xmstr = XmStringCreateSimple(str);
   XtVaSetValues(bracketCeLabel, XmNlabelString, xmstr, NULL);
   XmStringFree(xmstr);
   XtFree(label);
   XtFree(str);
}

/***********************************************************************
 *                                                                     *
 *   Add cut to cut editor.                                            *
 *                                                                     *
 ***********************************************************************/
static Cut *add_cut(CutEditorStruct *editor, enum cut_t type, int pos)
{
   Cut      *cut, *tmp;
   char      str[10];
   XmString  xmstr;
   int       i, selected_pos;

   /* set cut editor context */
   (void)get_ce_context(editor->widget);

   /* allocate cut structure and initialize to 0 */
   cut = (Cut *) XtCalloc(1, sizeof(Cut));

   switch (type) {
      case CUT:
         editor->ncuts++;
         editor->last_id++;
         cut->id = editor->last_id;
         cut->widget = create_cutExpr(cutsCeForm);
         (void)get_cut_context(cut->widget);
         sprintf(str, "%d", cut->id);
         xmstr = XmStringCreateSimple(str);
         XtVaSetValues(countCeLabel, XmNlabelString, xmstr, NULL);
         XmStringFree(xmstr);
         XmToggleButtonSetState(activeCeToggle, True, False);
         cut->active = True;
         break;
      case OPEN_BRACKET:
         selected_pos = get_selected_cut_pos(editor);
         tmp = get_cut_at_pos(editor, selected_pos);
         if (tmp->type == OPEN_BRACKET) {
            add_bracket(tmp);
            XtFree((char *)cut);
            select_cut(tmp);
            return tmp;
         }
         if (selected_pos == pos && pos != 1) {
            tmp = get_cut_at_pos(editor, selected_pos-1);
            if (tmp->type == OPEN_BRACKET) {
               add_bracket(tmp);
               XtFree((char *)cut);
               select_cut(tmp);
               return tmp;
            }
         }
         if (selected_pos != pos && pos != editor->ncuts) {
            tmp = get_cut_at_pos(editor, selected_pos+1);
            if (tmp->type == OPEN_BRACKET) {
               add_bracket(tmp);
               XtFree((char *)cut);
               select_cut(tmp);
               return tmp;
            }
         }
         cut->id = 0;
         cut->widget = create_cutExpr(cutsCeForm);
         (void)get_cut_context(cut->widget);
         xmstr = XmStringCreateSimple("(");
         XtVaSetValues(bracketCeLabel, XmNlabelString, xmstr, NULL);
         XmStringFree(xmstr);
         XtVaSetValues(countCeLabel,   XmNmappedWhenManaged, False, NULL);
         XtVaSetValues(activeCeToggle, XmNmappedWhenManaged, False, NULL);
         XtVaSetValues(expr1CeText,    XmNmappedWhenManaged, False, NULL);
         XtVaSetValues(operatorMenu,   XmNmappedWhenManaged, False, NULL);
         XtVaSetValues(expr2CeText,    XmNmappedWhenManaged, False, NULL);
         XtVaSetValues(logicalMenu,    XmNmappedWhenManaged, False, NULL);

         break;
      case CLOSE_BRACKET:
         selected_pos = get_selected_cut_pos(editor);
         tmp = get_cut_at_pos(editor, selected_pos);
         if (tmp->type == CLOSE_BRACKET) {
            add_bracket(tmp);
            XtFree((char *)cut);
            select_cut(tmp);
            return tmp;
         }
         if (selected_pos == pos && pos != 1) {
            tmp = get_cut_at_pos(editor, selected_pos-1);
            if (tmp->type == CLOSE_BRACKET) {
               add_bracket(tmp);
               XtFree((char *)cut);
               select_cut(tmp);
               return tmp;
            }
         }
         if (selected_pos != pos && pos != editor->ncuts) {
            tmp = get_cut_at_pos(editor, selected_pos+1);
            if (tmp->type == CLOSE_BRACKET) {
               add_bracket(tmp);
               XtFree((char *)cut);
               select_cut(tmp);
               return tmp;
            }
         }
         cut->id = 0;
         cut->widget = create_cutExpr(cutsCeForm);
         (void)get_cut_context(cut->widget);
         xmstr = XmStringCreateSimple(")");
         XtVaSetValues(bracketCeLabel, XmNlabelString, xmstr, NULL);
         XmStringFree(xmstr);
         XtVaSetValues(countCeLabel,   XmNmappedWhenManaged, False, NULL);
         XtVaSetValues(notCeButton,    XmNmappedWhenManaged, False, NULL);
         XtVaSetValues(activeCeToggle, XmNmappedWhenManaged, False, NULL);
         XtVaSetValues(expr1CeText,    XmNmappedWhenManaged, False, NULL);
         XtVaSetValues(operatorMenu,   XmNmappedWhenManaged, False, NULL);
         XtVaSetValues(expr2CeText,    XmNmappedWhenManaged, False, NULL);

         break;
   }

   /* fill in rest of cut structure */
   cut->type     = type;
   cut->selected = False;
   cut->editor   = editor;

   /* add event handlers so we can select a cut when a button is pressed */
   XtAddEventHandler(cut->widget, ButtonPressMask, False,
                     (XtEventHandler)post_menu, cut);
   XtAddEventHandler(countCeLabel, ButtonPressMask, False,
                     (XtEventHandler)post_menu, cut);
   XtAddEventHandler(bracketCeLabel, ButtonPressMask, False,
                     (XtEventHandler)post_menu, cut);
   XtAddEventHandler(expr1CeText, ButtonPressMask, False,
                     (XtEventHandler)post_menu, cut);
   XtAddEventHandler(expr2CeText, ButtonPressMask, False,
                     (XtEventHandler)post_menu, cut);

   /*
    * add other event handler to copy variable from N-tuple viewer to
    * cut expression (copy_variable is in ntuple.c)
    */
   XtAddEventHandler(expr1CeText, ButtonPressMask, False,
                     (XtEventHandler)copy_variable, NULL);
   XtAddEventHandler(expr2CeText, ButtonPressMask, False,
                     (XtEventHandler)copy_variable, NULL);

   /*
    * add modify verify callbacks on the expr text widgets
    */
   XtAddCallback(expr1CeText, XmNmodifyVerifyCallback,
                 (XtCallbackProc)activate_expression, cut);
   XtAddCallback(expr2CeText, XmNmodifyVerifyCallback,
                 (XtCallbackProc)activate_expression, cut);

   /*
    * add callbacks to activate toggle button and operator and logical menus
    */
   XtAddCallback(activeCeToggle, XmNvalueChangedCallback,
                 (XtCallbackProc)activate_toggle_cb, cut);
   XtAddCallback(notCeButton, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);
   XtAddCallback(notCeButton, XmNactivateCallback,
                 (XtCallbackProc)toggle_not_button, cut);
   XtAddCallback(operatorNULL, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);
   XtAddCallback(operatorEQ, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);
   XtAddCallback(operatorNE, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);
   XtAddCallback(operatorGT, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);
   XtAddCallback(operatorLT, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);
   XtAddCallback(operatorGE, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);
   XtAddCallback(operatorLE, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);
   XtAddCallback(operatorCT, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);
   XtAddCallback(operatorW, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);
   XtAddCallback(logicalNULL, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);
   XtAddCallback(logicalAND, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);
   XtAddCallback(logicalOR, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);
   XtAddCallback(logicalMULT, XmNactivateCallback,
                 (XtCallbackProc)operator_cb, cut);

   /* attach next cut at end of cut list if pos = 0, or at position pos */
   if (!editor->cuts) {
      editor->cuts     = cut;
      editor->last_cut = cut;
      XtVaSetValues(cut->widget,
                    XmNtopAttachment,   XmATTACH_FORM,
                    XmNtopOffset,       5,
                    XmNleftAttachment,  XmATTACH_FORM,
                    XmNleftOffset,      5,
                    XmNrightAttachment, XmATTACH_FORM,
                    XmNrightOffset,     5,
                    NULL);
   } else {
      if (pos == 0) {
         XtVaSetValues(cut->widget,
                       XmNtopAttachment,   XmATTACH_WIDGET,
                       XmNtopOffset,       5,
                       XmNtopWidget,       editor->last_cut->widget,
                       XmNleftAttachment,  XmATTACH_FORM,
                       XmNleftOffset,      5,
                       XmNrightAttachment, XmATTACH_FORM,
                       XmNrightOffset,     5,
                       NULL);
         editor->last_cut->link = cut;
         editor->last_cut       = cut;
      } else if (pos == 1) {
         XtVaSetValues(cut->widget,
                       XmNtopAttachment,   XmATTACH_FORM,
                       XmNtopOffset,       5,
                       XmNleftAttachment,  XmATTACH_FORM,
                       XmNleftOffset,      5,
                       XmNrightAttachment, XmATTACH_FORM,
                       XmNrightOffset,     5,
                       NULL);
         XtVaSetValues(editor->cuts->widget,
                       XmNtopAttachment,   XmATTACH_WIDGET,
                       XmNtopOffset,       5,
                       XmNtopWidget,       cut->widget,
                       XmNleftAttachment,  XmATTACH_FORM,
                       XmNleftOffset,      5,
                       XmNrightAttachment, XmATTACH_FORM,
                       XmNrightOffset,     5,
                       NULL);
         cut->link    = editor->cuts;
         editor->cuts = cut;
      } else {
         i   = 1;
         tmp = editor->cuts;
         while (i < pos-1) {
            i++;
            tmp = tmp->link;
            if (!tmp) {
               XtVaSetValues(cut->widget,
                             XmNtopAttachment,   XmATTACH_WIDGET,
                             XmNtopOffset,       5,
                             XmNtopWidget,       editor->last_cut->widget,
                             XmNleftAttachment,  XmATTACH_FORM,
                             XmNleftOffset,      5,
                             XmNrightAttachment, XmATTACH_FORM,
                             XmNrightOffset,     5,
                             NULL);
               editor->last_cut->link = cut;
               editor->last_cut       = cut;
               goto out;
            }
         }
         XtVaSetValues(cut->widget,
                       XmNtopAttachment,   XmATTACH_WIDGET,
                       XmNtopOffset,       5,
                       XmNtopWidget,       tmp->widget,
                       XmNleftAttachment,  XmATTACH_FORM,
                       XmNleftOffset,      5,
                       XmNrightAttachment, XmATTACH_FORM,
                       XmNrightOffset,     5,
                       NULL);
         if (tmp->link)
            XtVaSetValues(tmp->link->widget,
                          XmNtopAttachment,   XmATTACH_WIDGET,
                          XmNtopOffset,       5,
                          XmNtopWidget,       cut->widget,
                          XmNleftAttachment,  XmATTACH_FORM,
                          XmNleftOffset,      5,
                          XmNrightAttachment, XmATTACH_FORM,
                          XmNrightOffset,     5,
                          NULL);
         cut->link = tmp->link;
         tmp->link = cut;
      }
   }

out:
   show_logical_menu(editor);

   XtManageChild(cut->widget);

   XmUpdateDisplay(editor->widget);

   select_cut(cut);

   return cut;
}

/***********************************************************************
 *                                                                     *
 *   Delete cut from the cut editor.                                   *
 *                                                                     *
 ***********************************************************************/
static void delete_cut(CutEditorStruct *editor, int pos)
{
   Cut      *cut1, *cut2;
   char     *str, *label;
   XmString  xmstr;
   int       i;

   /* set cut editor context */
   (void)get_ce_context(editor->widget);

   /* delete all cuts */
   if (pos == 0) {

      cut1 = editor->cuts;
      while (cut1) {
         XtDestroyWidget(cut1->widget);
         cut2 = cut1;
         cut1 = cut1->link;
         XtFree((char *)cut2);
      }
      editor->ncuts    = 0;
      editor->last_id  = 0;
      editor->cuts     = (Cut *)NULL;
      editor->last_cut = (Cut *)NULL;

   } else {

      cut1 = editor->cuts;
      cut2 = (Cut *)NULL;
      i    = 0;
      while (cut1) {
         i++;
         if (i == pos) break;
         cut2 = cut1;
         cut1 = cut1->link;
      }

      /* item not found */
      if (!cut1) return;

      switch (cut1->type) {
         case CUT:
         case WEIGHT:
            editor->ncuts--;
            break;
         case OPEN_BRACKET:
            i = brackets(cut1, OPEN_BRACKET, &label);
            if (i > 1) {
               /* remove only one of the open brackets */
               str = XtCalloc(strlen(label), 1);
               strncpy(str, label, strlen(label)-1);
               xmstr = XmStringCreateSimple(str);
               XtVaSetValues(bracketCeLabel, XmNlabelString, xmstr, NULL);
               XmStringFree(xmstr);
               XtFree(str);
               XtFree(label);
               goto out;
            }
            XtFree(label);
            break;
         case CLOSE_BRACKET:
            i = brackets(cut1, CLOSE_BRACKET, &label);
            if (i > 1) {
               /* remove only one of the closing brackets */
               str = XtCalloc(strlen(label), 1);
               strncpy(str, label, strlen(label)-1);
               xmstr = XmStringCreateSimple(str);
               XtVaSetValues(bracketCeLabel, XmNlabelString, xmstr, NULL);
               XmStringFree(xmstr);
               XtFree(str);
               XtFree(label);
               goto out;
            }
            XtFree(label);
            break;
      }

      XtDestroyWidget(cut1->widget);
      if (cut2) {
         cut2->link = cut1->link;
         XtVaSetValues(cut2->link->widget,
                       XmNtopAttachment,  XmATTACH_WIDGET,
                       XmNtopOffset,      5,
                       XmNtopWidget,      cut2->widget,
                       XmNleftAttachment, XmATTACH_FORM,
                       XmNleftOffset,     5,
                       NULL);
         select_cut(cut2);
         if (editor->last_cut == cut1) editor->last_cut = cut2;
      } else {
         editor->cuts = cut1->link;
         if (editor->cuts) {
            XtVaSetValues(editor->cuts->widget,
                          XmNtopAttachment,  XmATTACH_FORM,
                          XmNtopOffset,      5,
                          XmNleftAttachment, XmATTACH_FORM,
                          XmNleftOffset,     5,
                          NULL);
            select_cut(editor->cuts);
         }
         if (editor->last_cut == cut1) editor->last_cut = NULL;
      }
      XtFree((char *)cut1);

      show_logical_menu(editor);
   }

out:
   XmUpdateDisplay(editor->widget);
}

/***********************************************************************
 *                                                                     *
 *   Add cuts to the cut editor.                                       *
 *                                                                     *
 ***********************************************************************/
static void add_cut_expression(Widget w, CutEditorStruct *editor,
                               XmAnyCallbackStruct *cbs)
{
   int pos;

   /* set cut editor context */
   (void)get_ce_context(editor->widget);

   pos = get_selected_cut_pos(editor);

   if (w == addBeforeCeButton) {
      (void)add_cut(editor, CUT, pos);
   }
   if (w == addAfterCeButton) {
      (void)add_cut(editor, CUT, pos+1);
   }
   if (w == addBeforeOpenCeButton) {
      (void)add_cut(editor, OPEN_BRACKET, pos);
   }
   if (w == addAfterOpenCeButton) {
      (void)add_cut(editor, OPEN_BRACKET, pos+1);
   }
   if (w == addBeforeCloseCeButton) {
      (void)add_cut(editor, CLOSE_BRACKET, pos);
   }
   if (w == addAfterCloseCeButton) {
      (void)add_cut(editor, CLOSE_BRACKET, pos+1);
   }
   indent_cuts(editor);
}

/***********************************************************************
 *                                                                     *
 *   Remove cuts from the cut editor.                                  *
 *                                                                     *
 ***********************************************************************/
static void delete_cut_expression(Widget w, CutEditorStruct *editor,
                                  XmAnyCallbackStruct *cbs)
{
   int pos;

   /* set cut editor context */
   (void)get_ce_context(editor->widget);

   pos = get_selected_cut_pos(editor);

   if (w == deleteCeButton) {
      delete_cut(editor, pos);
      if (editor->ncuts == 0)
         (void)add_cut(editor, CUT, 0);
   }
   if (w == deleteAllCeButton) {
      delete_cut(editor, 0);
      (void)add_cut(editor, CUT, 0);
   }
   indent_cuts(editor);

   cut_expression[0] = '\0';
   set_cut_expression(editor);
}

/***********************************************************************
 *                                                                     *
 *   Recursively remove empty bracket pairs from the cut expression.   *
 *   Like in 1.AND.()  --->  1.AND.                                    *
 *                                                                     *
 ***********************************************************************/
static void rm_empty_brackets(char *cutstr)
{
   char *str, *s1, *s2;
   int   pairs = 0;

   s1 = cutstr;
   s2 = str = XtCalloc(strlen(cutstr)+1, 1);

   while (*s1) {
      if (*s1 == '(' && *(s1+1) == ')') {
         s1 += 2;
         pairs++;
      } else {
         *s2 = *s1;
         s1++;
         s2++;
      }
   }
   strcpy(cutstr, str);
   XtFree(str);

   if (pairs) rm_empty_brackets(cutstr);
}

/***********************************************************************
 *                                                                     *
 *   Remove unused bracket pairs from the cut expression.              *
 *   Like in 1.AND.(2)   --->  1.AND.2                                 *
 *                                                                     *
 ***********************************************************************/
static void rm_unused_brackets(char *cutstr)
{
   char     *str, *s, *s1, *s2;
   Boolean   unused = False;

   s1 = cutstr;
   s2 = str = XtCalloc(strlen(cutstr)+1, 1);

   while (*s1) {
      if (*s1 == '(') {
         s = s1 + 1;
         unused = True;
         /*
          * look ahead to see if there is something else as a digit before
          * the closing bracket
          */
         while (*s != ')') {
            if (!isdigit((int)*s) && *s != '$') unused = False;
            s++;
         }
         if (unused)
            s1++;
         else {
            *s2 = *s1;
            s1++;
            s2++;
         }
      } else if (unused && s1 == s) {
         unused = False;
         s1++;
      } else {
         *s2 = *s1;
         s1++;
         s2++;
      }
   }
   strcpy(cutstr, str);
   XtFree(str);
}

/***********************************************************************
 *                                                                     *
 *   Remove unused logical expressions from the cut expression.        *
 *   Like in 1.AND.  ---> 1                                            *
 *                                                                     *
 ***********************************************************************/
static void rm_logicals(char *cutstr)
{
   char *s1, *s2;

   s1 = cutstr;
   s2 = strstr(s1, ".NOT.");
   while (s2) {
      if (*(s2+5) == '\0' || *(s2+5) == '.') {
         strcpy(s2, s2+5);
         s2 = strstr(s2, ".NOT.");
      } else
         s2 = strstr(s2+5, ".NOT.");
   }

   s1 = cutstr;
   s2 = strstr(s1, ".AND.");
   while (s2) {
      if (*(s2+5) == '\0' || *(s2+5) == ')') {
         strcpy(s2, s2+5);
         s2 = strstr(s2, ".AND.");
      } else
         s2 = strstr(s2+5, ".AND.");
   }

   s1 = cutstr;
   s2 = strstr(s1, ".OR.");
   while (s2) {
      if (*(s2+4) == '\0' || *(s2+4) == ')') {
         strcpy(s2, s2+4);
         s2 = strstr(s2, ".OR.");
      } else
         s2 = strstr(s2+4, ".OR.");
   }
}

/***********************************************************************
 *                                                                     *
 *   Set the cut expression label.                                     *
 *                                                                     *
 ***********************************************************************/
static void set_cut_expression(CutEditorStruct *editor)
{
   char     *label;
   XmString  xmstr;

   (void)get_ce_context(editor->widget);

   label = XtCalloc(MAX_CMD_LENGTH, 1);
   sprintf(label, "Cut Expression:  %s", cut_expression);
   xmstr = XmStringCreateSimple(label);
   XtVaSetValues(fullCeLabel, XmNlabelString, xmstr, NULL);
   XmStringFree(xmstr);
   XtFree(label);
}

/***********************************************************************
 *                                                                     *
 *   Apply cuts.                                                       *
 *                                                                     *
 ***********************************************************************/
static void apply_cb(Widget w, CutEditorStruct *editor,
                     XmAnyCallbackStruct *cbs)
{
   Cut       *cut;
   char      *comstr, *cutstr;
   char      *expr, *exprc, *str, *brackets;
   XmString   xmstr;
   Widget     operator, logical;
   Boolean    mapped;
   XmAnyCallbackStruct cbs1;

   /* set cut editor context */
   (void)get_ce_context(editor->widget);

   cut_expression[0] = '\0';
   xmstr = XmStringCreateSimple("Cut Expression:");
   XtVaSetValues(fullCeLabel, XmNlabelString, xmstr, NULL);
   XmStringFree(xmstr);

   if (!balanced_brackets(editor)) {
      km_warn_user(editor->widget, "Brackets are not balanced.", "");
      return;
   }

   comstr = XtCalloc(MAX_CMD_LENGTH, 1);
   /* cutstr = XtCalloc(MAX_CMD_LENGTH, 1); */
   cutstr = strdup( "" );

   /* erase previous cut definitions */
   sprintf(comstr, "Ntuple/Cuts $0 -");
   execute_kuip_cmd(comstr);

   /* loop over all cuts and send the active cuts to PAW */
   cut = editor->cuts;
   while (cut) {

      (void)get_cut_context(cut->widget);

      cut->cut[0]       = '\0';
      cut->not[0]       = '\0';
      cut->lvalue[0]    = '\0';
      cut->operator[0]  = '\0';
      cut->rvalue[0]    = '\0';
      cut->loperator[0] = '\0';
      (void)get_cut_context(cut->widget);

      switch (cut->type) {
         case CUT:

            /* NOT cut? */
            XtVaGetValues(notCeButton, XmNlabelString, &xmstr, NULL);
            str = extract_normal_string(xmstr);
            if (!strcmp(str, "!")) {
               strcat(cut->not, ".NOT.");
               if (cut->active)
                  /* strcat(cutstr, cut->not); */
                  cutstr = mstrcat( cutstr, cut->not );
            }
            XtFree(str);

            /* get cut number */
            if (cut->active) {
               XtVaGetValues(countCeLabel, XmNlabelString, &xmstr, NULL);
               str = extract_normal_string(xmstr);
               sprintf(comstr, "Ntuple/Cuts $%s ", str);
               /* strcat(cutstr, "$"); strcat(cutstr, str); */
               cutstr = mstrcat(cutstr, "$"); 
               cutstr = mstrcat(cutstr, str);
               XtFree(str);
            }

            /* get left value */
            expr = XmTextGetString(expr1CeText);
            if (strempty(expr) && cut->active) {
               km_warn_user(cut->widget, "No lvalue specified.", "");
               XtFree(expr);
               XtFree(comstr);
               /* XtFree(cutstr); */
               free( cutstr );
               return;
            }
            exprc = compress(expr);
            strcat(cut->lvalue, exprc);
            XtFree(expr);
            XtFree(exprc);

            /* get operator */
            XtVaGetValues(operatorMenu, XmNmenuHistory, &operator, NULL);
            if (operator == operatorNULL && cut->active) {
               km_warn_user(cut->widget, "No operator specified.", "");
               XtFree(comstr);
               /* XtFree(cutstr); */
               free( cutstr );
               return;
            }
            if (operator == operatorEQ)
               strcat(cut->operator, "=");
            else if (operator == operatorNE)
               strcat(cut->operator, "<>");
            else if (operator == operatorGT)
               strcat(cut->operator, ">");
            else if (operator == operatorLT)
               strcat(cut->operator, "<");
            else if (operator == operatorGE)
               strcat(cut->operator, ">=");
            else if (operator == operatorLE)
               strcat(cut->operator, "<=");
            else if (operator == operatorCT)
               strcat(cut->operator, "#");

            /* get right value */
            expr = XmTextGetString(expr2CeText);
            if (strempty(expr) && cut->active) {
               km_warn_user(cut->widget, "No rvalue specified.", "");
               XtFree(expr);
               XtFree(comstr);
               /* XtFree(cutstr); */
               free( cutstr );
               return;
            }
            exprc = compress(expr);
            strcat(cut->rvalue, exprc);
            XtFree(expr);
            XtFree(exprc);

            /* get logical operator */
            XtVaGetValues(logicalMenu, XmNmappedWhenManaged, &mapped, NULL);
            if (mapped) {
               XtVaGetValues(logicalMenu, XmNmenuHistory, &logical, NULL);
               if (logical == logicalNULL && cut->active) {
                  km_warn_user(cut->widget,
                            "No logical operator specified.", "");
                  XtFree(comstr);
                  /* XtFree(cutstr); */
                  free( cutstr );
                  return;
               }
               if (logical == logicalAND)
                  strcat(cut->loperator, ".AND.");
               else if (logical == logicalOR)
                  strcat(cut->loperator, ".OR.");
               else if (logical == logicalMULT)
                  strcat(cut->loperator, "*");

               if (cut->active)
                  /* strcat(cutstr, cut->loperator); */
                  cutstr = mstrcat(cutstr, cut->loperator);
            }

            sprintf(cut->cut, "%s%s%s", cut->lvalue, cut->operator,
                    cut->rvalue);
            if (cut->active) {
               strcat(comstr, cut->cut);
               execute_kuip_cmd(comstr);
            }

            break;
         case WEIGHT:

            /* get weight */
            expr = XmTextGetString(expr1CeText);
            if (strempty(expr) && cut->active) {
               km_warn_user(cut->widget, "No weight specified.", "");
               XtFree(expr);
               XtFree(comstr);
               /* XtFree(cutstr); */
               free( cutstr );
               return;
            }
            exprc = compress(expr);
            strcat(cut->lvalue, exprc);
            XtFree(expr);
            XtFree(exprc);

            if (cut->active)
               /* strcat(cutstr, cut->lvalue); */
               cutstr = mstrcat(cutstr, cut->lvalue);

            strcat(cut->operator, "W");

            /* get logical operator */
            XtVaGetValues(logicalMenu, XmNmappedWhenManaged, &mapped, NULL);
            if (mapped) {
               XtVaGetValues(logicalMenu, XmNmenuHistory, &logical, NULL);
               if (logical == logicalNULL && cut->active) {
                  km_warn_user(cut->widget,
                            "No logical operator specified.", "");
                  XtFree(comstr);
                  /* XtFree(cutstr); */
                  free( cutstr );
                  return;
               }
               if (logical == logicalAND)
                  strcat(cut->loperator, ".AND.");
               else if (logical == logicalOR)
                  strcat(cut->loperator, ".OR.");
               else if (logical == logicalMULT)
                  strcat(cut->loperator, "*");

               if (cut->active)
                  /* strcat(cutstr, cut->loperator); */
                  cutstr = mstrcat(cutstr, cut->loperator);
            }

            break;
         case OPEN_BRACKET:

            /* NOT bracket */
            XtVaGetValues(notCeButton, XmNlabelString, &xmstr, NULL);
            str = extract_normal_string(xmstr);
            if (!strcmp(str, "!")) {
               strcat(cut->not, ".NOT.");
               /* strcat(cutstr, cut->not); */
               cutstr = mstrcat(cutstr, cut->not);
            }
            XtFree(str);

            /* get open brackets */
            XtVaGetValues(bracketCeLabel, XmNlabelString, &xmstr, NULL);
            brackets = extract_normal_string(xmstr);
            /* strcat(cutstr, brackets); */
            cutstr = mstrcat(cutstr, brackets);
            strcat(cut->lvalue, brackets);
            XtFree(brackets);
            break;
         case CLOSE_BRACKET:

            /* get close brackets */
            XtVaGetValues(bracketCeLabel, XmNlabelString, &xmstr, NULL);
            brackets = extract_normal_string(xmstr);
            /* strcat(cutstr, brackets); */
            cutstr = mstrcat(cutstr, brackets);
            strcat(cut->lvalue, brackets);
            XtFree(brackets);

            /* get logical operator */
            XtVaGetValues(logicalMenu, XmNmappedWhenManaged, &mapped, NULL);
            if (mapped) {
               XtVaGetValues(logicalMenu, XmNmenuHistory, &logical, NULL);
               if (logical == logicalNULL) {
                  km_warn_user(cut->widget,
                               "No logical operator specified.", "");
                  XtFree(comstr);
                  /* XtFree(cutstr); */
                  free( cutstr );
                  return;
               }
               if (logical == logicalAND)
                  strcat(cut->loperator, ".AND.");
               else if (logical == logicalOR)
                  strcat(cut->loperator, ".OR.");
               else if (logical == logicalMULT)
                  strcat(cut->loperator, "*");

               /* strcat(cutstr, cut->loperator); */
               cutstr = mstrcat(cutstr, cut->loperator);
            }
            break;
      }
      cut = cut->link;
   }

   rm_empty_brackets(cutstr);
   rm_logicals(cutstr);
   rm_unused_brackets(cutstr);

   if (!strempty(cutstr)) {
      strcpy(cut_expression, cutstr);
      set_cut_expression(editor);
   }

   XtFree(comstr);
   /* XtFree(cutstr); */
   free( cutstr );

   if (w == applyPlotCeButton) {
      cbs1.reason = 0;
      cbs1.event  = (XEvent *) NULL;
      XtCallCallbacks(plotNtButton, XmNactivateCallback, &cbs1);
   }
}

/***********************************************************************
 *                                                                     *
 *   Activate or deactivate all cut expressions.                       *
 *                                                                     *
 ***********************************************************************/
static void activate(Widget w, CutEditorStruct *editor,
                     XmAnyCallbackStruct *cbs)
{
   Cut  *cut;

   /* set cut editor context */
   (void)get_ce_context(editor->widget);

   cut = editor->cuts;
   while (cut) {
      (void)get_cut_context(cut->widget);

      if (w == activateCeButton)
         XmToggleButtonSetState(activeCeToggle, True, True);
      else
         XmToggleButtonSetState(activeCeToggle, False, True);

      cut = cut->link;
   }
}

/***********************************************************************
 *                                                                     *
 *   Activate cut expression when something is written in the expr     *
 *   text widgets.                                                     *
 *                                                                     *
 ***********************************************************************/
static void activate_expression(Widget w, Cut *cut,
                                XmTextVerifyCallbackStruct *cbs)
{
   /********
   (void)get_cut_context(cut->widget);

   if (cbs->text->length)
      XmToggleButtonSetState(activeCeToggle, True, True);
   ********/

   cut_expression[0] = '\0';
   set_cut_expression(cut->editor);
}

/***********************************************************************
 *                                                                     *
 *   Activate or deactivate cut expression when hitting the activate   *
 *   toggle button. Also invalidate the current cut expression.        *
 *                                                                     *
 ***********************************************************************/
static void activate_toggle_cb(Widget w, Cut *cut,
                               XmToggleButtonCallbackStruct *cbs)
{
   cut->active = cbs->set;

   cut_expression[0] = '\0';
   set_cut_expression(cut->editor);
}

/***********************************************************************
 *                                                                     *
 *   Invalidate the current cut expression when changing the operator. *
 *                                                                     *
 ***********************************************************************/
static void operator_cb(Widget w,  Cut *cut, XmAnyCallbackStruct *cbs)
{
   cut_expression[0] = '\0';
   set_cut_expression(cut->editor);

   (void)get_cut_context(cut->widget);

   if (w == logicalNULL || w == logicalAND ||
       w == logicalOR   || w == logicalMULT ||
       w == notCeButton) return;

   if (w == operatorW)
      make_weight(cut, True);
   else
      make_weight(cut, False);
}

/***********************************************************************
 *                                                                     *
 *   Toggle the NOT button from "" to "!" and vice versa.              *
 *                                                                     *
 ***********************************************************************/
static void toggle_not_button(Widget w,  Cut *cut, XmAnyCallbackStruct *cbs)
{
   char        *str;
   XmString     xmstr;

   XtVaGetValues(w, XmNlabelString, &xmstr, NULL);
   str = extract_normal_string(xmstr);
   if (!strcmp(str, "!")) {
      xmstr = XmStringCreateSimple("");
      XtVaSetValues(w, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
      TogglePushButton(w, False);
   } else {
      xmstr = XmStringCreateSimple("!");
      XtVaSetValues(w, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
      TogglePushButton(w, True);
   }
   XtFree(str);
}

/***********************************************************************
 *                                                                     *
 *   Event handler for the pressing of the 1st and 3rd mouse buttons   *
 *   on a cut form or its text children.                               *
 *                                                                     *
 ***********************************************************************/
static void post_menu(Widget w, Cut *cut, XButtonPressedEvent *event)
{
   /* only handle 1st and 3rd mouse button */
   if (event->button == 2)
      return;

   (void)get_cut_context(cut->widget);

   select_cut(cut);

   /* popup = cut->popup_menu; */

   /*
    * position the menu at the location of the button press!  If we wanted
    * to position it elsewhere, we could change the x,y fields of the
    * event structure.
    */
   /*
   XmMenuPosition(popup, event);
   XtManageChild(popup);
   */
}

/***********************************************************************
 *                                                                     *
 *   Select cut and deselect all other cuts.                           *
 *                                                                     *
 ***********************************************************************/
static void select_cut(Cut *cut)
{
   Cut              *tmp;
   Pixel             bg;

   tmp = cut->editor->cuts;
   while (tmp) {
      if (tmp->selected) {
         if (tmp == cut) return;
         XtVaGetValues(tmp->widget, XmNbackground, &bg, NULL);
         XtVaSetValues(tmp->widget, XmNborderWidth, 1,
                       XmNborderColor, bg, NULL);
         tmp->selected = False;
      }
      tmp = tmp->link;
   }
   XtVaSetValues(cut->widget, XmNborderWidth, 1,
                 RES_CONVERT(XmNborderColor, "red"), NULL);
   cut->selected = True;

   if (cut->type == CUT)
      XtSetSensitive(dynCeButton, True);
   else
      XtSetSensitive(dynCeButton, False);
}

/***********************************************************************
 *                                                                     *
 *   Return the position of the currently selected cut.                *
 *                                                                     *
 ***********************************************************************/
static int get_selected_cut_pos(CutEditorStruct *editor)
{
   Cut    *cut;
   int     pos = 0;

   cut = editor->cuts;
   while (cut) {
      pos++;
      if (cut->selected) return pos;
      cut = cut->link;
   }

   return -1;
}

/***********************************************************************
 *                                                                     *
 *   Return the cut at position pos.                                   *
 *                                                                     *
 ***********************************************************************/
static Cut *get_cut_at_pos(CutEditorStruct *editor, int pos)
{
   Cut    *cut;
   int     i = 0;

   if (pos <= 0) return (Cut *)NULL;

   cut = editor->cuts;
   while (cut) {
      i++;
      if (i == pos) return cut;
      cut = cut->link;
   }

   return (Cut *)NULL;
}

/***********************************************************************
 *                                                                     *
 *   Return the currently selected cut.                                *
 *                                                                     *
 ***********************************************************************/
static Cut *get_selected_cut(CutEditorStruct *editor)
{
   Cut    *cut;

   cut = editor->cuts;
   while (cut) {
      if (cut->selected) return cut;
      cut = cut->link;
   }

   return (Cut *)NULL;
}

/***********************************************************************
 *                                                                     *
 *   Return the number of brackets.                                    *
 *                                                                     *
 ***********************************************************************/
static int brackets(Cut *cut, enum cut_t type, char **label)
{
   XmString   xmstr;
   char      *str;
   int        bracket = 0;

   (void)get_cut_context(cut->widget);
   XtVaGetValues(bracketCeLabel, XmNlabelString, &xmstr, NULL);
   str = *label = extract_normal_string(xmstr);

   while (*str) {
      switch (type) {
         case OPEN_BRACKET:
            if (*str == '(') bracket++;
            break;
         case CLOSE_BRACKET:
            if (*str == ')') bracket++;
            break;
      }
      str++;
   }
   return bracket;
}

/***********************************************************************
 *                                                                     *
 *   Are brackets balanced.                                            *
 *                                                                     *
 ***********************************************************************/
static Boolean balanced_brackets(CutEditorStruct *editor)
{
   Cut    *cut;
   char   *label;
   int     lbrack = 0;
   int     rbrack = 0;

   cut = editor->cuts;
   while (cut) {
      if (cut->type == OPEN_BRACKET) {
         lbrack += brackets(cut, OPEN_BRACKET, &label);
         XtFree(label);
      }
      if (cut->type == CLOSE_BRACKET) {
         rbrack += brackets(cut, CLOSE_BRACKET, &label);
         XtFree(label);
      }
      cut = cut->link;
   }
   return (lbrack == rbrack ? True : False);
}

/***********************************************************************
 *                                                                     *
 *   Indent cut expressions.                                           *
 *                                                                     *
 ***********************************************************************/
static void indent_cuts(CutEditorStruct *editor)
{
   Cut    *cut;
   char   *label;
   int     i;
   int     indent = 0;

   if (!balanced_brackets(editor)) return;

   cut = editor->cuts;
   while (cut) {

      (void)get_cut_context(cut->widget);

      switch (cut->type) {
         case CUT:
         case WEIGHT:
            XtVaSetValues(expr1CeText, XmNleftOffset, indent, NULL);
            break;
         case OPEN_BRACKET:
            XtVaSetValues(expr1CeText, XmNleftOffset, indent, NULL);
            i = brackets(cut, OPEN_BRACKET, &label);
            XtFree(label);
            indent += i*editor->indentation;
            break;
         case CLOSE_BRACKET:
            i = brackets(cut, CLOSE_BRACKET, &label);
            XtFree(label);
            indent -= i*editor->indentation;
            XtVaSetValues(expr1CeText, XmNleftOffset, indent, NULL);
            break;
      }
      cut = cut->link;
   }
   XmUpdateDisplay(editor->widget);
}

/***********************************************************************
 *                                                                     *
 *   Hide logical menu in case it is followed by a close bracket.      *
 *                                                                     *
 ***********************************************************************/
static void show_logical_menu(CutEditorStruct *editor)
{
   Cut   *cut, *cut1;

   cut1 = (Cut *)NULL;

   cut = editor->cuts;
   while (cut) {
      switch (cut->type) {
         case CUT:
         case WEIGHT:
            /*
             * if CUT then map the logical menu
             */
            (void)get_cut_context(cut->widget);
            XtVaSetValues(logicalMenu, XmNmappedWhenManaged, True, NULL);

            break;
         case OPEN_BRACKET:
            /*
             * if OPEN_BRACKET then unmap the logical menu
             */
            (void)get_cut_context(cut->widget);
            XtVaSetValues(logicalMenu, XmNmappedWhenManaged, False, NULL);

            break;
         case CLOSE_BRACKET:
            /*
             * if CLOSE_BRACKET then unmap the logical menu of the previous cut
             */
            if (cut1) {
               (void)get_cut_context(cut1->widget);
               XtVaSetValues(logicalMenu, XmNmappedWhenManaged, False, NULL);
            }

            /* and map logical menu of close bracket */
            (void)get_cut_context(cut->widget);
            XtVaSetValues(logicalMenu, XmNmappedWhenManaged, True, NULL);

            break;
      }

      cut1 = cut;
      cut  = cut->link;
   }

   /*
    * unmap logical menu of last cut
    */
   if (cut1) {
      (void)get_cut_context(cut1->widget);
      XtVaSetValues(logicalMenu, XmNmappedWhenManaged, False, NULL);
   }
}

/***********************************************************************
 *                                                                     *
 *   Get the indentation value from the prompt dialog.                 *
 *                                                                     *
 ***********************************************************************/
static void get_indent(Widget w, CutEditorStruct *editor,
                       XmSelectionBoxCallbackStruct *cbs)
{
   char          *str;
   MenuCbStruct  *item;

   str = extract_normal_string(cbs->value);
   editor->indentation = atoi(str);
   XtFree(str);

   indent_cuts(editor);

   XtVaGetValues(w, XmNuserData, &item, NULL);
   popdown_argument_dialog(item);
}

/***********************************************************************
 *                                                                     *
 *   Show the indentation prompt dialog.                               *
 *                                                                     *
 ***********************************************************************/
static void show_indentDialog(Widget w, CutEditorStruct *editor,
                              XmAnyCallbackStruct *cbs)
{
   Widget        dialog;
   MenuCbStruct *indent_menu;
   char         *str;
   XmString      xmstr;

   if (!editor->indentDialog) {
      dialog = create_indentCeDialog(editor->widget);

      /* unmanage the Help button */
      XtUnmanageChild(XmSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));

      /*
       *   install callbacks
       */
      indent_menu = make_menuStruct(w, dialog);
      InstallMwmCloseCallback(XtParent(dialog),
                              (XtCallbackProc)cancel_cb,
                              (XtPointer)indent_menu);

      XtAddCallback(dialog, XmNokCallback,
                    (XtCallbackProc)get_indent, editor);
      XtAddCallback(dialog, XmNcancelCallback,
                    (XtCallbackProc)cancel_cb, indent_menu);

      /* store MenuCbSruct in userData */
      XtVaSetValues(dialog, XmNuserData, indent_menu, NULL);

      editor->indentDialog = dialog;
   }

   /* set default value */
   str = XtCalloc(20, 1);
   sprintf(str, "%d", editor->indentation);
   xmstr = XmStringCreateSimple(str);
   XtVaSetValues(editor->indentDialog, XmNtextString, xmstr, NULL);
   XmStringFree(xmstr);
   XtFree(str);

   /* make menu item insensitive and popup dialog */
   XtSetSensitive(w, False);
   XtManageChild(editor->indentDialog);
}

/***********************************************************************
 *                                                                     *
 *   Show the dynamic cut dialog.                                      *
 *                                                                     *
 ***********************************************************************/
static void show_dynamicDialog(Widget w, CutEditorStruct *editor,
                               XmAnyCallbackStruct *cbs)
{
   Widget        dialog;
   char         *title;
   XmString      xmstr;
   Cut          *cut;

   /* set cut editor context */
   (void)get_ce_context(editor->widget);

   cut = get_selected_cut(editor);
   if (!cut) return;

   /* set cut context */
   (void)get_cut_context(cut->widget);

   /* return when not CUT or not active */
   if (cut->type != CUT) return;
   if (!cut->active) {
      km_warn_user(cut->widget, "Cut not active.", "");
      return;
   }

   if (!cut->dynamicDialog) {
      dialog = create_dynamicCeDialog(cut->widget);

      /*
       * get context of dynamicDialog
       */
      UxDynamicCeDialogContext = (_UxCdynamicCeDialog *)UxGetContext(dialog);

      /* set title */
      title = XtCalloc(MAX_STRING, 1);
      sprintf(title, "Dynamic Cut %d", cut->id);
      XtVaSetValues(XtParent(dialog), XmNtitle, title, NULL);
      XtFree(title);

      /*
       *   install callbacks
       */
      XtAddCallback(applyDyButton, XmNactivateCallback,
                    (XtCallbackProc)apply_dynamic, cut);
      XtAddCallback(resetDyButton, XmNactivateCallback,
                    (XtCallbackProc)reset_dynamic, cut);
      XtAddCallback(closeDyButton, XmNactivateCallback,
                    (XtCallbackProc)close_cb, dialog);
      XtAddCallback(dynamicDyScale, XmNvalueChangedCallback,
                    (XtCallbackProc)scale_dynamic, cut);
      XtAddCallback(dynamicDyScale, XmNdragCallback,
                    (XtCallbackProc)scale_dynamic, cut);
      XtAddCallback(percentDyText, XmNactivateCallback,
                    (XtCallbackProc)set_scale_range, cut);
      XtAddCallback(percentDyToggle, XmNvalueChangedCallback,
                    (XtCallbackProc)toggle_dynamic, cut);
      XtAddCallback(absoluteDyToggle, XmNvalueChangedCallback,
                    (XtCallbackProc)toggle_dynamic, cut);

      /* by default +/-20% of original value */
      XmTextSetString(percentDyText, "20");
      XtVaSetValues(dynamicDyScale, XmNminimum, -20,
                    XmNmaximum, 20,
                    XmNvalue, 0,
                    NULL);

      cut->dynamicDialog = dialog;

   } else {

      /* set context of dynamicDialog */
      UxDynamicCeDialogContext =
         (_UxCdynamicCeDialog *)UxGetContext(cut->dynamicDialog);

   }

   if (strlen(cut->cut) == 0) {
      km_warn_user(cut->widget, "No cut expression defined.\n\
First press the Apply button.", "");
      return;
   }

   if (appres.auto_refresh)
      XtSetSensitive(applyDyButton, False);
   else
      XtSetSensitive(applyDyButton, True);

   xmstr = XmStringCreateSimple(cut->cut);
   XtVaSetValues(cutDyLabel, XmNlabelString, xmstr, NULL);
   XmStringFree(xmstr);

   /* popup dialog */
   XtManageChild(cut->dynamicDialog);
}

/***********************************************************************
 *                                                                     *
 *   Change cut according to value set in scale.                       *
 *   Assumes that the dynamic cut dialog context has been set.         *
 *                                                                     *
 ***********************************************************************/
static void change_cut(Cut *cut, int value)
{
   char                  comstr[MAX_CMD_LENGTH], label[MAX_STRING];
   XmString              xmstr;

   if (XmToggleButtonGadgetGetState(percentDyToggle)) {
      sprintf(comstr, "Ntuple/Cuts $%d %s%s(%s)+((%s)*%f)", cut->id,
              cut->lvalue, cut->operator, cut->rvalue, cut->rvalue,
              (float)value*0.01);
      sprintf(label, "%s%s(%s)+((%s)*%f)",
              cut->lvalue, cut->operator, cut->rvalue, cut->rvalue,
              (float)value*0.01);
   } else {
      sprintf(comstr, "Ntuple/Cuts $%d %s%s(%s)+(%f)", cut->id,
              cut->lvalue, cut->operator, cut->rvalue, (float)value);
      sprintf(label, "%s%s(%s)+(%f)",
              cut->lvalue, cut->operator, cut->rvalue, (float)value);
   }

   xmstr = XmStringCreateSimple(label);
   XtVaSetValues(cutDyLabel, XmNlabelString, xmstr, NULL);
   XmStringFree(xmstr);

   XmUpdateDisplay(cut->dynamicDialog);

   execute_kuip_cmd(comstr);

   cut->dyn_cut_percent = value;
}

/***********************************************************************
 *                                                                     *
 *   Handle value changes in the dynamic scale.                        *
 *                                                                     *
 ***********************************************************************/
static void scale_dynamic(Widget w, Cut *cut, XmScaleCallbackStruct *cbs)
{
   XmAnyCallbackStruct   cbs1;

   if (cbs->value == cut->dyn_cut_percent) return;

   /* set context of dynamicDialog */
   UxDynamicCeDialogContext =
      (_UxCdynamicCeDialog *)UxGetContext(cut->dynamicDialog);

   change_cut(cut, cbs->value);

   if (appres.auto_refresh) {
      cbs1.reason = 0;
      cbs1.event  = (XEvent *) NULL;
      XtCallCallbacks(plotNtButton, XmNactivateCallback, &cbs1);
   }
}

/***********************************************************************
 *                                                                     *
 *   Set new scale range.                                              *
 *                                                                     *
 ***********************************************************************/
static void set_scale_range(Widget w, Cut *cut, XmAnyCallbackStruct *cbs)
{
   char  *str;
   int    range;

   /* set context of dynamicDialog */
   UxDynamicCeDialogContext =
      (_UxCdynamicCeDialog *)UxGetContext(cut->dynamicDialog);

   /* get new range */
   str = XmTextGetString(percentDyText);
   range = abs(atoi(str));
   XtVaSetValues(dynamicDyScale, XmNminimum, -range,
                 XmNmaximum, range,
                 NULL);
   XtFree(str);
}

/***********************************************************************
 *                                                                     *
 *   Plot the N-tuple using the dynamiclally changed cuts.             *
 *                                                                     *
 ***********************************************************************/
static void apply_dynamic(Widget w, Cut *cut, XmAnyCallbackStruct *cbs)
{
   XmAnyCallbackStruct   cbs1;

   cbs1.reason = 0;
   cbs1.event  = (XEvent *) NULL;
   XtCallCallbacks(plotNtButton, XmNactivateCallback, &cbs1);
}

/***********************************************************************
 *                                                                     *
 *   Handle value changes in the dynamic scale.                        *
 *                                                                     *
 ***********************************************************************/
static void reset_dynamic(Widget w, Cut *cut, XmAnyCallbackStruct *cbs)
{
   XmAnyCallbackStruct   cbs1;

   /* set context of dynamicDialog */
   UxDynamicCeDialogContext =
      (_UxCdynamicCeDialog *)UxGetContext(cut->dynamicDialog);

   /* by default +/-20% of original value */
   XmTextSetString(percentDyText, "20");
   XtVaSetValues(dynamicDyScale, XmNminimum, -20,
                 XmNmaximum, 20,
                 XmNvalue, 0,
                 NULL);

   XmToggleButtonGadgetSetState(percentDyToggle,  True,  False);
   XmToggleButtonGadgetSetState(absoluteDyToggle, False, False);

   change_cut(cut, 0);

   cbs1.reason = 0;
   cbs1.event  = (XEvent *) NULL;
   XtCallCallbacks(plotNtButton, XmNactivateCallback, &cbs1);
}

/***********************************************************************
 *                                                                     *
 *   Handle value changes for the percentage and absolute toggle       *
 *   buttons.                                                          *
 *                                                                     *
 ***********************************************************************/
static void toggle_dynamic(Widget w, Cut *cut,
                           XmToggleButtonCallbackStruct *cbs)
{
   XmAnyCallbackStruct   cbs1;

   /* set context of dynamicDialog */
   UxDynamicCeDialogContext =
      (_UxCdynamicCeDialog *)UxGetContext(cut->dynamicDialog);

   XtVaSetValues(dynamicDyScale, XmNvalue, 0, NULL);

   change_cut(cut, 0);

   cbs1.reason = 0;
   cbs1.event  = (XEvent *) NULL;
   XtCallCallbacks(plotNtButton, XmNactivateCallback, &cbs1);
}

