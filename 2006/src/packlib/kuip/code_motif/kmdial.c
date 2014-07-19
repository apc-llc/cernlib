/*
 * $Id: kmdial.c,v 1.1.1.1 1996/03/08 15:33:08 mclareni Exp $
 *
 * $Log: kmdial.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:08  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/09 06/07/94  14.26.30  by  Alfred Nathaniel*/
/*-- Author :    N.Cremel   03/04/92*/
/******************************************************************************/
/*                                                                            */
/*           MOTIF-based library for KUIP - deck KMDIAL                       */
/*                                                                            */
/*           General utilitity routines for handling dialogs :                */
/*                                                                            */
/*           - km_quit_confirm : Ask for confimation before quit              */
/*           - km_exit_confirm : Ask for confimation before exit              */
/*           - km_ask_user : Ask the user a question and wait for the answer  */
/*           - km_select : Give a message with a predefined number of answers */
/*           - km_warn_user : Warn with a message and wait till acknowledge   */
/*           - km_inform_user : Inform ...  and wait till acknowledge         */
/*           - km_prompt : Create prompt dialog and wait till OK or CANCEL    */
/*           - km_prompt_answer : same as km_prompt but no CANCEL button      */
/*                                                                            */
/******************************************************************************/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kmenu.h"

#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/DialogS.h>
#include <Xm/List.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>

#include "mkdims.h"
#include "kuip/mkutfu.h"
#include "mkutfm.h"
#include "mkutdm.h"

/***********************************************************************
 *                                                                     *
 *   Message Box for selection with 4 buttons                          *
 *                                                                     *
 ***********************************************************************/
static Widget create_select4(wtop, Button)
   Widget   wtop, Button[4];
{
   Widget        kuipSelect, Parent, Form;

   Parent = XtVaCreatePopupShell( "kuipSelect_shell",
                xmDialogShellWidgetClass, wtop,
                XmNshellUnitType, XmPIXELS,
                XmNtitle, "kuipSelect",
                NULL );

   Form = XtVaCreateWidget( "kuipSelect_form",
              xmFormWidgetClass,
              Parent,
              XmNunitType, XmPIXELS,
              XmNheight, 200,
              XmNwidth, 290,
              NULL );

   kuipSelect = XtVaCreateManagedWidget( "kuipSelect",
                    xmMessageBoxWidgetClass,
                    Form,
                    XmNresizePolicy, XmRESIZE_NONE,
                    XmNbottomAttachment, XmATTACH_FORM,
                    XmNleftAttachment, XmATTACH_FORM,
                    XmNrightAttachment, XmATTACH_FORM,
                    XmNtopAttachment, XmATTACH_FORM,
                    NULL );

   Button[0] = XtVaCreateManagedWidget( "okButton",
                  xmPushButtonWidgetClass,
                  kuipSelect,
                  RES_CONVERT( XmNlabelString, " Ok " ),
                  XmNx, 10,
                  XmNy, 150,
                  XmNrecomputeSize, FALSE,
                  XmNheight, 30,
                  XmNwidth, 50,
                  NULL );

   Button[1] = XtVaCreateManagedWidget( "goButton",
                  xmPushButtonWidgetClass,
                  kuipSelect,
                  RES_CONVERT( XmNlabelString, " Go " ),
                  XmNx, 80,
                  XmNy, 150,
                  XmNrecomputeSize, FALSE,
                  XmNheight, 30,
                  XmNwidth, 50,
                  NULL );

   Button[2] = XtVaCreateManagedWidget( "cancelButton",
                  xmPushButtonWidgetClass,
                  kuipSelect,
                  RES_CONVERT( XmNlabelString, " Cancel " ),
                  XmNx, 150,
                  XmNy, 150,
                  XmNrecomputeSize, FALSE,
                  XmNheight, 30,
                  XmNwidth, 50,
                  NULL );

   Button[3] = XtVaCreateManagedWidget( "helpButton",
                    xmPushButtonWidgetClass,
                    kuipSelect,
                    RES_CONVERT( XmNlabelString, " Help " ),
                    XmNx, 220,
                    XmNy, 150,
                  XmNrecomputeSize, FALSE,
                    XmNheight, 30,
                    XmNwidth, 50,
                    NULL );

   return ( kuipSelect );
}

/***********************************************************************
 *                                                                     *
 *   Get a response for the dialog.                                    *
 *                                                                     *
 ***********************************************************************/
static void response(w, answer, cbs)
   Widget   w;
   int     *answer;
   XmAnyCallbackStruct *cbs;
{
   if (cbs->reason == XmCR_OK)
      *answer = 1;
   else if (cbs->reason == XmCR_CANCEL)
      *answer = 2;
   else if (cbs->reason == XmCR_HELP)
      *answer = 3;
}

static void response1(w, answer, cbs)
   Widget   w;
   int     *answer;
   XmAnyCallbackStruct *cbs;
{
   *answer = 1;
}

static void response2(w, answer, cbs)
   Widget   w;
   int     *answer;
   XmAnyCallbackStruct *cbs;
{
   *answer = 2;
}

static void response3(w, answer, cbs)
   Widget   w;
   int     *answer;
   XmAnyCallbackStruct *cbs;
{
   *answer = 3;
}

static void response4(w, answer, cbs)
   Widget   w;
   int     *answer;
   XmAnyCallbackStruct *cbs;
{
   *answer = 4;
}

/***********************************************************************
 *                                                                     *
 *   Sets the destroyed flag for the dialog.                           *
 *                                                                     *
 ***********************************************************************/
static void destroy_dialog(w, destroyed, cbs)
   Widget w;
   Boolean *destroyed;
   XmAnyCallbackStruct *cbs;
{
   *destroyed = True;
}

/***********************************************************************
 *                                                                     *
 *   Ask for confimation before quit (return 1 or 0)                   *
 *                                                                     *
 ***********************************************************************/
int km_quit_confirm()
{
   char cmess[MAX_string];
   int answ;

   static char *clist[] = {"Yes", "No"};

   sprintf (cmess,"Do you really want to quit %s ? ",class_name);
   answ = km_select (XmDIALOG_QUESTION, cmess, 2, clist, 1);

   return (answ == 1 ? 1 : 0);
}

/***********************************************************************
 *                                                                     *
 *   Ask for confimation before exit (return 1 or 0)                   *
 *                                                                     *
 ***********************************************************************/
int km_exit_confirm()
{
   char cmess[MAX_string];
   int answ;

   static char *clist[] = {"Yes", "No"};

   sprintf (cmess,"Do you really want to exit %s ? ",class_name);

   answ = km_select (XmDIALOG_QUESTION, cmess, 2, clist, 1);

   return (answ == 1 ? 1 : 0);
}

/***********************************************************************
 *                                                                     *
 *   Ask the user a question and wait for the answer.                  *
 *                                                                     *
 ***********************************************************************/
Boolean km_ask_user(parent, question, ans1, ans2, default_ans)
   Widget  parent;
   char   *question, *ans1, *ans2;
   int     default_ans;
{
   static Widget dialog = NULL;
   static Boolean destroyed = False;
   static int    answer;
   Arg           al[5];
   int           ac;
   XmString      xmstr, text, yes, no;
   char          cval[MAX_string];

   answer = 0;

   /*
    *  check if the dialog has been destroyed (because its parent
    *  was destroyed)
    */
   if (destroyed) {
      destroyed = False;
      km_reset_appShell (dialog);
      dialog    = NULL;
   }

   /*
    * the dialog should be centered around the parent, if the parent
    * changes destroy the dialog and make it for the new parent
    */
   if (dialog && parent != XtParent(XtParent(dialog))) {
      XtDestroyWidget(dialog);
      dialog = NULL;
   }

   if (!dialog) {
      ac = 0;
      XtSetArg(al[ac], XmNautoUnmanage, False); ac++;
      XtSetArg(al[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;
      sprintf (cval, "%s Question", class_name);
      xmstr = XmStringCreateLtoR(cval, XmSTRING_DEFAULT_CHARSET);
      XtSetArg(al[ac], XmNdialogTitle, xmstr); ac++;
      XtSetArg(al[ac], XmNnoResize, True); ac++;
      XtSetArg(al[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
      dialog = XmCreateQuestionDialog(parent, "questionDialog", al, ac);
      km_store_appShell (dialog, cval);
      XtUnmanageChild(XmMessageBoxGetChild(dialog,
                     XmDIALOG_CANCEL_BUTTON));
      XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)response, &answer);
      XtAddCallback(dialog, XmNhelpCallback, (XtCallbackProc)response, &answer);
      XtAddCallback(dialog, XmNdestroyCallback, (XtCallbackProc)destroy_dialog,
                    &destroyed);
      XmStringFree(xmstr);
   }

   text = XmStringCreateLtoR(question, XmSTRING_DEFAULT_CHARSET);
   yes  = XmStringCreate(ans1, XmSTRING_DEFAULT_CHARSET);
   no   = XmStringCreate(ans2, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(dialog,
          XmNmessageString,      text,
          XmNokLabelString,      yes,
          XmNhelpLabelString,    no,
          XmNdefaultButtonType,  default_ans == 1 ?
                          XmDIALOG_OK_BUTTON : XmDIALOG_CANCEL_BUTTON,
          NULL);
   XmStringFree(text);
   XmStringFree(yes);
   XmStringFree(no);
   XtManageChild(dialog);

   while (answer == 0) {
      XtAppProcessEvent(UxAppContext, XtIMAll);
      XSync(XtDisplay(dialog), 0);
   }

   XtUnmanageChild(dialog);
   XSync(XtDisplay(dialog), 0);
   XmUpdateDisplay(dialog);
   km_reset_appShell (dialog);

   return(answer == 1 ? True : False);
}


/***********************************************************************
 *                                                                     *
 *   Give the user a message with a predefined number of answers (1, 2 *
 *   3, or 4).                                                         *
 *   Type of message can be :                                          *
 *   error (default) , information, question, working or warning.      *
 *                                                                     *
 ***********************************************************************/
int km_select(type, message, nb_ans, answ, def_ans)
   int type, nb_ans;
   char   *message;
   char   **answ;
   int     def_ans;
{
   static Widget dialog = NULL;
   static Boolean destroyed = False;
   static int    answer;
   Arg           al[20];
   Widget        mbox = 0;
   Widget        pB[4];
   int           i;
   XmString      xmstr, text, xmlist[4];
   char          cval[MAX_string];

   answer = 0;
   if (nb_ans > 4) nb_ans = 4;

   if (destroyed) {
      destroyed = False;
      km_reset_appShell (dialog);
      dialog    = NULL;
   }

   if (dialog) {
      XtDestroyWidget(dialog);
      dialog = NULL;
   }

   if (nb_ans == 4) {
       km_all_cursor(KM_WAIT_CURSOR);
       mbox = create_select4(km_toplevel, pB);
       dialog = XtParent (mbox);
   } else {
       dialog = XmCreateMessageDialog(km_toplevel, "kuipSelect", al, 0);
   }

   if (type == XmDIALOG_ERROR) {
       sprintf (cval, "%s Error", class_name);
   } else if (type == XmDIALOG_INFORMATION) {
       sprintf (cval, "%s Information", class_name);
   } else if (type == XmDIALOG_MESSAGE) {
       sprintf (cval, "%s Message", class_name);
   } else if (type == XmDIALOG_QUESTION) {
       sprintf (cval, "%s Question", class_name);
   } else if (type == XmDIALOG_WARNING) {
       sprintf (cval, "%s Warning", class_name);
   } else if (type == XmDIALOG_WORKING) {
       sprintf (cval, "%s Working", class_name);
   } else {
       sprintf (cval, "%s Message", class_name);
   }
   xmstr = XmStringCreateLtoR(cval, XmSTRING_DEFAULT_CHARSET);

   XtVaSetValues(dialog,
                 XmNdeleteResponse, XmDO_NOTHING,
                 XmNdialogStyle, XmDIALOG_APPLICATION_MODAL,
                 NULL);

   if (nb_ans < 4) {
       XtVaSetValues(dialog,
                     XmNautoUnmanage, False,
                     XmNnoResize, True,
                     XmNdialogType, (unsigned char) type,
                     XmNdialogTitle, xmstr,
                     NULL);
   } else {
       XtVaSetValues(mbox,
                     XmNautoUnmanage, False,
                     XmNnoResize, True,
                     XmNdialogType, (unsigned char) type,
                     XmNdialogTitle, xmstr,
                     NULL);
   }

   km_store_appShell (dialog, cval);
   XmStringFree(xmstr);
   XtAddCallback(dialog, XmNdestroyCallback, (XtCallbackProc)destroy_dialog,
                    &destroyed);

   text = XmStringCreateLtoR(message, XmSTRING_DEFAULT_CHARSET);
   for (i = 0; i < nb_ans; i++)
       xmlist[i]  = XmStringCreate(answ[i], XmSTRING_DEFAULT_CHARSET);
   if (nb_ans == 1) /* ok callback */ {
      XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
      XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
      XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)response, &answer);
      XtVaSetValues(dialog,
                    XmNmessageString, text,
                    XmNokLabelString, xmlist[0],
                    XmNdefaultButtonType, XmDIALOG_OK_BUTTON,
                    NULL);
   } else if (nb_ans == 2) /* ok, cancel callbacks */ {
      XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
      XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)response, &answer);
      XtAddCallback(dialog, XmNcancelCallback, (XtCallbackProc)response,
                    &answer);
      XtVaSetValues(dialog,
                    XmNmessageString, text,
                    XmNokLabelString, xmlist[0],
                    XmNcancelLabelString, xmlist[1],
                    XmNdefaultButtonType, def_ans == 1 ?
                                          XmDIALOG_OK_BUTTON :
                                          XmDIALOG_CANCEL_BUTTON,
                    NULL);
   } else if (nb_ans == 3) /* ok, cancel and help callbacks */ {
      XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)response, &answer);
      XtAddCallback(dialog, XmNcancelCallback, (XtCallbackProc)response,
                    &answer);
      XtAddCallback(dialog, XmNhelpCallback, (XtCallbackProc)response, &answer);
      XtVaSetValues(dialog,
                    XmNmessageString, text,
                    XmNokLabelString, xmlist[0],
                    XmNcancelLabelString, xmlist[1],
                    XmNhelpLabelString, xmlist[2],
                    NULL);
      if (def_ans == 1) {
          XtVaSetValues(dialog,
                        XmNdefaultButtonType, XmDIALOG_OK_BUTTON,
                        NULL);
      } else if (def_ans == 2) {
          XtVaSetValues(dialog,
                        XmNdefaultButtonType, XmDIALOG_CANCEL_BUTTON,
                        NULL);
      } else if (def_ans == 3) {
          XtVaSetValues(dialog,
                        XmNdefaultButtonType, XmDIALOG_HELP_BUTTON,
                        NULL);
      }
   } else if (nb_ans == 4) /* ok, cancel, go and help callbacks */ {
      XtUnmanageChild(XmMessageBoxGetChild(mbox, XmDIALOG_OK_BUTTON));
      XtUnmanageChild(XmMessageBoxGetChild(mbox, XmDIALOG_CANCEL_BUTTON));
      XtUnmanageChild(XmMessageBoxGetChild(mbox, XmDIALOG_HELP_BUTTON));
      XtAddCallback(pB[0], XmNactivateCallback, (XtCallbackProc)response1,
                    &answer);
      XtAddCallback(pB[1], XmNactivateCallback, (XtCallbackProc)response2,
                    &answer);
      XtAddCallback(pB[2], XmNactivateCallback, (XtCallbackProc)response3,
                    &answer);
      XtAddCallback(pB[3], XmNactivateCallback, (XtCallbackProc)response4,
                    &answer);
      XtVaSetValues(mbox,
                    XmNmessageString, text,
                    NULL);
      for (i = 0; i < nb_ans; i++)
           XtVaSetValues(pB[i], XmNlabelString, xmlist[i], NULL);
   }
   XmStringFree(text);
   for (i = 0; i < nb_ans; i++) XmStringFree(xmlist[i]);

   XtManageChild(dialog);

   while (answer == 0) {
      XtAppProcessEvent(UxAppContext, XtIMAll);
      XSync(XtDisplay(dialog), 0);
   }

   XtUnmanageChild(dialog);
   XSync(XtDisplay(dialog), 0);
   XmUpdateDisplay(dialog);
   km_reset_appShell (dialog);
   if (nb_ans == 4) km_all_cursor(KM_MAIN_CURSOR);

   return (answer);
}

/***********************************************************************
 *                                                                     *
 *   Warn the user with a message and wait till he acknowledges.       *
 *                                                                     *
 ***********************************************************************/
void km_warn_user(parent, warning, command)
   Widget parent;
   char  *warning, *command;
{
   static Widget dialog = NULL;
   static Boolean destroyed = False;
   static int    answer;
   Arg           al[5];
   int           ac;
   XmString      xmstr, text;
   char          cval[MAX_string];

   answer = 0;

   if (destroyed) {
      destroyed = False;
      km_reset_appShell (dialog);
      dialog    = NULL;
   }

   if (dialog && parent != XtParent(XtParent(dialog))) {
      XtDestroyWidget(dialog);
      dialog = NULL;
   }

   if (!dialog) {
      ac = 0;
      XtSetArg(al[ac], XmNautoUnmanage, False); ac++;
      XtSetArg(al[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;
      sprintf (cval, "%s Warning", class_name);
      xmstr = XmStringCreateLtoR(cval, XmSTRING_DEFAULT_CHARSET);
      XtSetArg(al[ac], XmNdialogTitle, xmstr); ac++;
      XtSetArg(al[ac], XmNnoResize, True); ac++;
      XtSetArg(al[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
      dialog = XmCreateWarningDialog(parent, "warningDialog", al, ac);
      km_store_appShell (dialog, cval);
      XtUnmanageChild(XmMessageBoxGetChild(dialog,
                     XmDIALOG_CANCEL_BUTTON));
      XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)response, &answer);
      XtAddCallback(dialog, XmNdestroyCallback, (XtCallbackProc)destroy_dialog,
                    &destroyed);
      XmStringFree(xmstr);
   }

   XtRemoveAllCallbacks(dialog, XmNhelpCallback);
   if (strlen(command) == 0)
      XtUnmanageChild(XmMessageBoxGetChild(dialog,
                     XmDIALOG_HELP_BUTTON));
   else {
      XtManageChild(XmMessageBoxGetChild(dialog,
                   XmDIALOG_HELP_BUTTON));
      XtAddCallback(dialog, XmNhelpCallback,
                    (XtCallbackProc)km_help_cb, command);
   }

   text = XmStringCreateLtoR(warning, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(dialog,
          XmNmessageString, text,
          NULL);
   XmStringFree(text);
   XtManageChild(dialog);

   while (answer == 0) {
      XtAppProcessEvent(UxAppContext, XtIMAll);
      XSync(XtDisplay(dialog), 0);
   }

   XtUnmanageChild(dialog);
   XSync(XtDisplay(dialog), 0);
   XmUpdateDisplay(dialog);
   km_reset_appShell (dialog);
}

/***********************************************************************
 *                                                                     *
 *   Inform the user with a message and wait till he acknowledges.     *
 *                                                                     *
 ***********************************************************************/
void km_inform_user(parent, warning, command)
   Widget parent;
   char  *warning, *command;
{
   static Widget dialog = NULL;
   static Boolean destroyed = False;
   static int    answer;
   Arg           al[5];
   int           ac;
   XmString      xmstr, text;
   char          cval[MAX_string];

   answer = 0;

   if (destroyed) {
      destroyed = False;
      km_reset_appShell (dialog);
      dialog    = NULL;
   }

   if (dialog && parent != XtParent(XtParent(dialog))) {
      XtDestroyWidget(dialog);
      dialog = NULL;
   }

   if (!dialog) {
      ac = 0;
      XtSetArg(al[ac], XmNautoUnmanage, False); ac++;
      XtSetArg(al[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;
      sprintf (cval, "%s Information", class_name);
      xmstr = XmStringCreateLtoR(cval, XmSTRING_DEFAULT_CHARSET);
      XtSetArg(al[ac], XmNdialogTitle, xmstr); ac++;
      XtSetArg(al[ac], XmNnoResize, True); ac++;
      XtSetArg(al[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
      dialog = XmCreateInformationDialog(parent, "informationDialog", al, ac);
      km_store_appShell (dialog, cval);
      XtUnmanageChild(XmMessageBoxGetChild(dialog,
                     XmDIALOG_CANCEL_BUTTON));
      XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)response, &answer);
      XtAddCallback(dialog, XmNdestroyCallback, (XtCallbackProc)destroy_dialog,
                    &destroyed);
      XmStringFree(xmstr);
   }

   XtRemoveAllCallbacks(dialog, XmNhelpCallback);
   if (strlen(command) == 0)
      XtUnmanageChild(XmMessageBoxGetChild(dialog,
                     XmDIALOG_HELP_BUTTON));
   else {
      XtManageChild(XmMessageBoxGetChild(dialog,
                   XmDIALOG_HELP_BUTTON));
      XtAddCallback(dialog, XmNhelpCallback,
                    (XtCallbackProc)km_help_cb, command);
   }

   text = XmStringCreateLtoR(warning, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(dialog,
          XmNmessageString, text,
          NULL);
   XmStringFree(text);
   XtManageChild(dialog);

   while (answer == 0) {
      XtAppProcessEvent(UxAppContext, XtIMAll);
      XSync(XtDisplay(dialog), 0);
   }

   XtUnmanageChild(dialog);
   XSync(XtDisplay(dialog), 0);
   XmUpdateDisplay(dialog);
   km_reset_appShell (dialog);
}

/***********************************************************************
 *                                                                     *
 *   Get user input for the prompt dialog.                             *
 *                                                                     *
 ***********************************************************************/
static void get_input(w, answer, cbs)
   Widget   w;
   int     *answer;
   XmSelectionBoxCallbackStruct *cbs;
{
   char  *text;

   if (cbs->reason == XmCR_OK) {
      text = km_normal_string(cbs->value);
      *answer = strlen(text);
      if (*answer == 0) *answer = 1; /* empty string */
      XtFree(text);
   } else if (cbs->reason == XmCR_CANCEL) {
      *answer = -1;
   }
}


/***********************************************************************
 *                                                                     *
 *   Create prompt dialog and wait till the user press OK or CANCEL    *
 *   Return the answer for OK or NULL string for CANCEL.               *
 *                                                                     *
 ***********************************************************************/

static char *prompt_dialog (prompt, init_string, cancel_flag)
     char *prompt, *init_string;
     int cancel_flag;
{
   static Widget  dialog = NULL;
   static Boolean destroyed = False;
   static int     answer;
   Arg            al[20];
   int            ac;
   XmString       xmstr1;
   XmString       xmstr2 = NULL;
   XmString       xmstr3 = NULL;
   char           cval[MAX_string];

   answer = 0;

   if (destroyed) {
      destroyed = False;
      km_reset_appShell (dialog);
      dialog    = NULL;
   }

   if (dialog) {
      XtDestroyWidget(dialog);
      dialog = NULL;
   }

   ac = 0;
   XtSetArg(al[ac], XmNautoUnmanage, False); ac++;
   XtSetArg(al[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;
   sprintf (cval, "%s Prompt", class_name);
   xmstr1 = XmStringCreateLtoR(cval, XmSTRING_DEFAULT_CHARSET);
   XtSetArg(al[ac], XmNdialogTitle, xmstr1); ac++;
   XtSetArg(al[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
   if (prompt != NULL) {
       xmstr2 = XmStringCreateLtoR(prompt, XmSTRING_DEFAULT_CHARSET);
       XtSetArg(al[ac], XmNselectionLabelString, xmstr2); ac++;
   }
   if (init_string != NULL) {
       xmstr3 = XmStringCreateLtoR(init_string, XmSTRING_DEFAULT_CHARSET);
       XtSetArg(al[ac], XmNtextString, xmstr3); ac++;
   }
   XtSetArg(al[ac], XmNtextColumns, 40); ac++;
   dialog = XmCreatePromptDialog(km_toplevel, "promptDialog", al, ac);
   km_store_appShell (dialog, cval);
   XmStringFree(xmstr1);
   if (xmstr2 != NULL)
       XmStringFree(xmstr2);
   if (xmstr3 != NULL)
       XmStringFree(xmstr3);
   XtUnmanageChild(XmSelectionBoxGetChild(dialog,
                   XmDIALOG_HELP_BUTTON));
   if (cancel_flag == 0) /* False */
       XtUnmanageChild(XmSelectionBoxGetChild(dialog,
                      XmDIALOG_CANCEL_BUTTON));
   XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)get_input, &answer);
   if (cancel_flag != 0) /* True */
       XtAddCallback(dialog, XmNcancelCallback,
                     (XtCallbackProc)get_input, &answer);
   XtAddCallback(dialog, XmNdestroyCallback, (XtCallbackProc)destroy_dialog,
                 &destroyed);

   XtManageChild(dialog);

   while (answer == 0) {
      XtAppProcessEvent(UxAppContext, XtIMAll);
      XSync(XtDisplay(dialog), 0);
   }

   XtUnmanageChild(dialog);
   XSync(XtDisplay(dialog), 0);
   XmUpdateDisplay(dialog);
   km_reset_appShell (dialog);

   if (answer > 0) {
      XmString text;
      char *t;

      XtVaGetValues(dialog, XmNtextString, &text, NULL);
      if( XmStringGetLtoR(text, XmSTRING_DEFAULT_CHARSET, &t) ) {
        char *rep = strdup(t);
        XtFree(t);
        XmStringFree(text);
        return rep;
      }
      XmStringFree(text);
      return strdup( "" );
   }

   return NULL;
}

char *km_prompt (prompt, init_string)
     char *prompt, *init_string;
{
     return prompt_dialog (prompt, init_string, 1);
}

char *km_prompt_answer (prompt, init_string)
     char *prompt, *init_string;
{
     return prompt_dialog (prompt, init_string, 0);
}

/***********************************************************************
 *                                                                     *
 *   Get the passwd keystroke by keystroke and replace the typed       *
 *   characters by *'s.                                                *
 *                                                                     *
 ***********************************************************************/
void check_passwd(text_w, passwd, cbs)
Widget        text_w;
char         *passwd;
XmTextVerifyCallbackStruct *cbs;
{
    int len;

    if (cbs->text->ptr == NULL) { /* backspace */
        cbs->endPos = strlen(passwd); /* delete from here to end */
        passwd[cbs->startPos] = 0; /* backspace--terminate */
        return;
    }

    if (cbs->text->length > 1) {
        cbs->doit = False; /* don't allow "paste" operations */
        return; /* make the user *type* the password! */
    }

    strncat(passwd, cbs->text->ptr, cbs->text->length);
    passwd[cbs->endPos + cbs->text->length] = 0;

    for (len = 0; len < cbs->text->length; len++)
        cbs->text->ptr[len] = '*';
}

/***********************************************************************
 *                                                                     *
 *   Create password prompt dialog and wait till the user presses OK   *
 *   or CANCEL. This routine is identical to km_prompt except that it  *
 *   replaces the typed characters by *'s.                             *
 *   Return the answer for OK or NULL string for CANCEL.               *
 *                                                                     *
 ***********************************************************************/
char *km_passwd_prompt (prompt)
     char *prompt;
{
   static Widget  dialog = NULL;
   static Boolean destroyed = False;
   static int     answer;
   static char    passwd[MAX_string];
   Widget         text_w;
   Arg            al[20];
   int            ac;
   XmString       xmstr1;
   XmString       xmstr2 = NULL;
   char           cval[MAX_string];

   answer    = 0;
   passwd[0] = '\0';

   if (destroyed) {
      destroyed = False;
      km_reset_appShell (dialog);
      dialog    = NULL;
   }

   if (dialog) {
      XtDestroyWidget(dialog);
      dialog = NULL;
   }

   ac = 0;
   XtSetArg(al[ac], XmNautoUnmanage, False); ac++;
   XtSetArg(al[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;
   sprintf (cval, "%s Password Prompt", class_name);
   xmstr1 = XmStringCreateLtoR(cval, XmSTRING_DEFAULT_CHARSET);
   XtSetArg(al[ac], XmNdialogTitle, xmstr1); ac++;
   XtSetArg(al[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
   if (prompt != NULL) {
       xmstr2 = XmStringCreateLtoR(prompt, XmSTRING_DEFAULT_CHARSET);
       XtSetArg(al[ac], XmNselectionLabelString, xmstr2); ac++;
   }

   XtSetArg(al[ac], XmNtextColumns, 40); ac++;
   dialog = XmCreatePromptDialog(km_toplevel, "promptDialog", al, ac);
   km_store_appShell (dialog, cval);
   XmStringFree(xmstr1);
   if (xmstr2 != NULL)
       XmStringFree(xmstr2);
   XtUnmanageChild(XmSelectionBoxGetChild(dialog,
                   XmDIALOG_HELP_BUTTON));
   XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)get_input, &answer);
   XtAddCallback(dialog, XmNcancelCallback, (XtCallbackProc)get_input, &answer);
   XtAddCallback(dialog, XmNdestroyCallback, (XtCallbackProc)destroy_dialog,
                 &destroyed);

   /* get the text widget and install modify callback */
   text_w = XmSelectionBoxGetChild(dialog, XmDIALOG_TEXT);
   XtAddCallback(text_w, XmNmodifyVerifyCallback, (XtCallbackProc)check_passwd,
                 passwd);

   XtManageChild(dialog);

   while (answer == 0) {
      XtAppProcessEvent(UxAppContext, XtIMAll);
      XSync(XtDisplay(dialog), 0);
   }

   XtUnmanageChild(dialog);
   XSync(XtDisplay(dialog), 0);
   XmUpdateDisplay(dialog);
   km_reset_appShell (dialog);

   if (answer > 0) {
      return strdup(passwd);
   }

   return NULL;
}
