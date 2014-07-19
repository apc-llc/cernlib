/*
 * $Id: dialogs.c,v 1.1.1.1 1996/03/08 15:33:03 mclareni Exp $
 *
 * $Log: dialogs.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:03  mclareni
 * Kuip
 *
 */
/*CMZ :  2.04/00 08/10/93  09.47.03  by  Nicole Cremel*/
/*-- Author :    Fons Rademakers   8/01/93*/
#include "kuip/kuip.h"

#include <stdio.h>
#include <string.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/DialogS.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>

extern XtAppContext     UxAppContext;

extern void helpPNBCallback(
#ifndef _NO_PROTO
                        Widget, char *, XmAnyCallbackStruct *
#endif
                       );


/***********************************************************************
 *                                                                     *
 *   Get a response for the dialog.                                    *
 *                                                                     *
 ***********************************************************************/
static void response( w, answer, cbs )
     Widget w;
     int *answer;
     XmAnyCallbackStruct *cbs;
{
   if (cbs->reason == XmCR_OK)
      *answer = 1;
   else if (cbs->reason == XmCR_HELP)
      *answer = 2;
   else if (cbs->reason == XmCR_CANCEL)
      *answer = 3;
}

/***********************************************************************
 *                                                                     *
 *   Sets the destroyed flag for the dialog.                           *
 *                                                                     *
 ***********************************************************************/
static void destroy_dialog( w, destroyed, cbs )
     Widget w;
     Boolean *destroyed;
     XmAnyCallbackStruct *cbs;
{
   *destroyed = True;
}

/***********************************************************************
 *                                                                     *
 *   Ask the user a question and wait for the answer.                  *
 *                                                                     *
 ***********************************************************************/
Boolean ask_user( parent, question, ans1, ans2, default_ans )
     Widget parent;
     char *question;
     char *ans1;
     char *ans2;
     int default_ans;
{
   static Widget  dialog = NULL;
   static Boolean destroyed = False;
   static int     answer;
   Arg            al[5];
   int            ac;
   XmString       xmstr, text, yes, no;

   answer    = 0;

   /*
    *  check if the dialog has been destroyed (because its parent
    *  was destroyed)
    */
   if (destroyed) {
      destroyed = False;
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
      xmstr = XmStringCreateLtoR("Question", XmSTRING_DEFAULT_CHARSET);
      XtSetArg(al[ac], XmNdialogTitle, xmstr); ac++;
      XtSetArg(al[ac], XmNnoResize, True); ac++;
      XtSetArg(al[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
      dialog = XmCreateQuestionDialog(parent, "questionDialog", al, ac);
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

   return(answer == 1 ? True : False);
}

/***********************************************************************
 *                                                                     *
 *   Warn the user and wait till he acknowledges.                      *
 *                                                                     *
 ***********************************************************************/
void warn_user( parent, warning )
     Widget parent;
     char *warning;
{
   static Widget  dialog = NULL;
   static Boolean destroyed = False;
   static int     answer;
   Arg            al[5];
   int            ac;
   XmString       xmstr, text;

   answer = 0;

   if (destroyed) {
      destroyed = False;
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
      xmstr = XmStringCreateLtoR("Warning", XmSTRING_DEFAULT_CHARSET);
      XtSetArg(al[ac], XmNdialogTitle, xmstr); ac++;
      XtSetArg(al[ac], XmNnoResize, True); ac++;
      XtSetArg(al[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
      dialog = XmCreateWarningDialog(parent, "warningDialog", al, ac);
      XtUnmanageChild(XmMessageBoxGetChild(dialog,
                                           XmDIALOG_CANCEL_BUTTON));
      XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)response, &answer);
      XtAddCallback(dialog, XmNdestroyCallback, (XtCallbackProc)destroy_dialog,
                    &destroyed);
      XmStringFree(xmstr);
   }

   XtRemoveAllCallbacks(dialog, XmNhelpCallback);
   XtUnmanageChild(XmMessageBoxGetChild(dialog,
                                        XmDIALOG_HELP_BUTTON));

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
}

/***********************************************************************
 *                                                                     *
 *   Inform the user and wait till he acknowledges.                    *
 *                                                                     *
 ***********************************************************************/
void inform_user( parent, warning )
     Widget parent;
     char *warning;
{
   static Widget  dialog = NULL;
   static Boolean destroyed = False;
   static int     answer;
   Arg            al[5];
   int            ac;
   XmString       xmstr, text;

   answer = 0;

   if (destroyed) {
      destroyed = False;
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
      xmstr = XmStringCreateLtoR("Information", XmSTRING_DEFAULT_CHARSET);
      XtSetArg(al[ac], XmNdialogTitle, xmstr); ac++;
      XtSetArg(al[ac], XmNnoResize, True); ac++;
      XtSetArg(al[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
      dialog = XmCreateInformationDialog(parent, "informationDialog", al, ac);
      XtUnmanageChild(XmMessageBoxGetChild(dialog,
                                           XmDIALOG_CANCEL_BUTTON));
      XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)response, &answer);
      XtAddCallback(dialog, XmNdestroyCallback, (XtCallbackProc)destroy_dialog,
                    &destroyed);
      XmStringFree(xmstr);
   }

   XtRemoveAllCallbacks(dialog, XmNhelpCallback);
   XtUnmanageChild(XmMessageBoxGetChild(dialog,
                                        XmDIALOG_HELP_BUTTON));

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
}


/***********************************************************************
 *                                                                     *
 *   Prompt the user and wait till he acknowledges.                    *
 *                                                                     *
 ***********************************************************************/
char *prompt_user( parent, prompt, default_line, help_item)
     Widget parent;
     char *prompt, *default_line, *help_item;
{
   static Widget  dialog = NULL;
   static Boolean destroyed = False;
   static int     answer;
   Arg            al[5];
   int            ac;
   XmString       xmstr, input_string;

   char *input_text = NULL;

   answer = 0;

   if (destroyed) {
      destroyed = False;
      dialog    = NULL;
   }

   if (dialog) {
      XtDestroyWidget(dialog);
      dialog = NULL;
   }

   if (!dialog) {
      ac = 0;
      XtSetArg(al[ac], XmNautoUnmanage, False); ac++;
      XtSetArg(al[ac], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); ac++;
      xmstr = XmStringCreateLtoR("Prompt", XmSTRING_DEFAULT_CHARSET);
      XtSetArg(al[ac], XmNdialogTitle, xmstr); ac++;
      XtSetArg(al[ac], XmNnoResize, True); ac++;
      XtSetArg(al[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
      XtSetArg (al[ac],XmNselectionLabelString,
                XmStringCreateLtoR(prompt,XmSTRING_DEFAULT_CHARSET)); ac++;
      if (strlen(default_line) != 0) {
          XtSetArg (al[ac],XmNtextString,
             XmStringCreateLtoR(default_line, XmSTRING_DEFAULT_CHARSET)); ac++;
      }
      dialog = XmCreatePromptDialog(parent, "promptDialog", al, ac);
      XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)response,
                    &answer);
      XtAddCallback(dialog, XmNcancelCallback,
                    (XtCallbackProc)response, &answer);
      XtAddCallback(dialog,XmNhelpCallback,
                     (XtCallbackProc)helpPNBCallback,(XtPointer)help_item);
      XtAddCallback(dialog, XmNdestroyCallback, (XtCallbackProc)destroy_dialog,
                    &destroyed);
      XmStringFree(xmstr);
   }

   XtManageChild(dialog);

   while (answer == 0) {
      XtAppProcessEvent(UxAppContext, XtIMAll);
      XSync(XtDisplay(dialog), 0);
   }

   XtUnmanageChild(dialog);
   XSync(XtDisplay(dialog), 0);
   XmUpdateDisplay(dialog);

   if (answer == 3) {
       return (NULL);
   } else {
       ac = 0;
       XtSetArg (al[ac], XmNtextString, &input_string); ac++;
       XtGetValues (dialog, al, ac);
       XmStringGetLtoR(input_string, XmSTRING_DEFAULT_CHARSET, &input_text);
       if( input_text == NULL || input_text[0] == '\0' )
           return NULL;
       else
           return (strdup(input_text));
   }

}
