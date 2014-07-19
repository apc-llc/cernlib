/*
 * $Id: help.c,v 1.1.1.1 1996/03/08 15:33:04 mclareni Exp $
 *
 * $Log: help.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:04  mclareni
 * Kuip
 *
 */
/*CMZ :  2.04/02 27/10/93  15.09.43  by  Fons Rademakers*/
/*-- Author :    Fons Rademakers   16/03/93*/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/Form.h>

#include "uxxt.h"

extern Widget   kuipIo;

Widget help[3];


char *help_text[3] = {"\
          Kxterm -- Kuip Terminal Emulator\n\n\
This terminal emulator combines the best features from\n\
the (now defunct) Apollo DM pads (like: input and transcript\n\
pads, automatic file backup of transcript pad, string\n\
search in pads, etc.) and the Korn shell emacs-style\n\
command line editing and command line recall mechanism.\n\n\
Commands are typed in the input pad behind the application prompt.\n\
Via the toggle buttons labeled \"H\" the input and/or transcript\n\
pad can be placed in hold mode. In hold mode one can paste or type\n\
a number of commands into the input pad and edit them without sending\n\
the commands to the application. Releasing the hold button\n\
will causes Kxterm to submit all lines, upto the line containing the\n\
cursor, to the application. To submit the lines below the cursor,\n\
just move the cursor down. In this way one can still edit the\n\
lines just before they are being submitted to the application.\n\n\
Commands can be edited in the input pad using emacs-like key\n\
sequences. See help \"On Edit Keys\".\n\n\
The transcript pad shows the executed commands and command\n\
output. When in hold mode the transcript pad does not scroll to\n\
make the new text visible.\n\n\
Below follows a description of the different Kxterm menus:\n\
   File\n\
      About Kxterm...\n\
         Displays version information about Kxterm.\n\
      About <Application>...\n\
         Displays version information about the application\n\
         Kxterm is servicing.\n\
      Save Transcript\n\
         Write the contents of the transcript pad to the current\n\
         file. If there is no current file a file selection box\n\
         will appear.\n\
      Save Transcript As...\n\
         Write the contents of the transcript pad to a user-specified\n\
         file.\n\
      Print...\n\
         Print the contents of the transcript pad (not yet implemented).\n\
      Kill\n\
         Send a SIGINT signal to the application to cause it to\n\
         core dump. This is useful when the application is hanging or\n\
         blocked. Use only in emergency situations.\n\
      Exit\n\
         Exit Kxterm and the application.\n\n\
   Edit\n\
      Cut\n\
         Remove the selected text. The selected text is written to the\n\
         Cut & Paste buffer. Using the \"Paste\" function, it can be\n\
         written to any X11 program. In the transcript pad \"Cut\"\n\
         defaults to the \"Copy\" function.\n\
      Copy\n\
         Copy the selected text. The selected text is written to the\n\
         Cut & Paste buffer. Using the \"Paste\" function, it can be\n\
         written to any X11 program.\n\
      Paste\n\
         Insert text from the Cut & Paste buffer at the cursor location\n\
         into the input pad.\n\
      Search...\n\
         Search for a text string in the transcript pad.\n\n\
   View\n\
      Show Input\n\
         Show in a window all commands entered via the input pad.\n\n\
   Options\n\
      Clear Transcript Pad\n\
         Clear all text off of the top of the transcript pad.\n\
      Echo Command\n\
         Echo executed commands in transcript pad.\n\
      Timing\n\
         Report command execution time (real and CPU time).\n\
      Iconify\n\
         Iconify Kxterm and all windows of the application.\n\n\
   Help\n\
      On Kxterm\n\
         The help you are currently reading.\n\
      On Edit Keys\n\
         Help on the emacs-style edit key sequences.\n\n\
All Kxterm menu's may be dynamically extended by the application.\n\
See for help on these new menu items the application help.\n\n\
",

"\
          Kxterm -- X Resources\n\n\
This is a list of X resources available to Kxterm.\n\n\
Users can specify their own values for these resources in the standard\n\
X/Motif way (via their own .Xdefaults file or, system wide, via a file\n\
in the /usr/lib/X11/app-defaults directory). One just has to prefix the\n\
desired resource by the class name of the application preceded by \"Kx\".\n\
So for application \"App\" the Kxterm resource class is \"KxApp\".\n\n\
Any default value is given behind the resource name.\n\n\
 *background:\n\n\
Specify the background color for all windows.\n\n\
 *foreground:\n\n\
Specify the foreground color for all windows.\n\n\
 *outputForm.height:        205\n\n\
Height of the transcript pad (in pixels). This will scale the size of the\n\
input pad accordingly.\n\n\
 *outputText*...:\n\n\
All resources of the transcript pad text widget.\n\n\
 *inputText*...:\n\n\
All resource of the input pad text widget.\n\n\
",

"\
          Kxterm -- Edit Key Sequences\n\n\
\"C-b\" means holding down the Control key and pressing the b key.\n\
\"M-\" stands for the Meta key and \"A-\" for the Alt key.\n\n\
C-b:              backward character\n\
A-b:              backward word\n\
M-b:              backward word\n\
Shift A-b:        backward word, extend selection\n\
Shift M-b:        backward word, extend selection\n\
A-[:              backward paragraph\n\
M-[:              backward paragraph\n\
Shift A-[:        backward paragraph, extend selection\n\
Shift M-[:        backward paragraph, extend selection\n\
A-<:              beginning of file\n\
M-<:              beginning of file\n\
C-a:              beginning of line\n\
Shift C-a:        beginning of line, extend selection\n\
C-osfInsert:      copy to clipboard\n\
Shift osfDelete:  cut to clipboard\n\
Shift osfInsert:  paste from clipboard\n\
Alt->:            end of file\n\
M->:              end of file\n\
C-e:              end of line\n\
Shift C-e:        end of line, extend selection\n\
C-f:              forward character\n\
A-]:              forward paragraph\n\
M-]:              forward paragraph\n\
Shift A-]:        forward paragraph, extend selection\n\
Shift M-]:        forward paragraph, extend selection\n\
C-A-f:            forward word\n\
C-M-f:            forward word\n\
C-d:              kill next character\n\
A-BS:             kill previous word\n\
M-BS:             kill previous word\n\
C-w:              kill region\n\
C-y:              yank back last thing killed\n\
C-k:              kill to end of line\n\
C-u:              kill line\n\
A-DEL:            kill to start of line\n\
M-DEL:            kill to start of line\n\
C-o:              newline and backup\n\
C-j:              newline and indent\n\
C-n:              get next command, in hold mode: next line\n\
C-osfLeft:        page left\n\
C-osfRight:       page right\n\
C-p:              get previous command, in hold mode: previous line\n\
C-g:              process cancel\n\
C-l:              redraw display\n\
C-osfDown:        next page\n\
C-osfUp:          previous page\n\
C-SPC:            set mark here\n\
C-c:              send kill signal to application\n\
C-h:              toggle hold button of pad containing input focus\n\
F8:               re-execute last executed command\n\
Shift F8:         put last executed command in input pad\n\
Shift-TAB:        change input focus\n\
"
};


/***********************************************************************
 *                                                                     *
 *   Destroy the HELP dialog.                                          *
 *                                                                     *
 ***********************************************************************/
static void destroy_help(w, dialog, call_data)
   Widget w;
   Widget dialog;
   XmAnyCallbackStruct *call_data;
{
    if (dialog == help[0])
       help[0] = NULL;
    else if (dialog == help[1])
       help[1] = NULL;
    else
       help[2] = NULL;

    UxDestroyInterface(dialog);
}

/***********************************************************************
 *                                                                     *
 *   Show the help text in a dialog.                                   *
 *                                                                     *
 ***********************************************************************/
static Widget show_help(parent, text, title)
   Widget   parent;
   char    *text;
   char    *title;
{
   Widget  help_shell;
   Widget  help_form;
   Widget  ok_button;
   Widget  sep;
   Widget  help_sw;
   Widget  text_w;

   help_shell = XtVaCreatePopupShell("help_shell",
                        xmDialogShellWidgetClass, parent,
                        XmNtitle, title,
                        XmNdeleteResponse, XmDESTROY,
                        NULL);

   help_form = XtVaCreateWidget("help_form",
                        xmFormWidgetClass, help_shell,
                        XmNautoUnmanage, FALSE,
                        XmNheight, 350,
                        XmNwidth, 635,
                        NULL);

   ok_button = XtVaCreateManagedWidget("ok_button",
                        xmPushButtonGadgetClass, help_form,
                        XmNdefaultButtonShadowThickness, 1,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNleftPosition, 50,
                        XmNleftOffset, -30,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshowAsDefault, 1,
                        RES_CONVERT(XmNlabelString, "OK"),
                        XmNheight, 30,
                        XmNwidth, 60,
                        NULL);

   sep = XtVaCreateManagedWidget("sep",
                        xmSeparatorGadgetClass, help_form,
                        XmNbottomWidget, ok_button,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        NULL);

   help_sw = XtVaCreateManagedWidget("help_sw",
                        xmScrolledWindowWidgetClass, help_form,
                        XmNbottomWidget, sep,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNshadowThickness, 0,
                        NULL);

   text_w = XtVaCreateManagedWidget("text_w",
                        xmTextWidgetClass, help_sw,
                        XmNeditMode, XmMULTI_LINE_EDIT ,
                        XmNeditable, FALSE,
                        XmNvalue, text,
                        NULL);

   XtVaSetValues(help_form, XmNdefaultButton, ok_button, NULL);

   XtAddCallback(ok_button, XmNactivateCallback,
                 (XtCallbackProc)destroy_help, help_shell);
   XtAddCallback(help_shell, XmNdestroyCallback,
                 (XtCallbackProc)destroy_help, help_shell);

   UxPopupInterface(help_shell, XtGrabNone);

   return(help_shell);
}

/***********************************************************************
 *                                                                     *
 *   Callback called by the help menu items.                           *
 *                                                                     *
 ***********************************************************************/
void help_cb(w, type, call_data)
    Widget   w;
    int     *type;
    XmAnyCallbackStruct *call_data;
{
   int t = (int)type;

   if (help[t])
      XRaiseWindow(XtDisplay(help[t]), XtWindow(help[t]));
   else {
      if (t == 0)
         help[t] = show_help(kuipIo, help_text[t], "Help on: Kxterm");
      else if (t == 1)
         help[t] = show_help(kuipIo, help_text[t], "Help on: Kxterm Resources");
      else
         help[t] = show_help(kuipIo, help_text[t], "Help on: Edit Keys");
   }
}

