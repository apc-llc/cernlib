/*
 * $Id: mail.c,v 1.2 1996/10/18 08:13:16 couet Exp $
 *
 * $Log: mail.c,v $
 * Revision 1.2  1996/10/18 08:13:16  couet
 * - #include <Xm/ToggleBG.h> was missing
 *
 * Revision 1.1.1.1  1996/03/01 11:38:57  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.05/18 12/09/94  13.50.40  by  Fons Rademakers*/
/*-- Author :    Fons Rademakers   31/08/94*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/Text.h>
#if defined(CERNLIB_UNIX)
#include <pwd.h>
#endif

#include "hmotif/pawm.h"

/* extern functions */
extern Widget create_mailDialog(Widget);
extern char *buginfo(void);
extern int br_mail_motif(char, char *, char *, char *, char *, char *);
extern void km_warn_user(Widget, char *, char *);

/* extern widgets */
extern Widget  km_toplevel;
extern Widget  mailMailButton;
extern Widget  cancelMailButton;
extern Widget  helpMailButton;
extern Widget  versionMailLabel;
extern Widget  bugMailToggle;
extern Widget  commentMailToggle;
extern Widget  userMailText;
extern Widget  instituteMailText;
extern Widget  phoneMailText;
extern Widget  emailMailText;
extern Widget  bugMailText;

/* extern data */
extern char *br_tmpl_bugrep[];
extern char *br_tmpl_comment[];

/* local functions */
static void handle_toggle(Widget, XtPointer, XmToggleButtonCallbackStruct *);
static void send_mail(Widget, XtPointer, XmAnyCallbackStruct *);

/* local data */
static Widget  mailDialog;

/* emacs translations for the bugMailText widget */
static char *translation_table =
  "Ctrl <Key>b:            backward-character()\n\
   Alt <Key>b:             backward-word()\n\
   Meta <Key>b:            backward-word()\n\
   Shift Alt <Key>b:       backward-word(extend)\n\
   Shift Meta <Key>b:      backward-word(extend)\n\
   Alt <Key>[:             backward-paragraph()\n\
   Meta <Key>[:            backward-paragraph()\n\
   Shift Alt <Key>[:       backward-paragraph(extend)\n\
   Shift Meta <Key>[:      backward-paragraph(extend)\n\
   Alt <Key><:             beginning-of-file()\n\
   Meta <Key><:            beginning-of-file()\n\
   Ctrl <Key>a:            beginning-of-line()\n\
   Shift Ctrl <Key>a:      beginning-of-line(extend)\n\
   Ctrl <Key>osfInsert:    copy-clipboard()\n\
   Shift <Key>osfDelete:   cut-clipboard()\n\
   Shift <Key>osfInsert:   paste-clipboard()\n\
   Alt <Key>>:             end-of-file()\n\
   Meta <Key>>:            end-of-file()\n\
   Ctrl <Key>e:            end-of-line()\n\
   Shift Ctrl <Key>e:      end-of-line(extend)\n\
   Ctrl <Key>f:            forward-character()\n\
   Alt <Key>]:             forward-paragraph()\n\
   Meta <Key>]:            forward-paragraph()\n\
   Shift Alt <Key>]:       forward-paragraph(extend)\n\
   Shift Meta <Key>]:      forward-paragraph(extend)\n\
   Ctrl Alt <Key>f:        forward-word()\n\
   Ctrl Meta <Key>f:       forward-word()\n\
   Ctrl <Key>d:            kill-next-character()\n\
   Alt <Key>BackSpace:     kill-previous-word()\n\
   Meta <Key>BackSpace:    kill-previous-word()\n\
   Ctrl <Key>w:            key-select() kill-selection()\n\
   Ctrl <Key>y:            unkill()\n\
   Ctrl <Key>k:            kill-to-end-of-line()\n\
   Ctrl <Key>u:            beginning-of-line() kill-to-end-of-line()\n\
   Alt <Key>Delete:        kill-to-start-of-line()\n\
   Meta <Key>Delete:       kill-to-start-of-line()\n\
   Ctrl <Key>o:            newline-and-backup()\n\
   Ctrl <Key>j:            newline-and-indent()\n\
   Ctrl <Key>n:            next-line()\n\
   Ctrl <Key>osfLeft:      page-left()\n\
   Ctrl <Key>osfRight:     page-right()\n\
   Ctrl <Key>p:            previous-line()\n\
   Ctrl <Key>g:            process-cancel()\n\
   Ctrl <Key>l:            redraw-display()\n\
   Ctrl <Key>osfDown:      next-page()\n\
   Ctrl <Key>osfUp:        previous-page()\n\
   <Key>osfDown:           next-line()\n\
   <Key>osfUp:             previous-line()\n\
   Ctrl <Key>space:        set-anchor()";


/***********************************************************************
 *                                                                     *
 *   Show (manage) the mail dialog.                                    *
 *                                                                     *
 ***********************************************************************/
void show_mailDialog(Widget parent)
{
   XmToggleButtonCallbackStruct cbs;

   if (!mailDialog) {

      mailDialog = create_mailDialog(parent);

      /*
       *   Install close callback
       */
      InstallMwmCloseCallback(XtParent(mailDialog),
                              (XtCallbackProc)close_cb,
                              (XtPointer)mailDialog);

      /*
       *   Override translation table for bugMailText
       */
      XtOverrideTranslations(bugMailText,
                             XtParseTranslationTable(translation_table));

      /*
       * Install callback on toggle buttons
       */
      XtAddCallback(bugMailToggle, XmNvalueChangedCallback,
                    (XtCallbackProc)handle_toggle, NULL);
      XtAddCallback(commentMailToggle, XmNvalueChangedCallback,
                    (XtCallbackProc)handle_toggle, NULL);

      /*
       *   Install callback on buttons
       */
      XtAddCallback(mailMailButton, XmNactivateCallback,
                    (XtCallbackProc)send_mail, NULL);
      XtAddCallback(cancelMailButton, XmNactivateCallback,
                    (XtCallbackProc)close_cb, mailDialog);
      XtAddCallback(helpMailButton, XmNactivateCallback,
                    (XtCallbackProc)help_cb, "BUGREPORT");

      /*
       *    Get PAW version string to display in dialog
       */
      XtVaSetValues(versionMailLabel,
                    RES_CONVERT(XmNlabelString, buginfo()), NULL);

#if defined(CERNLIB_UNIX)
      {
         char  *name;

         name = XmTextGetString(userMailText);

         /*
          *  Try to fill in some of the fields for the user
          */
         if (strempty(name)) {
            struct passwd *pw;
            char gecos[128], *s;

            pw = getpwuid(getuid());
            strcpy(gecos, pw->pw_gecos);
            s = strchr(gecos, ',');
            if (s) *s = '\0';
            XmTextSetString(userMailText, gecos);
         }

         XtFree(name);
      }
#endif
   }

   /*
    *     Put bug or comment template depending on the default setting
    */
   cbs.reason = 0;
   cbs.event  = (XEvent *) NULL;
   cbs.set    = 1;
   if (XmToggleButtonGadgetGetState(bugMailToggle))
      XtCallCallbacks(bugMailToggle, XmNvalueChangedCallback, &cbs);
   else
      XtCallCallbacks(commentMailToggle, XmNvalueChangedCallback, &cbs);

   UxPopupInterface(mailDialog, XtGrabNone);

   /*
    * If the dialog was already open, UxPopupInterface does nothing.  In
    * this case, at least make sure the window is raised to the top
    * of the window tree (or as high as it can get).
    */
   XMapRaised(XtDisplay(mailDialog), XtWindow(XtParent(mailDialog)));
}

/***********************************************************************
 *                                                                     *
 *   Handle mail type toggle button changes.                           *
 *                                                                     *
 ***********************************************************************/
static void handle_toggle(Widget w, XtPointer cd,
                          XmToggleButtonCallbackStruct *cbs)
{
   char **s;
   XmTextPosition pos = 0;

   if (!cbs->set) return;

   XmTextSetString(bugMailText, NULL);

   if (w == bugMailToggle)
      s = br_tmpl_bugrep;
   else
      s = br_tmpl_comment;

   for ( ; *s; s++) {
      XmTextInsert(bugMailText, pos, *s);
      pos += strlen(*s);
      XmTextInsert(bugMailText, pos, "\n");
      pos++;
   }
}

/***********************************************************************
 *                                                                     *
 *   Send mail.                                                        *
 *                                                                     *
 ***********************************************************************/
static void send_mail(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   char *name, *inst, *phone, *email, *mess, mode;

   mode = XmToggleButtonGadgetGetState(bugMailToggle) ? 'B' : 'C';

   name  = XmTextGetString(userMailText);
   inst  = XmTextGetString(instituteMailText);
   phone = XmTextGetString(phoneMailText);
   email = XmTextGetString(emailMailText);
   mess  = XmTextGetString(bugMailText);

   if (br_mail_motif(mode, name, inst, phone, email, mess))
      km_warn_user(mailDialog, "Error sending mail...", "");
   else
      UxPopdownInterface(mailDialog);

   XtFree(name);
   XtFree(inst);
   XtFree(phone);
   XtFree(email);
   XtFree(mess);
}
