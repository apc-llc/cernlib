/*
 * $Id: kmutim.c,v 1.1.1.1 1996/03/08 15:33:09 mclareni Exp $
 *
 * $Log: kmutim.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:09  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/03 09/05/95  11.54.44  by  N.Cremel*/
/*-- Author :    N.Cremel   03/04/92*/
/******************************************************************************/
/*                                                                            */
/*           MOTIF-based library for KUIP - deck KMUTIM                       */
/*                                                                            */
/*           General utilitity routines (refer some Motif/X11 include files)  */
/*                                                                            */
/*           - km_do_help :  Display text of any "Help_item" entry in the CDF */
/*           - km_normal_string : Get char string from XmString               */
/*           - km_insert_string : Insert a string at the end of a XmText      */
/*           - km_double_click : Check if doubleclick happened                */
/*           - km_show_cb : Show (manage) the widget passed as client_data    */
/*           - km_close_cb : Close (popdown) a popup dialog (client_data)     */
/*           - km_cancel_cb : Remove (unmanage) popup passed as client_data   */
/*           - km_reset_sensitive_cb : Reset sensitivity (client_data)        */
/*           - km_help_cb : Display HELP on command passed as client_data     */
/*           - km_make_menuStruct : Assign/fill the space for a MenuCbStruct  */
/*           - km_popup_MenuCbdialog : Popup dialog (in MenuCbStruct)         */
/*           - km_popdown_MenuCbdialog : Popdown dialog (in MenuCbStruct)     */
/*           - km_GetTrueShell : Get the true toplevel shell                  */
/*           - km_MwmCloseCallback : Install MwmCloseCallback                 */
/*           - km_IsShellIconic : Is shell in iconic state?                   */
/*           - km_IconifyShell : Iconify a shell                              */
/*           - km_DeIconifyShell : De-Iconify a shell                         */
/*           - km_FlushEvents :  equivalent to XtAppMainLoop                  */
/*           - km_wait_input : Ask user for input and wait till he responds   */
/*           - km_toggle_selection : Ask user for one choice between toggles  */
/*           - km_set_cursor : Set the cursor shape                           */
/*           - km_get_curpos : Get cursor positions xc and yc                 */
/*           - km_view_file : print content of a file into executive window   */
/*           - km_focus_window : to avoid window to disappear under kxterm    */
/*           - km_list_data : set data information for a list.                */
/*           - km_show_list : display list (after filling items).             */
/*           - km_show_filSel : display FileSelectionBox.                     */
/*                                                                            */
/******************************************************************************/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kmenu.h"
#include "kuip/browh1.h"

#include <Xm/Xm.h>
#if defined(__hpux) && XtSpecificationRelease == 4
#include <X11/Protocols.h>
#else
#include <Xm/Protocols.h>
#endif
#include <Xm/AtomMgr.h>

/* Global SEQUENCES (defined in MKCDES)                                       */
#include "mkincl.h"
#include "mkdims.h"
#include "mksres.h"
#include "mksreg.h"
#include "mkutdm.h"
#include "mkutfm.h"
#define CONTEXT_MACRO_ACCESS 1
#include "mkuxfs.h"
#include "mkuxli.h"
#undef CONTEXT_MACRO_ACCESS

/***********************************************************************
 *                     Global data                                     *
 ***********************************************************************/
static Widget  prompt_id = NULL;
static Boolean prompt_not_finished = False;
static Boolean prompt_cancel = False;
static ListData list_data;


/***********************************************************************
 *                                                                     *
 *   Forward declarations of local functions.                          *
 *                                                                     *
 ***********************************************************************/
static C_PROTO_3 (void return_input,
                         Widget widget,
                         int tag,
                         XmAnyCallbackStruct *data);
static C_PROTO_3 (void wcancel_activate,
                         Widget  widget,
                         caddr_t tag,
                         XmAnyCallbackStruct *callback_data);
#if 0
static C_PROTO_3 (void XtoPS_cb,
                         Widget   widget,
                         xtops_struct    *tag,
                         XmAnyCallbackStruct *data);
#endif
static void  okFCallback( Widget widget,
                         int (*tag)(),
                         XmFileSelectionBoxCallbackStruct *data);


/***********************************************************************
 *                                                                     *
 *   Forward declarations of extern functions.                         *
 *                                                                     *
 ***********************************************************************/
extern C_PROTO_5(void km_print_box,
                         Widget pw,
                         Position x,
                         Position y,
                         char *message,
                         int itype);

/* km_view_file : Print the content of a file into the executive window */
extern C_PROTO_3(void          km_view_file,
                                  Widget w,
                                  BrClientdata *client_data,
                                  XmAnyCallbackStruct *cbs);

extern C_PROTO_3(void km_clFCallback,
                         Widget widget,
                         caddr_t tag,
                         XmAnyCallbackStruct   *data);

/***********************************************************************
 *                                                                     *
 *  Display help corresponding to a "Help_item" entry in the CDF.      *
 *                                                                     *
 ***********************************************************************/
void km_do_help(w, client_data, call_data)
       Widget w;
       char *client_data;
       XmAnyCallbackStruct *call_data;
{
   char cmd[MAX_string];

   sprintf (cmd,"/KUIP/HELP %s", client_data);
   ku_exel (cmd);
}

/******************************************************************************/
/* void km_help_activate( widget, tag, callback_data )                        */
/*      prompt dialog help button callback                                    */
/******************************************************************************/
void km_help_activate( widget, tag, callback_data )
        Widget  widget;
        caddr_t tag;
        XmAnyCallbackStruct *callback_data;
{
    char *help_message = (char *) tag;
    Position xr, yr;

    km_get_curpos (callback_data->event,&xr,&yr);
    km_print_box (km_toplevel, xr, yr, help_message,2);
}

/***********************************************************************
 *                                                                     *
 *   Get char string from XmString.                                    *
 *   Returned string must be freed afterwards.                         *
 *                                                                     *
 ***********************************************************************/
char *km_normal_string(cs)
     XmString cs;
{
  char  *primitive_string;

  XmStringGetLtoR(cs, XmSTRING_DEFAULT_CHARSET, &primitive_string);

  return ((char *)primitive_string);
}

/***********************************************************************
 *                                                                     *
 *   Insert a string at the end of a XmText widget.                    *
 *                                                                     *
 ***********************************************************************/
C_DECL_3(void km_insert_string,
     Widget, text_widget,
     char *, buf,
     Boolean, show)
{
   int pos = XmTextGetLastPosition(text_widget);
   XmTextInsert(text_widget, pos, buf);

   if (show) {
      pos = XmTextGetLastPosition(text_widget);
      XmTextShowPosition(text_widget, pos);
   }
}

/***********************************************************************
 *                                                                     *
 *   Check if doubleclick happened.                                    *
 *                                                                     *
 ***********************************************************************/
Boolean km_double_click(time)
   Time time;
{
   static Time  click1 = 0;
   Boolean      rtn;

   rtn = False;

   /* in case of time wrap no double click */
   if (time < click1) return(rtn);

   if (time - click1 <= srec.dc_interval)
      rtn = True;
   click1 = time;

   return(rtn);
}

/***********************************************************************
 *                                                                     *
 *   Show (manage) the widget passed as the client_data.               *
 *                                                                     *
 ***********************************************************************/
void km_show_cb(w, dialog, call_data)
   Widget w, dialog;
   XmAnyCallbackStruct *call_data;
{
   XtManageChild(dialog);
}

/***********************************************************************
 *                                                                     *
 *   Close (popdown) a popup dialog (or shell) passed as client_data.  *
 *                                                                     *
 ***********************************************************************/
void km_close_cb(w, dialog, call_data)
     Widget w, dialog;
     XmAnyCallbackStruct *call_data;
{
   UxPopdownInterface(dialog);
}

/***********************************************************************
 *                                                                     *
 *   Remove (unmanage) popup passed as client_data (in MenuCbStruct).  *
 *                                                                     *
 ***********************************************************************/
void km_cancel_cb(w, item, call_data)
     Widget w;
     MenuCbStruct *item;
     XmAnyCallbackStruct *call_data;
{
   km_popdown_MenuCbdialog(item);
}

/***********************************************************************
 *                                                                     *
 *   Reset sensitivity on the widget passed as the client_data         *
 *   parameter.                                                        *
 *                                                                     *
 ***********************************************************************/
void km_reset_sensitive_cb(w, reset_widget, call_data)
     Widget w, reset_widget;
     XmAnyCallbackStruct *call_data;
{
   XtSetSensitive(reset_widget, True);
}

/***********************************************************************
 *                                                                     *
 *   Display (scrolled text) HELP on command passed as client_data.    *
 *                                                                     *
 ***********************************************************************/
void km_help_cb(w, help_cmd, call_data)
     Widget w;
     char *help_cmd;
     XmAnyCallbackStruct *call_data;
{
   Position xr, yr;

   km_get_curpos (call_data->event,&xr,&yr);
   km_display_sctext ("Help", help_cmd, xr, yr, 0);
}

/***********************************************************************
 *                                                                     *
 *   Assign the space for a MenuCbStruct and fill it.                  *
 *                                                                     *
 ***********************************************************************/
MenuCbStruct *km_make_menuStruct(button, dialog)
   Widget button, dialog;
{
   MenuCbStruct  *tmp;

   tmp = XtNew(MenuCbStruct);
   tmp->button = button;
   tmp->dialog = dialog;

   return tmp;
}

/***********************************************************************
 *                                                                     *
 *   Popup dialog (in MenuCbStruct) and make menu item insensitive.    *
 *                                                                     *
 ***********************************************************************/
void km_popup_MenuCbdialog(menu_item)
   MenuCbStruct *menu_item;
{
   XtSetSensitive(menu_item->button, False);
   XtManageChild(menu_item->dialog);
}

/***********************************************************************
 *                                                                     *
 *   Popdown dialog (in MenuCbStruct) and make menu item sensitive.    *
 *                                                                     *
 ***********************************************************************/
void km_popdown_MenuCbdialog(menu_item)
   MenuCbStruct *menu_item;
{
   XtSetSensitive(menu_item->button, True);
   XtUnmanageChild(menu_item->dialog);
}

/***********************************************************************
 *                                                                     *
 *   Returns the true shell widget.                                    *
 *                                                                     *
 ***********************************************************************/
Widget km_GetTrueShell(w)
     Widget w;
{
   Widget wgt = w;

   if (wgt && !XtIsShell(wgt)) {
      wgt = XtParent(wgt);
   }
   return wgt;
}

/***********************************************************************
 *                                                                     *
 *   Install MwmCloseCallback.                                         *
 *                                                                     *
 ***********************************************************************/
void km_MwmCloseCallback(w, callback, client_data)
     Widget w;
     XtCallbackProc callback;
     XtPointer client_data;
{
   static Atom a = (Atom) NULL;
   Widget shell  = km_GetTrueShell(w);

   if (!a)
      a = XmInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW", False);

   if (XmIsVendorShell(shell)) {
      XmAddWMProtocols(shell, &a, 1);
      XmAddWMProtocolCallback(shell, a, callback, client_data);
   }
}

/***********************************************************************
 *                                                                     *
 *   Is shell in IconicState?                                          *
 *                                                                     *
 ***********************************************************************/
Boolean km_IsShellIconic(w)
   Widget w;
{
   static Atom    a      = (Atom) NULL;
   Widget         shell  = km_GetTrueShell(w);
   Display       *dpy    = XtDisplay(shell);
   Window         window = XtWindow(shell);
   unsigned long *property = NULL;
   unsigned long  nitems;
   unsigned long  leftover;
   Atom           actual_type;
   int            actual_format;
   int            status;
   Boolean        state;

   if (!a)
      a = XmInternAtom(dpy, "WM_STATE", False);

   status = XGetWindowProperty(dpy, window,
                               a, 0L, 1L,
                               False, a, &actual_type, &actual_format,
                               &nitems, &leftover, (unsigned char **)&property);

   if ( !((status == Success) &&
          (actual_type == a) && (nitems == 1L))) {
      if (property) {
         XFree((char *)property);
         return(False);
      }
   }

   state = (*property == IconicState) ? True : False;

   XFree((char *)property);
   return(state);
}

/***********************************************************************
 *                                                                     *
 *   Iconify a shell.                                                  *
 *                                                                     *
 ***********************************************************************/
void km_IconifyShell(shell)
     Widget shell;
{
   static Atom          a = (Atom) NULL;
   Display             *dpy = XtDisplay(shell);
   XClientMessageEvent  ev;

   if (!a)
      a = XmInternAtom(dpy, "WM_CHANGE_STATE", False);

   ev.type         = ClientMessage;
   ev.display      = dpy;
   ev.message_type = a;
   ev.format       = 32;
   ev.data.l[0]    = IconicState;
   ev.window       = XtWindow(km_GetTrueShell(shell));

   XSendEvent(dpy,
            RootWindow(dpy, DefaultScreen(dpy)),
            True,
            (SubstructureRedirectMask | SubstructureNotifyMask),
            (XEvent *)&ev);
   XFlush(dpy);
}

/***********************************************************************
 *                                                                     *
 *   De-Iconify a shell.                                               *
 *                                                                     *
 ***********************************************************************/
void km_DeIconifyShell(w)
     Widget w;
{
   Widget shell = km_GetTrueShell(w);
   Display *dpy = XtDisplay(shell);

   XMapWindow(dpy, XtWindow(shell));
   XFlush(dpy);
}


#if 0
/******************************************************************************/
/* void XtoPS_cb()                                                            */
/*      XtoPS button activate callback                                        */
/*      produce postscript output of the widget                               */
/******************************************************************************/
static void XtoPS_cb(widget, tag, data )
      Widget   widget;
      xtops_struct    *tag;
      XmAnyCallbackStruct *data;
{
    char local_string[128];
    char *file_arg;
    char *postscript_name;


    if (tag->widget_name == NULL) {
       strcpy (local_string,class_name);
       strcat (local_string,".ps");
       postscript_name = km_wait_input ("Enter postscript file name",
                                        local_string,"HELP_XTOPS");
       if (postscript_name == NULL) return;
       file_arg = postscript_name; }
    else
       file_arg = tag->widget_name ;
    free (postscript_name);
    sprintf (local_string,"XtoPS -id %d -frame %s",
           XtWindow(tag->widget),file_arg);
    km_set_cursor (km_toplevel,km_wait_cursor);
    XFlush(km_Display);
    system (local_string);
    km_set_cursor (km_toplevel,km_main_cursor);
    XFlush(km_Display);
}
#endif


/******************************************************************************/
/* void km_FlushEvents : equivalent to XtAppMainLoop                          */
/******************************************************************************/
void  km_FlushEvents()
{
    XEvent event;

    while (XtAppPending(UxAppContext)) {
        XtAppNextEvent(UxAppContext, &event);
        XtDispatchEvent(&event);
    }
}


/******************************************************************************/
/* void return_input(widget, tag, data )                                      */
/*      prompt dialog ok button callback : end loop (see km_wait_input)       */
/******************************************************************************/
static void return_input(widget, tag, data )
      Widget   widget;
      int     tag;
      XmAnyCallbackStruct *data;
{
    prompt_not_finished = False;
}


/******************************************************************************/
/* void wcancel_activate( widget, tag, callback_data )                        */
/*      prompt dialog cancel button callback (see km_wait_input)              */
/******************************************************************************/
static void wcancel_activate( widget, tag, callback_data )
        Widget  widget;
        caddr_t tag;
        XmAnyCallbackStruct *callback_data;
{
    prompt_not_finished = False;
    prompt_cancel = True;
}


/******************************************************************************/
/* char *km_wait_input (prompt, default_line, help_item)                      */
/*                                                                            */
/*      char *prompt         :   prompt string                                */
/*      char *default_line   :   default value                                */
/*      char *help_item      :   item value for help                          */
/*                                                                            */
/*      Return               :   value given by the user                      */
/*                               NULL for cancel                              */
/*                                                                            */
/*      Ask user for input and wait till she responds (C callable)            */
/*      create a prompt dialog for a mandatory parameter input                */
/*      (Next event + Dispatch event) loop until the ok button is pressed     */
/*                                                                            */
/******************************************************************************/
char *km_wait_input (prompt, default_line, help_item)
    char *prompt, *default_line, *help_item;
{
    Arg arglist[5];
    int narg;
    char cval[MAX_string];
    XmString xmstr, input_string;

    char *input_text = NULL;


    if (prompt_id != NULL) {
        XtDestroyWidget (prompt_id);
        km_reset_appShell (prompt_id);
    }
    prompt_id = NULL;


    narg = 0;
    XtSetArg (arglist[narg],XmNselectionLabelString,
              XmStringCreateLtoR(prompt,km_charset)); narg++;
    if (strlen(default_line) != 0) {
        XtSetArg (arglist[narg],XmNtextString,
                 XmStringCreateLtoR(default_line, km_charset)); narg++; }
    sprintf (cval, "%s Prompt", class_name);
    xmstr = XmStringCreateLtoR(cval, XmSTRING_DEFAULT_CHARSET);
    XtSetArg(arglist[narg], XmNdialogTitle, xmstr); narg++;
    prompt_id = XmCreatePromptDialog(km_toplevel,
                "km_wait_input",arglist, narg);
    km_store_appShell (prompt_id, cval);
    XmStringFree(xmstr);
    XtAddCallback(prompt_id,XmNokCallback,(XtCallbackProc)return_input,NULL);
    XtAddCallback(prompt_id,XmNhelpCallback,
                   (XtCallbackProc)km_do_help,(XtPointer)help_item);
    XtAddCallback(prompt_id,XmNcancelCallback,
                  (XtCallbackProc)wcancel_activate,NULL);
    XtManageChild(prompt_id);
    prompt_not_finished = True;
    prompt_cancel = False;
    while (prompt_not_finished)
           km_FlushEvents();
    if (prompt_cancel) return (NULL);
    km_reset_appShell (prompt_id);
    narg = 0;
    XtSetArg (arglist[narg],XmNtextString,&input_string); narg++;
    XtGetValues (prompt_id,arglist,narg);
    XmStringGetLtoR(input_string,km_charset,&input_text);
    return (str0dup(input_text));
}

/***********************************************************************
 *                                                                     *
 * km_toggle_selection :                                               *
 *                                                                     *
 * Gives another taste of the "SelectionBox" widget with toggles       *
 * buttons instead of a list widget.                                   *
 *                                                                     *
 ***********************************************************************/
int km_toggle_selection (prompt, list, defsel)
    char *prompt;
    char **list;
    int defsel;
{
    Widget kts, ktsRowColumn, ktsForm1, ktsForm2,
           ktsSep, kts_OK, kts_Cancel,
           ktsToggle[10];
    char cval[MAX_string];
    int i, input;

    if (prompt_id != NULL) {
        XtDestroyWidget (prompt_id);
        km_reset_appShell (prompt_id);
    }
    prompt_id = NULL;


    /* Design and create widget "toggle_selection" */
    prompt_id = XtVaCreatePopupShell( "prompt_id",
                        xmDialogShellWidgetClass, km_toplevel,
                        NULL );
    sprintf (cval, "%s Selection", class_name);
    km_store_appShell (prompt_id, cval);
    kts = XtVaCreateWidget( "kts",
                        xmFormWidgetClass, prompt_id,
                        RES_CONVERT( XmNdialogTitle, cval ),
                        NULL );
    kts_OK = XtVaCreateManagedWidget( "kts_OK",
                        xmPushButtonGadgetClass, kts,
                        XmNdefaultButtonShadowThickness, 1,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNleftPosition, 20,
                        XmNleftOffset, -30,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshowAsDefault, 1,
                        RES_CONVERT( XmNlabelString, "OK" ),
                        XmNheight, 30,
                        XmNwidth, 60,
                        NULL );
    kts_Cancel = XtVaCreateManagedWidget( "kts_Cancel",
                        xmPushButtonWidgetClass, kts,
                        RES_CONVERT( XmNlabelString, "Cancel" ),
                        XmNleftPosition, 80,
                        XmNleftOffset, -30,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        XmNwidth, 60,
                        NULL );
    XtVaSetValues(kts,
                        XmNdefaultButton, kts_OK,
                        NULL );
    ktsSep = XtVaCreateManagedWidget( "ktsSep",
                        xmSeparatorGadgetClass, kts,
                        XmNbottomWidget, kts_OK,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        NULL );
    ktsForm1 = XtVaCreateManagedWidget( "ktsForm1",
                        xmFormWidgetClass, kts,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );
    XtVaCreateManagedWidget( "ktsLabel",
                        xmLabelGadgetClass, ktsForm1,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        XmNleftPosition, 10,
                        RES_CONVERT( XmNlabelString, prompt ),
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        NULL );
    ktsForm2 = XtVaCreateManagedWidget( "ktsForm2",
                        xmFormWidgetClass, kts,
                        XmNbottomWidget, ktsSep,
                        XmNtopWidget, ktsForm1,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );
    ktsRowColumn = XtVaCreateManagedWidget( "ktsRowColumn",
                        xmRowColumnWidgetClass, ktsForm2,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNradioBehavior, TRUE,
                        XmNpacking, XmPACK_COLUMN,
                        NULL );
    /* Separators to have a frame arround the RowColumn */
    XtVaCreateManagedWidget( "ktsSep1",
                        xmSeparatorGadgetClass, ktsForm2,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
    XtVaCreateManagedWidget( "ktsSep2",
                        xmSeparatorGadgetClass, ktsForm2,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNorientation, XmVERTICAL,
                        NULL );
    XtVaCreateManagedWidget( "ktsSep3",
                        xmSeparatorGadgetClass, ktsForm2,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        NULL );
    XtVaCreateManagedWidget( "ktsSep4",
                        xmSeparatorGadgetClass, ktsForm2,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNorientation, XmVERTICAL,
                        NULL );

    /* Loop on list to create as many toggles required */
    for (i=0; list[i]; i++) {
         if (i >= 10) break;
         if (i+1 == defsel)
             ktsToggle[i] = XtVaCreateManagedWidget( "ktsToggle",
                        xmToggleButtonGadgetClass, ktsRowColumn,
                        RES_CONVERT( XmNlabelString, list[i] ),
                        XmNset, TRUE,
                        XmNheight, 30,
                        NULL );
         else
             ktsToggle[i] = XtVaCreateManagedWidget( "ktsToggle",
                        xmToggleButtonGadgetClass, ktsRowColumn,
                        RES_CONVERT( XmNlabelString, list[i] ),
                        XmNheight, 30,
                        NULL );
    } /* end ... for (i=0; list[i]; i++) */

    UxPopupInterface(kts, XtGrabNone);


    XtAddCallback(kts_OK,XmNactivateCallback,(XtCallbackProc)return_input,NULL);
    XtAddCallback(kts_Cancel,XmNactivateCallback,
                            (XtCallbackProc)wcancel_activate,NULL);
    prompt_not_finished = True;
    prompt_cancel = False;
    while (prompt_not_finished)
           km_FlushEvents();
    if (prompt_cancel) return (0);
    km_reset_appShell (prompt_id);
    for (input=0; list[input]; input++) {
         Boolean set;

         if (input >= 10) break;
         XtVaGetValues (ktsToggle[input], XmNset, &set, NULL);
         if (set) return (input+1);
    }

    return (-1);
}

/***********************************************************************
 *                                                                     *
 *   Set the cursor shape.                                             *
 *                                                                     *
 ***********************************************************************/
extern void km_set_cursor (w, cursor)
     Widget w;
     Cursor cursor;
{
   XSetWindowAttributes attrs;
   Display *dpy = XtDisplay(w);

   if (!XtWindow(w)) return;

   attrs.cursor = cursor;

   XChangeWindowAttributes(dpy, XtWindow(w), CWCursor, &attrs);

   XFlush(dpy);
}


/******************************************************************************/
/* void km_get_curpos (event, *xc, *yc)                                       */
/******************************************************************************/
/*
 * Get cursor positions xc and yc
*/
void km_get_curpos (event, xc, yc)
       XEvent  *event;
       Position *xc, *yc;       /* output values                              */
{
    static int xmax, ymax;
    static int first = 0;

    if( event == NULL ) {
        *xc = 0;
        *yc = 0;
        return;
    }

/*  Get display size (first time only)                                        */
    if (first == 0) {
        first = 1;
        xmax = XDisplayWidth (km_Display,DefaultScreen(km_Display));
        ymax = XDisplayHeight (km_Display,DefaultScreen(km_Display)); }

    *xc = event->xbutton.x_root;
    *yc = event->xbutton.y_root;

    if (*xc < 0 || *xc > xmax) *xc = 0;
    if (*yc < 0 || *yc > ymax) *yc = 0;

    return;
}

/***********************************************************************
 *                                                                     *
 *   Print the content of a file into the executive window.            *
 *                                                                     *
 ***********************************************************************/
void km_view_file (w, client_data, cbs)
   Widget w;
   BrClientdata *client_data;
   XmAnyCallbackStruct *cbs;
{
   FILE *tfp;

   /* Open and read file */
   if ((tfp = fopen(client_data->stext, "r")) != NULL) {
      char line[1024];

      printf ("*** File %s :\n", client_data->stext);
      while( fgets( line, (sizeof line), tfp ) != NULL )
        printf ("%s", line);
   }
}

/***********************************************************************
 *                                                                     *
 * Event handler to avoid the window to disappear under kxterm         *
 *                                                                     *
 ***********************************************************************/
void km_focus_window(widget, tag, ev)
      Widget widget;
      caddr_t tag;
      XEnterWindowEvent *ev;
{
    XRaiseWindow ( UxDisplay, XtWindow(km_GetTrueShell(widget)) );
}

/***********************************************************************
 *                                                                     *
 * "OK callback" for a list. It automatically calls the user defined   *
 * routine for OK (from list_dat.user_callback).                       *
 *                                                                     *
 ***********************************************************************/
static int km_list_OK( widget, tag, callback_data )
        Widget  widget;
        caddr_t tag;
        XmAnyCallbackStruct *callback_data;
{
    Widget       kuipListText;
    char *selection;

    /* Get selected value */
    kuipListText = XmSelectionBoxGetChild (km_listSBox, XmDIALOG_TEXT);
    selection = (char *) XmTextGetString (kuipListText);

    /* Call user defined callback for OK with selection as input argument */
    list_data.user_callback (selection);

    /* Destroy list */
    km_destroy_list();

    return 0;
}

/***********************************************************************
 *                                                                     *
 * km_list_data (list_label, selection_label, help_text, callback)     *
 * Set data information for a user defined list.                       *
 *                                                                     *
 * char *list_label      :  label written at the beginning of the list *
 * char *selection_label :  label written before the selection         *
 * char *help_text       :  help message accessed throuh help button   *
 * int callback()        :  user defined routine called for OK         *
 * N.B. int callback( char* selection) where selection (input) is      *
 * the value which has been selected by the user.                      *
 ***********************************************************************/
void km_list_data( const char *list_label,
                  const char *selection_label,
                  const char *help_text,
                  int (*callback)() )
{
  /* Fill structure ListData with ... */

  /* ... list label (written at the beginning of the list) */
  list_data.listLabel = strdup (list_label);

  /* ... List selection label (written before the selection) */
  list_data.label = strdup (selection_label);

  /* ... help text (accessed throuh help button) */
  list_data.help = strdup (help_text);

  /* ... address of user defined routine called when OK button is pressed */
  list_data.user_callback = callback;

  /* ... OK callback (user written routine) */
  list_data.OKcallback = km_list_OK;
}

/***********************************************************************
 *                                                                     *
 * Display list after char **items is filled (and km_list data called) *
 *                                                                     *
 ***********************************************************************/
void  km_show_list (items)
  char **items;
{
  km_display_list ( items, NULL, list_data);
}

/***********************************************************************
 *                                                                     *
 * Display FileSelectionBox                                            *
 *                                                                     *
 ***********************************************************************/
void km_show_filSel( const char *title,
                    const char *dir,
                    const char *def,
                    const char *help,
                    int (*okCallback)() )
{
extern Widget km_fil_sel;
   char *filter = NULL;

   if (km_fil_sel != NULL) XtDestroyWidget (XtParent(km_fil_sel));
   km_fil_sel = NULL;

   km_fil_sel = create_kuipFilSel(km_toplevel);
   XtVaSetValues ( XtParent(km_fil_sel), XmNtitle, title,
                                         NULL);
   XtVaSetValues ( km_fil_sel, XmNdefaultPosition, True, NULL);

   /*
    *   find and restore the context
    *   (to get to the widgets belonging to this dialog)
    */
   UxKuipFilSelContext = (_UxCkuipFilSel *)UxGetContext( km_fil_sel );

   /* set filter and directory */
   if (dir != NULL) {
       char *p;

       filter = strdup (dir);
       XtVaSetValues ( km_FilSel,
                       RES_CONVERT( XmNpattern, filter ),
                       NULL);
       p = strrchr (filter, '/');
       if (p != NULL) {
           p[1] = '\0';
           XtVaSetValues ( km_FilSel,
                       RES_CONVERT( XmNdirectory, filter ),
                       NULL);
       }
   }

   if (def != NULL) {
       Widget listw;
       XmStringTable xmstr_list;
       int nc, l;

       filter = mstrcat (filter, def);
       listw = XmSelectionBoxGetChild( km_FilSel, XmDIALOG_LIST);
       XtVaGetValues ( listw, XmNitems, &xmstr_list,
                              XmNitemCount, &nc,
                              NULL);
       for (l = 0; l< nc; l++) {
            char *item;

            XmStringGetLtoR(xmstr_list[l], km_charset, &item);
            if (strcmp (item, filter) == 0) {
                XmListSelectItem ( listw, xmstr_list[l], True);
            }
            XtFree (item);
       }
   }
   if (filter != NULL) free (filter);

   /* Display FileSelectionBox */
   UxPopupInterface( km_fil_sel, XtGrabNone );

   XtAddCallback( km_FilSel, XmNhelpCallback,
                  (XtCallbackProc)km_help_activate, (XtPointer)help );

   XtAddCallback( km_FilSel, XmNokCallback,
                 (XtCallbackProc)okFCallback, (XtPointer)okCallback );
   XtAddCallback( km_FilSel, XmNcancelCallback,
                 (XtCallbackProc)km_clFCallback, NULL );
}

static void okFCallback( Widget widget,
                        int (*okCallback)(),
                        XmFileSelectionBoxCallbackStruct *data )
{
extern Widget km_fil_sel;
   char *file;

   /*
    *   find and restore the context
    *   (to get to the widgets belonging to this dialog)
    */
   UxKuipFilSelContext = (_UxCkuipFilSel *)UxGetContext( widget );

   XmStringGetLtoR(data->value, km_charset, &file);

   (*okCallback)(file);

   XtFree (file);

   if (km_fil_sel != NULL) XtDestroyWidget (XtParent(km_fil_sel));
   km_fil_sel = NULL;
}
