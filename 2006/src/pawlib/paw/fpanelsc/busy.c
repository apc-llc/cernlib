/*
 * $Id: busy.c,v 1.1.1.1 1996/03/01 11:39:06 mclareni Exp $
 *
 * $Log: busy.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:06  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.40  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Display busy dialog during long computation.
 =====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <Xm/DialogS.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <X11/cursorfont.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "hmotif/uxxt.h"
#include "fpanelsh/fplocal.h"
#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/mainpanel.h"
#undef CONTEXT_MACRO_ACCESS

#ifdef __cplusplus
}  /* extern "C" */
#endif


static Widget busyShell = NULL;  /* WorkingDialog displayed when timed out */

/***********************************************************************
 Set busy cursor over the Fit Panel and its subpanels.
***********************************************************************/
void hp_setcurs(Cursor curs, Widget except, void *ctxt)
{
    Display *display;
    Window win;

    UxMainPanelShellContext = (_UxCmainPanelShell *)ctxt;

    display = XtDisplay(mainPanelShell);

    if (mainPanelShell!=except && (win=XtWindow(mainPanelShell))!=0) {
        XDefineCursor(display, win, curs);
    }
    if (fileSelPanel!=except && (win=XtWindow(fileSelPanel))!=0) {
        XDefineCursor(display, win, curs);
    }
    if (contourPanel!=except && (win=XtWindow(contourPanel))!=0) {
        XDefineCursor(display, win, curs);
    }
    if (scanPanel!=except && (win=XtWindow(scanPanel))!=0) {
        XDefineCursor(display, win, curs);
    }
    if (vectPanel!=except && (win=XtWindow(vectPanel))!=0) {
        XDefineCursor(display, win, curs);
    }
    XFlush(display);
}

/***********************************************************************
 User Pressed the "Stop" button in dialog.
***********************************************************************/
void clb_busyStop(
Widget button,
XtPointer client_data,
XtPointer call_data)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *)client_data;

    interrCompute = True;
}

/***********************************************************************
 Turns on the "watch" cursor over the application to provide feedback
 for the user that he's going to be waiting a while before he can
 interact with the appliation again.
***********************************************************************/
void createBusyDialog(Widget parent, void *ctxt)
{
    static Cursor cursor;
    XSetWindowAttributes attrs;
    Display *dpy = XtDisplay(parent);
    XEvent event;
    extern void stop();
    Widget busyDialog;
    XtAppContext app_con = XtWidgetToApplicationContext(parent);
    Window dialogWin;

    if (busyShell != NULL) {
        return;
    }

    UxMainPanelShellContext = (_UxCmainPanelShell *)ctxt;

    interrCompute = False;

    if (!cursor) /* make sure the timeout cursor is initialized */
        cursor = XCreateFontCursor(dpy, XC_watch);

    /* turn on watch cursor */
    hp_setcurs(cursor, NULL, ctxt);

    busyShell = XtVaCreatePopupShell( "bShell",
            xmDialogShellWidgetClass,
            parent,
            XmNshellUnitType, Xm100TH_FONT_UNITS,
            NULL);

    busyDialog = XtVaCreateWidget( "bBox",
            xmMessageBoxWidgetClass,
            busyShell,
            XmNdialogType, XmDIALOG_INFORMATION,
            RES_CONVERT( XmNdialogTitle, "Busy" ),
            RES_CONVERT(XmNmessageString, "Computing in progress"),
            NULL);
    XtUnmanageChild
        (XmMessageBoxGetChild(busyDialog,XmDIALOG_HELP_BUTTON));
    XtUnmanageChild
        (XmMessageBoxGetChild(busyDialog,XmDIALOG_CANCEL_BUTTON));

    XtUnmanageChild  /* temporarily disable Stop button */
        (XmMessageBoxGetChild(busyDialog,XmDIALOG_OK_BUTTON));
#if 0
    XtVaSetValues(XmMessageBoxGetChild(busyDialog,XmDIALOG_OK_BUTTON),
        RES_CONVERT( XmNlabelString, "Stop"), NULL);
#endif

    XtAddCallback(busyDialog, XmNokCallback,
        (XtCallbackProc)clb_busyStop,(XtPointer)ctxt);

    XtAddGrab(busyDialog,True,False);
    XtManageChild(busyDialog);

    dialogWin = XtWindow(busyDialog);
    do {
        XtAppNextEvent(app_con, &event);
        XtDispatchEvent(&event);
    } while (!(event.type==Expose && event.xany.window == dialogWin));
    /* Let motif process all pending exposure events for us. */
    XmUpdateDisplay(parent);
    /* Make sure all our requests get to the server */
    XFlush(dpy);
}

/***********************************************************************
 Get rid of all button and keyboard events that occured during the time out.
 The user shouldn't have done anything during this time, so flush for
 button and keypress events. KeyRelease events are not discarded because
 accelerators require the corresponding release event before normal input
 can continue.
***********************************************************************/
void destroyBusyDialog(Widget parent, void *ctxt)
{
    XSetWindowAttributes attrs;
    Display *dpy = XtDisplay(parent);
    XtAppContext app_con = XtWidgetToApplicationContext(parent);
    XEvent event;

    UxMainPanelShellContext = (_UxCmainPanelShell *)ctxt;

    hp_setcurs(None, NULL, ctxt);

    while (XtAppPending(app_con)) {
        XtAppNextEvent(app_con, &event);
        XtDispatchEvent(&event);
    }
    XtDestroyWidget(busyShell);
    busyShell = NULL;
}

/***********************************************************************
 Make sure all the pending events are processed.
***********************************************************************/
void processXEvents()
{
    Display *dpy = XtDisplay(busyShell);
    Window win = XtWindow(busyShell);
    XtAppContext app_con = XtWidgetToApplicationContext(busyShell);
    XEvent event;

    /* Make sure all our requests get to the server */
    XFlush(dpy);

    /* Let motif process all pending exposure events for us. */
    XmUpdateDisplay(busyShell);

    /* Check the event loop for events */
    while (XtAppPending(app_con)) {
        XtAppNextEvent(app_con, &event);
        XtDispatchEvent(&event);
    }
}
