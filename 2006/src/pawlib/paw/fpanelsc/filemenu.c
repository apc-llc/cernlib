/*
 * $Id: filemenu.c,v 1.1.1.1 1996/03/01 11:39:07 mclareni Exp $
 *
 * $Log: filemenu.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:07  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.40  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Callback functions for File menu and for File Selection Subpanel.
 =====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/MenuShell.h>
#include <Xm/ToggleBG.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "fpanelsh/fplocal.h"
#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/parlineform.h"
#undef mainctxt
#undef UxParent
#include "fpanelsh/fileselpanel.h"
#undef UxParent
#include "fpanelsh/mainpanel.h"
#undef CONTEXT_MACRO_ACCESS

#ifdef __cplusplus
}  /* extern "C" */
#endif


/*******************************************************************************
     ################### File selection panel callbacks ###################
*******************************************************************************/

/***********************************************************************
 If pathname does not end with a filename return True.
***********************************************************************/
static Boolean emptyFileName(char *pname)
{
    char *chp = pname;
    char lastch = ' ';

    if (*chp == '\0') return(True);

    do {
        if (*chp!=' ' && *chp!='\t' && *chp!='\n') lastch = *chp;
    } while (*(++chp) != '\0');

    if (lastch == '/') return(True);
    else return(False);
}

/***********************************************************************
 OK button callback for Save command.
***********************************************************************/
static void clb_fileSaveOK(
Widget OKbutton,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "save";
    int lun = 7;
    int istat;
    String fname;
    int icondn;
    Widget fnameText;

    UxFileSelectionShellContext = (_UxCfileSelectionShell *) client_data;
    UxMainPanelShellContext = (_UxCmainPanelShell *) mainctxt;

    /***** open fortran file for Minuit *****/
    fnameText = XmFileSelectionBoxGetChild(fileSelectionBox, XmDIALOG_TEXT);
    XtVaGetValues(fnameText, XmNvalue, &fname,NULL);
    if (emptyFileName(fname)) {
        km_warn_user(fileSelPanel, "File name is empty","");
        return;
    }
    istat = ku_open(lun,fname,"UNKNOWN");
    if (istat != 0) {
        km_warn_user(fileSelPanel, "Can not open file","");
        return;
    }

    /***** save the parameters *****/
    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
    /***** add return command *****/
    ku_write(lun,"RETURN");

    /***** close fortran file for Minuit *****/
    ku_close(lun);

    XtDestroyWidget(fileSelPanel);
}

/***********************************************************************
 OK button callback for Input command.
***********************************************************************/
static void clb_fileInputOK(
Widget OKbutton,
XtPointer client_data,
XtPointer call_data)
{
    int lun = 7;
    int istat;
    String fname;
    int ierr;
    Widget fnameText;

    UxFileSelectionShellContext = (_UxCfileSelectionShell *) client_data;
    UxMainPanelShellContext = (_UxCmainPanelShell *) mainctxt;

    /***** open fortran file for Minuit *****/
    fnameText = XmFileSelectionBoxGetChild(fileSelectionBox, XmDIALOG_TEXT);
    XtVaGetValues(fnameText, XmNvalue, &fname,NULL);
    if (emptyFileName(fname)) {
        km_warn_user(fileSelPanel, "File name is empty","");
        return;
    }
    istat = ku_open(lun,fname,"OLD");
    if (istat != 0) {
        km_warn_user(fileSelPanel, "Can not open file","");
        return;
    }

    /***** read commands from the file *****/
    F77_CALLS_II(mninpu,&lun,&ierr);
    if (ierr != 0) {
        km_warn_user(fileSelPanel, "Error in Minuit reading input file","");
    }
    F77_CALLS_EE(mnintr,fcn,ufcn);

    /***** close fortran file for Minuit *****/
    ku_close(lun);

    XtDestroyWidget(fileSelPanel);

    /***** update the panel *****/
    get_mnparms((void *)UxMainPanelShellContext, False);
}

/*******************************************************************************
 Callback for the cancel button on File Selection Panel.
*******************************************************************************/
static void clb_fileCancel(
Widget cancelButton,
XtPointer client_data,
XtPointer call_data)
{
    UxFileSelectionShellContext = (_UxCfileSelectionShell *) client_data;
    UxMainPanelShellContext = (_UxCmainPanelShell *) mainctxt;

    XtDestroyWidget(fileSelPanel);
}

/*******************************************************************************
*******************************************************************************/
static void clb_destroyPanel(
Widget shell,
XtPointer client_data,
XtPointer call_data)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    fileSelPanel = NULL;
    XDefineCursor(XtDisplay(mainPanelShell), XtWindow(mainPanelShell), None);
}


/*******************************************************************************
     #################### File menu buttons callbacks ####################
*******************************************************************************/

/*******************************************************************************
 Callback for "Save..." menu item.
*******************************************************************************/
static void clb_fileSave(
Widget saveButton,
XtPointer client_data,
XtPointer call_data)
{
    Display *display;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    /***** create the panel *****/
    if (fileSelPanel == NULL) {
        fileSelPanel = create_fileSelectionPanel(mainPanelShell);
        UxFileSelectionShellContext =
        (_UxCfileSelectionShell *) UxGetContext(fileSelPanel);
        mainctxt = UxMainPanelShellContext;

        /***** define properties and callbacks of file selection panel */
        XtVaSetValues(fileSelectionShell,
            XmNtitle, "Save File Selection Panel",
            NULL );
        XtVaSetValues(fileSelectionBox,
            RES_CONVERT( XmNselectionLabelString, "Name of save file" ),
            NULL );
        XtRemoveAllCallbacks(fileSelectionBox, XmNokCallback);
        XtAddCallback( fileSelectionBox, XmNokCallback,
            (XtCallbackProc) clb_fileSaveOK,
            (XtPointer) UxFileSelectionShellContext);
        XtRemoveAllCallbacks(fileSelectionBox, XmNcancelCallback);
        XtAddCallback(fileSelectionBox, XmNcancelCallback,
            (XtCallbackProc) clb_fileCancel,
            (XtPointer) UxFileSelectionShellContext);
        XtAddCallback( fileSelectionShell, XmNdestroyCallback,
            (XtCallbackProc) clb_destroyPanel,
            (XtPointer) UxMainPanelShellContext);
    }

    /***** popup the panel *****/
    UxPopupInterface(fileSelPanel, exclusive_grab);

    /***** change the cursor *****/
    display = XtDisplay(mainPanelShell);
    if (blckCursor==0) blckCursor = XCreateFontCursor(display, XC_dot);
    XDefineCursor(display, XtWindow(mainPanelShell), blckCursor);
    if (selCursor==0) selCursor = XCreateFontCursor(display, XC_hand2);
    XDefineCursor(display, XtWindow(fileSelPanel), selCursor);
}

/*******************************************************************************
 Callback for "Input..." menu item.
*******************************************************************************/
static void clb_fileInput(
Widget inputButton,
XtPointer client_data,
XtPointer call_data)
{
    Cursor curs;
    Display *display;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    /***** create the panel *****/
    if (fileSelPanel == NULL) {
        fileSelPanel = create_fileSelectionPanel(mainPanelShell);
        UxFileSelectionShellContext =
            (_UxCfileSelectionShell *) UxGetContext(fileSelPanel);
        mainctxt = UxMainPanelShellContext;

        /***** define properties and callbacks of file selection panel */
        XtVaSetValues(fileSelectionShell,
            XmNtitle, "Input File Selection Panel",
            NULL );
        XtVaSetValues(fileSelectionBox,
            RES_CONVERT( XmNselectionLabelString, "Name of input file" ),
            NULL );
        XtRemoveAllCallbacks(fileSelectionBox, XmNokCallback);
        XtAddCallback( fileSelectionBox, XmNokCallback,
            (XtCallbackProc) clb_fileInputOK,
            (XtPointer) UxFileSelectionShellContext);
        XtRemoveAllCallbacks(fileSelectionBox, XmNcancelCallback);
        XtAddCallback(fileSelectionBox, XmNcancelCallback,
            (XtCallbackProc) clb_fileCancel,
            (XtPointer) UxFileSelectionShellContext);
        XtAddCallback( fileSelectionShell, XmNdestroyCallback,
            (XtCallbackProc) clb_destroyPanel,
            (XtPointer) UxMainPanelShellContext);
    }

    /***** popup the panel *****/
    UxPopupInterface(fileSelPanel, exclusive_grab);

    /***** change the cursor *****/
    display = XtDisplay(mainPanelShell);
    if (blckCursor==0) blckCursor = XCreateFontCursor(display, XC_dot);
    XDefineCursor(display, XtWindow(mainPanelShell), blckCursor);
    /*km_set_cursor(mainPanelShell,*/
    if (selCursor==0) selCursor = XCreateFontCursor(display, XC_hand2);
    XDefineCursor(display, XtWindow(fileSelPanel), selCursor);
}

/*******************************************************************************
 Callback for "Store all" menu item.
*******************************************************************************/
static void clb_fileStore(
Widget storeButton,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    for (iparline=0; iparline<npar; iparline++) {
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iparline]);
        par_store[iparline] = vpar[iparline];
        labelWriteFloat(parLineMenu_lstored, par_store[iparline]);
    }
}

/*******************************************************************************
 Callback for "Recall stored" menu item.
*******************************************************************************/
static void clb_fileRecall(
Widget recallButton,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;
    int updatemodesave;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    updatemodesave = updatemode;
    updatemode = UPDATE_DISABLED;
    for (iparline=0; iparline<npar; iparline++) {
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iparline]);
        /* set parameter value to the stored value */
        setParValueAndScale(par_store[iparline],
            UxParLineFormContext, PARCHN_OUTSIDE);
    }
    grupdate(UxMainPanelShellContext);
    updatemode = updatemodesave;
}

/*******************************************************************************
 Callback for "Recall starting" menu item.
*******************************************************************************/
static void clb_fileStart(
Widget startButton,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;
    int updatemodesave;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    updatemodesave = updatemode;
    updatemode = UPDATE_DISABLED;
    for (iparline=0; iparline<npar; iparline++) {
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iparline]);
        /* set parameter value to the starting value */
        setParValueAndScale(par_start[iparline],
            UxParLineFormContext, PARCHN_OUTSIDE);
    }
    grupdate(UxMainPanelShellContext);
    updatemode = updatemodesave;
}

/*******************************************************************************
 Callback for "Exit" menu item.
*******************************************************************************/
static void clb_fileExit(
Widget exitButton,
XtPointer client_data,
XtPointer call_data)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    XtDestroyWidget(mainPanelShell);
    endPanelEventLoop = True;
}

/***********************************************************************
 Register callbacks for file menu.
***********************************************************************/
void filemenuSetClbs(void *mctxt)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

    XtAddCallback(fileMenu_save, XmNactivateCallback,
            (XtCallbackProc) clb_fileSave,
            (XtPointer) UxMainPanelShellContext);
    XtAddCallback(fileMenu_input, XmNactivateCallback,
            (XtCallbackProc) clb_fileInput,
            (XtPointer) UxMainPanelShellContext);
    XtAddCallback(fileMenu_store, XmNactivateCallback,
            (XtCallbackProc) clb_fileStore,
            (XtPointer) UxMainPanelShellContext);
    XtAddCallback(fileMenu_recall, XmNactivateCallback,
            (XtCallbackProc) clb_fileRecall,
            (XtPointer) UxMainPanelShellContext);
    XtAddCallback(fileMenu_start, XmNactivateCallback,
            (XtCallbackProc) clb_fileStart,
            (XtPointer) UxMainPanelShellContext);
    XtAddCallback(fileMenu_exit, XmNactivateCallback,
            (XtCallbackProc) clb_fileExit,
            (XtPointer) UxMainPanelShellContext);
}
