/*
 * $Id: examenu.c,v 1.1.1.1 1996/03/01 11:39:07 mclareni Exp $
 *
 * $Log: examenu.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:07  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.40  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Callback functions for Examine menu and for Contour Subpanel.
 =====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/MenuShell.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/ToggleBG.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "fpanelsh/errordb.h"
#include "fpanelsh/fplocal.h"
#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/parlineform.h"
#undef mainctxt
#undef UxParent
#include "fpanelsh/contpanel.h"
#undef UxParent
#include "fpanelsh/mainpanel.h"
#undef CONTEXT_MACRO_ACCESS

#ifdef __cplusplus
}  /* extern "C" */
#endif


/*******************************************************************************
     ####################### Contour panel callbacks ######################
*******************************************************************************/

/***********************************************************************
DESCRIPTION:
 Set context for the contour panel and the corresponding main panel.
ARGUMENT: A pointer to the contour panel context.
***********************************************************************/
static void contPanelSetContext(void *cpct)
{
    UxContourPanelShellContext = (_UxCcontourPanelShell *) cpct;
    UxMainPanelShellContext = (_UxCmainPanelShell *)mainctxt;
}

/***********************************************************************
 Num1 text field callback.
***********************************************************************/
static void clb_num1Read(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    contPanelSetContext(client_data);

    textReadInt(text, &cont_num1Stat, &cont_num1, nonEmptyIntOK);
    textIntError((TextReadCode)currErrorCode,
        "parameter 1", 0, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        if (cont_num1 < 1 || npar < cont_num1) {
            textSetSemantError(text, &cont_num1Stat);
            km_warn_user(contourPanelShell, "Parameter number is invalid","");
            currErrorCode = TEXT_INVALID;
        }
        else {
            textRemoveSemantError(text, &cont_num1Stat);
        }
    }
}

/***********************************************************************
 Num2 text field callback.
***********************************************************************/
static void clb_num2Read(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    contPanelSetContext(client_data);

    textReadInt(text, &cont_num2Stat, &cont_num2, nonEmptyIntOK);
    textIntError((TextReadCode)currErrorCode,
        "parameter 2", 0, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        if (cont_num2 < 1 || npar < cont_num2) {
            textSetSemantError(text, &cont_num2Stat);
            km_warn_user(contourPanelShell, "Parameter number is invalid","");
            currErrorCode = TEXT_INVALID;
        }
        else {
            textRemoveSemantError(text, &cont_num2Stat);
        }
    }
}

/***********************************************************************
 Npt text field callback.
***********************************************************************/
static void clb_nptRead(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    contPanelSetContext(client_data);

    textReadInt(text, &cont_nptStat, &cont_npt, positiveIntOK);
    textIntError((TextReadCode)currErrorCode,
        "number of points", 0, mainPanelShell);

    if (currErrorCode == TEXT_OK) {
        if (cont_xpt != NULL) free(cont_xpt);
        cont_xpt = (double *)malloc(2*(cont_npt+1)*sizeof(double));
        cont_ypt = cont_xpt+cont_npt+1;
    }
}

/***********************************************************************
 Compute button callback.
***********************************************************************/
static void clb_contCompute(
Widget button,
XtPointer client_data,
XtPointer call_data)
{
    int ncont;
    char textstr[5];

    contPanelSetContext(client_data);

    if (cont_num1Stat.stat!=TEXTSTAT_READOK ||
        cont_num2Stat.stat!=TEXTSTAT_READOK ||
        cont_nptStat.stat!=TEXTSTAT_READOK)
    {
        return;
    }

    textWriteEmpty(contNfoundText, NULL);

    createBusyDialog(contourPanelShell, UxMainPanelShellContext);
    F77_CALLS_EIIIDDIE(mncont,fcn,
        &cont_num1,&cont_num2,&cont_npt,cont_xpt,cont_ypt,&cont_nfound,ufcn);
    destroyBusyDialog(contourPanelShell, UxMainPanelShellContext);
    get_mnglobcc(UxMainPanelShellContext);

    /***** this call restores internally kept parameter values for ufcn */
    {double ydummy; int iflag = 0;
    ((FCNTYPE)fcn)(&npar,NULL,&ydummy,vpar,&iflag,ufcn);
    }
    if (cont_nfound == 0) { /* less than 4 points found */
        printf("Less than 4 points found\n");
    }
    else if (cont_nfound < 0) { /* input arguments are not valid */
        printf("Input arguments are not valid\n");
    }
    else {
        sprintf(textstr,"%d",cont_nfound);
        textWriteName(contNfoundText, NULL, textstr);

        cont_xpt[cont_nfound] = cont_xpt[0];
        cont_ypt[cont_nfound] = cont_ypt[0];
        ncont = cont_nfound+1;

        hffplot(ncont, cont_xpt, cont_ypt,
            parname[cont_num1-1], parname[cont_num2-1],
            ContPlot, cont_overlay);
    }
}

/***********************************************************************
 "Save in PAW vectors" button callback.
***********************************************************************/
static void clb_contSave(
Widget button,
XtPointer client_data,
XtPointer call_data)
{
    char vname[MAX_PAWVECNAME+1];

    contPanelSetContext(client_data);

    cont_xSaveStat.change = TEXTSTAT_CHANGED;
    textReadName(contXsaveText, &cont_xSaveStat, vname, nonEmptyNameOK);
    textNameError((TextReadCode)currErrorCode,
        "PAW vector", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        fpsavevc(vname, cont_nfound+1, cont_xpt, mainPanelShell);
    }
    cont_ySaveStat.change = TEXTSTAT_CHANGED;
    textReadName(contYsaveText, &cont_ySaveStat, vname, nonEmptyNameOK);
    textNameError((TextReadCode)currErrorCode,
        "PAW vector", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        fpsavevc(vname, cont_nfound+1, cont_ypt, mainPanelShell);
    }
}

/***********************************************************************
 Overlay toggle callback.
***********************************************************************/
static void clb_overlayToggle(
Widget button,
XtPointer client_data,
XtPointer call_data)
{
    contPanelSetContext(client_data);
    cont_overlay = XmToggleButtonGetState(button);
}

/***********************************************************************
 Close button callback which pop down the Contour Subpanel.
***********************************************************************/
static void clb_contClose(
Widget button,
XtPointer client_data,
XtPointer call_data)
{
    contPanelSetContext(client_data);
    UxPopdownInterface(contourPanelShell);
}

/***********************************************************************
 Close button callback which destroys the Contour Subpanel.
***********************************************************************/
static void clb_closeDestroyPanel(
Widget shell,
XtPointer client_data,
XtPointer call_data)
{
    contPanelSetContext(client_data);
    XtDestroyWidget(contourPanel);
}

/***********************************************************************
 This callback is executed by Motif when the panel is destroyed.
***********************************************************************/
static void clb_destroyPanel(
Widget shell,
XtPointer client_data,
XtPointer call_data)
{
    contPanelSetContext(client_data);

    if (cont_xpt != NULL) free(cont_xpt);
    cont_xpt = NULL;
    contourPanel = NULL;
}

/***********************************************************************
 Create Contour Subpanel widgets, register callbacks, and set initial
 values.
***********************************************************************/
Widget createContourPanel(void *mctxt)
{
    Dimension hText;
    Atom xa_wm_delete_window;
    Widget cpshell;

    UxMainPanelShellContext = (_UxCmainPanelShell *) mctxt;

    cpshell = create_contourPanel(mainPanelShell);
    UxContourPanelShellContext =
        (_UxCcontourPanelShell *) UxGetContext(cpshell);
    mainctxt = mctxt;

    /***** XmNinitialFocus is not defined on all platforms *****/
#ifdef XmNinitialFocus
    XtVaSetValues(contourForm, XmNinitialFocus, contComputeButton, NULL );
#endif

    XtVaSetValues(contourPanelShell,XmNdeleteResponse,XmDO_NOTHING,NULL);
    xa_wm_delete_window = XmInternAtom(XtDisplay(contourPanelShell),
        "WM_DELETE_WINDOW",True);
    XmAddWMProtocolCallback(contourPanelShell,xa_wm_delete_window,
        clb_contClose, (XtPointer)UxContourPanelShellContext);

        /***** set geometry *****/
    utlQueryDimensions(contNum1Text,NULL,&hText);
    XtVaSetValues(contNum1Label, XmNheight, hText, NULL);
    XtVaSetValues(contNum2Label, XmNheight, hText, NULL);
    XtVaSetValues(contNptLabel, XmNheight, hText, NULL);
    XtVaSetValues(contNfoundLabel, XmNheight, hText, NULL);
    XtVaSetValues(contXsaveLabel, XmNheight, hText, NULL);
    XtVaSetValues(contYsaveLabel, XmNheight, hText, NULL);

    XtAddCallback( contourPanelShell, XmNdestroyCallback,
            (XtCallbackProc) clb_destroyPanel,
            (XtPointer) UxContourPanelShellContext);

    XtAddCallback( contNum1Text, XmNactivateCallback,
            (XtCallbackProc) clb_num1Read,
            (XtPointer) UxContourPanelShellContext);
    XtAddCallback( contNum1Text, XmNlosingFocusCallback,
            (XtCallbackProc) clb_num1Read,
            (XtPointer) UxContourPanelShellContext);
    XtAddCallback(contNum1Text, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &cont_num1Stat);

    XtAddCallback( contNum2Text, XmNactivateCallback,
            (XtCallbackProc) clb_num2Read,
            (XtPointer) UxContourPanelShellContext);
    XtAddCallback( contNum2Text, XmNlosingFocusCallback,
            (XtCallbackProc) clb_num2Read,
            (XtPointer) UxContourPanelShellContext);
    XtAddCallback(contNum2Text, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &cont_num2Stat);

    XtAddCallback( contNptText, XmNactivateCallback,
            (XtCallbackProc) clb_nptRead,
            (XtPointer) UxContourPanelShellContext);
    XtAddCallback( contNptText, XmNlosingFocusCallback,
            (XtCallbackProc) clb_nptRead,
            (XtPointer) UxContourPanelShellContext);
    XtAddCallback(contNptText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &cont_nptStat);

    XtAddCallback( contComputeButton, XmNactivateCallback,
            (XtCallbackProc) clb_contCompute,
            (XtPointer) UxContourPanelShellContext);

    XtAddCallback( contCloseButton, XmNactivateCallback,
            (XtCallbackProc) clb_contClose,
            (XtPointer) UxContourPanelShellContext);

    XtAddCallback( contHelpButton, XmNactivateCallback,
            (XtCallbackProc) km_help_cb,
            (XtPointer) fp_contpanel_help_text);

    XtAddCallback( contSaveButton, XmNactivateCallback,
            (XtCallbackProc) clb_contSave,
            (XtPointer) UxContourPanelShellContext);

    XtAddCallback( contOverlayToggle, XmNvalueChangedCallback,
            (XtCallbackProc) clb_overlayToggle,
            (XtPointer) UxContourPanelShellContext);

    textWriteInt(contNum1Text, &cont_num1Stat, cont_num1);
    cont_num1Stat.change = TEXTSTAT_CHANGED;
    clb_num1Read(contNum1Text,
        (XtPointer)UxContourPanelShellContext, (XtPointer)NULL);

    textWriteInt(contNum2Text, &cont_num2Stat, cont_num2);
    cont_num2Stat.change = TEXTSTAT_CHANGED;
    clb_num2Read(contNum2Text,
        (XtPointer)UxContourPanelShellContext, (XtPointer)NULL);

    textWriteInt(contNptText, &cont_nptStat, cont_npt);
    cont_nptStat.change = TEXTSTAT_CHANGED;
    clb_nptRead(contNptText,
        (XtPointer)UxContourPanelShellContext, (XtPointer)NULL);

    textWriteEmpty(contNfoundText, NULL);

    textWriteName(contXsaveText,&cont_xSaveStat,cont_xvecnam);
    textWriteName(contYsaveText,&cont_ySaveStat,cont_yvecnam);

    return(cpshell);
}

/***********************************************************************
 Close contour panel. Equivalent to activating contCloseButton.
***********************************************************************/
void closeContourPanel(void *mctxt)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *) mctxt;
    UxContourPanelShellContext =
        (_UxCcontourPanelShell *)UxGetContext(contourPanel);

    if (contourPanel != NULL) {
        XtCallCallbacks(contCloseButton, XmNactivateCallback, NULL);
    }
}


/*******************************************************************************
     ####################### Examine menu callbacks #######################
*******************************************************************************/

/***********************************************************************
 (Create and) popup contour panel. If the panel have been created before,
 it is popped down when closed, otherwise it is destroyed.
***********************************************************************/
void clb_exaContour(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    Atom xa_wm_delete_window;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;
    UxContourPanelShellContext =
        (_UxCcontourPanelShell *)UxGetContext(contourPanel);

    if (contourPanel==NULL) {
        contourPanel = createContourPanel(client_data);
        xa_wm_delete_window = XmInternAtom(XtDisplay(contourPanelShell),
            "WM_DELETE_WINDOW",True);
        XmAddWMProtocolCallback(contourPanelShell,xa_wm_delete_window,
            clb_closeDestroyPanel, (XtPointer)UxContourPanelShellContext);
        XtAddCallback( contCloseButton, XmNactivateCallback,
            (XtCallbackProc) clb_closeDestroyPanel,
            (XtPointer) UxContourPanelShellContext);
    }

    UxPopupInterface(contourPanel, no_grab);
}

/***********************************************************************
 Hesse menu item callback.
***********************************************************************/
void clb_exaHesse(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "hesse";
    int icondn;
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    createBusyDialog(mainPanelShell, UxMainPanelShellContext);
    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
    destroyBusyDialog(mainPanelShell, UxMainPanelShellContext);
    get_mnglobcc(UxMainPanelShellContext);
}

/*******************************************************************************
 Minos menu item callback.
*******************************************************************************/
void clb_exaMinos(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "minos";
    int icondn;
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    createBusyDialog(mainPanelShell, UxMainPanelShellContext);
    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
    destroyBusyDialog(mainPanelShell, UxMainPanelShellContext);
    if (icondn==0) {
        /***** read minuit errors *****/
        get_mneminos(UxMainPanelShellContext);
        get_mnglobcc(UxMainPanelShellContext);
    }
    else {
        km_warn_user(mainPanelShell, "Unable to compute MINOS errors","");
        return;
    }
}

/*******************************************************************************
 Correlations menu item callback.
*******************************************************************************/
void clb_exaCorrelation(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "sho cor";
    int icondn;
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
}

/*******************************************************************************
 Covariance menu item callback.
*******************************************************************************/
void clb_exaCovariance(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "sho cov";
    int icondn;
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
}

/*******************************************************************************
 Eigenvalues menu item callback.
*******************************************************************************/
void clb_exaEigenvalues(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "sho eig";
    int icondn;
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
}
