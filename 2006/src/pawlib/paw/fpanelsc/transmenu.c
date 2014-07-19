/*
 * $Id: transmenu.c,v 1.1.1.1 1996/03/01 11:39:09 mclareni Exp $
 *
 * $Log: transmenu.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:09  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.42  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Callback functions for Transfer menu and for Vector Subpanel.
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

#include "hmotif/uxxt.h"
#include "fpanelsh/errordb.h"
#include "fpanelsh/fplocal.h"
#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/vectpanel.h"
#undef UxParent
#include "fpanelsh/mainpanel.h"
#undef CONTEXT_MACRO_ACCESS

#ifdef __cplusplus
}  /* extern "C" */
#endif


/*******************************************************************************
     ####################### Vector panel callbacks ######################
*******************************************************************************/

/***********************************************************************
DESCRIPTION:
 Set context for the vector panel and the corresponding main panel.
ARGUMENT: A pointer to the vector panel context.
***********************************************************************/
static void vectPanelSetContext(void *vpct)
{
    UxVectPanelShellContext = (_UxCvectPanelShell *) vpct;
    UxMainPanelShellContext = (_UxCmainPanelShell *)mainctxt;
}

/***********************************************************************
 "Save in PAW vectors" button callback.
***********************************************************************/
static void clb_vectSave(
Widget button,
XtPointer client_data,
XtPointer call_data)
{
    char vname[80];

    vectPanelSetContext(client_data);

    vparStat.change = TEXTSTAT_CHANGED;
    textReadName(vectParText, &vparStat, vname, anyNameOrEmptyOK);
    textNameError((TextReadCode)currErrorCode,
        "PAW vector", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        fpsavevc(vname, npar, vpar, mainPanelShell);
    }

    vstepStat.change = TEXTSTAT_CHANGED;
    textReadName(vectStepText, &vstepStat, vname, anyNameOrEmptyOK);
    textNameError((TextReadCode)currErrorCode,
        "PAW vector", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        fpsavevc(vname, npar, vstep, mainPanelShell);
    }

    vlowStat.change = TEXTSTAT_CHANGED;
    textReadName(vectLowText, &vlowStat, vname, anyNameOrEmptyOK);
    textNameError((TextReadCode)currErrorCode,
        "PAW vector", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        fpsavevc(vname, npar, vlow, mainPanelShell);
    }

    vuppStat.change = TEXTSTAT_CHANGED;
    textReadName(vectUppText, &vuppStat, vname, anyNameOrEmptyOK);
    textNameError((TextReadCode)currErrorCode,
        "PAW vector", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        fpsavevc(vname, npar, vupp, mainPanelShell);
    }

    veparabStat.change = TEXTSTAT_CHANGED;
    textReadName(vectErrorText, &veparabStat, vname, anyNameOrEmptyOK);
    textNameError((TextReadCode)currErrorCode,
        "PAW vector", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        fpsavevc(vname, npar, veparab, mainPanelShell);
    }
}

/***********************************************************************
 Close button callback which pop down the Scan Subpanel.
***********************************************************************/
static void clb_vectClose(
Widget button,
XtPointer client_data,
XtPointer call_data)
{
    vectPanelSetContext(client_data);
    UxPopdownInterface(vectPanelShell);
}

/***********************************************************************
 Close button callback which destroys the Scan Subpanel.
***********************************************************************/
static void clb_closeDestroyPanel(
Widget shell,
XtPointer client_data,
XtPointer call_data)
{
    vectPanelSetContext(client_data);
    XtDestroyWidget(vectPanel);
}

/***********************************************************************
 This callback is executed by Motif when the panel is destroyed.
***********************************************************************/
static void clb_destroyPanel(
Widget shell,
XtPointer client_data,
XtPointer call_data)
{
    vectPanelSetContext(client_data);
    vectPanel = NULL;
}

/***********************************************************************
 Create Vector Subpanel widgets, register callbacks, and set initial
 values.
***********************************************************************/
Widget createVectPanel(void *mctxt)
{
    Dimension hText;
    Atom xa_wm_delete_window;
    Widget vpshell;

    UxMainPanelShellContext = (_UxCmainPanelShell *) mctxt;

    vpshell = create_vectPanel(mainPanelShell);
    UxVectPanelShellContext =
        (_UxCvectPanelShell *) UxGetContext(vpshell);
    mainctxt = mctxt;

    XtVaSetValues(vectPanelShell,XmNdeleteResponse,XmDO_NOTHING,NULL);
    xa_wm_delete_window = XmInternAtom(XtDisplay(vectPanelShell),
        "WM_DELETE_WINDOW",True);
    XmAddWMProtocolCallback(vectPanelShell,xa_wm_delete_window,
               clb_vectClose, (XtPointer)UxVectPanelShellContext);

        /***** set geometry *****/
    utlQueryDimensions(vectParText,NULL,&hText);
    XtVaSetValues(vectParLabel, XmNheight, hText, NULL);
    XtVaSetValues(vectStepLabel, XmNheight, hText, NULL);
    XtVaSetValues(vectLowLabel, XmNheight, hText, NULL);
    XtVaSetValues(vectUppLabel, XmNheight, hText, NULL);
    XtVaSetValues(vectErrorLabel, XmNheight, hText, NULL);

    XtAddCallback( vectPanelShell, XmNdestroyCallback,
            (XtCallbackProc) clb_destroyPanel,
            (XtPointer) UxVectPanelShellContext);

    XtAddCallback( vectSaveButton, XmNactivateCallback,
            (XtCallbackProc) clb_vectSave,
            (XtPointer) UxVectPanelShellContext);

    XtAddCallback( vectCloseButton, XmNactivateCallback,
            (XtCallbackProc) clb_vectClose,
            (XtPointer) UxVectPanelShellContext);

    XtAddCallback( vectHelpButton, XmNactivateCallback,
            (XtCallbackProc) km_help_cb,
            (XtPointer) fp_transpanel_help_text);

    textWriteName(vectParText,&vparStat,vec_pnam);
    textWriteName(vectStepText,&vstepStat,vec_snam);
    textWriteName(vectLowText,&vlowStat,vec_lnam);
    textWriteName(vectUppText,&vuppStat,vec_unam);
    textWriteName(vectErrorText,&veparabStat,vec_enam);

    return(vpshell);
}

/***********************************************************************
 Close vector panel. Equivalent to activating vectCloseButton.
***********************************************************************/
void closeVectPanel(void *mctxt)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *) mctxt;
    UxVectPanelShellContext =
        (_UxCvectPanelShell *)UxGetContext(vectPanel);

    if (vectPanel != NULL) {
        XtCallCallbacks(vectCloseButton, XmNactivateCallback, NULL);
    }
}


/*******************************************************************************
     ####################### Transfer menu callbacks #######################
*******************************************************************************/

/***********************************************************************
 (Create and) popup Vector Panel. If the panel have been created before,
 it is popped down when closed, otherwise it is destroyed.
***********************************************************************/
static void clb_vectors(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    Atom xa_wm_delete_window;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    if (vectPanel==NULL) {
        vectPanel = createVectPanel(client_data);
        xa_wm_delete_window = XmInternAtom(XtDisplay(vectPanelShell),
            "WM_DELETE_WINDOW",True);
        XmAddWMProtocolCallback(vectPanelShell,xa_wm_delete_window,
            clb_closeDestroyPanel, (XtPointer)UxVectPanelShellContext);
        XtAddCallback( vectCloseButton, XmNactivateCallback,
            (XtCallbackProc) clb_closeDestroyPanel,
            (XtPointer) UxVectPanelShellContext);
    }

    UxPopupInterface(vectPanel, no_grab);
}

/***********************************************************************
 Register callbacks for Transfer menu.
***********************************************************************/
void transmenuSetClbs(void *mctxt)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

    XtAddCallback(transMenu_vect, XmNactivateCallback,
            (XtCallbackProc) clb_vectors,
            (XtPointer) UxMainPanelShellContext);
}
