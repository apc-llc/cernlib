/*
 * $Id: scanmenu.c,v 1.1.1.1 1996/03/01 11:39:08 mclareni Exp $
 *
 * $Log: scanmenu.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:08  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.41  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Callback functions for Scan menu and for Scan Subpanel.
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
#include "fpanelsh/parlineform.h"
#undef mainctxt
#undef UxParent
#include "fpanelsh/scanpanel.h"
#undef UxParent
#include "fpanelsh/mainpanel.h"
#undef CONTEXT_MACRO_ACCESS

#ifdef __cplusplus
}  /* extern "C" */
#endif


/*******************************************************************************
     ####################### Scan panel callbacks ######################
*******************************************************************************/

/***********************************************************************
DESCRIPTION:
 Set context for the scan panel and the corresponding main panel.
ARGUMENT: A pointer to the scan panel context.
***********************************************************************/
static void scanPanelSetContext(void *spct)
{
    UxScanPanelShellContext = (_UxCscanPanelShell *) spct;
    UxMainPanelShellContext = (_UxCmainPanelShell *)mainctxt;
}

/***********************************************************************
 Num text field callback.
***********************************************************************/
static void clb_numRead(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    int prevnum = scan_num;
    scanPanelSetContext(client_data);

    textReadInt(text, &scan_numStat, &scan_num, nonEmptyIntOK);
    textIntError((TextReadCode)currErrorCode,
        "parameter", 0, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        if (scan_num < 1 || npar < scan_num) {
            textSetSemantError(text, &scan_numStat);
            km_warn_user(scanPanelShell, "Parameter number is invalid","");
            currErrorCode = TEXT_INVALID;
        }
        else {
            scan_xfrom = vscalemin[scan_num-1];
            scan_xfromStat.stat = TEXTSTAT_READOK;
            textWriteFloat(scanFromText, &scan_xfromStat, scan_xfrom);

            scan_xto = vscalemax[scan_num-1];
            scan_xtoStat.stat = TEXTSTAT_READOK;
            textWriteFloat(scanToText, &scan_xtoStat, scan_xto);

            textRemoveSemantError(text, &scan_numStat);
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
    scanPanelSetContext(client_data);

    textReadInt(text, &scan_nptStat, &scan_npt, positiveIntOK);
    textIntError((TextReadCode)currErrorCode,
        "number of points", 0, mainPanelShell);

    if (currErrorCode == TEXT_OK) {
        if (scan_xpt != NULL) free(scan_xpt);
        scan_xpt = (double *)malloc(2*scan_npt*sizeof(double));
        scan_ypt = scan_xpt+scan_npt;
    }
}

/***********************************************************************
 "From" text field callback.
***********************************************************************/
static void clb_xfromRead(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    scanPanelSetContext(client_data);

    textReadFloat(text, &scan_xfromStat, &scan_xfrom, nonEmptyFloatOK);
    textFloatError((TextReadCode)currErrorCode,
        "xfrom", 0, mainPanelShell);
}

/***********************************************************************
 "To" text field callback.
***********************************************************************/
static void clb_xtoRead(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    scanPanelSetContext(client_data);

    textReadFloat(text, &scan_xtoStat, &scan_xto, nonEmptyFloatOK);
    textFloatError((TextReadCode)currErrorCode,
        "xto", 0, mainPanelShell);
}

/***********************************************************************
 "Compute" button callback.
***********************************************************************/
static void clb_scanCompute(
Widget button,
XtPointer client_data,
XtPointer call_data)
{
    int pnum;
    float xcurr,xdel;
    int i;
    int iflag = 0;
    double pval, ydummy;

    scanPanelSetContext(client_data);

    if (scan_numStat.stat!=TEXTSTAT_READOK ||
        scan_nptStat.stat!=TEXTSTAT_READOK ||
        scan_xtoStat.stat!=TEXTSTAT_READOK ||
        scan_xfromStat.stat!=TEXTSTAT_READOK)
    {
        return;
    }

    createBusyDialog(scanPanelShell, UxMainPanelShellContext);

    pnum = scan_num - 1;
    xdel = (scan_xto-scan_xfrom)/(scan_npt-1);
    xcurr = scan_xfrom;
    pval = vpar[pnum];
    for (i=0; i<scan_npt; i++) {
        vpar[pnum] = scan_xpt[i] = xcurr;
        ((FCNTYPE)fcn)(&npar,NULL,&scan_ypt[i],vpar,&iflag,ufcn);
        xcurr += xdel;
    }
    vpar[pnum] = pval;
    /***** this call restores internally kept parameter values for ufcn */
    ((FCNTYPE)fcn)(&npar,NULL,&ydummy,vpar,&iflag,ufcn);

    hffplot(scan_npt, scan_xpt, scan_ypt,
       parname[scan_num-1], fmodel, FuncPlot, False);

    destroyBusyDialog(scanPanelShell, UxMainPanelShellContext);
}

/***********************************************************************
 Close button callback which pop down the Scan Subpanel.
***********************************************************************/
static void clb_scanClose(
Widget button,
XtPointer client_data,
XtPointer call_data)
{
    scanPanelSetContext(client_data);
    UxPopdownInterface(scanPanelShell);
}

/***********************************************************************
 Close button callback which destroys the Scan Subpanel.
***********************************************************************/
static void clb_closeDestroyPanel(
Widget shell,
XtPointer client_data,
XtPointer call_data)
{
    scanPanelSetContext(client_data);
    XtDestroyWidget(scanPanel);
}

/***********************************************************************
 This callback is executed by Motif when the panel is destroyed.
***********************************************************************/
static void clb_destroyPanel(
Widget shell,
XtPointer client_data,
XtPointer call_data)
{
    scanPanelSetContext(client_data);
    if (scan_xpt != NULL) free(scan_xpt);
    scan_xpt = NULL;
    scanPanel = NULL;
}

/***********************************************************************
 Create Scan Subpanel widgets, register callbacks, and set initial
 values.
***********************************************************************/
Widget createScanPanel(void *mctxt)
{
    Dimension hText;
    Atom xa_wm_delete_window;
    Widget spshell;

    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

    spshell = create_scanPanel(mainPanelShell);
    UxScanPanelShellContext =
        (_UxCscanPanelShell *) UxGetContext(spshell);
    mainctxt = mctxt;

    /***** XmNinitialFocus is not defined on all platforms *****/
#ifdef XmNinitialFocus
    XtVaSetValues(scanForm, XmNinitialFocus, scanComputeButton, NULL );
#endif

    XtVaSetValues(scanPanelShell,XmNdeleteResponse,XmDO_NOTHING,NULL);
    xa_wm_delete_window = XmInternAtom(XtDisplay(scanPanelShell),
        "WM_DELETE_WINDOW",True);
    XmAddWMProtocolCallback(scanPanelShell,xa_wm_delete_window,
               clb_scanClose, (XtPointer)UxScanPanelShellContext);

        /***** set geometry *****/
    utlQueryDimensions(scanNumText,NULL,&hText);
    XtVaSetValues(scanNumLabel, XmNheight, hText, NULL);
    XtVaSetValues(scanNptLabel, XmNheight, hText, NULL);
    XtVaSetValues(scanFromLabel, XmNheight, hText, NULL);
    XtVaSetValues(scanToLabel, XmNheight, hText, NULL);

    XtAddCallback( scanPanelShell, XmNdestroyCallback,
            (XtCallbackProc) clb_destroyPanel,
            (XtPointer) UxScanPanelShellContext);

    XtAddCallback( scanNumText, XmNactivateCallback,
            (XtCallbackProc) clb_numRead,
            (XtPointer) UxScanPanelShellContext);
    XtAddCallback( scanNumText, XmNlosingFocusCallback,
            (XtCallbackProc) clb_numRead,
            (XtPointer) UxScanPanelShellContext);
    XtAddCallback(scanNumText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &scan_numStat);

    XtAddCallback( scanNptText, XmNactivateCallback,
            (XtCallbackProc) clb_nptRead,
            (XtPointer) UxScanPanelShellContext);
    XtAddCallback( scanNptText, XmNlosingFocusCallback,
            (XtCallbackProc) clb_nptRead,
            (XtPointer) UxScanPanelShellContext);
    XtAddCallback(scanNptText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &scan_nptStat);

    XtAddCallback( scanFromText, XmNactivateCallback,
            (XtCallbackProc) clb_xfromRead,
            (XtPointer) UxScanPanelShellContext);
    XtAddCallback( scanFromText, XmNlosingFocusCallback,
            (XtCallbackProc) clb_xfromRead,
            (XtPointer) UxScanPanelShellContext);
    XtAddCallback(scanFromText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &scan_xfromStat);

    XtAddCallback( scanToText, XmNactivateCallback,
            (XtCallbackProc) clb_xtoRead,
            (XtPointer) UxScanPanelShellContext);
    XtAddCallback( scanToText, XmNlosingFocusCallback,
            (XtCallbackProc) clb_xtoRead,
            (XtPointer) UxScanPanelShellContext);
    XtAddCallback(scanToText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &scan_xtoStat);

    XtAddCallback( scanComputeButton, XmNactivateCallback,
            (XtCallbackProc) clb_scanCompute,
            (XtPointer) UxScanPanelShellContext);

    XtAddCallback( scanCloseButton, XmNactivateCallback,
            (XtCallbackProc) clb_scanClose,
            (XtPointer) UxScanPanelShellContext);

    XtAddCallback( scanHelpButton, XmNactivateCallback,
            (XtCallbackProc) km_help_cb,
            (XtPointer) fp_scanpanel_help_text);

    textWriteInt(scanNumText, &scan_numStat, scan_num);
    scan_numStat.change = TEXTSTAT_CHANGED;
    clb_numRead(scanNumText,
        (XtPointer)UxScanPanelShellContext, (XtPointer)NULL);

    textWriteInt(scanNptText, &scan_nptStat, scan_npt);
    scan_nptStat.change = TEXTSTAT_CHANGED;
    clb_nptRead(scanNptText,
        (XtPointer)UxScanPanelShellContext, (XtPointer)NULL);

    return(spshell);
}

/***********************************************************************
 Close scan panel. Equivalent to activating scanCloseButton.
***********************************************************************/
void closeScanPanel(void *mctxt)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *) mctxt;
    UxScanPanelShellContext =
        (_UxCscanPanelShell *)UxGetContext(scanPanel);

    if (scanPanel != NULL) {
        XtCallCallbacks(scanCloseButton, XmNactivateCallback, NULL);
    }
}


/*******************************************************************************
     ####################### Scan menu callbacks #######################
*******************************************************************************/

/***********************************************************************
 (Create and) popup Scan Panel. If the panel have been created before,
 it is popped down when closed, otherwise it is destroyed.
***********************************************************************/
void clb_scanOnedim(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    Atom xa_wm_delete_window;

    UxMainPanelShellContext = (_UxCmainPanelShell *)client_data;
    UxScanPanelShellContext =
            (_UxCscanPanelShell *)UxGetContext(scanPanel);

    if (scanPanel==NULL) {
        scanPanel = createScanPanel(client_data);
        xa_wm_delete_window = XmInternAtom(XtDisplay(scanPanelShell),
            "WM_DELETE_WINDOW",True);
        XmAddWMProtocolCallback(scanPanelShell,xa_wm_delete_window,
            clb_closeDestroyPanel, (XtPointer)UxScanPanelShellContext);
        XtAddCallback( scanCloseButton, XmNactivateCallback,
            (XtCallbackProc) clb_closeDestroyPanel,
            (XtPointer) UxScanPanelShellContext);
    }

    UxPopupInterface(scanPanel, no_grab);
}
