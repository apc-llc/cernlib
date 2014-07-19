/*
 * $Id: minimenu.c,v 1.4 2000/01/05 14:55:50 couet Exp $
 *
 * $Log: minimenu.c,v $
 * Revision 1.4  2000/01/05 14:55:50  couet
 * - The array arglis in the procedure min_cmnd was not dimensioned
 *   correctly. It was dimensioned to 1 instead of 2.
 *
 * Revision 1.3  1999/03/01 09:11:08  couet
 * - previous commit was wrong
 *
 * Revision 1.2  1999/03/01 09:04:47  couet
 * - ERROR
 *
 * Revision 1.1.1.1  1996/03/01 11:39:08  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.41  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Callback functions for Minimize menu.
 =====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/MenuShell.h>
#include <Xm/ToggleBG.h>
#include <Xm/Text.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "hmotif/uxxt.h"
#include "fpanelsh/fplocal.h"
#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/parlineform.h"
#undef UxParent
#include "fpanelsh/parhdrform.h"
#undef UxParent
#include "fpanelsh/mainpanel.h"
#undef CONTEXT_MACRO_ACCESS

#ifdef __cplusplus
}  /* extern "C" */
#endif


static void blockParameterChange();
static void unblockParameterChange();


/***********************************************************************
 Call Minuit command with up to two arguments and update the panel.
***********************************************************************/
static int min_cmnd(char cmnd[],int narg, double arg1, double arg2)
{
    int ierflg;
    double arglis[2];
    /*int ncalls = 2*npar*npar+10;*/ /* number of calls for one time */
    int ncalls = (int)arg1;
    double fmin,fedm;
    int npari,nparx;
    int istat;

    arglis[1] = arg2;
    createBusyDialog(mainPanelShell, UxMainPanelShellContext);
    do {
        arglis[0] = ncalls;
        F77_CALLS_ECDIIE(mnexcm, fcn, cmnd, strlen(cmnd),
                         arglis, &narg, &ierflg, ufcn);
        processXEvents();
        get_mnparms(UxMainPanelShellContext, True);
        istat = 3;
    } while (istat != 3 && interrCompute==False);
    destroyBusyDialog(mainPanelShell, UxMainPanelShellContext);

    if (istat!=3 && interrCompute==True) {
        printf("Computing interrrupted by the user\n");
    }
    else {
        printf("Computing completed: istat=%d ierflg=%d\n",istat,ierflg);
    }

    if (ierflg) {
        printf("Error executing %s\n",cmnd);
    }
    return(ierflg);
}

/***********************************************************************
 A function to execute before each minimizing call.
***********************************************************************/
static void pre_min()
{
    blockParameterChange();
    set_mnparms(UxMainPanelShellContext);
}

/***********************************************************************
 A function to execute after each minimizing call.
***********************************************************************/
static void post_min(int ierflg)
{
    int iline;

    unblockParameterChange();

    if (ierflg == 0) {
        if (eparab_valid==False) {
            eparab_valid = True;
            for (iline=0; iline<npar; iline++) {
                UxParLineFormContext =
                    (_UxCparLineForm *) UxGetContext(parLines[iline]);
                utl_markTextValid(parEparabText);
            }
            utl_markTextValid(fminText);
            utl_markTextValid(fedmText);
        }
    }
}

/***********************************************************************
 Minimize menu item callback.
***********************************************************************/
void clb_minMinimize(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int ierflg;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    pre_min();

    ierflg = min_cmnd("minimize", 2, 10000.0, tolerance);
    get_mnglobcc(UxMainPanelShellContext);

    post_min(ierflg);
}

/***********************************************************************
 Migrad menu item callback.
***********************************************************************/
void clb_minMigrad(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int ierflg;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    pre_min();

    ierflg = min_cmnd("migrad", 2, 10000.0, tolerance);
    get_mnglobcc(UxMainPanelShellContext);

    post_min(ierflg);
}

/***********************************************************************
 Seek menu item callback.
***********************************************************************/
void clb_minSeek(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int ierflg;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    pre_min();

    ierflg = min_cmnd("seek", 2, 10000.0, devs);

    post_min(ierflg);
}

/***********************************************************************
 Simplex menu item callback.
***********************************************************************/
void clb_minSimplex(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int ierflg;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    pre_min();

    ierflg = min_cmnd("simplex", 2, 10000.0, tolerance);

    post_min(ierflg);
}

/***********************************************************************
 Improve menu item callback.
***********************************************************************/
void clb_minImprove(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int ierflg;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    pre_min();

    ierflg = min_cmnd("improve", 0, 0.0, 0.0);

    post_min(ierflg);
}

/***********************************************************************
 Call when computations are in progress and any user action changing
 parameters should be blocked
***********************************************************************/
static void blockParameterChange()
{
    int i;

    for (i=0; i<npar; i++) {
        UxParLineFormContext = (_UxCparLineForm *) UxGetContext(parLines[i]);

            /***** block scales, parameter, limits, and step text field **/
        parChangeDisable(UxParLineFormContext);

        XtVaSetValues(parScale, XmNsensitive, False, NULL);
    }

    /***** block settings affecting computation *****/

    /***** block minimize menu fields *****/
    XtVaSetValues(minimizeMenu, XmNsensitive, False, NULL);

    /***** block examine menu fields *****/
    XtVaSetValues(examineMenu, XmNsensitive, False, NULL);

    /***** block scan menu fields *****/
    XtVaSetValues(scanMenu, XmNsensitive, False, NULL);

    /***** block file menu fields *****/
    XtVaSetValues(fileMenu_recall, XmNsensitive, False, NULL);
    XtVaSetValues(fileMenu_start, XmNsensitive, False, NULL);
}

/***********************************************************************
 Call when computations are finished user actions changing parameters
 should again be permitted
***********************************************************************/
static void unblockParameterChange()
{
    int i;

    for (i=0; i<npar; i++) {
        UxParLineFormContext = (_UxCparLineForm *) UxGetContext(parLines[i]);

            /***** unblock scales, parameter, limits, and step text field */
        parChangeEnable(UxParLineFormContext);

        XtVaSetValues(parScale, XmNsensitive, True, NULL);
    }

    /***** unblock settings affecting computation *****/

    /***** unblock minimize menu fields *****/
    XtVaSetValues(minimizeMenu, XmNsensitive, True, NULL);

    /***** block examine menu fields *****/
    XtVaSetValues(examineMenu, XmNsensitive, True, NULL);

    /***** block scan menu fields *****/
    XtVaSetValues(scanMenu, XmNsensitive, True, NULL);

    /***** block file menu fields *****/
    XtVaSetValues(fileMenu_recall, XmNsensitive, True, NULL);
    XtVaSetValues(fileMenu_start, XmNsensitive, True, NULL);

}
