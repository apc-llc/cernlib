/*
 * $Id: adjust.c,v 1.1.1.1 1996/03/01 11:39:06 mclareni Exp $
 *
 * $Log: adjust.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:06  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.40  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   08/04/95*/

/*******************************************************************************
 Callback functions for scale interval adjust device.
 =====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <math.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "hmotif/uxxt.h"
#include "fpanelsh/errordb.h"
#include "fpanelsh/fplocal.h"
#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/parlineform.h"
#undef UxParent
#include "fpanelsh/mainpanel.h"
#undef CONTEXT_MACRO_ACCESS

#ifdef __cplusplus
}  /* extern "C" */
#endif

static void clb_startRepeatClick(
Widget widget,
XtPointer client_data,
XtPointer call_data);

static void clb_endRepeatClick(
Widget widget,
XtPointer client_data,
XtPointer call_data);

static double changeStep(double xmin, double xmax);

/***********************************************************************
DESCRIPTION:
 Set context for a parameter line and the corresponding main panel.
ARGUMENT: A pointer to the parameter line context.
***********************************************************************/
static void parlineSetContext(void *plct)
{
    UxParLineFormContext = (_UxCparLineForm *) plct;
    UxMainPanelShellContext = (_UxCmainPanelShell *)mainctxt;
}

/***********************************************************************
 Disable scale
***********************************************************************/
static void parScaleDisable(void *plctxt)
{
    parlineSetContext(plctxt);

    XtVaSetValues(parScale, XmNsensitive, False, NULL);
}

/***********************************************************************
 Enable scale
***********************************************************************/
static void parScaleEnable(void *plctxt)
{
    parlineSetContext(plctxt);

    XtVaSetValues(parScale, XmNsensitive, True, NULL);
    setScaleWorld(parScale, vpar[parLineNumber], plctxt);
}

/***********************************************************************
 Modify value of dmin to make it as whole as possible with reasonable
 decrease of the interval.
***********************************************************************/
double roundUpScaleMin(double dmin, double dmax)
{
    double cstep;
    double dmin1;

    cstep = changeStep(dmin,dmax);

    dmin1 = floor(dmin/cstep+1.0)*cstep;
    if (dmin1 == dmin) dmin = dmin1 + cstep;
    else dmin = dmin1;

    return(dmin);
}

/***********************************************************************
 Modify value of dmin to make it as whole as possible with reasonable
 increase of the interval.
***********************************************************************/
double roundDnScaleMin(double dmin, double dmax)
{
    double cstep;
    double dmin1;

    cstep = changeStep(dmin,dmax);

    dmin1 = ceil(dmin/cstep-1.0)*cstep;
    if (dmin1 == dmin) dmin = dmin1 - cstep;
    else dmin = dmin1;

    return(dmin);
}

/***********************************************************************
 Modify value of dmax to make it as whole as possible with reasonable
 increase of the interval.
***********************************************************************/
double roundUpScaleMax(double dmin, double dmax)
{
    double cstep;
    double dmax1;

    cstep = changeStep(dmin,dmax);

    dmax1 = floor(dmax/cstep+1.0)*cstep;
    if (dmax1 == dmax) dmax = dmax1 + cstep;
    else dmax = dmax1;

    return(dmax);
}

/***********************************************************************
 Modify value of dmax to make it as whole as possible with reasonable
 decrease of the interval.
***********************************************************************/
double roundDnScaleMax(double dmin, double dmax)
{
    double cstep;
    double dmax1;

    cstep = changeStep(dmin,dmax);

    dmax1 = ceil(dmax/cstep-1.0)*cstep;
    if (dmax1 == dmax) dmax = dmax1 - cstep;
    else dmax = dmax1;

    return(dmax);
}

/***********************************************************************
 Set new scale interval. If limits are valid (this means also that the
 parameter value is inside the limits), the scale interval is inside
 the limits.
 If necessary, write new scale min and max into their fields.
 Find the new scale status and perform necessary transition actions.
***********************************************************************/
void setScaleInterval(double dmin, double dmax, void *plctxt, int reason)
{
    double dlow;
    double dupp;
    double dpar;
    ScaleStatCode newScaleStat = SCALESTAT_OK;
    ScaleStatCode prevScaleStat;
    int iparline;

    parlineSetContext(plctxt);
    iparline = parLineNumber;

    dlow = vlow[iparline];
    dupp = vupp[iparline];
    dpar = vpar[iparline];

    currErrorCode = SCALE_NOERR;

    prevScaleStat = (ScaleStatCode)parScaleStat;

    /****** force the scale interval to be inside valid limits ******/
    if (parLimitsStat == LIMITSTAT_OK) { /* check limits */
        if (dmin < dlow) {
            dmin = dlow;
            currErrorCode = SCALE_OUTLIMITS;
        }
        if (dmin > dupp) {
            dmin = dupp;
            currErrorCode = SCALE_OUTLIMITS;
        }
        if (dmax > dupp) {
            dmax = dupp;
            currErrorCode = SCALE_OUTLIMITS;
        }
        if (dmax < dlow) {
            dmax = dlow;
            currErrorCode = SCALE_OUTLIMITS;
        }
    }

    /****** make necessary changes ******/
    if (reason != MINMAXCHN_TYPED && dmin != vscalemin[iparline]) {
        textWriteFloat(parScaleMinText, &parScaleMinStat, dmin);
    }
    if (reason != MINMAXCHN_TYPED && dmax != vscalemax[iparline]) {
        textWriteFloat(parScaleMaxText, &parScaleMaxStat, dmax);
    }
    vscalemin[iparline] = dmin;
    vscalemax[iparline] = dmax;

    /***** find the new status of scale interval *****/
    if (dmin >= dmax) {
        newScaleStat = SCALESTAT_INVALID;
        currErrorCode = SCALE_OVERLAP;
    }
    else if (dpar < dmin) {
        newScaleStat = SCALESTAT_PARAMOUT;
        currErrorCode = SCALE_PARAMETEROUT;
    }
    else if (dpar > dmax) {
        newScaleStat = SCALESTAT_PARAMOUT;
        currErrorCode = SCALE_PARAMETEROUT;
    }

    /***** change the status of the scale *****/
    parScaleStat = newScaleStat;
    switch (newScaleStat) {
    case SCALESTAT_OK:
        vscaleunit =
            (vscalemax[iparline]-vscalemin[iparline])/
            res_data.parScaleResolution;
        setScaleWorld(parScale, vpar[iparline], plctxt);
            /***** depending on previous condition *****/
        switch(prevScaleStat) {
        case SCALESTAT_OK:
        break;
        case SCALESTAT_INVALID:
        case SCALESTAT_PARAMOUT:
            parScaleEnable(plctxt);
            textRemoveSemantError(parScaleMinText, &parScaleMinStat);
            textRemoveSemantError(parScaleMaxText, &parScaleMaxStat);
        break;
        default:;
        }
    break;
    case SCALESTAT_INVALID:
    case SCALESTAT_PARAMOUT:
            /***** depending on previous condition *****/
        switch(prevScaleStat) {
        case SCALESTAT_OK:
            textSetSemantError(parScaleMinText, &parScaleMinStat);
            textSetSemantError(parScaleMaxText, &parScaleMaxStat);
            parScaleDisable(plctxt);
        break;
        case SCALESTAT_INVALID:
        case SCALESTAT_PARAMOUT:
        break;
        default:;
        }
    break;
    default:;
    }
}

/***********************************************************************
 Execute when parScaleMinLeftArrow is clicked.
***********************************************************************/
static void minLeftClick(XtPointer client_data)
{
    int iparline;
    double dmin;
    int prevScaleStat;

    parlineSetContext(client_data);
    iparline = parLineNumber;

    prevScaleStat = parScaleStat;

    dmin = roundDnScaleMin(vscalemin[iparline],vscalemax[iparline]);
    setScaleInterval(dmin, vscalemax[iparline],
        UxParLineFormContext, MINMAXCHN_DOWN);

    if (currErrorCode != SCALE_NOERR && prevScaleStat == SCALESTAT_OK) {
            /* parameter outside limits, give warning */
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iparline]);
        XBell(XtDisplay(mainPanelShell),0);
    }
}

/***********************************************************************
 Execute when parScaleMinRightArrow is clicked.
***********************************************************************/
static void minRightClick(XtPointer client_data)
{
    int iparline;
    double dmin;
    int prevScaleStat;

    parlineSetContext(client_data);
    iparline = parLineNumber;

    prevScaleStat = parScaleStat;

    dmin = roundUpScaleMin(vscalemin[iparline],vscalemax[iparline]);
    setScaleInterval(dmin, vscalemax[iparline],
        UxParLineFormContext, MINMAXCHN_UP);

    if (currErrorCode != SCALE_NOERR && prevScaleStat == SCALESTAT_OK) {
            /* parameter outside limits, give warning */
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iparline]);
        XBell(XtDisplay(mainPanelShell),0);
    }
}

/***********************************************************************
 Execute when parScaleMaxLeftArrow is clicked.
***********************************************************************/
static void maxLeftClick(XtPointer client_data)
{
    int iparline;
    double dmax;
    int prevScaleStat;

    parlineSetContext(client_data);
    iparline = parLineNumber;

    prevScaleStat = parScaleStat;

    dmax = roundDnScaleMax(vscalemin[iparline],vscalemax[iparline]);
    setScaleInterval(vscalemin[iparline], dmax,
        UxParLineFormContext, MINMAXCHN_DOWN);

    if (currErrorCode != SCALE_NOERR && prevScaleStat == SCALESTAT_OK) {
            /* parameter outside limits, give warning */
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iparline]);
        XBell(XtDisplay(mainPanelShell),0);
    }
}

/***********************************************************************
 Execute when parScaleMaxRightArrow is clicked.
***********************************************************************/
static void maxRightClick(XtPointer client_data)
{
    int iparline;
    double dmax;
    int prevScaleStat;

    parlineSetContext(client_data);
    iparline = parLineNumber;

    prevScaleStat = parScaleStat;

    dmax = roundUpScaleMax(vscalemin[iparline],vscalemax[iparline]);
    setScaleInterval(vscalemin[iparline], dmax,
        UxParLineFormContext, MINMAXCHN_UP);

    if (currErrorCode != SCALE_NOERR && prevScaleStat == SCALESTAT_OK) {
            /* parameter outside limits, give warning */
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iparline]);
        XBell(XtDisplay(mainPanelShell),0);
    }
}

/***********************************************************************
 Callback for scale minimuum text field.
***********************************************************************/
void clb_parScaleMinLosingFocus(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    double dmin,dmax;
    TextStatusInfo parScaleMaxStat1;

    parlineSetContext(client_data);

    textReadFloat(text, &parScaleMinStat, &dmin, nonEmptyFloatOK);
    textFloatError((TextReadCode)currErrorCode,
        "scale min", parLineNumber, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        dmax = vscalemax[parLineNumber];
        setScaleInterval(dmin, dmax,
            UxParLineFormContext, MINMAXCHN_TYPED);
        parScaleError((ScaleErrorCode)currErrorCode,
            parLineNumber, mainPanelShell);
    }
}

/***********************************************************************
 Callback for scale maximum text field.
***********************************************************************/
void clb_parScaleMaxLosingFocus(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    int sval;
    double dmin,dmax;
    TextStatusInfo parScaleMinStat1;

    parlineSetContext(client_data);

    textReadFloat(text, &parScaleMaxStat, &dmax, nonEmptyFloatOK);
    textFloatError((TextReadCode)currErrorCode,
        "scale max", parLineNumber, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        dmin = vscalemin[parLineNumber];
        setScaleInterval(dmin, dmax,
            UxParLineFormContext, MINMAXCHN_TYPED);
        parScaleError((ScaleErrorCode)currErrorCode,
            parLineNumber, mainPanelShell);
    }
}


static int nrepeatclick;
static void (*clickProc)(XtPointer client_data);
static XtIntervalId wrk_repeatClickId;
XtAppContext app_context;
/***********************************************************************
 Xt working procedure used to repeat click.
***********************************************************************/
static void wrk_repeatClick(
XtPointer client_data,
XtIntervalId *id)
{
    clickProc(client_data);

    wrk_repeatClickId = XtAppAddTimeOut(app_context, AUTOREPEAT_DELAY,
            wrk_repeatClick, client_data);
}

/***********************************************************************
 Start repeating clicks when arrow button is armed.
***********************************************************************/
static void clb_startRepeatClick(
Widget arrbut,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;

    parlineSetContext(client_data);
    iparline = parLineNumber;

    nrepeatclick = 0;
    app_context = XtWidgetToApplicationContext(arrbut);

    if (arrbut==parScaleMinLeftArrow) clickProc = minLeftClick;
    else if (arrbut==parScaleMinRightArrow) clickProc = minRightClick;
    else if (arrbut==parScaleMaxLeftArrow) clickProc = maxLeftClick;
    else if (arrbut==parScaleMaxRightArrow) clickProc = maxRightClick;
    else clickProc = NULL;

    clickProc(client_data);

    wrk_repeatClickId = XtAppAddTimeOut(app_context, AUTOREPEAT_INITDELAY,
            wrk_repeatClick, client_data);
}

/***********************************************************************
 End repeating click when arrow button is disarmed.
***********************************************************************/
static void clb_endRepeatClick(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    XtRemoveTimeOut(wrk_repeatClickId);
}

/***********************************************************************
 Compute the limit change step to be as whole as possible.
***********************************************************************/
static double changeStep(double xmin, double xmax)
{
    double xlen = xmax - xmin;
    double xwhole,frac,xstep;

    /* find the smallest degree of 10 greater or equal than xlen */
    if (xlen >= 1)
        for (xwhole = 1.0; xwhole<xlen; xwhole *= 10.0);
    else
        for (xwhole = 1.0; 0.1*xwhole>xlen; xwhole *= 0.1);

    frac = xlen/xwhole;

    if (frac > 0.5) {
        xstep = 0.1*xwhole;
    }
    else if (frac > 0.3) {
        xstep = 0.05*xwhole;
    }
    else if (frac > 0.2) {
        xstep = 0.03*xwhole;
    }
    else {
        xstep = 0.02*xwhole;
    };

    return(xstep);
}


/***********************************************************************
 Register the callbacks with widgets.
***********************************************************************/
void adjustSetClbs(void *plctxt)
{
    int iline;

    parlineSetContext(plctxt);
    iline = parLineNumber;

    textWriteFloat(parScaleMinText, &parScaleMinStat, vscalemin[iline]);
    textWriteFloat(parScaleMaxText, &parScaleMaxStat, vscalemax[iline]);

    /****** additional settings ****/
    vscaleunit = (vscalemax[iline]-vscalemin[iline])/
                     res_data.parScaleResolution;
    setScaleWorld(parScale, vpar[iline], plctxt);

    /* value changed callbacks for text fields */
    XtAddCallback( parScaleMinText, XmNvalueChangedCallback,
            (XtCallbackProc) clb_textChanged,
            (XtPointer) &parScaleMinStat);
    XtAddCallback( parScaleMaxText, XmNvalueChangedCallback,
            (XtCallbackProc) clb_textChanged,
            (XtPointer) &parScaleMaxStat);

    /* losing focus callbacks for text fields */
    XtAddCallback( parScaleMinText, XmNlosingFocusCallback,
            (XtCallbackProc) clb_parScaleMinLosingFocus,
            (XtPointer) plctxt);
    XtAddCallback( parScaleMaxText, XmNlosingFocusCallback,
            (XtCallbackProc) clb_parScaleMaxLosingFocus,
            (XtPointer) plctxt);

    /* activate callbacks for text fields (process Return key) */
    XtAddCallback( parScaleMinText, XmNactivateCallback,
            (XtCallbackProc) clb_parScaleMinLosingFocus,
            (XtPointer) plctxt);
    XtAddCallback( parScaleMaxText, XmNactivateCallback,
            (XtCallbackProc) clb_parScaleMaxLosingFocus,
            (XtPointer) plctxt);

    XtAddCallback( parScaleMinLeftArrow, XmNarmCallback,
            (XtCallbackProc) clb_startRepeatClick,
            (XtPointer) plctxt);
    XtAddCallback( parScaleMinRightArrow, XmNarmCallback,
            (XtCallbackProc) clb_startRepeatClick,
            (XtPointer) plctxt);
    XtAddCallback( parScaleMaxLeftArrow, XmNarmCallback,
            (XtCallbackProc) clb_startRepeatClick,
            (XtPointer) plctxt);
    XtAddCallback( parScaleMaxRightArrow, XmNarmCallback,
            (XtCallbackProc) clb_startRepeatClick,
            (XtPointer) plctxt);

    XtAddCallback( parScaleMinLeftArrow, XmNdisarmCallback,
            (XtCallbackProc) clb_endRepeatClick,
            (XtPointer) plctxt);
    XtAddCallback( parScaleMinRightArrow, XmNdisarmCallback,
            (XtCallbackProc) clb_endRepeatClick,
            (XtPointer) plctxt);
    XtAddCallback( parScaleMaxLeftArrow, XmNdisarmCallback,
            (XtCallbackProc) clb_endRepeatClick,
            (XtPointer) plctxt);
    XtAddCallback( parScaleMaxRightArrow, XmNdisarmCallback,
            (XtCallbackProc) clb_endRepeatClick,
            (XtPointer) plctxt);
}
