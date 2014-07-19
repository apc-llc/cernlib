/*
 * $Id: parline.c,v 1.1.1.1 1996/03/01 11:39:08 mclareni Exp $
 *
 * $Log: parline.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:08  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 20/04/95  22.07.22  by  Gregory Kozlovsky*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Callback functions for parameter line widgets.
 =====================> Author: G. Kozlovsky, 1994 <===========================
********************************************************************************

 The values of parameter, lower and upper limits, and the scale min and
 max are all interconnected. Therefore, when the user changes them
 a temporary inconsistency may result. User must be warned and the
 actions which do not make sense (e.g. scale change in the scale min
 is greater than the scale max) must be blocked.
 Values of a parameter, its limits, step, and scale min and max must
 be consistent with each other.
   Limits:
     - lower limit must be less than upper limit
     - if both limits are 0, parameter is not limited and the limits
       fields are made empty
     - if user made one of the limit fields empty, limit are invalid
     - if limits have been changed parameter is forced inside the limits
   Parameter:
     - if parameter have been changed out of its limits, it is invalid
   Scale min and max:
     - if scale min is >= than scale max it is invalid
     - if scale interval does not intersect with the limits scale is
       invalid
     - if scale interval extends outside the limits it is truncated
       exactly to the limit

 ___Error processing___
     If an individual number field is syntactically invalid, it is marked
 by red shadow, warning message is popped-up, and the corresponding number
 in memory is not updated.
     If a combination of numbers is inconsistent, the user actions
 which are not defined are blocked, but the corresponding number in
 memory is updated.

*******************************************************************************/

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>

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
#include "fpanelsh/lexer.h"

#ifdef __cplusplus
}  /* extern "C" */
#endif


static LimitsStatCode compNewLimitsStat(int iparline);
static void setLimitsStat(LimitsStatCode newLimitsStat, int iparline);

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
DESCRIPTION:
    Set scale slider to the position corresponding to a value in world
    coordinates
ARGUMENTS: self-explicable
RETURN: ERROR_RET if scale is invalid or its value is out of limits
EXTERNALS: vscalemin, vscaleunit --- input
***********************************************************************/
int setScaleWorld(Widget wscale, double dval, void *plctxt)
{
    int sval;
    int iparline;

    parlineSetContext(plctxt);
    iparline = parLineNumber;

    if (parScaleStat != SCALESTAT_OK) return(ERROR_RET);

    sval = (int)((dval-vscalemin[iparline])/vscaleunit+0.5);

    if (sval < 0 || sval > res_data.parScaleResolution) {
        printf("Internal error: invalid scale value %d dval=%g\n",
            sval,dval);
        return(ERROR_RET);
    }
    XmScaleSetValue(wscale,sval);
    return(OK_RET);
}

/***********************************************************************
DESCRIPTION:
    Perform all the actions necessary to set the new value
    of parameter. The value of a parameter should always be changed using
    this function. This functions must be called only when all the
    consistency checks are done and the value is known to be valid.
    Actions:
        - update panel parameter values
        - update parameter value text field
        - update internal Minuit data
        - update graphics
        - compute the functional value and update its text field
        - mark error estimates invalid
ARGUMENTS: Self-explanatory
EXTERNALS: vpar - output
***********************************************************************/
void setParValue(double dval, void *plctxt, ParChangeReason reason)
{
    int iline;
    int iparline;

    parlineSetContext(plctxt);
    iparline = parLineNumber;

    /****** set new value of parameter, update text field if needed ******/
    vpar[iparline] = dval;
    if (reason != PARCHN_TYPED) {
        /* update value text field */
        textWriteFloat(parValueText, &parValueStat, dval);
    }

    /****** update the graphics ******/
    switch(updatemode) {
    case UPDATE_IMMEDIATE:
        grupdate(UxMainPanelShellContext);
    break;
    case UPDATE_DELAYED:
        if(reason != PARCHN_SCALEDRAG) {
            grupdate(UxMainPanelShellContext);
        }
    break;
    case UPDATE_DISABLED:
    break;
    default:;
    }

    /****** update internal Minuit data ******/
    switch(updatemode) {
    case UPDATE_IMMEDIATE: case UPDATE_DELAYED:
        if(reason!=PARCHN_SCALEDRAG && reason!=PARCHN_MINUIT) {
            set_mnparm(iparline,parname[iparline],
                vpar[iparline],vstep[iparline],vlow[iparline],vupp[iparline]);
        }
    break;
    case UPDATE_DISABLED:
    default:;
    }

    /***** update fit model value and mark Minuit computed errors invalid */
    fp_fitstatupdt(UxMainPanelShellContext);
}

/***********************************************************************
DESCRIPTION:
    Set the value of parameter when it is typed or changed not using
    the scale. If the new value is outside of the scale interval, change
    the interval to include the new value of parameter.
    The parameter is allowed to assume value outside of its limits.
    Actions:
        - set parameter value using setParValue
        - recompute limits status and set new status if changed
        - if parameter is outside the scale interval, increase it
        - set scale position corresponding to the parameter value
ARGUMENTS:
RETURN:
***********************************************************************/
int setParValueAndScale(double dval, void *plctxt, ParChangeReason reason)
{
    double dmin,dmax;
    LimitsStatCode newLimitsStat;
    int iparline;

    parlineSetContext(plctxt);
    iparline = parLineNumber;

        /* set parameter value to the new value */
    setParValue(dval, plctxt, reason);
        /* check if new value of parameter is outside of valid limits */
    newLimitsStat = compNewLimitsStat(iparline);
    if (newLimitsStat != parLimitsStat) {
        setLimitsStat(newLimitsStat, iparline);
        if (currErrorCode == LIMITS_PARAMETEROUT) {
            parLimitsError((LimitsErrorCode)currErrorCode,
                "parameter", iparline, mainPanelShell);
        }
    }

        /* increase scale interval to include new parameter value */
    dmin = vscalemin[iparline];
    dmax = vscalemax[iparline];
    if (dval < dmin) {
        dmin = roundDnScaleMin(dval,dmax);
    }
    else if (dval > dmax) {
        dmax = roundUpScaleMax(dmin,dval);
    }
    setScaleInterval(dmin, dmax, plctxt, MINMAXCHN_PARAMTYPED);

    return(0);
}

/***********************************************************************
 Disable parameter change.
***********************************************************************/
void parChangeDisable(void *plctxt)
{
    parlineSetContext(plctxt);

    XtVaSetValues(parValueText, XmNeditable, False, NULL);
    XtVaSetValues(parLineMenu_curr, XmNsensitive, False, NULL);
    XtVaSetValues(parLineMenu_stored, XmNsensitive, False, NULL);
    XtVaSetValues(parLineMenu_start, XmNsensitive, False, NULL);
}

/***********************************************************************
 Enable parameter change.
***********************************************************************/
void parChangeEnable(void *plctxt)
{
    parlineSetContext(plctxt);

    XtVaSetValues(parValueText, XmNeditable, True, NULL);
    XtVaSetValues(parLineMenu_curr, XmNsensitive, True, NULL);
    XtVaSetValues(parLineMenu_stored, XmNsensitive, True, NULL);
    XtVaSetValues(parLineMenu_start, XmNsensitive, True, NULL);
}

/***********************************************************************
 Parameter name text field callback.
***********************************************************************/
void clb_parnameLosingFocus(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;
    int numpar;

    parlineSetContext(client_data);
    iparline = parLineNumber;

    textReadName(widget, &parNameStat, parname[iparline], nonEmptyNameOK);
    textNameError((TextReadCode)currErrorCode,
        "parameter", iparline, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        set_mnparm(iparline,parname[iparline],
            vpar[iparline],vstep[iparline],vlow[iparline],vupp[iparline]);
        numpar = iparline + 1;
        F77_CALLS_CI(fpsetpnam,
            parname[iparline],strlen(parname[iparline]),&numpar);
        grupdate(UxMainPanelShellContext);
    }
}

/***********************************************************************
 Parameter value text field callback.
***********************************************************************/
void clb_parvalueLosingFocus(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;
    double dval;

    parlineSetContext(client_data);
    iparline = parLineNumber;

    textReadFloat(widget, &parValueStat, &dval, nonEmptyFloatOK);
    textFloatError((TextReadCode)currErrorCode,
        "parameter", iparline, mainPanelShell);

    if (currErrorCode == TEXT_OK) {
            /* set parameter value to the new value */
        setParValueAndScale(dval, client_data, PARCHN_TYPED);
    }
}

/***********************************************************************
 Compute limits status for the current values of limits and parameter
 Note: The parline/panel context must be set before calling this function.
***********************************************************************/
static LimitsStatCode compNewLimitsStat(int iparline)
{
    LimitsStatCode newLimitsStat;
    double dlow = vlow[iparline];
    double dupp = vupp[iparline];
    int clow, cupp;
    UxParLineFormContext = (_UxCparLineForm *)UxGetContext(parLines[iparline]);

    clow = parLowLimitStat.stat;
    cupp = parUppLimitStat.stat;

    currErrorCode = LIMITS_NOERR;

    /***** analyse the situation *****/
    if ((clow&TEXTSTAT_SYNTAX)==TEXTSTAT_EMPTY &&
         (cupp&TEXTSTAT_SYNTAX)==TEXTSTAT_EMPTY) {
                          newLimitsStat = LIMITSTAT_ABSENT;
    }
    else if ((clow&TEXTSTAT_SYNTAX)==TEXTSTAT_EMPTY) {
        if (dupp==0.0)    newLimitsStat = LIMITSTAT_ABSENT;
        else {
                          newLimitsStat = LIMITSTAT_INVALID;
                          currErrorCode = LIMITS_LOWEMPTY;
        }
    }
    else if ((cupp&TEXTSTAT_SYNTAX)==TEXTSTAT_EMPTY) {
        if (dlow==0.0)    newLimitsStat = LIMITSTAT_ABSENT;
        else {
                          newLimitsStat = LIMITSTAT_INVALID;
                          currErrorCode = LIMITS_UPPEMPTY;
        }
    }
    else if (dlow < dupp) newLimitsStat = LIMITSTAT_OK;
    else if (dlow > dupp) {
                          newLimitsStat = LIMITSTAT_INVALID;
                          currErrorCode = LIMITS_OVERLAP;
    }
        /* both limits zero */
    else if (dlow == 0)   newLimitsStat = LIMITSTAT_ABSENT;
    else if (dlow == dupp) { /* (dlow == dupp) != 0 */
                          newLimitsStat = LIMITSTAT_INVALID;
                          currErrorCode = LIMITS_OVERLAP;
    }
    else {
        internalError();
    }

    if (newLimitsStat==LIMITSTAT_OK) {
        if (vpar[iparline]<dlow || vpar[iparline]>dupp) {
            newLimitsStat = LIMITSTAT_PARAMOUT;
            currErrorCode = LIMITS_PARAMETEROUT;
        }
    }

    return(newLimitsStat);
}


/***********************************************************************
 Change limits status and perform action associated with transition:
     - mark validity of limits or parameter text fields
     - write 0 into empty field if limits are set, or make fields
       empty if limits are removed
 Note: The parline/panel context must be set before calling this function.
***********************************************************************/
static void setLimitsStat(LimitsStatCode newLimitsStat, int iparline)
{
    double dlow = vlow[iparline];
    double dupp = vupp[iparline];
    LimitsStatCode prevLimitsStat = (LimitsStatCode)parLimitsStat;

    UxParLineFormContext = (_UxCparLineForm *)UxGetContext(parLines[iparline]);

    /***** do the action *****/
    parLimitsStat = newLimitsStat;
    switch (newLimitsStat) {
    case LIMITSTAT_OK:
            /***** depending on previous condition *****/
        switch(prevLimitsStat) {
        case LIMITSTAT_ABSENT:  /* it is as if both parameters are changed */
            /* redraw another limit */
            if (dlow==0.0) textWriteFloat(parLowLimitText,
                &parLowLimitStat, dlow);
            if (dupp==0.0) textWriteFloat(parUppLimitText,
                &parUppLimitStat, dupp);
        break;
        case LIMITSTAT_PARAMOUT:
            textRemoveSemantError(parValueText, &parValueStat);
        break;
        case LIMITSTAT_INVALID:
                /* remove limit field marks */
            textRemoveSemantError(parLowLimitText, &parLowLimitStat);
            textRemoveSemantError(parUppLimitText, &parUppLimitStat);
        break;
        case LIMITSTAT_OK:
        default:;
        }
    break;
    case LIMITSTAT_ABSENT:
        textWriteEmpty(parLowLimitText, &parLowLimitStat);
        textWriteEmpty(parUppLimitText, &parUppLimitStat);
            /***** depending on previous condition *****/
        switch(prevLimitsStat) {
        case LIMITSTAT_ABSENT:  /* it is as if both parameters are changed */
        break;
        case LIMITSTAT_PARAMOUT:
            textRemoveSemantError(parValueText, &parValueStat);
        break;
        case LIMITSTAT_INVALID:
                /* remove limit field marks */
            textRemoveSemantError(parLowLimitText, &parLowLimitStat);
            textRemoveSemantError(parUppLimitText, &parUppLimitStat);
        break;
        case LIMITSTAT_OK:
        default:;
        }
    break;
    case LIMITSTAT_INVALID:
            /***** depending on previous condition *****/
        switch(prevLimitsStat) {
        case LIMITSTAT_ABSENT:  /* it is as if both parameters are changed */
        case LIMITSTAT_OK:
            /* mark limits fields invalid */
            textSetSemantError(parLowLimitText, &parLowLimitStat);
            textSetSemantError(parUppLimitText, &parUppLimitStat);
        break;
        case LIMITSTAT_PARAMOUT:
            textRemoveSemantError(parValueText, &parValueStat);
            /* mark limits fields invalid */
            textSetSemantError(parLowLimitText, &parLowLimitStat);
            textSetSemantError(parUppLimitText, &parUppLimitStat);
        break;
        case LIMITSTAT_INVALID:
        break;
        default:;
        }
    break;
    case LIMITSTAT_PARAMOUT:
            /***** depending on previous condition *****/
        switch(prevLimitsStat) {
        case LIMITSTAT_ABSENT:  /* it is as if both parameters are changed */
        case LIMITSTAT_OK:
            textSetSemantError(parValueText, &parValueStat);
        break;
        case LIMITSTAT_PARAMOUT:
        break;
        case LIMITSTAT_INVALID:
            textSetSemantError(parValueText, &parValueStat);
                /* remove limit field marks */
            textRemoveSemantError(parLowLimitText, &parLowLimitStat);
            textRemoveSemantError(parUppLimitText, &parUppLimitStat);
        break;
        default:;
        }
    break;
    default:;
    }
}

/***********************************************************************
 Lower limit text field callback.
***********************************************************************/
void clb_lowlimLosingFocus(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;
    double dlow;
    LimitsStatCode newLimitsStat;

    parlineSetContext(client_data);
    iparline = parLineNumber;

    textReadFloat(text, &parLowLimitStat, &dlow,
        anyFloatOrEmptyOK);
    textFloatError((TextReadCode)currErrorCode,
        "lower limit", iparline, mainPanelShell);
    if (currErrorCode==TEXT_OK || currErrorCode==TEXT_EMPTY) {
        vlow[iparline] = dlow;
        newLimitsStat = compNewLimitsStat(iparline);
        if (currErrorCode==LIMITS_UPPEMPTY) {
            textWriteFloat(parUppLimitText,
                &parUppLimitStat, vupp[iparline]);
        }
        newLimitsStat = compNewLimitsStat(iparline);
        setLimitsStat(newLimitsStat, iparline);
        parLimitsError((LimitsErrorCode)currErrorCode,
            "lower limit", iparline, mainPanelShell);
        setScaleInterval(vscalemin[iparline], vscalemax[iparline],
            UxParLineFormContext, MINMAXCHN_LIMITS);
    }
}

/***********************************************************************
 Upper limit text field callback.
***********************************************************************/
void clb_upplimLosingFocus(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;
    double dupp;
    LimitsStatCode newLimitsStat;

    parlineSetContext(client_data);
    iparline = parLineNumber;

    textReadFloat(text, &parUppLimitStat, &dupp,
        anyFloatOrEmptyOK);
    textFloatError((TextReadCode)currErrorCode,
        "upper limit", iparline, mainPanelShell);
    if (currErrorCode==TEXT_OK || currErrorCode==TEXT_EMPTY) {
        vupp[iparline] = dupp;
        newLimitsStat = compNewLimitsStat(iparline);
        if (currErrorCode==LIMITS_LOWEMPTY) {
            textWriteFloat(parLowLimitText,
                &parLowLimitStat, vlow[iparline]);
        }
        newLimitsStat = compNewLimitsStat(iparline);
        setLimitsStat(newLimitsStat, iparline);
        parLimitsError((LimitsErrorCode)currErrorCode,
            "upper limit", iparline, mainPanelShell);
        setScaleInterval(vscalemin[iparline], vscalemax[iparline],
            UxParLineFormContext, MINMAXCHN_LIMITS);
    }
}

/***********************************************************************
 Parameter step size text field callback.
***********************************************************************/
void clb_stepLosingFocus(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;

    parlineSetContext(client_data);
    iparline = parLineNumber;

    textReadFloat(text, &parStepStat, &vstep[iparline],
        positiveFloatOK);
    textFloatError((TextReadCode)currErrorCode,
        "step", iparline, mainPanelShell);
}

void clb_scaleAdjustButton(
Widget widget,
XtPointer client_data,
XtPointer call_data);

/***********************************************************************
 Fix/Release toggle button callback.
***********************************************************************/
void clb_fixChanged(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;
    int icondn;
    char comdstr[30];

    parlineSetContext(client_data);
    iparline = parLineNumber;
    UxParLineFormContext = (_UxCparLineForm *)UxGetContext(parLines[iparline]);

    if (vfixrel[iparline] == PARSTAT_FIXED) {
        sprintf(comdstr,"rel %d",iparline+1);
        F77_CALLS_ECIE(mncomd, fcn, comdstr, strlen(comdstr), &icondn, ufcn);
        vfixrel[iparline] = PARSTAT_RELED;
        XtVaSetValues(widget,
            XmNheight,hNameText,
            XmNwidth,wFixToggle,
            RES_CONVERT( XmNlabelString, "R" ),NULL);
    }
    else {
        sprintf(comdstr,"fix %d",iparline+1);
        F77_CALLS_ECIE(mncomd, fcn, comdstr, strlen(comdstr), &icondn, ufcn);
        vfixrel[iparline] = PARSTAT_FIXED;
        XtVaSetValues(widget,
            XmNheight,hNameText,
            XmNwidth,wFixToggle,
            RES_CONVERT( XmNlabelString, "F" ),NULL);
    }
}

/***********************************************************************
 Parameter scale changed callback.
***********************************************************************/
void clb_scaleChanged(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;
    int sval;
    double dval;

    parlineSetContext(client_data);
    iparline = parLineNumber;
    XmScaleGetValue(widget,&sval);
    dval = vscalemin[iparline] + vscaleunit*sval;
    setParValue(dval, client_data, PARCHN_SCALESET);
}

/***********************************************************************
 Parameter scale drag callback.
***********************************************************************/
void clb_scaleDrag(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;
    int sval;
    double dval;

    parlineSetContext(client_data);
    iparline = parLineNumber;

    XmScaleGetValue(widget,&sval);
    dval = vscalemin[iparline] + vscaleunit*sval;
    setParValue(dval, client_data, PARCHN_SCALEDRAG);
}

/***********************************************************************
 Adjust button callback.
***********************************************************************/
void clb_scaleAdjustButton(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;

    parlineSetContext(client_data);
    iparline = parLineNumber;

    if (parFullForm) {
        XtUnmanageChild(parScaleMinMaxForm);
        XtVaSetValues(parScaleMinMaxForm,
            XmNtopAttachment, XmATTACH_NONE, NULL);
        XtVaSetValues(parBotSeparator,
            XmNtopAttachment, XmATTACH_WIDGET,
            XmNtopWidget, parLineLeftForm, NULL);
        parFullForm = False;
    }
    else {
        XtVaSetValues(parBotSeparator,
            XmNtopAttachment, XmATTACH_WIDGET,
            XmNtopWidget, parScaleMinMaxForm, NULL);
        XtVaSetValues(parScaleMinMaxForm,
            XmNtopAttachment, XmATTACH_WIDGET,
            XmNtopOffset,0,
            XmNtopWidget, parLineLeftForm,
            XmNleftOffset,xScaleMinMax,
   /* this conditional code is due to unexplained Motif behaviour */
#if defined(CERNLIB_SUN)
            XmNwidth,0,
            XmNheight,0,
#endif
#if !defined(CERNLIB_SUN)
            XmNwidth,wScaleMinMaxForm,
            XmNheight,hNameText,
#endif
            NULL);
        XtManageChild(parScaleMinMaxForm);
        parFullForm = True;
    }

    resizeParForm(UxMainPanelShellContext);
}

/***********************************************************************
 An event-handler for posting menus.
***********************************************************************/
static void parLineFormMenuPost(
Widget wgt,
XtPointer client_data,
XEvent *event)
{
    int iparline;
    char textstr[MAX_PARNAMELENGTH+10];

    parlineSetContext(client_data);
    iparline = parLineNumber;

    sprintf(textstr,"Parameter %2d\n%s",iparline+1,parname[iparline]);
    XtVaSetValues(parLineMenu_label,
        RES_CONVERT(XmNlabelString, textstr), NULL);

    labelWriteFloat(parLineMenu_lcurr, vpar[iparline]);
    labelWriteFloat(parLineMenu_lstored, par_store[iparline]);
    labelWriteFloat(parLineMenu_lstart, par_start[iparline]);
}

/***********************************************************************
 "Store value" item of parameter line popup menu.
***********************************************************************/
static void clb_parLineMenuStore(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;

    parlineSetContext(client_data);
    iparline = parLineNumber;

    par_store[iparline] = vpar[iparline];
    labelWriteFloat(parLineMenu_lstored, par_store[iparline]);
}

/***********************************************************************
 "Recall current" item of parameter line popup menu.
***********************************************************************/
static void clb_parLineMenuCurr(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;

    parlineSetContext(client_data);
    iparline = parLineNumber;

        /* set parameter value to the new value */
    setParValueAndScale(vpar[iparline], client_data, PARCHN_OUTSIDE);
}

/***********************************************************************
 "Recall stored" item of parameter line popup menu.
***********************************************************************/
static void clb_parLineMenuStored(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;

    parlineSetContext(client_data);
    iparline = parLineNumber;

        /* set parameter value to the new value */
    setParValueAndScale(par_store[iparline], client_data, PARCHN_OUTSIDE);
}

/***********************************************************************
 "Recall starting" item of parameter line popup menu.
***********************************************************************/
static void clb_parLineMenuStart(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iparline;

    parlineSetContext(client_data);
    iparline = parLineNumber;

        /* set parameter value to the new value */
    setParValueAndScale(par_start[iparline], client_data, PARCHN_OUTSIDE);
}


/***********************************************************************
 Create one parameter line and register callbacks.
***********************************************************************/
Widget createParLine(void *mctxt, int iline)
{
    TextStatusInfo dummyStat;
    double dlow,dupp;
    LimitsStatCode newLimitsStat;

    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

    parLines[iline] = create_parLineForm(parForm,iline);
    UxParLineFormContext =
        (_UxCparLineForm *) UxGetContext(parLines[iline]);
    mainctxt = mctxt;

    /***** XmNtearOffModel is not defined on all platforms *****/
#ifdef XmNtearOffModel
    XtVaSetValues(parLineMenu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);
#endif

    /***** initialize scale interval *****/
    if (vlow[iline]==0.0 && vupp[iline]==0.0) { /* parameter unbounded */
        vscalemin[iline] = vpar[iline] - 3.0*vstep[iline];
        vscalemax[iline] = vpar[iline] + 3.0*vstep[iline];
    }
    else {  /* parameter bounded */
        vscalemin[iline] = vlow[iline];
        vscalemax[iline] = vupp[iline];
    }


    /***** initialize parameter lines *****/
    par_start[iline] = par_store[iline] = vpar[iline];

    parFullForm = False;
    parScaleStat = SCALESTAT_OK;
    parLimitsStat = LIMITSTAT_OK;
    parLineNumber = iline;

    /****** initialize text field status ******/
    parNameStat.change = TEXTSTAT_NOCHANGE;
    parNameStat.stat = TEXTSTAT_READOK;
    parValueStat.change = TEXTSTAT_NOCHANGE;
    parValueStat.stat = TEXTSTAT_READOK;
    parLowLimitStat.change = TEXTSTAT_NOCHANGE;
    parLowLimitStat.stat = TEXTSTAT_READOK;
    parUppLimitStat.change = TEXTSTAT_NOCHANGE;
    parUppLimitStat.stat = TEXTSTAT_READOK;
    parStepStat.change = TEXTSTAT_NOCHANGE;
    parStepStat.stat = TEXTSTAT_READOK;
    parScaleMinStat.change = TEXTSTAT_NOCHANGE;
    parScaleMinStat.stat = TEXTSTAT_READOK;
    parScaleMaxStat.change = TEXTSTAT_NOCHANGE;
    parScaleMaxStat.stat = TEXTSTAT_READOK;
    dummyStat.change = TEXTSTAT_NOCHANGE;
    dummyStat.stat = TEXTSTAT_READOK;

    /****** initialize text fields ******/
    XtVaSetValues(parNameText, XmNvalue, parname[iline],NULL);
    textWriteFloat(parValueText, &parValueStat, vpar[iline]);
    textWriteFloat(parLowLimitText, &parLowLimitStat, vlow[iline]);
    textWriteFloat(parUppLimitText, &parUppLimitStat, vupp[iline]);
    textWriteFloat(parStepText, &parStepStat, vstep[iline]);

    /***** set proper limits and scale status *****/
    newLimitsStat = compNewLimitsStat(iline);
    setLimitsStat(newLimitsStat, iline);
    parLimitsError((LimitsErrorCode)currErrorCode,
        "upper limit", iline, mainPanelShell);
    setScaleInterval(vscalemin[iline], vscalemax[iline],
        UxParLineFormContext, MINMAXCHN_LIMITS);

    /****** insert optional text fields *****/
    setLineView(UxParLineFormContext);

    /****** value changed callbacks for text fields ******/
    XtAddCallback( parNameText, XmNvalueChangedCallback,
            (XtCallbackProc) clb_textChanged,
            (XtPointer) &parNameStat);
    XtAddCallback( parValueText, XmNvalueChangedCallback,
            (XtCallbackProc) clb_textChanged,
            (XtPointer) &parValueStat);
    XtAddCallback( parLowLimitText, XmNvalueChangedCallback,
            (XtCallbackProc) clb_textChanged,
            (XtPointer) &parLowLimitStat);
    XtAddCallback( parUppLimitText, XmNvalueChangedCallback,
            (XtCallbackProc) clb_textChanged,
            (XtPointer) &parUppLimitStat);
    XtAddCallback( parStepText, XmNvalueChangedCallback,
            (XtCallbackProc) clb_textChanged,
            (XtPointer) &parStepStat);

    /****** losing focus callbacks for text fields ******/
    XtAddCallback( parNameText, XmNlosingFocusCallback,
            (XtCallbackProc) clb_parnameLosingFocus,
            (XtPointer) UxParLineFormContext);
    XtAddCallback( parStepText, XmNlosingFocusCallback,
            (XtCallbackProc) clb_stepLosingFocus,
            (XtPointer) UxParLineFormContext);
    XtAddCallback( parLowLimitText, XmNlosingFocusCallback,
            (XtCallbackProc) clb_lowlimLosingFocus,
            (XtPointer) UxParLineFormContext);
    XtAddCallback( parUppLimitText, XmNlosingFocusCallback,
            (XtCallbackProc) clb_upplimLosingFocus,
            (XtPointer) UxParLineFormContext);
    XtAddCallback( parValueText, XmNlosingFocusCallback,
            (XtCallbackProc) clb_parvalueLosingFocus,
            (XtPointer) UxParLineFormContext);

    /****** activate callbacks for text fields (process Return key) */
    XtAddCallback( parNameText, XmNactivateCallback,
            (XtCallbackProc) clb_parnameLosingFocus,
            (XtPointer) UxParLineFormContext);
    XtAddCallback( parStepText, XmNactivateCallback,
            (XtCallbackProc) clb_stepLosingFocus,
            (XtPointer) UxParLineFormContext);
    XtAddCallback( parLowLimitText, XmNactivateCallback,
            (XtCallbackProc) clb_lowlimLosingFocus,
            (XtPointer) UxParLineFormContext);
    XtAddCallback( parUppLimitText, XmNactivateCallback,
            (XtCallbackProc) clb_upplimLosingFocus,
            (XtPointer) UxParLineFormContext);
    XtAddCallback( parValueText, XmNactivateCallback,
            (XtCallbackProc) clb_parvalueLosingFocus,
            (XtPointer) UxParLineFormContext);

    /****** callbacks for widgets other than text fields *****/
    XtAddCallback( parFixToggle, XmNvalueChangedCallback,
            (XtCallbackProc) clb_fixChanged,
            (XtPointer) UxParLineFormContext);
    XtAddCallback( parScale, XmNvalueChangedCallback,
            (XtCallbackProc) clb_scaleChanged,
            (XtPointer) UxParLineFormContext);
    XtAddCallback( parScale, XmNdragCallback,
            (XtCallbackProc) clb_scaleDrag,
            (XtPointer) UxParLineFormContext);

    /****** additional settings ****/
    XtAddCallback( parScaleAdjustButton, XmNactivateCallback,
            (XtCallbackProc) clb_scaleAdjustButton,
            (XtPointer) UxParLineFormContext);

    /****** register event handlers and callbacks for the popup menu ******/
    XtInsertEventHandler(parLineForm, ButtonPressMask,
            False, (XtEventHandler) parLineFormMenuPost,
            (XtPointer) UxParLineFormContext, XtListHead);
    XtAddCallback( parLineMenu_store, XmNactivateCallback,
            (XtCallbackProc) clb_parLineMenuStore,
            (XtPointer) UxParLineFormContext);
    XtAddCallback( parLineMenu_curr, XmNactivateCallback,
            (XtCallbackProc) clb_parLineMenuCurr,
            (XtPointer) UxParLineFormContext);
    XtAddCallback( parLineMenu_stored, XmNactivateCallback,
            (XtCallbackProc) clb_parLineMenuStored,
            (XtPointer) UxParLineFormContext);
    XtAddCallback( parLineMenu_start, XmNactivateCallback,
            (XtCallbackProc) clb_parLineMenuStart,
            (XtPointer) UxParLineFormContext);

    adjustSetClbs(UxParLineFormContext);

    return(parLines[iline]);
}

/***********************************************************************
 Create nlines parameter lines starting from line istart.
***********************************************************************/
void createParLines(void *mctxt, int istart, int nlines)
{
    int iline;

    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

    for (iline=istart; iline<istart+nlines; iline++) {
        createParLine(mctxt, iline);
        if (iline == 0) {
                /* Compute preferred sizes of parameter lines elements */
            queryPreferredSizes(mctxt);
            XtVaSetValues(parLines[iline],
                XmNleftAttachment, XmATTACH_FORM,
                XmNrightAttachment, XmATTACH_FORM,
                XmNtopAttachment, XmATTACH_WIDGET,
                XmNtopWidget, parHeader, NULL);
        }
        else {
            XtVaSetValues(parLines[iline],
                XmNleftAttachment, XmATTACH_FORM,
                XmNrightAttachment, XmATTACH_FORM,
                XmNtopAttachment, XmATTACH_WIDGET,
                XmNtopWidget, parLines[iline-1], NULL);
        }
    }

        /* set sizes of line elements */
    sizeParLines(mctxt, istart, nlines);
}

/***********************************************************************
 Destroy nlines parameter lines starting from line istart.
***********************************************************************/
void destroyParLines(void *mctxt, int istart, int nlines)
{
    int iline;

    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

    for (iline=istart; iline<istart+nlines; iline++) {
        XtVaSetValues(parLines[iline],
            XmNleftAttachment, XmATTACH_NONE,
            XmNrightAttachment, XmATTACH_NONE,
            XmNtopAttachment, XmATTACH_NONE,
            NULL);
        XtUnmanageChild(parLines[iline]);
        XSync(XtDisplay(parLines[iline]),False);
        XtDestroyWidget(parLines[iline]);
        parLines[iline] = NULL;
    }
}

