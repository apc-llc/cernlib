/*
 * $Id: viewmenu.c,v 1.1.1.1 1996/03/01 11:39:09 mclareni Exp $
 *
 * $Log: viewmenu.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:09  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.42  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Callback functions for View menu.
 =====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/MenuShell.h>
#include <Xm/ToggleBG.h>

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


/***********************************************************************
 Insert a text widget(or a pair) in the chain of constraints.
***********************************************************************/
static void insertTextFields(Widget newlft, Widget newrgt)
{
    Widget left, right;

    if (newrgt == parUppLimitText) {
        if (steps_shown) right = parStepText;
        else if (eparab_shown) right = parEparabText;
        else if (eminos_shown) right = parEplusText;
        else if (globcc_shown) right = parGlobccText;
        else right = NULL;
        left = parFixToggle;
    }
    else if (newrgt == parStepText) {
        if (eparab_shown) right = parEparabText;
        else if (eminos_shown) right = parEplusText;
        else if (globcc_shown) right = parGlobccText;
        else right = NULL;
        if (limits_shown) left = parUppLimitText;
        else left = parFixToggle;
    }
    else if (newrgt == parEparabText) {
        if (eminos_shown) right = parEminusText;
        else if (globcc_shown) right = parGlobccText;
        else right = NULL;
        if (steps_shown) left = parStepText;
        else if (limits_shown) left = parUppLimitText;
        else left = parFixToggle;
    }
    else if (newrgt == parEplusText) {
        if (globcc_shown) right = parGlobccText;
        else right = NULL;
        if (eparab_shown) left = parEparabText;
        else if (steps_shown) left = parStepText;
        else if (limits_shown) left = parUppLimitText;
        else left = parFixToggle;
    }
    else if (newrgt == parGlobccText) {
        right = NULL;
        if (eminos_shown) left = parEplusText;
        else if (eparab_shown) left = parEparabText;
        else if (steps_shown) left = parStepText;
        else if (limits_shown) left = parUppLimitText;
        else left = parFixToggle;
    }

    if (right != NULL) {
        XtVaSetValues(right, XmNleftWidget, newrgt, NULL);
    }

    XtVaSetValues(newlft,
        XmNleftAttachment, XmATTACH_WIDGET,
        XmNleftOffset,0,
        XmNleftWidget, left, NULL);

    XtManageChild(newlft);
    if (newrgt!= newlft) XtManageChild(newrgt);
}

/***********************************************************************
 Delete a text widget(or a pair) from the chain of constraints.
***********************************************************************/
static void deleteTextFields(Widget dellft, Widget delrgt)
{
    Widget left, right;

    if (delrgt == parUppLimitText) {
        if (steps_shown) right = parStepText;
        else if (eparab_shown) right = parEparabText;
        else if (eminos_shown) right = parEplusText;
        else if (globcc_shown) right = parGlobccText;
        else right = NULL;
    }
    else if (delrgt == parStepText) {
        if (eparab_shown) right = parEparabText;
        else if (eminos_shown) right = parEplusText;
        else if (globcc_shown) right = parGlobccText;
        else right = NULL;
    }
    else if (delrgt == parEparabText) {
        if (eminos_shown) right = parEminusText;
        else if (globcc_shown) right = parGlobccText;
        else right = NULL;
    }
    else if (delrgt == parEplusText) {
        if (globcc_shown) right = parGlobccText;
        else right = NULL;
    }
    else if (delrgt == parGlobccText) {
        right = NULL;
    }

    if (right != NULL) {
        XtVaGetValues(dellft, XmNleftWidget, &left, NULL);
        XtVaSetValues(right, XmNleftWidget, left, NULL);
    }

    XtVaSetValues(dellft,
        XmNleftAttachment, XmATTACH_NONE, NULL);

    XtUnmanageChild(dellft);
    if (delrgt!= dellft) XtUnmanageChild(delrgt);
}

/***********************************************************************
 Insert a label(or a pair) in the chain of constraints.
***********************************************************************/
static void insertLabel(Widget newlft, Widget newrgt)
{
    Widget left, right;

    if (newrgt == parUppLimitLabel) {
        if (steps_shown) right = parStepLabel;
        else if (eparab_shown) right = parEparabLabel;
        else if (eminos_shown) right = parEplusLabel;
        else if (globcc_shown) right = parGlobccLabel;
        else right = NULL;
        left = parFixLabel;
    }
    else if (newrgt == parStepLabel) {
        if (eparab_shown) right = parEparabLabel;
        else if (eminos_shown) right = parEplusLabel;
        else if (globcc_shown) right = parGlobccLabel;
        else right = NULL;
        if (limits_shown) left = parUppLimitLabel;
        else left = parFixLabel;
    }
    else if (newrgt == parEparabLabel) {
        if (eminos_shown) right = parEminusLabel;
        else if (globcc_shown) right = parGlobccLabel;
        else right = NULL;
        if (steps_shown) left = parStepLabel;
        else if (limits_shown) left = parUppLimitLabel;
        else left = parFixLabel;
    }
    else if (newrgt == parEplusLabel) {
        if (globcc_shown) right = parGlobccLabel;
        else right = NULL;
        if (eparab_shown) left = parEparabLabel;
        else if (steps_shown) left = parStepLabel;
        else if (limits_shown) left = parUppLimitLabel;
        else left = parFixLabel;
    }
    else if (newrgt == parGlobccLabel) {
        right = NULL;
        if (eminos_shown) left = parEplusLabel;
        else if (eparab_shown) left = parEparabLabel;
        else if (steps_shown) left = parStepLabel;
        else if (limits_shown) left = parUppLimitLabel;
        else left = parFixLabel;
    }

    if (right != NULL) {
        XtVaSetValues(right, XmNleftWidget, newrgt, NULL);
    }

    XtVaSetValues(newlft,
        XmNleftAttachment, XmATTACH_WIDGET,
        XmNleftOffset,0,
        XmNleftWidget, left, NULL);

    XtManageChild(newlft);
    if (newrgt!= newlft) XtManageChild(newrgt);
}

/***********************************************************************
 Delete a label(or a pair) from the chain of constraints.
***********************************************************************/
static void deleteLabel(Widget dellft, Widget delrgt)
{
    Widget left, right;

    if (delrgt == parUppLimitLabel) {
        if (steps_shown) right = parStepLabel;
        else if (eparab_shown) right = parEparabLabel;
        else if (eminos_shown) right = parEplusLabel;
        else if (globcc_shown) right = parGlobccLabel;
        else right = NULL;
    }
    else if (delrgt == parStepLabel) {
        if (eparab_shown) right = parEparabLabel;
        else if (eminos_shown) right = parEplusLabel;
        else if (globcc_shown) right = parGlobccLabel;
        else right = NULL;
    }
    else if (delrgt == parEparabLabel) {
        if (eminos_shown) right = parEminusLabel;
        else if (globcc_shown) right = parGlobccLabel;
        else right = NULL;
    }
    else if (delrgt == parEplusLabel) {
        if (globcc_shown) right = parGlobccLabel;
        else right = NULL;
    }
    else if (delrgt == parGlobccLabel) {
        right = NULL;
    }

    if (right != NULL) {
        XtVaGetValues(dellft, XmNleftWidget, &left, NULL);
        XtVaSetValues(right, XmNleftWidget, left, NULL);
    }

    XtVaSetValues(dellft,
        XmNleftAttachment, XmATTACH_NONE, NULL);

    XtUnmanageChild(dellft);
    if (delrgt!= dellft) XtUnmanageChild(delrgt);
}


/***********************************************************************
 Set optional text fields of a parameter line.
***********************************************************************/
void setLineView(void *plct)
{
    UxParLineFormContext = (_UxCparLineForm *)plct;
    UxMainPanelShellContext = (_UxCmainPanelShell *)mainctxt;

    /****** insert optional text fields *****/
    if (limits_shown) {
        insertTextFields(parLowLimitText,parUppLimitText);
    }
    if (steps_shown) {
        insertTextFields(parStepText,parStepText);
    }
    if (eparab_shown) {
        insertTextFields(parEparabText,parEparabText);
    }
    if (eminos_shown) {
        insertTextFields(parEminusText,parEplusText);
    }
    if (globcc_shown) {
        insertTextFields(parGlobccText,parGlobccText);
    }
}

/***********************************************************************
 Toggle limits fields display in the parameter lines.
***********************************************************************/
void callbackViewLimits(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iline;
    Widget left,right;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    limits_shown = XmToggleButtonGadgetGetState(viewMenuLimits);

    resizeParForm(UxMainPanelShellContext);

    UxParHeaderFormContext = (_UxCparHeaderForm *) UxGetContext(parHeader);
    if (limits_shown) {
        insertLabel(parLowLimitLabel,parUppLimitLabel);
    }
    else {
        deleteLabel(parLowLimitLabel,parUppLimitLabel);
    }

    for (iline=0; iline<npar; iline++) {
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iline]);
        if (limits_shown) {
            insertTextFields(parLowLimitText,parUppLimitText);
        }
        else {
            deleteTextFields(parLowLimitText,parUppLimitText);
        }
    }
}

/***********************************************************************
 Toggle step fields display in the parameter lines.
***********************************************************************/
void callbackViewSteps(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iline;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    steps_shown = XmToggleButtonGadgetGetState(viewMenuSteps);

    resizeParForm(UxMainPanelShellContext);

    UxParHeaderFormContext = (_UxCparHeaderForm *) UxGetContext(parHeader);
    if (steps_shown) {
        insertLabel(parStepLabel,parStepLabel);
    }
    else {
        deleteLabel(parStepLabel,parStepLabel);
    }

    for (iline=0; iline<npar; iline++) {
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iline]);
        if (steps_shown) {
            insertTextFields(parStepText,parStepText);
        }
        else {
            deleteTextFields(parStepText,parStepText);
        }
    }
}

/***********************************************************************
 Toggle eparab fields display in the parameter lines.
***********************************************************************/
void callbackViewEparab(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iline;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    eparab_shown = XmToggleButtonGadgetGetState(viewMenuEparab);

    resizeParForm(UxMainPanelShellContext);

    UxParHeaderFormContext = (_UxCparHeaderForm *) UxGetContext(parHeader);
    if (eparab_shown) {
        insertLabel(parEparabLabel,parEparabLabel);
    }
    else {
        deleteLabel(parEparabLabel,parEparabLabel);
    }

    for (iline=0; iline<npar; iline++) {
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iline]);
        if (eparab_shown) {
            insertTextFields(parEparabText,parEparabText);
        }
        else {
            deleteTextFields(parEparabText,parEparabText);
        }
    }
}

/***********************************************************************
 Toggle minos error fields display in the parameter lines
***********************************************************************/
void callbackViewEminos(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iline;
    Widget left,right;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    eminos_shown = XmToggleButtonGadgetGetState(viewMenuEminos);

    resizeParForm(UxMainPanelShellContext);

    UxParHeaderFormContext = (_UxCparHeaderForm *) UxGetContext(parHeader);
    if (eminos_shown) {
        insertLabel(parEminusLabel,parEplusLabel);
    }
    else {
        deleteLabel(parEminusLabel,parEplusLabel);
    }

    for (iline=0; iline<npar; iline++) {
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iline]);
        if (eminos_shown) {
            insertTextFields(parEminusText,parEplusText);
        }
        else {
            deleteTextFields(parEminusText,parEplusText);
        }
    }
}

/***********************************************************************
 Toggle globcc fields display in the parameter lines
***********************************************************************/
void callbackViewGlobcc(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    int iline;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    globcc_shown = XmToggleButtonGadgetGetState(viewMenuGlobcc);

    resizeParForm(UxMainPanelShellContext);

    UxParHeaderFormContext = (_UxCparHeaderForm *) UxGetContext(parHeader);
    if (globcc_shown) {
        insertLabel(parGlobccLabel,parGlobccLabel);
    }
    else {
        deleteLabel(parGlobccLabel,parGlobccLabel);
    }

    for (iline=0; iline<npar; iline++) {
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iline]);
        if (globcc_shown) {
            insertTextFields(parGlobccText,parGlobccText);
        }
        else {
            deleteTextFields(parGlobccText,parGlobccText);
        }
    }
}

#include <X11/cursorfont.h>

/**********************************************************************
 Compute preferred sizes of parameter lines elements.
 The sizes are kept as interface-specific variables.
**********************************************************************/
void queryPreferredSizes(void *mctxt)
{
    XtWidgetGeometry prefGeom;
    Dimension wNumberLabel;

    UxMainPanelShellContext = (_UxCmainPanelShell *) mctxt;

    if (parLines[0]==NULL) return;

    /****** compute preferred sizes of the line elements ******/
    UxParHeaderFormContext = (_UxCparHeaderForm *) UxGetContext(parHeader);
    UxParLineFormContext = (_UxCparLineForm *) UxGetContext(parLines[0]);

    /****** query heights for parLine elements ******/
    /* the line height is defined by the parNameText widget */
    utlQueryDimensions(parNameText,&wNameText,&hNameText);
    utlQueryDimensions(parBotSeparator,NULL,&hBotSeparator);
    utlQueryDimensions(parTopSeparator,NULL,&hTopSeparator);
    wScaleAdjustButton = hNameText/2;

    /****** query widths for labels ******/
    utlQueryDimensions(parNumberValueLabel,&wNumberValue,NULL);
    utlQueryDimensions(parNumberLabel,&wNumberLabel,NULL);
    wNumberValue = max(wNumberValue,wNumberLabel);
    /*utlQueryDimensions(parScale,&wScale,NULL);*/
    wScale = scaleMinWidth;
    utlQueryDimensions(parValueText,&wValueText,NULL);
    utlQueryDimensions(parFixLabel,&wFixToggle,NULL);

    utlQueryDimensions(parLowLimitText,&wLowLimitText,NULL);
    utlQueryDimensions(parUppLimitText,&wUppLimitText,NULL);
    utlQueryDimensions(parStepText,&wStepText,NULL);
    utlQueryDimensions(parEparabText,&wEparabText,NULL);
    utlQueryDimensions(parEminusText,&wEminusText,NULL);
    utlQueryDimensions(parEplusText,&wEplusText,NULL);
    utlQueryDimensions(parGlobccText,&wGlobccText,NULL);

    /****** query misc dimensions ******/
    utlQueryDimensions(parScaleMinMaxForm,&wScaleMinMaxForm,NULL);
    /*XtVaGetValues(parBotSeparator,
        XmNshadowThickness,&parBotSepShadowThickness,NULL);*/
    XtVaGetValues(parScrolledWindow,
        XmNspacing,&dScrolledWinSpacing,
        XmNshadowThickness,&dScrolledWinShadow,NULL);
    /***** for some reason spacing is not right *****/
    dScrolledWinSpacing += res_data.parScrolledCorrection;
    utlQueryDimensions(vertScrollBar,&wVertScrollBar,NULL);
    utlQueryDimensions(horlScrollBar,NULL,&hHorlScrollBar);
}

/**********************************************************************
 Set sizes of parameter line elements
**********************************************************************/
void sizeParLines(void *mctxt, int istart, int nlines)
{
    int iline;
    Dimension parActiveMargins;
    Dimension parActiveIndSize;
    Dimension parLineHei,parHeaderHei;
    XSetWindowAttributes attrs;

    UxMainPanelShellContext = (_UxCmainPanelShell *) mctxt;

    /****** set sizes of elements of header line ******/
    parActiveMargins = hNameText/(Dimension)res_data.parActiveMarginFract;
    parActiveIndSize = hNameText-2*parActiveMargins;
    if (istart == 0) {
        UxParHeaderFormContext =
            (_UxCparHeaderForm *) UxGetContext(parHeader);
        XtVaSetValues(parNumberLabel,
            XmNheight,hNameText,
            XmNwidth,wNumberValue,NULL);
        XtVaSetValues(parNameLabel,
            XmNheight,hNameText,
            XmNwidth,wNameText,NULL);
        XtVaSetValues(parValueLabel,
            XmNheight,hNameText,
            XmNwidth,wValueText,NULL);
        XtVaSetValues(parFixLabel,
            XmNheight,hNameText,
            XmNwidth,wFixToggle,NULL);
        XtVaSetValues(parLowLimitLabel,
            XmNheight,hNameText,
            XmNwidth,wLowLimitText,NULL);
        XtVaSetValues(parUppLimitLabel,
            XmNheight,hNameText,
            XmNwidth,wUppLimitText,NULL);
        XtVaSetValues(parStepLabel,
            XmNheight,hNameText,
            XmNwidth,wStepText,NULL);
        XtVaSetValues(parEparabLabel,
            XmNheight,hNameText,
            XmNwidth,wEparabText,NULL);
        XtVaSetValues(parEminusLabel,
            XmNheight,hNameText,
            XmNwidth,wEminusText,NULL);
        XtVaSetValues(parEplusLabel,
            XmNheight,hNameText,
            XmNwidth,wEplusText,NULL);
        XtVaSetValues(parGlobccLabel,
            XmNheight,hNameText,
            XmNwidth,wGlobccText,NULL);
    }

    /****** set sizes of elements of parameter lines ******/
    for (iline=istart; iline<istart+nlines; iline++) {
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iline]);
        XtVaSetValues(parNumberValueLabel,
            XmNheight,hNameText,
            XmNwidth,wNumberValue,NULL);
        XtVaSetValues(parScale,
            XmNscaleHeight,hNameText,
            XmNheight,hNameText,
            XmNwidth,0,
            NULL);
        XtVaSetValues(parScaleAdjustButton,
            XmNheight,hNameText,
            XmNwidth,wScaleAdjustButton,NULL);
        XtVaSetValues(parValueText,
            XmNheight,hNameText,NULL);
        XtVaSetValues(parFixToggle,
            XmNheight,hNameText,
            XmNwidth,wFixToggle,NULL);
        XtVaSetValues(parLowLimitText,
            XmNheight,hNameText,
            XmNwidth,wLowLimitText,NULL);
        XtVaSetValues(parUppLimitText,
            XmNheight,hNameText,
            XmNwidth,wUppLimitText,NULL);
        XtVaSetValues(parStepText,
            XmNheight,hNameText,
            XmNwidth,wStepText,NULL);
        XtVaSetValues(parEparabText,
            XmNheight,hNameText,
            XmNwidth,wEparabText,NULL);
        XtVaSetValues(parEminusText,
            XmNheight,hNameText,
            XmNwidth,wEminusText,NULL);
        XtVaSetValues(parEplusText,
            XmNheight,hNameText,
            XmNwidth,wEplusText,NULL);
        XtVaSetValues(parGlobccText,
            XmNheight,hNameText,
            XmNwidth,wGlobccText,NULL);
        XtVaSetValues(parScaleMinMaxForm,
            XmNheight,hNameText,
            XmNwidth, wScaleMinMaxForm, NULL);
    }

    XtManageChild(parForm);
}

/**********************************************************************
 Compute and set proper size of the parameter form. It depends on the
 current number of lines, open scale interval adjusting devices,
 text fields displayed in the parameter lines, and the size of the
 panel.
**********************************************************************/
void resizeParForm(void *mctxt)
{
    int iline;
    Dimension clipWindowWid, clipWindowHei;
    Dimension clipWindowWid1, clipWindowHei1;
    Dimension LineWid, LinePixWid;
    Dimension visibleWid;
    Boolean putHorlBar = False, putVertBar = False;
    int xScaleMinMaxInt;

    UxMainPanelShellContext = (_UxCmainPanelShell *) mctxt;

    /***** compute minimum line width *****/
    LineWid = wNumberValue + wNameText +
        wScaleAdjustButton + wValueText + wFixToggle +
        scaleMinWidth;
    if (limits_shown) LineWid += wLowLimitText + wUppLimitText;
    if (steps_shown) LineWid += wStepText;
    if (eparab_shown) LineWid += wEparabText;
    if (eminos_shown) LineWid += wEminusText + wEplusText;
    if (globcc_shown) LineWid += wGlobccText;

    /***** compute parForm height *****/
    parFormHei = hNameText + hTopSeparator + npar*(hNameText+hBotSeparator);
    for (iline=0; iline<npar; iline++) {
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iline]);
        if (parFullForm) parFormHei += hNameText;
    }

    /****** decide on scroll bar placement ******/
    XtVaGetValues(parScrolledWindow,
        XmNwidth,&clipWindowWid,
        XmNheight,&clipWindowHei,NULL);
    clipWindowWid -= 2*dScrolledWinShadow;
    clipWindowHei -= 2*dScrolledWinShadow;
    clipWindowWid1 = clipWindowWid - (wVertScrollBar+dScrolledWinSpacing);
    clipWindowHei1 = clipWindowHei - (hHorlScrollBar+dScrolledWinSpacing);

    if (LineWid > clipWindowWid) { /* horizontal bar is needed */
        putHorlBar = True;
        if (parFormHei > clipWindowHei1) {
            putVertBar = True;
        }
    }
    else if (LineWid > clipWindowWid1) {
        if (parFormHei > clipWindowHei) {
            putVertBar = True;
            putHorlBar = True;
        }
    }
    else /* horizontal scroll bar not needed */
        if (parFormHei > clipWindowHei) {
            putVertBar = True;
    }

    /****** compute and set the width of parForm ******/
    if (putVertBar) visibleWid = clipWindowWid1;
    else visibleWid = clipWindowWid;
    parFormWid = max(LineWid,visibleWid);
    wScale = scaleMinWidth + parFormWid - LineWid;
        /* Dimension=unsigned short, therefore this trick is needed */
    xScaleMinMaxInt = wNumberValue + wNameText +
        (wScale-wScaleMinMaxForm)/2;
    xScaleMinMax = (Dimension)max(0,xScaleMinMaxInt);

    parFormWidPix = XmConvertUnits(parForm,
        XmHORIZONTAL,Xm100TH_FONT_UNITS,parFormWid,XmPIXELS);
    parFormHeiPix = XmConvertUnits(parForm,
        XmVERTICAL,Xm100TH_FONT_UNITS,parFormHei,XmPIXELS);

    /****** manage parameter lines ******/
    for (iline=0; iline<npar; iline++) {
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iline]);
        if (parFullForm) {
            XtVaSetValues(parScaleMinMaxForm,
                XmNleftOffset,xScaleMinMax,NULL);
        }
    }

    XtVaSetValues(parForm,
        XmNwidth,parFormWidPix,
        XmNheight,parFormHeiPix, NULL);

    /***** This loop should be unnecessary but without it resizing *
    ****** does not work properly with some X servers (NCD, VAX) ***/
    for (iline=0; iline<npar; iline++) {
        UxParLineFormContext =
            (_UxCparLineForm *) UxGetContext(parLines[iline]);
        XtVaSetValues(parScale,
            XmNscaleHeight,hNameText,
            XmNheight,hNameText,
            XmNwidth,0,
            NULL);
    }

    if (putHorlBar && !horlScrollBarOn) XtManageChild(horlScrollBar);
    if (!putHorlBar && horlScrollBarOn) XtUnmanageChild(horlScrollBar);
    if (putVertBar && !vertScrollBarOn) XtManageChild(vertScrollBar);
    if (!putVertBar && vertScrollBarOn) XtUnmanageChild(vertScrollBar);
    horlScrollBarOn = putHorlBar;
    vertScrollBarOn = putVertBar;
}

/**********************************************************************
 Action routine for resizing the parameter form.
**********************************************************************/
void action_resizeParForm(
Widget scrWindow,
XEvent *event,
String *parms,
Cardinal *numParms)
{
    UxMainPanelShellContext =
        (_UxCmainPanelShell *)UxGetContext (scrWindow);

    /***** if clipWindow geometry did not change, return *****/
    if (event->xconfigure.window != XtWindow(parScrolledWindow)) return;

    resizeParForm(UxMainPanelShellContext);
}

