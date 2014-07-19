/*
 * $Id: mainclbs.c,v 1.4 2000/01/13 15:42:11 couet Exp $
 *
 * $Log: mainclbs.c,v $
 * Revision 1.4  2000/01/13 15:42:11  couet
 * - mods to make the fitting panel work on LINUX
 *
 * Revision 1.3  1999/03/01 09:08:34  couet
 * - previous commit was wrong
 *
 * Revision 1.2  1999/03/01 09:04:47  couet
 * - ERROR
 *
 * Revision 1.1.1.1  1996/03/01 11:39:07  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/05 28/06/95  14.11.53  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Callback functions for the main panel.
 =====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/
#include <stdio.h>
#include <math.h>
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
#include "fpanelsh/parhdrform.h"
#undef UxParent
#include "fpanelsh/mainpanel.h"
#undef CONTEXT_MACRO_ACCESS
#include "fpanelsh/lexer.h"

#ifdef __cplusplus
}  /* extern "C" */
#endif


extern XtAppContext     UxAppContext;
extern Widget           UxTopLevel;
extern Display          *UxDisplay;
extern int              UxScreen;

void *fitPanelContext = NULL;  /* only one copy of the panel per run */
int mnInitialized = False; /* Minuit initialized once in a run */

/***** cursors *****/
Cursor normCursor = 0;
Cursor helpCursor = 0;
Cursor busyCursor = 0;
Cursor blckCursor = 0;
Cursor selCursor = 0;



/***********************************************************************
       Following are the translation tables.
***********************************************************************/
static char     *transTable1 = "#augment\n\
<ConfigureNotify>:resizeParForm()\n";

static XtActionsRec _Uxactions[] = {
    { "resizeParForm", (XtActionProc) action_resizeParForm }
};



/***********************************************************************
 Round float to the nearest integer.
***********************************************************************/
static int iround(float x)
{
    return((int)(x>0.0? (x+0.5) : (x-0.5)));
}

/***********************************************************************
 If panel state is invalid, block user actions which can not be
 meaningfully performed. If panel state is valid, allow the user to
 perform these actions.
***********************************************************************/
static void setPanelState(void *pctxt)
{
    int setcode = 1;
    UxMainPanelShellContext = (_UxCmainPanelShell *)pctxt;

    if (panelState == PanelStateValid) {
        XtVaSetValues(minimizeMenu_top_b, XmNsensitive, True, NULL);
        XtVaSetValues(examineMenu_top_b, XmNsensitive, True, NULL);
        XtVaSetValues(scanMenu_top_b, XmNsensitive, True, NULL);
        F77_CALLS_I(mnrset,&setcode); /* reset Minuit for new fit */
        set_mnparms(pctxt); /* set/reset parameters */
    }
    else {
        XtVaSetValues(minimizeMenu_top_b, XmNsensitive, False, NULL);
        XtVaSetValues(examineMenu_top_b, XmNsensitive, False, NULL);
        XtVaSetValues(scanMenu_top_b, XmNsensitive, False, NULL);
        closeContourPanel(UxMainPanelShellContext);
        closeScanPanel(UxMainPanelShellContext);
    }
}

/***********************************************************************
 Perform actions required when the number of the fit model parameters is
 changed.
***********************************************************************/
static void changenpar(void *pctxt,int np)
{
    int iline, numpar;
    char errmsg[80];

    UxMainPanelShellContext = (_UxCmainPanelShell *)pctxt;

    closeContourPanel(UxMainPanelShellContext);
    closeScanPanel(UxMainPanelShellContext);

    if (np > MAX_PARNUMBER) {
        sprintf(errmsg,
            "Number of parameters can not exceed %d", MAX_PARNUMBER);
        km_warn_user(mainPanelShell,errmsg,"");
        panelState |= FitFuncInvalid;
        setPanelState(UxMainPanelShellContext);
        return;
    }

   if (np > npar) {
        for (iline=npar; iline<np; iline++) {
            vpar[iline] = 0.0;
            vlow[iline] = vupp[iline] = 0.0;
            vstep[iline] = 0.33;
            veparab[iline] = veminus[iline] = veplus[iline] = 0.0;
            numpar = iline + 1;
            sprintf(parname[iline], "P%d", iline+1);
            F77_CALLS_CI(fpsetpnam,
                parname[iline],strlen(parname[iline]),&numpar);
       }
        createParLines(UxMainPanelShellContext, npar, np-npar);
    }
    else if (np < npar) {
        destroyParLines(UxMainPanelShellContext, np, npar-np);
    }

    npar = np;
    F77_CALLS_I(fpsetnpar,&np);
    resizeParForm(UxMainPanelShellContext);
}

/*******************************************************************************
     ################ Histogram id and range callbacks ##################
*******************************************************************************/

/***********************************************************************
 Histogram text field callback.
***********************************************************************/
static void clb_histoIdChanged(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    int ierflg;
    char hsid[20];

    UxMainPanelShellContext = (_UxCmainPanelShell *)client_data;

    textReadInt(text, &histoIdStat, &histid, nonEmptyIntOK);
    if (currErrorCode == TEXT_NOCHANGE) return;
    textIntError((TextReadCode)currErrorCode,
        "histogram id", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        sprintf(hsid,"%d",histid);
        F77_CALLS_CI(fphsetid,hsid,strlen(hsid),&ierflg);
        if (ierflg) {
            textSetSemantError(text, &histoIdStat);
            km_warn_user(mainPanelShell, "Histogram not found","");
        }
        else {
            textRemoveSemantError(text, &histoIdStat);
            fp_hsetdata(histid,UxMainPanelShellContext);
        }
    }

    if (histoIdStat.stat==TEXTSTAT_READOK) panelState &= ~FitObjInvalid;
    else panelState |= FitObjInvalid;
    setPanelState(UxMainPanelShellContext);
    fp_fitstatupdt(UxMainPanelShellContext);
    if (panelState&~FitObjInvalid && panelState&FitFuncInvalid) {
        F77_CALLS_(fphplot);
    }
    else {
        grupdate(UxMainPanelShellContext);
    }
}


/***********************************************************************
DESCRIPTION:
    Convert histogram ranges into a string and write it into a
    text widget

ARGUMENTS: self-explicable
***********************************************************************/
void histoRangeWrite(Widget text)
{
    char textstr[50];
    char *sptr = textstr;
    int i;

    for (i=0; i<histodim; i++) {
        sprintf(sptr,"%d:%d",lowbins[i], uppbins[i]);
        sptr += strlen(sptr);
        if (i != histodim-1) {
            strcpy(sptr,", ");
            sptr += 3;
        }
    }

    XmTextSetString(text,textstr);

    if (rangeStat.stat != TEXTSTAT_READOK) {
        utl_markTextValid(text);
        rangeStat.stat = TEXTSTAT_READOK;
    }
    rangeStat.change = TEXTSTAT_NOCHANGE;
}

/***********************************************************************
DESCRIPTION:
    Read the histogram range(s) from a text widget with syntax error cheking.
    The lower and upper bin values must be integers separated by a colon,
    or floating point values specifying range in x or y coordinates;
    the ranges for the different dimensions are separated by commas.
    The value is read only if it have been changed since the last reading
    as indicated by status. If a syntax error have been found, the
    text field is visually marked and a warning message is popped up.

ARGUMENTS:
    text - text widget
    *status - current status of the field on input, new status on
        output (enum textstatus)

RETURN: a termination code is returned (enum textreadret)

EXTERNALS:
    histodim - the dimension of the histogram
    lowbin, uppbin - integer arrays into which the read values
        of low and upper bins are returned if there is no syntax error
        and the field is not empty
***********************************************************************/
static void textReadRange(Widget text, TextStatusInfo *status)
{
    String textstr;
    int idim;
    int rlowbins[MAX_HISTODIM], ruppbins[MAX_HISTODIM];
    int hchannels[MAX_HISTODIM];
    int errdim;
    float rmin[MAX_HISTODIM],rmax[MAX_HISTODIM];

    if (status->change != TEXTSTAT_CHANGED) {
        currErrorCode = RANGE_NOCHANGE;
        return;
    }
    currErrorCode = RANGE_OK;

    XtVaGetValues(text,XmNvalue, &textstr,NULL);
    chgive(histid, &hchannels[0], &rmin[0], &rmax[0],
        &hchannels[1], &rmin[1], &rmax[1]);

    /****** read and analyze the field *****/
    lx_load(textstr);
    lx_next();
    if (lx_token.type == NEWLINE_T) { /* empty field */
        currErrorCode = RANGE_EMPTY;
        goto endproc;
    }

    for (idim=0; idim<histodim; idim++) {
            /****** read lower bin ******/
        rlowbins[idim] = 1; /* empty field = lowest bin */
        if (lx_token.type == INT_T) {
            rlowbins[idim] = lx_token.val.i;
            lx_next();
        }
        if (lx_token.type == DOUBLE_T) {
            rlowbins[idim] = iround(floor(hchannels[idim]*
                (lx_token.val.d/(rmax[idim]-rmin[idim]))+1));
            lx_next();
        }

            /***** check lower bin value *****/
        if (rlowbins[idim] < 1 || rlowbins[idim] > hchannels[idim]) {
            currErrorCode = RANGE_INVDLOWSEM;
            goto endproc;
        }

            /****** read range separator (:) ******/
        if (lx_token.type == SEPAR_T+':') {
        lx_next();
        }
        else {
           currErrorCode = RANGE_INVDLOWUPPSEP;
            goto endproc;
        }

            /****** read upper bin ******/
        ruppbins[idim] = hchannels[idim]; /* empty field = highest bin */
        if (lx_token.type == INT_T) {
            ruppbins[idim] = lx_token.val.i;
            lx_next();
        }
        if (lx_token.type == DOUBLE_T) {
            ruppbins[idim] = iround(ceil(hchannels[idim]*
                (lx_token.val.d/(rmax[idim]-rmin[idim]))+1));
            lx_next();
        }

            /***** check upper bin value *****/
        if (ruppbins[idim] < 1 || ruppbins[idim] > hchannels[idim]) {
            currErrorCode = RANGE_INVDUPPSEM;
            goto endproc;
        }
        if (rlowbins[idim] > ruppbins[idim]) {
            currErrorCode = RANGE_INVDORDER;
            goto endproc;
        }

            /****** read dimensions separator (,) ******/
        if (idim != histodim-1) {
            if (lx_token.type == SEPAR_T+',') {
                lx_next();
            }
            else {
                currErrorCode = RANGE_INVDDIMSEP;
                goto endproc;
            }
        }
    }

    if (lx_token.type != NEWLINE_T) {
        currErrorCode = RANGE_INVDAFTER;
    }

    /***** return the number or signal the error *****/
endproc:
    errdim = idim;
    status->change = TEXTSTAT_NOCHANGE;
    switch (currErrorCode) {
    case RANGE_OK:
        if (status->stat != TEXTSTAT_READOK) utl_markTextValid(text);
        for (idim=0; idim<histodim; idim++) {
            lowbins[idim] = rlowbins[idim];
            uppbins[idim] = ruppbins[idim];
        }
        status->stat = TEXTSTAT_READOK;
    break;
    case RANGE_EMPTY:
        if (status->stat == TEXTSTAT_READOK) utl_markTextInvalid(text);
        status->stat = TEXTSTAT_SEMANTERROR;
    break;
    case RANGE_INVDLOW: case RANGE_INVDUPP:
    case RANGE_INVDLOWSEM: case RANGE_INVDUPPSEM: case RANGE_INVDORDER:
    case RANGE_INVDLOWUPPSEP: case RANGE_INVDDIMSEP: case RANGE_INVDAFTER:
        if (status->stat == TEXTSTAT_READOK) utl_markTextInvalid(text);
        status->stat = TEXTSTAT_SYNTERROR;
    break;
    case RANGE_NOCHANGE:
    default:
        internalError();
    }

    textRangeError((RangeReadCode)currErrorCode, errdim, mainPanelShell);
}

/*******************************************************************************
 Range text field callback.
*******************************************************************************/
static void clb_rangeChanged(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    textReadRange(text, &rangeStat);
    if (currErrorCode == TEXT_OK) {
        F77_CALLS_II(fphsetrng,lowbins,uppbins);
        fp_fitstatupdt(UxMainPanelShellContext);
        if (panelState&~FitObjInvalid && panelState&FitFuncInvalid) {
            F77_CALLS_(fphplot);
        }
        else {
            grupdate(UxMainPanelShellContext);
        }
    }
}

/***********************************************************************
 When X, Y, or EY vector is changed, set internal vectors inside PAW
 and set the correct panelState.
***********************************************************************/
void fp_vset(void *pctxt)
{
    int ierflg;
    char xname[MAX_PAWVECNAME+1];
    char yname[MAX_PAWVECNAME+1];
    char eyname[MAX_PAWVECNAME+1];

    UxMainPanelShellContext = (_UxCmainPanelShell *) pctxt;

    panelState |= FitObjInvalid;

    if (xvectStat.stat!=TEXTSTAT_READOK) return;
    if (yvectStat.stat!=TEXTSTAT_READOK) return;
    if (eyvectStat.stat!=TEXTSTAT_READOK &&
        eyvectStat.stat!=TEXTSTAT_EMPTY) return;

    xvectStat.change = TEXTSTAT_CHANGED;
    textReadName(xvectText, &xvectStat, xname, nonEmptyNameOK);
    yvectStat.change = TEXTSTAT_CHANGED;
    textReadName(yvectText, &yvectStat, yname, nonEmptyNameOK);
    eyvectStat.change = TEXTSTAT_CHANGED;
    textReadName(eyvectText, &eyvectStat, eyname, anyNameOrEmptyOK);

    F77_CALLS_CCCI(fpvset, xname,strlen(xname),yname,strlen(yname),
        eyname,strlen(eyname),&ierflg);

    if (ierflg) {
        km_warn_user(mainPanelShell, "Error setting working vectors","");
        return;
    }

    panelState &= ~FitObjInvalid;  /* mark fit object valid */
}


/***********************************************************************
 X vector text field callback.
***********************************************************************/
static void clb_xvectChanged(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    int ierflg;
    char vname[MAX_PAWVECNAME+1];
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    textReadName(text, &xvectStat,vname, nonEmptyNameOK);
    if (currErrorCode == TEXT_NOCHANGE) return;
    textNameError((TextReadCode)currErrorCode,
        "PAW vector", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        if (fp_vexist(mainPanelShell,vname)==True) {
            textRemoveSemantError(text, &xvectStat);
        }
        else {
            textSetSemantError(text, &xvectStat);
        }
    }

    fp_vset(UxMainPanelShellContext);
    setPanelState(UxMainPanelShellContext);
    fp_fitstatupdt(UxMainPanelShellContext);
    if (panelState&~FitObjInvalid && panelState&FitFuncInvalid) {
        F77_CALLS_(fpvplot);
    }
    else {
        grupdate(UxMainPanelShellContext);
    }
}

/***********************************************************************
 Y vector text field callback.
***********************************************************************/
static void clb_yvectChanged(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    char vname[MAX_PAWVECNAME+1];
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    textReadName(text, &yvectStat,vname, nonEmptyNameOK);
    if (currErrorCode == TEXT_NOCHANGE) return;
    textNameError((TextReadCode)currErrorCode,
        "PAW vector", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        if (fp_vexist(mainPanelShell,vname)==True) {
            textRemoveSemantError(text, &yvectStat);
        }
        else {
            textSetSemantError(text, &yvectStat);
        }
    }

    fp_vset(UxMainPanelShellContext);
    setPanelState(UxMainPanelShellContext);
    fp_fitstatupdt(UxMainPanelShellContext);
    if (panelState&~FitObjInvalid && panelState&FitFuncInvalid) {
        F77_CALLS_(fpvplot);
    }
    else {
        grupdate(UxMainPanelShellContext);
    }
}

/***********************************************************************
 EY vector text field callback.
***********************************************************************/
static void clb_eyvectChanged(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    char vname[MAX_PAWVECNAME+1];
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    textReadName(text, &eyvectStat,vname, anyNameOrEmptyOK);
    if (currErrorCode == TEXT_NOCHANGE) return;
    textNameError((TextReadCode)currErrorCode,
        "PAW vector", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        if (fp_vexist(mainPanelShell,vname)==True) {
            textRemoveSemantError(text, &eyvectStat);
        }
        else {
            textSetSemantError(text, &eyvectStat);
        }
    }
    else if (currErrorCode == TEXT_EMPTY) {
        textRemoveSemantError(text, &eyvectStat);
        fp_vset(UxMainPanelShellContext);
    }

    fp_vset(UxMainPanelShellContext);
    setPanelState(UxMainPanelShellContext);
    fp_fitstatupdt(UxMainPanelShellContext);
    if (panelState&~FitObjInvalid && panelState&FitFuncInvalid) {
        F77_CALLS_(fpvplot);
    }
    else {
        grupdate(UxMainPanelShellContext);
    }
}

/***********************************************************************
 Optn text field callback for Histo Fit Panel.
***********************************************************************/
static void clb_histoOptnChanged(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    int ioptd=0, ioptw=0, ioptu = 0, iopterr=0;
    int ioptdold,ioptwold,ioptuold;
    char *ptr = optn;
    char optc;

    UxMainPanelShellContext = (_UxCmainPanelShell *)client_data;

    textReadString(text, &optnStat, optn);
    if (currErrorCode == TEXT_NOCHANGE) return;
    if (currErrorCode == TEXT_OK) {
        while (optc=*ptr++) {
            switch (optc) {
            case 'D': case 'd': ioptd = 1; break;
            case 'W': case 'w': ioptw = 1; break;
            case 'U': case 'u': ioptu = 1; break;
            case ' ': break;
            default: iopterr = 1; break;
            }
        }
        F77_CALLS_I(fpgetoptd,&ioptdold);
        F77_CALLS_I(fpgetoptw,&ioptwold);
        F77_CALLS_I(fpgetoptu,&ioptuold);
        F77_CALLS_I(fpsetoptd,&ioptd);
        F77_CALLS_I(fpsetoptw,&ioptw);
        F77_CALLS_I(fpsetoptu,&ioptu);
        if (iopterr) {
            textSetSemantError(text, &optnStat);
            km_warn_user(mainPanelShell,
                "Only options D, W, and U are legal","");
        }
        else {
            textRemoveSemantError(text, &optnStat);
        }
    }

    if (ioptd!=ioptdold || ioptw!=ioptwold || ioptu!=ioptuold) {
        fp_fitstatupdt(UxMainPanelShellContext);
    }
}

/***********************************************************************
 Optn text field callback for Vector Fit Panel.
***********************************************************************/
static void clb_vectOptnChanged(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    int ioptd=0, ioptu=0, iopterr=0;
    int ioptdold,ioptuold;
    char *ptr = optn;
    char optc;

    UxMainPanelShellContext = (_UxCmainPanelShell *)client_data;

    textReadString(text, &optnStat, optn);
    if (currErrorCode == TEXT_NOCHANGE) return;
    if (currErrorCode == TEXT_OK) {
        while (optc=*ptr++) {
            switch (optc) {
            case 'D': case 'd': ioptd = 1; break;
            case 'U': case 'u': ioptd = 1; break;
            case ' ': break;
            default: iopterr = 1; break;
            }
        }
        F77_CALLS_I(fpgetoptd,&ioptdold);
        F77_CALLS_I(fpgetoptu,&ioptuold);
        F77_CALLS_I(fpsetoptd,&ioptd);
        F77_CALLS_I(fpsetoptu,&ioptu);
        if (iopterr) {
            textSetSemantError(text, &optnStat);
            km_warn_user(mainPanelShell, "Only options D and U are legal","");
        }
        else {
            textRemoveSemantError(text, &optnStat);
        }
    }

    if (ioptd!=ioptdold || ioptu!=ioptuold) {
        fp_fitstatupdt(UxMainPanelShellContext);
    }
}



/*******************************************************************************
     ################### Function and fit model callbacks ##################
*******************************************************************************/

/***********************************************************************
 Function text field callback.
***********************************************************************/
static void clb_ffuncChanged(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    int np,ierflg;
    UxMainPanelShellContext = (_UxCmainPanelShell *)client_data;

    textReadString(text, &ffuncStat, fexpr);
    if (currErrorCode == TEXT_NOCHANGE) return;
    if (currErrorCode == TEXT_OK) {
        F77_CALLS_CPII(fpsetufcn,fexpr,strlen(fexpr),&ufcn,&np,&ierflg);
        if (ierflg) {
            textSetSemantError(text, &ffuncStat);
            XtVaSetValues(nparText,XmNeditable, FALSE, NULL);
            km_warn_user(mainPanelShell, "Illegal fit function","");
        }
        else {
            textRemoveSemantError(text, &ffuncStat);
            if (np == 0) { /* allow npar input */
                XtVaSetValues(nparText,XmNeditable, TRUE, NULL);
            }
            else { /* set fixed npar */
                textWriteInt(nparText, &nparStat, np);
                XtVaSetValues(nparText, XmNeditable, FALSE, NULL);
                changenpar(UxMainPanelShellContext, np);
                eparab_valid = globcc_valid = eminos_valid = True;
            }
        }
    }

    if (ffuncStat.stat==TEXTSTAT_READOK) panelState &= ~FitFuncInvalid;
    else panelState |= FitFuncInvalid;
    setPanelState(UxMainPanelShellContext);
    fp_fitstatupdt(UxMainPanelShellContext);
    grupdate(UxMainPanelShellContext);
}

/***********************************************************************
 Npar text field callback.
***********************************************************************/
static void clb_nparChanged(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    int np;

    UxMainPanelShellContext = (_UxCmainPanelShell *)client_data;

    textReadInt(text, &nparStat, &np, nonEmptyIntOK);
    textIntError((TextReadCode)currErrorCode,
        "npar", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        changenpar(UxMainPanelShellContext, np);
            /* force fit status marking for newly created lines */
        eparab_valid = globcc_valid = eminos_valid = True;
        fp_fitstatupdt(UxMainPanelShellContext);
    }
}

/***********************************************************************
 Fit model text field callback.
***********************************************************************/
static void clb_fmodelChanged(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    int ierflg;
    UxMainPanelShellContext = (_UxCmainPanelShell *)client_data;

    textReadName(text, &fmodelStat, fmodel, nonEmptyNameOK);
    if (currErrorCode == TEXT_NOCHANGE) return;
    textNameError((TextReadCode)currErrorCode,
        "fit model", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        if (histoIdText != NULL) { /* histogram fit model */
            F77_CALLS_CPI(fphsetfmod,fmodel,strlen(fmodel),&fcn,&ierflg);
            if (ierflg) {
                textSetSemantError(text, &fmodelStat);
                km_warn_user(mainPanelShell,
                    "Only chis2 and LogL fit models are implemented","");

            }
            else {
                textRemoveSemantError(text, &fmodelStat);
            }
        }
        else {  /* vector fit model */
            F77_CALLS_CPI(fpvsetfmod,fmodel,strlen(fmodel),&fcn,&ierflg);
            if (ierflg) {
                textSetSemantError(text, &fmodelStat);
                km_warn_user(mainPanelShell,
                    "Only chis2 fit model is implemented for vectors","");

            }
            else {
                textRemoveSemantError(text, &fmodelStat);
            }
        }
    }

    if (fmodelStat.stat==TEXTSTAT_READOK) panelState &= ~FitModelInvalid;
    else panelState |= FitModelInvalid;
    setPanelState(UxMainPanelShellContext);
    fp_fitstatupdt(UxMainPanelShellContext);
    grupdate(UxMainPanelShellContext);
}


/*******************************************************************************
     ####################### Settings callbacks ######################
*******************************************************************************/

/***********************************************************************
 Callbacks for the strategy menu.
***********************************************************************/
void clb_setStrategy0(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "set str 0";
    int icondn;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    strategy = 0;
    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
}

void clb_setStrategy1(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "set str 1";
    int icondn;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    strategy = 1;
    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
}

void clb_setStrategy2(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "set str 2";
    int icondn;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    strategy = 2;
    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
}

/***********************************************************************
 Callbacks for the printout menu.
***********************************************************************/
void clb_setPrintout_1(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "set pri -1";
    int icondn;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    printout = -1;
    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
}

void clb_setPrintout0(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "set pri 0";
    int icondn;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    printout = 0;
    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
}

void clb_setPrintout1(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "set pri 1";
    int icondn;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    printout = 1;
    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
}

void clb_setPrintout2(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "set pri 2";
    int icondn;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    printout = 2;
    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
}

void clb_setPrintout3(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    static char cmd[] = "set pri 3";
    int icondn;

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    printout = 3;
    F77_CALLS_ECIE(mncomd, fcn, cmd, (sizeof cmd - 1), &icondn, ufcn);
}

/***********************************************************************
 Update menu Immediate item callback.
***********************************************************************/
void clb_updateImmediate(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    updatemode = UPDATE_IMMEDIATE;
    grupdate(UxMainPanelShellContext);
}

/***********************************************************************
 Update menu Delayed item callback.
***********************************************************************/
void clb_updateDelayed(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    updatemode = UPDATE_DELAYED;
    grupdate(UxMainPanelShellContext);
}

/***********************************************************************
 Update menu Disabled item callback.
***********************************************************************/
void clb_updateDisabled(
Widget widget,
XtPointer client_data,
XtPointer call_data)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    updatemode = UPDATE_DISABLED;
}

/***********************************************************************
 Errordef text field callback.
***********************************************************************/
static void clb_errordefChanged(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    int ierflg;
    int narg = 1;
    double arglis[1];

    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    textReadFloat(text, &errordefStat, &errordef, positiveFloatOK);
    textFloatError((TextReadCode)currErrorCode, "errordef", -1, mainPanelShell);
    if (currErrorCode == TEXT_OK) {
        static char cmd[] = "set errordef";
        arglis[0] = errordef;
        F77_CALLS_ECDIIE(mnexcm, fcn, cmd, (sizeof cmd - 1),
                         arglis, &narg, &ierflg, ufcn);
        if (ierflg) {
            printf("Error in set errordef %g command\n",errordef);
        }
    }
}

/***********************************************************************
 Tolerance text field callback.
***********************************************************************/
static void clb_toleranceChanged(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    textReadFloat(text, &toleranceStat, &tolerance, positiveFloatOK);
    textFloatError((TextReadCode)currErrorCode,
        "tolerance", -1, mainPanelShell);
}

/***********************************************************************
 Devs text field callback.
***********************************************************************/
static void clb_devsChanged(
Widget text,
XtPointer client_data,
XtPointer call_data)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    textReadFloat(text, &devsStat, &devs, positiveFloatOK);
    textFloatError((TextReadCode)currErrorCode, "devs", -1, mainPanelShell);
}

/***********************************************************************
 Destroy main panel widgets.
***********************************************************************/
static void clb_destroyMainPanel(
Widget shell,
XtPointer client_data,
XtPointer call_data)

{
    UxMainPanelShellContext = (_UxCmainPanelShell *) client_data;

    km_reset_appShell(shell);
    fitPanelContext = NULL;
}

/***********************************************************************
 Create main panel widgets, register callbacks, and set initial values
 for some panel data. The fit object line widgets which differ between
 Histo Fit and Vector fit panels are not created here.
***********************************************************************/
void *createMainPanel()
{
    Widget histoFitPanel;

    if (fitPanelContext != NULL) {
        printf("Error: Only one fitting panel can be opened\n");
        return(NULL);
    }

    set_hfresources(); /* set initial resources */

    /****** create the widgets *****/
    histoFitPanel = create_mainPanelShell(UxTopLevel);
    UxMainPanelShellContext =
    fitPanelContext = (_UxCmainPanelShell *)UxGetContext (histoFitPanel);

    /***** XmNtearOffModel is not defined on all platforms *****/
#ifdef XmNtearOffModel
    XtVaSetValues(hfMenuBar, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);
    XtVaSetValues(fileMenu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);
    XtVaSetValues(minimizeMenu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);
    XtVaSetValues(examineMenu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);
    XtVaSetValues(scanMenu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);
    XtVaSetValues(transMenu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);
    XtVaSetValues(viewMenu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);
    XtVaSetValues(helpMenu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);
#endif

    /****** register destroy callback for what was done in this procedure ***/
    XtAddCallback( mainPanelShell, XmNdestroyCallback,
        (XtCallbackProc) clb_destroyMainPanel,
        (XtPointer) UxMainPanelShellContext );

    /****** get Ids of scroll window subwidgets ******/
    XtVaGetValues(parScrolledWindow,
        XmNclipWindow,&clipWindow,
        XmNhorizontalScrollBar, &horlScrollBar,
        XmNverticalScrollBar, &vertScrollBar,
        NULL);

    utl_initMarkText(ffuncText); /* initialize text marking utility */

    /***** set action tables *****/
    XtAppAddActions( UxAppContext, _Uxactions, XtNumber(_Uxactions) );
#if !defined(CERNLIB_LINUX)
    XtVaSetValues(parScrolledWindow,
        RES_CONVERT(XmNtranslations, transTable1),NULL);
#endif
    /* set Ux context in callback modules when the focus is in */

    /****** create parameter lines header ******/
    parHeader = (swidget)create_parHeaderForm(parForm);
    XtVaSetValues(parHeader,
        XmNleftAttachment, XmATTACH_FORM,
        XmNrightAttachment, XmATTACH_FORM,
        XmNtopAttachment, XmATTACH_FORM, NULL);

    /****** set initial panel status *****/
    panelState = FitObjInvalid|FitFuncInvalid|FitModelInvalid;

    /****** subpanels ******/
    fileSelPanel = NULL;
    contourPanel = NULL;
    scanPanel = NULL;
    vectPanel = NULL;

    /****** set initial values for visual parameters ******/
    scaleMinWidth = 1500;  /* in Xm100TH_FONT_UNITS */

    /****** set initial values for status variables */
    strategy = 0;
    printout = -1;
    updatemode = UPDATE_IMMEDIATE;
    errordef = 1.0;
    tolerance = 0.1;
    devs = 3.0;

    /****** error status variables *****/
    eparab_valid = True;
    eminos_valid = True;
    globcc_valid = True;

    /****** show status variables *****/
    limits_shown = False;
    steps_shown = False;
    eparab_shown = False;
    eminos_shown = False;
    globcc_shown = False;

    vertScrollBarOn = True;
    horlScrollBarOn = True;

    /***** Initialize contour panel variables at the start *****/
    cont_num1 = 1;
    cont_num2 = 2;
    cont_npt = 25;
    cont_xpt = NULL;
    cont_ypt = NULL;
    cont_nfound = 0;
    cont_overlay = False;
    cont_xvecnam[0] = '\0';
    cont_yvecnam[0] = '\0';

    /***** Initialize scan panel variables at the start *****/
    scan_num = 1;
    scan_npt = 25;
    scan_xpt = NULL;
    scan_ypt = NULL;


    /***** initial status for empty text fields ****/
    optnStat.stat = TEXTSTAT_READOK;
    optnStat.change = TEXTSTAT_NOCHANGE;

    ffuncStat.stat = TEXTSTAT_SYNTERROR;
    ffuncStat.change = TEXTSTAT_NOCHANGE;
    nparStat.stat = TEXTSTAT_SYNTERROR;
    nparStat.change = TEXTSTAT_NOCHANGE;
    fmodelStat.stat = TEXTSTAT_SYNTERROR;
    fmodelStat.change = TEXTSTAT_NOCHANGE;

    /***** initial value for text fields *****/
    textWriteFloat(errordefText, &errordefStat, errordef);
    textWriteFloat(toleranceText, &toleranceStat, tolerance);
    textWriteFloat(devsText, &devsStat, devs);


    /***** value changed callbacks for text fields *****/
    XtAddCallback( optnText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &optnStat);

    XtAddCallback( ffuncText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &ffuncStat);
    XtAddCallback( nparText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &nparStat);
    XtAddCallback( fmodelText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &fmodelStat);

    XtAddCallback( errordefText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &errordefStat);
    XtAddCallback( toleranceText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &toleranceStat);
    XtAddCallback( devsText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &devsStat);

    /***** losing focus callbacks for text fields *****/
    XtAddCallback( ffuncText, XmNlosingFocusCallback,
        (XtCallbackProc) clb_ffuncChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( nparText, XmNlosingFocusCallback,
        (XtCallbackProc) clb_nparChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( fmodelText, XmNlosingFocusCallback,
        (XtCallbackProc) clb_fmodelChanged,
        (XtPointer) UxMainPanelShellContext);

    XtAddCallback( errordefText, XmNlosingFocusCallback,
        (XtCallbackProc) clb_errordefChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( toleranceText, XmNlosingFocusCallback,
        (XtCallbackProc) clb_toleranceChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( devsText, XmNlosingFocusCallback,
        (XtCallbackProc) clb_devsChanged,
        (XtPointer) UxMainPanelShellContext);

    /***** activate callbacks for text fields (process Return key) *****/
    XtAddCallback( ffuncText, XmNactivateCallback,
        (XtCallbackProc) clb_ffuncChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( nparText, XmNactivateCallback,
        (XtCallbackProc) clb_nparChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( fmodelText, XmNactivateCallback,
        (XtCallbackProc) clb_fmodelChanged,
        (XtPointer) UxMainPanelShellContext);

    XtAddCallback( errordefText, XmNactivateCallback,
        (XtCallbackProc) clb_errordefChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( toleranceText, XmNactivateCallback,
        (XtCallbackProc) clb_toleranceChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( devsText, XmNactivateCallback,
        (XtCallbackProc) clb_devsChanged,
        (XtPointer) UxMainPanelShellContext);

    /***** pull-down menu callbacks *****/
    filemenuSetClbs(UxMainPanelShellContext);
    transmenuSetClbs(UxMainPanelShellContext);
    helpmenuSetClbs(UxMainPanelShellContext);

    return(fitPanelContext);
}


/***********************************************************************
 Create Histo Fit panel.
***********************************************************************/
void *createHistoFitPanel()
{
    UxMainPanelShellContext =
        (_UxCmainPanelShell *)createMainPanel();
    if (UxMainPanelShellContext==NULL) return(NULL);

    /***** make panel known to KUIP *****/
    km_store_appShell(mainPanelShell, "Histo Fit Panel");

    XtVaSetValues(mainPanelShell,
                    XmNiconName, res_data.histoFitTitle,
                    XmNtitle, res_data.histoFitTitle,
                    NULL );

    /***** to be able to distinguish between the panels *****/
    xvectLabel = NULL;
    yvectLabel = NULL;
    eyvectLabel = NULL;
    xvectText = NULL;
    yvectText = NULL;
    eyvectText = NULL;

    /* Creation of histoIdLabel */
    histoIdLabel = XtVaCreateManagedWidget( "histoIdLabel",
                    xmLabelGadgetClass,
                    mainForm,
                    RES_CONVERT( XmNlabelString, "Histogram" ),
                    XmNleftAttachment, XmATTACH_FORM,
                    XmNleftOffset, 100,
                    XmNunitType, Xm100TH_FONT_UNITS,
                    NULL );
    UxPutContext( histoIdLabel, (char *) UxMainPanelShellContext );

    /* Creation of histoIdText */
    histoIdText = XtVaCreateManagedWidget( "histoIdText",
                    xmTextFieldWidgetClass,
                    mainForm,
                    XmNcolumns, res_data.histidColumns,
                    XmNunitType, Xm100TH_FONT_UNITS,
                    XmNmarginHeight, res_data.textMarginHeight,
                    XmNmarginWidth, res_data.textMarginWidth,
                    XmNleftAttachment, XmATTACH_WIDGET,
                    XmNleftWidget, histoIdLabel,
                    NULL );
    UxPutContext( histoIdText, (char *) UxMainPanelShellContext );

    /* Creation of nbinLabel */
    nbinLabel = XtVaCreateManagedWidget( "nbinLabel",
                    xmLabelGadgetClass,
                    mainForm,
                    RES_CONVERT( XmNlabelString, "(      )" ),
                    XmNunitType, Xm100TH_FONT_UNITS,
                    XmNleftAttachment, XmATTACH_WIDGET,
                    XmNleftWidget, histoIdText,
                    NULL );
    UxPutContext( nbinLabel, (char *) UxMainPanelShellContext );

    /* Creation of rangeLabel */
    rangeLabel = XtVaCreateManagedWidget( "rangeLabel",
                    xmLabelGadgetClass,
                    mainForm,
                    RES_CONVERT( XmNlabelString, "Range" ),
                    XmNunitType, Xm100TH_FONT_UNITS,
                    XmNleftAttachment, XmATTACH_WIDGET,
                    XmNleftOffset, res_data.funcAreaOffset,
                    XmNleftWidget, nbinLabel,
                    NULL );
    UxPutContext( rangeLabel, (char *) UxMainPanelShellContext );

    /* Creation of rangeText */
    rangeText = XtVaCreateManagedWidget( "rangeText",
                    xmTextFieldWidgetClass,
                    mainForm,
                    XmNcolumns, res_data.rangeColumns,
                    XmNunitType, Xm100TH_FONT_UNITS,
                    XmNmarginHeight, res_data.textMarginHeight,
                    XmNmarginWidth, res_data.textMarginWidth,
                    XmNleftAttachment, XmATTACH_WIDGET,
                    XmNleftWidget, rangeLabel,
                    NULL );
    UxPutContext( rangeText, (char *) UxMainPanelShellContext );

    XtVaSetValues(optnLabel,
                    XmNleftAttachment, XmATTACH_WIDGET,
                    XmNleftOffset, res_data.funcAreaOffset,
                    XmNleftWidget, rangeText,
                    NULL );

   /***** initial status for empty text fields ****/
    histoIdStat.stat = TEXTSTAT_SYNTERROR;
    histoIdStat.change = TEXTSTAT_NOCHANGE;
    rangeStat.stat = TEXTSTAT_EMPTY;
    rangeStat.change = TEXTSTAT_NOCHANGE;

    /***** value changed callbacks for text fields *****/
    XtAddCallback( histoIdText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &histoIdStat);
    XtAddCallback(rangeText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &rangeStat);

    /***** losing focus callbacks for text fields *****/
    XtAddCallback( histoIdText, XmNlosingFocusCallback,
        (XtCallbackProc) clb_histoIdChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( rangeText, XmNlosingFocusCallback,
        (XtCallbackProc) clb_rangeChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( optnText, XmNlosingFocusCallback,
        (XtCallbackProc) clb_histoOptnChanged,
        (XtPointer) UxMainPanelShellContext);

    /***** activate callbacks for text fields (process Return key) *****/
    XtAddCallback( histoIdText, XmNactivateCallback,
        (XtCallbackProc) clb_histoIdChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( rangeText, XmNactivateCallback,
        (XtCallbackProc) clb_rangeChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( optnText, XmNactivateCallback,
        (XtCallbackProc) clb_histoOptnChanged,
        (XtPointer) UxMainPanelShellContext);

    return(UxMainPanelShellContext);
}

/***********************************************************************
 Create Vector Fit panel.
***********************************************************************/
void *createVectorFitPanel()
{
    UxMainPanelShellContext =
        (_UxCmainPanelShell *)createMainPanel();
    if (UxMainPanelShellContext==NULL) return(NULL);

    /***** make panel known to KUIP *****/
    km_store_appShell(mainPanelShell, "Vector Fit Panel");

    XtVaSetValues(mainPanelShell,
                    XmNiconName, res_data.vectorFitTitle,
                    XmNtitle, res_data.vectorFitTitle,
                    NULL );

    /***** to be able to distinguish between the panels *****/
    histoIdLabel = NULL;
    histoIdText = NULL;
    nbinLabel = NULL;
    rangeLabel = NULL;
    rangeText = NULL;

    /* Creation of xvectLabel */
    xvectLabel = XtVaCreateManagedWidget( "xvectLabel",
                    xmLabelGadgetClass,
                    mainForm,
                    RES_CONVERT( XmNlabelString, "X" ),
                    XmNleftAttachment, XmATTACH_FORM,
                    XmNleftOffset, 100,
                    XmNunitType, Xm100TH_FONT_UNITS,
                    NULL );
    UxPutContext( xvectLabel, (char *) UxMainPanelShellContext );

    /* Creation of xvectText */
    xvectText = XtVaCreateManagedWidget( "xvectText",
                    xmTextFieldWidgetClass,
                    mainForm,
                    XmNcolumns, res_data.vectnameColumns,
                    XmNunitType, Xm100TH_FONT_UNITS,
                    XmNmarginHeight, res_data.textMarginHeight,
                    XmNmarginWidth, res_data.textMarginWidth,
                    XmNleftAttachment, XmATTACH_WIDGET,
                    XmNleftWidget, xvectLabel,
                    NULL );
    UxPutContext( xvectText, (char *) UxMainPanelShellContext );

    /* Creation of yvectLabel */
    yvectLabel = XtVaCreateManagedWidget( "yvectLabel",
                    xmLabelGadgetClass,
                    mainForm,
                    RES_CONVERT( XmNlabelString, "Y" ),
                    XmNunitType, Xm100TH_FONT_UNITS,
                    XmNleftAttachment, XmATTACH_WIDGET,
                    XmNleftWidget, xvectText,
                    NULL );
    UxPutContext( yvectLabel, (char *) UxMainPanelShellContext );

    /* Creation of yvectText */
    yvectText = XtVaCreateManagedWidget( "yvectText",
                    xmTextFieldWidgetClass,
                    mainForm,
                    XmNcolumns, res_data.vectnameColumns,
                    XmNunitType, Xm100TH_FONT_UNITS,
                    XmNmarginHeight, res_data.textMarginHeight,
                    XmNmarginWidth, res_data.textMarginWidth,
                    XmNleftAttachment, XmATTACH_WIDGET,
                    XmNleftWidget, yvectLabel,
                    NULL );
    UxPutContext( yvectText, (char *) UxMainPanelShellContext );

    /* Creation of eyvectLabel */
    eyvectLabel = XtVaCreateManagedWidget( "eyvectLabel",
                    xmLabelGadgetClass,
                    mainForm,
                    RES_CONVERT( XmNlabelString, "EY" ),
                    XmNunitType, Xm100TH_FONT_UNITS,
                    XmNleftAttachment, XmATTACH_WIDGET,
                    XmNleftWidget, yvectText,
                    NULL );
    UxPutContext( eyvectLabel, (char *) UxMainPanelShellContext );

    /* Creation of eyvectText */
    eyvectText = XtVaCreateManagedWidget( "eyvectText",
                    xmTextFieldWidgetClass,
                    mainForm,
                    XmNcolumns, res_data.vectnameColumns,
                    XmNunitType, Xm100TH_FONT_UNITS,
                    XmNmarginHeight, res_data.textMarginHeight,
                    XmNmarginWidth, res_data.textMarginWidth,
                    XmNleftAttachment, XmATTACH_WIDGET,
                    XmNleftWidget, eyvectLabel,
                    NULL );
    UxPutContext( eyvectText, (char *) UxMainPanelShellContext );

    XtVaSetValues(optnLabel,
                    XmNleftAttachment, XmATTACH_WIDGET,
                    XmNleftOffset, res_data.funcAreaOffset,
                    XmNleftWidget, eyvectText,
                    NULL );


   /***** initial status for empty text fields ****/
    xvectStat.stat = TEXTSTAT_EMPTY;
    xvectStat.change = TEXTSTAT_NOCHANGE;
    yvectStat.stat = TEXTSTAT_EMPTY;
    yvectStat.change = TEXTSTAT_NOCHANGE;
    eyvectStat.stat = TEXTSTAT_EMPTY;
    eyvectStat.change = TEXTSTAT_NOCHANGE;

    /***** value changed callbacks for text fields *****/
    XtAddCallback( xvectText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &xvectStat);
    XtAddCallback( yvectText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &yvectStat);
    XtAddCallback( eyvectText, XmNvalueChangedCallback,
        (XtCallbackProc) clb_textChanged, (XtPointer) &eyvectStat);

    /***** losing focus callbacks for text fields *****/
    XtAddCallback( xvectText, XmNlosingFocusCallback,
        (XtCallbackProc) clb_xvectChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( yvectText, XmNlosingFocusCallback,
        (XtCallbackProc) clb_yvectChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( eyvectText, XmNlosingFocusCallback,
        (XtCallbackProc) clb_eyvectChanged,
        (XtPointer) UxMainPanelShellContext);

    /***** activate callbacks for text fields (process Return key) *****/
    XtAddCallback( xvectText, XmNactivateCallback,
        (XtCallbackProc) clb_xvectChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( yvectText, XmNactivateCallback,
        (XtCallbackProc) clb_yvectChanged,
        (XtPointer) UxMainPanelShellContext);
    XtAddCallback( eyvectText, XmNactivateCallback,
        (XtCallbackProc) clb_eyvectChanged,
        (XtPointer) UxMainPanelShellContext);

    return(UxMainPanelShellContext);
}

