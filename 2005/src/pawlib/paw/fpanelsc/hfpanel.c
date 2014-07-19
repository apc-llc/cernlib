/*
 * $Id: hfpanel.c,v 1.1.1.1 1996/03/01 11:39:07 mclareni Exp $
 *
 * $Log: hfpanel.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:07  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/05 28/06/95  10.36.29  by  Unknown*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Create, start, change status, and destroy Histo Fit Panel --- outside
 callable functions.
 =====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>


#ifdef __cplusplus
extern "C" {
#endif

#include "fpanelsh/errordb.h"
#include "fpanelsh/fplocal.h"
#include "hmotif/uxxt.h"
#undef UxParent
#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/mainpanel.h"
#undef CONTEXT_MACRO_ACCESS

#ifdef __cplusplus
}  /* extern "C" */
#endif

extern int mnInitialized;

/***********************************************************************
 Destroy callback for the panel invoked by PAW command histo/fit.
***********************************************************************/
static void clb_destroyCmnd(
Widget shell,
XtPointer client_data,
XtPointer call_data)
{
    F77_CALLS_(fphgrend); /* end the graphics */

    free(parLines);

    free(parname);

    free(vpar);
    free(vlow);
    free(vupp);
    free(vstep);
    free(veparab);

    free(veminus);
    free(veplus);

    free(par_start);
    free(par_store);

    free(vscalemin);
    free(vscalemax);

    free(par_active);
    free(vfixrel);
    free(par_enabled);

    endPanelEventLoop = True;
}

/***********************************************************************
 Create and start histo fit panel invoked by PAW command histo/fit.
***********************************************************************/
#ifndef vms

   void fphcmndpanel_(
      int *_Uxhistid, char *_Uxfexpr,
      void *_Uxfcn, void *_Uxufcn,
      int *_Uxnpar, double *_Uxvpar,
      double *_Uxvlow, double *_Uxvupp,
      double *_Uxvstep, double *_Uxveparab, int felen)
   {

#else /* vms */

   void fphcmndpanel(
      int *_Uxhistid, struct dsc$descriptor_s *dsc_Uxfexpr,
      void *_Uxfcn, void *_Uxufcn,
      int *_Uxnpar, double *_Uxvpar,
      double *_Uxvlow, double *_Uxvupp,
      double *_Uxvstep, double *_Uxveparab)

   {

      int felen = dsc_Uxfexpr->dsc$w_length;
      char *_Uxfexpr = dsc_Uxfexpr->dsc$a_pointer;

#endif

    char *cptr;
    int i,iline,ipar;
    char tstr[50];
    float xmi, xma, ymi, yma;

    setbuf(stdout, NULL);

    mnInitialized = True; /* Minuit was initialized in hminut */

    /***** create the panel *****/
    UxMainPanelShellContext =
        (_UxCmainPanelShell *)createHistoFitPanel(); /* create main panel */
    if (UxMainPanelShellContext==NULL) return;

    XtAddCallback( mainPanelShell, XmNdestroyCallback,
        (XtCallbackProc) clb_destroyCmnd,
        (XtPointer) UxMainPanelShellContext );

    grupdate = hfgrupdate; /* set appropriate graphics update */
    F77_CALLS_(fphgrinit); /* initialize the graphical data */

    /***** define vectors for parameters data *****/
    npar = MAX_PARNUMBER;
    parLines = (Widget *)malloc(npar*sizeof(Widget));
        for (iline=0; iline<npar; iline++) parLines[iline] = NULL;

    parname = (char **) malloc(npar*(MAX_MINUIT_NAMELEN+4+sizeof(char *)));
    cptr = (char *)parname + npar*sizeof(char *);
    for (ipar=0; ipar<npar; ipar++) {
        parname[ipar] = cptr;
        cptr += MAX_MINUIT_NAMELEN+4;
    }

    vpar = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) vpar[iline] = 0.0;
    vlow = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) vlow[iline] = 0.0;
    vupp = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) vupp[iline] = 0.0;
    vstep = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) vstep[iline] = 0.0;
    veparab = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) veparab[iline] = 0.0;

    veminus = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) veminus[iline] = 0.0;
    veplus = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) veplus[iline] = 0.0;

    par_start = (double *)malloc(sizeof(double)*npar);
    par_store = (double *)malloc(sizeof(double)*npar);

    par_active = (int *)malloc(sizeof(int)*npar);
        for (iline=0; iline<npar; iline++) par_active[iline] = True;
    vfixrel = (int *)malloc(sizeof(int)*npar);
        for (iline=0; iline<npar; iline++) vfixrel[iline] = True;
    par_enabled = (int *)malloc(sizeof(int)*npar);
        for (iline=0; iline<npar; iline++) par_enabled[iline] = True;

    vscalemin = (double *)malloc(sizeof(double)*npar);
    vscalemax = (double *)malloc(sizeof(double)*npar);

    /***** get PAW vectors to use in the panel */
    npar = *_Uxnpar;
    for (ipar=0; ipar<npar; ipar++) { /* names are copied into local array */
        get_mnname(ipar, parname[ipar]);
    }
    for (iline=0; iline<npar; iline++) vpar[iline] = _Uxvpar[iline];
    for (iline=0; iline<npar; iline++) vlow[iline] = _Uxvlow[iline];
    for (iline=0; iline<npar; iline++) vupp[iline] = _Uxvupp[iline];
    for (iline=0; iline<npar; iline++) vstep[iline] = _Uxvstep[iline];
    for (iline=0; iline<npar; iline++) veparab[iline] = _Uxveparab[iline];

    /***** set current PAW function *****/
    fcn = _Uxfcn;
    ufcn = _Uxufcn;

        /***** write histogram id and number of bins *****/
    histid = *_Uxhistid;
    textWriteInt(histoIdText,&histoIdStat,histid);
        /***** set values of histogram's internal data, write them on panel */
    fp_hsetdata(histid,UxMainPanelShellContext);

        /***** function expression *****/
    strftoc(_Uxfexpr,felen);
    strcpy(fexpr,_Uxfexpr);
    textWriteName(ffuncText,&ffuncStat,fexpr);
    textWriteInt(nparText,&nparStat,npar);

        /***** write fit model fcn name *****/
    fp_hgetfmod(fmodel);
    textWriteName(fmodelText,&fmodelStat,fmodel);

        /***** write options *****/
    fp_hsetoptn(UxMainPanelShellContext);

        /***** get errordef value *****/
    get_mnerrdef(&errordef);
    textWriteFloat(errordefText, &errordefStat, errordef);

        /***** get vector names *****/
    F77_CALLS_CCCCC(fpgetpnam,
        vec_pnam,MAX_PAWVECNAME,
        vec_lnam,MAX_PAWVECNAME,vec_unam,MAX_PAWVECNAME,
        vec_snam,MAX_PAWVECNAME,vec_enam,MAX_PAWVECNAME);
    strftoc(vec_pnam,MAX_PAWVECNAME);
    strftoc(vec_lnam,MAX_PAWVECNAME);
    strftoc(vec_unam,MAX_PAWVECNAME);
    strftoc(vec_snam,MAX_PAWVECNAME);
    strftoc(vec_enam,MAX_PAWVECNAME);

    /***** create parameter lines *****/
    createParLines(UxMainPanelShellContext, 0, npar);

    /***** create pop-up panels *****/
#if 0
    contourPanel = createContourPanel(UxMainPanelShellContext);
    scanPanel = createScanPanel(UxMainPanelShellContext);
    vectPanel = createVectPanel(UxMainPanelShellContext);
#endif

    /****** synchronize panel data with PAW and minuit data ******/
    get_mnparms((void *)UxMainPanelShellContext, False);

    /****** fit object, function, and model are defined *****/
    panelState = PanelStateValid;

    /***** realize widget, etc by XtPopup *****/
    UxPopupInterface(mainPanelShell, no_grab);
    resizeParForm(UxMainPanelShellContext);

    fp_fitstatupdt(UxMainPanelShellContext);
    grupdate(UxMainPanelShellContext); /* draw the histogram */

    endPanelEventLoop = False;
}

/***********************************************************************
 Destroy callback for the panel invoked through PAW++ browser.
***********************************************************************/
static void clb_destroyBrs(
Widget shell,
XtPointer client_data,
XtPointer call_data)
{
    free(parLines);

    free(parname);

    free(vpar);
    free(vlow);
    free(vupp);
    free(vstep);
    free(veparab);

    free(veminus);
    free(veplus);

    free(par_start);
    free(par_store);

    free(vscalemin);
    free(vscalemax);

    free(par_active);
    free(vfixrel);
    free(par_enabled);

    endPanelEventLoop = True;
}

/***********************************************************************
 Create and start histo fit panel invoked through PAW++ browser.
***********************************************************************/
void fphbrspanel()
{
    char *cptr;
    int i,iline,ipar;

    setbuf(stdout, NULL);

    /***** create the panel *****/
    UxMainPanelShellContext =
        (_UxCmainPanelShell *)createHistoFitPanel(); /* create main panel */
    if (UxMainPanelShellContext==NULL) return;

    XtAddCallback( mainPanelShell, XmNdestroyCallback,
        (XtCallbackProc) clb_destroyBrs,
        (XtPointer) UxMainPanelShellContext );

    grupdate = hfgrupdate; /* set appropriate graphics update */

    /***** define vectors for parameters data *****/
    npar = MAX_PARNUMBER;
    parLines = (Widget *)malloc(npar*sizeof(Widget));
        for (iline=0; iline<npar; iline++) parLines[iline] = NULL;

    parname = (char **) malloc(npar*(MAX_MINUIT_NAMELEN+4+sizeof(char *)));
    cptr = (char *)parname + npar*sizeof(char *);
    for (ipar=0; ipar<npar; ipar++) {
        parname[ipar] = cptr;
        cptr += MAX_MINUIT_NAMELEN+4;
    }

    vpar = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) vpar[iline] = 0.0;
    vlow = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) vlow[iline] = 0.0;
    vupp = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) vupp[iline] = 0.0;
    vstep = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) vstep[iline] = 0.0;
    veparab = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) veparab[iline] = 0.0;

    veminus = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) veminus[iline] = 0.0;
    veplus = (double *)malloc(sizeof(double)*npar);
        for (iline=0; iline<npar; iline++) veplus[iline] = 0.0;

    par_start = (double *)malloc(sizeof(double)*npar);
    par_store = (double *)malloc(sizeof(double)*npar);

    par_active = (int *)malloc(sizeof(int)*npar);
        for (iline=0; iline<npar; iline++) par_active[iline] = True;
    vfixrel = (int *)malloc(sizeof(int)*npar);
        for (iline=0; iline<npar; iline++) vfixrel[iline] = True;
    par_enabled = (int *)malloc(sizeof(int)*npar);
        for (iline=0; iline<npar; iline++) par_enabled[iline] = True;

    vscalemin = (double *)malloc(sizeof(double)*npar);
    vscalemax = (double *)malloc(sizeof(double)*npar);

        /***** write options *****/
    fp_hsetoptn(UxMainPanelShellContext);

        /***** get errordef value *****/
    get_mnerrdef(&errordef);
    textWriteFloat(errordefText, &errordefStat, errordef);

        /***** initialize vector names *****/
    vec_pnam[0] = '\0';
    vec_lnam[0] = '\0';
    vec_unam[0] = '\0';
    vec_snam[0] = '\0';
    vec_enam[0] = '\0';

    /****** fit object, function, and model are undefined *****/
    npar = 0;
    panelState = FitObjInvalid|FitFuncInvalid|FitModelInvalid;

    /***** initialize Minuit *****/
     if (mnInitialized == False) {
         F77_CALLS_(fpmninit);
         mnInitialized = True;
     }

    /***** realize widget, etc by XtPopup *****/
    UxPopupInterface(mainPanelShell, no_grab);
    resizeParForm(UxMainPanelShellContext);
}

/***********************************************************************
 Display given histogram id in the Histogram text field on the panel.
***********************************************************************/
int fp_hsetid(int ihid, void *mctxt)
{
    int ierflg;
    char tstr[50];
    float xmi, xma, ymi, yma;
    char hsid[20];

    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

        /***** write histogram id *****/
    histid = ihid;
    textWriteInt(histoIdText,&histoIdStat,histid);

    histoIdStat.change = TEXTSTAT_CHANGED;
    XtCallCallbacks(histoIdText, XmNactivateCallback, NULL);

    return(0);
}

