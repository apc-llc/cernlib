/*
 * $Id: vfpanel.c,v 1.1.1.1 1996/03/01 11:39:09 mclareni Exp $
 *
 * $Log: vfpanel.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:09  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/05 28/06/95  10.41.03  by  Unknown*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Create, start, change status, and destroy Vector Fit Panel --- outside
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
 Destroy callback for the panel invoked by PAW command vector/fit.
***********************************************************************/
static void clb_destroyMainPanel(
Widget shell,
XtPointer client_data,
XtPointer call_data)
{
    F77_CALLS_(fpvgrend); /* end the graphics */

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

/*******************************************************************************
 Create and start vector fit panel invoked by PAW command vector/fit.
*******************************************************************************/
#ifndef vms

   void fpvcmndpanel_(
      char *_Uxfexpr,
      void *_Uxfcn, void *_Uxufcn,
      int *_Uxnpar, double *_Uxvpar,
      double *_Uxvlow, double *_Uxvupp,
      double *_Uxvstep, double *_Uxveparab, int felen)
   {

#else /* vms */

   void fpvcmndpanel(
      struct dsc$descriptor_s *dsc_Uxfexpr,
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
    char xvname[MAX_PAWVECNAME+1];
    char yvname[MAX_PAWVECNAME+1];
    char eyvname[MAX_PAWVECNAME+1];

    setbuf(stdout, NULL);

    mnInitialized = True; /* Minuit was initialized in hminut */

    /***** create the panel *****/
    UxMainPanelShellContext =
        (_UxCmainPanelShell *)createVectorFitPanel(); /* create main panel */
    if (UxMainPanelShellContext==NULL) return;

    XtAddCallback( mainPanelShell, XmNdestroyCallback,
        (XtCallbackProc) clb_destroyMainPanel,
        (XtPointer) UxMainPanelShellContext );

    grupdate = vfgrupdate; /* set appropriate graphics update */
    F77_CALLS_(fpvgrinit); /* initialize the graphical data */

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

        /***** get vector names *****/
    F77_CALLS_CCC(fpvgetnam,
        xvname,MAX_PAWVECNAME,yvname,MAX_PAWVECNAME,eyvname,MAX_PAWVECNAME);
    strftoc(xvname,MAX_PAWVECNAME);
    strftoc(yvname,MAX_PAWVECNAME);
    strftoc(eyvname,MAX_PAWVECNAME);
    textWriteName(xvectText,&xvectStat,xvname);
    textWriteName(yvectText,&yvectStat,yvname);
    textWriteName(eyvectText,&eyvectStat,eyvname);
    F77_CALLS_CCCCC(fpgetpnam,
        vec_pnam,MAX_PAWVECNAME,
        vec_lnam,MAX_PAWVECNAME,vec_unam,MAX_PAWVECNAME,
        vec_snam,MAX_PAWVECNAME,vec_enam,MAX_PAWVECNAME);
    strftoc(vec_pnam,MAX_PAWVECNAME);
    strftoc(vec_lnam,MAX_PAWVECNAME);
    strftoc(vec_unam,MAX_PAWVECNAME);
    strftoc(vec_snam,MAX_PAWVECNAME);
    strftoc(vec_enam,MAX_PAWVECNAME);

        /***** function expression *****/
    strftoc(_Uxfexpr,felen);
    strcpy(fexpr,_Uxfexpr);
    textWriteName(ffuncText,&ffuncStat,fexpr);
    textWriteInt(nparText,&nparStat,npar);

        /***** write fit model fcn name *****/
    fp_vgetfmod(fmodel);
    textWriteName(fmodelText,&fmodelStat,fmodel);

        /***** write options *****/
    fp_vsetoptn(UxMainPanelShellContext);

        /***** get errordef value *****/
    get_mnerrdef(&errordef);
    textWriteFloat(errordefText, &errordefStat, errordef);


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

    grupdate(UxMainPanelShellContext); /* draw the histogram */

    endPanelEventLoop = False;
}
