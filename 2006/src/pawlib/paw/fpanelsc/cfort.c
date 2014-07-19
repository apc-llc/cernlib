/*
 * $Id: cfort.c,v 1.1.1.1 1996/03/01 11:39:06 mclareni Exp $
 *
 * $Log: cfort.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:06  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.40  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Interface between Fit Panel and HBOOK, PAW, and MINUIT.
 =====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/
#include <stdlib.h>
#include <stdio.h>
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

#ifdef __cplusplus
}  /* extern "C" */
#endif


/***********************************************************************
 Convert Fortran string to C string
***********************************************************************/
char *strftoc(char *s, int l)
{
    while (l-- && s[l] == ' ');
    s[l+1] = '\0';
    return(s);
}

/***********************************************************************
 Convert C string to Fortran string
***********************************************************************/
char *strctof(char *s, int l)
{
    char *sp = s;
    while (*sp) sp++;           /* find first trailing character */
    while (sp-s<l) *sp++ = ' '; /* fill trailing space with blanks */
    return(s);
}


/***********************************************************************
##################### Interface with PAW and HBOOK #####################
***********************************************************************/

/***********************************************************************
 Read histogram data
***********************************************************************/
void chgive(int id,
    int *nxp, float *xmip, float *xmap,int *nyp, float *ymip, float *ymap)
{
    char TITLE[4];
    INTEGER NWT;
    INTEGER LOC;
    INTEGER LENTITLE = 4;


    F77_CALLS_ICIRRIRRII(hgive, &id, TITLE, LENTITLE,
                         nxp, xmip, xmap, nyp, ymip, ymap, &NWT, &LOC);
}

/***********************************************************************
 Plot a function using HPLOT
***********************************************************************/
void hffplot(int npta, double xpta[], double ypta[],
char xtit[], char ytit[],
PlotType pt, int ovrl)
{
    int i;
    float *xptf, *yptf;
    float xmin,xmax,ymin,ymax;
    float xdel,ydel;
    int num1 = 1;

    /***** set current attributes *****/
#if 0
    hplatt_(&num1);
#endif

    xptf = (float *)malloc(2*npta*sizeof(float));
    yptf = xptf+npta;
    xmin = xmax = xpta[0];
    ymin = ymax = ypta[0];
    for (i=0; i<npta; i++) {
        xmin = min(xmin,xpta[i]);
        xmax = max(xmax,xpta[i]);
        ymin = min(ymin,ypta[i]);
        ymax = max(ymax,ypta[i]);
        xptf[i] = xpta[i];
        yptf[i] = ypta[i];
    }

    if (ovrl == False) {
            /***** set plot margins *****/
        ydel = ymax - ymin;
        ymax += 0.05*ydel;
        if (pt == ContPlot) {
            xdel = xmax - xmin;
            xmin -= 0.05*xdel;
            xmax += 0.05*xdel;
            ymin -= 0.05*ydel;
        }

        F77_CALLS_RRRRC(hplfra,&xmin,&xmax,&ymin,&ymax," ",1);
        F77_CALLS_CC(hplax,xtit,strlen(xtit),ytit,strlen(ytit));
    }

    F77_CALLS_IRRC(igraph,&npta,xptf,yptf,"C",1);
    F77_CALLS_(igterm);
    free(xptf);
}

/***********************************************************************
 Save given double precision vector in a KUIP vector. If a vector with
 the specified name does not exist in KUIP, a vector with the specified
 length is created. If the existing vector is of wrong type or it is
 too short, an error message is given.
***********************************************************************/
int fpsavevc(char vname[], int n, double vect[], Widget panelShell)
{
    int ierflg;
    char errmsg[MAX_ERRMSGLEN];

    F77_CALLS_CIDI(fpsavev, vname, strlen(vname), &n, vect, &ierflg);
    switch (ierflg) {
    case 1:
        sprintf(errmsg,"Existing vector %s not REAL\n",vname);
        km_warn_user(panelShell,errmsg,"");
    break;
    case 2:
        sprintf(errmsg,"Existing vector %s too short\n",vname);
        km_warn_user(panelShell,errmsg,"");
    break;
    default:;
    }
    return(ierflg);
}

/***********************************************************************
 Update graphics for Histo Fit Panel if
     a) both histogram and fit function are valid and
     b) the histogram is 1D (there is no way to plot fit function for 2D).
***********************************************************************/
void hfgrupdate(void *mctxt)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

    if (((panelState&~FitModelInvalid)==PanelStateValid) && (histodim==1))
        F77_CALLS_IDE(fphgrupdt,&npar,vpar,ufcn);
}

/***********************************************************************
 Update graphics for Vector Fit Panel if the vector pair and fit
 function are both valid.
***********************************************************************/
void vfgrupdate(void *mctxt)
{
    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

    if ((panelState&~FitModelInvalid) == PanelStateValid)
        F77_CALLS_IDE(fpvgrupdt,&npar,vpar,ufcn);
}


/***********************************************************************
######################### Interface with MINUIT ########################
***********************************************************************/

/***********************************************************************
 Read minuit status
***********************************************************************/
void get_mnerrdef(double *errdefp)
{
    int ndummy,istat;
    double fdummy;
    F77_CALLS_DDDIII(mnstat,&fdummy,&fdummy,errdefp,&ndummy,&ndummy,&istat);
}

/***********************************************************************
 Import a parameter name from minuit.
***********************************************************************/
void get_mnname(int iparnum, char parnamei[])
{
    int i, numpar, ivarbl;
    double vpari,veparabi,vlowi,vuppi;

    numpar = iparnum + 1;
    F77_CALLS_ICDDDDI(mnpout,&numpar,parnamei,MAX_MINUIT_NAMELEN,
        &vpari,&veparabi,&vlowi,&vuppi,&ivarbl);
    strftoc(parnamei, MAX_MINUIT_NAMELEN);
}

/***********************************************************************
 Import minuit names and display them parameter name field.
***********************************************************************/
void get_mnnames(void *ctx)
{
    int i, numpar, ivarbl;
    double vpari,veparabi,vlowi,vuppi;

    UxMainPanelShellContext = (_UxCmainPanelShell *)ctx;

    for(i=0; i<npar; i++) {
        UxParLineFormContext = (_UxCparLineForm *) UxGetContext(parLines[i]);
        numpar = i + 1;
        F77_CALLS_ICDDDDI(mnpout,&numpar,parname[i],MAX_MINUIT_NAMELEN,
            &vpari,&veparabi,&vlowi,&vuppi,&ivarbl);
        strftoc(parname[i], MAX_MINUIT_NAMELEN);
        textWriteName(parNameText, &parNameStat, parname[i]);
    }
}

/***********************************************************************
 Import minuit parameters, fmin, fedm, and display them in their
 respective fields. If ifeparab is True, import and display parabolic
 errors as well.
***********************************************************************/
void get_mnparms(void *ctx, Boolean ifeparab)
{
    int i, numpar, ivarbl;
    double vpari,veparabi,vlowi,vuppi;
    char parnamei[MAX_MINUIT_NAMELEN];
    int updatemodesave;
    double fmin,fedm;
    int npari,nparx,istat;
    char textstr[50];

    UxMainPanelShellContext = (_UxCmainPanelShell *)ctx;

    updatemodesave = updatemode;
    updatemode = UPDATE_DISABLED;
    for(i=0; i<npar; i++) {
        UxParLineFormContext = (_UxCparLineForm *) UxGetContext(parLines[i]);
        numpar = i + 1;
        F77_CALLS_ICDDDDI(mnpout,&numpar,parnamei,MAX_MINUIT_NAMELEN,
            &vpari,&veparabi,&vlowi,&vuppi,&ivarbl);
        setParValueAndScale(vpari, UxParLineFormContext, PARCHN_MINUIT);
        if (ifeparab == True) {
            veparab[i] = veparabi;
            textWriteFloat(parEparabText, NULL, veparabi);
        }
    }
    grupdate(UxMainPanelShellContext);
    updatemode = updatemodesave;

    F77_CALLS_DDDIII(mnstat,&fmin,&fedm,&errordef,&npari,&nparx,&istat);
    sprintf(textstr,"%.4g",fmin);
    XmTextSetString(fminText,textstr);
    sprintf(textstr,"%.4g",fedm);
    XmTextSetString(fedmText,textstr);
}

/***********************************************************************
 Import minuit globcc estimates and display them in respective field.
***********************************************************************/
void get_mnglobcc(void *ctx)
{
    int i, numpar, istat;
    double veparabi,eplusi,eminusi,globcci;
    int ndummy;
    double fdummy;

    UxMainPanelShellContext = (_UxCmainPanelShell *)ctx;

    for(i=0; i<npar; i++) {
        UxParLineFormContext = (_UxCparLineForm *) UxGetContext(parLines[i]);
        numpar = i + 1;
        F77_CALLS_IDDDD(mnerrs,&numpar,&eplusi,&eminusi,&veparabi,&globcci);
        textWriteFloat(parGlobccText, NULL, globcci);
    }

    F77_CALLS_DDDIII(mnstat,&fdummy,&fdummy,&fdummy,&ndummy,&ndummy,&istat);

    if (istat==3) {
        globcc_valid = True;
        for(i=0; i<npar; i++) {
            UxParLineFormContext =
                (_UxCparLineForm *) UxGetContext(parLines[i]);
            utl_markTextValid(parGlobccText);
        }
    }
}

/***********************************************************************
 Import minuit minos errors and globcc estimates and display them in
 respective fields.
***********************************************************************/
void get_mneminos(void *ctx)
{
    int i, numpar;
    double veparabi,eplusi,eminusi,globcci;

    UxMainPanelShellContext = (_UxCmainPanelShell *)ctx;

    for(i=0; i<npar; i++) {
        UxParLineFormContext = (_UxCparLineForm *) UxGetContext(parLines[i]);
        numpar = i + 1;
        F77_CALLS_IDDDD(mnerrs,&numpar,&eplusi,&eminusi,&veparabi,&globcci);
        textWriteFloat(parEminusText, NULL, eminusi);
        utl_markTextValid(parEminusText);
        textWriteFloat(parEplusText, NULL, eplusi);
        utl_markTextValid(parEplusText);
    }
    eminos_valid = True;
}

/***********************************************************************
 Set parameter value inside Minuit
***********************************************************************/
void set_mnparm(int iparnum, char parnamei[], double vpari,double vstepi,
double vlowi,double vuppi)
{
    int numpar, ivarbl;
    char parnamef[MAX_MINUIT_NAMELEN+1];

    parnamef[MAX_MINUIT_NAMELEN] = '\0';
    numpar = iparnum + 1;
    strncpy(parnamef,parnamei,MAX_MINUIT_NAMELEN);
    strctof(parnamef, MAX_MINUIT_NAMELEN);
    F77_CALLS_ICDDDDI(mnparm, &numpar, parnamef, MAX_MINUIT_NAMELEN,
            &vpari,&vstepi, &vlowi,&vuppi,&ivarbl);
}

/***********************************************************************
 Set parameter values inside Minuit for all the parameters.
***********************************************************************/
void set_mnparms(void *ctx)
{
    int i;
    UxMainPanelShellContext = (_UxCmainPanelShell *)ctx;

    for(i=0; i<npar; i++) {
        set_mnparm(i,parname[i],vpar[i],vstep[i],vlow[i],vupp[i]);
    }
}

/***********************************************************************
 Check if a REAL KUIP vector with the given name exists. If a vector
 does not exist or has a wrong type, display an error message.
***********************************************************************/
Boolean fp_vexist(Widget shell,char *vname)
{
    int ierflg;
    char errmsg[80];

    F77_CALLS_CI(fpvexist,vname,strlen(vname),&ierflg);
    switch (ierflg) {
    case 1:
        sprintf(errmsg,"Vector %s has wrong type\n",vname);
        km_warn_user(shell, errmsg, "");
    break;
    case 2:
        sprintf(errmsg,"Vector %s does not exist\n",vname);
        km_warn_user(shell, errmsg, "");
    break;
    default:;
    }

    if (ierflg==0) return(True);
    else return(False);
}

/***********************************************************************
 Find out what is the current fit model set inside PAW for
 Histo Fit Panel.
***********************************************************************/
Boolean fp_hgetfmod(char *fmodname)
{
    int loglmode;

    F77_CALLS_I(fphgetfmod,&loglmode);

    if (loglmode) strcpy(fmodname,"LogL");
    else strcpy(fmodname,"chis2");

    return(True);
}

/***********************************************************************
 Find out what is the current fit model set inside PAW for
 Vector Fit Panel.
***********************************************************************/
Boolean fp_vgetfmod(char *fmodname)
{
    int userfmod;

    F77_CALLS_I(fpvgetfmod,&userfmod);

    if (userfmod) strcpy(fmodname,"User_fit");
    else strcpy(fmodname,"chis2");

    return(True);
}

/***********************************************************************
 Update fit state. Update fit model value and mark Minuit computed
 errors invalid. This function is called when fit object, fit model,
 fit function or its parameters have changed.
***********************************************************************/
void fp_fitstatupdt(void *mctxt)
{
    int iflag = 0;
    int iline;
    double fval;
    char textstr[50];

    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

    /***** compute the functional value *****/
    if (panelState == PanelStateValid) {
        ((FCNTYPE)fcn)(&npar,NULL,&fval,vpar,&iflag,ufcn);
        sprintf(textstr,"%.4g",fval);
        XmTextSetString(fvalText,textstr);
    }

    /***** update error status *****/
    if (eparab_valid) {
        eparab_valid = False;
        for (iline=0; iline<npar; iline++) {
            UxParLineFormContext =
                (_UxCparLineForm *) UxGetContext(parLines[iline]);
            utl_markTextInvalid(parEparabText);
        }
        utl_markTextInvalid(fminText);
        utl_markTextInvalid(fedmText);
    }
    if (globcc_valid) {
        globcc_valid = False;
        for (iline=0; iline<npar; iline++) {
            UxParLineFormContext =
                (_UxCparLineForm *) UxGetContext(parLines[iline]);
            utl_markTextInvalid(parGlobccText);
        }
    }
    if (eminos_valid) {
        eminos_valid = False;
        for (iline=0; iline<npar; iline++) {
            UxParLineFormContext =
                (_UxCparLineForm *) UxGetContext(parLines[iline]);
            utl_markTextInvalid(parEminusText);
            utl_markTextInvalid(parEplusText);
        }
    }
}

/***********************************************************************
 Set values of all the internal data associated with a histogram.
 Write number of bins and the histogram range on the panel.
***********************************************************************/
void fp_hsetdata(int hsid,void *mctxt)
{
    char tstr[50];
    float xmi, xma, ymi, yma;

    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

        /***** get histogram parameters *****/
    chgive(hsid, &histonx, &xmi, &xma, &histony, &ymi, &yma);
    if (histony == 0) histodim = 1;
    else histodim = 2;

        /***** write number of bins *****/
    if (histodim==1) sprintf(tstr,"(%d)",histonx);
    else sprintf(tstr,"(%d,%d)",histonx,histony);
    XtVaSetValues(nbinLabel,
        RES_CONVERT(XmNlabelString,tstr),
        XmNheight, hNameText, NULL);

        /***** histo range field *****/
    F77_CALLS_IIII(fphgetrng,
        &lowbins[0], &uppbins[0], &lowbins[1], &uppbins[1]);
    histoRangeWrite(rangeText);
}

/***********************************************************************
 Find out what are the current fit options set inside PAW and display
 them on the panel (Histo Fit Panel).
***********************************************************************/
void fp_hsetoptn(void *mctxt)
{
    int ioptd,ioptw;
    char tstr[10];
    char *ptr = tstr;

    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

    F77_CALLS_I(fpgetoptd,&ioptd);
    F77_CALLS_I(fpgetoptw,&ioptw);
    if (ioptd) *ptr++ = 'D';
    if (ioptw) *ptr++ = 'W';
    *ptr = '\0';
    textWriteName(optnText,&optnStat,tstr);
}

/***********************************************************************
 Find out what are the current fit options set inside PAW and display
 them on the panel (Vector Fit Panel).
***********************************************************************/
void fp_vsetoptn(void *mctxt)
{
    int ioptd;
    char tstr[10];
    char *ptr = tstr;

    UxMainPanelShellContext = (_UxCmainPanelShell *)mctxt;

    F77_CALLS_I(fpgetoptd,&ioptd);
    if (ioptd) *ptr++ = 'D';
    *ptr = '\0';
    textWriteName(optnText,&optnStat,tstr);
}
