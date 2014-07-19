/*
 * $Id: fpanels.h,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
 *
 * $Log: fpanels.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/03 21/06/95  15.52.41  by  Yana Gaponenko*/
/*-- Author :    Gregory Kozlovsky   08/04/95*/

/*******************************************************************************
 Outside callable routines of Histogram Fit and Vector Fit Panel.
 =====================> Author: G. Kozlovsky, 1994 <===========================
 ******************************************************************************/
#ifdef __cplusplus
extern "C" {
#endif

/***********************************************************************
        Constants
 **********************************************************************/

#define MAX_PARNAMELENGTH 12 /* maximum length for parameter name */
#define MAX_HISTODIM 3       /* max histogram dimension forseen */
#define MAX_MINUIT_NAMELEN 10    /* max name length in minuit */
#define MAX_PARNUMBER 35     /* max number of minuit parameters */
#define MAX_PAWVECNAME 30    /* max length of paw vector name */

/****** delays for autorepeat when a button is pressed *****/
#define AUTOREPEAT_INITDELAY 300
#define AUTOREPEAT_DELAY 100

/***********************************************************************
 Externals
***********************************************************************/
extern void *fitPanelContext;  /* only one copy of the panel per run */

/***********************************************************************
 Function prototypes
***********************************************************************/

void *createHistoFitPanel(); /* Create Histo Fit panel user interface */
void *createVectorFitPanel(); /* Create Vector Fit panel user interface */
    /* Create nlines parameter lines starting from line istart */
void createParLines(void *mctxt, int istart, int nlines);

/* Display given histogram id in the Histogram text field on the panel */
int fp_hsetid(int ihid, void *mctxt);

/* Create and start histo fit panel invoked through PAW++ browser */
void fphbrspanel();

/*  Create and start histo fit panel invoked by PAW command histo/fit */
#ifndef vms
    void fphcmndpanel_(
    int *_Uxhistid, char *_Uxfexpr,
    void *_Uxfcn, void *_Uxufcn,
    int *_Uxnpar, double *_Uxvpar,
    double *_Uxvlow, double *_Uxvupp,
    double *_Uxvstep, double *_Uxveparab, int felen);
#else /* vms */
#include <descrip.h>
    void fphcmndpanel(
    int *_Uxhistid,
    struct dsc$descriptor_s *dsc_Uxfexpr,
    void *_Uxfcn, void *_Uxufcn,
    int *_Uxnpar, double *_Uxvpar,
    double *_Uxvlow, double *_Uxvupp,
    double *_Uxvstep, double *_Uxveparab);
#endif

/*  Create and start vector fit panel invoked by PAW command vector/fit */
#ifndef vms
    void fpvcmndpanel_(
    char *_Uxfexpr,
    void *_Uxfcn, void *_Uxufcn,
    int *_Uxnpar, double *_Uxvpar,
    double *_Uxvlow, double *_Uxvupp,
    double *_Uxvstep, double *_Uxveparab, int felen);
#else /* vms */
#include <descrip.h>
    void fpvcmndpanel(
    struct dsc$descriptor_s *dsc_Uxfexpr,
    void *_Uxfcn, void *_Uxufcn,
    int *_Uxnpar, double *_Uxvpar,
    double *_Uxvlow, double *_Uxvupp,
    double *_Uxvstep, double *_Uxveparab);
#endif

#ifdef __cplusplus
}  /* extern "C" */
#endif

