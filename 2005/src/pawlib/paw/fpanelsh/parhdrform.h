/*
 * $Id: parhdrform.h,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
 *
 * $Log: parhdrform.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 08/04/95  23.39.23  by  Gregory Kozlovsky*/
/*-- Author :    Gregory Kozlovsky   08/04/95*/


/*******************************************************************************
       parhdrform.h
       This header file is included by parhdrform.c

*******************************************************************************/

#ifndef _PARHDRFORM_INCLUDED
#define _PARHDRFORM_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/SeparatoG.h>
#include <Xm/Label.h>
#include <Xm/Form.h>

/*******************************************************************************
       The definition of the context structure:
       If you create multiple copies of your interface, the context
       structure ensures that your callbacks use the variables for the
       correct copy.

       For each swidget in the interface, each argument to the Interface
       function, and each variable in the Interface Specific section of the
       Declarations Editor, there is an entry in the context structure.
       and a #define.  The #define makes the variable name refer to the
       corresponding entry in the context structure.
*******************************************************************************/

typedef struct
{
        Widget  UxparHeaderForm;
        Widget  UxparHeaderRightForm;
        Widget  UxparValueLabel;
        Widget  UxparFixLabel;
        Widget  UxparLowLimitLabel;
        Widget  UxparUppLimitLabel;
        Widget  UxparStepLabel;
        Widget  UxparEparabLabel;
        Widget  UxparEminusLabel;
        Widget  UxparEplusLabel;
        Widget  UxparGlobccLabel;
        Widget  UxparHeaderLeftForm;
        Widget  UxparNumberLabel;
        Widget  UxparNameLabel;
        Widget  UxparTopSeparator;
        swidget UxUxParent;
} _UxCparHeaderForm;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCparHeaderForm       *UxParHeaderFormContext;
#define parHeaderForm           UxParHeaderFormContext->UxparHeaderForm
#define parHeaderRightForm      UxParHeaderFormContext->UxparHeaderRightForm
#define parValueLabel           UxParHeaderFormContext->UxparValueLabel
#define parFixLabel             UxParHeaderFormContext->UxparFixLabel
#define parLowLimitLabel        UxParHeaderFormContext->UxparLowLimitLabel
#define parUppLimitLabel        UxParHeaderFormContext->UxparUppLimitLabel
#define parStepLabel            UxParHeaderFormContext->UxparStepLabel
#define parEparabLabel          UxParHeaderFormContext->UxparEparabLabel
#define parEminusLabel          UxParHeaderFormContext->UxparEminusLabel
#define parEplusLabel           UxParHeaderFormContext->UxparEplusLabel
#define parGlobccLabel          UxParHeaderFormContext->UxparGlobccLabel
#define parHeaderLeftForm       UxParHeaderFormContext->UxparHeaderLeftForm
#define parNumberLabel          UxParHeaderFormContext->UxparNumberLabel
#define parNameLabel            UxParHeaderFormContext->UxparNameLabel
#define parTopSeparator         UxParHeaderFormContext->UxparTopSeparator
#define UxParent                UxParHeaderFormContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_parHeaderForm( swidget _UxUxParent );

#endif  /* _PARHDRFORM_INCLUDED */
