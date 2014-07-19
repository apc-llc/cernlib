/*
 * $Id: vectpanel.h,v 1.1.1.1 1996/03/01 11:39:10 mclareni Exp $
 *
 * $Log: vectpanel.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 08/04/95  23.41.21  by  Gregory Kozlovsky*/
/*-- Author :    Gregory Kozlovsky   08/04/95*/


/*******************************************************************************
       vectpanel.h
       This header file is included by vectpanel.c

*******************************************************************************/

#ifndef _VECTPANEL_INCLUDED
#define _VECTPANEL_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/SeparatoG.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <X11/Shell.h>

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
        Widget  UxvectPanelShell;
        Widget  UxvectForm;
        Widget  UxvectSaveButton;
        Widget  UxvectParLabel;
        Widget  UxvectParText;
        Widget  UxvectStepLabel;
        Widget  UxvectStepText;
        Widget  UxvectLowLabel;
        Widget  UxvectLowText;
        Widget  UxvectUppLabel;
        Widget  UxvectUppText;
        Widget  UxvectErrorLabel;
        Widget  UxvectErrorText;
        Widget  UxvectCmdSeparator;
        Widget  UxvectCloseButton;
        Widget  UxvectHelpButton;
        void    *Uxmainctxt;
        TextStatusInfo  UxvparStat;
        TextStatusInfo  UxvstepStat;
        TextStatusInfo  UxvlowStat;
        TextStatusInfo  UxvuppStat;
        TextStatusInfo  UxveparabStat;
        swidget UxUxParent;
} _UxCvectPanelShell;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCvectPanelShell      *UxVectPanelShellContext;
#define vectPanelShell          UxVectPanelShellContext->UxvectPanelShell
#define vectForm                UxVectPanelShellContext->UxvectForm
#define vectSaveButton          UxVectPanelShellContext->UxvectSaveButton
#define vectParLabel            UxVectPanelShellContext->UxvectParLabel
#define vectParText             UxVectPanelShellContext->UxvectParText
#define vectStepLabel           UxVectPanelShellContext->UxvectStepLabel
#define vectStepText            UxVectPanelShellContext->UxvectStepText
#define vectLowLabel            UxVectPanelShellContext->UxvectLowLabel
#define vectLowText             UxVectPanelShellContext->UxvectLowText
#define vectUppLabel            UxVectPanelShellContext->UxvectUppLabel
#define vectUppText             UxVectPanelShellContext->UxvectUppText
#define vectErrorLabel          UxVectPanelShellContext->UxvectErrorLabel
#define vectErrorText           UxVectPanelShellContext->UxvectErrorText
#define vectCmdSeparator        UxVectPanelShellContext->UxvectCmdSeparator
#define vectCloseButton         UxVectPanelShellContext->UxvectCloseButton
#define vectHelpButton          UxVectPanelShellContext->UxvectHelpButton
#define mainctxt                UxVectPanelShellContext->Uxmainctxt
#define vparStat                UxVectPanelShellContext->UxvparStat
#define vstepStat               UxVectPanelShellContext->UxvstepStat
#define vlowStat                UxVectPanelShellContext->UxvlowStat
#define vuppStat                UxVectPanelShellContext->UxvuppStat
#define veparabStat             UxVectPanelShellContext->UxveparabStat
#define UxParent                UxVectPanelShellContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_vectPanel( swidget _UxUxParent );

#endif  /* _VECTPANEL_INCLUDED */
