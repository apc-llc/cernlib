/*
 * $Id: contpanel.h,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
 *
 * $Log: contpanel.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 08/04/95  23.33.21  by  Gregory Kozlovsky*/
/*-- Author :    Gregory Kozlovsky   08/04/95*/


/*******************************************************************************
       contpanel.h
       This header file is included by contpanel.c

*******************************************************************************/

#ifndef _CONTPANEL_INCLUDED
#define _CONTPANEL_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/SeparatoG.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/ToggleB.h>
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
        Widget  UxcontourPanelShell;
        Widget  UxcontourForm;
        Widget  UxcontComputeButton;
        Widget  UxcontOverlayToggle;
        Widget  UxcontNum1Label;
        Widget  UxcontNum1Text;
        Widget  UxcontNum2Label;
        Widget  UxcontNum2Text;
        Widget  UxcontNptLabel;
        Widget  UxcontNptText;
        Widget  UxcontNfoundLabel;
        Widget  UxcontNfoundText;
        Widget  UxcontSeparator;
        Widget  UxcontSaveButton;
        Widget  UxcontXsaveLabel;
        Widget  UxcontXsaveText;
        Widget  UxcontYsaveLabel;
        Widget  UxcontYsaveText;
        Widget  UxcontCmdSeparator;
        Widget  UxcontCloseButton;
        Widget  UxcontHelpButton;
        void    *Uxmainctxt;
        TextStatusInfo  Uxcont_num1Stat;
        TextStatusInfo  Uxcont_num2Stat;
        TextStatusInfo  Uxcont_nptStat;
        TextStatusInfo  Uxcont_xSaveStat;
        TextStatusInfo  Uxcont_ySaveStat;
        swidget UxUxParent;
} _UxCcontourPanelShell;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCcontourPanelShell   *UxContourPanelShellContext;
#define contourPanelShell       UxContourPanelShellContext->UxcontourPanelShell
#define contourForm             UxContourPanelShellContext->UxcontourForm
#define contComputeButton       UxContourPanelShellContext->UxcontComputeButton
#define contOverlayToggle       UxContourPanelShellContext->UxcontOverlayToggle
#define contNum1Label           UxContourPanelShellContext->UxcontNum1Label
#define contNum1Text            UxContourPanelShellContext->UxcontNum1Text
#define contNum2Label           UxContourPanelShellContext->UxcontNum2Label
#define contNum2Text            UxContourPanelShellContext->UxcontNum2Text
#define contNptLabel            UxContourPanelShellContext->UxcontNptLabel
#define contNptText             UxContourPanelShellContext->UxcontNptText
#define contNfoundLabel         UxContourPanelShellContext->UxcontNfoundLabel
#define contNfoundText          UxContourPanelShellContext->UxcontNfoundText
#define contSeparator           UxContourPanelShellContext->UxcontSeparator
#define contSaveButton          UxContourPanelShellContext->UxcontSaveButton
#define contXsaveLabel          UxContourPanelShellContext->UxcontXsaveLabel
#define contXsaveText           UxContourPanelShellContext->UxcontXsaveText
#define contYsaveLabel          UxContourPanelShellContext->UxcontYsaveLabel
#define contYsaveText           UxContourPanelShellContext->UxcontYsaveText
#define contCmdSeparator        UxContourPanelShellContext->UxcontCmdSeparator
#define contCloseButton         UxContourPanelShellContext->UxcontCloseButton
#define contHelpButton          UxContourPanelShellContext->UxcontHelpButton
#define mainctxt                UxContourPanelShellContext->Uxmainctxt
#define cont_num1Stat           UxContourPanelShellContext->Uxcont_num1Stat
#define cont_num2Stat           UxContourPanelShellContext->Uxcont_num2Stat
#define cont_nptStat            UxContourPanelShellContext->Uxcont_nptStat
#define cont_xSaveStat          UxContourPanelShellContext->Uxcont_xSaveStat
#define cont_ySaveStat          UxContourPanelShellContext->Uxcont_ySaveStat
#define UxParent                UxContourPanelShellContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_contourPanel( swidget _UxUxParent );

#endif  /* _CONTPANEL_INCLUDED */
