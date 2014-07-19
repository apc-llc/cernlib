/*
 * $Id: scanpanel.h,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
 *
 * $Log: scanpanel.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 08/04/95  23.40.42  by  Gregory Kozlovsky*/
/*-- Author :    Gregory Kozlovsky   08/04/95*/


/*******************************************************************************
       scanpanel.h
       This header file is included by scanpanel.c

*******************************************************************************/

#ifndef _SCANPANEL_INCLUDED
#define _SCANPANEL_INCLUDED


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
        Widget  UxscanPanelShell;
        Widget  UxscanForm;
        Widget  UxscanComputeButton;
        Widget  UxscanNumLabel;
        Widget  UxscanNumText;
        Widget  UxscanNptLabel;
        Widget  UxscanNptText;
        Widget  UxscanFromLabel;
        Widget  UxscanFromText;
        Widget  UxscanToLabel;
        Widget  UxscanToText;
        Widget  UxscanCmdSeparator;
        Widget  UxscanCloseButton;
        Widget  UxscanHelpButton;
        void    *Uxmainctxt;
        TextStatusInfo  Uxscan_numStat;
        TextStatusInfo  Uxscan_nptStat;
        TextStatusInfo  Uxscan_xfromStat;
        TextStatusInfo  Uxscan_xtoStat;
        swidget UxUxParent;
} _UxCscanPanelShell;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCscanPanelShell      *UxScanPanelShellContext;
#define scanPanelShell          UxScanPanelShellContext->UxscanPanelShell
#define scanForm                UxScanPanelShellContext->UxscanForm
#define scanComputeButton       UxScanPanelShellContext->UxscanComputeButton
#define scanNumLabel            UxScanPanelShellContext->UxscanNumLabel
#define scanNumText             UxScanPanelShellContext->UxscanNumText
#define scanNptLabel            UxScanPanelShellContext->UxscanNptLabel
#define scanNptText             UxScanPanelShellContext->UxscanNptText
#define scanFromLabel           UxScanPanelShellContext->UxscanFromLabel
#define scanFromText            UxScanPanelShellContext->UxscanFromText
#define scanToLabel             UxScanPanelShellContext->UxscanToLabel
#define scanToText              UxScanPanelShellContext->UxscanToText
#define scanCmdSeparator        UxScanPanelShellContext->UxscanCmdSeparator
#define scanCloseButton         UxScanPanelShellContext->UxscanCloseButton
#define scanHelpButton          UxScanPanelShellContext->UxscanHelpButton
#define mainctxt                UxScanPanelShellContext->Uxmainctxt
#define scan_numStat            UxScanPanelShellContext->Uxscan_numStat
#define scan_nptStat            UxScanPanelShellContext->Uxscan_nptStat
#define scan_xfromStat          UxScanPanelShellContext->Uxscan_xfromStat
#define scan_xtoStat            UxScanPanelShellContext->Uxscan_xtoStat
#define UxParent                UxScanPanelShellContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_scanPanel( swidget _UxUxParent );

#endif  /* _SCANPANEL_INCLUDED */
