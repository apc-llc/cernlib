/*
 * $Id: mkuxpa.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkuxpa.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */

/*******************************************************************************
       kuipPanel.h
       This header file is included by kuipPanel.c

*******************************************************************************/

#ifndef       _KUIPPANEL_INCLUDED
#define       _KUIPPANEL_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/CascadeB.h>
#include <Xm/Separator.h>
#include <Xm/ToggleBG.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/MainW.h>

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

typedef       struct
{
       Widget       UxkuipPanel;
       Widget       UxkuipPanelFrame;
       Widget       UxkuipPanelForm;
       Widget       Uxkm_scrollPI;
       Widget       Uxkm_rowcolPI;
       Widget       Uxkm_scrollPN;
       Widget       Uxkm_rowcolPN;
       Widget       Uxkm_scrollPB;
       Widget       Uxkm_rowcolPB;
       Widget       Uxkm_scrollPC;
       Widget       Uxkm_rowcolPC;
       Widget       Uxkm_scrollP1;
       Widget       Uxkm_rowcolP1;
       Widget       UxkuipPanelMenu;
       Widget       UxFilePdMenPN;
       Widget       Uxkm_savePNB;
       Widget       Uxkm_okPNB;
       Widget       UxFileMenPN;
       Widget       UxViewPdMenPN;
       Widget       Uxkm_viewPNB_name;
       Widget       Uxkm_viewPNB_icon;
       Widget       Uxkm_viewPNB_both;
       Widget       Uxkm_viewPNB_sep;
       Widget       Uxkm_viewPNB_command;
       Widget       Uxkm_viewPNB_command1;
       Widget       UxViewMenPN;
       Widget       UxHelpPdMenPN;
       Widget       Uxkm_helpPNB1;
       Widget       Uxkm_helpPNB2;
       Widget       Uxkm_helpPNB3;
       Widget       Uxkm_helpPNB4;
       Widget       Uxkm_helpPNB5;
       Widget       Uxkm_helpPNB6;
       Widget       UxHelpPdMenPN_b7;
       Widget       Uxkm_helpCONTEXT;
       Widget       UxHelpMenPN;
} _UxCkuipPanel;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCkuipPanel           *UxKuipPanelContext;
#define kuipPanel               UxKuipPanelContext->UxkuipPanel
#define kuipPanelFrame          UxKuipPanelContext->UxkuipPanelFrame
#define kuipPanelForm           UxKuipPanelContext->UxkuipPanelForm
#define km_scrollPI             UxKuipPanelContext->Uxkm_scrollPI
#define km_rowcolPI             UxKuipPanelContext->Uxkm_rowcolPI
#define km_scrollPN             UxKuipPanelContext->Uxkm_scrollPN
#define km_rowcolPN             UxKuipPanelContext->Uxkm_rowcolPN
#define km_scrollPB             UxKuipPanelContext->Uxkm_scrollPB
#define km_rowcolPB             UxKuipPanelContext->Uxkm_rowcolPB
#define km_scrollPC             UxKuipPanelContext->Uxkm_scrollPC
#define km_rowcolPC             UxKuipPanelContext->Uxkm_rowcolPC
#define km_scrollP1             UxKuipPanelContext->Uxkm_scrollP1
#define km_rowcolP1             UxKuipPanelContext->Uxkm_rowcolP1
#define kuipPanelMenu           UxKuipPanelContext->UxkuipPanelMenu
#define FilePdMenPN             UxKuipPanelContext->UxFilePdMenPN
#define km_savePNB              UxKuipPanelContext->Uxkm_savePNB
#define km_okPNB                UxKuipPanelContext->Uxkm_okPNB
#define FileMenPN               UxKuipPanelContext->UxFileMenPN
#define ViewPdMenPN             UxKuipPanelContext->UxViewPdMenPN
#define km_viewPNB_name         UxKuipPanelContext->Uxkm_viewPNB_name
#define km_viewPNB_icon         UxKuipPanelContext->Uxkm_viewPNB_icon
#define km_viewPNB_both         UxKuipPanelContext->Uxkm_viewPNB_both
#define km_viewPNB_sep          UxKuipPanelContext->Uxkm_viewPNB_sep
#define km_viewPNB_command      UxKuipPanelContext->Uxkm_viewPNB_command
#define km_viewPNB_command1     UxKuipPanelContext->Uxkm_viewPNB_command1
#define ViewMenPN               UxKuipPanelContext->UxViewMenPN
#define HelpPdMenPN             UxKuipPanelContext->UxHelpPdMenPN
#define km_helpPNB1             UxKuipPanelContext->Uxkm_helpPNB1
#define km_helpPNB2             UxKuipPanelContext->Uxkm_helpPNB2
#define km_helpPNB3             UxKuipPanelContext->Uxkm_helpPNB3
#define km_helpPNB4             UxKuipPanelContext->Uxkm_helpPNB4
#define km_helpPNB5             UxKuipPanelContext->Uxkm_helpPNB5
#define km_helpPNB6             UxKuipPanelContext->Uxkm_helpPNB6
#define HelpPdMenPN_b7          UxKuipPanelContext->UxHelpPdMenPN_b7
#define km_helpCONTEXT          UxKuipPanelContext->Uxkm_helpCONTEXT
#define HelpMenPN               UxKuipPanelContext->UxHelpMenPN

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget       create_kuipPanel();

#endif       /* _KUIPPANEL_INCLUDED */
