/*
 * $Id: uxpan.h,v 1.2 1996/04/11 10:04:25 cernlib Exp $
 *
 * $Log: uxpan.h,v $
 * Revision 1.2  1996/04/11 10:04:25  cernlib
 * Remobe fortran comment lines
 *
 * Revision 1.1.1.1  1996/03/08 15:33:05  mclareni
 * Kuip
 *
 */
/*******************************************************************************
       kuipPanel.h
       This header file is included by kuipPanel.c

*******************************************************************************/

#ifndef        _KUIPPANEL_INCLUDED
#define        _KUIPPANEL_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "uxxt.h"

#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/SeparatoG.h>
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

typedef        struct
{
        Widget        UxkuipPanel;
        Widget        UxseparatorPanel;
        Widget        Uxkm_scrollPNW;
        Widget        Uxkm_rowcolPNW;
        Widget        Uxkm_okPNB;
        Widget        Uxkm_savePNB;
        Widget        Uxkm_helpPNB;
} _UxCkuipPanel;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCkuipPanel           *UxKuipPanelContext;
#define kuipPanel               UxKuipPanelContext->UxkuipPanel
#define separatorPanel          UxKuipPanelContext->UxseparatorPanel
#define km_scrollPNW            UxKuipPanelContext->Uxkm_scrollPNW
#define km_rowcolPNW            UxKuipPanelContext->Uxkm_rowcolPNW
#define km_okPNB                UxKuipPanelContext->Uxkm_okPNB
#define km_savePNB              UxKuipPanelContext->Uxkm_savePNB
#define km_helpPNB              UxKuipPanelContext->Uxkm_helpPNB

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget        create_kuipPanel();

#endif        /* _KUIPPANEL_INCLUDED */
