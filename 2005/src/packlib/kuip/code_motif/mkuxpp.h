/*
 * $Id: mkuxpp.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkuxpp.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */

/*******************************************************************************
       PalPanel.h
       This header file is included by PalPanel.c

*******************************************************************************/

#ifndef       _PALPANEL_INCLUDED
#define       _PALPANEL_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/Label.h>
#include <Xm/ArrowB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Frame.h>
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

typedef       struct
{
       Widget       UxPalPanel;
       Widget       UxPNPframe1;
       Widget       UxPNPform1;
       Widget       Uxkm_rowcolMI;
       Widget       Uxkm_scrollMI;
       Widget       Uxkm_rowcolMN;
       Widget       Uxkm_scrollMN;
       Widget       Uxkm_rowcolMB;
       Widget       Uxkm_scrollMB;
       Widget       Uxkm_rowcolMC;
       Widget       Uxkm_scrollMC;
       Widget       Uxkm_rowcolM1;
       Widget       Uxkm_scrollM1;
       Widget       UxarrowButtonP;
       Widget       Uxkm_palLabelP;
       swidget       UxUxParent;
} _UxCPalPanel;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCPalPanel            *UxPalPanelContext;
#define PalPanel                UxPalPanelContext->UxPalPanel
#define PNPframe1               UxPalPanelContext->UxPNPframe1
#define PNPform1                UxPalPanelContext->UxPNPform1
#define km_rowcolMI             UxPalPanelContext->Uxkm_rowcolMI
#define km_scrollMI             UxPalPanelContext->Uxkm_scrollMI
#define km_rowcolMN             UxPalPanelContext->Uxkm_rowcolMN
#define km_scrollMN             UxPalPanelContext->Uxkm_scrollMN
#define km_rowcolMB             UxPalPanelContext->Uxkm_rowcolMB
#define km_scrollMB             UxPalPanelContext->Uxkm_scrollMB
#define km_rowcolMC             UxPalPanelContext->Uxkm_rowcolMC
#define km_scrollMC             UxPalPanelContext->Uxkm_scrollMC
#define km_rowcolM1             UxPalPanelContext->Uxkm_rowcolM1
#define km_scrollM1             UxPalPanelContext->Uxkm_scrollM1
#define arrowButtonP            UxPalPanelContext->UxarrowButtonP
#define km_palLabelP            UxPalPanelContext->Uxkm_palLabelP
#define UxParent                UxPalPanelContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget       create_PalPanel();

#endif       /* _PALPANEL_INCLUDED */
