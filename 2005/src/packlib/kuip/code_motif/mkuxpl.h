/*
 * $Id: mkuxpl.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkuxpl.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */

/*******************************************************************************
       kuipPalette.h
       This header file is included by kuipPalette.c

*******************************************************************************/

#ifndef       _KUIPPALETTE_INCLUDED
#define       _KUIPPALETTE_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/CascadeB.h>
#include <Xm/ToggleBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
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
       Widget       UxkuipPalette;
       Widget       UxkuipPaletteFrame;
       Widget       UxkuipPaletteForm;
       Widget       UxPalScWindow;
       Widget       Uxkm_palForm;
       Widget       UxPaletteMenu;
       Widget       UxFilePdMenu;
       Widget       Uxkm_addPAL;
       Widget       UxFilePdMenu_b6;
       Widget       Uxkm_closePAL;
       Widget       UxFilePdMenu_b5;
       Widget       Uxkm_exitPAL;
       Widget       UxFileMenu;
       Widget       UxViewPdMenu;
       Widget       Uxkm_viewPAL_icon;
       Widget       Uxkm_viewPAL_name;
       Widget       Uxkm_viewPAL_both;
       Widget       UxViewMenu;
       Widget       UxHelpPdMenu;
       Widget       Uxkm_helpPAL;
       Widget       UxHelpMenu;
} _UxCkuipPalette;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCkuipPalette         *UxKuipPaletteContext;
#define kuipPalette             UxKuipPaletteContext->UxkuipPalette
#define kuipPaletteFrame        UxKuipPaletteContext->UxkuipPaletteFrame
#define kuipPaletteForm         UxKuipPaletteContext->UxkuipPaletteForm
#define PalScWindow             UxKuipPaletteContext->UxPalScWindow
#define km_palForm              UxKuipPaletteContext->Uxkm_palForm
#define PaletteMenu             UxKuipPaletteContext->UxPaletteMenu
#define FilePdMenu              UxKuipPaletteContext->UxFilePdMenu
#define km_addPAL               UxKuipPaletteContext->Uxkm_addPAL
#define FilePdMenu_b6           UxKuipPaletteContext->UxFilePdMenu_b6
#define km_closePAL             UxKuipPaletteContext->Uxkm_closePAL
#define FilePdMenu_b5           UxKuipPaletteContext->UxFilePdMenu_b5
#define km_exitPAL              UxKuipPaletteContext->Uxkm_exitPAL
#define FileMenu                UxKuipPaletteContext->UxFileMenu
#define ViewPdMenu              UxKuipPaletteContext->UxViewPdMenu
#define km_viewPAL_icon         UxKuipPaletteContext->Uxkm_viewPAL_icon
#define km_viewPAL_name         UxKuipPaletteContext->Uxkm_viewPAL_name
#define km_viewPAL_both         UxKuipPaletteContext->Uxkm_viewPAL_both
#define ViewMenu                UxKuipPaletteContext->UxViewMenu
#define HelpPdMenu              UxKuipPaletteContext->UxHelpPdMenu
#define km_helpPAL              UxKuipPaletteContext->Uxkm_helpPAL
#define HelpMenu                UxKuipPaletteContext->UxHelpMenu

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget       create_kuipPalette();

#endif       /* _KUIPPALETTE_INCLUDED */
