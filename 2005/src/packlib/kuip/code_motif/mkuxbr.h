/*
 * $Id: mkuxbr.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkuxbr.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */
/*******************************************************************************
       kuipBrowser.h
       This header file is included by kuipBrowser.c

*******************************************************************************/

#ifndef        _KUIPBROWSER_INCLUDED
#define        _KUIPBROWSER_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/CascadeB.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/SeparatoG.h>
#include <Xm/List.h>
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

typedef        struct
{
        Widget        UxkuipBrowser;
        Widget        UxkuipBrowserFrame;
        Widget        UxbrowserForm;
        Widget        UxfileScWindow;
        Widget        UxfileList;
        Widget        UxdirScWindow;
        Widget        UxtopSeparator1;
        Widget        UxbottomSeparator1;
        Widget        UxcloseButton;
        Widget        UxcloneButton;
        Widget        Uxform1;
        Widget        UxseparatorGadget1;
        Widget        Uxform2;
        Widget        UxpathLabel;
        Widget        UxpathText;
        Widget        Uxform3;
        Widget        UxcounterLabel;
        Widget        Uxform4;
        Widget        UxdirNameLabel;
        Widget        UxfileNameLabel;
        Widget        UxbrowserMenu;
        Widget        UxFilePdMenu;
        Widget        UxFileMenu;
        Widget        UxViewPdMenu;
        Widget        UxViewPdMenu_allcycle;
        Widget        Uxmenu1_p2_b4;
        Widget        UxViewPdMenu_icon;
        Widget        UxViewPdMenu_sicon;
        Widget        UxViewPdMenu_nicon;
        Widget        UxViewPdMenu_title;
        Widget        Uxmenu1_p2_b6;
        Widget        UxViewPdMenu_selall;
        Widget        Uxmenu1_p2_b8;
        Widget        UxViewPdMenu_filt;
        Widget        UxViewMenu;
        Widget        UxHelpPdMenu;
        Widget        UxHelpPdMenu_help;
        Widget        UxHelpMenu;
} _UxCkuipBrowser;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCkuipBrowser         *UxKuipBrowserContext;
#define kuipBrowser             UxKuipBrowserContext->UxkuipBrowser
#define kuipBrowserFrame        UxKuipBrowserContext->UxkuipBrowserFrame
#define browserForm             UxKuipBrowserContext->UxbrowserForm
#define fileScWindow            UxKuipBrowserContext->UxfileScWindow
#define fileList                UxKuipBrowserContext->UxfileList
#define dirScWindow             UxKuipBrowserContext->UxdirScWindow
#define topSeparator1           UxKuipBrowserContext->UxtopSeparator1
#define bottomSeparator1        UxKuipBrowserContext->UxbottomSeparator1
#define closeButton             UxKuipBrowserContext->UxcloseButton
#define cloneButton             UxKuipBrowserContext->UxcloneButton
#define form1                   UxKuipBrowserContext->Uxform1
#define separatorGadget1        UxKuipBrowserContext->UxseparatorGadget1
#define form2                   UxKuipBrowserContext->Uxform2
#define pathLabel               UxKuipBrowserContext->UxpathLabel
#define pathText                UxKuipBrowserContext->UxpathText
#define form3                   UxKuipBrowserContext->Uxform3
#define counterLabel            UxKuipBrowserContext->UxcounterLabel
#define form4                   UxKuipBrowserContext->Uxform4
#define dirNameLabel            UxKuipBrowserContext->UxdirNameLabel
#define fileNameLabel           UxKuipBrowserContext->UxfileNameLabel
#define browserMenu             UxKuipBrowserContext->UxbrowserMenu
#define FilePdMenu              UxKuipBrowserContext->UxFilePdMenu
#define FileMenu                UxKuipBrowserContext->UxFileMenu
#define ViewPdMenu              UxKuipBrowserContext->UxViewPdMenu
#define ViewPdMenu_allcycle     UxKuipBrowserContext->UxViewPdMenu_allcycle
#define menu1_p2_b4             UxKuipBrowserContext->Uxmenu1_p2_b4
#define ViewPdMenu_icon         UxKuipBrowserContext->UxViewPdMenu_icon
#define ViewPdMenu_sicon        UxKuipBrowserContext->UxViewPdMenu_sicon
#define ViewPdMenu_nicon        UxKuipBrowserContext->UxViewPdMenu_nicon
#define ViewPdMenu_title        UxKuipBrowserContext->UxViewPdMenu_title
#define menu1_p2_b6             UxKuipBrowserContext->Uxmenu1_p2_b6
#define ViewPdMenu_selall       UxKuipBrowserContext->UxViewPdMenu_selall
#define menu1_p2_b8             UxKuipBrowserContext->Uxmenu1_p2_b8
#define ViewPdMenu_filt         UxKuipBrowserContext->UxViewPdMenu_filt
#define ViewMenu                UxKuipBrowserContext->UxViewMenu
#define HelpPdMenu              UxKuipBrowserContext->UxHelpPdMenu
#define HelpPdMenu_help         UxKuipBrowserContext->UxHelpPdMenu_help
#define HelpMenu                UxKuipBrowserContext->UxHelpMenu

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget        create_kuipBrowser();

#endif        /* _KUIPBROWSER_INCLUDED */
