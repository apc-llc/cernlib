/*
 * $Id: kuxpal.c,v 1.1.1.1 1996/03/08 15:33:10 mclareni Exp $
 *
 * $Log: kuxpal.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:10  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/17 19/09/94  10.18.28  by  N.Cremel*/
/*-- Author :    N.Cremel   13/08/92*/
/*
 * kuipPalette.c
 */

/*******************************************************************************
       kuipPalette.c

       Associated Header file: kuipPalette.h
*******************************************************************************/

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
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "kuipPalette.h" */
#include "mkuxpl.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget       _Uxbuild_kuipPalette()
{
       Widget              _UxParent;
       Widget              FilePdMenu_shell;
       Widget              ViewPdMenu_shell;
       Widget              HelpPdMenu_shell;


       /* Creation of kuipPalette */
       _UxParent = XtVaCreatePopupShell( "kuipPalette_shell",
                     topLevelShellWidgetClass, UxTopLevel,
                     XmNtitle, "kuipPalette",
                     XmNiconName, "kuipPalette",
                     NULL );

       kuipPalette = XtVaCreateManagedWidget( "kuipPalette",
                     xmMainWindowWidgetClass,
                     _UxParent,
                     XmNmappedWhenManaged, TRUE,
                     NULL );
       UxPutContext( kuipPalette, (char *) UxKuipPaletteContext );


       /* Creation of kuipPaletteFrame */
       kuipPaletteFrame = XtVaCreateManagedWidget( "kuipPaletteFrame",
                     xmFrameWidgetClass,
                     kuipPalette,
                     NULL );
       UxPutContext( kuipPaletteFrame, (char *) UxKuipPaletteContext );


       /* Creation of kuipPaletteForm */
       kuipPaletteForm = XtVaCreateManagedWidget( "kuipPaletteForm",
                     xmFormWidgetClass,
                     kuipPaletteFrame,
                     XmNautoUnmanage, FALSE,
                     NULL );
       UxPutContext( kuipPaletteForm, (char *) UxKuipPaletteContext );


       /* Creation of PalScWindow */
       PalScWindow = XtVaCreateManagedWidget( "PalScWindow",
                     xmScrolledWindowWidgetClass,
                     kuipPaletteForm,
                     XmNshadowThickness, 2,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNbottomOffset, 5,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 5,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 5,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNtopOffset, 5,
                     XmNscrollBarDisplayPolicy, XmSTATIC,
                     XmNscrollingPolicy, XmAUTOMATIC,
                     NULL );
       UxPutContext( PalScWindow, (char *) UxKuipPaletteContext );


       /* Creation of km_palForm */
       km_palForm = XtVaCreateManagedWidget( "km_palForm",
                     xmFormWidgetClass,
                     PalScWindow,
                     XmNautoUnmanage, FALSE,
                     NULL );
       UxPutContext( km_palForm, (char *) UxKuipPaletteContext );


       /* Creation of PaletteMenu */
       PaletteMenu = XtVaCreateManagedWidget( "PaletteMenu",
                     xmRowColumnWidgetClass,
                     kuipPalette,
                     XmNrowColumnType, XmMENU_BAR,
                     XmNmenuAccelerator, "<KeyUp>F10",
                     NULL );
       UxPutContext( PaletteMenu, (char *) UxKuipPaletteContext );


       /* Creation of FilePdMenu */
       FilePdMenu_shell = XtVaCreatePopupShell ("FilePdMenu_shell",
                     xmMenuShellWidgetClass, PaletteMenu,
                     XmNwidth, 1,
                     XmNheight, 1,
                     XmNallowShellResize, TRUE,
                     XmNoverrideRedirect, TRUE,
                     NULL );

       FilePdMenu = XtVaCreateWidget( "FilePdMenu",
                     xmRowColumnWidgetClass,
                     FilePdMenu_shell,
                     XmNrowColumnType, XmMENU_PULLDOWN,
                     NULL );
       UxPutContext( FilePdMenu, (char *) UxKuipPaletteContext );


       /* Creation of km_addPAL */
       km_addPAL = XtVaCreateManagedWidget( "km_addPAL",
                     xmPushButtonGadgetClass,
                     FilePdMenu,
                     RES_CONVERT( XmNlabelString, "Add Panel ..." ),
                     RES_CONVERT( XmNmnemonic, "A" ),
                     NULL );
       UxPutContext( km_addPAL, (char *) UxKuipPaletteContext );


       /* Creation of FilePdMenu_b6 */
       FilePdMenu_b6 = XtVaCreateManagedWidget( "FilePdMenu_b6",
                     xmSeparatorGadgetClass,
                     FilePdMenu,
                     NULL );
       UxPutContext( FilePdMenu_b6, (char *) UxKuipPaletteContext );


       /* Creation of km_closePAL */
       km_closePAL = XtVaCreateManagedWidget( "km_closePAL",
                     xmPushButtonGadgetClass,
                     FilePdMenu,
                     RES_CONVERT( XmNlabelString, "Close" ),
                     RES_CONVERT( XmNmnemonic, "C" ),
                     NULL );
       UxPutContext( km_closePAL, (char *) UxKuipPaletteContext );


       /* Creation of FilePdMenu_b5 */
       FilePdMenu_b5 = XtVaCreateManagedWidget( "FilePdMenu_b5",
                     xmSeparatorGadgetClass,
                     FilePdMenu,
                     NULL );
       UxPutContext( FilePdMenu_b5, (char *) UxKuipPaletteContext );


       /* Creation of km_exitPAL */
       km_exitPAL = XtVaCreateManagedWidget( "km_exitPAL",
                     xmPushButtonGadgetClass,
                     FilePdMenu,
                     RES_CONVERT( XmNlabelString, "Exit" ),
                     RES_CONVERT( XmNmnemonic, "E" ),
                     NULL );
       UxPutContext( km_exitPAL, (char *) UxKuipPaletteContext );


       /* Creation of FileMenu */
       FileMenu = XtVaCreateManagedWidget( "FileMenu",
                     xmCascadeButtonWidgetClass,
                     PaletteMenu,
                     RES_CONVERT( XmNlabelString, "File" ),
                     RES_CONVERT( XmNmnemonic, "F" ),
                     XmNsubMenuId, FilePdMenu,
                     NULL );
       UxPutContext( FileMenu, (char *) UxKuipPaletteContext );


       /* Creation of ViewPdMenu */
       ViewPdMenu_shell = XtVaCreatePopupShell ("ViewPdMenu_shell",
                     xmMenuShellWidgetClass, PaletteMenu,
                     XmNwidth, 1,
                     XmNheight, 1,
                     XmNallowShellResize, TRUE,
                     XmNoverrideRedirect, TRUE,
                     NULL );

       ViewPdMenu = XtVaCreateWidget( "ViewPdMenu",
                     xmRowColumnWidgetClass,
                     ViewPdMenu_shell,
                     XmNradioBehavior, TRUE,
                     XmNrowColumnType, XmMENU_PULLDOWN,
                     NULL );
       UxPutContext( ViewPdMenu, (char *) UxKuipPaletteContext );


       /* Creation of km_viewPAL_icon */
       km_viewPAL_icon = XtVaCreateManagedWidget( "km_viewPAL_icon",
                     xmToggleButtonGadgetClass,
                     ViewPdMenu,
                     RES_CONVERT( XmNlabelString, "By Icon" ),
                     XmNset, TRUE,
                     NULL );
       UxPutContext( km_viewPAL_icon, (char *) UxKuipPaletteContext );


       /* Creation of km_viewPAL_name */
       km_viewPAL_name = XtVaCreateManagedWidget( "km_viewPAL_name",
                     xmToggleButtonGadgetClass,
                     ViewPdMenu,
                     RES_CONVERT( XmNlabelString, "By Name" ),
                     NULL );
       UxPutContext( km_viewPAL_name, (char *) UxKuipPaletteContext );


       /* Creation of km_viewPAL_both */
       km_viewPAL_both = XtVaCreateManagedWidget( "km_viewPAL_both",
                     xmToggleButtonGadgetClass,
                     ViewPdMenu,
                     RES_CONVERT( XmNlabelString, "By Name and Icon" ),
                     NULL );
       UxPutContext( km_viewPAL_both, (char *) UxKuipPaletteContext );


       /* Creation of ViewMenu */
       ViewMenu = XtVaCreateManagedWidget( "ViewMenu",
                     xmCascadeButtonWidgetClass,
                     PaletteMenu,
                     RES_CONVERT( XmNlabelString, "View" ),
                     RES_CONVERT( XmNmnemonic, "V" ),
                     XmNsubMenuId, ViewPdMenu,
                     NULL );
       UxPutContext( ViewMenu, (char *) UxKuipPaletteContext );


       /* Creation of HelpPdMenu */
       HelpPdMenu_shell = XtVaCreatePopupShell ("HelpPdMenu_shell",
                     xmMenuShellWidgetClass, PaletteMenu,
                     XmNwidth, 1,
                     XmNheight, 1,
                     XmNallowShellResize, TRUE,
                     XmNoverrideRedirect, TRUE,
                     NULL );

       HelpPdMenu = XtVaCreateWidget( "HelpPdMenu",
                     xmRowColumnWidgetClass,
                     HelpPdMenu_shell,
                     XmNrowColumnType, XmMENU_PULLDOWN,
                     NULL );
       UxPutContext( HelpPdMenu, (char *) UxKuipPaletteContext );


       /* Creation of km_helpPAL */
       km_helpPAL = XtVaCreateManagedWidget( "km_helpPAL",
                     xmPushButtonGadgetClass,
                     HelpPdMenu,
                     RES_CONVERT( XmNlabelString, "Help..." ),
                     NULL );
       UxPutContext( km_helpPAL, (char *) UxKuipPaletteContext );


       /* Creation of HelpMenu */
       HelpMenu = XtVaCreateManagedWidget( "HelpMenu",
                     xmCascadeButtonWidgetClass,
                     PaletteMenu,
                     RES_CONVERT( XmNlabelString, "Help" ),
                     RES_CONVERT( XmNmnemonic, "H" ),
                     XmNsubMenuId, HelpPdMenu,
                     NULL );
       UxPutContext( HelpMenu, (char *) UxKuipPaletteContext );

       XtVaSetValues(PaletteMenu,
                     XmNmenuHelpWidget, HelpMenu,
                     NULL );


       XtAddCallback( kuipPalette, XmNdestroyCallback,
              (XtCallbackProc) UxDestroyContextCB,
              (XtPointer) UxKuipPaletteContext);

       XmMainWindowSetAreas( kuipPalette, PaletteMenu, (Widget) NULL,
                     (Widget) NULL, (Widget) NULL, kuipPaletteFrame );

       return ( kuipPalette );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget       create_kuipPalette()
{
       Widget                  rtrn;
       _UxCkuipPalette         *UxContext;

       UxKuipPaletteContext = UxContext =
            (_UxCkuipPalette *) UxNewContext( sizeof(_UxCkuipPalette), False );


       rtrn = _Uxbuild_kuipPalette();

       return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

