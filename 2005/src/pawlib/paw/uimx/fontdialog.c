/*
 * $Id: fontdialog.c,v 1.1.1.1 1996/03/01 11:39:33 mclareni Exp $
 *
 * $Log: fontdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:33  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.06  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        fontDialog.c
        (Generated from interface file fontDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>

/*******************************************************************************
        Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern swidget fontSettingsDialog;

/*******************************************************************************
        The definition of the context structure:
        If you create multiple instances of your interface, the context
        structure ensures that your callbacks use the variables for the
        correct instance.

        For each swidget in the interface, each argument to the Interface
        function, and each variable in the Instance Specific section of the
        Declarations Editor, there is an entry in the context structure.
        and a #define.  The #define makes the variable name refer to the
        corresponding entry in the context structure.
*******************************************************************************/

typedef struct
{
        Widget  UxFontSep;
        Widget  UxseparatorGadget2;
} _UxCfontDialog;

#define FontSep                 UxFontDialogContext->UxFontSep
#define separatorGadget2        UxFontDialogContext->UxseparatorGadget2

static _UxCfontDialog   *UxFontDialogContext;

Widget  fontDialog;
Widget  FontRowCol;
Widget  Font13;
Widget  Font1;
Widget  Font2;
Widget  Font3;
Widget  Font4;
Widget  Font5;
Widget  Font6;
Widget  Font7;
Widget  Font8;
Widget  Font9;
Widget  Font10;
Widget  Font11;
Widget  Font12;
Widget  Font0;
Widget  FontMenu;
Widget  PrecisionRowCol;
Widget  Pre0;
Widget  Pre1;
Widget  Pre2;
Widget  PrecisionMenu;
Widget  OkFontButton;
Widget  ResetFontButton;
Widget  CloseFontButton;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_fontDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_fontDialog()
{
        Widget  fontDialog_shell;
        Widget  FontRowCol_shell;
        Widget  PrecisionRowCol_shell;

        fontDialog_shell = XtVaCreatePopupShell( "fontDialog_shell",
                        xmDialogShellWidgetClass, fontSettingsDialog,
                        XmNx, 178,
                        XmNy, 115,
                        XmNwidth, 300,
                        XmNheight, 200,
                        XmNallowShellResize, FALSE,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "fontDialog",
                        NULL );

        fontDialog = XtVaCreateWidget( "fontDialog",
                        xmFormWidgetClass, fontDialog_shell,
                        RES_CONVERT( XmNdialogTitle, "Font" ),
                        XmNautoUnmanage, FALSE,
                        XmNdialogStyle, XmDIALOG_MODELESS,
                        XmNnoResize, TRUE,
                        XmNheight, 200,
                        XmNwidth, 300,
                        XmNunitType, XmPIXELS,
                        NULL );

        UxPutContext( fontDialog, (char *) UxFontDialogContext );

        FontRowCol_shell = XtVaCreatePopupShell ("FontRowCol_shell",
                        xmMenuShellWidgetClass, fontDialog,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        FontRowCol = XtVaCreateWidget( "FontRowCol",
                        xmRowColumnWidgetClass, FontRowCol_shell,
                        XmNmappedWhenManaged, TRUE,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );

        UxPutContext( FontRowCol, (char *) UxFontDialogContext );

        Font13 = XtVaCreateManagedWidget( "Font13",
                        xmPushButtonWidgetClass, FontRowCol,
               RES_CONVERT( XmNfontList, "*-times-medium-r-normal*-120-*" ),
                        XmNuserData, (XtPointer) 13,
                        RES_CONVERT( XmNlabelString, "Times Roman" ),
                        NULL );

        UxPutContext( Font13, (char *) UxFontDialogContext );

        Font1 = XtVaCreateManagedWidget( "Font1",
                        xmPushButtonWidgetClass, FontRowCol,
             RES_CONVERT( XmNfontList, "*-times-medium-i-normal*-120-*" ),
                        XmNuserData, (XtPointer) 1,
                        RES_CONVERT( XmNlabelString, "Times Italic" ),
                        NULL );

        UxPutContext( Font1, (char *) UxFontDialogContext );

        Font2 = XtVaCreateManagedWidget( "Font2",
                        xmPushButtonWidgetClass, FontRowCol,
             RES_CONVERT( XmNfontList, "*-times-bold-r-normal*-120-*" ),
                        XmNuserData, (XtPointer) 2,
                        RES_CONVERT( XmNlabelString, "Times Bold" ),
                        NULL );

        UxPutContext( Font2, (char *) UxFontDialogContext );

        Font3 = XtVaCreateManagedWidget( "Font3",
                        xmPushButtonWidgetClass, FontRowCol,
             RES_CONVERT( XmNfontList, "*-times-bold-i-normal*-120-*" ),
                        XmNuserData, (XtPointer) 3,
                        RES_CONVERT( XmNlabelString, "Times Bold Italic" ),
                        NULL );

        UxPutContext( Font3, (char *) UxFontDialogContext );

        Font4 = XtVaCreateManagedWidget( "Font4",
                        xmPushButtonWidgetClass, FontRowCol,
           RES_CONVERT( XmNfontList, "*-helvetica-medium-r-normal*-120-*" ),
                        XmNuserData, (XtPointer) 4,
                        RES_CONVERT( XmNlabelString, "Helvetica" ),
                        NULL );

        UxPutContext( Font4, (char *) UxFontDialogContext );

        Font5 = XtVaCreateManagedWidget( "Font5",
                        xmPushButtonWidgetClass, FontRowCol,
              RES_CONVERT( XmNfontList, "*-helvetica-medium-o-normal*-120-*" ),
                        XmNuserData, (XtPointer) 5,
                        RES_CONVERT( XmNlabelString, "Helvetica Oblique" ),
                        NULL );

        UxPutContext( Font5, (char *) UxFontDialogContext );

        Font6 = XtVaCreateManagedWidget( "Font6",
                        xmPushButtonWidgetClass, FontRowCol,
          RES_CONVERT( XmNfontList, "*-helvetica-bold-r-normal*-120-*" ),
                        XmNuserData, (XtPointer) 6,
                        RES_CONVERT( XmNlabelString, "Helvetica Bold" ),
                        NULL );

        UxPutContext( Font6, (char *) UxFontDialogContext );

        Font7 = XtVaCreateManagedWidget( "Font7",
                        xmPushButtonWidgetClass, FontRowCol,
          RES_CONVERT( XmNfontList, "*-helvetica-bold-o-normal*-120-*" ),
                        XmNuserData, (XtPointer) 7,
                       RES_CONVERT( XmNlabelString, "Helvetica Bold Oblique" ),
                        NULL );

        UxPutContext( Font7, (char *) UxFontDialogContext );

        Font8 = XtVaCreateManagedWidget( "Font8",
                        xmPushButtonWidgetClass, FontRowCol,
             RES_CONVERT( XmNfontList, "*-courier-medium-r-normal*-120-*" ),
                        XmNuserData, (XtPointer) 8,
                        RES_CONVERT( XmNlabelString, "Courier" ),
                        NULL );

        UxPutContext( Font8, (char *) UxFontDialogContext );

        Font9 = XtVaCreateManagedWidget( "Font9",
                        xmPushButtonWidgetClass, FontRowCol,
            RES_CONVERT( XmNfontList, "*-courier-medium-o-normal*-120-*" ),
                        XmNuserData, (XtPointer) 9,
                        RES_CONVERT( XmNlabelString, "Courier Oblique" ),
                        NULL );

        UxPutContext( Font9, (char *) UxFontDialogContext );

        Font10 = XtVaCreateManagedWidget( "Font10",
                        xmPushButtonWidgetClass, FontRowCol,
           RES_CONVERT( XmNfontList, "*-courier-bold-r-normal*-120-*" ),
                        XmNuserData, (XtPointer) 10,
                        RES_CONVERT( XmNlabelString, "Courier Bold" ),
                        NULL );

        UxPutContext( Font10, (char *) UxFontDialogContext );

        Font11 = XtVaCreateManagedWidget( "Font11",
                        xmPushButtonWidgetClass, FontRowCol,
           RES_CONVERT( XmNfontList, "*-courier-bold-o-normal*-120-*" ),
                        XmNuserData, (XtPointer) 11,
                        RES_CONVERT( XmNlabelString, "Courier Bold Oblique" ),
                        NULL );

        UxPutContext( Font11, (char *) UxFontDialogContext );

        Font12 = XtVaCreateManagedWidget( "Font12",
                        xmPushButtonWidgetClass, FontRowCol,
             RES_CONVERT( XmNfontList, "*-symbol-medium-r-normal*-120-*" ),
                        XmNuserData, (XtPointer) 12,
                        RES_CONVERT( XmNlabelString, "Symbol" ),
                        NULL );

        UxPutContext( Font12, (char *) UxFontDialogContext );

        FontSep = XtVaCreateManagedWidget( "FontSep",
                        xmSeparatorGadgetClass, FontRowCol,
                        NULL );

        UxPutContext( FontSep, (char *) UxFontDialogContext );

        Font0 = XtVaCreateManagedWidget( "Font0",
                        xmPushButtonWidgetClass, FontRowCol,
                        XmNuserData, (XtPointer) 14,
                        RES_CONVERT( XmNlabelString, "HIGZ Sofware" ),
                        NULL );

        UxPutContext( Font0, (char *) UxFontDialogContext );

        FontMenu = XtVaCreateManagedWidget( "FontMenu",
                        xmRowColumnWidgetClass, fontDialog,
                        XmNmappedWhenManaged, TRUE,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 10,
                        XmNleftOffset, 5,
                        RES_CONVERT( XmNlabelString, "Font :" ),
                        XmNsubMenuId, FontRowCol,
                        XmNrowColumnType, XmMENU_OPTION,
                        NULL );

        UxPutContext( FontMenu, (char *) UxFontDialogContext );

        PrecisionRowCol_shell = XtVaCreatePopupShell ("PrecisionRowCol_shell",
                        xmMenuShellWidgetClass, fontDialog,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        PrecisionRowCol = XtVaCreateWidget( "PrecisionRowCol",
                        xmRowColumnWidgetClass, PrecisionRowCol_shell,
                        XmNmappedWhenManaged, TRUE,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );

        UxPutContext( PrecisionRowCol, (char *) UxFontDialogContext );

        Pre0 = XtVaCreateManagedWidget( "Pre0",
                        xmPushButtonWidgetClass, PrecisionRowCol,
                        XmNmappedWhenManaged, TRUE,
                        XmNuserData, (XtPointer) 1,
                        RES_CONVERT( XmNlabelString, "Hardware (0)" ),
                        NULL );

        UxPutContext( Pre0, (char *) UxFontDialogContext );

        Pre1 = XtVaCreateManagedWidget( "Pre1",
                        xmPushButtonWidgetClass, PrecisionRowCol,
                        XmNuserData, (XtPointer) 2,
                        RES_CONVERT( XmNlabelString, "String (1)" ),
                        NULL );

        UxPutContext( Pre1, (char *) UxFontDialogContext );

        Pre2 = XtVaCreateManagedWidget( "Pre2",
                        xmPushButtonWidgetClass, PrecisionRowCol,
                        XmNuserData, (XtPointer) 3,
                        RES_CONVERT( XmNlabelString, "Software (2)" ),
                        NULL );

        UxPutContext( Pre2, (char *) UxFontDialogContext );

        PrecisionMenu = XtVaCreateManagedWidget( "PrecisionMenu",
                        xmRowColumnWidgetClass, fontDialog,
                        XmNmappedWhenManaged, TRUE,
                        XmNtopWidget, FontMenu,
                        XmNtopOffset, 20,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightAttachment, XmATTACH_NONE,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        RES_CONVERT( XmNlabelString, "Precision :" ),
                        XmNbottomAttachment, XmATTACH_NONE,
                        XmNsubMenuId, PrecisionRowCol,
                        XmNrowColumnType, XmMENU_OPTION,
                        NULL );

        UxPutContext( PrecisionMenu, (char *) UxFontDialogContext );

        OkFontButton = XtVaCreateManagedWidget( "OkFontButton",
                        xmPushButtonGadgetClass, fontDialog,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "OK" ),
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 480,
                        XmNx, 10,
                        NULL );

        UxPutContext( OkFontButton, (char *) UxFontDialogContext );

        ResetFontButton = XtVaCreateManagedWidget( "ResetFontButton",
                        xmPushButtonGadgetClass, fontDialog,
                        XmNrightOffset, 30,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftOffset, 15,
                        XmNleftWidget, OkFontButton,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 480,
                        XmNx, 10,
                        NULL );

        UxPutContext( ResetFontButton, (char *) UxFontDialogContext );

        CloseFontButton = XtVaCreateManagedWidget( "CloseFontButton",
                        xmPushButtonGadgetClass, fontDialog,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftOffset, 15,
                        XmNleftWidget, ResetFontButton,
                        XmNleftAttachment, XmATTACH_NONE,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 480,
                        XmNx, 10,
                        NULL );

        UxPutContext( CloseFontButton, (char *) UxFontDialogContext );

        separatorGadget2 = XtVaCreateManagedWidget( "separatorGadget2",
                        xmSeparatorGadgetClass, fontDialog,
                        XmNbottomWidget, OkFontButton,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        NULL );

        UxPutContext( separatorGadget2, (char *) UxFontDialogContext );

        XtAddCallback( fontDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxFontDialogContext );

        XtVaSetValues(fontDialog,
                        XmNdefaultButton, OkFontButton,
                        NULL );

        XtVaSetValues(FontRowCol,
                        XmNmenuHistory, Font0,
                        NULL );

        XtVaSetValues(FontMenu,
                        XmNmenuHistory, Font0,
                        NULL );

        XtVaSetValues(PrecisionRowCol,
                        XmNmenuHistory, Pre0,
                        NULL );

        XtVaSetValues(PrecisionMenu,
                        XmNmenuHistory, Pre0,
                        NULL );



        return ( fontDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_fontDialog()
{
        Widget                  rtrn;
        _UxCfontDialog          *UxContext;

        UxFontDialogContext = UxContext =
                (_UxCfontDialog *) XtMalloc( sizeof(_UxCfontDialog) );

        rtrn = _Uxbuild_fontDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_fontDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_fontDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

