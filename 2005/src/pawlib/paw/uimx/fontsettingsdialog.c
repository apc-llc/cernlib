/*
 * $Id: fontsettingsdialog.c,v 1.1.1.1 1996/03/01 11:39:33 mclareni Exp $
 *
 * $Log: fontsettingsdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:33  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.06  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        fontSettingsDialog.c
        (Generated from interface file fontSettingsDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/Form.h>

/*******************************************************************************
        Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern swidget histoStyle;

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
        Widget  UxseparatorGadget3;
        Widget  UxEncadreForm2;
        Widget  UxTitre2;
        Widget  UxUpperLeftSep2;
        Widget  UxLeftSep2;
        Widget  UxBottomSep2;
        Widget  UxRightSep2;
        Widget  UxUpperRightSep2;
} _UxCfontSettingsDialog;

#define separatorGadget3        UxFontSettingsDialogContext->UxseparatorGadget3
#define EncadreForm2            UxFontSettingsDialogContext->UxEncadreForm2
#define Titre2                  UxFontSettingsDialogContext->UxTitre2
#define UpperLeftSep2           UxFontSettingsDialogContext->UxUpperLeftSep2
#define LeftSep2                UxFontSettingsDialogContext->UxLeftSep2
#define BottomSep2              UxFontSettingsDialogContext->UxBottomSep2
#define RightSep2               UxFontSettingsDialogContext->UxRightSep2
#define UpperRightSep2          UxFontSettingsDialogContext->UxUpperRightSep2

static _UxCfontSettingsDialog   *UxFontSettingsDialogContext;

Widget  fontSettingsDialog;
Widget  ApplyFontSettingsBut;
Widget  ResetFontSettingsBut;
Widget  CloseFontSettingsBut;
Widget  FontRow;
Widget  LabelsButton;
Widget  GlobalButton;
Widget  ValuesButton;
Widget  TextButton;
Widget  CommentButton;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_fontSettingsDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_fontSettingsDialog()
{
        Widget  fontSettingsDialog_shell;

        fontSettingsDialog_shell = XtVaCreatePopupShell(
                        "fontSettingsDialog_shell",
                        xmDialogShellWidgetClass, histoStyle,
                        XmNx, 454,
                        XmNy, 310,
                        XmNwidth, 280,
                        XmNheight, 270,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "fontSettingsDialog",
                        NULL );

        fontSettingsDialog = XtVaCreateWidget( "fontSettingsDialog",
                        xmFormWidgetClass, fontSettingsDialog_shell,
                        XmNnoResize, TRUE,
                        RES_CONVERT( XmNdialogTitle, "Font Settings" ),
                        XmNautoUnmanage, FALSE,
                        XmNheight, 270,
                        XmNwidth, 280,
                        XmNunitType, XmPIXELS,
                        NULL );

      UxPutContext( fontSettingsDialog, (char *) UxFontSettingsDialogContext );

        ApplyFontSettingsBut = XtVaCreateManagedWidget( "ApplyFontSettingsBut",
                        xmPushButtonGadgetClass, fontSettingsDialog,
                        XmNleftOffset, 7,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Apply" ),
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 376,
                        XmNx, 13,
                        NULL );

   UxPutContext( ApplyFontSettingsBut, (char *) UxFontSettingsDialogContext );

        ResetFontSettingsBut = XtVaCreateManagedWidget( "ResetFontSettingsBut",
                        xmPushButtonGadgetClass, fontSettingsDialog,
                        XmNleftWidget, ApplyFontSettingsBut,
                        XmNleftOffset, 7,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 376,
                        XmNx, 147,
                        NULL );

    UxPutContext( ResetFontSettingsBut, (char *) UxFontSettingsDialogContext );

        CloseFontSettingsBut = XtVaCreateManagedWidget( "CloseFontSettingsBut",
                        xmPushButtonGadgetClass, fontSettingsDialog,
                        XmNleftWidget, ResetFontSettingsBut,
                        XmNleftOffset, 7,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 200,
                        XmNx, 95,
                        NULL );

    UxPutContext( CloseFontSettingsBut, (char *) UxFontSettingsDialogContext );

        separatorGadget3 = XtVaCreateManagedWidget( "separatorGadget3",
                        xmSeparatorGadgetClass, fontSettingsDialog,
                        XmNbottomWidget, ApplyFontSettingsBut,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        NULL );

       UxPutContext( separatorGadget3, (char *) UxFontSettingsDialogContext );

        EncadreForm2 = XtVaCreateManagedWidget( "EncadreForm2",
                        xmFormWidgetClass, fontSettingsDialog,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomWidget, separatorGadget3,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNheight, 175,
                        XmNwidth, 274,
                        XmNy, 4,
                        XmNx, 2,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( EncadreForm2, (char *) UxFontSettingsDialogContext );

        Titre2 = XtVaCreateManagedWidget( "Titre2",
                        xmLabelGadgetClass, EncadreForm2,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNmarginWidth, 0,
                        XmNrecomputeSize, TRUE,
                        RES_CONVERT( XmNlabelString, "Fonts" ),
                        XmNheight, 20,
                        XmNwidth, 50,
                        XmNy, 0,
                        XmNx, 10,
                        NULL );

        UxPutContext( Titre2, (char *) UxFontSettingsDialogContext );

        UpperLeftSep2 = XtVaCreateManagedWidget( "UpperLeftSep2",
                        xmSeparatorGadgetClass, EncadreForm2,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, Titre2,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 2,
                        XmNwidth, 99,
                        XmNy, 200,
                        XmNx, 87,
                        NULL );

        UxPutContext( UpperLeftSep2, (char *) UxFontSettingsDialogContext );

        LeftSep2 = XtVaCreateManagedWidget( "LeftSep2",
                        xmSeparatorGadgetClass, EncadreForm2,
                        XmNtopOffset, 11,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNorientation, XmVERTICAL,
                        XmNheight, 116,
                        XmNwidth, 2,
                        XmNy, 201,
                        XmNx, 84,
                        NULL );

        UxPutContext( LeftSep2, (char *) UxFontSettingsDialogContext );

        FontRow = XtVaCreateManagedWidget( "FontRow",
                        xmRowColumnWidgetClass, EncadreForm2,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopOffset, 15,
                        XmNrightOffset, 2,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 2,
                        XmNbottomOffset, 2,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNresizeWidth, FALSE,
                        XmNresizeHeight, TRUE,
                        XmNwidth, 225,
                        XmNheight, 170,
                        XmNmarginWidth, 5,
                        XmNmarginHeight, 5,
                        XmNborderWidth, 0,
                        XmNy, 30,
                        XmNx, 20,
                        NULL );

        UxPutContext( FontRow, (char *) UxFontSettingsDialogContext );

        LabelsButton = XtVaCreateManagedWidget( "LabelsButton",
                        xmPushButtonWidgetClass, FontRow,
                        XmNheight, 30,
                        RES_CONVERT( XmNfontList,
          "-adobe-courier-bold-o-normal--25-180-100-100-m-150-iso8859-1" ),
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNlabelString, "Axis Labels" ),
                        XmNy, 129,
                        XmNx, 9,
                        NULL );

        UxPutContext( LabelsButton, (char *) UxFontSettingsDialogContext );

        GlobalButton = XtVaCreateManagedWidget( "GlobalButton",
                        xmPushButtonWidgetClass, FontRow,
                        XmNwidth, 164,
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNlabelString, "Global title" ),
                        XmNy, 70,
                        XmNx, 6,
                        NULL );

        UxPutContext( GlobalButton, (char *) UxFontSettingsDialogContext );

        ValuesButton = XtVaCreateManagedWidget( "ValuesButton",
                        xmPushButtonWidgetClass, FontRow,
                        XmNheight, 30,
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNlabelString, "Axis Values" ),
                        XmNy, 100,
                        XmNx, 790,
                        NULL );

        UxPutContext( ValuesButton, (char *) UxFontSettingsDialogContext );

        TextButton = XtVaCreateManagedWidget( "TextButton",
                        xmPushButtonWidgetClass, FontRow,
                        XmNheight, 30,
                        XmNwidth, 218,
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNlabelString, "Text and title" ),
                        XmNy, 89,
                        XmNx, -31,
                        NULL );

        UxPutContext( TextButton, (char *) UxFontSettingsDialogContext );

        CommentButton = XtVaCreateManagedWidget( "CommentButton",
                        xmPushButtonWidgetClass, FontRow,
                        XmNheight, 30,
                        XmNwidth, 224,
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNlabelString, "Comment" ),
                        XmNy, 39,
                        XmNx, -41,
                        NULL );

        UxPutContext( CommentButton, (char *) UxFontSettingsDialogContext );

        BottomSep2 = XtVaCreateManagedWidget( "BottomSep2",
                        xmSeparatorGadgetClass, EncadreForm2,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 2,
                        XmNwidth, 292,
                        XmNy, 315,
                        XmNx, 86,
                        NULL );

        UxPutContext( BottomSep2, (char *) UxFontSettingsDialogContext );

        RightSep2 = XtVaCreateManagedWidget( "RightSep2",
                        xmSeparatorGadgetClass, EncadreForm2,
                        XmNtopOffset, 11,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_NONE,
                        XmNbottomOffset, 1,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNorientation, XmVERTICAL,
                        XmNheight, 116,
                        XmNwidth, 2,
                        XmNy, 201,
                        XmNx, 375,
                        NULL );

        UxPutContext( RightSep2, (char *) UxFontSettingsDialogContext );

        UpperRightSep2 = XtVaCreateManagedWidget( "UpperRightSep2",
                        xmSeparatorGadgetClass, EncadreForm2,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftWidget, Titre2,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNheight, 2,
                        XmNwidth, 106,
                        XmNy, 200,
                        XmNx, 274,
                        NULL );

        UxPutContext( UpperRightSep2, (char *) UxFontSettingsDialogContext );

        XtAddCallback( fontSettingsDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxFontSettingsDialogContext );

        XtVaSetValues(fontSettingsDialog,
                        XmNdefaultButton, ApplyFontSettingsBut,
                        NULL );



        return ( fontSettingsDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_fontSettingsDialog()
{
        Widget                  rtrn;
        _UxCfontSettingsDialog  *UxContext;

        UxFontSettingsDialogContext = UxContext =
        (_UxCfontSettingsDialog *) XtMalloc( sizeof(_UxCfontSettingsDialog) );

        rtrn = _Uxbuild_fontSettingsDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_fontSettingsDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_fontSettingsDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

