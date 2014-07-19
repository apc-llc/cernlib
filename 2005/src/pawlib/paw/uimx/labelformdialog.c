/*
 * $Id: labelformdialog.c,v 1.1.1.1 1996/03/01 11:39:34 mclareni Exp $
 *
 * $Log: labelformdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:34  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.06  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        labelFormDialog.c
        (Generated from interface file labelFormDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleBG.h>
#include <Xm/Form.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/Form.h>

/*******************************************************************************
        Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern swidget axisFormDialog;

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
        Widget  UxseparatorGadget11;
        Widget  UxlabelForm0;
        Widget  UxlabelForm1;
        Widget  UxlabelForm2;
        Widget  UxlabelForm3;
        Widget  UxlabelForm4;
        Widget  UxlabelForm5;
        Widget  UxlabelForm6;
        Widget  UxlabelForm7;
        Widget  UxlabelForm8;
        Widget  UxlabelForm9;
} _UxClabelFormDialog;

#define separatorGadget11       UxLabelFormDialogContext->UxseparatorGadget11
#define labelForm0              UxLabelFormDialogContext->UxlabelForm0
#define labelForm1              UxLabelFormDialogContext->UxlabelForm1
#define labelForm2              UxLabelFormDialogContext->UxlabelForm2
#define labelForm3              UxLabelFormDialogContext->UxlabelForm3
#define labelForm4              UxLabelFormDialogContext->UxlabelForm4
#define labelForm5              UxLabelFormDialogContext->UxlabelForm5
#define labelForm6              UxLabelFormDialogContext->UxlabelForm6
#define labelForm7              UxLabelFormDialogContext->UxlabelForm7
#define labelForm8              UxLabelFormDialogContext->UxlabelForm8
#define labelForm9              UxLabelFormDialogContext->UxlabelForm9

static _UxClabelFormDialog      *UxLabelFormDialogContext;

Widget  labelFormDialog;
Widget  labelCloseButton;
Widget  labeltoggleButtonGadget0;
Widget  labelrowColumn;
Widget  labelGadget11;
Widget  labelText1;
Widget  labeltoggleButtonGadget1;
Widget  labelText2;
Widget  labeltoggleButtonGadget2;
Widget  labelText3;
Widget  labeltoggleButtonGadget3;
Widget  labelText4;
Widget  labeltoggleButtonGadget4;
Widget  labelText5;
Widget  labeltoggleButtonGadget5;
Widget  labelText6;
Widget  labeltoggleButtonGadget6;
Widget  labelText7;
Widget  labeltoggleButtonGadget7;
Widget  labelText8;
Widget  labeltoggleButtonGadget8;
Widget  labelText9;
Widget  labeltoggleButtonGadget9;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_labelFormDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_labelFormDialog()
{
        Widget  labelFormDialog_shell;

        labelFormDialog_shell = XtVaCreatePopupShell( "labelFormDialog_shell",
                        xmDialogShellWidgetClass, axisFormDialog,
                        XmNx, 697,
                        XmNy, 136,
                        XmNwidth, 362,
                        XmNheight, 520,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "labelFormDialog",
                        NULL );

        labelFormDialog = XtVaCreateWidget( "labelFormDialog",
                        xmFormWidgetClass, labelFormDialog_shell,
                        XmNautoUnmanage, FALSE,
                        RES_CONVERT( XmNdialogTitle, "Axis Labels" ),
                        XmNheight, 520,
                        XmNwidth, 362,
                        XmNunitType, XmPIXELS,
                        NULL );

        UxPutContext( labelFormDialog, (char *) UxLabelFormDialogContext );

        labelCloseButton = XtVaCreateManagedWidget( "labelCloseButton",
                        xmPushButtonGadgetClass, labelFormDialog,
                        XmNrightPosition, 65,
                        XmNleftPosition, 35,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftAttachment, XmATTACH_POSITION,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNheight, 30,
                        XmNwidth, 110,
                        XmNy, 360,
                        XmNx, 120,
                        NULL );

        UxPutContext( labelCloseButton, (char *) UxLabelFormDialogContext );

        separatorGadget11 = XtVaCreateManagedWidget( "separatorGadget11",
                        xmSeparatorGadgetClass, labelFormDialog,
                        XmNbottomWidget, labelCloseButton,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNhighlightThickness, 2,
                        NULL );

        UxPutContext( separatorGadget11, (char *) UxLabelFormDialogContext );

        labelForm0 = XtVaCreateManagedWidget( "labelForm0",
                        xmFormWidgetClass, labelFormDialog,
                        XmNtopOffset, 5,
                        XmNheight, 35,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 8,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNwidth, 125,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( labelForm0, (char *) UxLabelFormDialogContext );

labeltoggleButtonGadget0 = XtVaCreateManagedWidget( "labeltoggleButtonGadget0",
                        xmToggleButtonGadgetClass, labelForm0,
                        XmNwidth, 360,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Numeric" ),
                        NULL );

UxPutContext( labeltoggleButtonGadget0, (char *) UxLabelFormDialogContext );

        labelrowColumn = XtVaCreateManagedWidget( "labelrowColumn",
                        xmRowColumnWidgetClass, labelFormDialog,
                        XmNtopWidget, labelForm0,
                        XmNbottomWidget, separatorGadget11,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNy, 45,
                        XmNx, 15,
                        NULL );

        UxPutContext( labelrowColumn, (char *) UxLabelFormDialogContext );

        labelGadget11 = XtVaCreateManagedWidget( "labelGadget11",
                        xmLabelGadgetClass, labelrowColumn,
                        RES_CONVERT( XmNlabelString, "Alphanumeric Labels:" ),
                        XmNheight, 30,
                        XmNy, 15,
                        NULL );

        UxPutContext( labelGadget11, (char *) UxLabelFormDialogContext );

        labelForm1 = XtVaCreateManagedWidget( "labelForm1",
                        xmFormWidgetClass, labelrowColumn,
                        XmNheight, 40,
                        XmNwidth, 125,
                        XmNy, 13,
                        XmNx, 210,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( labelForm1, (char *) UxLabelFormDialogContext );

        labelText1 = XtVaCreateManagedWidget( "labelText1",
                        xmTextWidgetClass, labelForm1,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 30,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 35,
                        XmNy, 50,
                        NULL );

        UxPutContext( labelText1, (char *) UxLabelFormDialogContext );

labeltoggleButtonGadget1 = XtVaCreateManagedWidget( "labeltoggleButtonGadget1",
                        xmToggleButtonGadgetClass, labelForm1,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNheight, 20,
                        XmNwidth, 20,
                        XmNy, 55,
                        XmNx, 35,
                        NULL );

UxPutContext( labeltoggleButtonGadget1, (char *) UxLabelFormDialogContext );

        labelForm2 = XtVaCreateManagedWidget( "labelForm2",
                        xmFormWidgetClass, labelrowColumn,
                        XmNheight, 40,
                        XmNwidth, 125,
                        XmNy, 51,
                        XmNx, 210,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( labelForm2, (char *) UxLabelFormDialogContext );

        labelText2 = XtVaCreateManagedWidget( "labelText2",
                        xmTextWidgetClass, labelForm2,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 30,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 35,
                        XmNy, 60,
                        XmNx, 115,
                        NULL );

        UxPutContext( labelText2, (char *) UxLabelFormDialogContext );

labeltoggleButtonGadget2 = XtVaCreateManagedWidget( "labeltoggleButtonGadget2",
                        xmToggleButtonGadgetClass, labelForm2,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNheight, 20,
                        XmNwidth, 20,
                        XmNy, 65,
                        XmNx, 45,
                        NULL );

UxPutContext( labeltoggleButtonGadget2, (char *) UxLabelFormDialogContext );

        labelForm3 = XtVaCreateManagedWidget( "labelForm3",
                        xmFormWidgetClass, labelrowColumn,
                        XmNheight, 40,
                        XmNwidth, 125,
                        XmNy, 89,
                        XmNx, 210,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( labelForm3, (char *) UxLabelFormDialogContext );

        labelText3 = XtVaCreateManagedWidget( "labelText3",
                        xmTextWidgetClass, labelForm3,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 30,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 35,
                        XmNy, 70,
                        XmNx, 125,
                        NULL );

        UxPutContext( labelText3, (char *) UxLabelFormDialogContext );

labeltoggleButtonGadget3 = XtVaCreateManagedWidget( "labeltoggleButtonGadget3",
                        xmToggleButtonGadgetClass, labelForm3,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 20,
                        XmNwidth, 20,
                        XmNy, 75,
                        XmNx, 55,
                        NULL );

UxPutContext( labeltoggleButtonGadget3, (char *) UxLabelFormDialogContext );

        labelForm4 = XtVaCreateManagedWidget( "labelForm4",
                        xmFormWidgetClass, labelrowColumn,
                        XmNshadowType, XmSHADOW_OUT,
                        XmNshadowThickness, 0,
                        XmNheight, 40,
                        XmNwidth, 194,
                        XmNy, 3,
                        XmNx, 3,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( labelForm4, (char *) UxLabelFormDialogContext );

        labelText4 = XtVaCreateManagedWidget( "labelText4",
                        xmTextWidgetClass, labelForm4,
                        XmNwidth, 301,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 30,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNy, 0,
                        XmNx, 35,
                        NULL );

        UxPutContext( labelText4, (char *) UxLabelFormDialogContext );

labeltoggleButtonGadget4 = XtVaCreateManagedWidget( "labeltoggleButtonGadget4",
                        xmToggleButtonGadgetClass, labelForm4,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 20,
                        XmNwidth, 20,
                        XmNy, 35,
                        XmNx, 15,
                        NULL );

UxPutContext( labeltoggleButtonGadget4, (char *) UxLabelFormDialogContext );

        labelForm5 = XtVaCreateManagedWidget( "labelForm5",
                        xmFormWidgetClass, labelrowColumn,
                        XmNshadowType, XmSHADOW_OUT,
                        XmNshadowThickness, 0,
                        XmNheight, 40,
                        XmNwidth, 194,
                        XmNy, 185,
                        XmNx, 13,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( labelForm5, (char *) UxLabelFormDialogContext );

        labelText5 = XtVaCreateManagedWidget( "labelText5",
                        xmTextWidgetClass, labelForm5,
                        XmNwidth, 301,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 30,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNy, 0,
                        XmNx, 35,
                        NULL );

        UxPutContext( labelText5, (char *) UxLabelFormDialogContext );

labeltoggleButtonGadget5 = XtVaCreateManagedWidget( "labeltoggleButtonGadget5",
                        xmToggleButtonGadgetClass, labelForm5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 20,
                        XmNwidth, 20,
                        XmNy, 35,
                        XmNx, 15,
                        NULL );

UxPutContext( labeltoggleButtonGadget5, (char *) UxLabelFormDialogContext );

        labelForm6 = XtVaCreateManagedWidget( "labelForm6",
                        xmFormWidgetClass, labelrowColumn,
                        XmNshadowType, XmSHADOW_OUT,
                        XmNshadowThickness, 0,
                        XmNheight, 40,
                        XmNwidth, 194,
                        XmNy, 228,
                        XmNx, 13,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( labelForm6, (char *) UxLabelFormDialogContext );

        labelText6 = XtVaCreateManagedWidget( "labelText6",
                        xmTextWidgetClass, labelForm6,
                        XmNwidth, 301,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 30,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNy, 0,
                        XmNx, 35,
                        NULL );

        UxPutContext( labelText6, (char *) UxLabelFormDialogContext );

labeltoggleButtonGadget6 = XtVaCreateManagedWidget( "labeltoggleButtonGadget6",
                        xmToggleButtonGadgetClass, labelForm6,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 20,
                        XmNwidth, 20,
                        XmNy, 35,
                        XmNx, 15,
                        NULL );

UxPutContext( labeltoggleButtonGadget6, (char *) UxLabelFormDialogContext );

        labelForm7 = XtVaCreateManagedWidget( "labelForm7",
                        xmFormWidgetClass, labelrowColumn,
                        XmNshadowType, XmSHADOW_OUT,
                        XmNshadowThickness, 0,
                        XmNheight, 40,
                        XmNwidth, 194,
                        XmNy, 271,
                        XmNx, 13,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( labelForm7, (char *) UxLabelFormDialogContext );

        labelText7 = XtVaCreateManagedWidget( "labelText7",
                        xmTextWidgetClass, labelForm7,
                        XmNwidth, 301,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 30,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNy, 0,
                        XmNx, 35,
                        NULL );

        UxPutContext( labelText7, (char *) UxLabelFormDialogContext );

labeltoggleButtonGadget7 = XtVaCreateManagedWidget( "labeltoggleButtonGadget7",
                        xmToggleButtonGadgetClass, labelForm7,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 20,
                        XmNwidth, 20,
                        XmNy, 35,
                        XmNx, 15,
                        NULL );

UxPutContext( labeltoggleButtonGadget7, (char *) UxLabelFormDialogContext );

        labelForm8 = XtVaCreateManagedWidget( "labelForm8",
                        xmFormWidgetClass, labelrowColumn,
                        XmNshadowType, XmSHADOW_OUT,
                        XmNshadowThickness, 0,
                        XmNheight, 40,
                        XmNwidth, 194,
                        XmNy, 314,
                        XmNx, 13,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( labelForm8, (char *) UxLabelFormDialogContext );

        labelText8 = XtVaCreateManagedWidget( "labelText8",
                        xmTextWidgetClass, labelForm8,
                        XmNwidth, 301,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 30,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNy, 0,
                        XmNx, 35,
                        NULL );

        UxPutContext( labelText8, (char *) UxLabelFormDialogContext );

labeltoggleButtonGadget8 = XtVaCreateManagedWidget( "labeltoggleButtonGadget8",
                        xmToggleButtonGadgetClass, labelForm8,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 20,
                        XmNwidth, 20,
                        XmNy, 35,
                        XmNx, 15,
                        NULL );

UxPutContext( labeltoggleButtonGadget8, (char *) UxLabelFormDialogContext );

        labelForm9 = XtVaCreateManagedWidget( "labelForm9",
                        xmFormWidgetClass, labelrowColumn,
                        XmNshadowType, XmSHADOW_OUT,
                        XmNshadowThickness, 0,
                        XmNheight, 40,
                        XmNwidth, 194,
                        XmNy, 357,
                        XmNx, 13,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( labelForm9, (char *) UxLabelFormDialogContext );

        labelText9 = XtVaCreateManagedWidget( "labelText9",
                        xmTextWidgetClass, labelForm9,
                        XmNwidth, 301,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 30,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNy, 0,
                        XmNx, 35,
                        NULL );

        UxPutContext( labelText9, (char *) UxLabelFormDialogContext );

labeltoggleButtonGadget9 = XtVaCreateManagedWidget( "labeltoggleButtonGadget9",
                        xmToggleButtonGadgetClass, labelForm9,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 20,
                        XmNwidth, 20,
                        XmNy, 35,
                        XmNx, 15,
                        NULL );

UxPutContext( labeltoggleButtonGadget9, (char *) UxLabelFormDialogContext );

        XtAddCallback( labelFormDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxLabelFormDialogContext );



        return ( labelFormDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_labelFormDialog()
{
        Widget                  rtrn;
        _UxClabelFormDialog     *UxContext;

        UxLabelFormDialogContext = UxContext =
                (_UxClabelFormDialog *) XtMalloc( sizeof(_UxClabelFormDialog) );

        rtrn = _Uxbuild_labelFormDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_labelFormDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_labelFormDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

