/*
 * $Id: attributesformdialog.c,v 1.1.1.1 1996/03/01 11:39:32 mclareni Exp $
 *
 * $Log: attributesformdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:32  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.04  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        attributesFormDialog.c
        (Generated from interface file attributesFormDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Text.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <Xm/Label.h>
#include <Xm/ArrowB.h>
#include <Xm/Form.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
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
        Widget  UxseparatorGadget1;
        Widget  Uxform9;
        Widget  UxEncadreForm1;
        Widget  UxTitre1;
        Widget  UxUpperLeftSep1;
        Widget  UxLeftSep1;
        Widget  UxBottomSep1;
        Widget  UxRightSep1;
        Widget  UxUpperRightSep1;
        Widget  UxEncadreForm7;
        Widget  UxTitre7;
        Widget  UxUpperLeftSep7;
        Widget  UxLeftSep7;
        Widget  UxBottomSep7;
        Widget  UxRightSep7;
        Widget  UxUpperRightSep7;
        Widget  Uxform2;
        Widget  UxUpperLeftSep9;
        Widget  UxLeftSep9;
        Widget  UxBottomSep9;
        Widget  UxRightSep9;
        Widget  UxUpperRightSep9;
} _UxCattributesFormDialog;

#define separatorGadget1      UxAttributesFormDialogContext->UxseparatorGadget1
#define form9                   UxAttributesFormDialogContext->Uxform9
#define EncadreForm1            UxAttributesFormDialogContext->UxEncadreForm1
#define Titre1                  UxAttributesFormDialogContext->UxTitre1
#define UpperLeftSep1           UxAttributesFormDialogContext->UxUpperLeftSep1
#define LeftSep1                UxAttributesFormDialogContext->UxLeftSep1
#define BottomSep1              UxAttributesFormDialogContext->UxBottomSep1
#define RightSep1               UxAttributesFormDialogContext->UxRightSep1
#define UpperRightSep1          UxAttributesFormDialogContext->UxUpperRightSep1
#define EncadreForm7            UxAttributesFormDialogContext->UxEncadreForm7
#define Titre7                  UxAttributesFormDialogContext->UxTitre7
#define UpperLeftSep7           UxAttributesFormDialogContext->UxUpperLeftSep7
#define LeftSep7                UxAttributesFormDialogContext->UxLeftSep7
#define BottomSep7              UxAttributesFormDialogContext->UxBottomSep7
#define RightSep7               UxAttributesFormDialogContext->UxRightSep7
#define UpperRightSep7          UxAttributesFormDialogContext->UxUpperRightSep7
#define form2                   UxAttributesFormDialogContext->Uxform2
#define UpperLeftSep9           UxAttributesFormDialogContext->UxUpperLeftSep9
#define LeftSep9                UxAttributesFormDialogContext->UxLeftSep9
#define BottomSep9              UxAttributesFormDialogContext->UxBottomSep9
#define RightSep9               UxAttributesFormDialogContext->UxRightSep9
#define UpperRightSep9          UxAttributesFormDialogContext->UxUpperRightSep9

static _UxCattributesFormDialog *UxAttributesFormDialogContext;

Widget  attributesFormDialog;
Widget  ApplyAttributesBut;
Widget  ResetAttributesBut;
Widget  CloseAttributesBut;
Widget  LineWidthUpArrow;
Widget  LineWidthDownArrow;
Widget  LineWidthLabel;
Widget  LineWidthValueLabel;
Widget  ObjectRow;
Widget  BoxToggle;
Widget  HistoToggle;
Widget  YAxisToggle;
Widget  XAxisToggle;
Widget  FunctionToggle;
Widget  PictureToggle;
Widget  HatchStyleButton;
Widget  ColorButton;
Widget  ResetObjectBut;
Widget  TitleForm;
Widget  TitleLabel;
Widget  TitleText;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_attributesFormDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_attributesFormDialog()
{
        Widget  attributesFormDialog_shell;

        attributesFormDialog_shell = XtVaCreatePopupShell(
                        "attributesFormDialog_shell",
                        xmDialogShellWidgetClass, histoStyle,
                        XmNx, 676,
                        XmNy, 222,
                        XmNwidth, 284,
                        XmNheight, 413,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "attributesFormDialog",
                        NULL );

        attributesFormDialog = XtVaCreateWidget( "attributesFormDialog",
                        xmFormWidgetClass, attributesFormDialog_shell,
                        XmNautoUnmanage, FALSE,
                        XmNnoResize, TRUE,
                        RES_CONVERT( XmNdialogTitle,
                        "Object Attributes Settings" ),
                        XmNheight, 413,
                        XmNwidth, 284,
                        XmNunitType, XmPIXELS,
                        NULL );

  UxPutContext( attributesFormDialog, (char *) UxAttributesFormDialogContext );

        separatorGadget1 = XtVaCreateManagedWidget( "separatorGadget1",
                        xmSeparatorGadgetClass, attributesFormDialog,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 50,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 20,
                        XmNwidth, 1220,
                        XmNy, 280,
                        XmNx, 0,
                        NULL );

    UxPutContext( separatorGadget1, (char *) UxAttributesFormDialogContext );

        ApplyAttributesBut = XtVaCreateManagedWidget( "ApplyAttributesBut",
                        xmPushButtonGadgetClass, attributesFormDialog,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        RES_CONVERT( XmNlabelString, "Apply" ),
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 376,
                        XmNx, 13,
                        NULL );

   UxPutContext( ApplyAttributesBut, (char *) UxAttributesFormDialogContext );

        ResetAttributesBut = XtVaCreateManagedWidget( "ResetAttributesBut",
                        xmPushButtonGadgetClass, attributesFormDialog,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftWidget, ApplyAttributesBut,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 376,
                        XmNx, 00147,
                        NULL );

   UxPutContext( ResetAttributesBut, (char *) UxAttributesFormDialogContext );

        CloseAttributesBut = XtVaCreateManagedWidget( "CloseAttributesBut",
                        xmPushButtonGadgetClass, attributesFormDialog,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftWidget, ResetAttributesBut,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 376,
                        XmNx, 284,
                        NULL );

   UxPutContext( CloseAttributesBut, (char *) UxAttributesFormDialogContext );

        form9 = XtVaCreateManagedWidget( "form9",
                        xmFormWidgetClass, attributesFormDialog,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 220,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 36,
                        XmNwidth, 180,
                        XmNy, 246,
                        XmNx, 18,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form9, (char *) UxAttributesFormDialogContext );

        LineWidthUpArrow = XtVaCreateManagedWidget( "LineWidthUpArrow",
                        xmArrowButtonWidgetClass, form9,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowThickness, 0,
                        XmNarrowDirection, XmARROW_RIGHT,
                        RES_CONVERT( XmNforeground, "#9f9abe" ),
                        XmNheight, 36,
                        XmNwidth, 42,
                        XmNy, 0,
                        XmNx, 232,
                        NULL );

    UxPutContext( LineWidthUpArrow, (char *) UxAttributesFormDialogContext );

        LineWidthDownArrow = XtVaCreateManagedWidget( "LineWidthDownArrow",
                        xmArrowButtonWidgetClass, form9,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowThickness, 0,
                        XmNarrowDirection, XmARROW_LEFT,
                        RES_CONVERT( XmNforeground, "#9f9abe" ),
                        XmNheight, 36,
                        XmNwidth, 40,
                        XmNy, 0,
                        XmNx, 0,
                        NULL );

 UxPutContext( LineWidthDownArrow, (char *) UxAttributesFormDialogContext );

        LineWidthLabel = XtVaCreateManagedWidget( "LineWidthLabel",
                        xmLabelWidgetClass, form9,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrecomputeSize, FALSE,
                      RES_CONVERT( XmNlabelString, "Histogram Line Width :" ),
                        XmNalignment, XmALIGNMENT_CENTER,
                        XmNheight, 17,
                        XmNwidth, 165,
                        XmNy, 8,
                        XmNx, 39,
                        NULL );

        UxPutContext( LineWidthLabel, (char *) UxAttributesFormDialogContext );

        LineWidthValueLabel = XtVaCreateManagedWidget( "LineWidthValueLabel",
                        xmLabelGadgetClass, form9,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "XX" ),
                        XmNheight, 17,
                        XmNwidth, 30,
                        XmNy, 3,
                        XmNx, 203,
                        NULL );

  UxPutContext( LineWidthValueLabel, (char *) UxAttributesFormDialogContext );

        EncadreForm1 = XtVaCreateManagedWidget( "EncadreForm1",
                        xmFormWidgetClass, attributesFormDialog,
                        XmNheight, 210,
                        XmNwidth, 115,
                        XmNy, 5,
                        XmNx, 5,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( EncadreForm1, (char *) UxAttributesFormDialogContext );

        Titre1 = XtVaCreateManagedWidget( "Titre1",
                        xmLabelGadgetClass, EncadreForm1,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNmarginWidth, 0,
                        XmNrecomputeSize, TRUE,
                        RES_CONVERT( XmNlabelString, "Object" ),
                        XmNheight, 20,
                        XmNwidth, 60,
                        XmNy, 0,
                        XmNx, 10,
                        NULL );

        UxPutContext( Titre1, (char *) UxAttributesFormDialogContext );

        UpperLeftSep1 = XtVaCreateManagedWidget( "UpperLeftSep1",
                        xmSeparatorGadgetClass, EncadreForm1,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, Titre1,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 2,
                        XmNwidth, 99,
                        XmNy, 200,
                        XmNx, 87,
                        NULL );

        UxPutContext( UpperLeftSep1, (char *) UxAttributesFormDialogContext );

        LeftSep1 = XtVaCreateManagedWidget( "LeftSep1",
                        xmSeparatorGadgetClass, EncadreForm1,
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

        UxPutContext( LeftSep1, (char *) UxAttributesFormDialogContext );

        BottomSep1 = XtVaCreateManagedWidget( "BottomSep1",
                        xmSeparatorGadgetClass, EncadreForm1,
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

        UxPutContext( BottomSep1, (char *) UxAttributesFormDialogContext );

        RightSep1 = XtVaCreateManagedWidget( "RightSep1",
                        xmSeparatorGadgetClass, EncadreForm1,
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

        UxPutContext( RightSep1, (char *) UxAttributesFormDialogContext );

        UpperRightSep1 = XtVaCreateManagedWidget( "UpperRightSep1",
                        xmSeparatorGadgetClass, EncadreForm1,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftWidget, Titre1,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNheight, 2,
                        XmNwidth, 106,
                        XmNy, 200,
                        XmNx, 274,
                        NULL );

        UxPutContext( UpperRightSep1, (char *) UxAttributesFormDialogContext );

        ObjectRow = XtVaCreateManagedWidget( "ObjectRow",
                        xmRowColumnWidgetClass, EncadreForm1,
                        XmNtopOffset, 17,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNradioAlwaysOne, TRUE,
                        XmNborderWidth, 0,
                        XmNradioBehavior, TRUE,
                        XmNheight, 170,
                        XmNwidth, 123,
                        XmNy, 21,
                        XmNx, 269,
                        NULL );

        UxPutContext( ObjectRow, (char *) UxAttributesFormDialogContext );

        BoxToggle = XtVaCreateManagedWidget( "BoxToggle",
                        xmToggleButtonGadgetClass, ObjectRow,
                        XmNuserData, (XtPointer) "B",
                        XmNindicatorType, XmONE_OF_MANY,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Box" ),
                        XmNheight, 27,
                        XmNwidth, 113,
                        XmNy, 46,
                        XmNx, 276,
                        NULL );

        UxPutContext( BoxToggle, (char *) UxAttributesFormDialogContext );

        HistoToggle = XtVaCreateManagedWidget( "HistoToggle",
                        xmToggleButtonGadgetClass, ObjectRow,
                        XmNuserData, (XtPointer) "H",
                        XmNindicatorType, XmONE_OF_MANY,
                        RES_CONVERT( XmNlabelString, "Histogram" ),
                        XmNheight, 27,
                        XmNwidth, 113,
                        XmNy, 21,
                        XmNx, 275,
                        NULL );

        UxPutContext( HistoToggle, (char *) UxAttributesFormDialogContext );

        YAxisToggle = XtVaCreateManagedWidget( "YAxisToggle",
                        xmToggleButtonGadgetClass, ObjectRow,
                        XmNuserData, (XtPointer) "Y",
                        XmNindicatorType, XmONE_OF_MANY,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Y Axis" ),
                        XmNheight, 27,
                        XmNwidth, 113,
                        XmNy, 168,
                        XmNx, 275,
                        NULL );

        UxPutContext( YAxisToggle, (char *) UxAttributesFormDialogContext );

        XAxisToggle = XtVaCreateManagedWidget( "XAxisToggle",
                        xmToggleButtonGadgetClass, ObjectRow,
                        XmNuserData, (XtPointer) "X",
                        XmNindicatorType, XmONE_OF_MANY,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "X Axis" ),
                        XmNheight, 27,
                        XmNwidth, 113,
                        XmNy, 141,
                        XmNx, 277,
                        NULL );

        UxPutContext( XAxisToggle, (char *) UxAttributesFormDialogContext );

        FunctionToggle = XtVaCreateManagedWidget( "FunctionToggle",
                        xmToggleButtonGadgetClass, ObjectRow,
                        XmNuserData, (XtPointer) "F",
                        XmNindicatorType, XmONE_OF_MANY,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Function" ),
                        XmNheight, 27,
                        XmNwidth, 113,
                        XmNy, 112,
                        XmNx, 275,
                        NULL );

        UxPutContext( FunctionToggle, (char *) UxAttributesFormDialogContext );

        PictureToggle = XtVaCreateManagedWidget( "PictureToggle",
                        xmToggleButtonGadgetClass, ObjectRow,
                        XmNuserData, (XtPointer) "P",
                        XmNindicatorType, XmONE_OF_MANY,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Picture" ),
                        XmNheight, 27,
                        XmNwidth, 113,
                        XmNy, 78,
                        XmNx, 275,
                        NULL );

        UxPutContext( PictureToggle, (char *) UxAttributesFormDialogContext );

        EncadreForm7 = XtVaCreateManagedWidget( "EncadreForm7",
                        xmFormWidgetClass, attributesFormDialog,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNheight, 210,
                        XmNwidth, 148,
                        XmNy, 17,
                        XmNx, 128,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( EncadreForm7, (char *) UxAttributesFormDialogContext );

        Titre7 = XtVaCreateManagedWidget( "Titre7",
                        xmLabelGadgetClass, EncadreForm7,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNmarginWidth, 0,
                        XmNrecomputeSize, TRUE,
                        RES_CONVERT( XmNlabelString, "Action" ),
                        XmNheight, 20,
                        XmNwidth, 60,
                        XmNy, 0,
                        XmNx, 10,
                        NULL );

        UxPutContext( Titre7, (char *) UxAttributesFormDialogContext );

        UpperLeftSep7 = XtVaCreateManagedWidget( "UpperLeftSep7",
                        xmSeparatorGadgetClass, EncadreForm7,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, Titre7,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 2,
                        XmNwidth, 99,
                        XmNy, 200,
                        XmNx, 87,
                        NULL );

        UxPutContext( UpperLeftSep7, (char *) UxAttributesFormDialogContext );

        LeftSep7 = XtVaCreateManagedWidget( "LeftSep7",
                        xmSeparatorGadgetClass, EncadreForm7,
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

        UxPutContext( LeftSep7, (char *) UxAttributesFormDialogContext );

        BottomSep7 = XtVaCreateManagedWidget( "BottomSep7",
                        xmSeparatorGadgetClass, EncadreForm7,
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

        UxPutContext( BottomSep7, (char *) UxAttributesFormDialogContext );

        RightSep7 = XtVaCreateManagedWidget( "RightSep7",
                        xmSeparatorGadgetClass, EncadreForm7,
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

        UxPutContext( RightSep7, (char *) UxAttributesFormDialogContext );

        UpperRightSep7 = XtVaCreateManagedWidget( "UpperRightSep7",
                        xmSeparatorGadgetClass, EncadreForm7,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftWidget, Titre7,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNheight, 2,
                        XmNwidth, 106,
                        XmNy, 200,
                        XmNx, 274,
                        NULL );

        UxPutContext( UpperRightSep7, (char *) UxAttributesFormDialogContext );

        form2 = XtVaCreateManagedWidget( "form2",
                        xmFormWidgetClass, EncadreForm7,
                        XmNbottomOffset, 10,
                        XmNtopOffset, 15,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 110,
                        XmNwidth, 150,
                        XmNy, 110,
                        XmNx, 130,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form2, (char *) UxAttributesFormDialogContext );

        HatchStyleButton = XtVaCreateManagedWidget( "HatchStyleButton",
                        xmPushButtonGadgetClass, form2,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Hatch Style..." ),
                        XmNheight, 30,
                        XmNwidth, 126,
                        XmNy, 21,
                        XmNx, 11,
                        NULL );

    UxPutContext( HatchStyleButton, (char *) UxAttributesFormDialogContext );

        ColorButton = XtVaCreateManagedWidget( "ColorButton",
                        xmPushButtonGadgetClass, form2,
                        XmNtopWidget, HatchStyleButton,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Color..." ),
                        XmNheight, 30,
                        XmNwidth, 130,
                        XmNy, 78,
                        XmNx, 9,
                        NULL );

        UxPutContext( ColorButton, (char *) UxAttributesFormDialogContext );

        ResetObjectBut = XtVaCreateManagedWidget( "ResetObjectBut",
                        xmPushButtonGadgetClass, form2,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Reset Object" ),
                        XmNheight, 30,
                        XmNwidth, 130,
                        XmNy, 139,
                        XmNx, 10,
                        NULL );

        UxPutContext( ResetObjectBut, (char *) UxAttributesFormDialogContext );

        TitleForm = XtVaCreateManagedWidget( "TitleForm",
                        xmFormWidgetClass, attributesFormDialog,
                        XmNtopOffset, 260,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNheight, 73,
                        XmNwidth, 251,
                        XmNy, 285,
                        XmNx, 20,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( TitleForm, (char *) UxAttributesFormDialogContext );

        TitleLabel = XtVaCreateManagedWidget( "TitleLabel",
                        xmLabelGadgetClass, TitleForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNmarginWidth, 0,
                        RES_CONVERT( XmNlabelString, "Histogram Title" ),
                        XmNheight, 22,
                        XmNwidth, 120,
                        XmNy, 0,
                        XmNx, 10,
                        NULL );

        UxPutContext( TitleLabel, (char *) UxAttributesFormDialogContext );

        UpperLeftSep9 = XtVaCreateManagedWidget( "UpperLeftSep9",
                        xmSeparatorGadgetClass, TitleForm,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, TitleLabel,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 2,
                        XmNwidth, 10,
                        XmNy, 11,
                        XmNx, 0,
                        NULL );

        UxPutContext( UpperLeftSep9, (char *) UxAttributesFormDialogContext );

        LeftSep9 = XtVaCreateManagedWidget( "LeftSep9",
                        xmSeparatorGadgetClass, TitleForm,
                        XmNtopOffset, 11,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNorientation, XmVERTICAL,
                        XmNheight, 60,
                        XmNwidth, 2,
                        XmNy, 12,
                        XmNx, 0,
                        NULL );

        UxPutContext( LeftSep9, (char *) UxAttributesFormDialogContext );

        BottomSep9 = XtVaCreateManagedWidget( "BottomSep9",
                        xmSeparatorGadgetClass, TitleForm,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 2,
                        XmNwidth, 251,
                        XmNy, 70,
                        XmNx, 0,
                        NULL );

        UxPutContext( BottomSep9, (char *) UxAttributesFormDialogContext );

        RightSep9 = XtVaCreateManagedWidget( "RightSep9",
                        xmSeparatorGadgetClass, TitleForm,
                        XmNtopOffset, 11,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_NONE,
                        XmNbottomOffset, 1,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNorientation, XmVERTICAL,
                        XmNheight, 59,
                        XmNwidth, 2,
                        XmNy, 12,
                        XmNx, 249,
                        NULL );

        UxPutContext( RightSep9, (char *) UxAttributesFormDialogContext );

        UpperRightSep9 = XtVaCreateManagedWidget( "UpperRightSep9",
                        xmSeparatorGadgetClass, TitleForm,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftWidget, TitleLabel,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNheight, 2,
                        XmNwidth, 193,
                        XmNy, 11,
                        XmNx, 58,
                        NULL );

        UxPutContext( UpperRightSep9, (char *) UxAttributesFormDialogContext );

        TitleText = XtVaCreateManagedWidget( "TitleText",
                        xmTextWidgetClass, TitleForm,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 35,
                        XmNwidth, 160,
                        XmNy, 303,
                        XmNx, 65,
                        NULL );

        UxPutContext( TitleText, (char *) UxAttributesFormDialogContext );

        XtAddCallback( attributesFormDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxAttributesFormDialogContext );

        XtVaSetValues(attributesFormDialog,
                        XmNdefaultButton, ApplyAttributesBut,
                        NULL );



        return ( attributesFormDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_attributesFormDialog()
{
        Widget                  rtrn;
        _UxCattributesFormDialog *UxContext;

        UxAttributesFormDialogContext = UxContext =
    (_UxCattributesFormDialog *) XtMalloc( sizeof(_UxCattributesFormDialog) );

        rtrn = _Uxbuild_attributesFormDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_attributesFormDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_attributesFormDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

