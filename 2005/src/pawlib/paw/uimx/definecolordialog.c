/*
 * $Id: definecolordialog.c,v 1.1.1.1 1996/03/01 11:39:32 mclareni Exp $
 *
 * $Log: definecolordialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:32  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.05  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        defineColorDialog.c
        (Generated from interface file defineColorDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Frame.h>
#include <Xm/Scale.h>
#include <Xm/ArrowB.h>
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/Form.h>

/*******************************************************************************
        Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern swidget generalAttributesDialog;

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
        Widget  UxseparatorGadget5;
        Widget  Uxformedit;
        Widget  Uxtextcolorlabel;
        Widget  Uxform7;
        Widget  Uxform8;
        Widget  UxModifyForm;
        Widget  UxEncadreForm3;
        Widget  UxTitre3;
        Widget  UxUpperLeftSep3;
        Widget  UxLeftSep3;
        Widget  UxBottomSep3;
        Widget  UxRightSep3;
        Widget  UxUpperRightSep3;
        Widget  UxHLSForm;
        Widget  UxHueForm;
        Widget  Uxhue_red_min1;
        Widget  Uxhue_yellow1;
        Widget  Uxhue_green1;
        Widget  Uxhue_cyan1;
        Widget  Uxhue_blue1;
        Widget  Uxhue_magenta1;
        Widget  Uxhue_red_max1;
        Widget  UxSaturationForm;
        Widget  UxLightnessForm;
        Widget  UxEncadreForm4;
        Widget  UxTitre4;
        Widget  UxUpperLeftSep4;
        Widget  UxLeftSep4;
        Widget  UxBottomSep4;
        Widget  UxRightSep4;
        Widget  UxUpperRightSep4;
        Widget  UxRGBForm;
        Widget  UxRedForm;
        Widget  UxGreenForm;
        Widget  UxBlueForm;
} _UxCdefineColorDialog;

#define separatorGadget5        UxDefineColorDialogContext->UxseparatorGadget5
#define formedit                UxDefineColorDialogContext->Uxformedit
#define textcolorlabel          UxDefineColorDialogContext->Uxtextcolorlabel
#define form7                   UxDefineColorDialogContext->Uxform7
#define form8                   UxDefineColorDialogContext->Uxform8
#define ModifyForm              UxDefineColorDialogContext->UxModifyForm
#define EncadreForm3            UxDefineColorDialogContext->UxEncadreForm3
#define Titre3                  UxDefineColorDialogContext->UxTitre3
#define UpperLeftSep3           UxDefineColorDialogContext->UxUpperLeftSep3
#define LeftSep3                UxDefineColorDialogContext->UxLeftSep3
#define BottomSep3              UxDefineColorDialogContext->UxBottomSep3
#define RightSep3               UxDefineColorDialogContext->UxRightSep3
#define UpperRightSep3          UxDefineColorDialogContext->UxUpperRightSep3
#define HLSForm                 UxDefineColorDialogContext->UxHLSForm
#define HueForm                 UxDefineColorDialogContext->UxHueForm
#define hue_red_min1            UxDefineColorDialogContext->Uxhue_red_min1
#define hue_yellow1             UxDefineColorDialogContext->Uxhue_yellow1
#define hue_green1              UxDefineColorDialogContext->Uxhue_green1
#define hue_cyan1               UxDefineColorDialogContext->Uxhue_cyan1
#define hue_blue1               UxDefineColorDialogContext->Uxhue_blue1
#define hue_magenta1            UxDefineColorDialogContext->Uxhue_magenta1
#define hue_red_max1            UxDefineColorDialogContext->Uxhue_red_max1
#define SaturationForm          UxDefineColorDialogContext->UxSaturationForm
#define LightnessForm           UxDefineColorDialogContext->UxLightnessForm
#define EncadreForm4            UxDefineColorDialogContext->UxEncadreForm4
#define Titre4                  UxDefineColorDialogContext->UxTitre4
#define UpperLeftSep4           UxDefineColorDialogContext->UxUpperLeftSep4
#define LeftSep4                UxDefineColorDialogContext->UxLeftSep4
#define BottomSep4              UxDefineColorDialogContext->UxBottomSep4
#define RightSep4               UxDefineColorDialogContext->UxRightSep4
#define UpperRightSep4          UxDefineColorDialogContext->UxUpperRightSep4
#define RGBForm                 UxDefineColorDialogContext->UxRGBForm
#define RedForm                 UxDefineColorDialogContext->UxRedForm
#define GreenForm               UxDefineColorDialogContext->UxGreenForm
#define BlueForm                UxDefineColorDialogContext->UxBlueForm

static _UxCdefineColorDialog    *UxDefineColorDialogContext;

Widget  defineColorDialog;
Widget  ApplyColorBut;
Widget  ResetColorButton;
Widget  CloseColorBut;
Widget  colornumberlabel1;
Widget  ColorIndex;
Widget  DefColorUpArrow;
Widget  DefColorDownArrow;
Widget  NcolText;
Widget  NcolUpArrow;
Widget  NcolDownArrow;
Widget  HueScale;
Widget  SaturationScale;
Widget  SaturationMax;
Widget  SaturationMin;
Widget  LightnessScale;
Widget  LightnessMin;
Widget  LightnessMax;
Widget  RedScale;
Widget  RedMin;
Widget  RedMax;
Widget  GreenScale;
Widget  GreenMax;
Widget  GreenMin;
Widget  BlueScale;
Widget  BlueMin;
Widget  BlueMax;
Widget  DefineColorBut;
Widget  ResetColorBut;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_defineColorDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_defineColorDialog()
{
        Widget  defineColorDialog_shell;

        defineColorDialog_shell =
                        XtVaCreatePopupShell( "defineColorDialog_shell",
                        xmDialogShellWidgetClass, generalAttributesDialog,
                        XmNx, 707,
                        XmNy, 253,
                        XmNwidth, 329,
                        XmNheight, 420,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "defineColorDialog",
                        NULL );

        defineColorDialog = XtVaCreateWidget( "defineColorDialog",
                        xmFormWidgetClass, defineColorDialog_shell,
                        RES_CONVERT( XmNdialogTitle, "Define Color" ),
                        XmNautoUnmanage, FALSE,
                        XmNnoResize, TRUE,
                        XmNheight, 420,
                        XmNwidth, 329,
                        XmNunitType, XmPIXELS,
                        NULL );

        UxPutContext( defineColorDialog, (char *) UxDefineColorDialogContext );

        separatorGadget5 = XtVaCreateManagedWidget( "separatorGadget5",
                        xmSeparatorGadgetClass, defineColorDialog,
                        XmNseparatorType, XmSHADOW_ETCHED_IN,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 50,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 20,
                        XmNwidth, 329,
                        XmNy, 436,
                        XmNx, 3,
                        NULL );

        UxPutContext( separatorGadget5, (char *) UxDefineColorDialogContext );

        ApplyColorBut = XtVaCreateManagedWidget( "ApplyColorBut",
                        xmPushButtonGadgetClass, defineColorDialog,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 13,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Apply" ),
                        XmNheight, 30,
                        XmNwidth, 80,
                        XmNy, 367,
                        XmNx, 14,
                        NULL );

        UxPutContext( ApplyColorBut, (char *) UxDefineColorDialogContext );

        ResetColorButton = XtVaCreateManagedWidget( "ResetColorButton",
                        xmPushButtonGadgetClass, defineColorDialog,
                        XmNleftWidget, ApplyColorBut,
                        XmNleftOffset, 13,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        XmNheight, 30,
                        XmNwidth, 80,
                        XmNy, 367,
                        XmNx, 113,
                        NULL );

        UxPutContext( ResetColorButton, (char *) UxDefineColorDialogContext );

        CloseColorBut = XtVaCreateManagedWidget( "CloseColorBut",
                        xmPushButtonGadgetClass, defineColorDialog,
                        XmNleftWidget, ResetColorButton,
                        XmNleftOffset, 13,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNheight, 30,
                        XmNwidth, 80,
                        XmNy, 367,
                        XmNx, 220,
                        NULL );

        UxPutContext( CloseColorBut, (char *) UxDefineColorDialogContext );

        formedit = XtVaCreateManagedWidget( "formedit",
                        xmFormWidgetClass, defineColorDialog,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNheight, 94,
                        XmNwidth, 321,
                        XmNy, 55,
                        XmNx, 4,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( formedit, (char *) UxDefineColorDialogContext );

        textcolorlabel = XtVaCreateManagedWidget( "textcolorlabel",
                        xmLabelGadgetClass, formedit,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Current Color Index :" ),
                        XmNheight, 24,
                        XmNwidth, 173,
                        XmNy, 110,
                        XmNx, 0,
                        NULL );

        UxPutContext( textcolorlabel, (char *) UxDefineColorDialogContext );

        colornumberlabel1 = XtVaCreateManagedWidget( "colornumberlabel1",
                        xmLabelGadgetClass, formedit,
                        XmNtopWidget, textcolorlabel,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Color Number (NCOL) :" ),
                        XmNheight, 24,
                        XmNwidth, 173,
                        XmNy, 53,
                        XmNx, 0,
                        NULL );

        UxPutContext( colornumberlabel1, (char *) UxDefineColorDialogContext );

        form7 = XtVaCreateManagedWidget( "form7",
                        xmFormWidgetClass, formedit,
                        XmNleftWidget, textcolorlabel,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNheight, 32,
                        XmNwidth, 105,
                        XmNy, 0,
                        XmNx, 216,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form7, (char *) UxDefineColorDialogContext );

        ColorIndex = XtVaCreateManagedWidget( "ColorIndex",
                        xmTextWidgetClass, form7,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNvalue, "1",
                        XmNheight, 33,
                        XmNwidth, 46,
                        XmNy, -1,
                        XmNx, 53,
                        NULL );

        UxPutContext( ColorIndex, (char *) UxDefineColorDialogContext );

        DefColorUpArrow = XtVaCreateManagedWidget( "DefColorUpArrow",
                        xmArrowButtonWidgetClass, form7,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftWidget, ColorIndex,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowThickness, 0,
                        XmNarrowDirection, XmARROW_RIGHT,
                        RES_CONVERT( XmNforeground, "#9f9f50" ),
                        XmNheight, 35,
                        XmNwidth, 50,
                        XmNy, 0,
                        XmNx, 97,
                        NULL );

        UxPutContext( DefColorUpArrow, (char *) UxDefineColorDialogContext );

        DefColorDownArrow = XtVaCreateManagedWidget( "DefColorDownArrow",
                        xmArrowButtonWidgetClass, form7,
                        XmNrightWidget, ColorIndex,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowThickness, 0,
                        XmNarrowDirection, XmARROW_LEFT,
                        RES_CONVERT( XmNforeground, "#9f9f50" ),
                        XmNheight, 35,
                        XmNwidth, 49,
                        XmNy, -1,
                        XmNx, 7,
                        NULL );

        UxPutContext( DefColorDownArrow, (char *) UxDefineColorDialogContext );

        form8 = XtVaCreateManagedWidget( "form8",
                        xmFormWidgetClass, formedit,
                        XmNleftWidget, colornumberlabel1,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, form7,
                        XmNtopOffset, 15,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNheight, 31,
                        XmNwidth, 150,
                        XmNy, 47,
                        XmNx, 216,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form8, (char *) UxDefineColorDialogContext );

        NcolText = XtVaCreateManagedWidget( "NcolText",
                        xmTextWidgetClass, form8,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNvalue, "1",
                        XmNheight, 33,
                        XmNwidth, 46,
                        XmNy, -1,
                        XmNx, 53,
                        NULL );

        UxPutContext( NcolText, (char *) UxDefineColorDialogContext );

        NcolUpArrow = XtVaCreateManagedWidget( "NcolUpArrow",
                        xmArrowButtonWidgetClass, form8,
                        RES_CONVERT( XmNforeground, "#9f9abe" ),
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftWidget, NcolText,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowThickness, 0,
                        XmNarrowDirection, XmARROW_RIGHT,
                        XmNheight, 35,
                        XmNwidth, 50,
                        XmNy, 0,
                        XmNx, 97,
                        NULL );

        UxPutContext( NcolUpArrow, (char *) UxDefineColorDialogContext );

        NcolDownArrow = XtVaCreateManagedWidget( "NcolDownArrow",
                        xmArrowButtonWidgetClass, form8,
                        RES_CONVERT( XmNforeground, "#9f9abe" ),
                        XmNrightWidget, NcolText,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowThickness, 0,
                        XmNarrowDirection, XmARROW_LEFT,
                        XmNheight, 35,
                        XmNwidth, 49,
                        XmNy, -1,
                        XmNx, 7,
                        NULL );

        UxPutContext( NcolDownArrow, (char *) UxDefineColorDialogContext );

        ModifyForm = XtVaCreateManagedWidget( "ModifyForm",
                        xmFormWidgetClass, defineColorDialog,
                        XmNtopWidget, formedit,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 60,
                        XmNbottomAttachment, XmATTACH_NONE,
                        XmNy, 104,
                        XmNx, 5,
                        XmNheight, 248,
                        XmNwidth, 319,
                        XmNmarginWidth, 10,
                        XmNmarginHeight, 10,
                        XmNborderWidth, 0,
                        NULL );

        UxPutContext( ModifyForm, (char *) UxDefineColorDialogContext );

        EncadreForm3 = XtVaCreateManagedWidget( "EncadreForm3",
                        xmFormWidgetClass, ModifyForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 110,
                        XmNwidth, 317,
                        XmNy, 5,
                        XmNx, -3,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( EncadreForm3, (char *) UxDefineColorDialogContext );

        Titre3 = XtVaCreateManagedWidget( "Titre3",
                        xmLabelGadgetClass, EncadreForm3,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNmarginWidth, 0,
                        XmNrecomputeSize, TRUE,
             RES_CONVERT( XmNlabelString, "Hue / Saturation / Intensity" ),
                        XmNheight, 20,
                        XmNwidth, 220,
                        XmNy, 0,
                        XmNx, 10,
                        NULL );

        UxPutContext( Titre3, (char *) UxDefineColorDialogContext );

        UpperLeftSep3 = XtVaCreateManagedWidget( "UpperLeftSep3",
                        xmSeparatorGadgetClass, EncadreForm3,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, Titre3,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 2,
                        XmNwidth, 99,
                        XmNy, 200,
                        XmNx, 87,
                        NULL );

        UxPutContext( UpperLeftSep3, (char *) UxDefineColorDialogContext );

        LeftSep3 = XtVaCreateManagedWidget( "LeftSep3",
                        xmSeparatorGadgetClass, EncadreForm3,
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

        UxPutContext( LeftSep3, (char *) UxDefineColorDialogContext );

        BottomSep3 = XtVaCreateManagedWidget( "BottomSep3",
                        xmSeparatorGadgetClass, EncadreForm3,
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

        UxPutContext( BottomSep3, (char *) UxDefineColorDialogContext );

        RightSep3 = XtVaCreateManagedWidget( "RightSep3",
                        xmSeparatorGadgetClass, EncadreForm3,
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

        UxPutContext( RightSep3, (char *) UxDefineColorDialogContext );

        UpperRightSep3 = XtVaCreateManagedWidget( "UpperRightSep3",
                        xmSeparatorGadgetClass, EncadreForm3,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftWidget, Titre3,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNheight, 2,
                        XmNwidth, 106,
                        XmNy, 200,
                        XmNx, 274,
                        NULL );

        UxPutContext( UpperRightSep3, (char *) UxDefineColorDialogContext );

        HLSForm = XtVaCreateManagedWidget( "HLSForm",
                        xmFormWidgetClass, EncadreForm3,
                        XmNtopOffset, 18,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 2,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 2,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 2,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNy, 0,
                        XmNx, 0,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( HLSForm, (char *) UxDefineColorDialogContext );

        HueForm = XtVaCreateManagedWidget( "HueForm",
                        xmFormWidgetClass, HLSForm,
                        XmNtopOffset, 5,
                        XmNwidth, 200,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNborderWidth, 0,
                        XmNheight, 50,
                        NULL );

        UxPutContext( HueForm, (char *) UxDefineColorDialogContext );

        HueScale = XtVaCreateManagedWidget( "HueScale",
                        xmScaleWidgetClass, HueForm,
                        XmNy, 34,
                        XmNx, 0,
                        XmNtopPosition, 50,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_POSITION,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshowValue, FALSE,
                        XmNprocessingDirection, XmMAX_ON_RIGHT,
                        XmNorientation, XmHORIZONTAL,
                        XmNminimum, 0,
                        XmNmaximum, 359,
                        XmNhighlightThickness, 0,
                        XmNheight, 16,
                        NULL );

        UxPutContext( HueScale, (char *) UxDefineColorDialogContext );

        hue_red_min1 = XtVaCreateManagedWidget( "hue_red_min1",
                        xmFrameWidgetClass, HueForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightPosition, 10,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomWidget, HueScale,
                        XmNbottomPosition, 30,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNshadowType, XmSHADOW_IN,
                        XmNshadowThickness, 1,
                        RES_CONVERT( XmNbackground, "#c44" ),
                        NULL );

        UxPutContext( hue_red_min1, (char *) UxDefineColorDialogContext );

        hue_yellow1 = XtVaCreateManagedWidget( "hue_yellow1",
                        xmFrameWidgetClass, HueForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightPosition, 26,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 10,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomWidget, HueScale,
                        XmNbottomPosition, 30,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNshadowType, XmSHADOW_IN,
                        XmNshadowThickness, 1,
                        RES_CONVERT( XmNbackground, "#bb3" ),
                        XmNheight, 15,
                        XmNwidth, 50,
                        XmNy, 0,
                        XmNx, 30,
                        NULL );

        UxPutContext( hue_yellow1, (char *) UxDefineColorDialogContext );

        hue_green1 = XtVaCreateManagedWidget( "hue_green1",
                        xmFrameWidgetClass, HueForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightPosition, 42,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 26,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomWidget, HueScale,
                        XmNbottomPosition, 30,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNshadowType, XmSHADOW_IN,
                        XmNshadowThickness, 1,
                        RES_CONVERT( XmNbackground, "#3c3" ),
                        NULL );

        UxPutContext( hue_green1, (char *) UxDefineColorDialogContext );

        hue_cyan1 = XtVaCreateManagedWidget( "hue_cyan1",
                        xmFrameWidgetClass, HueForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightPosition, 58,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 42,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomWidget, HueScale,
                        XmNbottomPosition, 30,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNshadowType, XmSHADOW_IN,
                        XmNshadowThickness, 1,
                        RES_CONVERT( XmNbackground, "#3bb" ),
                        NULL );

        UxPutContext( hue_cyan1, (char *) UxDefineColorDialogContext );

        hue_blue1 = XtVaCreateManagedWidget( "hue_blue1",
                        xmFrameWidgetClass, HueForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightPosition, 74,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 58,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomWidget, HueScale,
                        XmNbottomPosition, 30,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNshadowType, XmSHADOW_IN,
                        XmNshadowThickness, 1,
                        RES_CONVERT( XmNbackground, "#33c" ),
                        NULL );

        UxPutContext( hue_blue1, (char *) UxDefineColorDialogContext );

        hue_magenta1 = XtVaCreateManagedWidget( "hue_magenta1",
                        xmFrameWidgetClass, HueForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightPosition, 90,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 74,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomWidget, HueScale,
                        XmNbottomPosition, 30,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNshadowType, XmSHADOW_IN,
                        XmNshadowThickness, 1,
                        RES_CONVERT( XmNbackground, "#b3b" ),
                        NULL );

        UxPutContext( hue_magenta1, (char *) UxDefineColorDialogContext );

        hue_red_max1 = XtVaCreateManagedWidget( "hue_red_max1",
                        xmFrameWidgetClass, HueForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftPosition, 90,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomWidget, HueScale,
                        XmNbottomPosition, 30,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNshadowType, XmSHADOW_IN,
                        XmNshadowThickness, 1,
                        RES_CONVERT( XmNbackground, "#c44" ),
                        XmNheight, 6,
                        XmNwidth, 86,
                        XmNy, 4,
                        XmNx, 7,
                        NULL );

        UxPutContext( hue_red_max1, (char *) UxDefineColorDialogContext );

        SaturationForm = XtVaCreateManagedWidget( "SaturationForm",
                        xmFormWidgetClass, HLSForm,
                        XmNx, 0,
                        XmNy, 87,
                        XmNheight, 15,
                        XmNtopWidget, HueForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNborderWidth, 0,
                        XmNwidth, 311,
                        NULL );

        UxPutContext( SaturationForm, (char *) UxDefineColorDialogContext );

        SaturationScale = XtVaCreateManagedWidget( "SaturationScale",
                        xmScaleWidgetClass, SaturationForm,
                        XmNleftPosition, 4,
                        XmNrightPosition, 96,
                        XmNheight, 15,
                        XmNtopOffset, 0,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomWidget, NULL,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshowValue, FALSE,
                        XmNprocessingDirection, XmMAX_ON_RIGHT,
                        XmNorientation, XmHORIZONTAL,
                        XmNminimum, 0,
                        XmNmaximum, 100,
                        XmNhighlightThickness, 0,
                        XmNdecimalPoints, 2,
                        NULL );

        UxPutContext( SaturationScale, (char *) UxDefineColorDialogContext );

        SaturationMax = XtVaCreateManagedWidget( "SaturationMax",
                        xmFrameWidgetClass, SaturationForm,
                        XmNwidth, 15,
                        XmNheight, 15,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftWidget, SaturationScale,
                        XmNleftOffset, 1,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowType, XmSHADOW_IN,
                        NULL );

        UxPutContext( SaturationMax, (char *) UxDefineColorDialogContext );

        SaturationMin = XtVaCreateManagedWidget( "SaturationMin",
                        xmFrameWidgetClass, SaturationForm,
                        XmNrightWidget, SaturationScale,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightOffset, 1,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowType, XmSHADOW_IN,
                        XmNheight, 15,
                        XmNwidth, 15,
                        NULL );

        UxPutContext( SaturationMin, (char *) UxDefineColorDialogContext );

        LightnessForm = XtVaCreateManagedWidget( "LightnessForm",
                        xmFormWidgetClass, HLSForm,
                        XmNheight, 15,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_NONE,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopWidget, SaturationForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNborderWidth, 0,
                        NULL );

        UxPutContext( LightnessForm, (char *) UxDefineColorDialogContext );

        LightnessScale = XtVaCreateManagedWidget( "LightnessScale",
                        xmScaleWidgetClass, LightnessForm,
                        XmNrightPosition, 96,
                        XmNleftPosition, 4,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNtopWidget, NULL,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNbottomWidget, NULL,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshowValue, FALSE,
                        XmNprocessingDirection, XmMAX_ON_RIGHT,
                        XmNorientation, XmHORIZONTAL,
                        XmNminimum, 0,
                        XmNmaximum, 100,
                        XmNhighlightThickness, 0,
                        XmNdecimalPoints, 2,
                        XmNheight, 16,
                        NULL );

        UxPutContext( LightnessScale, (char *) UxDefineColorDialogContext );

        LightnessMin = XtVaCreateManagedWidget( "LightnessMin",
                        xmFrameWidgetClass, LightnessForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, LightnessScale,
                        XmNleftOffset, 0,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowType, XmSHADOW_IN,
                        RES_CONVERT( XmNbackground, "#000" ),
                        XmNheight, 15,
                        XmNwidth, 15,
                        NULL );

        UxPutContext( LightnessMin, (char *) UxDefineColorDialogContext );

        LightnessMax = XtVaCreateManagedWidget( "LightnessMax",
                        xmFrameWidgetClass, LightnessForm,
                        XmNleftWidget, LightnessScale,
                        XmNleftOffset, 1,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNshadowType, XmSHADOW_IN,
                        XmNshadowThickness, 2,
                        RES_CONVERT( XmNbackground, "#fff" ),
                        XmNheight, 15,
                        XmNwidth, 15,
                        NULL );

        UxPutContext( LightnessMax, (char *) UxDefineColorDialogContext );

        EncadreForm4 = XtVaCreateManagedWidget( "EncadreForm4",
                        xmFormWidgetClass, ModifyForm,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopWidget, EncadreForm3,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNheight, 80,
                        XmNwidth, 326,
                        XmNy, 269,
                        XmNx, 1,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( EncadreForm4, (char *) UxDefineColorDialogContext );

        Titre4 = XtVaCreateManagedWidget( "Titre4",
                        xmLabelGadgetClass, EncadreForm4,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNmarginWidth, 0,
                        XmNrecomputeSize, TRUE,
                        RES_CONVERT( XmNlabelString, "Red / Green / Blue" ),
                        XmNheight, 20,
                        XmNwidth, 150,
                        XmNy, 0,
                        XmNx, 10,
                        NULL );

        UxPutContext( Titre4, (char *) UxDefineColorDialogContext );

        UpperLeftSep4 = XtVaCreateManagedWidget( "UpperLeftSep4",
                        xmSeparatorGadgetClass, EncadreForm4,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, Titre4,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 2,
                        XmNwidth, 99,
                        XmNy, 200,
                        XmNx, 87,
                        NULL );

        UxPutContext( UpperLeftSep4, (char *) UxDefineColorDialogContext );

        LeftSep4 = XtVaCreateManagedWidget( "LeftSep4",
                        xmSeparatorGadgetClass, EncadreForm4,
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

        UxPutContext( LeftSep4, (char *) UxDefineColorDialogContext );

        BottomSep4 = XtVaCreateManagedWidget( "BottomSep4",
                        xmSeparatorGadgetClass, EncadreForm4,
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

        UxPutContext( BottomSep4, (char *) UxDefineColorDialogContext );

        RightSep4 = XtVaCreateManagedWidget( "RightSep4",
                        xmSeparatorGadgetClass, EncadreForm4,
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

        UxPutContext( RightSep4, (char *) UxDefineColorDialogContext );

        UpperRightSep4 = XtVaCreateManagedWidget( "UpperRightSep4",
                        xmSeparatorGadgetClass, EncadreForm4,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftWidget, Titre4,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNheight, 2,
                        XmNwidth, 106,
                        XmNy, 200,
                        XmNx, 274,
                        NULL );

        UxPutContext( UpperRightSep4, (char *) UxDefineColorDialogContext );

        RGBForm = XtVaCreateManagedWidget( "RGBForm",
                        xmFormWidgetClass, EncadreForm4,
                        XmNtopOffset, 20,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 2,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 2,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 2,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNborderWidth, 0,
                        XmNheight, 86,
                        XmNwidth, 326,
                        XmNy, 115,
                        XmNx, 0,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( RGBForm, (char *) UxDefineColorDialogContext );

        RedForm = XtVaCreateManagedWidget( "RedForm",
                        xmFormWidgetClass, RGBForm,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNallowOverlap, FALSE,
                        XmNborderWidth, 0,
                        XmNheight, 15,
                        NULL );

        UxPutContext( RedForm, (char *) UxDefineColorDialogContext );

        RedScale = XtVaCreateManagedWidget( "RedScale",
                        xmScaleWidgetClass, RedForm,
                        XmNrightPosition, 96,
                        XmNleftPosition, 4,
                        RES_CONVERT( XmNbackground, "#ff0000" ),
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshowValue, FALSE,
                        XmNprocessingDirection, XmMAX_ON_RIGHT,
                        XmNorientation, XmHORIZONTAL,
                        XmNminimum, 0,
                        XmNmaximum, 65535,
                        XmNhighlightThickness, 0,
                        XmNheight, 15,
                        NULL );

        UxPutContext( RedScale, (char *) UxDefineColorDialogContext );

        RedMin = XtVaCreateManagedWidget( "RedMin",
                        xmFrameWidgetClass, RedForm,
                        XmNrightWidget, RedScale,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 1,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowType, XmSHADOW_IN,
                        XmNheight, 15,
                        XmNwidth, 15,
                        NULL );

        UxPutContext( RedMin, (char *) UxDefineColorDialogContext );

        RedMax = XtVaCreateManagedWidget( "RedMax",
                        xmFrameWidgetClass, RedForm,
                        XmNleftWidget, RedScale,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 1,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowType, XmSHADOW_IN,
                        XmNheight, 15,
                        XmNwidth, 15,
                        NULL );

        UxPutContext( RedMax, (char *) UxDefineColorDialogContext );

        GreenForm = XtVaCreateManagedWidget( "GreenForm",
                        xmFormWidgetClass, RGBForm,
                        XmNtopOffset, 0,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopWidget, RedForm,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNborderWidth, 0,
                        XmNheight, 15,
                        NULL );

        UxPutContext( GreenForm, (char *) UxDefineColorDialogContext );

        GreenScale = XtVaCreateManagedWidget( "GreenScale",
                        xmScaleWidgetClass, GreenForm,
                        RES_CONVERT( XmNbackground, "#00ff00" ),
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightPosition, 96,
                        XmNleftPosition, 4,
                        XmNtopOffset, 0,
                        XmNrightWidget, NULL,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftWidget, NULL,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshowValue, FALSE,
                        XmNprocessingDirection, XmMAX_ON_RIGHT,
                        XmNorientation, XmHORIZONTAL,
                        XmNminimum, 0,
                        XmNmaximum, 65535,
                        XmNhighlightThickness, 0,
                        XmNheight, 15,
                        NULL );

        UxPutContext( GreenScale, (char *) UxDefineColorDialogContext );

        GreenMax = XtVaCreateManagedWidget( "GreenMax",
                        xmFrameWidgetClass, GreenForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftWidget, GreenScale,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 1,
                        XmNrightWidget, NULL,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowType, XmSHADOW_IN,
                        XmNheight, 15,
                        XmNwidth, 15,
                        NULL );

        UxPutContext( GreenMax, (char *) UxDefineColorDialogContext );

        GreenMin = XtVaCreateManagedWidget( "GreenMin",
                        xmFrameWidgetClass, GreenForm,
                        XmNrightWidget, GreenScale,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 1,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowType, XmSHADOW_IN,
                        XmNheight, 15,
                        XmNwidth, 15,
                        NULL );

        UxPutContext( GreenMin, (char *) UxDefineColorDialogContext );

        BlueForm = XtVaCreateManagedWidget( "BlueForm",
                        xmFormWidgetClass, RGBForm,
                        XmNtopOffset, 0,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_NONE,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopWidget, GreenForm,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNborderWidth, 0,
                        XmNheight, 15,
                        NULL );

        UxPutContext( BlueForm, (char *) UxDefineColorDialogContext );

        BlueScale = XtVaCreateManagedWidget( "BlueScale",
                        xmScaleWidgetClass, BlueForm,
                        RES_CONVERT( XmNbackground, "#0000ff" ),
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightPosition, 96,
                        XmNleftPosition, 4,
                        XmNtopOffset, 0,
                        XmNrightWidget, NULL,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftWidget, NULL,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshowValue, FALSE,
                        XmNprocessingDirection, XmMAX_ON_RIGHT,
                        XmNorientation, XmHORIZONTAL,
                        XmNminimum, 0,
                        XmNmaximum, 65535,
                        XmNhighlightThickness, 0,
                        XmNheight, 16,
                        NULL );

        UxPutContext( BlueScale, (char *) UxDefineColorDialogContext );

        BlueMin = XtVaCreateManagedWidget( "BlueMin",
                        xmFrameWidgetClass, BlueForm,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNwidth, 15,
                        XmNtopOffset, 0,
                        XmNrightWidget, BlueScale,
                        XmNrightOffset, 1,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowType, XmSHADOW_IN,
                        XmNheight, 15,
                        NULL );

        UxPutContext( BlueMin, (char *) UxDefineColorDialogContext );

        BlueMax = XtVaCreateManagedWidget( "BlueMax",
                        xmFrameWidgetClass, BlueForm,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftWidget, BlueScale,
                        XmNleftOffset, 1,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowType, XmSHADOW_IN,
                        RES_CONVERT( XmNborderColor, "#11f" ),
                        XmNheight, 15,
                        XmNwidth, 15,
                        NULL );

        UxPutContext( BlueMax, (char *) UxDefineColorDialogContext );

        DefineColorBut = XtVaCreateManagedWidget( "DefineColorBut",
                        xmPushButtonGadgetClass, ModifyForm,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Define Color" ),
                        XmNheight, 26,
                        XmNwidth, 130,
                        XmNy, 213,
                        XmNx, 28,
                        NULL );

        UxPutContext( DefineColorBut, (char *) UxDefineColorDialogContext );

        ResetColorBut = XtVaCreateManagedWidget( "ResetColorBut",
                        xmPushButtonGadgetClass, ModifyForm,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightOffset, 20,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 20,
                        XmNleftAttachment, XmATTACH_NONE,
                        RES_CONVERT( XmNlabelString, "Reset Color" ),
                        XmNheight, 26,
                        XmNwidth, 130,
                        XmNy, 82,
                        XmNx, 21,
                        NULL );

        UxPutContext( ResetColorBut, (char *) UxDefineColorDialogContext );

        XtAddCallback( defineColorDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxDefineColorDialogContext );

        XtVaSetValues(defineColorDialog,
                        XmNdefaultButton, ApplyColorBut,
                        NULL );



        return ( defineColorDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_defineColorDialog()
{
        Widget                  rtrn;
        _UxCdefineColorDialog   *UxContext;

        UxDefineColorDialogContext = UxContext =
            (_UxCdefineColorDialog *) XtMalloc( sizeof(_UxCdefineColorDialog) );

        rtrn = _Uxbuild_defineColorDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_defineColorDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_defineColorDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

