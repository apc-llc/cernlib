/*
 * $Id: colordialog.c,v 1.1.1.1 1996/03/01 11:39:32 mclareni Exp $
 *
 * $Log: colordialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:32  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.05  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        colorDialog.c
        (Generated from interface file colorDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/LabelG.h>
#include <Xm/ArrowB.h>
#include <Xm/Text.h>
#include <Xm/Form.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/Form.h>

/*******************************************************************************
        Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern swidget attributesFormDialog;

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
        Widget  UxseparatorGadget7;
        Widget  Uxframe3;
        Widget  Uxform6;
        Widget  Uxform5;
        Widget  Uxtextcolorlabel1;
} _UxCcolorDialog;

#define separatorGadget7        UxColorDialogContext->UxseparatorGadget7
#define frame3                  UxColorDialogContext->Uxframe3
#define form6                   UxColorDialogContext->Uxform6
#define form5                   UxColorDialogContext->Uxform5
#define textcolorlabel1         UxColorDialogContext->Uxtextcolorlabel1

static _UxCcolorDialog  *UxColorDialogContext;

Widget  colorDialog;
Widget  ApplyAttColorBut;
Widget  ResetAttColorButton;
Widget  CloseAttColorBut;
Widget  TargetRow;
Widget  SurfaceToggle;
Widget  StatToggle;
Widget  ZoneToggle;
Widget  ContourToggle;
Widget  AttColorText;
Widget  ColorUpArrow;
Widget  ColorDownArrow;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_colorDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_colorDialog()
{
        Widget  colorDialog_shell;

        colorDialog_shell = XtVaCreatePopupShell( "colorDialog_shell",
                        xmDialogShellWidgetClass, attributesFormDialog,
                        XmNx, 365,
                        XmNy, 317,
                        XmNwidth, 333,
                        XmNheight, 200,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "colorDialog",
                        NULL );

        colorDialog = XtVaCreateWidget( "colorDialog",
                        xmFormWidgetClass, colorDialog_shell,
                        RES_CONVERT( XmNdialogTitle, "Object Color Setting" ),
                        XmNautoUnmanage, FALSE,
                        XmNnoResize, TRUE,
                        XmNheight, 200,
                        XmNwidth, 333,
                        XmNunitType, XmPIXELS,
                        NULL );

        UxPutContext( colorDialog, (char *) UxColorDialogContext );

        separatorGadget7 = XtVaCreateManagedWidget( "separatorGadget7",
                        xmSeparatorGadgetClass, colorDialog,
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

        UxPutContext( separatorGadget7, (char *) UxColorDialogContext );

        ApplyAttColorBut = XtVaCreateManagedWidget( "ApplyAttColorBut",
                        xmPushButtonGadgetClass, colorDialog,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Apply" ),
                        XmNheight, 30,
                        XmNwidth, 90,
                        XmNy, 376,
                        XmNx, 13,
                        NULL );

        UxPutContext( ApplyAttColorBut, (char *) UxColorDialogContext );

        ResetAttColorButton = XtVaCreateManagedWidget( "ResetAttColorButton",
                        xmPushButtonGadgetClass, colorDialog,
                        XmNleftWidget, ApplyAttColorBut,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        XmNheight, 30,
                        XmNwidth, 86,
                        XmNy, 376,
                        XmNx, 147,
                        NULL );

        UxPutContext( ResetAttColorButton, (char *) UxColorDialogContext );

        CloseAttColorBut = XtVaCreateManagedWidget( "CloseAttColorBut",
                        xmPushButtonGadgetClass, colorDialog,
                        XmNleftWidget, ResetAttColorButton,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNheight, 30,
                        XmNwidth, 90,
                        XmNy, 376,
                        XmNx, 284,
                        NULL );

        UxPutContext( CloseAttColorBut, (char *) UxColorDialogContext );

        frame3 = XtVaCreateManagedWidget( "frame3",
                        xmFrameWidgetClass, colorDialog,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 78,
                        XmNwidth, 317,
                        XmNy, 100,
                        XmNx, 9,
                        NULL );

        UxPutContext( frame3, (char *) UxColorDialogContext );

        TargetRow = XtVaCreateManagedWidget( "TargetRow",
                        xmRowColumnWidgetClass, frame3,
                        XmNheight, 66,
                        XmNradioBehavior, TRUE,
                        XmNspacing, 0,
                        XmNpacking, XmPACK_NONE,
                        XmNorientation, XmHORIZONTAL,
                        XmNmarginWidth, 0,
                        XmNmarginHeight, 0,
                        XmNwidth, 313,
                        XmNy, 10,
                        XmNx, 2,
                        NULL );

        UxPutContext( TargetRow, (char *) UxColorDialogContext );

        SurfaceToggle = XtVaCreateManagedWidget( "SurfaceToggle",
                        xmToggleButtonGadgetClass, TargetRow,
                        RES_CONVERT( XmNlabelString, "Surface" ),
                        XmNheight, 1,
                        XmNwidth, 90,
                        XmNy, 7,
                        XmNx, 9,
                        NULL );

        UxPutContext( SurfaceToggle, (char *) UxColorDialogContext );

        StatToggle = XtVaCreateManagedWidget( "StatToggle",
                        xmToggleButtonGadgetClass, TargetRow,
                        RES_CONVERT( XmNlabelString, "Statistic box shadow" ),
                        XmNheight, 29,
                        XmNwidth, 154,
                        XmNy, 7,
                        XmNx, 130,
                        NULL );

        UxPutContext( StatToggle, (char *) UxColorDialogContext );

        ZoneToggle = XtVaCreateManagedWidget( "ZoneToggle",
                        xmToggleButtonGadgetClass, TargetRow,
                        RES_CONVERT( XmNlabelString, "Zone box shadow" ),
                        XmNheight, 20,
                        XmNwidth, 123,
                        XmNy, 41,
                        XmNx, 130,
                        NULL );

        UxPutContext( ZoneToggle, (char *) UxColorDialogContext );

        ContourToggle = XtVaCreateManagedWidget( "ContourToggle",
                        xmToggleButtonGadgetClass, TargetRow,
                        RES_CONVERT( XmNlabelString, "Contour" ),
                        XmNheight, 29,
                        XmNwidth, 188,
                        XmNy, 41,
                        XmNx, 9,
                        NULL );

        UxPutContext( ContourToggle, (char *) UxColorDialogContext );

        form6 = XtVaCreateManagedWidget( "form6",
                        xmFormWidgetClass, colorDialog,
                        XmNheight, 34,
                        XmNwidth, 260,
                        XmNy, 95,
                        XmNx, 7,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form6, (char *) UxColorDialogContext );

        form5 = XtVaCreateManagedWidget( "form5",
                        xmFormWidgetClass, form6,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 34,
                        XmNwidth, 158,
                        XmNy, 94,
                        XmNx, 112,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form5, (char *) UxColorDialogContext );

        AttColorText = XtVaCreateManagedWidget( "AttColorText",
                        xmTextWidgetClass, form5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNvalue, "1",
                        XmNheight, 33,
                        XmNwidth, 46,
                        XmNy, -1,
                        XmNx, 53,
                        NULL );

        UxPutContext( AttColorText, (char *) UxColorDialogContext );

        ColorUpArrow = XtVaCreateManagedWidget( "ColorUpArrow",
                        xmArrowButtonWidgetClass, form5,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftWidget, AttColorText,
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

        UxPutContext( ColorUpArrow, (char *) UxColorDialogContext );

        ColorDownArrow = XtVaCreateManagedWidget( "ColorDownArrow",
                        xmArrowButtonWidgetClass, form5,
                        XmNrightWidget, AttColorText,
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

        UxPutContext( ColorDownArrow, (char *) UxColorDialogContext );

        textcolorlabel1 = XtVaCreateManagedWidget( "textcolorlabel1",
                        xmLabelGadgetClass, form6,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Color Index :" ),
                        XmNheight, 24,
                        XmNwidth, 100,
                        XmNy, 96,
                        XmNx, 21,
                        NULL );

        UxPutContext( textcolorlabel1, (char *) UxColorDialogContext );

        XtAddCallback( colorDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxColorDialogContext );

        XtVaSetValues(colorDialog,
                        XmNdefaultButton, ApplyAttColorBut,
                        NULL );



        return ( colorDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_colorDialog()
{
        Widget                  rtrn;
        _UxCcolorDialog         *UxContext;

        UxColorDialogContext = UxContext =
                (_UxCcolorDialog *) XtMalloc( sizeof(_UxCcolorDialog) );

        rtrn = _Uxbuild_colorDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_colorDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_colorDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

