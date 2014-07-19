/*
 * $Id: axisformdialog.c,v 1.1.1.1 1996/03/01 11:39:32 mclareni Exp $
 *
 * $Log: axisformdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:32  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.05  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        axisFormDialog.c
        (Generated from interface file axisFormDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Scale.h>
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
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
        Widget  UxseparatorGadget10;
        Widget  Uxframe1;
        Widget  UxrowColumn8;
        Widget  Uxform3;
        Widget  Uxframe2;
        Widget  UxlabelGadget2;
        Widget  UxlabelGadget3;
        Widget  UxlabelGadget4;
} _UxCaxisFormDialog;

#define separatorGadget10       UxAxisFormDialogContext->UxseparatorGadget10
#define frame1                  UxAxisFormDialogContext->Uxframe1
#define rowColumn8              UxAxisFormDialogContext->UxrowColumn8
#define form3                   UxAxisFormDialogContext->Uxform3
#define frame2                  UxAxisFormDialogContext->Uxframe2
#define labelGadget2            UxAxisFormDialogContext->UxlabelGadget2
#define labelGadget3            UxAxisFormDialogContext->UxlabelGadget3
#define labelGadget4            UxAxisFormDialogContext->UxlabelGadget4

static _UxCaxisFormDialog       *UxAxisFormDialogContext;

Widget  axisFormDialog;
Widget  AxisApplyButton;
Widget  AxisResetButton;
Widget  AxisCloseButton;
Widget  XAxisToggleButtonGadget;
Widget  YAxisToggleButtonGadget;
Widget  ZAxisToggleButtonGadget;
Widget  labelPushButtonGadget;
Widget  logtoggleButtonGadget;
Widget  gridtoggleButtonGadget;
Widget  orienPushButtonGadget;
Widget  ticktoggleButtonGadget;
Widget  DivAxisform;
Widget  AxisPrimaryText;
Widget  AxisPrimaryScale;
Widget  AxisSecondaryText;
Widget  AxisSecondaryScale;
Widget  AxisTertiaryText;
Widget  AxisTertiaryScale;
Widget  NoOptimiToggleButtonGadget;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_axisFormDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_axisFormDialog()
{
        Widget  axisFormDialog_shell;

        axisFormDialog_shell = XtVaCreatePopupShell( "axisFormDialog_shell",
                        xmDialogShellWidgetClass, histoStyle,
                        XmNx, 79,
                        XmNy, 154,
                        XmNwidth, 343,
                        XmNheight, 501,
                        XmNtitle, "axisFormDialog",
                        NULL );

        axisFormDialog = XtVaCreateWidget( "axisFormDialog",
                        xmFormWidgetClass, axisFormDialog_shell,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNautoUnmanage, FALSE,
                        XmNfractionBase, 100,
                        XmNnoResize, TRUE,
                        RES_CONVERT( XmNdialogTitle, "Axis Settings" ),
                        XmNheight, 501,
                        XmNwidth, 343,
                        NULL );

        UxPutContext( axisFormDialog, (char *) UxAxisFormDialogContext );

        AxisApplyButton = XtVaCreateManagedWidget( "AxisApplyButton",
                        xmPushButtonGadgetClass, axisFormDialog,
                        XmNrightPosition, 25,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNshowAsDefault, 1,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Apply" ),
                        XmNheight, 30,
                        XmNwidth, 90,
                        XmNy, 565,
                        XmNx, 5,
                        NULL );

        UxPutContext( AxisApplyButton, (char *) UxAxisFormDialogContext );

        AxisResetButton = XtVaCreateManagedWidget( "AxisResetButton",
                        xmPushButtonGadgetClass, axisFormDialog,
                        XmNrightPosition, 60,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 40,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        XmNheight, 30,
                        XmNwidth, 90,
                        XmNy, 565,
                        XmNx, 125,
                        NULL );

        UxPutContext( AxisResetButton, (char *) UxAxisFormDialogContext );

        separatorGadget10 = XtVaCreateManagedWidget( "separatorGadget10",
                        xmSeparatorGadgetClass, axisFormDialog,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 50,
                        XmNbottomAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( separatorGadget10, (char *) UxAxisFormDialogContext );

        AxisCloseButton = XtVaCreateManagedWidget( "AxisCloseButton",
                        xmPushButtonGadgetClass, axisFormDialog,
                        XmNrightPosition, 95,
                        XmNleftPosition, 75,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_POSITION,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNheight, 30,
                        XmNwidth, 90,
                        XmNy, 565,
                        XmNx, 245,
                        NULL );

        UxPutContext( AxisCloseButton, (char *) UxAxisFormDialogContext );

        frame1 = XtVaCreateManagedWidget( "frame1",
                        xmFrameWidgetClass, axisFormDialog,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( frame1, (char *) UxAxisFormDialogContext );

        rowColumn8 = XtVaCreateManagedWidget( "rowColumn8",
                        xmRowColumnWidgetClass, frame1,
                        XmNresizeWidth, FALSE,
                        XmNmarginWidth, 6,
                        XmNorientation, XmHORIZONTAL,
                        XmNspacing, 45,
                        XmNradioBehavior, TRUE,
                        NULL );

        UxPutContext( rowColumn8, (char *) UxAxisFormDialogContext );

XAxisToggleButtonGadget = XtVaCreateManagedWidget( "XAxisToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn8,
                        XmNset, TRUE,
                        RES_CONVERT( XmNlabelString, "X Axis" ),
                        XmNheight, 35,
                        XmNwidth, 60,
                        XmNy, 5,
                        XmNx, 0,
                        NULL );

UxPutContext( XAxisToggleButtonGadget, (char *) UxAxisFormDialogContext );

YAxisToggleButtonGadget = XtVaCreateManagedWidget( "YAxisToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn8,
                        RES_CONVERT( XmNlabelString, "Y Axis" ),
                        XmNheight, 29,
                        XmNwidth, 60,
                        XmNy, 32,
                        XmNx, 3,
                        NULL );

UxPutContext( YAxisToggleButtonGadget, (char *) UxAxisFormDialogContext );

ZAxisToggleButtonGadget = XtVaCreateManagedWidget( "ZAxisToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn8,
                        RES_CONVERT( XmNlabelString, "Z Axis" ),
                        XmNheight, 35,
                        XmNwidth, 245,
                        XmNy, 45,
                        XmNx, 13,
                        NULL );

UxPutContext( ZAxisToggleButtonGadget, (char *) UxAxisFormDialogContext );

        form3 = XtVaCreateManagedWidget( "form3",
                        xmFormWidgetClass, axisFormDialog,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopWidget, frame1,
                        XmNtopOffset, 15,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form3, (char *) UxAxisFormDialogContext );

labelPushButtonGadget = XtVaCreateManagedWidget( "labelPushButtonGadget",
                        xmPushButtonGadgetClass, form3,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNwidth, 110,
                        XmNalignment, XmALIGNMENT_CENTER,
                        XmNy, 90,
                        XmNx, 145,
                        RES_CONVERT( XmNlabelString, "Labels..." ),
                        XmNheight, 30,
                        NULL );

        UxPutContext( labelPushButtonGadget, (char *) UxAxisFormDialogContext );

logtoggleButtonGadget = XtVaCreateManagedWidget( "logtoggleButtonGadget",
                        xmToggleButtonGadgetClass, form3,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Logarithmic" ),
                        XmNheight, 25,
                        NULL );

        UxPutContext( logtoggleButtonGadget, (char *) UxAxisFormDialogContext );

gridtoggleButtonGadget = XtVaCreateManagedWidget( "gridtoggleButtonGadget",
                        xmToggleButtonGadgetClass, form3,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        XmNwidth, 100,
                        RES_CONVERT( XmNlabelString, "Grid" ),
                        XmNheight, 25,
                        NULL );

UxPutContext( gridtoggleButtonGadget, (char *) UxAxisFormDialogContext );

orienPushButtonGadget = XtVaCreateManagedWidget( "orienPushButtonGadget",
                        xmPushButtonGadgetClass, form3,
                        XmNy, 35,
                        XmNx, -5,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopWidget, labelPushButtonGadget,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNwidth, 110,
                        RES_CONVERT( XmNlabelString, "Orientation..." ),
                        XmNalignment, XmALIGNMENT_CENTER,
                        XmNheight, 30,
                        NULL );

        UxPutContext( orienPushButtonGadget, (char *) UxAxisFormDialogContext );

ticktoggleButtonGadget = XtVaCreateManagedWidget( "ticktoggleButtonGadget",
                        xmToggleButtonGadgetClass, form3,
                        XmNtopOffset, 20,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        XmNwidth, 100,
                        RES_CONVERT( XmNlabelString, "Ticks" ),
                        XmNheight, 25,
                        NULL );

UxPutContext( ticktoggleButtonGadget, (char *) UxAxisFormDialogContext );

        frame2 = XtVaCreateManagedWidget( "frame2",
                        xmFrameWidgetClass, axisFormDialog,
                        XmNbottomWidget, separatorGadget10,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( frame2, (char *) UxAxisFormDialogContext );

        DivAxisform = XtVaCreateManagedWidget( "DivAxisform",
                        xmFormWidgetClass, frame2,
                        XmNfractionBase, 100,
                        XmNheight, 287,
                        XmNwidth, 329,
                        XmNy, 5,
                        XmNx, 2,
                        NULL );

        UxPutContext( DivAxisform, (char *) UxAxisFormDialogContext );

        labelGadget2 = XtVaCreateManagedWidget( "labelGadget2",
                        xmLabelGadgetClass, DivAxisform,
                        XmNx, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 45,
                        XmNwidth, 210,
                        XmNy, 14,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
RES_CONVERT( XmNlabelString, "Number of Primary Divisions" ),
                        NULL );

        UxPutContext( labelGadget2, (char *) UxAxisFormDialogContext );

        AxisPrimaryText = XtVaCreateManagedWidget( "AxisPrimaryText",
                        xmTextWidgetClass, DivAxisform,
                        XmNcolumns, 7,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopPosition, 20,
                        XmNtopOffset, 40,
                        XmNheight, 30,
                        XmNvalue, "10",
                        XmNmarginHeight, 3,
                        NULL );

        UxPutContext( AxisPrimaryText, (char *) UxAxisFormDialogContext );

        AxisPrimaryScale = XtVaCreateManagedWidget( "AxisPrimaryScale",
                        xmScaleWidgetClass, DivAxisform,
                        XmNmaximum, 20,
                        XmNshowValue, TRUE,
                        XmNnavigationType, XmTAB_GROUP,
                        RES_CONVERT( XmNtitleString, "" ),
                        XmNresizable, FALSE,
                        XmNminimum, 1,
                        XmNvalue, 10,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        XmNtopWidget, AxisPrimaryText,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNorientation, XmHORIZONTAL,
                        NULL );

        UxPutContext( AxisPrimaryScale, (char *) UxAxisFormDialogContext );

        labelGadget3 = XtVaCreateManagedWidget( "labelGadget3",
                        xmLabelGadgetClass, DivAxisform,
                        XmNy, 14,
                        XmNx, 0,
                        XmNwidth, 230,
                        XmNheight, 21,
                        XmNtopWidget, AxisPrimaryScale,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
RES_CONVERT( XmNlabelString, "Number of Secondary Divisions" ),
                        NULL );

        UxPutContext( labelGadget3, (char *) UxAxisFormDialogContext );

        AxisSecondaryText = XtVaCreateManagedWidget( "AxisSecondaryText",
                        xmTextWidgetClass, DivAxisform,
                        XmNcolumns, 7,
                        XmNtopWidget, AxisPrimaryScale,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNvalue, "5",
                        XmNmarginHeight, 3,
                        XmNheight, 30,
                        NULL );

        UxPutContext( AxisSecondaryText, (char *) UxAxisFormDialogContext );

        AxisSecondaryScale = XtVaCreateManagedWidget( "AxisSecondaryScale",
                        xmScaleWidgetClass, DivAxisform,
                        XmNscaleMultiple, 1,
                        XmNmaximum, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNwidth, 316,
                        XmNheight, 44,
                        XmNtopWidget, AxisSecondaryText,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNshowValue, TRUE,
                        XmNnavigationType, XmTAB_GROUP,
                        RES_CONVERT( XmNtitleString, "" ),
                        XmNminimum, 1,
                        XmNvalue, 5,
                        XmNorientation, XmHORIZONTAL,
                        NULL );

        UxPutContext( AxisSecondaryScale, (char *) UxAxisFormDialogContext );

        labelGadget4 = XtVaCreateManagedWidget( "labelGadget4",
                        xmLabelGadgetClass, DivAxisform,
                        XmNwidth, 210,
                        XmNtopWidget, AxisSecondaryScale,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNx, 45,
                        XmNy, 299,
                        XmNheight, 21,
RES_CONVERT( XmNlabelString, "Number of Tertiary Divisions" ),
                        NULL );

        UxPutContext( labelGadget4, (char *) UxAxisFormDialogContext );

        AxisTertiaryText = XtVaCreateManagedWidget( "AxisTertiaryText",
                        xmTextWidgetClass, DivAxisform,
                        XmNcolumns, 7,
                        XmNtopWidget, AxisSecondaryScale,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNvalue, "1",
                        XmNmarginHeight, 3,
                        XmNheight, 30,
                        XmNy, 195,
                        NULL );

        UxPutContext( AxisTertiaryText, (char *) UxAxisFormDialogContext );

        AxisTertiaryScale = XtVaCreateManagedWidget( "AxisTertiaryScale",
                        xmScaleWidgetClass, DivAxisform,
                        XmNscaleMultiple, 1,
                        XmNmaximum, 5,
                        XmNtopWidget, AxisTertiaryText,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNwidth, 316,
                        XmNheight, 44,
                        XmNshowValue, TRUE,
                        XmNnavigationType, XmTAB_GROUP,
                        RES_CONVERT( XmNtitleString, "" ),
                        XmNminimum, 1,
                        XmNvalue, 1,
                        XmNorientation, XmHORIZONTAL,
                        NULL );

        UxPutContext( AxisTertiaryScale, (char *) UxAxisFormDialogContext );

NoOptimiToggleButtonGadget =
                        XtVaCreateManagedWidget( "NoOptimiToggleButtonGadget",
                        xmToggleButtonGadgetClass, DivAxisform,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        XmNheight, 30,
                        XmNy, 135,
                        XmNx, 10,
                        RES_CONVERT( XmNlabelString, "No Optimization" ),
                        NULL );

UxPutContext( NoOptimiToggleButtonGadget, (char *) UxAxisFormDialogContext );

        XtAddCallback( axisFormDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxAxisFormDialogContext );

        XtVaSetValues(axisFormDialog,
                        XmNdefaultButton, AxisApplyButton,
                        NULL );



        return ( axisFormDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_axisFormDialog()
{
        Widget                  rtrn;
        _UxCaxisFormDialog      *UxContext;

        UxAxisFormDialogContext = UxContext =
                (_UxCaxisFormDialog *) XtMalloc( sizeof(_UxCaxisFormDialog) );

        rtrn = _Uxbuild_axisFormDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_axisFormDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_axisFormDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

