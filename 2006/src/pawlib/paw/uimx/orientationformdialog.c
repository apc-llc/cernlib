/*
 * $Id: orientationformdialog.c,v 1.1.1.1 1996/03/01 11:39:35 mclareni Exp $
 *
 * $Log: orientationformdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:35  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.07  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        orientationFormDialog.c
        (Generated from interface file orientationFormDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/LabelG.h>
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
        Widget  UxxdirLabel;
        Widget  UxydirLabel;
        Widget  UxseparatorGadget12;
        Widget  UxrowColumn9;
        Widget  UxydirFrame;
        Widget  UxrowColumn10;
} _UxCorientationFormDialog;

#define xdirLabel               UxOrientationFormDialogContext->UxxdirLabel
#define ydirLabel               UxOrientationFormDialogContext->UxydirLabel
#define separatorGadget12   UxOrientationFormDialogContext->UxseparatorGadget12
#define rowColumn9              UxOrientationFormDialogContext->UxrowColumn9
#define ydirFrame               UxOrientationFormDialogContext->UxydirFrame
#define rowColumn10             UxOrientationFormDialogContext->UxrowColumn10

static _UxCorientationFormDialog        *UxOrientationFormDialogContext;

Widget  orientationFormDialog;
Widget  resetdirPushButton;
Widget  closedirPushButton;
Widget  xdirFrame;
Widget  xdirtoggleButton1;
Widget  xdirtoggleButton2;
Widget  xdirtoggleButton3;
Widget  xdirtoggleButton4;
Widget  xdirtoggleButton5;
Widget  xdirtoggleButton6;
Widget  xdirtoggleButton7;
Widget  xdirtoggleButton8;
Widget  ydirtoggleButton1;
Widget  ydirtoggleButton2;
Widget  ydirtoggleButton3;
Widget  ydirtoggleButton4;
Widget  ydirtoggleButton5;
Widget  ydirtoggleButton6;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_orientationFormDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_orientationFormDialog()
{
        Widget  orientationFormDialog_shell;

        orientationFormDialog_shell =
XtVaCreatePopupShell( "orientationFormDialog_shell",
                        xmDialogShellWidgetClass, axisFormDialog,
                        XmNx, 900,
                        XmNy, 370,
                        XmNwidth, 240,
                        XmNheight, 440,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "orientationFormDialog",
                        NULL );

        orientationFormDialog = XtVaCreateWidget( "orientationFormDialog",
                        xmFormWidgetClass, orientationFormDialog_shell,
                        XmNautoUnmanage, FALSE,
                        RES_CONVERT( XmNdialogTitle, "Label Orientation" ),
                        XmNheight, 440,
                        XmNwidth, 240,
                        XmNunitType, XmPIXELS,
                        NULL );

 UxPutContext( orientationFormDialog, (char *) UxOrientationFormDialogContext );

        xdirLabel = XtVaCreateManagedWidget( "xdirLabel",
                        xmLabelGadgetClass, orientationFormDialog,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "X Axis" ),
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNheight, 20,
                        NULL );

        UxPutContext( xdirLabel, (char *) UxOrientationFormDialogContext );

        ydirLabel = XtVaCreateManagedWidget( "ydirLabel",
                        xmLabelGadgetClass, orientationFormDialog,
                        XmNleftPosition, 52,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        XmNtopAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Y Axis" ),
                        XmNtopOffset, 5,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNheight, 20,
                        NULL );

        UxPutContext( ydirLabel, (char *) UxOrientationFormDialogContext );

        resetdirPushButton = XtVaCreateManagedWidget( "resetdirPushButton",
                        xmPushButtonGadgetClass, orientationFormDialog,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        XmNheight, 30,
                        XmNwidth, 100,
                        NULL );

    UxPutContext( resetdirPushButton, (char *) UxOrientationFormDialogContext );

        closedirPushButton = XtVaCreateManagedWidget( "closedirPushButton",
                        xmPushButtonGadgetClass, orientationFormDialog,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNheight, 30,
                        XmNwidth, 100,
                        NULL );

  UxPutContext( closedirPushButton, (char *) UxOrientationFormDialogContext );

        separatorGadget12 = XtVaCreateManagedWidget( "separatorGadget12",
                        xmSeparatorGadgetClass, orientationFormDialog,
                        XmNbottomOffset, 5,
                        XmNbottomWidget, resetdirPushButton,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );

  UxPutContext( separatorGadget12, (char *) UxOrientationFormDialogContext );

        xdirFrame = XtVaCreateManagedWidget( "xdirFrame",
                        xmFrameWidgetClass, orientationFormDialog,
                        XmNwidth, 120,
                        XmNrightPosition, 48,
                        XmNbottomWidget, separatorGadget12,
                        XmNheight, 358,
                        XmNtopWidget, xdirLabel,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNy, 25,
                        XmNx, 5,
                        NULL );

        UxPutContext( xdirFrame, (char *) UxOrientationFormDialogContext );

        rowColumn9 = XtVaCreateManagedWidget( "rowColumn9",
                        xmRowColumnWidgetClass, xdirFrame,
                        XmNheight, 348,
                        XmNradioBehavior, TRUE,
                        XmNy, 5,
                        XmNx, 2,
                        NULL );

        UxPutContext( rowColumn9, (char *) UxOrientationFormDialogContext );

        xdirtoggleButton1 = XtVaCreateManagedWidget( "xdirtoggleButton1",
                        xmToggleButtonGadgetClass, rowColumn9,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

   UxPutContext( xdirtoggleButton1, (char *) UxOrientationFormDialogContext );

        xdirtoggleButton2 = XtVaCreateManagedWidget( "xdirtoggleButton2",
                        xmToggleButtonGadgetClass, rowColumn9,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

     UxPutContext( xdirtoggleButton2, (char *) UxOrientationFormDialogContext );

        xdirtoggleButton3 = XtVaCreateManagedWidget( "xdirtoggleButton3",
                        xmToggleButtonGadgetClass, rowColumn9,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

    UxPutContext( xdirtoggleButton3, (char *) UxOrientationFormDialogContext );

        xdirtoggleButton4 = XtVaCreateManagedWidget( "xdirtoggleButton4",
                        xmToggleButtonGadgetClass, rowColumn9,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

    UxPutContext( xdirtoggleButton4, (char *) UxOrientationFormDialogContext );

        xdirtoggleButton5 = XtVaCreateManagedWidget( "xdirtoggleButton5",
                        xmToggleButtonGadgetClass, rowColumn9,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

    UxPutContext( xdirtoggleButton5, (char *) UxOrientationFormDialogContext );

        xdirtoggleButton6 = XtVaCreateManagedWidget( "xdirtoggleButton6",
                        xmToggleButtonGadgetClass, rowColumn9,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

     UxPutContext( xdirtoggleButton6, (char *) UxOrientationFormDialogContext );

        xdirtoggleButton7 = XtVaCreateManagedWidget( "xdirtoggleButton7",
                        xmToggleButtonGadgetClass, rowColumn9,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

     UxPutContext( xdirtoggleButton7, (char *) UxOrientationFormDialogContext );

        xdirtoggleButton8 = XtVaCreateManagedWidget( "xdirtoggleButton8",
                        xmToggleButtonGadgetClass, rowColumn9,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

     UxPutContext( xdirtoggleButton8, (char *) UxOrientationFormDialogContext );

        ydirFrame = XtVaCreateManagedWidget( "ydirFrame",
                        xmFrameWidgetClass, orientationFormDialog,
                        XmNleftPosition, 52,
                        XmNbottomWidget, separatorGadget12,
                        XmNtopWidget, ydirLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNy, 300,
                        XmNx, 185,
                        NULL );

        UxPutContext( ydirFrame, (char *) UxOrientationFormDialogContext );

        rowColumn10 = XtVaCreateManagedWidget( "rowColumn10",
                        xmRowColumnWidgetClass, ydirFrame,
                        XmNheight, 356,
                        XmNwidth, 113,
                        XmNradioBehavior, TRUE,
                        XmNy, 0,
                        XmNx, 2,
                        NULL );

        UxPutContext( rowColumn10, (char *) UxOrientationFormDialogContext );

        ydirtoggleButton1 = XtVaCreateManagedWidget( "ydirtoggleButton1",
                        xmToggleButtonGadgetClass, rowColumn10,
                        XmNmarginLeft, 25,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

     UxPutContext( ydirtoggleButton1, (char *) UxOrientationFormDialogContext );

        ydirtoggleButton2 = XtVaCreateManagedWidget( "ydirtoggleButton2",
                        xmToggleButtonGadgetClass, rowColumn10,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

     UxPutContext( ydirtoggleButton2, (char *) UxOrientationFormDialogContext );

        ydirtoggleButton3 = XtVaCreateManagedWidget( "ydirtoggleButton3",
                        xmToggleButtonGadgetClass, rowColumn10,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

    UxPutContext( ydirtoggleButton3, (char *) UxOrientationFormDialogContext );

        ydirtoggleButton4 = XtVaCreateManagedWidget( "ydirtoggleButton4",
                        xmToggleButtonGadgetClass, rowColumn10,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

     UxPutContext( ydirtoggleButton4, (char *) UxOrientationFormDialogContext );

        ydirtoggleButton5 = XtVaCreateManagedWidget( "ydirtoggleButton5",
                        xmToggleButtonGadgetClass, rowColumn10,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

    UxPutContext( ydirtoggleButton5, (char *) UxOrientationFormDialogContext );

        ydirtoggleButton6 = XtVaCreateManagedWidget( "ydirtoggleButton6",
                        xmToggleButtonGadgetClass, rowColumn10,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );

     UxPutContext( ydirtoggleButton6, (char *) UxOrientationFormDialogContext );

        XtAddCallback( orientationFormDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxOrientationFormDialogContext );

        XtVaSetValues(xdirtoggleButton1,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(xdirtoggleButton2,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(xdirtoggleButton3,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(xdirtoggleButton4,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(xdirtoggleButton5,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(xdirtoggleButton6,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(xdirtoggleButton7,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(xdirtoggleButton8,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(ydirtoggleButton1,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(ydirtoggleButton2,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(ydirtoggleButton3,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(ydirtoggleButton4,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(ydirtoggleButton5,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(ydirtoggleButton6,
                        XmNlabelType, XmPIXMAP,
                        NULL );



        return ( orientationFormDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_orientationFormDialog()
{
        Widget                  rtrn;
        _UxCorientationFormDialog *UxContext;

        UxOrientationFormDialogContext = UxContext =
   (_UxCorientationFormDialog *) XtMalloc( sizeof(_UxCorientationFormDialog) );

        rtrn = _Uxbuild_orientationFormDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_orientationFormDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_orientationFormDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

