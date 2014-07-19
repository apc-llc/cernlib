/*
 * $Id: dynamiccedialog.c,v 1.1.1.1 1996/03/01 11:39:33 mclareni Exp $
 *
 * $Log: dynamiccedialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:33  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.05  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        dynamicCeDialog.c
        (Generated from interface file dynamicCeDialog.i)
        Associated Header file: dynamicCeDialog.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Scale.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/Form.h>


/*******************************************************************************
        The following header file defines the context structure.
*******************************************************************************/

#include "hmotif/dynamiccedialog.h"


/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_dynamicCeDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_dynamicCeDialog()
{
        Widget  dynamicCeDialog_shell;

        dynamicCeDialog_shell = XtVaCreatePopupShell( "dynamicCeDialog_shell",
                        xmDialogShellWidgetClass, dyn_parent,
                        XmNwidth, 460,
                        XmNheight, 220,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "dynamicCeDialog",
                        NULL );

        dynamicCeDialog = XtVaCreateWidget( "dynamicCeDialog",
                        xmFormWidgetClass, dynamicCeDialog_shell,
                        XmNautoUnmanage, FALSE,
                        RES_CONVERT( XmNdialogTitle, "Dynamic Cut" ),
                        XmNheight, 220,
                        XmNwidth, 460,
                        XmNunitType, XmPIXELS,
                        NULL );

        UxPutContext( dynamicCeDialog, (char *) UxDynamicCeDialogContext );

        DyForm2 = XtVaCreateManagedWidget( "DyForm2",
                        xmFormWidgetClass, dynamicCeDialog,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( DyForm2, (char *) UxDynamicCeDialogContext );

        DyLabel1 = XtVaCreateManagedWidget( "DyLabel1",
                        xmLabelWidgetClass, DyForm2,
                        RES_CONVERT( XmNlabelString, "Scale Range +/- " ),
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( DyLabel1, (char *) UxDynamicCeDialogContext );

        percentDyText = XtVaCreateManagedWidget( "percentDyText",
                        xmTextWidgetClass, DyForm2,
                        XmNcolumns, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftWidget, DyLabel1,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 35,
                        NULL );

        UxPutContext( percentDyText, (char *) UxDynamicCeDialogContext );

        applyDyButton = XtVaCreateManagedWidget( "applyDyButton",
                        xmPushButtonWidgetClass, dynamicCeDialog,
                        XmNrightPosition, 25,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshowAsDefault, 1,
                        RES_CONVERT( XmNlabelString, "Apply" ),
                        XmNheight, 30,
                        NULL );

        UxPutContext( applyDyButton, (char *) UxDynamicCeDialogContext );

        resetDyButton = XtVaCreateManagedWidget( "resetDyButton",
                        xmPushButtonWidgetClass, dynamicCeDialog,
                        XmNrightPosition, 60,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 40,
                        XmNleftAttachment, XmATTACH_POSITION,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        NULL );

        UxPutContext( resetDyButton, (char *) UxDynamicCeDialogContext );

        closeDyButton = XtVaCreateManagedWidget( "closeDyButton",
                        xmPushButtonWidgetClass, dynamicCeDialog,
                        XmNrightPosition, 95,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 75,
                        XmNleftAttachment, XmATTACH_POSITION,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        XmNwidth, 105,
                        XmNy, 245,
                        XmNx, 350,
                        NULL );

        UxPutContext( closeDyButton, (char *) UxDynamicCeDialogContext );

        DySeparator3 = XtVaCreateManagedWidget( "DySeparator3",
                        xmSeparatorWidgetClass, dynamicCeDialog,
                        XmNbottomWidget, applyDyButton,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( DySeparator3, (char *) UxDynamicCeDialogContext );

        DyFrame2 = XtVaCreateManagedWidget( "DyFrame2",
                        xmFrameWidgetClass, dynamicCeDialog,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNheight, 65,
                        NULL );

        UxPutContext( DyFrame2, (char *) UxDynamicCeDialogContext );

        DyRowColumn1 = XtVaCreateManagedWidget( "DyRowColumn1",
                        xmRowColumnWidgetClass, DyFrame2,
                        XmNorientation, XmHORIZONTAL,
                        XmNradioBehavior, TRUE,
                        NULL );

        UxPutContext( DyRowColumn1, (char *) UxDynamicCeDialogContext );

        percentDyToggle = XtVaCreateManagedWidget( "percentDyToggle",
                        xmToggleButtonGadgetClass, DyRowColumn1,
                        XmNset, TRUE,
                        RES_CONVERT( XmNlabelString, "Percent" ),
                        NULL );

        UxPutContext( percentDyToggle, (char *) UxDynamicCeDialogContext );

        absoluteDyToggle = XtVaCreateManagedWidget( "absoluteDyToggle",
                        xmToggleButtonGadgetClass, DyRowColumn1,
                        RES_CONVERT( XmNlabelString, "Absolute" ),
                        NULL );

        UxPutContext( absoluteDyToggle, (char *) UxDynamicCeDialogContext );

        dynamicDyScale = XtVaCreateManagedWidget( "dynamicDyScale",
                        xmScaleWidgetClass, dynamicCeDialog,
                        XmNshowValue, TRUE,
                        XmNtopWidget, DyFrame2,
                        XmNtopOffset, 30,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNorientation, XmHORIZONTAL,
                        NULL );

        UxPutContext( dynamicDyScale, (char *) UxDynamicCeDialogContext );

        cutDyLabel = XtVaCreateManagedWidget( "cutDyLabel",
                        xmLabelWidgetClass, dynamicCeDialog,
                        XmNrecomputeSize, FALSE,
                        XmNtopWidget, dynamicDyScale,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Cut" ),
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        XmNheight, 20,
                        NULL );

        UxPutContext( cutDyLabel, (char *) UxDynamicCeDialogContext );

        XtAddCallback( dynamicCeDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxDynamicCeDialogContext );

        XtVaSetValues(dynamicCeDialog,
                        XmNdefaultButton, applyDyButton,
                        NULL );



        return ( dynamicCeDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_dynamicCeDialog( _Uxdyn_parent )
        swidget _Uxdyn_parent;
{
        Widget                  rtrn;
        _UxCdynamicCeDialog     *UxContext;

        UxDynamicCeDialogContext = UxContext =
                (_UxCdynamicCeDialog *) XtMalloc( sizeof(_UxCdynamicCeDialog) );

        dyn_parent = _Uxdyn_parent;
        rtrn = _Uxbuild_dynamicCeDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_dynamicCeDialog( _Uxdyn_parent )
        swidget _Uxdyn_parent;
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_dynamicCeDialog( _Uxdyn_parent );

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

