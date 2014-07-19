/*
 * $Id: angleformdialog.c,v 1.1.1.1 1996/03/01 11:39:32 mclareni Exp $
 *
 * $Log: angleformdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:32  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.04  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        angleFormDialog.c
        (Generated from interface file angleFormDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/Form.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/DrawingA.h>
#include <Xm/Frame.h>
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
        Widget  UxAngleFrame;
        Widget  UxseparatorGadget9;
        Widget  Uxlabel1;
        Widget  Uxform6;
        Widget  Uxlabel2;
} _UxCangleFormDialog;

#define AngleFrame              UxAngleFormDialogContext->UxAngleFrame
#define separatorGadget9        UxAngleFormDialogContext->UxseparatorGadget9
#define label1                  UxAngleFormDialogContext->Uxlabel1
#define form6                   UxAngleFormDialogContext->Uxform6
#define label2                  UxAngleFormDialogContext->Uxlabel2

static _UxCangleFormDialog      *UxAngleFormDialogContext;

Widget  angleFormDialog;
Widget  AngleDrawingArea;
Widget  AngleApplyButton;
Widget  AngleResetButton;
Widget  AngleCloseButton;
Widget  form5;
Widget  AngleThetaText;
Widget  AnglePhiText;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_angleFormDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_angleFormDialog()
{
        Widget  angleFormDialog_shell;

        angleFormDialog_shell = XtVaCreatePopupShell( "angleFormDialog_shell",
                        xmDialogShellWidgetClass, histoStyle,
                        XmNwidth, 260,
                        XmNheight, 200,
                        XmNtitle, "angleFormDialog",
                        NULL );

        angleFormDialog = XtVaCreateWidget( "angleFormDialog",
                        xmFormWidgetClass, angleFormDialog_shell,
                        XmNautoUnmanage, FALSE,
                        XmNnoResize, TRUE,
                        RES_CONVERT( XmNdialogTitle, "Viewing Angles" ),
                        XmNheight, 200,
                        XmNwidth, 260,
                        NULL );

        UxPutContext( angleFormDialog, (char *) UxAngleFormDialogContext );

        AngleFrame = XtVaCreateManagedWidget( "AngleFrame",
                        xmFrameWidgetClass, angleFormDialog,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNtopOffset, 8,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNshadowType, XmSHADOW_IN,
                        XmNheight, 60,
                        NULL );

        UxPutContext( AngleFrame, (char *) UxAngleFormDialogContext );

        AngleDrawingArea = XtVaCreateManagedWidget( "AngleDrawingArea",
                        xmDrawingAreaWidgetClass, AngleFrame,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNheight, 120,
                        XmNwidth, 120,
                        NULL );

        UxPutContext( AngleDrawingArea, (char *) UxAngleFormDialogContext );

        AngleApplyButton = XtVaCreateManagedWidget( "AngleApplyButton",
                        xmPushButtonGadgetClass, angleFormDialog,
                        XmNwidth, 70,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNshowAsDefault, 1,
                        RES_CONVERT( XmNlabelString, "Apply" ),
                        XmNheight, 30,
                        XmNy, 153,
                        NULL );

        UxPutContext( AngleApplyButton, (char *) UxAngleFormDialogContext );

        AngleResetButton = XtVaCreateManagedWidget( "AngleResetButton",
                        xmPushButtonGadgetClass, angleFormDialog,
                        XmNy, 150,
                        XmNx, 100,
                        XmNwidth, 70,
                        XmNleftWidget, AngleApplyButton,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, " Reset " ),
                        XmNheight, 30,
                        NULL );

        UxPutContext( AngleResetButton, (char *) UxAngleFormDialogContext );

        separatorGadget9 = XtVaCreateManagedWidget( "separatorGadget9",
                        xmSeparatorGadgetClass, angleFormDialog,
                        XmNtopWidget, AngleFrame,
                        XmNbottomWidget, AngleResetButton,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( separatorGadget9, (char *) UxAngleFormDialogContext );

        AngleCloseButton = XtVaCreateManagedWidget( "AngleCloseButton",
                        xmPushButtonGadgetClass, angleFormDialog,
                        XmNwidth, 70,
                        XmNy, 160,
                        XmNx, 170,
                        XmNleftWidget, AngleResetButton,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, " Close " ),
                        XmNheight, 30,
                        NULL );

        UxPutContext( AngleCloseButton, (char *) UxAngleFormDialogContext );

        form5 = XtVaCreateManagedWidget( "form5",
                        xmFormWidgetClass, angleFormDialog,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftWidget, AngleFrame,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 15,
                        XmNx, 185,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form5, (char *) UxAngleFormDialogContext );

        AngleThetaText = XtVaCreateManagedWidget( "AngleThetaText",
                        xmTextWidgetClass, form5,
                        XmNwidth, 70,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNmarginWidth, 2,
                        XmNmarginHeight, 2,
                        XmNheight, 30,
                        XmNy, 65,
                        XmNx, 160,
                        NULL );

        UxPutContext( AngleThetaText, (char *) UxAngleFormDialogContext );

        label1 = XtVaCreateManagedWidget( "label1",
                        xmLabelWidgetClass, form5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Theta:" ),
                        XmNheight, 30,
                        NULL );

        UxPutContext( label1, (char *) UxAngleFormDialogContext );

        form6 = XtVaCreateManagedWidget( "form6",
                        xmFormWidgetClass, angleFormDialog,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftWidget, AngleFrame,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, form5,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNheight, 30,
                        XmNwidth, 120,
                        XmNy, 35,
                        XmNx, 5,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form6, (char *) UxAngleFormDialogContext );

        AnglePhiText = XtVaCreateManagedWidget( "AnglePhiText",
                        xmTextWidgetClass, form6,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNmarginWidth, 2,
                        XmNmarginHeight, 2,
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 15,
                        XmNx, 201,
                        NULL );

        UxPutContext( AnglePhiText, (char *) UxAngleFormDialogContext );

        label2 = XtVaCreateManagedWidget( "label2",
                        xmLabelWidgetClass, form6,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Phi:" ),
                        XmNheight, 20,
                        NULL );

        UxPutContext( label2, (char *) UxAngleFormDialogContext );

        XtAddCallback( angleFormDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxAngleFormDialogContext );

        XtVaSetValues(angleFormDialog,
                        XmNdefaultButton, AngleApplyButton,
                        NULL );



        return ( angleFormDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_angleFormDialog()
{
        Widget                  rtrn;
        _UxCangleFormDialog     *UxContext;

        UxAngleFormDialogContext = UxContext =
                (_UxCangleFormDialog *) XtMalloc( sizeof(_UxCangleFormDialog) );

        rtrn = _Uxbuild_angleFormDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_angleFormDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_angleFormDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

