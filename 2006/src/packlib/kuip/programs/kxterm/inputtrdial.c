/*
 * $Id: inputtrdial.c,v 1.1.1.1 1996/03/08 15:33:04 mclareni Exp $
 *
 * $Log: inputtrdial.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:04  mclareni
 * Kuip
 *
 */
/*CMZ :  2.00/09 18/01/93  12.55.17  by  Alfred Nathaniel*/
/*-- Author :*/

/*******************************************************************************
        inputTrDialog.c
        (Generated from interface file inputTrDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "uxxt.h"

#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>

/*******************************************************************************
        Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern swidget kuipIo;

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
        Widget  UxinputTrDialog;
        Widget  Uxseparator2;
        Widget  UxscrolledWindow;
} _UxCinputTrDialog;

#define inputTrDialog           UxInputTrDialogContext->UxinputTrDialog
#define separator2              UxInputTrDialogContext->Uxseparator2
#define scrolledWindow          UxInputTrDialogContext->UxscrolledWindow

static _UxCinputTrDialog        *UxInputTrDialogContext;

Widget  closeInputTrButton;
Widget  inputTrText;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_inputTrDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_inputTrDialog()
{
        Widget  inputTrDialog_shell;

        inputTrDialog_shell = XtVaCreatePopupShell( "inputTrDialog_shell",
                        xmDialogShellWidgetClass, kuipIo,
                        XmNwidth, 520,
                        XmNheight, 315,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "inputTrDialog",
                        NULL );

        inputTrDialog = XtVaCreateWidget( "inputTrDialog",
                        xmFormWidgetClass, inputTrDialog_shell,
                        XmNdefaultPosition, FALSE,
                        XmNautoUnmanage, FALSE,
                        RES_CONVERT( XmNdialogTitle, "Input Transcript" ),
                        XmNheight, 315,
                        XmNwidth, 520,
                        XmNunitType, XmPIXELS,
                        NULL );

        UxPutContext( inputTrDialog, (char *) UxInputTrDialogContext );

        closeInputTrButton = XtVaCreateManagedWidget( "closeInputTrButton",
                        xmPushButtonWidgetClass, inputTrDialog,
                        XmNrightPosition, 60,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 40,
                        XmNleftAttachment, XmATTACH_POSITION,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        XmNwidth, 60,
                        NULL );

        UxPutContext( closeInputTrButton, (char *) UxInputTrDialogContext );

        separator2 = XtVaCreateManagedWidget( "separator2",
                        xmSeparatorWidgetClass, inputTrDialog,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomWidget, closeInputTrButton,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        NULL );

        UxPutContext( separator2, (char *) UxInputTrDialogContext );

        scrolledWindow = XtVaCreateManagedWidget( "scrolledWindow",
                        xmScrolledWindowWidgetClass, inputTrDialog,
                        XmNbottomWidget, separator2,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNshadowThickness, 0,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        NULL );

        UxPutContext( scrolledWindow, (char *) UxInputTrDialogContext );

        inputTrText = XtVaCreateManagedWidget( "inputTrText",
                        xmTextWidgetClass, scrolledWindow,
                        XmNeditable, FALSE,
                        XmNeditMode, XmMULTI_LINE_EDIT ,
                        XmNheight, 210,
                        XmNwidth, 495,
                        NULL );

        UxPutContext( inputTrText, (char *) UxInputTrDialogContext );

        XtAddCallback( inputTrDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxInputTrDialogContext );

        XtVaSetValues(inputTrDialog,
                        XmNdefaultButton, closeInputTrButton,
                        NULL );



        return ( inputTrDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_inputTrDialog()
{
        Widget                  rtrn;
        _UxCinputTrDialog       *UxContext;

        UxInputTrDialogContext = UxContext =
                (_UxCinputTrDialog *) XtMalloc( sizeof(_UxCinputTrDialog) );

        rtrn = _Uxbuild_inputTrDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_inputTrDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_inputTrDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

