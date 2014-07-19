/*
 * $Id: fitformdialog.c,v 1.1.1.1 1996/03/01 11:39:33 mclareni Exp $
 *
 * $Log: fitformdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:33  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.06  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        fitFormDialog.c
        (Generated from interface file fitFormDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
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
        Widget  UxrowColumn4;
} _UxCfitFormDialog;

#define separatorGadget3        UxFitFormDialogContext->UxseparatorGadget3
#define rowColumn4              UxFitFormDialogContext->UxrowColumn4

static _UxCfitFormDialog        *UxFitFormDialogContext;

Widget  fitFormDialog;
Widget  FitPushButtonGadget;
Widget  ChiToggleButtonGadget;
Widget  ErrorsToggleButtonGadget;
Widget  ParametersToggleButtonGadget;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_fitFormDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_fitFormDialog()
{
        Widget  fitFormDialog_shell;

        fitFormDialog_shell = XtVaCreatePopupShell( "fitFormDialog_shell",
                        xmDialogShellWidgetClass, histoStyle,
                        XmNx, 525,
                        XmNy, 483,
                        XmNwidth, 140,
                        XmNheight, 138,
                        XmNtitle, "fitFormDialog",
                        NULL );

        fitFormDialog = XtVaCreateWidget( "fitFormDialog",
                        xmFormWidgetClass, fitFormDialog_shell,
                        XmNnoResize, TRUE,
                        RES_CONVERT( XmNdialogTitle, "Fits" ),
                        XmNheight, 138,
                        XmNwidth, 140,
                        NULL );

        UxPutContext( fitFormDialog, (char *) UxFitFormDialogContext );

        FitPushButtonGadget = XtVaCreateManagedWidget( "FitPushButtonGadget",
                        xmPushButtonGadgetClass, fitFormDialog,
                        XmNrightPosition, 80,
                        XmNleftPosition, 20,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, " Close " ),
                        XmNheight, 30,
                        NULL );

        UxPutContext( FitPushButtonGadget, (char *) UxFitFormDialogContext );

        separatorGadget3 = XtVaCreateManagedWidget( "separatorGadget3",
                        xmSeparatorGadgetClass, fitFormDialog,
                        XmNbottomWidget, FitPushButtonGadget,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        NULL );

        UxPutContext( separatorGadget3, (char *) UxFitFormDialogContext );

        rowColumn4 = XtVaCreateManagedWidget( "rowColumn4",
                        xmRowColumnWidgetClass, fitFormDialog,
                        XmNbottomWidget, separatorGadget3,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNspacing, 0,
                        NULL );

        UxPutContext( rowColumn4, (char *) UxFitFormDialogContext );

        ChiToggleButtonGadget =
                        XtVaCreateManagedWidget( "ChiToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn4,
                        XmNset, TRUE,
                        RES_CONVERT( XmNlabelString, "Chi Square" ),
                        XmNheight, 20,
                        XmNwidth, 100,
                        XmNy, 13,
                        XmNx, 13,
                        NULL );

        UxPutContext( ChiToggleButtonGadget, (char *) UxFitFormDialogContext );

        ErrorsToggleButtonGadget =
                        XtVaCreateManagedWidget( "ErrorsToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn4,
                        RES_CONVERT( XmNlabelString, "Errors" ),
                        XmNheight, 20,
                        XmNwidth, 100,
                        XmNy, 41,
                        XmNx, 13,
                        NULL );

      UxPutContext( ErrorsToggleButtonGadget, (char *) UxFitFormDialogContext );

        ParametersToggleButtonGadget =
                       XtVaCreateManagedWidget( "ParametersToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn4,
                        XmNset, TRUE,
                        RES_CONVERT( XmNlabelString, "Parameters" ),
                        XmNheight, 20,
                        XmNwidth, 100,
                        XmNy, 41,
                        XmNx, 13,
                        NULL );

  UxPutContext( ParametersToggleButtonGadget, (char *) UxFitFormDialogContext );

        XtAddCallback( fitFormDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxFitFormDialogContext );



        return ( fitFormDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_fitFormDialog()
{
        Widget                  rtrn;
        _UxCfitFormDialog       *UxContext;

        UxFitFormDialogContext = UxContext =
                (_UxCfitFormDialog *) XtMalloc( sizeof(_UxCfitFormDialog) );

        rtrn = _Uxbuild_fitFormDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_fitFormDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_fitFormDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

