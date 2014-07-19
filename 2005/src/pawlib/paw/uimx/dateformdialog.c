/*
 * $Id: dateformdialog.c,v 1.1.1.1 1996/03/01 11:39:32 mclareni Exp $
 *
 * $Log: dateformdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:32  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.05  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        dateFormDialog.c
        (Generated from interface file dateFormDialog.i)
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
        Widget  UxseparatorGadget13;
        Widget  UxrowColumn7;
} _UxCdateFormDialog;

#define separatorGadget13       UxDateFormDialogContext->UxseparatorGadget13
#define rowColumn7              UxDateFormDialogContext->UxrowColumn7

static _UxCdateFormDialog       *UxDateFormDialogContext;

Widget  dateFormDialog;
Widget  FileDatePushButtonGadget1;
Widget  TLToggleButtonGadget1;
Widget  TRToggleButtonGadget1;
Widget  BLToggleButtonGadget1;
Widget  BRToggleButtonGadget1;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_dateFormDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_dateFormDialog()
{
        Widget  dateFormDialog_shell;

        dateFormDialog_shell = XtVaCreatePopupShell( "dateFormDialog_shell",
                        xmDialogShellWidgetClass, histoStyle,
                        XmNheight, 166,
                        XmNtitle, "dateFormDialog",
                        NULL );

        dateFormDialog = XtVaCreateWidget( "dateFormDialog",
                        xmFormWidgetClass, dateFormDialog_shell,
                        XmNnoResize, TRUE,
                        RES_CONVERT( XmNdialogTitle, "Date Position" ),
                        XmNheight, 166,
                        NULL );

        UxPutContext( dateFormDialog, (char *) UxDateFormDialogContext );

        FileDatePushButtonGadget1 =
                        XtVaCreateManagedWidget( "FileDatePushButtonGadget1",
                        xmPushButtonGadgetClass, dateFormDialog,
                        XmNrightPosition, 80,
                        XmNleftPosition, 20,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, " Close " ),
                        XmNheight, 30,
                        XmNwidth, 60,
                        XmNy, 130,
                        XmNx, 10,
                        NULL );

   UxPutContext( FileDatePushButtonGadget1, (char *) UxDateFormDialogContext );

        separatorGadget13 = XtVaCreateManagedWidget( "separatorGadget13",
                        xmSeparatorGadgetClass, dateFormDialog,
                        XmNbottomWidget, FileDatePushButtonGadget1,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        NULL );

        UxPutContext( separatorGadget13, (char *) UxDateFormDialogContext );

        rowColumn7 = XtVaCreateManagedWidget( "rowColumn7",
                        xmRowColumnWidgetClass, dateFormDialog,
                        XmNbottomWidget, separatorGadget13,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNwhichButton, 1,
                        XmNspacing, 0,
                        XmNradioBehavior, TRUE,
                        XmNradioAlwaysOne, TRUE,
                        NULL );

        UxPutContext( rowColumn7, (char *) UxDateFormDialogContext );

        TLToggleButtonGadget1 =
                        XtVaCreateManagedWidget( "TLToggleButtonGadget1",
                        xmToggleButtonGadgetClass, rowColumn7,
                        RES_CONVERT( XmNlabelString, "Top Left" ),
                        XmNheight, 20,
                        XmNwidth, 100,
                        XmNy, 9,
                        XmNx, 9,
                        NULL );

        UxPutContext( TLToggleButtonGadget1, (char *) UxDateFormDialogContext );

       TRToggleButtonGadget1 = XtVaCreateManagedWidget( "TRToggleButtonGadget1",
                        xmToggleButtonGadgetClass, rowColumn7,
                        RES_CONVERT( XmNlabelString, "Top Right" ),
                        XmNheight, 20,
                        XmNwidth, 100,
                        XmNy, 13,
                        XmNx, 13,
                        NULL );

        UxPutContext( TRToggleButtonGadget1, (char *) UxDateFormDialogContext );

        BLToggleButtonGadget1 =
                        XtVaCreateManagedWidget( "BLToggleButtonGadget1",
                        xmToggleButtonGadgetClass, rowColumn7,
                        RES_CONVERT( XmNlabelString, "Bottom Left" ),
                        XmNheight, 20,
                        XmNwidth, 100,
                        XmNy, 41,
                        XmNx, 13,
                        NULL );

        UxPutContext( BLToggleButtonGadget1, (char *) UxDateFormDialogContext );

        BRToggleButtonGadget1 =
                        XtVaCreateManagedWidget( "BRToggleButtonGadget1",
                        xmToggleButtonGadgetClass, rowColumn7,
                        RES_CONVERT( XmNlabelString, "Bottom Right" ),
                        XmNheight, 60,
                        XmNwidth, 107,
                        XmNy, 3,
                        XmNx, 280,
                        NULL );

        UxPutContext( BRToggleButtonGadget1, (char *) UxDateFormDialogContext );

        XtAddCallback( dateFormDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxDateFormDialogContext );



        return ( dateFormDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_dateFormDialog()
{
        Widget                  rtrn;
        _UxCdateFormDialog      *UxContext;

        UxDateFormDialogContext = UxContext =
                (_UxCdateFormDialog *) XtMalloc( sizeof(_UxCdateFormDialog) );

        rtrn = _Uxbuild_dateFormDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_dateFormDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_dateFormDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

