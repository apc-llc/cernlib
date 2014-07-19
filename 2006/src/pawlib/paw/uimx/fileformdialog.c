/*
 * $Id: fileformdialog.c,v 1.1.1.1 1996/03/01 11:39:33 mclareni Exp $
 *
 * $Log: fileformdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:33  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.05  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        fileFormDialog.c
        (Generated from interface file fileFormDialog.i)
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
        Widget  UxseparatorGadget5;
        Widget  UxrowColumn6;
} _UxCfileFormDialog;

#define separatorGadget5        UxFileFormDialogContext->UxseparatorGadget5
#define rowColumn6              UxFileFormDialogContext->UxrowColumn6

static _UxCfileFormDialog       *UxFileFormDialogContext;

Widget  fileFormDialog;
Widget  FileDatePushButtonGadget;
Widget  TLToggleButtonGadget;
Widget  TRToggleButtonGadget;
Widget  BLToggleButtonGadget;
Widget  BRToggleButtonGadget;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_fileFormDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_fileFormDialog()
{
        Widget  fileFormDialog_shell;

        fileFormDialog_shell = XtVaCreatePopupShell( "fileFormDialog_shell",
                        xmDialogShellWidgetClass, histoStyle,
                        XmNx, 356,
                        XmNy, 296,
                        XmNheight, 169,
                        XmNtitle, "fileFormDialog",
                        NULL );

        fileFormDialog = XtVaCreateWidget( "fileFormDialog",
                        xmFormWidgetClass, fileFormDialog_shell,
                        XmNnoResize, TRUE,
                        RES_CONVERT( XmNdialogTitle, "File Position" ),
                        XmNheight, 169,
                        NULL );

        UxPutContext( fileFormDialog, (char *) UxFileFormDialogContext );

        FileDatePushButtonGadget =
                        XtVaCreateManagedWidget( "FileDatePushButtonGadget",
                        xmPushButtonGadgetClass, fileFormDialog,
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

     UxPutContext( FileDatePushButtonGadget, (char *) UxFileFormDialogContext );

        separatorGadget5 = XtVaCreateManagedWidget( "separatorGadget5",
                        xmSeparatorGadgetClass, fileFormDialog,
                        XmNbottomWidget, FileDatePushButtonGadget,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        NULL );

        UxPutContext( separatorGadget5, (char *) UxFileFormDialogContext );

        rowColumn6 = XtVaCreateManagedWidget( "rowColumn6",
                        xmRowColumnWidgetClass, fileFormDialog,
                        XmNbottomWidget, separatorGadget5,
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

        UxPutContext( rowColumn6, (char *) UxFileFormDialogContext );

        TLToggleButtonGadget = XtVaCreateManagedWidget( "TLToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn6,
                        RES_CONVERT( XmNlabelString, "Top Left" ),
                        XmNheight, 20,
                        XmNwidth, 100,
                        XmNy, 9,
                        XmNx, 9,
                        NULL );

        UxPutContext( TLToggleButtonGadget, (char *) UxFileFormDialogContext );

        TRToggleButtonGadget = XtVaCreateManagedWidget( "TRToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn6,
                        RES_CONVERT( XmNlabelString, "Top Right" ),
                        XmNheight, 20,
                        XmNwidth, 100,
                        XmNy, 13,
                        XmNx, 13,
                        NULL );

        UxPutContext( TRToggleButtonGadget, (char *) UxFileFormDialogContext );

        BLToggleButtonGadget = XtVaCreateManagedWidget( "BLToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn6,
                        RES_CONVERT( XmNlabelString, "Bottom Left" ),
                        XmNheight, 20,
                        XmNwidth, 100,
                        XmNy, 41,
                        XmNx, 13,
                        NULL );

        UxPutContext( BLToggleButtonGadget, (char *) UxFileFormDialogContext );

        BRToggleButtonGadget = XtVaCreateManagedWidget( "BRToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn6,
                        RES_CONVERT( XmNlabelString, "Bottom Right    " ),
                        XmNheight, 28,
                        XmNwidth, 122,
                        XmNy, 87,
                        XmNx, 3,
                        NULL );

        UxPutContext( BRToggleButtonGadget, (char *) UxFileFormDialogContext );

        XtAddCallback( fileFormDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxFileFormDialogContext );



        return ( fileFormDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_fileFormDialog()
{
        Widget                  rtrn;
        _UxCfileFormDialog      *UxContext;

        UxFileFormDialogContext = UxContext =
                (_UxCfileFormDialog *) XtMalloc( sizeof(_UxCfileFormDialog) );

        rtrn = _Uxbuild_fileFormDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_fileFormDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_fileFormDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

