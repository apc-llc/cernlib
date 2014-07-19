/*
 * $Id: statformdialog.c,v 1.1.1.1 1996/03/01 11:39:35 mclareni Exp $
 *
 * $Log: statformdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:35  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.07  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        statFormDialog.c
        (Generated from interface file statFormDialog.i)
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
        Widget  UxseparatorGadget1;
        Widget  UxrowColumn1;
} _UxCstatFormDialog;

#define separatorGadget1        UxStatFormDialogContext->UxseparatorGadget1
#define rowColumn1              UxStatFormDialogContext->UxrowColumn1

static _UxCstatFormDialog       *UxStatFormDialogContext;

Widget  statFormDialog;
Widget  StatPushButtonGadget;
Widget  IdToggleButtonGadget;
Widget  EntriesToggleButtonGadget;
Widget  MeanToggleButtonGadget;
Widget  RmsToggleButtonGadget;
Widget  UnderToggleButtonGadget;
Widget  OverToggleButtonGadget;
Widget  AllchanToggleButtonGadget;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_statFormDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_statFormDialog()
{
        Widget  statFormDialog_shell;

        statFormDialog_shell = XtVaCreatePopupShell( "statFormDialog_shell",
                        xmDialogShellWidgetClass, histoStyle,
                        XmNx, 79,
                        XmNy, 479,
                        XmNwidth, 140,
                        XmNheight, 250,
                        XmNtitle, "statFormDialog",
                        NULL );

        statFormDialog = XtVaCreateWidget( "statFormDialog",
                        xmFormWidgetClass, statFormDialog_shell,
                        XmNnoResize, TRUE,
                        RES_CONVERT( XmNdialogTitle, "Statistics" ),
                        XmNheight, 250,
                        XmNwidth, 140,
                        NULL );

        UxPutContext( statFormDialog, (char *) UxStatFormDialogContext );

        StatPushButtonGadget = XtVaCreateManagedWidget( "StatPushButtonGadget",
                        xmPushButtonGadgetClass, statFormDialog,
                        XmNrightPosition, 80,
                        XmNleftPosition, 20,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, " Close " ),
                        XmNheight, 30,
                        NULL );

        UxPutContext( StatPushButtonGadget, (char *) UxStatFormDialogContext );

        separatorGadget1 = XtVaCreateManagedWidget( "separatorGadget1",
                        xmSeparatorGadgetClass, statFormDialog,
                        XmNbottomWidget, StatPushButtonGadget,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        NULL );

        UxPutContext( separatorGadget1, (char *) UxStatFormDialogContext );

        rowColumn1 = XtVaCreateManagedWidget( "rowColumn1",
                        xmRowColumnWidgetClass, statFormDialog,
                        XmNbottomWidget, separatorGadget1,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNspacing, 0,
                        XmNwidth, 130,
                        XmNy, 7,
                        XmNx, 5,
                        NULL );

        UxPutContext( rowColumn1, (char *) UxStatFormDialogContext );

        IdToggleButtonGadget = XtVaCreateManagedWidget( "IdToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn1,
                        XmNset, TRUE,
                        RES_CONVERT( XmNlabelString, "Histogram ID" ),
                        XmNheight, 20,
                        XmNwidth, 100,
                        XmNy, 10,
                        XmNx, 10,
                        NULL );

        UxPutContext( IdToggleButtonGadget, (char *) UxStatFormDialogContext );

        EntriesToggleButtonGadget =
XtVaCreateManagedWidget( "EntriesToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn1,
                        XmNset, TRUE,
                        RES_CONVERT( XmNlabelString, "Entries" ),
                        XmNheight, 30,
                        XmNwidth, 100,
                        XmNy, 38,
                        XmNx, 13,
                        NULL );

   UxPutContext( EntriesToggleButtonGadget, (char *) UxStatFormDialogContext );

        MeanToggleButtonGadget =
XtVaCreateManagedWidget( "MeanToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn1,
                        XmNset, TRUE,
                        RES_CONVERT( XmNlabelString, "Mean value" ),
                        XmNheight, 28,
                        XmNwidth, 120,
                        XmNy, 59,
                        XmNx, 5,
                        NULL );

     UxPutContext( MeanToggleButtonGadget, (char *) UxStatFormDialogContext );

      RmsToggleButtonGadget = XtVaCreateManagedWidget( "RmsToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn1,
                        XmNset, TRUE,
                        RES_CONVERT( XmNlabelString, "R.M.S." ),
                        XmNheight, 28,
                        XmNwidth, 130,
                        XmNy, 89,
                        XmNx, 5,
                        NULL );

        UxPutContext( RmsToggleButtonGadget, (char *) UxStatFormDialogContext );

        UnderToggleButtonGadget =
 XtVaCreateManagedWidget( "UnderToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn1,
                        RES_CONVERT( XmNlabelString, "Underflows" ),
                        XmNheight, 28,
                        XmNwidth, 130,
                        XmNy, 109,
                        XmNx, 5,
                        NULL );

     UxPutContext( UnderToggleButtonGadget, (char *) UxStatFormDialogContext );

        OverToggleButtonGadget =
XtVaCreateManagedWidget( "OverToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn1,
                        RES_CONVERT( XmNlabelString, "Overflows" ),
                        XmNheight, 28,
                        XmNwidth, 130,
                        XmNy, 139,
                        XmNx, 5,
                        NULL );

     UxPutContext( OverToggleButtonGadget, (char *) UxStatFormDialogContext );

        AllchanToggleButtonGadget =
XtVaCreateManagedWidget( "AllchanToggleButtonGadget",
                        xmToggleButtonGadgetClass, rowColumn1,
                        RES_CONVERT( XmNlabelString, "All channels" ),
                        XmNheight, 28,
                        XmNwidth, 120,
                        XmNy, 169,
                        XmNx, 5,
                        NULL );

    UxPutContext( AllchanToggleButtonGadget, (char *) UxStatFormDialogContext );

        XtAddCallback( statFormDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxStatFormDialogContext );



        return ( statFormDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_statFormDialog()
{
        Widget                  rtrn;
        _UxCstatFormDialog      *UxContext;

        UxStatFormDialogContext = UxContext =
                (_UxCstatFormDialog *) XtMalloc( sizeof(_UxCstatFormDialog) );

        rtrn = _Uxbuild_statFormDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_statFormDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_statFormDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

