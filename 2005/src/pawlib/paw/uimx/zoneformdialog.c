/*
 * $Id: zoneformdialog.c,v 1.1.1.1 1996/03/01 11:39:35 mclareni Exp $
 *
 * $Log: zoneformdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:35  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.07  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        zoneFormDialog.c
        (Generated from interface file zoneFormDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Form.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
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
        Widget  Uxseparator1;
} _UxCzoneFormDialog;

#define separator1              UxZoneFormDialogContext->Uxseparator1

static _UxCzoneFormDialog       *UxZoneFormDialogContext;

Widget  zoneFormDialog;
Widget  zoneCloseButton;
Widget  zoneResetButton;
Widget  zoneForm;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_zoneFormDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_zoneFormDialog()
{
        Widget  zoneFormDialog_shell;

        zoneFormDialog_shell = XtVaCreatePopupShell( "zoneFormDialog_shell",
                        xmDialogShellWidgetClass, histoStyle,
                        XmNx, 455,
                        XmNy, 338,
                        XmNwidth, 200,
                        XmNheight, 240,
                        XmNtitle, "zoneFormDialog",
                        NULL );

        zoneFormDialog = XtVaCreateWidget( "zoneFormDialog",
                        xmFormWidgetClass, zoneFormDialog_shell,
                        XmNnoResize, TRUE,
                        RES_CONVERT( XmNdialogTitle, "Zones" ),
                        XmNautoUnmanage, FALSE,
                        XmNheight, 240,
                        XmNwidth, 200,
                        NULL );

        UxPutContext( zoneFormDialog, (char *) UxZoneFormDialogContext );

        zoneCloseButton = XtVaCreateManagedWidget( "zoneCloseButton",
                        xmPushButtonWidgetClass, zoneFormDialog,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, " Close " ),
                        XmNheight, 30,
                        XmNwidth, 80,
                        NULL );

        UxPutContext( zoneCloseButton, (char *) UxZoneFormDialogContext );

        separator1 = XtVaCreateManagedWidget( "separator1",
                        xmSeparatorWidgetClass, zoneFormDialog,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomWidget, zoneCloseButton,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        NULL );

        UxPutContext( separator1, (char *) UxZoneFormDialogContext );

        zoneResetButton = XtVaCreateManagedWidget( "zoneResetButton",
                        xmPushButtonWidgetClass, zoneFormDialog,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        XmNwidth, 80,
                        NULL );

        UxPutContext( zoneResetButton, (char *) UxZoneFormDialogContext );

        zoneForm = XtVaCreateManagedWidget( "zoneForm",
                        xmFormWidgetClass, zoneFormDialog,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomWidget, separator1,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNheight, 155,
                        XmNwidth, 75,
                        XmNy, 10,
                        XmNx, 85,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( zoneForm, (char *) UxZoneFormDialogContext );

        XtAddCallback( zoneFormDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxZoneFormDialogContext );



        return ( zoneFormDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_zoneFormDialog()
{
        Widget                  rtrn;
        _UxCzoneFormDialog      *UxContext;

        UxZoneFormDialogContext = UxContext =
                (_UxCzoneFormDialog *) XtMalloc( sizeof(_UxCzoneFormDialog) );

        rtrn = _Uxbuild_zoneFormDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_zoneFormDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_zoneFormDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

