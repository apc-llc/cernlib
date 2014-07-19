/*
 * $Id: opencedialog.c,v 1.1.1.1 1996/03/01 11:39:35 mclareni Exp $
 *
 * $Log: opencedialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:35  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.07  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        openCeDialog.c
        (Generated from interface file openCeDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/FileSB.h>


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
        Widget  UxopenCeDialog;
        swidget Uxsave_parent;
} _UxCopenCeDialog;

#define openCeDialog            UxOpenCeDialogContext->UxopenCeDialog
#define save_parent             UxOpenCeDialogContext->Uxsave_parent

static _UxCopenCeDialog *UxOpenCeDialogContext;


/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_openCeDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_openCeDialog()
{
        Widget  openCeDialog_shell;

        openCeDialog_shell = XtVaCreatePopupShell( "openCeDialog_shell",
                        xmDialogShellWidgetClass, save_parent,
                        XmNwidth, 355,
                        XmNheight, 435,
                        XmNtitle, "openCeDialog",
                        NULL );

        openCeDialog = XtVaCreateWidget( "openCeDialog",
                        xmFileSelectionBoxWidgetClass, openCeDialog_shell,
                      RES_CONVERT( XmNselectionLabelString, "Enter File Name" ),
                        RES_CONVERT( XmNpattern, "*.cuts" ),
                        RES_CONVERT( XmNdialogTitle, "Open Cuts File" ),
                        XmNheight, 435,
                        XmNwidth, 355,
                        NULL );

        UxPutContext( openCeDialog, (char *) UxOpenCeDialogContext );

        XtAddCallback( openCeDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxOpenCeDialogContext );



        return ( openCeDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_openCeDialog( _Uxsave_parent )
        swidget _Uxsave_parent;
{
        Widget                  rtrn;
        _UxCopenCeDialog        *UxContext;

        UxOpenCeDialogContext = UxContext =
                (_UxCopenCeDialog *) XtMalloc( sizeof(_UxCopenCeDialog) );

        save_parent = _Uxsave_parent;
        rtrn = _Uxbuild_openCeDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_openCeDialog( _Uxsave_parent )
        swidget _Uxsave_parent;
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_openCeDialog( _Uxsave_parent );

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

