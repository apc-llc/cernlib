/*
 * $Id: saveascedialog.c,v 1.1.1.1 1996/03/01 11:39:35 mclareni Exp $
 *
 * $Log: saveascedialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:35  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.07  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        saveAsCeDialog.c
        (Generated from interface file saveAsCeDialog.i)
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
        Widget  UxsaveAsCeDialog;
        swidget Uxopen_parent;
} _UxCsaveAsCeDialog;

#define saveAsCeDialog          UxSaveAsCeDialogContext->UxsaveAsCeDialog
#define open_parent             UxSaveAsCeDialogContext->Uxopen_parent

static _UxCsaveAsCeDialog       *UxSaveAsCeDialogContext;


/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_saveAsCeDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_saveAsCeDialog()
{
        Widget  saveAsCeDialog_shell;

        saveAsCeDialog_shell = XtVaCreatePopupShell( "saveAsCeDialog_shell",
                        xmDialogShellWidgetClass, open_parent,
                        XmNwidth, 375,
                        XmNheight, 420,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "saveAsCeDialog",
                        NULL );

        saveAsCeDialog = XtVaCreateWidget( "saveAsCeDialog",
                        xmFileSelectionBoxWidgetClass, saveAsCeDialog_shell,
                     RES_CONVERT( XmNselectionLabelString, "Enter File Name" ),
                        RES_CONVERT( XmNpattern, "*.cuts" ),
                        RES_CONVERT( XmNdialogTitle, "Save Cuts" ),
                        XmNheight, 420,
                        XmNwidth, 375,
                        XmNunitType, XmPIXELS,
                        NULL );

        UxPutContext( saveAsCeDialog, (char *) UxSaveAsCeDialogContext );

        XtAddCallback( saveAsCeDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxSaveAsCeDialogContext );



        return ( saveAsCeDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_saveAsCeDialog( _Uxopen_parent )
        swidget _Uxopen_parent;
{
        Widget                  rtrn;
        _UxCsaveAsCeDialog      *UxContext;

        UxSaveAsCeDialogContext = UxContext =
                (_UxCsaveAsCeDialog *) XtMalloc( sizeof(_UxCsaveAsCeDialog) );

        open_parent = _Uxopen_parent;
        rtrn = _Uxbuild_saveAsCeDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_saveAsCeDialog( _Uxopen_parent )
        swidget _Uxopen_parent;
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_saveAsCeDialog( _Uxopen_parent );

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

