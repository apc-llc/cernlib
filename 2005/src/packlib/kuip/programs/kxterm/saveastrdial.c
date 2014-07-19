/*
 * $Id: saveastrdial.c,v 1.1.1.1 1996/03/08 15:33:04 mclareni Exp $
 *
 * $Log: saveastrdial.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:04  mclareni
 * Kuip
 *
 */
/*CMZ :  2.00/10 19/01/93  17.50.58  by  Rene Brun*/
/*-- Author :*/

/*******************************************************************************
        saveAsTrDialog.c
        (Generated from interface file saveAsTrDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "uxxt.h"

#include <Xm/FileSB.h>

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
        Widget  UxsaveAsTrDialog;
} _UxCsaveAsTrDialog;

#define saveAsTrDialog          UxSaveAsTrDialogContext->UxsaveAsTrDialog

static _UxCsaveAsTrDialog       *UxSaveAsTrDialogContext;


/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_saveAsTrDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_saveAsTrDialog()
{
        Widget  saveAsTrDialog_shell;

        saveAsTrDialog_shell = XtVaCreatePopupShell( "saveAsTrDialog_shell",
                        xmDialogShellWidgetClass, kuipIo,
                        XmNx, 415,
                        XmNy, 300,
                        XmNwidth, 380,
                        XmNheight, 400,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "saveAsTrDialog",
                        NULL );

        saveAsTrDialog = XtVaCreateWidget( "saveAsTrDialog",
                        xmFileSelectionBoxWidgetClass, saveAsTrDialog_shell,
                    RES_CONVERT( XmNselectionLabelString, "Enter File Name" ),
                        RES_CONVERT( XmNdirMask, "*.pad" ),
                        RES_CONVERT( XmNdialogTitle, "Save Transcript Pad" ),
                        XmNheight, 400,
                        XmNwidth, 380,
                        XmNunitType, XmPIXELS,
                        NULL );

        UxPutContext( saveAsTrDialog, (char *) UxSaveAsTrDialogContext );

        XtAddCallback( saveAsTrDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxSaveAsTrDialogContext );



        return ( saveAsTrDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_saveAsTrDialog()
{
        Widget                  rtrn;
        _UxCsaveAsTrDialog      *UxContext;

        UxSaveAsTrDialogContext = UxContext =
                (_UxCsaveAsTrDialog *) XtMalloc( sizeof(_UxCsaveAsTrDialog) );

        rtrn = _Uxbuild_saveAsTrDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_saveAsTrDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_saveAsTrDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

