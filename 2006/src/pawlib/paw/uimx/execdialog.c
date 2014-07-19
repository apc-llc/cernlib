/*
 * $Id: execdialog.c,v 1.1.1.1 1996/03/01 11:39:33 mclareni Exp $
 *
 * $Log: execdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:33  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.05  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        execDialog.c
        (Generated from interface file execDialog.i)
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
        swidget Uxexec_parent;
} _UxCexecDialog;

#define exec_parent             UxExecDialogContext->Uxexec_parent

static _UxCexecDialog   *UxExecDialogContext;

Widget  execDialog;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_execDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_execDialog()
{
        Widget  execDialog_shell;

        execDialog_shell = XtVaCreatePopupShell( "execDialog_shell",
                        xmDialogShellWidgetClass, exec_parent,
                        XmNx, 231,
                        XmNy, 193,
                        XmNwidth, 390,
                        XmNheight, 416,
                        XmNtitle, "execDialog",
                        NULL );

        execDialog = XtVaCreateWidget( "execDialog",
                        xmFileSelectionBoxWidgetClass, execDialog_shell,
                        RES_CONVERT( XmNdirMask, "*.kumac" ),
                   RES_CONVERT( XmNselectionLabelString, "Execute PAW Macro" ),
                        RES_CONVERT( XmNdialogTitle, "Exec Arguments" ),
                        XmNheight, 416,
                        XmNwidth, 390,
                        NULL );

        UxPutContext( execDialog, (char *) UxExecDialogContext );

        XtAddCallback( execDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxExecDialogContext );



        return ( execDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_execDialog( _Uxexec_parent )
        swidget _Uxexec_parent;
{
        Widget                  rtrn;
        _UxCexecDialog          *UxContext;

        UxExecDialogContext = UxContext =
                (_UxCexecDialog *) XtMalloc( sizeof(_UxCexecDialog) );

        exec_parent = _Uxexec_parent;
        rtrn = _Uxbuild_execDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_execDialog( _Uxexec_parent )
        swidget _Uxexec_parent;
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_execDialog( _Uxexec_parent );

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

