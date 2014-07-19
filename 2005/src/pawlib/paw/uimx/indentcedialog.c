/*
 * $Id: indentcedialog.c,v 1.1.1.1 1996/03/01 11:39:34 mclareni Exp $
 *
 * $Log: indentcedialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:34  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.06  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        indentCeDialog.c
        (Generated from interface file indentCeDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/SelectioB.h>


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
        Widget  UxindentCeDialog;
        swidget Uxcut_editor;
} _UxCindentCeDialog;

#define indentCeDialog          UxIndentCeDialogContext->UxindentCeDialog
#define cut_editor              UxIndentCeDialogContext->Uxcut_editor

static _UxCindentCeDialog       *UxIndentCeDialogContext;


/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_indentCeDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_indentCeDialog()
{
        Widget  indentCeDialog_shell;

        indentCeDialog_shell = XtVaCreatePopupShell( "indentCeDialog_shell",
                        xmDialogShellWidgetClass, cut_editor,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "indentCeDialog",
                        NULL );

        indentCeDialog = XtVaCreateWidget( "indentCeDialog",
                        xmSelectionBoxWidgetClass, indentCeDialog_shell,
                        RES_CONVERT( XmNtextString, "15" ),
                        RES_CONVERT( XmNselectionLabelString, "Indentation" ),
                        RES_CONVERT( XmNdialogTitle, "Indentation" ),
                        XmNautoUnmanage, FALSE,
                        XmNunitType, XmPIXELS,
                        XmNdialogType, XmDIALOG_PROMPT,
                        NULL );

        UxPutContext( indentCeDialog, (char *) UxIndentCeDialogContext );

        XtAddCallback( indentCeDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxIndentCeDialogContext );



        return ( indentCeDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_indentCeDialog( _Uxcut_editor )
        swidget _Uxcut_editor;
{
        Widget                  rtrn;
        _UxCindentCeDialog      *UxContext;

        UxIndentCeDialogContext = UxContext =
                (_UxCindentCeDialog *) XtMalloc( sizeof(_UxCindentCeDialog) );

        cut_editor = _Uxcut_editor;
        rtrn = _Uxbuild_indentCeDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_indentCeDialog( _Uxcut_editor )
        swidget _Uxcut_editor;
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_indentCeDialog( _Uxcut_editor );

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

