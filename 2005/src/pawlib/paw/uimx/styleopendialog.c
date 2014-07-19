/*
 * $Id: styleopendialog.c,v 1.1.1.1 1996/03/01 11:39:35 mclareni Exp $
 *
 * $Log: styleopendialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:35  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.07  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        styleOpenDialog.c
        (Generated from interface file styleOpenDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/FileSB.h>

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
        Widget  UxstyleOpenDialog;
} _UxCstyleOpenDialog;

#define styleOpenDialog         UxStyleOpenDialogContext->UxstyleOpenDialog

static _UxCstyleOpenDialog      *UxStyleOpenDialogContext;


/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_styleOpenDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_styleOpenDialog()
{
        Widget  styleOpenDialog_shell;

        styleOpenDialog_shell = XtVaCreatePopupShell( "styleOpenDialog_shell",
                        xmDialogShellWidgetClass, histoStyle,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "styleOpenDialog",
                        NULL );

        styleOpenDialog = XtVaCreateWidget( "styleOpenDialog",
                        xmFileSelectionBoxWidgetClass, styleOpenDialog_shell,
           RES_CONVERT( XmNselectionLabelString, "Enter the style file name" ),
                        RES_CONVERT( XmNpattern, "*.style" ),
                        RES_CONVERT( XmNdialogTitle, "Open Style" ),
                        XmNunitType, XmPIXELS,
                        NULL );

        UxPutContext( styleOpenDialog, (char *) UxStyleOpenDialogContext );

        XtAddCallback( styleOpenDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxStyleOpenDialogContext );



        return ( styleOpenDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_styleOpenDialog()
{
        Widget                  rtrn;
        _UxCstyleOpenDialog     *UxContext;

        UxStyleOpenDialogContext = UxContext =
                (_UxCstyleOpenDialog *) XtMalloc( sizeof(_UxCstyleOpenDialog) );

        rtrn = _Uxbuild_styleOpenDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_styleOpenDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_styleOpenDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

