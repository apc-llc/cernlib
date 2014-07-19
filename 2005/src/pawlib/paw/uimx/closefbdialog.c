/*
 * $Id: closefbdialog.c,v 1.1.1.1 1996/03/01 11:39:32 mclareni Exp $
 *
 * $Log: closefbdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:32  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.05  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        closeFbDialog.c
        (Generated from interface file closeFbDialog.i)
        Associated Header file: closeFbDialog.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/SelectioB.h>


/*******************************************************************************
        The following header file defines the context structure.
*******************************************************************************/

#include "hmotif/closefbdialog.h"


/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_closeFbDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_closeFbDialog()
{
        Widget  closeFbDialog_shell;

        closeFbDialog_shell = XtVaCreatePopupShell( "closeFbDialog_shell",
                        xmDialogShellWidgetClass, close_parent,
                        XmNx, 360,
                        XmNy, 400,
                        XmNwidth, 380,
                        XmNheight, 340,
                        XmNtitle, "closeFbDialog",
                        NULL );

        closeFbDialog = XtVaCreateWidget( "closeFbDialog",
                        xmSelectionBoxWidgetClass, closeFbDialog_shell,
                        RES_CONVERT( XmNselectionLabelString, "Close File" ),
                        RES_CONVERT( XmNlistLabelString, "Connected Files" ),
                        RES_CONVERT( XmNapplyLabelString, "Close" ),
                        RES_CONVERT( XmNdialogTitle, "Close Arguments" ),
                        XmNautoUnmanage, FALSE,
                        XmNheight, 340,
                        XmNwidth, 380,
                        NULL );

        UxPutContext( closeFbDialog, (char *) UxCloseFbDialogContext );

        XtAddCallback( closeFbDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxCloseFbDialogContext );



        return ( closeFbDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_closeFbDialog( _Uxclose_parent )
        swidget _Uxclose_parent;
{
        Widget                  rtrn;
        _UxCcloseFbDialog       *UxContext;

        UxCloseFbDialogContext = UxContext =
                (_UxCcloseFbDialog *) XtMalloc( sizeof(_UxCcloseFbDialog) );

        close_parent = _Uxclose_parent;
        rtrn = _Uxbuild_closeFbDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_closeFbDialog( _Uxclose_parent )
        swidget _Uxclose_parent;
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_closeFbDialog( _Uxclose_parent );

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

