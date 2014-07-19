/*
 * $Id: fileselpanel.c,v 1.1.1.1 1996/03/01 11:39:07 mclareni Exp $
 *
 * $Log: fileselpanel.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:07  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.40  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/


/*******************************************************************************
        fileselpanel.c

       Associated Header file: fileselpanel.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/FileSB.h>
#include <X11/Shell.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/fileselpanel.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_fileSelectionShell()
{
        Widget          _UxParent;


        /* Creation of fileSelectionShell */
        _UxParent = UxParent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        fileSelectionShell = XtVaCreatePopupShell( "fileSelectionShell",
                        transientShellWidgetClass,
                        _UxParent,
                        XmNwidth, 4500,
                        XmNheight, 5500,
                        XmNshellUnitType, Xm100TH_FONT_UNITS,
                        XmNtitle, "File Selection Panel",
                        NULL );
        UxPutContext( fileSelectionShell, (char *) UxFileSelectionShellContext
);


        /* Creation of fileSelectionBox */
        fileSelectionBox = XtVaCreateManagedWidget( "fileSelectionBox",
                        xmFileSelectionBoxWidgetClass,
                        fileSelectionShell,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        NULL );
        UxPutContext( fileSelectionBox, (char *) UxFileSelectionShellContext );


        XtAddCallback( fileSelectionShell, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxFileSelectionShellContext);


        return ( fileSelectionShell );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_fileSelectionPanel( swidget _UxUxParent )
{
        Widget                  rtrn;
        _UxCfileSelectionShell  *UxContext;

        UxFileSelectionShellContext = UxContext =
                (_UxCfileSelectionShell *) UxNewContext( sizeof(
_UxCfileSelectionShell),
False );

        UxParent = _UxUxParent;

        rtrn = _Uxbuild_fileSelectionShell();

        XtUnmanageChild(
            XmFileSelectionBoxGetChild(fileSelectionBox, XmDIALOG_HELP_BUTTON)
        );

        return(rtrn);
}
