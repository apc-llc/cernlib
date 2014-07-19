/*
 * $Id: kuxfse.c,v 1.1.1.1 1996/03/08 15:33:10 mclareni Exp $
 *
 * $Log: kuxfse.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:10  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/09 15/04/94  17.04.45  by  N.Cremel*/
/*-- Author :    N.Cremel   28/06/93*/
/*
 *  kuipFilSel.c
 */

/*******************************************************************************
       kuipFilSel.c

       Associated Header file: kuipFilSel.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/FileSB.h>
#include <Xm/Form.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "kuipFilSel.h" */
#include "mkuxfs.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget       _Uxbuild_kuipFilSel()
{
       Widget              _UxParent;


       /* Creation of kuipFilSel */
       _UxParent = fileParent;
       if ( _UxParent == NULL )
       {
              _UxParent = UxTopLevel;
       }

       _UxParent = XtVaCreatePopupShell( "kuipFilSel_shell",
                     xmDialogShellWidgetClass, _UxParent,
                     XmNwidth, 300,
                     XmNshellUnitType, XmPIXELS,
                     XmNtitle, "kuipFilSel",
                     NULL );

       kuipFilSel = XtVaCreateWidget( "kuipFilSel",
                     xmFormWidgetClass,
                     _UxParent,
                     XmNunitType, XmPIXELS,
                     XmNdefaultPosition, FALSE,
                     XmNwidth, 300,
                     NULL );
       UxPutContext( kuipFilSel, (char *) UxKuipFilSelContext );


       /* Creation of km_FilSel */
       km_FilSel = XtVaCreateManagedWidget( "km_FilSel",
                     xmFileSelectionBoxWidgetClass,
                     kuipFilSel,
                     XmNresizePolicy, XmRESIZE_NONE,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNtopAttachment, XmATTACH_FORM,
                     NULL );
       UxPutContext( km_FilSel, (char *) UxKuipFilSelContext );


       XtAddCallback( kuipFilSel, XmNdestroyCallback,
              (XtCallbackProc) UxDestroyContextCB,
              (XtPointer) UxKuipFilSelContext);


       return ( kuipFilSel );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget       create_kuipFilSel( _UxfileParent )
       swidget       _UxfileParent;
{
       Widget                  rtrn;
       _UxCkuipFilSel          *UxContext;

       UxKuipFilSelContext = UxContext =
              (_UxCkuipFilSel *) UxNewContext( sizeof(_UxCkuipFilSel), False );

       fileParent = _UxfileParent;

       rtrn = _Uxbuild_kuipFilSel();

       return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

