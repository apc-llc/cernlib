/*
 * $Id: kuxlishlp.c,v 1.1.1.1 1996/03/08 15:33:10 mclareni Exp $
 *
 * $Log: kuxlishlp.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:10  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/07 22/06/95  14.04.47  by  N.Cremel*/
/*-- Author :    N.Cremel   23/02/95*/
/*
 * kuipListHelp.c
 */

/*******************************************************************************
        kuipListHelp.c

       Associated Header file: kuipListHelp.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/Text.h>
#include <Xm/SelectioB.h>
#include <Xm/Form.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "kuipListHelp.h" */
#include "mkuxlihlp.h"
/* for km_listSBox + add line "km_listSBox = km_listSBoxHelp;" */
#include "mkuxli.h"
#undef CONTEXT_MACRO_ACCESS

Widget        km_listSBoxHelp;
Widget        km_listText;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget        _Uxbuild_kuipListHelp()
{
        Widget                _UxParent;


        /* Creation of kuipListHelp */
        _UxParent = listHelpParent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        _UxParent = XtVaCreatePopupShell( "kuipListHelp_shell",
                        xmDialogShellWidgetClass, _UxParent,
                        XmNx, 350,
                        XmNy, 600,
                        XmNwidth, 400,
                        XmNheight, 400,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "kuipListHelp",
                        NULL );

        kuipListHelp = XtVaCreateWidget( "kuipListHelp",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNunitType, XmPIXELS,
                        XmNwidth, 400,
                        XmNheight, 400,
                        NULL );
        UxPutContext( kuipListHelp, (char *) UxKuipListHelpContext );


        /* Creation of km_listSBoxHelp */
        km_listSBoxHelp = XtVaCreateManagedWidget( "km_listSBoxHelp",
                        xmSelectionBoxWidgetClass,
                        kuipListHelp,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 0,
                        XmNy, 20,
                        XmNwidth, 270,
                        XmNheight, 220,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        RES_CONVERT( XmNlistLabelString, "List of Commands" ),
                        RES_CONVERT( XmNselectionLabelString, "Command" ),
                        RES_CONVERT( XmNhelpLabelString, "Quick Help" ),
                        RES_CONVERT( XmNokLabelString, "Full Help" ),
                        NULL );
        UxPutContext( km_listSBoxHelp, (char *) UxKuipListHelpContext );
        km_listSBox = km_listSBoxHelp;


        /* Creation of km_listText */
        km_listText = XtVaCreateManagedWidget( "km_listText",
                        xmTextWidgetClass,
                        km_listSBoxHelp,
                        NULL );
        UxPutContext( km_listText, (char *) UxKuipListHelpContext );


        XtAddCallback( kuipListHelp, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxKuipListHelpContext);


        return ( kuipListHelp );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget        create_kuipListHelp( _UxlistHelpParent )
        swidget        _UxlistHelpParent;
{
        Widget                  rtrn;
        _UxCkuipListHelp        *UxContext;

        UxKuipListHelpContext = UxContext =
                (_UxCkuipListHelp *) UxNewContext( sizeof(_UxCkuipListHelp), 
False );

        listHelpParent = _UxlistHelpParent;

        rtrn = _Uxbuild_kuipListHelp();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

