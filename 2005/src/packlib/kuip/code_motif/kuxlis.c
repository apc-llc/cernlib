/*
 * $Id: kuxlis.c,v 1.1.1.1 1996/03/08 15:33:10 mclareni Exp $
 *
 * $Log: kuxlis.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:10  mclareni
 * Kuip
 *
 */
/*CMZ :  2.03/14 03/09/93  15.22.50  by  Nicole Cremel*/
/*-- Author :*/
/*
 * kuipList.c
 */

/*******************************************************************************
        kuipList.c

       Associated Header file: kuipList.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/SelectioB.h>
#include <Xm/Form.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "kuipList.h" */
#include "mkuxli.h"
#undef CONTEXT_MACRO_ACCESS

Widget        km_listSBox;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget        _Uxbuild_kuipList()
{
        Widget                _UxParent;


        /* Creation of kuipList */
        _UxParent = listParent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        _UxParent = XtVaCreatePopupShell( "kuipList_shell",
                        xmDialogShellWidgetClass, _UxParent,
                        XmNx, 350,
                        XmNy, 600,
                        XmNwidth, 270,
                        XmNheight, 280,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "kuipList",
                        NULL );

        kuipList = XtVaCreateWidget( "kuipList",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNunitType, XmPIXELS,
                        XmNwidth, 270,
                        XmNheight, 280,
                        NULL );
        UxPutContext( kuipList, (char *) UxKuipListContext );


        /* Creation of km_listSBox */
        km_listSBox = XtVaCreateManagedWidget( "km_listSBox",
                        xmSelectionBoxWidgetClass,
                        kuipList,
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
                        NULL );
        UxPutContext( km_listSBox, (char *) UxKuipListContext );


        XtAddCallback( kuipList, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxKuipListContext);


        return ( kuipList );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget        create_kuipList( _UxlistParent )
        swidget        _UxlistParent;
{
        Widget                  rtrn;
        _UxCkuipList            *UxContext;

        UxKuipListContext = UxContext =
                (_UxCkuipList *) UxNewContext( sizeof(_UxCkuipList), False );

        listParent = _UxlistParent;

        rtrn = _Uxbuild_kuipList();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

