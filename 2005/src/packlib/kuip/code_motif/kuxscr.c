/*
 * $Id: kuxscr.c,v 1.1.1.1 1996/03/08 15:33:11 mclareni Exp $
 *
 * $Log: kuxscr.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:11  mclareni
 * Kuip
 *
 */
/*CMZ :  2.03/14 03/09/93  15.30.05  by  Nicole Cremel*/
/*-- Author :    N.Cremel   07/08/92*/
/*
 * kuipScroll.c
 */

/*******************************************************************************
        kuipScroll.c

       Associated Header file: kuipScroll.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/*#include "UxXt.h"*/
#include "mkuxxt.h"

#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/Form.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/*#include "kuipScroll.h"*/
#include "mkuxsc.h"
#undef CONTEXT_MACRO_ACCESS

Widget        km_scrollOK;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget        _Uxbuild_kuipScroll()
{
        Widget                _UxParent;


        /* Creation of kuipScroll */
        _UxParent = scrollParent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        _UxParent = XtVaCreatePopupShell( "kuipScroll_shell",
                        xmDialogShellWidgetClass, _UxParent,
                        XmNwidth, 500,
                        XmNheight, 350,
                        XmNtitle, "kuipScroll",
                        NULL );

        kuipScroll = XtVaCreateWidget( "kuipScroll",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNwidth, 500,
                        XmNheight, 350,
                        XmNautoUnmanage, FALSE,
                        NULL );
        UxPutContext( kuipScroll, (char *) UxKuipScrollContext );


        /* Creation of km_scrollOK */
        km_scrollOK = XtVaCreateManagedWidget( "km_scrollOK",
                        xmPushButtonGadgetClass,
                        kuipScroll,
                        XmNwidth, 60,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "OK" ),
                        XmNshowAsDefault, 1,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftOffset, -30,
                        XmNleftPosition, 50,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNdefaultButtonShadowThickness, 1,
                        NULL );
        UxPutContext( km_scrollOK, (char *) UxKuipScrollContext );


        /* Creation of scrollSep */
        scrollSep = XtVaCreateManagedWidget( "scrollSep",
                        xmSeparatorGadgetClass,
                        kuipScroll,
                        XmNwidth, 780,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNbottomWidget, km_scrollOK,
                        NULL );
        UxPutContext( scrollSep, (char *) UxKuipScrollContext );


        /* Creation of ScWindow */
        ScWindow = XtVaCreateManagedWidget( "ScWindow",
                        xmScrolledWindowWidgetClass,
                        kuipScroll,
                        XmNshadowThickness, 0,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        XmNbottomWidget, scrollSep,
                        NULL );
        UxPutContext( ScWindow, (char *) UxKuipScrollContext );


        /* Creation of scrollText */
        scrollText = XtVaCreateManagedWidget( "scrollText",
                        xmTextWidgetClass,
                        ScWindow,
                        XmNx, 68,
                        XmNy, 0,
                        XmNwidth, 560,
                        XmNheight, 350,
                        XmNeditable, FALSE,
                        XmNeditMode, XmMULTI_LINE_EDIT ,
                        NULL );
        UxPutContext( scrollText, (char *) UxKuipScrollContext );

        XtVaSetValues(kuipScroll,
                        XmNdefaultButton, km_scrollOK,
                        NULL );


        XtAddCallback( kuipScroll, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxKuipScrollContext);


        return ( kuipScroll );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget        create_kuipScroll( _UxscrollParent )
        swidget        _UxscrollParent;
{
        Widget                  rtrn;
        _UxCkuipScroll          *UxContext;

        UxKuipScrollContext = UxContext =
              (_UxCkuipScroll *) UxNewContext( sizeof(_UxCkuipScroll), False );

        scrollParent = _UxscrollParent;

        rtrn = _Uxbuild_kuipScroll();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

