/*
 * $Id: kuxhlp.c,v 1.1.1.1 1996/03/08 15:33:10 mclareni Exp $
 *
 * $Log: kuxhlp.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:10  mclareni
 * Kuip
 *
 */
#if defined(CERNLIB_OLD)
/*CMZ :  2.03/14 03/09/93  15.21.08  by  Nicole Cremel*/
/*-- Author :    N.Cremel   07/08/92*/
/*
 * kuipHelp.c
 */

/*******************************************************************************
        kuipHelp.c

       Associated Header file: kuipHelp.h
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
/*#include "kuipHelp.h"*/
#include "kuip/mkuxhl.h"
#undef CONTEXT_MACRO_ACCESS

Widget        km_hlpOK;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget        _Uxbuild_kuipHelp()
{
        Widget                _UxParent;


        /* Creation of kuipHelp */
        _UxParent = helpParent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        _UxParent = XtVaCreatePopupShell( "kuipHelp_shell",
                        xmDialogShellWidgetClass, _UxParent,
                        XmNwidth, 500,
                        XmNheight, 350,
                        XmNtitle, "kuipHelp",
                        NULL );

        kuipHelp = XtVaCreateWidget( "kuipHelp",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNwidth, 500,
                        XmNheight, 350,
                        XmNautoUnmanage, FALSE,
                        RES_CONVERT( XmNdialogTitle, "Help" ),
                        NULL );
        UxPutContext( kuipHelp, (char *) UxKuipHelpContext );


        /* Creation of km_hlpOK */
        km_hlpOK = XtVaCreateManagedWidget( "km_hlpOK",
                        xmPushButtonGadgetClass,
                        kuipHelp,
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
        UxPutContext( km_hlpOK, (char *) UxKuipHelpContext );


        /* Creation of helpSep */
        helpSep = XtVaCreateManagedWidget( "helpSep",
                        xmSeparatorGadgetClass,
                        kuipHelp,
                        XmNwidth, 780,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNbottomWidget, km_hlpOK,
                        NULL );
        UxPutContext( helpSep, (char *) UxKuipHelpContext );


        /* Creation of helpScWindow */
        helpScWindow = XtVaCreateManagedWidget( "helpScWindow",
                        xmScrolledWindowWidgetClass,
                        kuipHelp,
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
                        XmNbottomWidget, helpSep,
                        NULL );
        UxPutContext( helpScWindow, (char *) UxKuipHelpContext );


        /* Creation of hlpText */
        hlpText = XtVaCreateManagedWidget( "hlpText",
                        xmTextWidgetClass,
                        helpScWindow,
                        XmNx, 68,
                        XmNy, 0,
                        XmNwidth, 560,
                        XmNheight, 350,
                        XmNeditable, FALSE,
                        XmNeditMode, XmMULTI_LINE_EDIT ,
                        NULL );
        UxPutContext( hlpText, (char *) UxKuipHelpContext );

        XtVaSetValues(kuipHelp,
                        XmNdefaultButton, km_hlpOK,
                        NULL );


        XtAddCallback( kuipHelp, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxKuipHelpContext);


        return ( kuipHelp );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget        create_kuipHelp( _UxhelpParent )
        swidget        _UxhelpParent;
{
        Widget                  rtrn;
        _UxCkuipHelp            *UxContext;

        UxKuipHelpContext = UxContext =
                (_UxCkuipHelp *) UxNewContext( sizeof(_UxCkuipHelp), False );

        helpParent = _UxhelpParent;

        rtrn = _Uxbuild_kuipHelp();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

#endif
