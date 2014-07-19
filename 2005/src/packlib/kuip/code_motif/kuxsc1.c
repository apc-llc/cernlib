/*
 * $Id: kuxsc1.c,v 1.1.1.1 1996/03/08 15:33:11 mclareni Exp $
 *
 * $Log: kuxsc1.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:11  mclareni
 * Kuip
 *
 */
/*CMZ :  2.03/14 03/09/93  15.31.22  by  Nicole Cremel*/
/*-- Author :    N.Cremel   07/08/92*/
/*
 * kuipScroll1.c
 */

/*******************************************************************************
        kuipScroll1.c

       Associated Header file: kuipScroll1.h
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
/*#include "kuipScroll1.h"*/
#include "mkuxs1.h"
#undef CONTEXT_MACRO_ACCESS

Widget        km_scrollOK1;
Widget        km_scrollCL1;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget        _Uxbuild_kuipScroll1()
{
        Widget                _UxParent;


        /* Creation of kuipScroll1 */
        _UxParent = scroll1Parent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        _UxParent = XtVaCreatePopupShell( "kuipScroll1_shell",
                        xmDialogShellWidgetClass, _UxParent,
                        XmNwidth, 500,
                        XmNheight, 350,
                        XmNtitle, "kuipScroll1",
                        NULL );

        kuipScroll1 = XtVaCreateWidget( "kuipScroll1",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNwidth, 500,
                        XmNheight, 350,
                        XmNautoUnmanage, FALSE,
                        NULL );
        UxPutContext( kuipScroll1, (char *) UxKuipScroll1Context );


        /* Creation of km_scrollOK1 */
        km_scrollOK1 = XtVaCreateManagedWidget( "km_scrollOK1",
                        xmPushButtonGadgetClass,
                        kuipScroll1,
                        XmNwidth, 60,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "OK" ),
                        XmNshowAsDefault, 1,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNdefaultButtonShadowThickness, 1,
                        NULL );
        UxPutContext( km_scrollOK1, (char *) UxKuipScroll1Context );


        /* Creation of scrollSep1 */
        scrollSep1 = XtVaCreateManagedWidget( "scrollSep1",
                        xmSeparatorGadgetClass,
                        kuipScroll1,
                        XmNwidth, 780,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNbottomWidget, km_scrollOK1,
                        NULL );
        UxPutContext( scrollSep1, (char *) UxKuipScroll1Context );


        /* Creation of ScWindow1 */
        ScWindow1 = XtVaCreateManagedWidget( "ScWindow1",
                        xmScrolledWindowWidgetClass,
                        kuipScroll1,
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
                        XmNbottomWidget, scrollSep1,
                        NULL );
        UxPutContext( ScWindow1, (char *) UxKuipScroll1Context );


        /* Creation of scrollText1 */
        scrollText1 = XtVaCreateManagedWidget( "scrollText1",
                        xmTextWidgetClass,
                        ScWindow1,
                        XmNx, 68,
                        XmNy, 0,
                        XmNwidth, 560,
                        XmNheight, 350,
                        XmNeditable, FALSE,
                        XmNeditMode, XmMULTI_LINE_EDIT ,
                        NULL );
        UxPutContext( scrollText1, (char *) UxKuipScroll1Context );


        /* Creation of km_scrollCL1 */
        km_scrollCL1 = XtVaCreateManagedWidget( "km_scrollCL1",
                        xmPushButtonGadgetClass,
                        kuipScroll1,
                        XmNwidth, 60,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 10,
                        RES_CONVERT( XmNlabelString, " Clear " ),
                        NULL );
        UxPutContext( km_scrollCL1, (char *) UxKuipScroll1Context );

        XtVaSetValues(kuipScroll1,
                        XmNdefaultButton, km_scrollOK1,
                        NULL );


        XtAddCallback( kuipScroll1, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxKuipScroll1Context);


        return ( kuipScroll1 );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget        create_kuipScroll1( _Uxscroll1Parent )
        swidget        _Uxscroll1Parent;
{
        Widget                  rtrn;
        _UxCkuipScroll1         *UxContext;

        UxKuipScroll1Context = UxContext =
            (_UxCkuipScroll1 *) UxNewContext( sizeof(_UxCkuipScroll1), False );

        scroll1Parent = _Uxscroll1Parent;

        rtrn = _Uxbuild_kuipScroll1();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

