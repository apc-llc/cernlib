/*
 * $Id: kuxolt.c,v 1.1.1.1 1996/03/08 15:33:10 mclareni Exp $
 *
 * $Log: kuxolt.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:10  mclareni
 * Kuip
 *
 */
/*CMZ :  2.03/14 03/09/93  15.32.44  by  Nicole Cremel*/
/*-- Author :    N.Cremel   28/06/93*/
/*
 *  kuipOptList.c
 */

/*******************************************************************************
        kuipOptList.c

       Associated Header file: kuipOptList.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/Form.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "kuipOptList.h" */
#include "mkuxol.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget        _Uxbuild_kuipOptList()
{
        Widget                _UxParent;


        /* Creation of kuipOptList */
        _UxParent = XtVaCreatePopupShell( "kuipOptList_shell",
                        xmDialogShellWidgetClass, UxTopLevel,
                        XmNwidth, 200,
                        XmNheight, 250,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "kuipOptList",
                        NULL );

        kuipOptList = XtVaCreateWidget( "kuipOptList",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNunitType, XmPIXELS,
                        XmNwidth, 200,
                        XmNheight, 250,
                        XmNdefaultPosition, FALSE,
                        NULL );
        UxPutContext( kuipOptList, (char *) UxKuipOptListContext );


        /* Creation of OptListSep */
        OptListSep = XtVaCreateManagedWidget( "OptListSep",
                        xmSeparatorGadgetClass,
                        kuipOptList,
                        XmNheight, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 45,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        NULL );
        UxPutContext( OptListSep, (char *) UxKuipOptListContext );


        /* Creation of OKButton */
        OKButton = XtVaCreateManagedWidget( "OKButton",
                        xmPushButtonGadgetClass,
                        kuipOptList,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, " OK " ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNwidth, 60,
                        NULL );
        UxPutContext( OKButton, (char *) UxKuipOptListContext );


        /* Creation of CCLButton */
        CCLButton = XtVaCreateManagedWidget( "CCLButton",
                        xmPushButtonGadgetClass,
                        kuipOptList,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        RES_CONVERT( XmNlabelString, " Cancel " ),
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 10,
                        XmNwidth, 60,
                        NULL );
        UxPutContext( CCLButton, (char *) UxKuipOptListContext );


        /* Creation of scrolledWindow1 */
        scrolledWindow1 = XtVaCreateManagedWidget( "scrolledWindow1",
                        xmScrolledWindowWidgetClass,
                        kuipOptList,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        XmNheight, 260,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 5,
                        XmNbottomWidget, OptListSep,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopOffset, 5,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( scrolledWindow1, (char *) UxKuipOptListContext );


        /* Creation of OptList */
        OptList = XtVaCreateManagedWidget( "OptList",
                        xmListWidgetClass,
                        scrolledWindow1,
                        XmNselectionPolicy, XmEXTENDED_SELECT,
                        XmNlistSizePolicy, XmCONSTANT,
                        NULL );
        UxPutContext( OptList, (char *) UxKuipOptListContext );


        XtAddCallback( kuipOptList, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxKuipOptListContext);


        return ( kuipOptList );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget        create_kuipOptList()
{
        Widget                  rtrn;
        _UxCkuipOptList         *UxContext;

        UxKuipOptListContext = UxContext =
            (_UxCkuipOptList *) UxNewContext( sizeof(_UxCkuipOptList), False );


        rtrn = _Uxbuild_kuipOptList();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

