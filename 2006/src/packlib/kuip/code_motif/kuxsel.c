/*
 * $Id: kuxsel.c,v 1.1.1.1 1996/03/08 15:33:11 mclareni Exp $
 *
 * $Log: kuxsel.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:11  mclareni
 * Kuip
 *
 */
/*CMZ :  2.03/14 07/09/93  15.33.42  by  Nicole Cremel*/
/*-- Author :    Nicole Cremel   02/09/93*/
/*
 *  kuipSelect.c
 */
/*******************************************************************************
        kuipSelect.c

       Associated Header file: kuipSelect.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/PushBG.h>
#include <Xm/MessageB.h>
#include <Xm/Form.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "kuipSelect.h" */
#include "mkuxse.h"
#undef CONTEXT_MACRO_ACCESS

Widget        km_messageBox;
Widget        km_goButton;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget        _Uxbuild_kuipSelect()
{
        Widget                _UxParent;


        /* Creation of kuipSelect */
        _UxParent = UxParent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        _UxParent = XtVaCreatePopupShell( "kuipSelect_shell",
                        xmDialogShellWidgetClass, _UxParent,
                        XmNheight, 200,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "kuipSelect",
                        NULL );

        kuipSelect = XtVaCreateWidget( "kuipSelect",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNunitType, XmPIXELS,
                        XmNheight, 200,
                        NULL );
        UxPutContext( kuipSelect, (char *) UxKuipSelectContext );


        /* Creation of km_messageBox */
        km_messageBox = XtVaCreateManagedWidget( "km_messageBox",
                        xmMessageBoxWidgetClass,
                        kuipSelect,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( km_messageBox, (char *) UxKuipSelectContext );


        /* Creation of km_goButton */
        km_goButton = XtVaCreateManagedWidget( "km_goButton",
                        xmPushButtonGadgetClass,
                        km_messageBox,
                        XmNwidth, 40,
                        XmNheight, 20,
                        RES_CONVERT( XmNlabelString, " Go " ),
                        NULL );
        UxPutContext( km_goButton, (char *) UxKuipSelectContext );


        XtAddCallback( kuipSelect, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxKuipSelectContext);


        return ( kuipSelect );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget        create_kuipSelect( _UxUxParent )
        swidget        _UxUxParent;
{
        Widget                  rtrn;
        _UxCkuipSelect          *UxContext;

        UxKuipSelectContext = UxContext =
              (_UxCkuipSelect *) UxNewContext( sizeof(_UxCkuipSelect), False );

        UxParent = _UxUxParent;

        rtrn = _Uxbuild_kuipSelect();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

