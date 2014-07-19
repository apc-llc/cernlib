/*
 * $Id: uxpanel.c,v 1.1.1.1 1996/03/08 15:33:04 mclareni Exp $
 *
 * $Log: uxpanel.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:04  mclareni
 * Kuip
 *
 */
/*CMZ :  2.04/00 28/09/93  15.08.20  by  Nicole Cremel*/
/*-- Author :    Nicole Cremel   28/09/93*/
/*
 * kuipPanel.c
 */

/*******************************************************************************
        kuipPanel.c

       Associated Header file: kuipPanel.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "uxxt.h"

#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/SeparatoG.h>
#include <Xm/Form.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "kuipPanel.h" */
#include "uxpan.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget        _Uxbuild_kuipPanel()
{
        Widget                _UxParent;


        /* Creation of kuipPanel */
        _UxParent = XtVaCreatePopupShell( "kuipPanel_shell",
                        topLevelShellWidgetClass, UxTopLevel,
                        XmNtitle, "kuipPanel",
                        XmNiconName, "kuipPanel",
                        NULL );

        kuipPanel = XtVaCreateManagedWidget( "kuipPanel",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNdefaultPosition, FALSE,
                        NULL );
        UxPutContext( kuipPanel, (char *) UxKuipPanelContext );


        /* Creation of separatorPanel */
        separatorPanel = XtVaCreateManagedWidget( "separatorPanel",
                        xmSeparatorGadgetClass,
                        kuipPanel,
                        XmNheight, 10,
                        XmNbottomOffset, 45,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        NULL );
        UxPutContext( separatorPanel, (char *) UxKuipPanelContext );


        /* Creation of km_scrollPNW */
        km_scrollPNW = XtVaCreateManagedWidget( "km_scrollPNW",
                        xmScrolledWindowWidgetClass,
                        kuipPanel,
                        XmNscrollingPolicy, XmAUTOMATIC,
                        XmNbottomOffset, 56,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNtopOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( km_scrollPNW, (char *) UxKuipPanelContext );


        /* Creation of km_rowcolPNW */
        km_rowcolPNW = XtVaCreateManagedWidget( "km_rowcolPNW",
                        xmRowColumnWidgetClass,
                        km_scrollPNW,
                        XmNadjustLast, FALSE,
                        XmNorientation, XmHORIZONTAL,
                        XmNpacking, XmPACK_COLUMN,
                        NULL );
        UxPutContext( km_rowcolPNW, (char *) UxKuipPanelContext );


        /* Creation of km_okPNB */
        km_okPNB = XtVaCreateManagedWidget( "km_okPNB",
                        xmPushButtonWidgetClass,
                        kuipPanel,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, " Close " ),
                        XmNrecomputeSize, FALSE,
                        XmNdefaultButtonShadowThickness, 1,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNshowAsDefault, 1,
                        NULL );
        UxPutContext( km_okPNB, (char *) UxKuipPanelContext );


        /* Creation of km_savePNB */
        km_savePNB = XtVaCreateManagedWidget( "km_savePNB",
                        xmPushButtonWidgetClass,
                        kuipPanel,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        RES_CONVERT( XmNlabelString, " Save " ),
                        XmNrecomputeSize, FALSE,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNdefaultButtonShadowThickness, 1,
                        XmNrightPosition, 50,
                        XmNrightOffset, -30,
                        XmNwidth, 50,
                        NULL );
        UxPutContext( km_savePNB, (char *) UxKuipPanelContext );


        /* Creation of km_helpPNB */
        km_helpPNB = XtVaCreateManagedWidget( "km_helpPNB",
                        xmPushButtonWidgetClass,
                        kuipPanel,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        RES_CONVERT( XmNlabelString, " Help " ),
                        XmNdefaultButtonShadowThickness, 1,
                        NULL );
        UxPutContext( km_helpPNB, (char *) UxKuipPanelContext );


        XtAddCallback( kuipPanel, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxKuipPanelContext);


        return ( kuipPanel );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget        create_kuipPanel()
{
        Widget                  rtrn;
        _UxCkuipPanel           *UxContext;

        UxKuipPanelContext = UxContext =
                (_UxCkuipPanel *) UxNewContext( sizeof(_UxCkuipPanel), False );


        rtrn = _Uxbuild_kuipPanel();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/
