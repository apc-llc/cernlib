/*
 * $Id: kuxcmp.c,v 1.1.1.1 1996/03/08 15:33:10 mclareni Exp $
 *
 * $Log: kuxcmp.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:10  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/03 12/01/95  14.11.00  by  N.Cremel*/
/*-- Author :*/
/*
 * kuipCommand.c
 */

/*******************************************************************************
        kuipCommand.c

       Associated Header file: kuipCommand.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/Form.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "kuipCommand.h" */
#include "mkuxcm.h"
#include "mkutdm.h"
#undef CONTEXT_MACRO_ACCESS

Widget        km_cmdOK;
Widget        km_cmdEX;
Widget        km_cmdRE;
Widget        km_cmdCA;
Widget        km_cmdHE;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget        _Uxbuild_kuipCommandP()
{
        Widget                _UxParent;


        /* Creation of kuipCommandP */
        _UxParent = XtVaCreatePopupShell( "kuipCommandP_shell",
                        xmDialogShellWidgetClass, UxTopLevel,
                        XmNallowShellResize, TRUE,
                        XmNtitle, "kuipCommandP",
                        NULL );

        kuipCommandP = XtVaCreateWidget( "kuipCommandP",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNautoUnmanage, FALSE,
                        RES_CONVERT( XmNdialogTitle, "Command" ),
                        XmNdefaultPosition, FALSE,
                        XmNresizePolicy, XmRESIZE_ANY,
                        NULL );
        UxPutContext( kuipCommandP, (char *) UxKuipCommandPContext );


        /* Creation of commandSep */
        commandSep = XtVaCreateManagedWidget( "commandSep",
                        xmSeparatorGadgetClass,
                        kuipCommandP,
                        XmNheight, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 45,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        NULL );
        UxPutContext( commandSep, (char *) UxKuipCommandPContext );


        /* Creation of km_cmdOK */
        km_cmdOK = XtVaCreateManagedWidget( "km_cmdOK",
                        xmPushButtonGadgetClass,
                        kuipCommandP,
                        XmNwidth, 80,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "OK" ),
                        XmNshowAsDefault, 1,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightOffset, 5,
                        XmNrightPosition, 20,
                        XmNmnemonicCharSet, "O",
                        NULL );
        UxPutContext( km_cmdOK, (char *) UxKuipCommandPContext );


        /* Creation of km_cmdEX */
        km_cmdEX = XtVaCreateManagedWidget( "km_cmdEX",
                        xmPushButtonGadgetClass,
                        kuipCommandP,
                        XmNwidth, 80,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Execute" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, km_cmdOK,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightOffset, 5,
                        XmNrightPosition, 40,
                        RES_CONVERT( XmNmnemonic, "E" ),
                        NULL );
        UxPutContext( km_cmdEX, (char *) UxKuipCommandPContext );


        /* Creation of km_cmdRE */
        km_cmdRE = XtVaCreateManagedWidget( "km_cmdRE",
                        xmPushButtonGadgetClass,
                        kuipCommandP,
                        XmNwidth, 80,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, km_cmdEX,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightOffset, 5,
                        XmNrightPosition, 60,
                        XmNtopAttachment, XmATTACH_NONE,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        RES_CONVERT( XmNmnemonic, "R" ),
                        NULL );
        UxPutContext( km_cmdRE, (char *) UxKuipCommandPContext );


        /* Creation of km_cmdCA */
        km_cmdCA = XtVaCreateManagedWidget( "km_cmdCA",
                        xmPushButtonGadgetClass,
                        kuipCommandP,
                        XmNwidth, 80,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, km_cmdRE,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightOffset, 5,
                        XmNrightPosition, 80,
                        RES_CONVERT( XmNlabelString, "Cancel" ),
                        RES_CONVERT( XmNmnemonic, "C" ),
                        NULL );
        UxPutContext( km_cmdCA, (char *) UxKuipCommandPContext );


        /* Creation of km_cmdHE */
        km_cmdHE = XtVaCreateManagedWidget( "km_cmdHE",
                        xmPushButtonGadgetClass,
                        kuipCommandP,
                        XmNwidth, 80,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, km_cmdCA,
                        XmNrightPosition, 100,
                        RES_CONVERT( XmNlabelString, "Help" ),
                        RES_CONVERT( XmNmnemonic, "H" ),
                        NULL );
        UxPutContext( km_cmdHE, (char *) UxKuipCommandPContext );

        XtVaSetValues(kuipCommandP,
                        XmNdefaultButton, km_cmdOK,
                        NULL );


        XtAddCallback( kuipCommandP, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxKuipCommandPContext);


        return ( kuipCommandP );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget        create_kuipCommand()
{
        Widget                  rtrn;
        _UxCkuipCommandP        *UxContext;

        UxKuipCommandPContext = UxContext =
          (_UxCkuipCommandP *) UxNewContext( sizeof(_UxCkuipCommandP), False );


        rtrn = _Uxbuild_kuipCommandP();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

