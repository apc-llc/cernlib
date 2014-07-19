/*
 * $Id: helpdialog.c,v 1.1.1.1 1996/03/01 11:39:33 mclareni Exp $
 *
 * $Log: helpdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:33  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.06  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        helpDialog.c
        (Generated from interface file helpDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>


/*******************************************************************************
        The definition of the context structure:
        If you create multiple instances of your interface, the context
        structure ensures that your callbacks use the variables for the
        correct instance.

        For each swidget in the interface, each argument to the Interface
        function, and each variable in the Instance Specific section of the
        Declarations Editor, there is an entry in the context structure.
        and a #define.  The #define makes the variable name refer to the
        corresponding entry in the context structure.
*******************************************************************************/

typedef struct
{
        Widget  UxhelpScWindow;
        Widget  UxhelpSep;
} _UxChelpDialog;

#define helpScWindow            UxHelpDialogContext->UxhelpScWindow
#define helpSep                 UxHelpDialogContext->UxhelpSep

static _UxChelpDialog   *UxHelpDialogContext;

Widget  helpDialog;
Widget  helpText;
Widget  helpOK;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_helpDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_helpDialog()
{
        Widget  helpDialog_shell;

        helpDialog_shell = XtVaCreatePopupShell( "helpDialog_shell",
                        xmDialogShellWidgetClass, UxTopLevel,
                        XmNx, 441,
                        XmNy, 211,
                        XmNwidth, 650,
                        XmNheight, 410,
                        XmNtitle, "helpDialog",
                        NULL );

        helpDialog = XtVaCreateWidget( "helpDialog",
                        xmFormWidgetClass, helpDialog_shell,
                        RES_CONVERT( XmNdialogTitle, "Help" ),
                        XmNautoUnmanage, FALSE,
                        XmNheight, 410,
                        XmNwidth, 650,
                        NULL );

        UxPutContext( helpDialog, (char *) UxHelpDialogContext );

        helpScWindow = XtVaCreateManagedWidget( "helpScWindow",
                        xmScrolledWindowWidgetClass, helpDialog,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomPosition, 80,
                        XmNbottomOffset, 80,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNshadowThickness, 0,
                        XmNy, 20,
                        XmNx, 20,
                        NULL );

        UxPutContext( helpScWindow, (char *) UxHelpDialogContext );

        helpText = XtVaCreateManagedWidget( "helpText",
                        xmTextWidgetClass, helpScWindow,
                        XmNeditMode, XmMULTI_LINE_EDIT ,
                        XmNeditable, FALSE,
                        XmNheight, 300,
                        XmNwidth, 560,
                        XmNy, 0,
                        XmNx, 68,
                        NULL );

        UxPutContext( helpText, (char *) UxHelpDialogContext );

        helpOK = XtVaCreateManagedWidget( "helpOK",
                        xmPushButtonGadgetClass, helpDialog,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNrightPosition, 55,
                        XmNleftPosition, 50,
                        XmNleftOffset, -30,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshowAsDefault, 1,
                        RES_CONVERT( XmNlabelString, "OK" ),
                        XmNheight, 30,
                        XmNwidth, 60,
                        XmNy, 370,
                        XmNx, 292,
                        NULL );

        UxPutContext( helpOK, (char *) UxHelpDialogContext );

        helpSep = XtVaCreateManagedWidget( "helpSep",
                        xmSeparatorGadgetClass, helpDialog,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 60,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 10,
                        XmNwidth, 510,
                        XmNy, 330,
                        XmNx, 70,
                        NULL );

        UxPutContext( helpSep, (char *) UxHelpDialogContext );

        XtAddCallback( helpDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxHelpDialogContext );

        XtVaSetValues(helpDialog,
                        XmNdefaultButton, helpOK,
                        NULL );



        return ( helpDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_helpDialog()
{
        Widget                  rtrn;
        _UxChelpDialog          *UxContext;

        UxHelpDialogContext = UxContext =
                (_UxChelpDialog *) XtMalloc( sizeof(_UxChelpDialog) );

        rtrn = _Uxbuild_helpDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_helpDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_helpDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

