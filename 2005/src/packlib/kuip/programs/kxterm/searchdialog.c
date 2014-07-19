/*
 * $Id: searchdialog.c,v 1.1.1.1 1996/03/08 15:33:04 mclareni Exp $
 *
 * $Log: searchdialog.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:04  mclareni
 * Kuip
 *
 */
/*CMZ :  2.00/09 18/01/93  12.55.18  by  Alfred Nathaniel*/
/*-- Author :*/

/*******************************************************************************
        searchDialog.c
        (Generated from interface file searchDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "uxxt.h"

#include <Xm/ToggleB.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/Form.h>

/*******************************************************************************
        Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern swidget kuipIo;

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
        Widget  UxsearchDialog;
        Widget  Uxform2;
        Widget  Uxlabel3;
        Widget  Uxseparator3;
        Widget  Uxform1;
} _UxCsearchDialog;

#define searchDialog            UxSearchDialogContext->UxsearchDialog
#define form2                   UxSearchDialogContext->Uxform2
#define label3                  UxSearchDialogContext->Uxlabel3
#define separator3              UxSearchDialogContext->Uxseparator3
#define form1                   UxSearchDialogContext->Uxform1

static _UxCsearchDialog *UxSearchDialogContext;

Widget  searchText;
Widget  closeSearchButton;
Widget  caseToggle;
Widget  nextSearchButton;
Widget  previousSearchButton;
Widget  allSearchButton;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_searchDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_searchDialog()
{
        Widget  searchDialog_shell;

        searchDialog_shell = XtVaCreatePopupShell( "searchDialog_shell",
                        xmDialogShellWidgetClass, kuipIo,
                        XmNwidth, 395,
                        XmNheight, 170,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "searchDialog",
                        NULL );

        searchDialog = XtVaCreateWidget( "searchDialog",
                        xmFormWidgetClass, searchDialog_shell,
                        XmNdefaultPosition, FALSE,
                        RES_CONVERT( XmNdialogTitle, "Search..." ),
                        XmNautoUnmanage, FALSE,
                        XmNheight, 170,
                        XmNwidth, 395,
                        XmNunitType, XmPIXELS,
                        NULL );

        UxPutContext( searchDialog, (char *) UxSearchDialogContext );

        form2 = XtVaCreateManagedWidget( "form2",
                        xmFormWidgetClass, searchDialog,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 10,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form2, (char *) UxSearchDialogContext );

        label3 = XtVaCreateManagedWidget( "label3",
                        xmLabelWidgetClass, form2,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Search For:" ),
                        XmNheight, 25,
                        NULL );

        UxPutContext( label3, (char *) UxSearchDialogContext );

        searchText = XtVaCreateManagedWidget( "searchText",
                        xmTextWidgetClass, form2,
                        XmNleftWidget, label3,
                        XmNleftOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, label3,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 35,
                        NULL );

        UxPutContext( searchText, (char *) UxSearchDialogContext );

        closeSearchButton = XtVaCreateManagedWidget( "closeSearchButton",
                        xmPushButtonWidgetClass, searchDialog,
                        XmNrightPosition, 60,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 40,
                        XmNleftAttachment, XmATTACH_POSITION,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        XmNwidth, 60,
                        NULL );

        UxPutContext( closeSearchButton, (char *) UxSearchDialogContext );

        separator3 = XtVaCreateManagedWidget( "separator3",
                        xmSeparatorWidgetClass, searchDialog,
                        XmNbottomWidget, closeSearchButton,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        NULL );

        UxPutContext( separator3, (char *) UxSearchDialogContext );

        form1 = XtVaCreateManagedWidget( "form1",
                        xmFormWidgetClass, searchDialog,
                        XmNtopWidget, form2,
                        XmNtopOffset, 15,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form1, (char *) UxSearchDialogContext );

        caseToggle = XtVaCreateManagedWidget( "caseToggle",
                        xmToggleButtonWidgetClass, form1,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNy, -5,
                        XmNx, 50,
                        RES_CONVERT( XmNlabelString, "Case Sensitive" ),
                        XmNheight, 25,
                        NULL );

        UxPutContext( caseToggle, (char *) UxSearchDialogContext );

        nextSearchButton = XtVaCreateManagedWidget( "nextSearchButton",
                        xmPushButtonWidgetClass, form1,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Next" ),
                        XmNheight, 30,
                        XmNwidth, 75,
                        NULL );

        UxPutContext( nextSearchButton, (char *) UxSearchDialogContext );

        previousSearchButton = XtVaCreateManagedWidget( "previousSearchButton",
                        xmPushButtonWidgetClass, form1,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, nextSearchButton,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Previous" ),
                        XmNheight, 30,
                        XmNwidth, 75,
                        NULL );

        UxPutContext( previousSearchButton, (char *) UxSearchDialogContext );

        allSearchButton = XtVaCreateManagedWidget( "allSearchButton",
                        xmPushButtonWidgetClass, form1,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, previousSearchButton,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        RES_CONVERT( XmNlabelString, "All" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        XmNwidth, 75,
                        NULL );

        UxPutContext( allSearchButton, (char *) UxSearchDialogContext );

        XtAddCallback( searchDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxSearchDialogContext );

        XtVaSetValues(searchDialog,
                        XmNdefaultButton, nextSearchButton,
                        NULL );



        return ( searchDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_searchDialog()
{
        Widget                  rtrn;
        _UxCsearchDialog        *UxContext;

        UxSearchDialogContext = UxContext =
                (_UxCsearchDialog *) XtMalloc( sizeof(_UxCsearchDialog) );

        rtrn = _Uxbuild_searchDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_searchDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_searchDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

