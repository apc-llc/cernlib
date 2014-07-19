/*
 * $Id: maildialog.c,v 1.1.1.1 1996/03/01 11:39:35 mclareni Exp $
 *
 * $Log: maildialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:35  mclareni
 * Paw
 *
 */
/*CMZ :  2.05/17 02/09/94  23.57.43  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
       mailDialog.c
       (Generated from interface file mailDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/Text.h>
#include <Xm/Form.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/Form.h>



/*******************************************************************************
       The definition of the context structure:
       If you create multiple copies of your interface, the context
       structure ensures that your callbacks use the variables for the
       correct copy.

       For each swidget in the interface, each argument to the Interface
       function, and each variable in the Interface Specific section of the
       Declarations Editor, there is an entry in the context structure.
       and a #define.  The #define makes the variable name refer to the
       corresponding entry in the context structure.
*******************************************************************************/

typedef struct
{
        Widget  UxseparatorGadget5;
        Widget  UxseparatorGadget6;
        Widget  UxrowColumn1;
        Widget  UxseparatorGadget3;
        Widget  Uxform2;
        Widget  UxlabelGadget4;
        Widget  UxlabelGadget5;
        Widget  Uxform3;
        Widget  UxlabelGadget6;
        Widget  UxlabelGadget7;
        Widget  UxseparatorGadget4;
        Widget  UxscrolledWindow1;
        swidget Uxmail_parent;
} _UxCmailDialog;

static _UxCmailDialog          *UxMailDialogContext;
#define separatorGadget5        UxMailDialogContext->UxseparatorGadget5
#define separatorGadget6        UxMailDialogContext->UxseparatorGadget6
#define rowColumn1              UxMailDialogContext->UxrowColumn1
#define separatorGadget3        UxMailDialogContext->UxseparatorGadget3
#define form2                   UxMailDialogContext->Uxform2
#define labelGadget4            UxMailDialogContext->UxlabelGadget4
#define labelGadget5            UxMailDialogContext->UxlabelGadget5
#define form3                   UxMailDialogContext->Uxform3
#define labelGadget6            UxMailDialogContext->UxlabelGadget6
#define labelGadget7            UxMailDialogContext->UxlabelGadget7
#define separatorGadget4        UxMailDialogContext->UxseparatorGadget4
#define scrolledWindow1         UxMailDialogContext->UxscrolledWindow1
#define mail_parent             UxMailDialogContext->Uxmail_parent


Widget  mailDialog;
Widget  mailMailButton;
Widget  cancelMailButton;
Widget  helpMailButton;
Widget  versionMailLabel;
Widget  bugMailToggle;
Widget  commentMailToggle;
Widget  userMailText;
Widget  instituteMailText;
Widget  phoneMailText;
Widget  emailMailText;
Widget  bugMailText;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_mailDialog( swidget _Uxmail_parent );

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_mailDialog()
{
        Widget          _UxParent;


        /* Creation of mailDialog */
        _UxParent = mail_parent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        _UxParent = XtVaCreatePopupShell( "mailDialog_shell",
                        xmDialogShellWidgetClass, _UxParent,
                        XmNwidth, 755,
                        XmNheight, 650,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "mailDialog",
                        NULL );

        mailDialog = XtVaCreateWidget( "mailDialog",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNwidth, 755,
                        XmNheight, 650,
                        XmNunitType, XmPIXELS,
                        XmNautoUnmanage, FALSE,
                        RES_CONVERT( XmNdialogTitle, "Mail to Developers" ),
                        XmNshadowThickness, 1,
                        NULL );
        UxPutContext( mailDialog, (char *) UxMailDialogContext );


        /* Creation of mailMailButton */
        mailMailButton = XtVaCreateManagedWidget( "mailMailButton",
                        xmPushButtonGadgetClass,
                        mailDialog,
                        XmNwidth, 110,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        RES_CONVERT( XmNlabelString, "Mail" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 30,
                        NULL );
        UxPutContext( mailMailButton, (char *) UxMailDialogContext );


        /* Creation of cancelMailButton */
        cancelMailButton = XtVaCreateManagedWidget( "cancelMailButton",
                        xmPushButtonGadgetClass,
                        mailDialog,
                        XmNwidth, 110,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        RES_CONVERT( XmNlabelString, "Cancel" ),
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftOffset, -60,
                        XmNleftPosition, 50,
                        NULL );
        UxPutContext( cancelMailButton, (char *) UxMailDialogContext );


        /* Creation of helpMailButton */
        helpMailButton = XtVaCreateManagedWidget( "helpMailButton",
                        xmPushButtonGadgetClass,
                        mailDialog,
                        XmNwidth, 110,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        RES_CONVERT( XmNlabelString, "Help..." ),
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 30,
                        NULL );
        UxPutContext( helpMailButton, (char *) UxMailDialogContext );


        /* Creation of separatorGadget5 */
        separatorGadget5 = XtVaCreateManagedWidget( "separatorGadget5",
                        xmSeparatorGadgetClass,
                        mailDialog,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNbottomWidget, mailMailButton,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        NULL );
        UxPutContext( separatorGadget5, (char *) UxMailDialogContext );


        /* Creation of versionMailLabel */
        versionMailLabel = XtVaCreateManagedWidget( "versionMailLabel",
                        xmLabelGadgetClass,
                        mailDialog,
                        XmNheight, 60,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 5,
                        XmNbottomWidget, separatorGadget5,
                        RES_CONVERT( XmNlabelString, "Version information" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        NULL );
        UxPutContext( versionMailLabel, (char *) UxMailDialogContext );


        /* Creation of separatorGadget6 */
        separatorGadget6 = XtVaCreateManagedWidget( "separatorGadget6",
                        xmSeparatorGadgetClass,
                        mailDialog,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 5,
                        XmNbottomWidget, versionMailLabel,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        NULL );
        UxPutContext( separatorGadget6, (char *) UxMailDialogContext );


        /* Creation of rowColumn1 */
        rowColumn1 = XtVaCreateManagedWidget( "rowColumn1",
                        xmRowColumnWidgetClass,
                        mailDialog,
                        XmNheight, 45,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 75,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        XmNorientation, XmHORIZONTAL,
                        XmNradioBehavior, TRUE,
                        XmNspacing, 270,
                        NULL );
        UxPutContext( rowColumn1, (char *) UxMailDialogContext );


        /* Creation of bugMailToggle */
        bugMailToggle = XtVaCreateManagedWidget( "bugMailToggle",
                        xmToggleButtonGadgetClass,
                        rowColumn1,
                        RES_CONVERT( XmNlabelString, "Mail Bug Report" ),
                        XmNindicatorSize, 25,
                        XmNalignment, XmALIGNMENT_END,
                        XmNspacing, 20,
                        XmNset, TRUE,
                        NULL );
        UxPutContext( bugMailToggle, (char *) UxMailDialogContext );


        /* Creation of commentMailToggle */
        commentMailToggle = XtVaCreateManagedWidget( "commentMailToggle",
                        xmToggleButtonGadgetClass,
                        rowColumn1,
                        XmNalignment, XmALIGNMENT_END,
                        XmNindicatorSize, 25,
                        RES_CONVERT( XmNlabelString, "Mail Comment" ),
                        XmNspacing, 20,
                        NULL );
        UxPutContext( commentMailToggle, (char *) UxMailDialogContext );


        /* Creation of separatorGadget3 */
        separatorGadget3 = XtVaCreateManagedWidget( "separatorGadget3",
                        xmSeparatorGadgetClass,
                        mailDialog,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, rowColumn1,
                        NULL );
        UxPutContext( separatorGadget3, (char *) UxMailDialogContext );


        /* Creation of form2 */
        form2 = XtVaCreateManagedWidget( "form2",
                        xmFormWidgetClass,
                        mailDialog,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNheight, 35,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, separatorGadget3,
                        NULL );
        UxPutContext( form2, (char *) UxMailDialogContext );


        /* Creation of labelGadget4 */
        labelGadget4 = XtVaCreateManagedWidget( "labelGadget4",
                        xmLabelGadgetClass,
                        form2,
                        XmNwidth, 45,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        RES_CONVERT( XmNlabelString, "Name:" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNalignment, XmALIGNMENT_END,
                        NULL );
        UxPutContext( labelGadget4, (char *) UxMailDialogContext );


        /* Creation of userMailText */
        userMailText = XtVaCreateManagedWidget( "userMailText",
                        xmTextWidgetClass,
                        form2,
                        XmNheight, 35,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, labelGadget4,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 47,
                        NULL );
        UxPutContext( userMailText, (char *) UxMailDialogContext );


        /* Creation of labelGadget5 */
        labelGadget5 = XtVaCreateManagedWidget( "labelGadget5",
                        xmLabelGadgetClass,
                        form2,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        RES_CONVERT( XmNlabelString, "Institute:" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 10,
                        XmNleftWidget, userMailText,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNwidth, 65,
                        XmNalignment, XmALIGNMENT_END,
                        NULL );
        UxPutContext( labelGadget5, (char *) UxMailDialogContext );


        /* Creation of instituteMailText */
        instituteMailText = XtVaCreateManagedWidget( "instituteMailText",
                        xmTextWidgetClass,
                        form2,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNleftWidget, labelGadget5,
                        NULL );
        UxPutContext( instituteMailText, (char *) UxMailDialogContext );


        /* Creation of form3 */
        form3 = XtVaCreateManagedWidget( "form3",
                        xmFormWidgetClass,
                        mailDialog,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNheight, 35,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, form2,
                        NULL );
        UxPutContext( form3, (char *) UxMailDialogContext );


        /* Creation of labelGadget6 */
        labelGadget6 = XtVaCreateManagedWidget( "labelGadget6",
                        xmLabelGadgetClass,
                        form3,
                        XmNwidth, 45,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        RES_CONVERT( XmNlabelString, "Phone:" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNalignment, XmALIGNMENT_END,
                        NULL );
        UxPutContext( labelGadget6, (char *) UxMailDialogContext );


        /* Creation of phoneMailText */
        phoneMailText = XtVaCreateManagedWidget( "phoneMailText",
                        xmTextWidgetClass,
                        form3,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, labelGadget6,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 47,
                        NULL );
        UxPutContext( phoneMailText, (char *) UxMailDialogContext );


        /* Creation of labelGadget7 */
        labelGadget7 = XtVaCreateManagedWidget( "labelGadget7",
                        xmLabelGadgetClass,
                        form3,
                        XmNwidth, 65,
                        XmNalignment, XmALIGNMENT_END,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        RES_CONVERT( XmNlabelString, "E-mail:" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 10,
                        XmNleftWidget, phoneMailText,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        NULL );
        UxPutContext( labelGadget7, (char *) UxMailDialogContext );


        /* Creation of emailMailText */
        emailMailText = XtVaCreateManagedWidget( "emailMailText",
                        xmTextWidgetClass,
                        form3,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, labelGadget7,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        NULL );
        UxPutContext( emailMailText, (char *) UxMailDialogContext );


        /* Creation of separatorGadget4 */
        separatorGadget4 = XtVaCreateManagedWidget( "separatorGadget4",
                        xmSeparatorGadgetClass,
                        mailDialog,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, form3,
                        NULL );
        UxPutContext( separatorGadget4, (char *) UxMailDialogContext );


        /* Creation of scrolledWindow1 */
        scrolledWindow1 = XtVaCreateManagedWidget( "scrolledWindow1",
                        xmScrolledWindowWidgetClass,
                        mailDialog,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNshadowThickness, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, separatorGadget4,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 5,
                        XmNbottomWidget, separatorGadget6,
                        NULL );
        UxPutContext( scrolledWindow1, (char *) UxMailDialogContext );


        /* Creation of bugMailText */
        bugMailText = XtVaCreateManagedWidget( "bugMailText",
                        xmTextWidgetClass,
                        scrolledWindow1,
                        XmNheight, 305,
                        XmNeditMode, XmMULTI_LINE_EDIT ,
                        XmNeditable, TRUE,
                        NULL );
        UxPutContext( bugMailText, (char *) UxMailDialogContext );

        XtVaSetValues(mailDialog,
                        XmNdefaultButton, mailMailButton,
                        XmNcancelButton, cancelMailButton,
                        NULL );


        XtAddCallback( mailDialog, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxMailDialogContext);


        return ( mailDialog );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_mailDialog( swidget _Uxmail_parent )
{
        Widget                  rtrn;
        _UxCmailDialog          *UxContext;

        UxMailDialogContext = UxContext =
               (_UxCmailDialog *) UxNewContext( sizeof(_UxCmailDialog), False );

        mail_parent = _Uxmail_parent;

        rtrn = _Uxbuild_mailDialog();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

