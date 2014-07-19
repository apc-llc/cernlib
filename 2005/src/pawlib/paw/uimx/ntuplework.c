/*
 * $Id: ntuplework.c,v 1.1.1.1 1996/03/01 11:39:35 mclareni Exp $
 *
 * $Log: ntuplework.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:35  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 10/03/94  17.46.02  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
       ntupleWork.c
       (Generated from interface file ntupleWork.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/ScrollBar.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
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
        Widget  UxntupleWork;
        Widget  Uxseparator1;
        Widget  Uxlabel3;
        Widget  Uxlabel1;
        Widget  Uxlabel2;
        Widget  Uxlabel4;
        swidget Uxwork_parent;
} _UxCntupleWork;

static _UxCntupleWork          *UxNtupleWorkContext;
#define ntupleWork              UxNtupleWorkContext->UxntupleWork
#define separator1              UxNtupleWorkContext->Uxseparator1
#define label3                  UxNtupleWorkContext->Uxlabel3
#define label1                  UxNtupleWorkContext->Uxlabel1
#define label2                  UxNtupleWorkContext->Uxlabel2
#define label4                  UxNtupleWorkContext->Uxlabel4
#define work_parent             UxNtupleWorkContext->Uxwork_parent


Widget  messNtWLabel;
Widget  stopNtWButton;
Widget  stopNtWLabel;
Widget  passNtWLabel;
Widget  progressNtWScrollbar;
Widget  minNtWLabel;
Widget  maxNtWLabel;
Widget  chainNtWLabel;
Widget  eventNtWLabel;
Widget  fileNtWLabel;
Widget  hostNtWLabel;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_ntupleWork( swidget _Uxwork_parent );

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_ntupleWork()
{
        Widget          _UxParent;


        /* Creation of ntupleWork */
        _UxParent = work_parent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        _UxParent = XtVaCreatePopupShell( "ntupleWork_shell",
                        xmDialogShellWidgetClass, _UxParent,
                        XmNwidth, 530,
                        XmNheight, 320,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "ntupleWork",
                        NULL );

        ntupleWork = XtVaCreateWidget( "ntupleWork",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNunitType, XmPIXELS,
                        XmNwidth, 530,
                        XmNheight, 320,
                        XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
                        RES_CONVERT( XmNdialogTitle, "Ntuple Work Dialog" ),
                        XmNautoUnmanage, FALSE,
                        XmNnoResize, TRUE,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );
        UxPutContext( ntupleWork, (char *) UxNtupleWorkContext );


        /* Creation of messNtWLabel */
        messNtWLabel = XtVaCreateManagedWidget( "messNtWLabel",
                        xmLabelWidgetClass,
                        ntupleWork,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString,
                        "Making 2 passes   (lacking xx MB of buffer space)." ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 20,
                        XmNheight, 20,
                        NULL );
        UxPutContext( messNtWLabel, (char *) UxNtupleWorkContext );


        /* Creation of stopNtWButton */
        stopNtWButton = XtVaCreateManagedWidget( "stopNtWButton",
                        xmPushButtonWidgetClass,
                        ntupleWork,
                        XmNwidth, 60,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Stop" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 15,
                        NULL );
        UxPutContext( stopNtWButton, (char *) UxNtupleWorkContext );


        /* Creation of separator1 */
        separator1 = XtVaCreateManagedWidget( "separator1",
                        xmSeparatorWidgetClass,
                        ntupleWork,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 15,
                        XmNbottomWidget, stopNtWButton,
                        NULL );
        UxPutContext( separator1, (char *) UxNtupleWorkContext );


        /* Creation of stopNtWLabel */
        stopNtWLabel = XtVaCreateManagedWidget( "stopNtWLabel",
                        xmLabelWidgetClass,
                        ntupleWork,
                        XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                        RES_CONVERT( XmNlabelString, "at next update." ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 10,
                        XmNleftWidget, stopNtWButton,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        XmNbottomWidget, stopNtWButton,
                        XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
                        XmNtopWidget, stopNtWButton,
                        NULL );
        UxPutContext( stopNtWLabel, (char *) UxNtupleWorkContext );


        /* Creation of label3 */
        label3 = XtVaCreateManagedWidget( "label3",
                        xmLabelWidgetClass,
                        ntupleWork,
                        XmNheight, 20,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Host:" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 10,
                        XmNtopWidget, messNtWLabel,
                        NULL );
        UxPutContext( label3, (char *) UxNtupleWorkContext );


        /* Creation of passNtWLabel */
        passNtWLabel = XtVaCreateManagedWidget( "passNtWLabel",
                        xmLabelWidgetClass,
                        ntupleWork,
                        RES_CONVERT( XmNlabelString, "Pass 1" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 10,
                        XmNtopWidget, label3,
       XmNfontList, UxConvertFontList( "*-helvetica-bold-r-normal*-240-*" ),
                        NULL );
        UxPutContext( passNtWLabel, (char *) UxNtupleWorkContext );


        /* Creation of progressNtWScrollbar */
        progressNtWScrollbar = XtVaCreateManagedWidget( "progressNtWScrollbar",
                        xmScrollBarWidgetClass,
                        ntupleWork,
                        XmNheight, 20,
                        XmNorientation, XmHORIZONTAL,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 20,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 20,
                        XmNshowArrows, FALSE,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 30,
                        XmNtopWidget, passNtWLabel,
                        RES_CONVERT( XmNtroughColor, "red" ),
                        XmNsliderSize, 1,
                        NULL );
        UxPutContext( progressNtWScrollbar, (char *) UxNtupleWorkContext );


        /* Creation of minNtWLabel */
        minNtWLabel = XtVaCreateManagedWidget( "minNtWLabel",
                        xmLabelWidgetClass,
                        ntupleWork,
                        XmNheight, 20,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "0 %" ),
                        XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftWidget, progressNtWScrollbar,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, progressNtWScrollbar,
                        XmNwidth, 100,
                        NULL );
        UxPutContext( minNtWLabel, (char *) UxNtupleWorkContext );


        /* Creation of maxNtWLabel */
        maxNtWLabel = XtVaCreateManagedWidget( "maxNtWLabel",
                        xmLabelWidgetClass,
                        ntupleWork,
                        XmNheight, 20,
                        RES_CONVERT( XmNlabelString, "100 %" ),
                        XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
                        XmNrightWidget, progressNtWScrollbar,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, progressNtWScrollbar,
                        XmNalignment, XmALIGNMENT_END,
                        XmNwidth, 100,
                        NULL );
        UxPutContext( maxNtWLabel, (char *) UxNtupleWorkContext );


        /* Creation of label1 */
        label1 = XtVaCreateManagedWidget( "label1",
                        xmLabelWidgetClass,
                        ntupleWork,
                        XmNheight, 20,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Current File:" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 10,
                        XmNtopWidget, minNtWLabel,
                        NULL );
        UxPutContext( label1, (char *) UxNtupleWorkContext );


        /* Creation of chainNtWLabel */
        chainNtWLabel = XtVaCreateManagedWidget( "chainNtWLabel",
                        xmLabelWidgetClass,
                        ntupleWork,
                        XmNheight, 20,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, label1,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 10,
                        XmNtopWidget, minNtWLabel,
                        NULL );
        UxPutContext( chainNtWLabel, (char *) UxNtupleWorkContext );


        /* Creation of label2 */
        label2 = XtVaCreateManagedWidget( "label2",
                        xmLabelWidgetClass,
                        ntupleWork,
                        XmNheight, 20,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Files Processed:" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, label1,
                        XmNleftOffset, 10,
                        XmNtopOffset, 5,
                        NULL );
        UxPutContext( label2, (char *) UxNtupleWorkContext );


        /* Creation of label4 */
        label4 = XtVaCreateManagedWidget( "label4",
                        xmLabelWidgetClass,
                        ntupleWork,
                        XmNheight, 20,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Events Processed:" ),
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 45,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, label1,
                        NULL );
        UxPutContext( label4, (char *) UxNtupleWorkContext );


        /* Creation of eventNtWLabel */
        eventNtWLabel = XtVaCreateManagedWidget( "eventNtWLabel",
                        xmLabelWidgetClass,
                        ntupleWork,
                        XmNheight, 20,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, label4,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, label1,
                        NULL );
        UxPutContext( eventNtWLabel, (char *) UxNtupleWorkContext );


        /* Creation of fileNtWLabel */
        fileNtWLabel = XtVaCreateManagedWidget( "fileNtWLabel",
                        xmLabelWidgetClass,
                        ntupleWork,
                        XmNheight, 20,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, label2,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, label1,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightWidget, label4,
                        NULL );
        UxPutContext( fileNtWLabel, (char *) UxNtupleWorkContext );


        /* Creation of hostNtWLabel */
        hostNtWLabel = XtVaCreateManagedWidget( "hostNtWLabel",
                        xmLabelWidgetClass,
                        ntupleWork,
                        XmNheight, 20,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Local" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, label3,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 10,
                        XmNtopWidget, messNtWLabel,
                        NULL );
        UxPutContext( hostNtWLabel, (char *) UxNtupleWorkContext );


        XtAddCallback( ntupleWork, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxNtupleWorkContext);


        return ( ntupleWork );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_ntupleWork( swidget _Uxwork_parent )
{
        Widget                  rtrn;
        _UxCntupleWork          *UxContext;

        UxNtupleWorkContext = UxContext =
            (_UxCntupleWork *) UxNewContext( sizeof(_UxCntupleWork), False );

        work_parent = _Uxwork_parent;

        rtrn = _Uxbuild_ntupleWork();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

