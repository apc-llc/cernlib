/*
 * $Id: openfbdialog.c,v 1.1.1.1 1996/03/01 11:39:35 mclareni Exp $
 *
 * $Log: openfbdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:35  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.07  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        openFbDialog.c
        (Generated from interface file openFbDialog.i)
        Associated Header file: openFbDialog.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Text.h>
#include <Xm/Form.h>
#include <Xm/FileSB.h>
#include <Xm/SeparatoG.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>


/*******************************************************************************
        The following header file defines the context structure.
*******************************************************************************/

#include "hmotif/openfbdialog.h"


/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_openFbDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_openFbDialog()
{
        Widget  openFbDialog_shell;

        openFbDialog_shell = XtVaCreatePopupShell( "openFbDialog_shell",
                        xmDialogShellWidgetClass, file_parent,
                        XmNx, 493,
                        XmNy, 256,
                        XmNwidth, 470,
                        XmNheight, 520,
                        XmNtitle, "openFbDialog",
                        NULL );

        openFbDialog = XtVaCreateWidget( "openFbDialog",
                        xmFormWidgetClass, openFbDialog_shell,
                        XmNresizePolicy, XmRESIZE_GROW,
                        RES_CONVERT( XmNdialogTitle, "Open Arguments" ),
                        XmNautoUnmanage, FALSE,
                        XmNshadowThickness, 1,
                        XmNheight, 520,
                        XmNwidth, 470,
                        NULL );

        UxPutContext( openFbDialog, (char *) UxOpenFbDialogContext );

        frame1 = XtVaCreateManagedWidget( "frame1",
                        xmFrameWidgetClass, openFbDialog,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( frame1, (char *) UxOpenFbDialogContext );

        rowColumn1 = XtVaCreateManagedWidget( "rowColumn1",
                        xmRowColumnWidgetClass, frame1,
                        XmNradioBehavior, TRUE,
                        NULL );

        UxPutContext( rowColumn1, (char *) UxOpenFbDialogContext );

        fileTogRo = XtVaCreateManagedWidget( "fileTogRo",
                        xmToggleButtonGadgetClass, rowColumn1,
                        XmNset, TRUE,
                        RES_CONVERT( XmNlabelString, "Open in Read Only Mode" ),
                        NULL );

        UxPutContext( fileTogRo, (char *) UxOpenFbDialogContext );

        fileTogUp = XtVaCreateManagedWidget( "fileTogUp",
                        xmToggleButtonGadgetClass, rowColumn1,
                        RES_CONVERT( XmNlabelString, "Open in Update Mode" ),
                        NULL );

        UxPutContext( fileTogUp, (char *) UxOpenFbDialogContext );

        fileTogNew = XtVaCreateManagedWidget( "fileTogNew",
                        xmToggleButtonGadgetClass, rowColumn1,
                        RES_CONVERT( XmNlabelString, "Create New File" ),
                        NULL );

        UxPutContext( fileTogNew, (char *) UxOpenFbDialogContext );

        separatorGadget1 = XtVaCreateManagedWidget( "separatorGadget1",
                        xmSeparatorGadgetClass, openFbDialog,
                        XmNtopOffset, 110,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( separatorGadget1, (char *) UxOpenFbDialogContext );

        fileSelBox = XtVaCreateManagedWidget( "fileSelBox",
                        xmFileSelectionBoxWidgetClass, openFbDialog,
                        XmNtopWidget, separatorGadget1,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 1,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 1,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 1,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNdirMask, "*.hbook" ),
                        RES_CONVERT( XmNselectionLabelString, "Open File" ),
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNheight, 394,
                        XmNwidth, 398,
                        XmNy, 123,
                        XmNx, 1,
                        NULL );

        UxPutContext( fileSelBox, (char *) UxOpenFbDialogContext );

        form1 = XtVaCreateManagedWidget( "form1",
                        xmFormWidgetClass, openFbDialog,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomWidget, separatorGadget1,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNwidth, 65,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form1, (char *) UxOpenFbDialogContext );

        recordLength = XtVaCreateManagedWidget( "recordLength",
                        xmTextWidgetClass, form1,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNmarginWidth, 2,
                        XmNmarginHeight, 2,
                        XmNheight, 30,
                        NULL );

        UxPutContext( recordLength, (char *) UxOpenFbDialogContext );

        scrolledWindow2 = XtVaCreateManagedWidget( "scrolledWindow2",
                        xmScrolledWindowWidgetClass, form1,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomWidget, recordLength,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNshadowThickness, 0,
                        NULL );

        UxPutContext( scrolledWindow2, (char *) UxOpenFbDialogContext );

        recordLengthList = XtVaCreateManagedWidget( "recordLengthList",
                        xmListWidgetClass, scrolledWindow2,
                        XmNlistSizePolicy, XmCONSTANT,
                        XmNselectionPolicy, XmBROWSE_SELECT,
                        XmNautomaticSelection, TRUE,
                        NULL );

        UxPutContext( recordLengthList, (char *) UxOpenFbDialogContext );

        labelGadget2 = XtVaCreateManagedWidget( "labelGadget2",
                        xmLabelGadgetClass, openFbDialog,
                        XmNrightWidget, form1,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Record\nLength: " ),
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        XmNheight, 40,
                        NULL );

        UxPutContext( labelGadget2, (char *) UxOpenFbDialogContext );

        scrolledWindow3 = XtVaCreateManagedWidget( "scrolledWindow3",
                        xmScrolledWindowWidgetClass, openFbDialog,
                        XmNwidth, 60,
                        XmNbottomOffset, 5,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, labelGadget2,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNbottomWidget, separatorGadget1,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNshadowThickness, 0,
                        NULL );

        UxPutContext( scrolledWindow3, (char *) UxOpenFbDialogContext );

        fileUnits = XtVaCreateManagedWidget( "fileUnits",
                        xmListWidgetClass, scrolledWindow3,
                        XmNlistSizePolicy, XmCONSTANT,
                        XmNselectionPolicy, XmBROWSE_SELECT,
                        NULL );

        UxPutContext( fileUnits, (char *) UxOpenFbDialogContext );

        labelGadget1 = XtVaCreateManagedWidget( "labelGadget1",
                        xmLabelGadgetClass, openFbDialog,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, scrolledWindow3,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        RES_CONVERT( XmNlabelString, "Unit: " ),
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        XmNheight, 20,
                        XmNy, 0,
                        XmNx, 230,
                        NULL );

        UxPutContext( labelGadget1, (char *) UxOpenFbDialogContext );

        XtAddCallback( openFbDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxOpenFbDialogContext );



        return ( openFbDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_openFbDialog( _Uxfile_parent )
        swidget _Uxfile_parent;
{
        Widget                  rtrn;
        _UxCopenFbDialog        *UxContext;

        UxOpenFbDialogContext = UxContext =
                (_UxCopenFbDialog *) XtMalloc( sizeof(_UxCopenFbDialog) );

        file_parent = _Uxfile_parent;
        rtrn = _Uxbuild_openFbDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_openFbDialog( _Uxfile_parent )
        swidget _Uxfile_parent;
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_openFbDialog( _Uxfile_parent );

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

