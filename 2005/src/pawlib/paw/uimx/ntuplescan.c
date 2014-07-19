/*
 * $Id: ntuplescan.c,v 1.1.1.1 1996/03/01 11:39:35 mclareni Exp $
 *
 * $Log: ntuplescan.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:35  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.07  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        ntupleScan.c
        (Generated from interface file ntupleScan.i)
        Associated Header file: ntupleScan.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/SeparatoG.h>
#include <Xm/Form.h>


/*******************************************************************************
        The following header file defines the context structure.
*******************************************************************************/

#include "hmotif/ntuplescan.h"


/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_ntupleScan();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_ntupleScan()
{
        Widget  ntupleScan_shell;

        ntupleScan_shell = XtVaCreatePopupShell( "ntupleScan_shell",
                        topLevelShellWidgetClass, UxTopLevel,
                        XmNwidth, 640,
                        XmNheight, 360,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "ntupleScan",
                        XmNiconName, "ntupleScan",
                        NULL );

        ntupleScan = XtVaCreateManagedWidget( "ntupleScan",
                        xmFormWidgetClass, ntupleScan_shell,
                        RES_CONVERT( XmNdialogTitle, "Ntuple Scanner 1" ),
                        XmNheight, 360,
                        XmNwidth, 640,
                        XmNunitType, XmPIXELS,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( ntupleScan, (char *) UxNtupleScanContext );

        separatorGadget3 = XtVaCreateManagedWidget( "separatorGadget3",
                        xmSeparatorGadgetClass, ntupleScan,
                        XmNtopOffset, 30,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( separatorGadget3, (char *) UxNtupleScanContext );

        separatorGadget4 = XtVaCreateManagedWidget( "separatorGadget4",
                        xmSeparatorGadgetClass, ntupleScan,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 40,
                        XmNbottomAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( separatorGadget4, (char *) UxNtupleScanContext );

        closeScButton = XtVaCreateManagedWidget( "closeScButton",
                        xmPushButtonWidgetClass, ntupleScan,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Exit" ),
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        XmNwidth, 70,
                        NULL );

        UxPutContext( closeScButton, (char *) UxNtupleScanContext );

        helpScButton = XtVaCreateManagedWidget( "helpScButton",
                        xmPushButtonWidgetClass, ntupleScan,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Help..." ),
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        XmNwidth, 70,
                        NULL );

        UxPutContext( helpScButton, (char *) UxNtupleScanContext );

        form6 = XtVaCreateManagedWidget( "form6",
                        xmFormWidgetClass, ntupleScan,
                        XmNtopWidget, separatorGadget3,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form6, (char *) UxNtupleScanContext );

        labelGadget1 = XtVaCreateManagedWidget( "labelGadget1",
                        xmLabelGadgetClass, form6,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "First Row" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( labelGadget1, (char *) UxNtupleScanContext );

        firstScText = XtVaCreateManagedWidget( "firstScText",
                        xmTextWidgetClass, form6,
                        XmNmarginWidth, 2,
                        XmNmarginHeight, 2,
                        XmNcolumns, 8,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftWidget, labelGadget1,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( firstScText, (char *) UxNtupleScanContext );

        labelGadget2 = XtVaCreateManagedWidget( "labelGadget2",
                        xmLabelGadgetClass, form6,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftWidget, firstScText,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        RES_CONVERT( XmNlabelString, "Number of Rows" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( labelGadget2, (char *) UxNtupleScanContext );

        nrowScText = XtVaCreateManagedWidget( "nrowScText",
                        xmTextWidgetClass, form6,
                        XmNmarginWidth, 2,
                        XmNmarginHeight, 2,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftWidget, labelGadget2,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNcolumns, 8,
                        XmNbottomAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( nrowScText, (char *) UxNtupleScanContext );

        nextScButton = XtVaCreateManagedWidget( "nextScButton",
                        xmPushButtonWidgetClass, form6,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNwidth, 70,
                        XmNleftAttachment, XmATTACH_NONE,
                        RES_CONVERT( XmNlabelString, "Next" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( nextScButton, (char *) UxNtupleScanContext );

        previousScButton = XtVaCreateManagedWidget( "previousScButton",
                        xmPushButtonWidgetClass, form6,
                        XmNrightWidget, nextScButton,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 15,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftAttachment, XmATTACH_NONE,
                        RES_CONVERT( XmNlabelString, "Previous" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNwidth, 70,
                        NULL );

        UxPutContext( previousScButton, (char *) UxNtupleScanContext );

        scanScButton = XtVaCreateManagedWidget( "scanScButton",
                        xmPushButtonWidgetClass, form6,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, previousScButton,
                        XmNrightOffset, 15,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        RES_CONVERT( XmNlabelString, "Re-Scan" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNwidth, 70,
                        NULL );

        UxPutContext( scanScButton, (char *) UxNtupleScanContext );

        separatorGadget5 = XtVaCreateManagedWidget( "separatorGadget5",
                        xmSeparatorGadgetClass, ntupleScan,
                        XmNtopWidget, form6,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( separatorGadget5, (char *) UxNtupleScanContext );

        columnsScLabel = XtVaCreateManagedWidget( "columnsScLabel",
                        xmLabelWidgetClass, ntupleScan,
                        XmNleftPosition, 75,
                        XmNleftAttachment, XmATTACH_POSITION,
                        RES_CONVERT( XmNlabelString, "0 R  0 C" ),
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNalignment, XmALIGNMENT_END,
                        XmNheight, 25,
                        XmNwidth, 95,
                        NULL );

        UxPutContext( columnsScLabel, (char *) UxNtupleScanContext );

        titleScLabel = XtVaCreateManagedWidget( "titleScLabel",
                        xmLabelWidgetClass, ntupleScan,
                        XmNrightWidget, columnsScLabel,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        XmNheight, 25,
                        NULL );

        UxPutContext( titleScLabel, (char *) UxNtupleScanContext );

        XtAddCallback( ntupleScan, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxNtupleScanContext );



        return ( ntupleScan );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_ntupleScan()
{
        Widget                  rtrn;
        _UxCntupleScan          *UxContext;

        UxNtupleScanContext = UxContext =
                (_UxCntupleScan *) XtMalloc( sizeof(_UxCntupleScan) );

        rtrn = _Uxbuild_ntupleScan();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_ntupleScan()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_ntupleScan();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

