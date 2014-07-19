/*
 * $Id: scanpanel.c,v 1.1.1.1 1996/03/01 11:39:08 mclareni Exp $
 *
 * $Log: scanpanel.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:08  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.42  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/


/*******************************************************************************
        scanpanel.c

       Associated Header file: scanpanel.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/SeparatoG.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <X11/Shell.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/


#include "fpanelsh/fplocal.h"
extern SpecResData res_data;


/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/scanpanel.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_scanPanelShell()
{
        Widget          _UxParent;


        /* Creation of scanPanelShell */
        _UxParent = UxParent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        scanPanelShell = XtVaCreatePopupShell( "scanPanelShell",
                        topLevelShellWidgetClass,
                        _UxParent,
                        XmNshellUnitType, Xm100TH_FONT_UNITS,
                        XmNtitle, "Scan Panel",
                        XmNiconName, "Scan Panel",
                        NULL );
        UxPutContext( scanPanelShell, (char *) UxScanPanelShellContext );


        /* Creation of scanForm */
        scanForm = XtVaCreateManagedWidget( "scanForm",
                        xmFormWidgetClass,
                        scanPanelShell,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        NULL );
        UxPutContext( scanForm, (char *) UxScanPanelShellContext );


        /* Creation of scanComputeButton */
        scanComputeButton = XtVaCreateManagedWidget( "scanComputeButton",
                        xmPushButtonWidgetClass,
                        scanForm,
                        XmNwidth, 1000,
                        XmNheight, 400,
                        RES_CONVERT( XmNlabelString, "Compute" ),
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( scanComputeButton, (char *) UxScanPanelShellContext );


        /* Creation of scanNumLabel */
        scanNumLabel = XtVaCreateManagedWidget( "scanNumLabel",
                        xmLabelGadgetClass,
                        scanForm,
                        RES_CONVERT( XmNlabelString, "Num" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftOffset, 100,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, scanComputeButton,
                        NULL );
        UxPutContext( scanNumLabel, (char *) UxScanPanelShellContext );


        /* Creation of scanNumText */
        scanNumText = XtVaCreateManagedWidget( "scanNumText",
                        xmTextFieldWidgetClass,
                        scanForm,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNcolumns, 2,
                        XmNeditable, TRUE,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 100,
                        XmNleftWidget, scanNumLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, scanComputeButton,
                        NULL );
        UxPutContext( scanNumText, (char *) UxScanPanelShellContext );


        /* Creation of scanNptLabel */
        scanNptLabel = XtVaCreateManagedWidget( "scanNptLabel",
                        xmLabelGadgetClass,
                        scanForm,
                        RES_CONVERT( XmNlabelString, "Npt" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 200,
                        XmNleftWidget, scanNumText,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, scanComputeButton,
                        NULL );
        UxPutContext( scanNptLabel, (char *) UxScanPanelShellContext );


        /* Creation of scanNptText */
        scanNptText = XtVaCreateManagedWidget( "scanNptText",
                        xmTextFieldWidgetClass,
                        scanForm,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNcolumns, 3,
                        XmNeditable, TRUE,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 100,
                        XmNleftWidget, scanNptLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, scanComputeButton,
                        NULL );
        UxPutContext( scanNptText, (char *) UxScanPanelShellContext );


        /* Creation of scanFromLabel */
        scanFromLabel = XtVaCreateManagedWidget( "scanFromLabel",
                        xmLabelGadgetClass,
                        scanForm,
                        RES_CONVERT( XmNlabelString, "From" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 200,
                        XmNleftWidget, scanNptText,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, scanComputeButton,
                        NULL );
        UxPutContext( scanFromLabel, (char *) UxScanPanelShellContext );


        /* Creation of scanFromText */
        scanFromText = XtVaCreateManagedWidget( "scanFromText",
                        xmTextFieldWidgetClass,
                        scanForm,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNcolumns, 8,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 100,
                        XmNleftWidget, scanFromLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, scanComputeButton,
                        NULL );
        UxPutContext( scanFromText, (char *) UxScanPanelShellContext );


        /* Creation of scanToLabel */
        scanToLabel = XtVaCreateManagedWidget( "scanToLabel",
                        xmLabelGadgetClass,
                        scanForm,
                        RES_CONVERT( XmNlabelString, "To" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 200,
                        XmNleftWidget, scanFromText,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, scanComputeButton,
                        NULL );
        UxPutContext( scanToLabel, (char *) UxScanPanelShellContext );


        /* Creation of scanToText */
        scanToText = XtVaCreateManagedWidget( "scanToText",
                        xmTextFieldWidgetClass,
                        scanForm,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNcolumns, 8,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 100,
                        XmNleftWidget, scanToLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, scanComputeButton,
                        NULL );
        UxPutContext( scanToText, (char *) UxScanPanelShellContext );


        /* Creation of scanCmdSeparator */
        scanCmdSeparator = XtVaCreateManagedWidget( "scanCmdSeparator",
                        xmSeparatorGadgetClass,
                        scanForm,
                        XmNorientation, XmHORIZONTAL,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, scanNumText,
                        NULL );
        UxPutContext( scanCmdSeparator, (char *) UxScanPanelShellContext );


        /* Creation of scanCloseButton */
        scanCloseButton = XtVaCreateManagedWidget( "scanCloseButton",
                        xmPushButtonWidgetClass,
                        scanForm,
                        XmNwidth, 1000,
                        XmNheight, 400,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, scanCmdSeparator,
                        NULL );
        UxPutContext( scanCloseButton, (char *) UxScanPanelShellContext );


        /* Creation of scanHelpButton */
        scanHelpButton = XtVaCreateManagedWidget( "scanHelpButton",
                        xmPushButtonWidgetClass,
                        scanForm,
                        XmNwidth, 1000,
                        XmNheight, 400,
                        RES_CONVERT( XmNlabelString, "Help" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 200,
                        XmNleftWidget, scanCloseButton,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, scanCmdSeparator,
                        NULL );
        UxPutContext( scanHelpButton, (char *) UxScanPanelShellContext );

        XtVaSetValues(scanForm,
                        XmNcancelButton, scanCloseButton,
                        NULL );


        XtAddCallback( scanPanelShell, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxScanPanelShellContext);


        return ( scanPanelShell );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_scanPanel( swidget _UxUxParent )
{
        Widget                  rtrn;
        _UxCscanPanelShell      *UxContext;

        UxScanPanelShellContext = UxContext =
                (_UxCscanPanelShell *) UxNewContext( sizeof(_UxCscanPanelShell)
, False );

        UxParent = _UxUxParent;

        rtrn = _Uxbuild_scanPanelShell();

        return(rtrn);
}
