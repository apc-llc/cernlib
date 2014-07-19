/*
 * $Id: contpanel.c,v 1.1.1.1 1996/03/01 11:39:06 mclareni Exp $
 *
 * $Log: contpanel.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:06  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.40  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/


/*******************************************************************************
        contpanel.c

       Associated Header file: contpanel.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/SeparatoG.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/ToggleB.h>
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
#include "fpanelsh/contpanel.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_contourPanelShell()
{
        Widget          _UxParent;


        /* Creation of contourPanelShell */
        _UxParent = UxParent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        contourPanelShell = XtVaCreatePopupShell( "contourPanelShell",
                        topLevelShellWidgetClass,
                        _UxParent,
                        XmNshellUnitType, Xm100TH_FONT_UNITS,
                        XmNtitle, "Contour Panel",
                        XmNiconName, "Contour Panel",
                        XmNallowShellResize, TRUE,
                        NULL );
        UxPutContext( contourPanelShell, (char *) UxContourPanelShellContext );


        /* Creation of contourForm */
        contourForm = XtVaCreateManagedWidget( "contourForm",
                        xmFormWidgetClass,
                        contourPanelShell,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        NULL );
        UxPutContext( contourForm, (char *) UxContourPanelShellContext );


        /* Creation of contComputeButton */
        contComputeButton = XtVaCreateManagedWidget( "contComputeButton",
                        xmPushButtonWidgetClass,
                        contourForm,
                        XmNwidth, 1000,
                        XmNheight, 400,
                        XmNbottomOffset, 100,
                        RES_CONVERT( XmNlabelString, "Compute" ),
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( contComputeButton, (char *) UxContourPanelShellContext );


        /* Creation of contOverlayToggle */
        contOverlayToggle = XtVaCreateManagedWidget( "contOverlayToggle",
                        xmToggleButtonWidgetClass,
                        contourForm,
                        RES_CONVERT( XmNlabelString, "Overlay" ),
                        XmNleftOffset, 300,
                        XmNheight, 400,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, contComputeButton,
                        NULL );
        UxPutContext( contOverlayToggle, (char *) UxContourPanelShellContext );


        /* Creation of contNum1Label */
        contNum1Label = XtVaCreateManagedWidget( "contNum1Label",
                        xmLabelGadgetClass,
                        contourForm,
                        RES_CONVERT( XmNlabelString, "Num1" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftOffset, 100,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contComputeButton,
                        NULL );
        UxPutContext( contNum1Label, (char *) UxContourPanelShellContext );


        /* Creation of contNum1Text */
        contNum1Text = XtVaCreateManagedWidget( "contNum1Text",
                        xmTextFieldWidgetClass,
                        contourForm,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNcolumns, 2,
                        XmNeditable, TRUE,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 100,
                        XmNleftWidget, contNum1Label,
                        XmNmaxLength, 2,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contComputeButton,
                        NULL );
        UxPutContext( contNum1Text, (char *) UxContourPanelShellContext );


        /* Creation of contNum2Label */
        contNum2Label = XtVaCreateManagedWidget( "contNum2Label",
                        xmLabelGadgetClass,
                        contourForm,
                        RES_CONVERT( XmNlabelString, "Num2" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 200,
                        XmNleftWidget, contNum1Text,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contComputeButton,
                        NULL );
        UxPutContext( contNum2Label, (char *) UxContourPanelShellContext );


        /* Creation of contNum2Text */
        contNum2Text = XtVaCreateManagedWidget( "contNum2Text",
                        xmTextFieldWidgetClass,
                        contourForm,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNcolumns, 2,
                        XmNeditable, TRUE,
                        XmNmaxLength, 2,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 100,
                        XmNleftWidget, contNum2Label,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contComputeButton,
                        NULL );
        UxPutContext( contNum2Text, (char *) UxContourPanelShellContext );


        /* Creation of contNptLabel */
        contNptLabel = XtVaCreateManagedWidget( "contNptLabel",
                        xmLabelGadgetClass,
                        contourForm,
                        RES_CONVERT( XmNlabelString, "Npt" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 200,
                        XmNleftWidget, contNum2Text,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contComputeButton,
                        NULL );
        UxPutContext( contNptLabel, (char *) UxContourPanelShellContext );


        /* Creation of contNptText */
        contNptText = XtVaCreateManagedWidget( "contNptText",
                        xmTextFieldWidgetClass,
                        contourForm,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNcolumns, 2,
                        XmNeditable, TRUE,
                        XmNmaxLength, 2,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 100,
                        XmNleftWidget, contNptLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contComputeButton,
                        NULL );
        UxPutContext( contNptText, (char *) UxContourPanelShellContext );


        /* Creation of contNfoundLabel */
        contNfoundLabel = XtVaCreateManagedWidget( "contNfoundLabel",
                        xmLabelGadgetClass,
                        contourForm,
                        RES_CONVERT( XmNlabelString, "Nfnd" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 200,
                        XmNleftWidget, contNptText,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contComputeButton,
                        NULL );
        UxPutContext( contNfoundLabel, (char *) UxContourPanelShellContext );


        /* Creation of contNfoundText */
        contNfoundText = XtVaCreateManagedWidget( "contNfoundText",
                        xmTextFieldWidgetClass,
                        contourForm,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNcolumns, 2,
                        XmNeditable, FALSE,
                        XmNmaxLength, 2,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 100,
                        XmNleftWidget, contNfoundLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contComputeButton,
                        NULL );
        UxPutContext( contNfoundText, (char *) UxContourPanelShellContext );


        /* Creation of contSeparator */
        contSeparator = XtVaCreateManagedWidget( "contSeparator",
                        xmSeparatorGadgetClass,
                        contourForm,
                        XmNorientation, XmHORIZONTAL,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contNum1Text,
                        NULL );
        UxPutContext( contSeparator, (char *) UxContourPanelShellContext );


        /* Creation of contSaveButton */
        contSaveButton = XtVaCreateManagedWidget( "contSaveButton",
                        xmPushButtonWidgetClass,
                        contourForm,
                        XmNheight, 400,
                        RES_CONVERT( XmNlabelString, "Save in PAW vectors" ),
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contSeparator,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( contSaveButton, (char *) UxContourPanelShellContext );


        /* Creation of contXsaveLabel */
        contXsaveLabel = XtVaCreateManagedWidget( "contXsaveLabel",
                        xmLabelGadgetClass,
                        contourForm,
                        RES_CONVERT( XmNlabelString, "X" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contSaveButton,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( contXsaveLabel, (char *) UxContourPanelShellContext );


        /* Creation of contXsaveText */
        contXsaveText = XtVaCreateManagedWidget( "contXsaveText",
                        xmTextFieldWidgetClass,
                        contourForm,
                        XmNcolumns, res_data.kuipNameColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNmaxLength, res_data.parNameColumns,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 100,
                        XmNleftWidget, contXsaveLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contSaveButton,
                        NULL );
        UxPutContext( contXsaveText, (char *) UxContourPanelShellContext );


        /* Creation of contYsaveLabel */
        contYsaveLabel = XtVaCreateManagedWidget( "contYsaveLabel",
                        xmLabelGadgetClass,
                        contourForm,
                        RES_CONVERT( XmNlabelString, "Y" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contXsaveLabel,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( contYsaveLabel, (char *) UxContourPanelShellContext );


        /* Creation of contYsaveText */
        contYsaveText = XtVaCreateManagedWidget( "contYsaveText",
                        xmTextFieldWidgetClass,
                        contourForm,
                        XmNcolumns, res_data.kuipNameColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNmaxLength, res_data.parNameColumns,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 100,
                        XmNleftWidget, contXsaveLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contXsaveText,
                        NULL );
        UxPutContext( contYsaveText, (char *) UxContourPanelShellContext );


        /* Creation of contCmdSeparator */
        contCmdSeparator = XtVaCreateManagedWidget( "contCmdSeparator",
                        xmSeparatorGadgetClass,
                        contourForm,
                        XmNorientation, XmHORIZONTAL,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contYsaveText,
                        NULL );
        UxPutContext( contCmdSeparator, (char *) UxContourPanelShellContext );


        /* Creation of contCloseButton */
        contCloseButton = XtVaCreateManagedWidget( "contCloseButton",
                        xmPushButtonWidgetClass,
                        contourForm,
                        XmNwidth, 1000,
                        XmNheight, 400,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 100,
                        XmNtopWidget, contCmdSeparator,
                        NULL );
        UxPutContext( contCloseButton, (char *) UxContourPanelShellContext );


        /* Creation of contHelpButton */
        contHelpButton = XtVaCreateManagedWidget( "contHelpButton",
                        xmPushButtonWidgetClass,
                        contourForm,
                        XmNwidth, 1000,
                        XmNheight, 400,
                        RES_CONVERT( XmNlabelString, "Help" ),
                        XmNbottomOffset, 100,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 200,
                        XmNleftWidget, contCloseButton,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, contCmdSeparator,
                        NULL );
        UxPutContext( contHelpButton, (char *) UxContourPanelShellContext );

        XtVaSetValues(contourForm,
                        XmNcancelButton, contCloseButton,
                        NULL );


        XtAddCallback( contourPanelShell, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxContourPanelShellContext);


        return ( contourPanelShell );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_contourPanel( swidget _UxUxParent )
{
        Widget                  rtrn;
        _UxCcontourPanelShell   *UxContext;

        UxContourPanelShellContext = UxContext =
                (_UxCcontourPanelShell *) UxNewContext( sizeof(
_UxCcontourPanelShell), False
);

        UxParent = _UxUxParent;

        rtrn = _Uxbuild_contourPanelShell();

        return(rtrn);
}
