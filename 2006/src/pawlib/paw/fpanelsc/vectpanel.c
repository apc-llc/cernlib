/*
 * $Id: vectpanel.c,v 1.1.1.1 1996/03/01 11:39:09 mclareni Exp $
 *
 * $Log: vectpanel.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:09  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.42  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/


/*******************************************************************************
        vectpanel.c

       Associated Header file: vectpanel.h
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
#include "fpanelsh/vectpanel.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_vectPanelShell()
{
        Widget          _UxParent;


        /* Creation of vectPanelShell */
        _UxParent = UxParent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        vectPanelShell = XtVaCreatePopupShell( "vectPanelShell",
                        topLevelShellWidgetClass,
                        _UxParent,
                        XmNshellUnitType, Xm100TH_FONT_UNITS,
                        XmNtitle, "Vector Transfer Panel",
                        XmNiconName, "Vector Transfer Panel",
                        XmNallowShellResize, TRUE,
                        NULL );
        UxPutContext( vectPanelShell, (char *) UxVectPanelShellContext );


        /* Creation of vectForm */
        vectForm = XtVaCreateManagedWidget( "vectForm",
                        xmFormWidgetClass,
                        vectPanelShell,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        NULL );
        UxPutContext( vectForm, (char *) UxVectPanelShellContext );


        /* Creation of vectSaveButton */
        vectSaveButton = XtVaCreateManagedWidget( "vectSaveButton",
                        xmPushButtonWidgetClass,
                        vectForm,
                        XmNheight, 400,
                        RES_CONVERT( XmNlabelString, "Save in PAW vectors" ),
                        XmNx, 0,
                        XmNy, 0,
                        XmNleftOffset, 100,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( vectSaveButton, (char *) UxVectPanelShellContext );


        /* Creation of vectParLabel */
        vectParLabel = XtVaCreateManagedWidget( "vectParLabel",
                        xmLabelGadgetClass,
                        vectForm,
                        RES_CONVERT( XmNlabelString, "Parameters" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftOffset, 100,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, vectSaveButton,
                        NULL );
        UxPutContext( vectParLabel, (char *) UxVectPanelShellContext );


        /* Creation of vectParText */
        vectParText = XtVaCreateManagedWidget( "vectParText",
                        xmTextFieldWidgetClass,
                        vectForm,
                        XmNcolumns, res_data.kuipNameColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNmaxLength, res_data.parNameColumns,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 1600,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, vectSaveButton,
                        NULL );
        UxPutContext( vectParText, (char *) UxVectPanelShellContext );


        /* Creation of vectStepLabel */
        vectStepLabel = XtVaCreateManagedWidget( "vectStepLabel",
                        xmLabelGadgetClass,
                        vectForm,
                        RES_CONVERT( XmNlabelString, "Steps" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, vectParText,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( vectStepLabel, (char *) UxVectPanelShellContext );


        /* Creation of vectStepText */
        vectStepText = XtVaCreateManagedWidget( "vectStepText",
                        xmTextFieldWidgetClass,
                        vectForm,
                        XmNcolumns, res_data.kuipNameColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNmaxLength, res_data.parNameColumns,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 1600,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, vectParText,
                        NULL );
        UxPutContext( vectStepText, (char *) UxVectPanelShellContext );


        /* Creation of vectLowLabel */
        vectLowLabel = XtVaCreateManagedWidget( "vectLowLabel",
                        xmLabelGadgetClass,
                        vectForm,
                        RES_CONVERT( XmNlabelString, "Lower bounds" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, vectStepText,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( vectLowLabel, (char *) UxVectPanelShellContext );


        /* Creation of vectLowText */
        vectLowText = XtVaCreateManagedWidget( "vectLowText",
                        xmTextFieldWidgetClass,
                        vectForm,
                        XmNcolumns, res_data.kuipNameColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNmaxLength, res_data.parNameColumns,
                        XmNx, 0,
                        XmNy, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, vectStepText,
                        XmNleftOffset, 1600,
                        NULL );
        UxPutContext( vectLowText, (char *) UxVectPanelShellContext );


        /* Creation of vectUppLabel */
        vectUppLabel = XtVaCreateManagedWidget( "vectUppLabel",
                        xmLabelGadgetClass,
                        vectForm,
                        RES_CONVERT( XmNlabelString, "Upper bounds" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, vectLowText,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( vectUppLabel, (char *) UxVectPanelShellContext );


        /* Creation of vectUppText */
        vectUppText = XtVaCreateManagedWidget( "vectUppText",
                        xmTextFieldWidgetClass,
                        vectForm,
                        XmNcolumns, res_data.kuipNameColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNmaxLength, res_data.parNameColumns,
                        XmNx, 0,
                        XmNy, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, vectLowText,
                        XmNleftOffset, 1600,
                        NULL );
        UxPutContext( vectUppText, (char *) UxVectPanelShellContext );


        /* Creation of vectErrorLabel */
        vectErrorLabel = XtVaCreateManagedWidget( "vectErrorLabel",
                        xmLabelGadgetClass,
                        vectForm,
                        RES_CONVERT( XmNlabelString, "Errors" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, vectUppText,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( vectErrorLabel, (char *) UxVectPanelShellContext );


        /* Creation of vectErrorText */
        vectErrorText = XtVaCreateManagedWidget( "vectErrorText",
                        xmTextFieldWidgetClass,
                        vectForm,
                        XmNcolumns, res_data.kuipNameColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNmaxLength, res_data.parNameColumns,
                        XmNx, 0,
                        XmNy, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, vectUppText,
                        XmNleftOffset, 1600,
                        NULL );
        UxPutContext( vectErrorText, (char *) UxVectPanelShellContext );


        /* Creation of vectCmdSeparator */
        vectCmdSeparator = XtVaCreateManagedWidget( "vectCmdSeparator",
                        xmSeparatorGadgetClass,
                        vectForm,
                        XmNorientation, XmHORIZONTAL,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, vectErrorText,
                        NULL );
        UxPutContext( vectCmdSeparator, (char *) UxVectPanelShellContext );


        /* Creation of vectCloseButton */
        vectCloseButton = XtVaCreateManagedWidget( "vectCloseButton",
                        xmPushButtonWidgetClass,
                        vectForm,
                        XmNwidth, 1000,
                        XmNheight, 400,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, vectCmdSeparator,
                        NULL );
        UxPutContext( vectCloseButton, (char *) UxVectPanelShellContext );


        /* Creation of vectHelpButton */
        vectHelpButton = XtVaCreateManagedWidget( "vectHelpButton",
                        xmPushButtonWidgetClass,
                        vectForm,
                        XmNwidth, 1000,
                        XmNheight, 400,
                        RES_CONVERT( XmNlabelString, "Help" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 100,
                        XmNtopWidget, vectCmdSeparator,
                        XmNleftWidget, vectCloseButton,
                        NULL );
        UxPutContext( vectHelpButton, (char *) UxVectPanelShellContext );

        XtVaSetValues(vectForm,
                        XmNcancelButton, vectCloseButton,
                        NULL );


        XtAddCallback( vectPanelShell, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxVectPanelShellContext);


        return ( vectPanelShell );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_vectPanel( swidget _UxUxParent )
{
        Widget                  rtrn;
        _UxCvectPanelShell      *UxContext;

        UxVectPanelShellContext = UxContext =
                (_UxCvectPanelShell *) UxNewContext( sizeof(_UxCvectPanelShell)
, False );

        UxParent = _UxUxParent;

        rtrn = _Uxbuild_vectPanelShell();

        return(rtrn);
}
