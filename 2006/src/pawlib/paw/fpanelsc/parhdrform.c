/*
 * $Id: parhdrform.c,v 1.1.1.1 1996/03/01 11:39:08 mclareni Exp $
 *
 * $Log: parhdrform.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:08  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.41  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/


/*******************************************************************************
        parhdrform.c

       Associated Header file: parhdrform.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/SeparatoG.h>
#include <Xm/Label.h>
#include <Xm/Form.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/parhdrform.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_parHeaderForm()
{
        Widget          _UxParent;


        /* Creation of parHeaderForm */
        _UxParent = UxParent;
        if ( _UxParent == NULL )
        {
                _UxParent = XtVaCreatePopupShell( "parHeaderForm_shell",
                        topLevelShellWidgetClass, UxTopLevel,
                        XmNshellUnitType, Xm100TH_FONT_UNITS,
                        XmNtitle, "parHeaderForm",
                        XmNiconName, "parHeaderForm",
                        NULL );

        }

        parHeaderForm = XtVaCreateManagedWidget( "parHeaderForm",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNresizePolicy, XmRESIZE_ANY,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNmarginHeight, 0,
                        XmNmarginWidth, 0,
                        RES_CONVERT( XmNforeground, "Red" ),
                        NULL );
        UxPutContext( parHeaderForm, (char *) UxParHeaderFormContext );


        /* Creation of parHeaderRightForm */
        parHeaderRightForm = XtVaCreateManagedWidget( "parHeaderRightForm",
                        xmFormWidgetClass,
                        parHeaderForm,
                        XmNresizePolicy, XmRESIZE_ANY,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNallowOverlap, FALSE,
                        XmNshadowThickness, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( parHeaderRightForm, (char *) UxParHeaderFormContext );


        /* Creation of parValueLabel */
        parValueLabel = XtVaCreateManagedWidget( "parValueLabel",
                        xmLabelWidgetClass,
                        parHeaderRightForm,
                        RES_CONVERT( XmNlabelString, "Value" ),
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNforeground, "red" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( parValueLabel, (char *) UxParHeaderFormContext );


        /* Creation of parFixLabel */
        parFixLabel = XtVaCreateManagedWidget( "parFixLabel",
                        xmLabelWidgetClass,
                        parHeaderRightForm,
                        RES_CONVERT( XmNlabelString, "F/R" ),
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNforeground, "red" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, parValueLabel,
                        NULL );
        UxPutContext( parFixLabel, (char *) UxParHeaderFormContext );


        /* Creation of parLowLimitLabel */
        parLowLimitLabel = XtVaCreateWidget( "parLowLimitLabel",
                        xmLabelWidgetClass,
                        parHeaderRightForm,
                        RES_CONVERT( XmNlabelString, "Lower limit" ),
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNforeground, "red" ),
                        NULL );
        UxPutContext( parLowLimitLabel, (char *) UxParHeaderFormContext );


        /* Creation of parUppLimitLabel */
        parUppLimitLabel = XtVaCreateWidget( "parUppLimitLabel",
                        xmLabelWidgetClass,
                        parHeaderRightForm,
                        RES_CONVERT( XmNlabelString, "Upper limit" ),
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNforeground, "red" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, parLowLimitLabel,
                        NULL );
        UxPutContext( parUppLimitLabel, (char *) UxParHeaderFormContext );


        /* Creation of parStepLabel */
        parStepLabel = XtVaCreateWidget( "parStepLabel",
                        xmLabelWidgetClass,
                        parHeaderRightForm,
                        RES_CONVERT( XmNlabelString, "Step" ),
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNforeground, "red" ),
                        NULL );
        UxPutContext( parStepLabel, (char *) UxParHeaderFormContext );


        /* Creation of parEparabLabel */
        parEparabLabel = XtVaCreateWidget( "parEparabLabel",
                        xmLabelWidgetClass,
                        parHeaderRightForm,
                        RES_CONVERT( XmNlabelString, "Eparab" ),
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNforeground, "red" ),
                        NULL );
        UxPutContext( parEparabLabel, (char *) UxParHeaderFormContext );


        /* Creation of parEminusLabel */
        parEminusLabel = XtVaCreateWidget( "parEminusLabel",
                        xmLabelWidgetClass,
                        parHeaderRightForm,
                        RES_CONVERT( XmNlabelString, "Eminus" ),
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNforeground, "red" ),
                        NULL );
        UxPutContext( parEminusLabel, (char *) UxParHeaderFormContext );


        /* Creation of parEplusLabel */
        parEplusLabel = XtVaCreateWidget( "parEplusLabel",
                        xmLabelWidgetClass,
                        parHeaderRightForm,
                        RES_CONVERT( XmNlabelString, "Eplus" ),
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNforeground, "red" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, parEminusLabel,
                        NULL );
        UxPutContext( parEplusLabel, (char *) UxParHeaderFormContext );


        /* Creation of parGlobccLabel */
        parGlobccLabel = XtVaCreateWidget( "parGlobccLabel",
                        xmLabelWidgetClass,
                        parHeaderRightForm,
                        RES_CONVERT( XmNlabelString, "Globcc" ),
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNforeground, "red" ),
                        NULL );
        UxPutContext( parGlobccLabel, (char *) UxParHeaderFormContext );


        /* Creation of parHeaderLeftForm */
        parHeaderLeftForm = XtVaCreateManagedWidget( "parHeaderLeftForm",
                        xmFormWidgetClass,
                        parHeaderForm,
                        XmNresizePolicy, XmRESIZE_ANY,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNallowOverlap, FALSE,
                        XmNshadowThickness, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( parHeaderLeftForm, (char *) UxParHeaderFormContext );


        /* Creation of parNumberLabel */
        parNumberLabel = XtVaCreateManagedWidget( "parNumberLabel",
                        xmLabelWidgetClass,
                        parHeaderLeftForm,
                        RES_CONVERT( XmNlabelString, "No" ),
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNforeground, "red" ),
                        NULL );
        UxPutContext( parNumberLabel, (char *) UxParHeaderFormContext );


        /* Creation of parNameLabel */
        parNameLabel = XtVaCreateManagedWidget( "parNameLabel",
                        xmLabelWidgetClass,
                        parHeaderLeftForm,
                        RES_CONVERT( XmNlabelString, "Name" ),
                        XmNrecomputeSize, FALSE,
                        RES_CONVERT( XmNforeground, "red" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, parNumberLabel,
                        NULL );
        UxPutContext( parNameLabel, (char *) UxParHeaderFormContext );


        /* Creation of parTopSeparator */
        parTopSeparator = XtVaCreateManagedWidget( "parTopSeparator",
                        xmSeparatorGadgetClass,
                        parHeaderForm,
                        XmNseparatorType, XmSHADOW_ETCHED_OUT,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, parHeaderRightForm,
                        NULL );
        UxPutContext( parTopSeparator, (char *) UxParHeaderFormContext );


        XtAddCallback( parHeaderForm, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxParHeaderFormContext);


        return ( parHeaderForm );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_parHeaderForm( swidget _UxUxParent )
{
        Widget                  rtrn;
        _UxCparHeaderForm       *UxContext;

        UxParHeaderFormContext = UxContext =
                (_UxCparHeaderForm *) UxNewContext( sizeof(_UxCparHeaderForm),
False );

        UxParent = _UxUxParent;

        rtrn = _Uxbuild_parHeaderForm();

        return(rtrn);
}
