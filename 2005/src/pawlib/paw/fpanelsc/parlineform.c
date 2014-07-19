/*
 * $Id: parlineform.c,v 1.1.1.1 1996/03/01 11:39:08 mclareni Exp $
 *
 * $Log: parlineform.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:08  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/07 24/07/95  09.31.09  by  Julian Bunn*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/


/*******************************************************************************
        parlineform.c

       Associated Header file: parlineform.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/PushBG.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <Xm/ArrowBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/Scale.h>
#include <Xm/Label.h>
#include <Xm/ToggleB.h>
#include <Xm/TextF.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/


#include "fpanelsh/fplocal.h"
#define CONTEXT_MACRO_ACCESS 1
#define _UX_XT_H_
/*#define Widget swidget*/  /* workaround for UIMX interprete bug */
#include "fpanelsh/mainpanel.h"
#undef Widget
#undef UxParent
#undef CONTEXT_MACRO_ACCESS
extern SpecResData res_data;

static char parNumberStr[3]; /* parameter number put here */


/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/parlineform.h"
#undef CONTEXT_MACRO_ACCESS

/*******************************************************************************
       The following function is an event-handler for posting menus.
*******************************************************************************/

static void     _UxparLineFormMenuPost(
                        Widget wgt,
                        XtPointer client_data,
                        XEvent *event)
{
        Widget  menu = (Widget) client_data;
        int     which_button;

        XtVaGetValues( menu, XmNwhichButton, &which_button, NULL );

        if ( event->xbutton.button == which_button )
        {
                XmMenuPosition( menu, (XButtonPressedEvent *) event );
                XtManageChild( menu );
        }
}


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_parLineForm()
{
        Widget          _UxParent;
        Widget          parLineMenu_shell;


        /* Creation of parLineForm */
        _UxParent = UxParent;
        if ( _UxParent == NULL )
        {
                _UxParent = XtVaCreatePopupShell( "parLineForm_shell",
                        topLevelShellWidgetClass, UxTopLevel,
                        XmNshellUnitType, Xm100TH_FONT_UNITS,
                        XmNtitle, "parLineForm",
                        XmNiconName, "parLineForm",
                        NULL );

        }

        parLineForm = XtVaCreateManagedWidget( "parLineForm",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNresizePolicy, XmRESIZE_ANY,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNallowOverlap, FALSE,
                        NULL );
        UxPutContext( parLineForm, (char *) UxParLineFormContext );


        /* Creation of parLineRightForm */
        parLineRightForm = XtVaCreateManagedWidget( "parLineRightForm",
                        xmFormWidgetClass,
                        parLineForm,
                        XmNresizePolicy, XmRESIZE_ANY,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNallowOverlap, FALSE,
                        XmNshadowThickness, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( parLineRightForm, (char *) UxParLineFormContext );


        /* Creation of parScaleAdjustButton */
        parScaleAdjustButton = XtVaCreateManagedWidget( "parScaleAdjustButton",
                        xmPushButtonWidgetClass,
                        parLineRightForm,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );
        UxPutContext( parScaleAdjustButton, (char *) UxParLineFormContext );


        /* Creation of parValueText */
        parValueText = XtVaCreateManagedWidget( "parValueText",
                        xmTextFieldWidgetClass,
                        parLineRightForm,
                        XmNcolumns, res_data.parValueColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, parScaleAdjustButton,
                        NULL );
        UxPutContext( parValueText, (char *) UxParLineFormContext );


        /* Creation of parFixToggle */
        parFixToggle = XtVaCreateManagedWidget( "parFixToggle",
                        xmToggleButtonWidgetClass,
                        parLineRightForm,
                        RES_CONVERT( XmNlabelString, "R" ),
                        XmNindicatorSize, 0,
                        XmNborderWidth, 0,
                        XmNhighlightOnEnter, FALSE,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, parValueText,
                        NULL );
        UxPutContext( parFixToggle, (char *) UxParLineFormContext );


        /* Creation of parLowLimitText */
        parLowLimitText = XtVaCreateWidget( "parLowLimitText",
                        xmTextFieldWidgetClass,
                        parLineRightForm,
                        XmNcolumns, res_data.parLimitsColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        NULL );
        UxPutContext( parLowLimitText, (char *) UxParLineFormContext );


        /* Creation of parUppLimitText */
        parUppLimitText = XtVaCreateWidget( "parUppLimitText",
                        xmTextFieldWidgetClass,
                        parLineRightForm,
                        XmNcolumns, res_data.parLimitsColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, parLowLimitText,
                        NULL );
        UxPutContext( parUppLimitText, (char *) UxParLineFormContext );


        /* Creation of parStepText */
        parStepText = XtVaCreateWidget( "parStepText",
                        xmTextFieldWidgetClass,
                        parLineRightForm,
                        XmNcolumns, res_data.parStepColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        NULL );
        UxPutContext( parStepText, (char *) UxParLineFormContext );


        /* Creation of parEparabText */
        parEparabText = XtVaCreateWidget( "parEparabText",
                        xmTextFieldWidgetClass,
                        parLineRightForm,
                        XmNcolumns, res_data.parErrorsColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNeditable, FALSE,
                        XmNcursorPositionVisible, FALSE,
                        NULL );
        UxPutContext( parEparabText, (char *) UxParLineFormContext );


        /* Creation of parEminusText */
        parEminusText = XtVaCreateWidget( "parEminusText",
                        xmTextFieldWidgetClass,
                        parLineRightForm,
                        XmNcolumns, res_data.parErrorsColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNeditable, FALSE,
                        XmNcursorPositionVisible, FALSE,
                        NULL );
        UxPutContext( parEminusText, (char *) UxParLineFormContext );


        /* Creation of parEplusText */
        parEplusText = XtVaCreateWidget( "parEplusText",
                        xmTextFieldWidgetClass,
                        parLineRightForm,
                        XmNcolumns, res_data.parErrorsColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNeditable, FALSE,
                        XmNcursorPositionVisible, FALSE,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, parEminusText,
                        NULL );
        UxPutContext( parEplusText, (char *) UxParLineFormContext );


        /* Creation of parGlobccText */
        parGlobccText = XtVaCreateWidget( "parGlobccText",
                        xmTextFieldWidgetClass,
                        parLineRightForm,
                        XmNcolumns, res_data.parErrorsColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNeditable, FALSE,
                        XmNcursorPositionVisible, FALSE,
                        NULL );
        UxPutContext( parGlobccText, (char *) UxParLineFormContext );


        /* Creation of parLineLeftForm */
        parLineLeftForm = XtVaCreateManagedWidget( "parLineLeftForm",
                        xmFormWidgetClass,
                        parLineForm,
                        XmNresizePolicy, XmRESIZE_ANY,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNallowOverlap, FALSE,
                        XmNshadowThickness, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightWidget, parLineRightForm,
                        NULL );
        UxPutContext( parLineLeftForm, (char *) UxParLineFormContext );


        /* Creation of parNumberValueLabel */
        parNumberValueLabel = XtVaCreateManagedWidget( "parNumberValueLabel",
                        xmLabelWidgetClass,
                        parLineLeftForm,
                        RES_CONVERT( XmNlabelString, parNumberStr ),
                        XmNrecomputeSize, FALSE,
                        NULL );
        UxPutContext( parNumberValueLabel, (char *) UxParLineFormContext );


        /* Creation of parNameText */
        parNameText = XtVaCreateManagedWidget( "parNameText",
                        xmTextFieldWidgetClass,
                        parLineLeftForm,
                        XmNcolumns, res_data.parNameColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, parNumberValueLabel,
                        XmNmaxLength, res_data.parNameColumns,
                        NULL );
        UxPutContext( parNameText, (char *) UxParLineFormContext );


        /* Creation of parScale */
        parScale = XtVaCreateManagedWidget( "parScale",
                        xmScaleWidgetClass,
                        parLineLeftForm,
                        XmNorientation, XmHORIZONTAL,
                        XmNwidth, res_data.parScaleWidth,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, parNameText,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNmaximum, res_data.parScaleResolution,
                        XmNscaleMultiple, res_data.parScaleResolution/10,
                        NULL );
        UxPutContext( parScale, (char *) UxParLineFormContext );


        /* Creation of parBotSeparator */
        parBotSeparator = XtVaCreateManagedWidget( "parBotSeparator",
                        xmSeparatorGadgetClass,
                        parLineForm,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNheight, res_data.parBotSeparatorHeight,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, parLineLeftForm,
                        NULL );
        UxPutContext( parBotSeparator, (char *) UxParLineFormContext );


        /* Creation of parScaleMinMaxForm */
        parScaleMinMaxForm = XtVaCreateWidget( "parScaleMinMaxForm",
                        xmFormWidgetClass,
                        parLineForm,
                        XmNresizePolicy, XmRESIZE_ANY,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNallowOverlap, FALSE,
                        XmNshadowThickness, 0,
                        NULL );
        UxPutContext( parScaleMinMaxForm, (char *) UxParLineFormContext );


        /* Creation of parScaleMinText */
        parScaleMinText = XtVaCreateManagedWidget( "parScaleMinText",
                        xmTextFieldWidgetClass,
                        parScaleMinMaxForm,
                        XmNcolumns, res_data.parScaleMinMaxColumns,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        NULL );
        UxPutContext( parScaleMinText, (char *) UxParLineFormContext );


        /* Creation of parScaleMinLeftArrow */
        parScaleMinLeftArrow = XtVaCreateManagedWidget( "parScaleMinLeftArrow",
                        xmArrowButtonGadgetClass,
                        parScaleMinMaxForm,
                        XmNwidth, 300,
                        XmNarrowDirection, XmARROW_LEFT,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftWidget, parScaleMinText,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNshadowThickness, 0,
                        NULL );
        UxPutContext( parScaleMinLeftArrow, (char *) UxParLineFormContext );


        /* Creation of parScaleMinRightArrow */
        parScaleMinRightArrow = XtVaCreateManagedWidget(
"parScaleMinRightArrow",
                        xmArrowButtonGadgetClass,
                        parScaleMinMaxForm,
                        XmNwidth, 300,
                        XmNarrowDirection, XmARROW_RIGHT,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, parScaleMinLeftArrow,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowThickness, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( parScaleMinRightArrow, (char *) UxParLineFormContext );


        /* Creation of parMinMaxSeparator */
        parMinMaxSeparator = XtVaCreateManagedWidget( "parMinMaxSeparator",
                        xmSeparatorGadgetClass,
                        parScaleMinMaxForm,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftWidget, parScaleMinRightArrow,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNorientation, XmVERTICAL,
                        NULL );
        UxPutContext( parMinMaxSeparator, (char *) UxParLineFormContext );


        /* Creation of parScaleMaxLeftArrow */
        parScaleMaxLeftArrow = XtVaCreateManagedWidget( "parScaleMaxLeftArrow",
                        xmArrowButtonGadgetClass,
                        parScaleMinMaxForm,
                        XmNwidth, 300,
                        XmNarrowDirection, XmARROW_LEFT,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, parMinMaxSeparator,
                        XmNshadowThickness, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( parScaleMaxLeftArrow, (char *) UxParLineFormContext );


        /* Creation of parScaleMaxRightArrow */
        parScaleMaxRightArrow = XtVaCreateManagedWidget(
"parScaleMaxRightArrow",
                        xmArrowButtonGadgetClass,
                        parScaleMinMaxForm,
                        XmNwidth, 300,
                        XmNarrowDirection, XmARROW_RIGHT,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNrightAttachment, XmATTACH_NONE,
                        XmNrightWidget, NULL,
                        XmNleftWidget, parScaleMaxLeftArrow,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNshadowThickness, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( parScaleMaxRightArrow, (char *) UxParLineFormContext );


        /* Creation of parScaleMaxText */
        parScaleMaxText = XtVaCreateManagedWidget( "parScaleMaxText",
                        xmTextFieldWidgetClass,
                        parScaleMinMaxForm,
                        XmNcolumns, res_data.parScaleMinMaxColumns,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftWidget, parScaleMaxRightArrow,
                        NULL );
        UxPutContext( parScaleMaxText, (char *) UxParLineFormContext );


        /* Creation of parLineMenu */
        parLineMenu_shell = XtVaCreatePopupShell ("parLineMenu_shell",
                        xmMenuShellWidgetClass, parLineForm,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        parLineMenu = XtVaCreateWidget( "parLineMenu",
                        xmRowColumnWidgetClass,
                        parLineMenu_shell,
                        XmNrowColumnType, XmMENU_POPUP,
                        XmNx, 10,
                        XmNy, 0,
                        XmNmenuAccelerator, "Shift <KeyUp>F10",
                        NULL );
        UxPutContext( parLineMenu, (char *) UxParLineFormContext );


        /* Creation of parLineMenu_label */
        parLineMenu_label = XtVaCreateManagedWidget( "parLineMenu_label",
                        xmLabelGadgetClass,
                        parLineMenu,
                        RES_CONVERT( XmNlabelString, "parLineMenu_label" ),
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        NULL );
        UxPutContext( parLineMenu_label, (char *) UxParLineFormContext );


        /* Creation of parLineMenu_sep */
        parLineMenu_sep = XtVaCreateManagedWidget( "parLineMenu_sep",
                        xmSeparatorGadgetClass,
                        parLineMenu,
                        NULL );
        UxPutContext( parLineMenu_sep, (char *) UxParLineFormContext );


        /* Creation of parLineMenu_store */
        parLineMenu_store = XtVaCreateManagedWidget( "parLineMenu_store",
                        xmPushButtonGadgetClass,
                        parLineMenu,
                        RES_CONVERT( XmNlabelString, "Store value" ),
                        NULL );
        UxPutContext( parLineMenu_store, (char *) UxParLineFormContext );


        /* Creation of parLineMenu_sep1 */
        parLineMenu_sep1 = XtVaCreateManagedWidget( "parLineMenu_sep1",
                        xmSeparatorGadgetClass,
                        parLineMenu,
                        NULL );
        UxPutContext( parLineMenu_sep1, (char *) UxParLineFormContext );


        /* Creation of parLineMenu_lcurr */
        parLineMenu_lcurr = XtVaCreateManagedWidget( "parLineMenu_lcurr",
                        xmLabelWidgetClass,
                        parLineMenu,
                        RES_CONVERT( XmNlabelString, "parLineMenu_lcurr" ),
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        XmNshadowThickness, 28,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNhighlightThickness, 28,
                        NULL );
        UxPutContext( parLineMenu_lcurr, (char *) UxParLineFormContext );


        /* Creation of parLineMenu_curr */
        parLineMenu_curr = XtVaCreateManagedWidget( "parLineMenu_curr",
                        xmPushButtonGadgetClass,
                        parLineMenu,
                        RES_CONVERT( XmNlabelString, "Recall current" ),
                        NULL );
        UxPutContext( parLineMenu_curr, (char *) UxParLineFormContext );


        /* Creation of parLineMenu_sep2 */
        parLineMenu_sep2 = XtVaCreateManagedWidget( "parLineMenu_sep2",
                        xmSeparatorGadgetClass,
                        parLineMenu,
                        NULL );
        UxPutContext( parLineMenu_sep2, (char *) UxParLineFormContext );


        /* Creation of parLineMenu_lstored */
        parLineMenu_lstored = XtVaCreateManagedWidget( "parLineMenu_lstored",
                        xmLabelWidgetClass,
                        parLineMenu,
                        RES_CONVERT( XmNlabelString, "parLineMenu_lsaved" ),
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        NULL );
        UxPutContext( parLineMenu_lstored, (char *) UxParLineFormContext );


        /* Creation of parLineMenu_stored */
        parLineMenu_stored = XtVaCreateManagedWidget( "parLineMenu_stored",
                        xmPushButtonGadgetClass,
                        parLineMenu,
                        RES_CONVERT( XmNlabelString, "Recall stored" ),
                        NULL );
        UxPutContext( parLineMenu_stored, (char *) UxParLineFormContext );


        /* Creation of parLineMenu_sep3 */
        parLineMenu_sep3 = XtVaCreateManagedWidget( "parLineMenu_sep3",
                        xmSeparatorGadgetClass,
                        parLineMenu,
                        NULL );
        UxPutContext( parLineMenu_sep3, (char *) UxParLineFormContext );


        /* Creation of parLineMenu_lstart */
        parLineMenu_lstart = XtVaCreateManagedWidget( "parLineMenu_lstart",
                        xmLabelWidgetClass,
                        parLineMenu,
                        RES_CONVERT( XmNlabelString, "parLineMenu_lstart" ),
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        NULL );
        UxPutContext( parLineMenu_lstart, (char *) UxParLineFormContext );


        /* Creation of parLineMenu_start */
        parLineMenu_start = XtVaCreateManagedWidget( "parLineMenu_start",
                        xmPushButtonGadgetClass,
                        parLineMenu,
                        RES_CONVERT( XmNlabelString, "Recall starting" ),
                        NULL );
        UxPutContext( parLineMenu_start, (char *) UxParLineFormContext );


        XtAddCallback( parLineForm, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxParLineFormContext);

        XtAddEventHandler(parLineForm, ButtonPressMask,
                        False, (XtEventHandler) _UxparLineFormMenuPost, (
XtPointer) parLineMenu );

        return ( parLineForm );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_parLineForm( swidget _UxUxParent, int _Uxipar )
{
        Widget                  rtrn;
        _UxCparLineForm         *UxContext;

        UxParLineFormContext = UxContext =
                (_UxCparLineForm *) UxNewContext( sizeof(_UxCparLineForm),
False );

        UxParent = _UxUxParent;
        ipar = _Uxipar;

        {
                sprintf(parNumberStr,"%2d",ipar+1);
                rtrn = _Uxbuild_parLineForm();

                return(rtrn);
        }
}
