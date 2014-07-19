/*
 * $Id: mainpanel.c,v 1.1.1.1 1996/03/01 11:39:07 mclareni Exp $
 *
 * $Log: mainpanel.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:07  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.41  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/


/*******************************************************************************
        mainpanel.c

       Associated Header file: mainpanel.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/CascadeBG.h>
#include <Xm/ToggleBG.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/SeparatoG.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/MainW.h>
#include <Xm/Frame.h>
#include <X11/Shell.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include "fpanelsh/fplocal.h"


/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "fpanelsh/mainpanel.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_mainPanelShell()
{
        Widget          _UxParent;
        Widget          optionMenu_p3_shell;
        Widget          optionMenu_p4_shell;
        Widget          updateModeMenu_p1_shell;
        Widget          fileMenu_shell;
        Widget          minimizeMenu_shell;
        Widget          examineMenu_shell;
        Widget          scanMenu_shell;
        Widget          transMenu_shell;
        Widget          viewMenu_shell;
        Widget          helpMenu_shell;


        /* Creation of mainPanelShell */
        _UxParent = UxParent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        mainPanelShell = XtVaCreatePopupShell( "mainPanelShell",
                        topLevelShellWidgetClass,
                        _UxParent,
                        XmNwidth, res_data.topLevelWidth,
                        XmNheight, res_data.topLevelHeight,
                        NULL );
        UxPutContext( mainPanelShell, (char *) UxMainPanelShellContext );


        /* Creation of mainPanelFrame */
        mainPanelFrame = XtVaCreateManagedWidget( "mainPanelFrame",
                        xmFrameWidgetClass,
                        mainPanelShell,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        NULL );
        UxPutContext( mainPanelFrame, (char *) UxMainPanelShellContext );


        /* Creation of mainWindow */
        mainWindow = XtVaCreateManagedWidget( "mainWindow",
                        xmMainWindowWidgetClass,
                        mainPanelFrame,
                        XmNunitType, XmPIXELS,
                        NULL );
        UxPutContext( mainWindow, (char *) UxMainPanelShellContext );


        /* Creation of mainForm */
        mainForm = XtVaCreateManagedWidget( "mainForm",
                        xmFormWidgetClass,
                        mainWindow,
                        XmNunitType, XmPIXELS,
                        NULL );
        UxPutContext( mainForm, (char *) UxMainPanelShellContext );


        /* Creation of errordefLabel */
        errordefLabel = XtVaCreateManagedWidget( "errordefLabel",
                        xmLabelGadgetClass,
                        mainForm,
                        RES_CONVERT( XmNlabelString, "Errordef" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 100,
                        NULL );
        UxPutContext( errordefLabel, (char *) UxMainPanelShellContext );


        /* Creation of errordefText */
        errordefText = XtVaCreateManagedWidget( "errordefText",
                        xmTextFieldWidgetClass,
                        mainForm,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, errordefLabel,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNcolumns, res_data.errordefColumns,
                        NULL );
        UxPutContext( errordefText, (char *) UxMainPanelShellContext );


        /* Creation of toleranceLabel */
        toleranceLabel = XtVaCreateManagedWidget( "toleranceLabel",
                        xmLabelGadgetClass,
                        mainForm,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Tolerance" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, res_data.settingsAreaOffset,
                        XmNleftWidget, errordefText,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        NULL );
        UxPutContext( toleranceLabel, (char *) UxMainPanelShellContext );


        /* Creation of toleranceText */
        toleranceText = XtVaCreateManagedWidget( "toleranceText",
                        xmTextFieldWidgetClass,
                        mainForm,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, toleranceLabel,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNcolumns, res_data.toleranceColumns,
                        NULL );
        UxPutContext( toleranceText, (char *) UxMainPanelShellContext );


        /* Creation of devsLabel */
        devsLabel = XtVaCreateManagedWidget( "devsLabel",
                        xmLabelGadgetClass,
                        mainForm,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Devs" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, res_data.settingsAreaOffset,
                        XmNleftWidget, toleranceText,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        NULL );
        UxPutContext( devsLabel, (char *) UxMainPanelShellContext );


        /* Creation of devsText */
        devsText = XtVaCreateManagedWidget( "devsText",
                        xmTextFieldWidgetClass,
                        mainForm,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, devsLabel,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNcolumns, res_data.devsColumns,
                        NULL );
        UxPutContext( devsText, (char *) UxMainPanelShellContext );


        /* Creation of setSubSeparator */
        setSubSeparator = XtVaCreateManagedWidget( "setSubSeparator",
                        xmSeparatorGadgetClass,
                        mainForm,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomWidget, errordefText,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( setSubSeparator, (char *) UxMainPanelShellContext );


        /* Creation of optionMenu_p3 */
        optionMenu_p3_shell = XtVaCreatePopupShell ("optionMenu_p3_shell",
                        xmMenuShellWidgetClass, mainForm,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        optionMenu_p3 = XtVaCreateWidget( "optionMenu_p3",
                        xmRowColumnWidgetClass,
                        optionMenu_p3_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( optionMenu_p3, (char *) UxMainPanelShellContext );


        /* Creation of optionMenu_p3_b2 */
        optionMenu_p3_b2 = XtVaCreateManagedWidget( "optionMenu_p3_b2",
                        xmPushButtonGadgetClass,
                        optionMenu_p3,
                        RES_CONVERT( XmNlabelString, "0" ),
                        NULL );
        XtAddCallback( optionMenu_p3_b2, XmNactivateCallback,
                (XtCallbackProc) clb_setStrategy0,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( optionMenu_p3_b2, (char *) UxMainPanelShellContext );


        /* Creation of optionMenu_p3_b3 */
        optionMenu_p3_b3 = XtVaCreateManagedWidget( "optionMenu_p3_b3",
                        xmPushButtonGadgetClass,
                        optionMenu_p3,
                        RES_CONVERT( XmNlabelString, "1" ),
                        NULL );
        XtAddCallback( optionMenu_p3_b3, XmNactivateCallback,
                (XtCallbackProc) clb_setStrategy1,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( optionMenu_p3_b3, (char *) UxMainPanelShellContext );


        /* Creation of optionMenu_p3_b4 */
        optionMenu_p3_b4 = XtVaCreateManagedWidget( "optionMenu_p3_b4",
                        xmPushButtonGadgetClass,
                        optionMenu_p3,
                        RES_CONVERT( XmNlabelString, "2" ),
                        NULL );
        XtAddCallback( optionMenu_p3_b4, XmNactivateCallback,
                (XtCallbackProc) clb_setStrategy2,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( optionMenu_p3_b4, (char *) UxMainPanelShellContext );


        /* Creation of strategyMenu */
        strategyMenu = XtVaCreateManagedWidget( "strategyMenu",
                        xmRowColumnWidgetClass,
                        mainForm,
                        XmNrowColumnType, XmMENU_OPTION,
                        XmNsubMenuId, optionMenu_p3,
                        RES_CONVERT( XmNlabelString, "Strategy" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNwhichButton, 2,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomWidget, setSubSeparator,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 100,
                        NULL );
        UxPutContext( strategyMenu, (char *) UxMainPanelShellContext );


        /* Creation of optionMenu_p4 */
        optionMenu_p4_shell = XtVaCreatePopupShell ("optionMenu_p4_shell",
                        xmMenuShellWidgetClass, mainForm,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        optionMenu_p4 = XtVaCreateWidget( "optionMenu_p4",
                        xmRowColumnWidgetClass,
                        optionMenu_p4_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( optionMenu_p4, (char *) UxMainPanelShellContext );


        /* Creation of optionMenu_p4_b2 */
        optionMenu_p4_b2 = XtVaCreateManagedWidget( "optionMenu_p4_b2",
                        xmPushButtonGadgetClass,
                        optionMenu_p4,
                        RES_CONVERT( XmNlabelString, "-1" ),
                        NULL );
        XtAddCallback( optionMenu_p4_b2, XmNactivateCallback,
                (XtCallbackProc) clb_setPrintout_1,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( optionMenu_p4_b2, (char *) UxMainPanelShellContext );


        /* Creation of optionMenu_p4_b3 */
        optionMenu_p4_b3 = XtVaCreateManagedWidget( "optionMenu_p4_b3",
                        xmPushButtonGadgetClass,
                        optionMenu_p4,
                        RES_CONVERT( XmNlabelString, "0" ),
                        NULL );
        XtAddCallback( optionMenu_p4_b3, XmNactivateCallback,
                (XtCallbackProc) clb_setPrintout0,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( optionMenu_p4_b3, (char *) UxMainPanelShellContext );


        /* Creation of optionMenu_p4_b4 */
        optionMenu_p4_b4 = XtVaCreateManagedWidget( "optionMenu_p4_b4",
                        xmPushButtonGadgetClass,
                        optionMenu_p4,
                        RES_CONVERT( XmNlabelString, "1" ),
                        NULL );
        XtAddCallback( optionMenu_p4_b4, XmNactivateCallback,
                (XtCallbackProc) clb_setPrintout1,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( optionMenu_p4_b4, (char *) UxMainPanelShellContext );


        /* Creation of optionMenu_p4_b5 */
        optionMenu_p4_b5 = XtVaCreateManagedWidget( "optionMenu_p4_b5",
                        xmPushButtonGadgetClass,
                        optionMenu_p4,
                        RES_CONVERT( XmNlabelString, "2" ),
                        NULL );
        XtAddCallback( optionMenu_p4_b5, XmNactivateCallback,
                (XtCallbackProc) clb_setPrintout2,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( optionMenu_p4_b5, (char *) UxMainPanelShellContext );


        /* Creation of optionMenu_p4_b6 */
        optionMenu_p4_b6 = XtVaCreateManagedWidget( "optionMenu_p4_b6",
                        xmPushButtonGadgetClass,
                        optionMenu_p4,
                        RES_CONVERT( XmNlabelString, "3" ),
                        NULL );
        XtAddCallback( optionMenu_p4_b6, XmNactivateCallback,
                (XtCallbackProc) clb_setPrintout3,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( optionMenu_p4_b6, (char *) UxMainPanelShellContext );


        /* Creation of printoutMenu */
        printoutMenu = XtVaCreateManagedWidget( "printoutMenu",
                        xmRowColumnWidgetClass,
                        mainForm,
                        XmNrowColumnType, XmMENU_OPTION,
                        XmNsubMenuId, optionMenu_p4,
                        RES_CONVERT( XmNlabelString, "Printout" ),
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomWidget, setSubSeparator,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 400,
                        XmNleftWidget, strategyMenu,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        NULL );
        UxPutContext( printoutMenu, (char *) UxMainPanelShellContext );


        /* Creation of updateModeMenu_p1 */
        updateModeMenu_p1_shell = XtVaCreatePopupShell (
"updateModeMenu_p1_shell",
                        xmMenuShellWidgetClass, mainForm,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        updateModeMenu_p1 = XtVaCreateWidget( "updateModeMenu_p1",
                        xmRowColumnWidgetClass,
                        updateModeMenu_p1_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( updateModeMenu_p1, (char *) UxMainPanelShellContext );


        /* Creation of updateModeMenu_p1_b1 */
        updateModeMenu_p1_b1 = XtVaCreateManagedWidget( "updateModeMenu_p1_b1",
                        xmPushButtonGadgetClass,
                        updateModeMenu_p1,
                        RES_CONVERT( XmNlabelString, "Immediate" ),
                        NULL );
        XtAddCallback( updateModeMenu_p1_b1, XmNactivateCallback,
                (XtCallbackProc) clb_updateImmediate,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( updateModeMenu_p1_b1, (char *) UxMainPanelShellContext );


        /* Creation of updateModeMenu_p1_b2 */
        updateModeMenu_p1_b2 = XtVaCreateManagedWidget( "updateModeMenu_p1_b2",
                        xmPushButtonGadgetClass,
                        updateModeMenu_p1,
                        RES_CONVERT( XmNlabelString, "Delayed" ),
                        NULL );
        XtAddCallback( updateModeMenu_p1_b2, XmNactivateCallback,
                (XtCallbackProc) clb_updateDelayed,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( updateModeMenu_p1_b2, (char *) UxMainPanelShellContext );


        /* Creation of updateModeMenu_p1_b3 */
        updateModeMenu_p1_b3 = XtVaCreateManagedWidget( "updateModeMenu_p1_b3",
                        xmPushButtonGadgetClass,
                        updateModeMenu_p1,
                        RES_CONVERT( XmNlabelString, "Disabled" ),
                        NULL );
        XtAddCallback( updateModeMenu_p1_b3, XmNactivateCallback,
                (XtCallbackProc) clb_updateDisabled,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( updateModeMenu_p1_b3, (char *) UxMainPanelShellContext );


        /* Creation of updateModeMenu */
        updateModeMenu = XtVaCreateManagedWidget( "updateModeMenu",
                        xmRowColumnWidgetClass,
                        mainForm,
                        XmNrowColumnType, XmMENU_OPTION,
                        XmNsubMenuId, updateModeMenu_p1,
                        RES_CONVERT( XmNlabelString, "Update" ),
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomWidget, setSubSeparator,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 400,
                        XmNleftWidget, printoutMenu,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        NULL );
        UxPutContext( updateModeMenu, (char *) UxMainPanelShellContext );


        /* Creation of setZoneSeparator */
        setZoneSeparator = XtVaCreateManagedWidget( "setZoneSeparator",
                        xmSeparatorGadgetClass,
                        mainForm,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomWidget, strategyMenu,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( setZoneSeparator, (char *) UxMainPanelShellContext );


        /* Creation of optnLabel */
        optnLabel = XtVaCreateManagedWidget( "optnLabel",
                        xmLabelGadgetClass,
                        mainForm,
                        RES_CONVERT( XmNlabelString, "Optn" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        NULL );
        UxPutContext( optnLabel, (char *) UxMainPanelShellContext );


        /* Creation of optnText */
        optnText = XtVaCreateManagedWidget( "optnText",
                        xmTextFieldWidgetClass,
                        mainForm,
                        XmNcolumns, res_data.optnColumns,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, optnLabel,
                        NULL );
        UxPutContext( optnText, (char *) UxMainPanelShellContext );


        /* Creation of histoSeparator */
        histoSeparator = XtVaCreateManagedWidget( "histoSeparator",
                        xmSeparatorGadgetClass,
                        mainForm,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 0,
                        XmNtopWidget, optnText,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( histoSeparator, (char *) UxMainPanelShellContext );


        /* Creation of ffuncLabel */
        ffuncLabel = XtVaCreateManagedWidget( "ffuncLabel",
                        xmLabelGadgetClass,
                        mainForm,
                        RES_CONVERT( XmNlabelString, "Function" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, histoSeparator,
                        NULL );
        UxPutContext( ffuncLabel, (char *) UxMainPanelShellContext );


        /* Creation of ffuncText */
        ffuncText = XtVaCreateManagedWidget( "ffuncText",
                        xmTextFieldWidgetClass,
                        mainForm,
                        XmNcolumns, res_data.ffuncColumns,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, ffuncLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, histoSeparator,
                        NULL );
        UxPutContext( ffuncText, (char *) UxMainPanelShellContext );


        /* Creation of nparLabel */
        nparLabel = XtVaCreateManagedWidget( "nparLabel",
                        xmLabelGadgetClass,
                        mainForm,
                        RES_CONVERT( XmNlabelString, "Npar" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, res_data.funcAreaOffset,
                        XmNleftWidget, ffuncText,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, histoSeparator,
                        NULL );
        UxPutContext( nparLabel, (char *) UxMainPanelShellContext );


        /* Creation of nparText */
        nparText = XtVaCreateManagedWidget( "nparText",
                        xmTextFieldWidgetClass,
                        mainForm,
                        XmNcolumns, 2,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, nparLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, histoSeparator,
                        NULL );
        UxPutContext( nparText, (char *) UxMainPanelShellContext );


        /* Creation of fmodelLabel */
        fmodelLabel = XtVaCreateManagedWidget( "fmodelLabel",
                        xmLabelGadgetClass,
                        mainForm,
                        RES_CONVERT( XmNlabelString, "Fit model" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, res_data.funcAreaOffset,
                        XmNleftWidget, nparText,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, histoSeparator,
                        NULL );
        UxPutContext( fmodelLabel, (char *) UxMainPanelShellContext );


        /* Creation of fmodelText */
        fmodelText = XtVaCreateManagedWidget( "fmodelText",
                        xmTextFieldWidgetClass,
                        mainForm,
                        XmNcolumns, res_data.fmodelColumns,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, fmodelLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, histoSeparator,
                        NULL );
        UxPutContext( fmodelText, (char *) UxMainPanelShellContext );


        /* Creation of ffuncSeparator */
        ffuncSeparator = XtVaCreateManagedWidget( "ffuncSeparator",
                        xmSeparatorGadgetClass,
                        mainForm,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, ffuncText,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( ffuncSeparator, (char *) UxMainPanelShellContext );


        /* Creation of fvalLabel */
        fvalLabel = XtVaCreateManagedWidget( "fvalLabel",
                        xmLabelGadgetClass,
                        mainForm,
                        RES_CONVERT( XmNlabelString, "Fval" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, ffuncSeparator,
                        NULL );
        UxPutContext( fvalLabel, (char *) UxMainPanelShellContext );


        /* Creation of fvalText */
        fvalText = XtVaCreateManagedWidget( "fvalText",
                        xmTextFieldWidgetClass,
                        mainForm,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNcolumns, res_data.fvalColumns,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, fvalLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, ffuncSeparator,
                        XmNeditable, FALSE,
                        XmNcursorPositionVisible, FALSE,
                        NULL );
        UxPutContext( fvalText, (char *) UxMainPanelShellContext );


        /* Creation of fminLabel */
        fminLabel = XtVaCreateManagedWidget( "fminLabel",
                        xmLabelGadgetClass,
                        mainForm,
                        RES_CONVERT( XmNlabelString, "Fmin" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, res_data.statAreaOffset,
                        XmNleftWidget, fvalText,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, ffuncSeparator,
                        NULL );
        UxPutContext( fminLabel, (char *) UxMainPanelShellContext );


        /* Creation of fminText */
        fminText = XtVaCreateManagedWidget( "fminText",
                        xmTextFieldWidgetClass,
                        mainForm,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNcolumns, res_data.fvalColumns,
                        XmNeditable, FALSE,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, fminLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, ffuncSeparator,
                        XmNcursorPositionVisible, FALSE,
                        NULL );
        UxPutContext( fminText, (char *) UxMainPanelShellContext );


        /* Creation of fedmLabel */
        fedmLabel = XtVaCreateManagedWidget( "fedmLabel",
                        xmLabelGadgetClass,
                        mainForm,
                        RES_CONVERT( XmNlabelString, "Fedm" ),
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, res_data.statAreaOffset,
                        XmNleftWidget, fminText,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, ffuncSeparator,
                        NULL );
        UxPutContext( fedmLabel, (char *) UxMainPanelShellContext );


        /* Creation of fedmText */
        fedmText = XtVaCreateManagedWidget( "fedmText",
                        xmTextFieldWidgetClass,
                        mainForm,
                        XmNmarginHeight, res_data.textMarginHeight,
                        XmNmarginWidth, res_data.textMarginWidth,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNcolumns, res_data.fedmColumns,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, fedmLabel,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, ffuncSeparator,
                        XmNeditable, FALSE,
                        XmNcursorPositionVisible, FALSE,
                        NULL );
        UxPutContext( fedmText, (char *) UxMainPanelShellContext );


        /* Creation of statSeparator */
        statSeparator = XtVaCreateManagedWidget( "statSeparator",
                        xmSeparatorGadgetClass,
                        mainForm,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, fvalText,
                        NULL );
        UxPutContext( statSeparator, (char *) UxMainPanelShellContext );


        /* Creation of parZoneLabel */
        parZoneLabel = XtVaCreateManagedWidget( "parZoneLabel",
                        xmLabelGadgetClass,
                        mainForm,
                        RES_CONVERT( XmNlabelString, "Parameters" ),
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 0,
                        XmNtopWidget, statSeparator,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftOffset, 100,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( parZoneLabel, (char *) UxMainPanelShellContext );


        /* Creation of parScrolledWindow */
        parScrolledWindow = XtVaCreateManagedWidget( "parScrolledWindow",
                        xmScrolledWindowWidgetClass,
                        mainForm,
                        XmNscrollingPolicy, XmAUTOMATIC,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNunitType, Xm100TH_FONT_UNITS,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomWidget, setZoneSeparator,
                        XmNvisualPolicy, XmCONSTANT,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNshadowThickness, res_data.parScrolledShadow,
                        XmNspacing, res_data.parScrolledSpacing,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, parZoneLabel,
                        NULL );
        UxPutContext( parScrolledWindow, (char *) UxMainPanelShellContext );


        /* Creation of parForm */
        parForm = XtVaCreateWidget( "parForm",
                        xmFormWidgetClass,
                        parScrolledWindow,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNallowOverlap, FALSE,
                        XmNnoResize, TRUE,
                        XmNunitType, XmPIXELS,
                        NULL );
        UxPutContext( parForm, (char *) UxMainPanelShellContext );


        /* Creation of hfMenuBar */
        hfMenuBar = XtVaCreateManagedWidget( "hfMenuBar",
                        xmRowColumnWidgetClass,
                        mainWindow,
                        XmNrowColumnType, XmMENU_BAR,
                        XmNmenuAccelerator, "<KeyUp>F10",
                        NULL );
        UxPutContext( hfMenuBar, (char *) UxMainPanelShellContext );


        /* Creation of fileMenu */
        fileMenu_shell = XtVaCreatePopupShell ("fileMenu_shell",
                        xmMenuShellWidgetClass, hfMenuBar,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        fileMenu = XtVaCreateWidget( "fileMenu",
                        xmRowColumnWidgetClass,
                        fileMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( fileMenu, (char *) UxMainPanelShellContext );


        /* Creation of fileMenu_save */
        fileMenu_save = XtVaCreateManagedWidget( "fileMenu_save",
                        xmPushButtonGadgetClass,
                        fileMenu,
                        RES_CONVERT( XmNlabelString, "Save..." ),
                        RES_CONVERT( XmNmnemonic, "S" ),
                        NULL );
        UxPutContext( fileMenu_save, (char *) UxMainPanelShellContext );


        /* Creation of fileMenu_input */
        fileMenu_input = XtVaCreateManagedWidget( "fileMenu_input",
                        xmPushButtonGadgetClass,
                        fileMenu,
                        RES_CONVERT( XmNlabelString, "Input..." ),
                        RES_CONVERT( XmNmnemonic, "R" ),
                        NULL );
        UxPutContext( fileMenu_input, (char *) UxMainPanelShellContext );


        /* Creation of fileMenu_store */
        fileMenu_store = XtVaCreateManagedWidget( "fileMenu_store",
                        xmPushButtonGadgetClass,
                        fileMenu,
                        RES_CONVERT( XmNlabelString, "Store all" ),
                        NULL );
        UxPutContext( fileMenu_store, (char *) UxMainPanelShellContext );


        /* Creation of fileMenu_recall */
        fileMenu_recall = XtVaCreateManagedWidget( "fileMenu_recall",
                        xmPushButtonGadgetClass,
                        fileMenu,
                        RES_CONVERT( XmNlabelString, "Recall stored" ),
                        NULL );
        UxPutContext( fileMenu_recall, (char *) UxMainPanelShellContext );


        /* Creation of fileMenu_start */
        fileMenu_start = XtVaCreateManagedWidget( "fileMenu_start",
                        xmPushButtonGadgetClass,
                        fileMenu,
                        RES_CONVERT( XmNlabelString, "Recall starting" ),
                        NULL );
        UxPutContext( fileMenu_start, (char *) UxMainPanelShellContext );


        /* Creation of fileMenu_exit */
        fileMenu_exit = XtVaCreateManagedWidget( "fileMenu_exit",
                        xmPushButtonGadgetClass,
                        fileMenu,
                        RES_CONVERT( XmNlabelString, "Exit" ),
                        RES_CONVERT( XmNmnemonic, "E" ),
                        NULL );
        UxPutContext( fileMenu_exit, (char *) UxMainPanelShellContext );


        /* Creation of fileMenu_top_b */
        fileMenu_top_b = XtVaCreateManagedWidget( "fileMenu_top_b",
                        xmCascadeButtonGadgetClass,
                        hfMenuBar,
                        RES_CONVERT( XmNlabelString, "File" ),
                        RES_CONVERT( XmNmnemonic, "F" ),
                        XmNsubMenuId, fileMenu,
                        NULL );
        UxPutContext( fileMenu_top_b, (char *) UxMainPanelShellContext );


        /* Creation of minimizeMenu */
        minimizeMenu_shell = XtVaCreatePopupShell ("minimizeMenu_shell",
                        xmMenuShellWidgetClass, hfMenuBar,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        minimizeMenu = XtVaCreateWidget( "minimizeMenu",
                        xmRowColumnWidgetClass,
                        minimizeMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( minimizeMenu, (char *) UxMainPanelShellContext );


        /* Creation of minimizeMenu_b1 */
        minimizeMenu_b1 = XtVaCreateManagedWidget( "minimizeMenu_b1",
                        xmPushButtonGadgetClass,
                        minimizeMenu,
                        RES_CONVERT( XmNlabelString, "Migrad" ),
                        NULL );
        XtAddCallback( minimizeMenu_b1, XmNactivateCallback,
                (XtCallbackProc) clb_minMigrad,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( minimizeMenu_b1, (char *) UxMainPanelShellContext );


        /* Creation of minimizeMenu1_b2 */
        minimizeMenu1_b2 = XtVaCreateManagedWidget( "minimizeMenu1_b2",
                        xmPushButtonGadgetClass,
                        minimizeMenu,
                        RES_CONVERT( XmNlabelString, "Minimize" ),
                        NULL );
        XtAddCallback( minimizeMenu1_b2, XmNactivateCallback,
                (XtCallbackProc) clb_minMinimize,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( minimizeMenu1_b2, (char *) UxMainPanelShellContext );


        /* Creation of minimizeMenu1_b3 */
        minimizeMenu1_b3 = XtVaCreateManagedWidget( "minimizeMenu1_b3",
                        xmPushButtonGadgetClass,
                        minimizeMenu,
                        RES_CONVERT( XmNlabelString, "Seek" ),
                        NULL );
        XtAddCallback( minimizeMenu1_b3, XmNactivateCallback,
                (XtCallbackProc) clb_minSeek,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( minimizeMenu1_b3, (char *) UxMainPanelShellContext );


        /* Creation of MinimizeMenu1_b4 */
        MinimizeMenu1_b4 = XtVaCreateManagedWidget( "MinimizeMenu1_b4",
                        xmPushButtonGadgetClass,
                        minimizeMenu,
                        RES_CONVERT( XmNlabelString, "Simplex" ),
                        NULL );
        XtAddCallback( MinimizeMenu1_b4, XmNactivateCallback,
                (XtCallbackProc) clb_minSimplex,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( MinimizeMenu1_b4, (char *) UxMainPanelShellContext );


        /* Creation of minimizeMenu1_b5 */
        minimizeMenu1_b5 = XtVaCreateManagedWidget( "minimizeMenu1_b5",
                        xmPushButtonGadgetClass,
                        minimizeMenu,
                        RES_CONVERT( XmNlabelString, "Improve" ),
                        NULL );
        XtAddCallback( minimizeMenu1_b5, XmNactivateCallback,
                (XtCallbackProc) clb_minImprove,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( minimizeMenu1_b5, (char *) UxMainPanelShellContext );


        /* Creation of minimizeMenu_top_b */
        minimizeMenu_top_b = XtVaCreateManagedWidget( "minimizeMenu_top_b",
                        xmCascadeButtonGadgetClass,
                        hfMenuBar,
                        RES_CONVERT( XmNlabelString, "Minimize" ),
                        RES_CONVERT( XmNmnemonic, "M" ),
                        XmNsubMenuId, minimizeMenu,
                        NULL );
        UxPutContext( minimizeMenu_top_b, (char *) UxMainPanelShellContext );


        /* Creation of examineMenu */
        examineMenu_shell = XtVaCreatePopupShell ("examineMenu_shell",
                        xmMenuShellWidgetClass, hfMenuBar,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        examineMenu = XtVaCreateWidget( "examineMenu",
                        xmRowColumnWidgetClass,
                        examineMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( examineMenu, (char *) UxMainPanelShellContext );


        /* Creation of examineMenu_b1 */
        examineMenu_b1 = XtVaCreateManagedWidget( "examineMenu_b1",
                        xmPushButtonGadgetClass,
                        examineMenu,
                        RES_CONVERT( XmNlabelString, "Contour..." ),
                        NULL );
        XtAddCallback( examineMenu_b1, XmNactivateCallback,
                (XtCallbackProc) clb_exaContour,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( examineMenu_b1, (char *) UxMainPanelShellContext );


        /* Creation of examineMenu_b2 */
        examineMenu_b2 = XtVaCreateManagedWidget( "examineMenu_b2",
                        xmPushButtonGadgetClass,
                        examineMenu,
                        RES_CONVERT( XmNlabelString, "Hesse" ),
                        NULL );
        XtAddCallback( examineMenu_b2, XmNactivateCallback,
                (XtCallbackProc) clb_exaHesse,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( examineMenu_b2, (char *) UxMainPanelShellContext );


        /* Creation of examineMenu_b3 */
        examineMenu_b3 = XtVaCreateManagedWidget( "examineMenu_b3",
                        xmPushButtonGadgetClass,
                        examineMenu,
                        RES_CONVERT( XmNlabelString, "Minos" ),
                        NULL );
        XtAddCallback( examineMenu_b3, XmNactivateCallback,
                (XtCallbackProc) clb_exaMinos,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( examineMenu_b3, (char *) UxMainPanelShellContext );


        /* Creation of examineMenu_b4 */
        examineMenu_b4 = XtVaCreateManagedWidget( "examineMenu_b4",
                        xmPushButtonGadgetClass,
                        examineMenu,
                        RES_CONVERT( XmNlabelString, "Correlations" ),
                        NULL );
        XtAddCallback( examineMenu_b4, XmNactivateCallback,
                (XtCallbackProc) clb_exaCorrelation,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( examineMenu_b4, (char *) UxMainPanelShellContext );


        /* Creation of examineMenu_b5 */
        examineMenu_b5 = XtVaCreateManagedWidget( "examineMenu_b5",
                        xmPushButtonGadgetClass,
                        examineMenu,
                        RES_CONVERT( XmNlabelString, "Covariance" ),
                        NULL );
        XtAddCallback( examineMenu_b5, XmNactivateCallback,
                (XtCallbackProc) clb_exaCovariance,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( examineMenu_b5, (char *) UxMainPanelShellContext );


        /* Creation of examineMenu_b6 */
        examineMenu_b6 = XtVaCreateManagedWidget( "examineMenu_b6",
                        xmPushButtonGadgetClass,
                        examineMenu,
                        RES_CONVERT( XmNlabelString, "Eigenvalues" ),
                        NULL );
        XtAddCallback( examineMenu_b6, XmNactivateCallback,
                (XtCallbackProc) clb_exaEigenvalues,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( examineMenu_b6, (char *) UxMainPanelShellContext );


        /* Creation of examineMenu_top_b */
        examineMenu_top_b = XtVaCreateManagedWidget( "examineMenu_top_b",
                        xmCascadeButtonGadgetClass,
                        hfMenuBar,
                        RES_CONVERT( XmNlabelString, "Examine" ),
                        RES_CONVERT( XmNmnemonic, "E" ),
                        XmNsubMenuId, examineMenu,
                        NULL );
        UxPutContext( examineMenu_top_b, (char *) UxMainPanelShellContext );


        /* Creation of scanMenu */
        scanMenu_shell = XtVaCreatePopupShell ("scanMenu_shell",
                        xmMenuShellWidgetClass, hfMenuBar,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        scanMenu = XtVaCreateWidget( "scanMenu",
                        xmRowColumnWidgetClass,
                        scanMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( scanMenu, (char *) UxMainPanelShellContext );


        /* Creation of scanMenu1_b1 */
        scanMenu1_b1 = XtVaCreateManagedWidget( "scanMenu1_b1",
                        xmPushButtonGadgetClass,
                        scanMenu,
                        RES_CONVERT( XmNlabelString, "1-D scan..." ),
                        NULL );
        XtAddCallback( scanMenu1_b1, XmNactivateCallback,
                (XtCallbackProc) clb_scanOnedim,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( scanMenu1_b1, (char *) UxMainPanelShellContext );


        /* Creation of scanMenu1_b2 */
        scanMenu1_b2 = XtVaCreateManagedWidget( "scanMenu1_b2",
                        xmPushButtonGadgetClass,
                        scanMenu,
                        RES_CONVERT( XmNlabelString, "2-D scan..." ),
                        NULL );
        UxPutContext( scanMenu1_b2, (char *) UxMainPanelShellContext );


        /* Creation of scanMenu_top_b */
        scanMenu_top_b = XtVaCreateManagedWidget( "scanMenu_top_b",
                        xmCascadeButtonGadgetClass,
                        hfMenuBar,
                        RES_CONVERT( XmNlabelString, "Scan" ),
                        RES_CONVERT( XmNmnemonic, "S" ),
                        XmNsubMenuId, scanMenu,
                        NULL );
        UxPutContext( scanMenu_top_b, (char *) UxMainPanelShellContext );


        /* Creation of transMenu */
        transMenu_shell = XtVaCreatePopupShell ("transMenu_shell",
                        xmMenuShellWidgetClass, hfMenuBar,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        transMenu = XtVaCreateWidget( "transMenu",
                        xmRowColumnWidgetClass,
                        transMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( transMenu, (char *) UxMainPanelShellContext );


        /* Creation of transMenu_vect */
        transMenu_vect = XtVaCreateManagedWidget( "transMenu_vect",
                        xmPushButtonGadgetClass,
                        transMenu,
                        RES_CONVERT( XmNlabelString, "PAW vectors..." ),
                        NULL );
        UxPutContext( transMenu_vect, (char *) UxMainPanelShellContext );


        /* Creation of transMenu_top_b */
        transMenu_top_b = XtVaCreateManagedWidget( "transMenu_top_b",
                        xmCascadeButtonGadgetClass,
                        hfMenuBar,
                        RES_CONVERT( XmNlabelString, "Transfer" ),
                        XmNsubMenuId, transMenu,
                        RES_CONVERT( XmNmnemonic, "T" ),
                        NULL );
        UxPutContext( transMenu_top_b, (char *) UxMainPanelShellContext );


        /* Creation of viewMenu */
        viewMenu_shell = XtVaCreatePopupShell ("viewMenu_shell",
                        xmMenuShellWidgetClass, hfMenuBar,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        viewMenu = XtVaCreateWidget( "viewMenu",
                        xmRowColumnWidgetClass,
                        viewMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( viewMenu, (char *) UxMainPanelShellContext );


        /* Creation of viewMenuLimits */
        viewMenuLimits = XtVaCreateManagedWidget( "viewMenuLimits",
                        xmToggleButtonGadgetClass,
                        viewMenu,
                        RES_CONVERT( XmNlabelString, "Limits" ),
                        NULL );
        XtAddCallback( viewMenuLimits, XmNvalueChangedCallback,
                (XtCallbackProc) callbackViewLimits,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( viewMenuLimits, (char *) UxMainPanelShellContext );


        /* Creation of viewMenuSteps */
        viewMenuSteps = XtVaCreateManagedWidget( "viewMenuSteps",
                        xmToggleButtonGadgetClass,
                        viewMenu,
                        RES_CONVERT( XmNlabelString, "Steps" ),
                        NULL );
        XtAddCallback( viewMenuSteps, XmNvalueChangedCallback,
                (XtCallbackProc) callbackViewSteps,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( viewMenuSteps, (char *) UxMainPanelShellContext );


        /* Creation of viewMenuEparab */
        viewMenuEparab = XtVaCreateManagedWidget( "viewMenuEparab",
                        xmToggleButtonGadgetClass,
                        viewMenu,
                        RES_CONVERT( XmNlabelString, "Eparab" ),
                        NULL );
        XtAddCallback( viewMenuEparab, XmNvalueChangedCallback,
                (XtCallbackProc) callbackViewEparab,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( viewMenuEparab, (char *) UxMainPanelShellContext );


        /* Creation of viewMenuEminos */
        viewMenuEminos = XtVaCreateManagedWidget( "viewMenuEminos",
                        xmToggleButtonGadgetClass,
                        viewMenu,
                        RES_CONVERT( XmNlabelString, "Eminos" ),
                        NULL );
        XtAddCallback( viewMenuEminos, XmNvalueChangedCallback,
                (XtCallbackProc) callbackViewEminos,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( viewMenuEminos, (char *) UxMainPanelShellContext );


        /* Creation of viewMenuGlobcc */
        viewMenuGlobcc = XtVaCreateManagedWidget( "viewMenuGlobcc",
                        xmToggleButtonGadgetClass,
                        viewMenu,
                        RES_CONVERT( XmNlabelString, "Globcc" ),
                        NULL );
        XtAddCallback( viewMenuGlobcc, XmNvalueChangedCallback,
                (XtCallbackProc) callbackViewGlobcc,
                (XtPointer) UxMainPanelShellContext );

        UxPutContext( viewMenuGlobcc, (char *) UxMainPanelShellContext );


        /* Creation of viewMenu_top_b */
        viewMenu_top_b = XtVaCreateManagedWidget( "viewMenu_top_b",
                        xmCascadeButtonGadgetClass,
                        hfMenuBar,
                        RES_CONVERT( XmNlabelString, "View" ),
                        RES_CONVERT( XmNmnemonic, "V" ),
                        XmNsubMenuId, viewMenu,
                        NULL );
        UxPutContext( viewMenu_top_b, (char *) UxMainPanelShellContext );


        /* Creation of helpMenu */
        helpMenu_shell = XtVaCreatePopupShell ("helpMenu_shell",
                        xmMenuShellWidgetClass, hfMenuBar,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        helpMenu = XtVaCreateWidget( "helpMenu",
                        xmRowColumnWidgetClass,
                        helpMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( helpMenu, (char *) UxMainPanelShellContext );


        /* Creation of helpMenu_panel */
        helpMenu_panel = XtVaCreateManagedWidget( "helpMenu_panel",
                        xmPushButtonGadgetClass,
                        helpMenu,
                        RES_CONVERT( XmNlabelString, "Histogram Fit Panel" ),
                        NULL );
        UxPutContext( helpMenu_panel, (char *) UxMainPanelShellContext );


        /* Creation of helpMenu_minuit */
        helpMenu_minuit = XtVaCreateManagedWidget( "helpMenu_minuit",
                        xmPushButtonGadgetClass,
                        helpMenu,
                        RES_CONVERT( XmNlabelString, "Minuit" ),
                        NULL );
        UxPutContext( helpMenu_minuit, (char *) UxMainPanelShellContext );


        /* Creation of helpMenu_context */
        helpMenu_context = XtVaCreateManagedWidget( "helpMenu_context",
                        xmPushButtonGadgetClass,
                        helpMenu,
                        RES_CONVERT( XmNlabelString, "On context" ),
                        NULL );
        UxPutContext( helpMenu_context, (char *) UxMainPanelShellContext );


        /* Creation of helpMenu_top_b */
        helpMenu_top_b = XtVaCreateManagedWidget( "helpMenu_top_b",
                        xmCascadeButtonGadgetClass,
                        hfMenuBar,
                        RES_CONVERT( XmNlabelString, "Help" ),
                        RES_CONVERT( XmNmnemonic, "H" ),
                        XmNsubMenuId, helpMenu,
                        NULL );
        UxPutContext( helpMenu_top_b, (char *) UxMainPanelShellContext );

        XtVaSetValues(hfMenuBar,
                        XmNmenuHelpWidget, helpMenu_top_b,
                        NULL );


        XtAddCallback( mainPanelShell, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxMainPanelShellContext);

        XmMainWindowSetAreas( mainWindow, hfMenuBar, (Widget) NULL,
                        (Widget) NULL, (Widget) NULL, mainForm );

        return ( mainPanelShell );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_mainPanelShell( swidget _UxUxParent )
{
        Widget                  rtrn;
        _UxCmainPanelShell      *UxContext;

        UxMainPanelShellContext = UxContext =
                (_UxCmainPanelShell *) UxNewContext( sizeof(_UxCmainPanelShell)
, False );

        UxParent = _UxUxParent;

        rtrn = _Uxbuild_mainPanelShell();


        return(rtrn);
}
