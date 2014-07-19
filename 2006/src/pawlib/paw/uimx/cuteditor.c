/*
 * $Id: cuteditor.c,v 1.1.1.1 1996/03/01 11:39:32 mclareni Exp $
 *
 * $Log: cuteditor.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:32  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.05  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        cutEditor.c
        (Generated from interface file cutEditor.i)
        Associated Header file: cutEditor.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Label.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/CascadeBG.h>
#include <Xm/CascadeB.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>


/*******************************************************************************
        The following header file defines the context structure.
*******************************************************************************/

#include "hmotif/cuteditor.h"


/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_cutEditor();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_cutEditor()
{
        Widget  cutEditor_shell;
        Widget  menu1_p1_shell;
        Widget  menu1_p3_shell;
        Widget  menu1_p2_shell;

        cutEditor_shell = XtVaCreatePopupShell( "cutEditor_shell",
                        topLevelShellWidgetClass, UxTopLevel,
                        XmNx, 375,
                        XmNy, 160,
                        XmNwidth, 700,
                        XmNheight, 300,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "cutEditor",
                        XmNiconName, "cutEditor",
                        NULL );

        cutEditor = XtVaCreateManagedWidget( "cutEditor",
                        xmMainWindowWidgetClass, cutEditor_shell,
                        XmNheight, 300,
                        XmNwidth, 700,
                        XmNunitType, XmPIXELS,
                        NULL );

        UxPutContext( cutEditor, (char *) UxCutEditorContext );

        menu1 = XtVaCreateManagedWidget( "menu1",
                        xmRowColumnWidgetClass, cutEditor,
                        XmNmenuAccelerator, "<KeyUp>F10",
                        XmNrowColumnType, XmMENU_BAR,
                        NULL );

        UxPutContext( menu1, (char *) UxCutEditorContext );

        menu1_p1_shell = XtVaCreatePopupShell ("menu1_p1_shell",
                        xmMenuShellWidgetClass, menu1,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        menu1_p1 = XtVaCreateWidget( "menu1_p1",
                        xmRowColumnWidgetClass, menu1_p1_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );

        UxPutContext( menu1_p1, (char *) UxCutEditorContext );

        openCeButton = XtVaCreateManagedWidget( "openCeButton",
                        xmPushButtonGadgetClass, menu1_p1,
                        RES_CONVERT( XmNmnemonic, "O" ),
                        RES_CONVERT( XmNlabelString, "Open..." ),
                        NULL );

        UxPutContext( openCeButton, (char *) UxCutEditorContext );

        saveCeButton = XtVaCreateManagedWidget( "saveCeButton",
                        xmPushButtonGadgetClass, menu1_p1,
                        RES_CONVERT( XmNmnemonic, "S" ),
                        RES_CONVERT( XmNlabelString, "Save Cuts" ),
                        NULL );

        UxPutContext( saveCeButton, (char *) UxCutEditorContext );

        saveasCeButton = XtVaCreateManagedWidget( "saveasCeButton",
                        xmPushButtonGadgetClass, menu1_p1,
                        RES_CONVERT( XmNmnemonic, "A" ),
                        RES_CONVERT( XmNlabelString, "Save Cuts As..." ),
                        NULL );

        UxPutContext( saveasCeButton, (char *) UxCutEditorContext );

        closeCeButton = XtVaCreateManagedWidget( "closeCeButton",
                        xmPushButtonGadgetClass, menu1_p1,
                        RES_CONVERT( XmNmnemonic, "C" ),
                        RES_CONVERT( XmNlabelString, "Close" ),
                        NULL );

        UxPutContext( closeCeButton, (char *) UxCutEditorContext );

        menu1_top_b1 = XtVaCreateManagedWidget( "menu1_top_b1",
                        xmCascadeButtonWidgetClass, menu1,
                        XmNsubMenuId, menu1_p1,
                        RES_CONVERT( XmNmnemonic, "F" ),
                        RES_CONVERT( XmNlabelString, "File" ),
                        NULL );

        UxPutContext( menu1_top_b1, (char *) UxCutEditorContext );

        menu1_p3_shell = XtVaCreatePopupShell ("menu1_p3_shell",
                        xmMenuShellWidgetClass, menu1,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        menu1_p3 = XtVaCreateWidget( "menu1_p3",
                        xmRowColumnWidgetClass, menu1_p3_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );

        UxPutContext( menu1_p3, (char *) UxCutEditorContext );

        addBeforeCeButton = XtVaCreateManagedWidget( "addBeforeCeButton",
                        xmPushButtonGadgetClass, menu1_p3,
                        RES_CONVERT( XmNmnemonic, "B" ),
                        RES_CONVERT( XmNlabelString, "Add Cut Before" ),
                        NULL );

        UxPutContext( addBeforeCeButton, (char *) UxCutEditorContext );

        addAfterCeButton = XtVaCreateManagedWidget( "addAfterCeButton",
                        xmPushButtonGadgetClass, menu1_p3,
                        RES_CONVERT( XmNmnemonic, "A" ),
                        RES_CONVERT( XmNlabelString, "Add Cut After" ),
                        NULL );

        UxPutContext( addAfterCeButton, (char *) UxCutEditorContext );

        menu1_p3_b7 = XtVaCreateManagedWidget( "menu1_p3_b7",
                        xmSeparatorGadgetClass, menu1_p3,
                        NULL );

        UxPutContext( menu1_p3_b7, (char *) UxCutEditorContext );

       addBeforeOpenCeButton = XtVaCreateManagedWidget( "addBeforeOpenCeButton",
                        xmPushButtonGadgetClass, menu1_p3,
                        RES_CONVERT( XmNlabelString, "Add  (  Before" ),
                        NULL );

        UxPutContext( addBeforeOpenCeButton, (char *) UxCutEditorContext );

        addAfterOpenCeButton = XtVaCreateManagedWidget( "addAfterOpenCeButton",
                        xmPushButtonGadgetClass, menu1_p3,
                        RES_CONVERT( XmNlabelString, "Add  (  After" ),
                        NULL );

        UxPutContext( addAfterOpenCeButton, (char *) UxCutEditorContext );

     addBeforeCloseCeButton = XtVaCreateManagedWidget( "addBeforeCloseCeButton",
                        xmPushButtonGadgetClass, menu1_p3,
                        RES_CONVERT( XmNlabelString, "Add  )  Before" ),
                        NULL );

        UxPutContext( addBeforeCloseCeButton, (char *) UxCutEditorContext );

      addAfterCloseCeButton = XtVaCreateManagedWidget( "addAfterCloseCeButton",
                        xmPushButtonGadgetClass, menu1_p3,
                        RES_CONVERT( XmNlabelString, "Add  )  After" ),
                        NULL );

        UxPutContext( addAfterCloseCeButton, (char *) UxCutEditorContext );

        menu1_p3_b8 = XtVaCreateManagedWidget( "menu1_p3_b8",
                        xmSeparatorGadgetClass, menu1_p3,
                        NULL );

        UxPutContext( menu1_p3_b8, (char *) UxCutEditorContext );

        deleteCeButton = XtVaCreateManagedWidget( "deleteCeButton",
                        xmPushButtonGadgetClass, menu1_p3,
                        RES_CONVERT( XmNmnemonic, "D" ),
                        RES_CONVERT( XmNlabelString, "Delete Item" ),
                        NULL );

        UxPutContext( deleteCeButton, (char *) UxCutEditorContext );

        deleteAllCeButton = XtVaCreateManagedWidget( "deleteAllCeButton",
                        xmPushButtonGadgetClass, menu1_p3,
                        RES_CONVERT( XmNmnemonic, "e" ),
                        RES_CONVERT( XmNlabelString, "Delete All Items" ),
                        NULL );

        UxPutContext( deleteAllCeButton, (char *) UxCutEditorContext );

        menu1_top_b3 = XtVaCreateManagedWidget( "menu1_top_b3",
                        xmCascadeButtonGadgetClass, menu1,
                        XmNsubMenuId, menu1_p3,
                        RES_CONVERT( XmNmnemonic, "E" ),
                        RES_CONVERT( XmNlabelString, "Edit" ),
                        NULL );

        UxPutContext( menu1_top_b3, (char *) UxCutEditorContext );

        menu1_p2_shell = XtVaCreatePopupShell ("menu1_p2_shell",
                        xmMenuShellWidgetClass, menu1,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        menu1_p2 = XtVaCreateWidget( "menu1_p2",
                        xmRowColumnWidgetClass, menu1_p2_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );

        UxPutContext( menu1_p2, (char *) UxCutEditorContext );

        treeCeButton = XtVaCreateManagedWidget( "treeCeButton",
                        xmPushButtonGadgetClass, menu1_p2,
                        RES_CONVERT( XmNmnemonic, "S" ),
                        RES_CONVERT( XmNlabelString, "Show Tree..." ),
                        NULL );

        UxPutContext( treeCeButton, (char *) UxCutEditorContext );

        dynCeButton = XtVaCreateManagedWidget( "dynCeButton",
                        xmPushButtonGadgetClass, menu1_p2,
                        RES_CONVERT( XmNmnemonic, "D" ),
                        RES_CONVERT( XmNlabelString, "Dynamic Mode..." ),
                        NULL );

        UxPutContext( dynCeButton, (char *) UxCutEditorContext );

        menu1_p2_b4 = XtVaCreateManagedWidget( "menu1_p2_b4",
                        xmSeparatorGadgetClass, menu1_p2,
                        NULL );

        UxPutContext( menu1_p2_b4, (char *) UxCutEditorContext );

        indentCeButton = XtVaCreateManagedWidget( "indentCeButton",
                        xmPushButtonGadgetClass, menu1_p2,
                        RES_CONVERT( XmNmnemonic, "I" ),
                        RES_CONVERT( XmNlabelString, "Indentation..." ),
                        NULL );

        UxPutContext( indentCeButton, (char *) UxCutEditorContext );

        menu1_p2_b6 = XtVaCreateManagedWidget( "menu1_p2_b6",
                        xmSeparatorGadgetClass, menu1_p2,
                        NULL );

        UxPutContext( menu1_p2_b6, (char *) UxCutEditorContext );

        activateCeButton = XtVaCreateManagedWidget( "activateCeButton",
                        xmPushButtonGadgetClass, menu1_p2,
                        RES_CONVERT( XmNmnemonic, "A" ),
                        RES_CONVERT( XmNlabelString, "Activate All Cuts" ),
                        NULL );

        UxPutContext( activateCeButton, (char *) UxCutEditorContext );

        deactivateCeButton = XtVaCreateManagedWidget( "deactivateCeButton",
                        xmPushButtonGadgetClass, menu1_p2,
                        RES_CONVERT( XmNmnemonic, "e" ),
                        RES_CONVERT( XmNlabelString, "De-activate All Cuts" ),
                        NULL );

        UxPutContext( deactivateCeButton, (char *) UxCutEditorContext );

        menu1_top_b2 = XtVaCreateManagedWidget( "menu1_top_b2",
                        xmCascadeButtonGadgetClass, menu1,
                        XmNsubMenuId, menu1_p2,
                        RES_CONVERT( XmNmnemonic, "O" ),
                        RES_CONVERT( XmNlabelString, "Options" ),
                        NULL );

        UxPutContext( menu1_top_b2, (char *) UxCutEditorContext );

        frame1 = XtVaCreateManagedWidget( "frame1",
                        xmFrameWidgetClass, cutEditor,
                        NULL );

        UxPutContext( frame1, (char *) UxCutEditorContext );

        form1 = XtVaCreateManagedWidget( "form1",
                        xmFormWidgetClass, frame1,
                        XmNy, 10,
                        XmNx, 10,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form1, (char *) UxCutEditorContext );

        separator1 = XtVaCreateManagedWidget( "separator1",
                        xmSeparatorWidgetClass, form1,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 40,
                        XmNbottomAttachment, XmATTACH_FORM,
                        NULL );

        UxPutContext( separator1, (char *) UxCutEditorContext );

        close2CeButton = XtVaCreateManagedWidget( "close2CeButton",
                        xmPushButtonWidgetClass, form1,
                        XmNrightPosition, 95,
                        XmNleftPosition, 75,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_POSITION,
                        RES_CONVERT( XmNlabelString, " Close " ),
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        NULL );

        UxPutContext( close2CeButton, (char *) UxCutEditorContext );

        separator2 = XtVaCreateManagedWidget( "separator2",
                        xmSeparatorWidgetClass, form1,
                        XmNtopOffset, 30,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 5,
                        XmNwidth, 280,
                        XmNy, 355,
                        XmNx, 190,
                        NULL );

        UxPutContext( separator2, (char *) UxCutEditorContext );

        scrolledCeWindow = XtVaCreateManagedWidget( "scrolledCeWindow",
                        xmScrolledWindowWidgetClass, form1,
                        XmNtopWidget, separator2,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomWidget, separator1,
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNheight, 320,
                        XmNwidth, 615,
                        XmNy, 10,
                        XmNx, 5,
                        XmNscrollingPolicy, XmAUTOMATIC,
                        NULL );

        UxPutContext( scrolledCeWindow, (char *) UxCutEditorContext );

        cutsCeForm = XtVaCreateManagedWidget( "cutsCeForm",
                        xmFormWidgetClass, scrolledCeWindow,
                        XmNwidth, 630,
                        XmNresizePolicy, XmRESIZE_ANY,
                        NULL );

        UxPutContext( cutsCeForm, (char *) UxCutEditorContext );

        fullCeLabel = XtVaCreateManagedWidget( "fullCeLabel",
                        xmLabelWidgetClass, form1,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Cut Expression:" ),
                        XmNrightOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomWidget, separator2,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNheight, 20,
                        NULL );

        UxPutContext( fullCeLabel, (char *) UxCutEditorContext );

        applyCeButton = XtVaCreateManagedWidget( "applyCeButton",
                        xmPushButtonWidgetClass, form1,
                        XmNrightPosition, 25,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNleftAttachment, XmATTACH_POSITION,
                        RES_CONVERT( XmNlabelString, "Apply" ),
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        NULL );

        UxPutContext( applyCeButton, (char *) UxCutEditorContext );

        applyPlotCeButton = XtVaCreateManagedWidget( "applyPlotCeButton",
                        xmPushButtonWidgetClass, form1,
                        XmNrightPosition, 60,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 40,
                        XmNleftAttachment, XmATTACH_POSITION,
                        RES_CONVERT( XmNlabelString, "Apply & Plot" ),
                        XmNbottomOffset, 5,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 30,
                        NULL );

        UxPutContext( applyPlotCeButton, (char *) UxCutEditorContext );

        XtAddCallback( cutEditor, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxCutEditorContext );


        XmMainWindowSetAreas( cutEditor, menu1, (Widget) NULL,
                        (Widget) NULL, (Widget) NULL, frame1 );

        return ( cutEditor );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_cutEditor()
{
        Widget                  rtrn;
        _UxCcutEditor           *UxContext;

        UxCutEditorContext = UxContext =
                (_UxCcutEditor *) XtMalloc( sizeof(_UxCcutEditor) );

        rtrn = _Uxbuild_cutEditor();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_cutEditor()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_cutEditor();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

