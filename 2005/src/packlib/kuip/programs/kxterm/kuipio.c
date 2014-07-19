/*
 * $Id: kuipio.c,v 1.1.1.1 1996/03/08 15:33:04 mclareni Exp $
 *
 * $Log: kuipio.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:04  mclareni
 * Kuip
 *
 */
/*CMZ :  2.04/04 13/12/93  10.26.28  by  Alfred Nathaniel*/
/*-- Author :*/

/*******************************************************************************
       kuipIo.c
       (Generated from interface file kuipIo.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "uxxt.h"

#include <Xm/CascadeB.h>
#include <Xm/Separator.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/MainW.h>



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
        Widget  UxkuipIo;
        Widget  Uxframe1;
        Widget  Uxform1;
        Widget  UxoutputForm;
        Widget  Uxform3;
        Widget  Uxlabel2;
        Widget  UxscrolledWindow2;
        Widget  UxinputForm;
        Widget  Uxform2;
        Widget  Uxlabel1;
        Widget  UxscrolledWindow1;
        Widget  UxFileMenu;
        Widget  UxFileMenu_b11;
        Widget  Uxmenu1_p1_b9;
        Widget  Uxmenu1_p1_b8;
        Widget  UxFileMenu_b8;
        Widget  Uxmenu1_top_b1;
        Widget  UxEditMenu;
        Widget  Uxmenu1_p2_b5;
        Widget  Uxmenu1_top_b2;
        Widget  UxViewMenu;
        Widget  Uxmenu1_top_b3;
        Widget  UxOptionMenu;
        Widget  Uxmenu1_top_b4;
        Widget  UxHelpMenu;
        Widget  Uxmenu1_top_b5;
} _UxCkuipIo;

static _UxCkuipIo              *UxKuipIoContext;
#define kuipIo                  UxKuipIoContext->UxkuipIo
#define frame1                  UxKuipIoContext->Uxframe1
#define form1                   UxKuipIoContext->Uxform1
#define outputForm              UxKuipIoContext->UxoutputForm
#define form3                   UxKuipIoContext->Uxform3
#define label2                  UxKuipIoContext->Uxlabel2
#define scrolledWindow2         UxKuipIoContext->UxscrolledWindow2
#define inputForm               UxKuipIoContext->UxinputForm
#define form2                   UxKuipIoContext->Uxform2
#define label1                  UxKuipIoContext->Uxlabel1
#define scrolledWindow1         UxKuipIoContext->UxscrolledWindow1
#define FileMenu                UxKuipIoContext->UxFileMenu
#define FileMenu_b11            UxKuipIoContext->UxFileMenu_b11
#define menu1_p1_b9             UxKuipIoContext->Uxmenu1_p1_b9
#define menu1_p1_b8             UxKuipIoContext->Uxmenu1_p1_b8
#define FileMenu_b8             UxKuipIoContext->UxFileMenu_b8
#define menu1_top_b1            UxKuipIoContext->Uxmenu1_top_b1
#define EditMenu                UxKuipIoContext->UxEditMenu
#define menu1_p2_b5             UxKuipIoContext->Uxmenu1_p2_b5
#define menu1_top_b2            UxKuipIoContext->Uxmenu1_top_b2
#define ViewMenu                UxKuipIoContext->UxViewMenu
#define menu1_top_b3            UxKuipIoContext->Uxmenu1_top_b3
#define OptionMenu              UxKuipIoContext->UxOptionMenu
#define menu1_top_b4            UxKuipIoContext->Uxmenu1_top_b4
#define HelpMenu                UxKuipIoContext->UxHelpMenu
#define menu1_top_b5            UxKuipIoContext->Uxmenu1_top_b5


Widget  panedWindow;
Widget  outputHoldButton;
Widget  lcdLabel;
Widget  outputText;
Widget  inputHoldButton;
Widget  cwdLabel;
Widget  inputText;
Widget  menuBar;
Widget  aboutButton;
Widget  aboutAppButton;
Widget  saveTransButton;
Widget  saveTransAsButton;
Widget  printButton;
Widget  killButton;
Widget  exitButton;
Widget  cutButton;
Widget  copyButton;
Widget  pasteButton;
Widget  searchButton;
Widget  showInputButton;
Widget  clearTrButton;
Widget  helpKxtermButton;
Widget  helpResourceButton;
Widget  helpEditButton;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_kuipIo();

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_kuipIo()
{
        Widget          _UxParent;
        Widget          FileMenu_shell;
        Widget          EditMenu_shell;
        Widget          ViewMenu_shell;
        Widget          OptionMenu_shell;
        Widget          HelpMenu_shell;


        /* Creation of kuipIo */
        _UxParent = XtVaCreatePopupShell( "kuipIo_shell",
                        topLevelShellWidgetClass, UxTopLevel,
                        XmNx, 0,
                        XmNy, 0,
                        XmNwidth, 700,
                        XmNheight, 495,
                        XmNshellUnitType, XmPIXELS,
                        NULL );

        kuipIo = XtVaCreateManagedWidget( "kuipIo",
                        xmMainWindowWidgetClass,
                        _UxParent,
                        XmNunitType, XmPIXELS,
                        XmNwidth, 700,
                        XmNheight, 495,
                        XmNshowSeparator, FALSE,
                        NULL );
        UxPutContext( kuipIo, (char *) UxKuipIoContext );


        /* Creation of frame1 */
        frame1 = XtVaCreateManagedWidget( "frame1",
                        xmFrameWidgetClass,
                        kuipIo,
                        NULL );
        UxPutContext( frame1, (char *) UxKuipIoContext );


        /* Creation of form1 */
        form1 = XtVaCreateManagedWidget( "form1",
                        xmFormWidgetClass,
                        frame1,
                        NULL );
        UxPutContext( form1, (char *) UxKuipIoContext );


        /* Creation of panedWindow */
        panedWindow = XtVaCreateManagedWidget( "panedWindow",
                        xmPanedWindowWidgetClass,
                        form1,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( panedWindow, (char *) UxKuipIoContext );


        /* Creation of outputForm */
        outputForm = XtVaCreateManagedWidget( "outputForm",
                        xmFormWidgetClass,
                        panedWindow,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );
        UxPutContext( outputForm, (char *) UxKuipIoContext );


        /* Creation of form3 */
        form3 = XtVaCreateManagedWidget( "form3",
                        xmFormWidgetClass,
                        outputForm,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( form3, (char *) UxKuipIoContext );


        /* Creation of label2 */
        label2 = XtVaCreateManagedWidget( "label2",
                        xmLabelWidgetClass,
                        form3,
                        XmNheight, 20,
                        RES_CONVERT( XmNlabelString, "Transcript Pad" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( label2, (char *) UxKuipIoContext );


        /* Creation of outputHoldButton */
        outputHoldButton = XtVaCreateManagedWidget( "outputHoldButton",
                        xmPushButtonWidgetClass,
                        form3,
                        XmNheight, 25,
                        RES_CONVERT( XmNlabelString, "H" ),
                        XmNwidth, 25,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 20,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( outputHoldButton, (char *) UxKuipIoContext );


        /* Creation of lcdLabel */
        lcdLabel = XtVaCreateManagedWidget( "lcdLabel",
                        xmLabelWidgetClass,
                        form3,
                        XmNheight, 20,
                        XmNalignment, XmALIGNMENT_END,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, label2,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 10,
                        XmNrightWidget, outputHoldButton,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( lcdLabel, (char *) UxKuipIoContext );


        /* Creation of scrolledWindow2 */
        scrolledWindow2 = XtVaCreateManagedWidget( "scrolledWindow2",
                        xmScrolledWindowWidgetClass,
                        outputForm,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNshadowThickness, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, form3,
                        NULL );
        UxPutContext( scrolledWindow2, (char *) UxKuipIoContext );


        /* Creation of outputText */
        outputText = XtVaCreateManagedWidget( "outputText",
                        xmTextWidgetClass,
                        scrolledWindow2,
                        XmNeditable, FALSE,
                        XmNeditMode, XmMULTI_LINE_EDIT ,
                        XmNblinkRate, 0,
                        NULL );
        UxPutContext( outputText, (char *) UxKuipIoContext );


        /* Creation of inputForm */
        inputForm = XtVaCreateManagedWidget( "inputForm",
                        xmFormWidgetClass,
                        panedWindow,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );
        UxPutContext( inputForm, (char *) UxKuipIoContext );


        /* Creation of form2 */
        form2 = XtVaCreateManagedWidget( "form2",
                        xmFormWidgetClass,
                        inputForm,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( form2, (char *) UxKuipIoContext );


        /* Creation of label1 */
        label1 = XtVaCreateManagedWidget( "label1",
                        xmLabelWidgetClass,
                        form2,
                        XmNheight, 20,
                        RES_CONVERT( XmNlabelString, "Input Pad" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( label1, (char *) UxKuipIoContext );


        /* Creation of inputHoldButton */
        inputHoldButton = XtVaCreateManagedWidget( "inputHoldButton",
                        xmPushButtonWidgetClass,
                        form2,
                        RES_CONVERT( XmNlabelString, "H" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 20,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNheight, 25,
                        XmNwidth, 25,
                        NULL );
        UxPutContext( inputHoldButton, (char *) UxKuipIoContext );


        /* Creation of cwdLabel */
        cwdLabel = XtVaCreateManagedWidget( "cwdLabel",
                        xmLabelWidgetClass,
                        form2,
                        XmNheight, 20,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 30,
                        XmNleftWidget, label1,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 10,
                        XmNrightWidget, inputHoldButton,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNalignment, XmALIGNMENT_END,
                        RES_CONVERT( XmNlabelString, "" ),
                        NULL );
        UxPutContext( cwdLabel, (char *) UxKuipIoContext );


        /* Creation of scrolledWindow1 */
        scrolledWindow1 = XtVaCreateManagedWidget( "scrolledWindow1",
                        xmScrolledWindowWidgetClass,
                        inputForm,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNshadowThickness, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, form2,
                        NULL );
        UxPutContext( scrolledWindow1, (char *) UxKuipIoContext );


        /* Creation of inputText */
        inputText = XtVaCreateManagedWidget( "inputText",
                        xmTextWidgetClass,
                        scrolledWindow1,
                        XmNeditMode, XmMULTI_LINE_EDIT ,
                        XmNverifyBell, FALSE,
                        XmNblinkRate, 0,
                        NULL );
        UxPutContext( inputText, (char *) UxKuipIoContext );


        /* Creation of menuBar */
        menuBar = XtVaCreateManagedWidget( "menuBar",
                        xmRowColumnWidgetClass,
                        kuipIo,
                        XmNrowColumnType, XmMENU_BAR,
                        XmNmenuAccelerator, "<KeyUp>F10",
                        NULL );
        UxPutContext( menuBar, (char *) UxKuipIoContext );


        /* Creation of FileMenu */
        FileMenu_shell = XtVaCreatePopupShell ("FileMenu_shell",
                        xmMenuShellWidgetClass, menuBar,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        FileMenu = XtVaCreateWidget( "FileMenu",
                        xmRowColumnWidgetClass,
                        FileMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( FileMenu, (char *) UxKuipIoContext );


        /* Creation of aboutButton */
        aboutButton = XtVaCreateManagedWidget( "aboutButton",
                        xmPushButtonWidgetClass,
                        FileMenu,
                        RES_CONVERT( XmNlabelString, "About Kxterm..." ),
                        NULL );
        UxPutContext( aboutButton, (char *) UxKuipIoContext );


        /* Creation of aboutAppButton */
        aboutAppButton = XtVaCreateManagedWidget( "aboutAppButton",
                        xmPushButtonWidgetClass,
                        FileMenu,
                        RES_CONVERT( XmNlabelString, "About Application..." ),
                        NULL );
        UxPutContext( aboutAppButton, (char *) UxKuipIoContext );


        /* Creation of FileMenu_b11 */
        FileMenu_b11 = XtVaCreateManagedWidget( "FileMenu_b11",
                        xmSeparatorWidgetClass,
                        FileMenu,
                        NULL );
        UxPutContext( FileMenu_b11, (char *) UxKuipIoContext );


        /* Creation of saveTransButton */
        saveTransButton = XtVaCreateManagedWidget( "saveTransButton",
                        xmPushButtonWidgetClass,
                        FileMenu,
                        RES_CONVERT( XmNlabelString, "Save Transcript" ),
                        RES_CONVERT( XmNmnemonic, "S" ),
                        NULL );
        UxPutContext( saveTransButton, (char *) UxKuipIoContext );


        /* Creation of saveTransAsButton */
        saveTransAsButton = XtVaCreateManagedWidget( "saveTransAsButton",
                        xmPushButtonWidgetClass,
                        FileMenu,
                        RES_CONVERT( XmNlabelString, "Save Transcript As..." ),
                        RES_CONVERT( XmNmnemonic, "A" ),
                        NULL );
        UxPutContext( saveTransAsButton, (char *) UxKuipIoContext );


        /* Creation of menu1_p1_b9 */
        menu1_p1_b9 = XtVaCreateManagedWidget( "menu1_p1_b9",
                        xmSeparatorWidgetClass,
                        FileMenu,
                        NULL );
        UxPutContext( menu1_p1_b9, (char *) UxKuipIoContext );


        /* Creation of printButton */
        printButton = XtVaCreateManagedWidget( "printButton",
                        xmPushButtonWidgetClass,
                        FileMenu,
                        RES_CONVERT( XmNlabelString, "Print..." ),
                        RES_CONVERT( XmNmnemonic, "P" ),
                        NULL );
        UxPutContext( printButton, (char *) UxKuipIoContext );


        /* Creation of menu1_p1_b8 */
        menu1_p1_b8 = XtVaCreateManagedWidget( "menu1_p1_b8",
                        xmSeparatorWidgetClass,
                        FileMenu,
                        NULL );
        UxPutContext( menu1_p1_b8, (char *) UxKuipIoContext );


        /* Creation of killButton */
        killButton = XtVaCreateManagedWidget( "killButton",
                        xmPushButtonWidgetClass,
                        FileMenu,
                        RES_CONVERT( XmNlabelString, "Kill" ),
                        RES_CONVERT( XmNmnemonic, "K" ),
                        NULL );
        UxPutContext( killButton, (char *) UxKuipIoContext );


        /* Creation of FileMenu_b8 */
        FileMenu_b8 = XtVaCreateManagedWidget( "FileMenu_b8",
                        xmSeparatorWidgetClass,
                        FileMenu,
                        NULL );
        UxPutContext( FileMenu_b8, (char *) UxKuipIoContext );


        /* Creation of exitButton */
        exitButton = XtVaCreateManagedWidget( "exitButton",
                        xmPushButtonWidgetClass,
                        FileMenu,
                        RES_CONVERT( XmNlabelString, "Exit" ),
                        RES_CONVERT( XmNmnemonic, "E" ),
                        NULL );
        UxPutContext( exitButton, (char *) UxKuipIoContext );


        /* Creation of menu1_top_b1 */
        menu1_top_b1 = XtVaCreateManagedWidget( "menu1_top_b1",
                        xmCascadeButtonWidgetClass,
                        menuBar,
                        RES_CONVERT( XmNlabelString, "File" ),
                        RES_CONVERT( XmNmnemonic, "F" ),
                        XmNsubMenuId, FileMenu,
                        NULL );
        UxPutContext( menu1_top_b1, (char *) UxKuipIoContext );


        /* Creation of EditMenu */
        EditMenu_shell = XtVaCreatePopupShell ("EditMenu_shell",
                        xmMenuShellWidgetClass, menuBar,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        EditMenu = XtVaCreateWidget( "EditMenu",
                        xmRowColumnWidgetClass,
                        EditMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( EditMenu, (char *) UxKuipIoContext );


        /* Creation of cutButton */
        cutButton = XtVaCreateManagedWidget( "cutButton",
                        xmPushButtonWidgetClass,
                        EditMenu,
                        RES_CONVERT( XmNlabelString, "Cut" ),
                        RES_CONVERT( XmNmnemonic, "t" ),
                        XmNaccelerator, "Shift<Key>Delete",
                        RES_CONVERT( XmNacceleratorText, "Shift+Del" ),
                        NULL );
        UxPutContext( cutButton, (char *) UxKuipIoContext );


        /* Creation of copyButton */
        copyButton = XtVaCreateManagedWidget( "copyButton",
                        xmPushButtonWidgetClass,
                        EditMenu,
                        RES_CONVERT( XmNlabelString, "Copy" ),
                        RES_CONVERT( XmNmnemonic, "C" ),
                        XmNaccelerator, "Ctrl<Key>InsertChar",
                        RES_CONVERT( XmNacceleratorText, "Ctrl+Ins" ),
                        NULL );
        UxPutContext( copyButton, (char *) UxKuipIoContext );


        /* Creation of pasteButton */
        pasteButton = XtVaCreateManagedWidget( "pasteButton",
                        xmPushButtonWidgetClass,
                        EditMenu,
                        RES_CONVERT( XmNlabelString, "Paste" ),
                        RES_CONVERT( XmNmnemonic, "P" ),
                        XmNaccelerator, "Shift<Key>InsertChar",
                        RES_CONVERT( XmNacceleratorText, "Shift+Ins" ),
                        NULL );
        UxPutContext( pasteButton, (char *) UxKuipIoContext );


        /* Creation of menu1_p2_b5 */
        menu1_p2_b5 = XtVaCreateManagedWidget( "menu1_p2_b5",
                        xmSeparatorWidgetClass,
                        EditMenu,
                        NULL );
        UxPutContext( menu1_p2_b5, (char *) UxKuipIoContext );


        /* Creation of searchButton */
        searchButton = XtVaCreateManagedWidget( "searchButton",
                        xmPushButtonWidgetClass,
                        EditMenu,
                        RES_CONVERT( XmNlabelString, "Search..." ),
                        RES_CONVERT( XmNmnemonic, "S" ),
                        XmNaccelerator, "Ctrl<Key>s",
                        RES_CONVERT( XmNacceleratorText, "Ctrl+s" ),
                        NULL );
        UxPutContext( searchButton, (char *) UxKuipIoContext );


        /* Creation of menu1_top_b2 */
        menu1_top_b2 = XtVaCreateManagedWidget( "menu1_top_b2",
                        xmCascadeButtonWidgetClass,
                        menuBar,
                        RES_CONVERT( XmNlabelString, "Edit" ),
                        XmNsubMenuId, EditMenu,
                        RES_CONVERT( XmNmnemonic, "E" ),
                        NULL );
        UxPutContext( menu1_top_b2, (char *) UxKuipIoContext );


        /* Creation of ViewMenu */
        ViewMenu_shell = XtVaCreatePopupShell ("ViewMenu_shell",
                        xmMenuShellWidgetClass, menuBar,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        ViewMenu = XtVaCreateWidget( "ViewMenu",
                        xmRowColumnWidgetClass,
                        ViewMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( ViewMenu, (char *) UxKuipIoContext );


        /* Creation of showInputButton */
        showInputButton = XtVaCreateManagedWidget( "showInputButton",
                        xmPushButtonWidgetClass,
                        ViewMenu,
                        RES_CONVERT( XmNlabelString, "Show Input" ),
                        RES_CONVERT( XmNmnemonic, "S" ),
                        NULL );
        UxPutContext( showInputButton, (char *) UxKuipIoContext );


        /* Creation of menu1_top_b3 */
        menu1_top_b3 = XtVaCreateManagedWidget( "menu1_top_b3",
                        xmCascadeButtonWidgetClass,
                        menuBar,
                        RES_CONVERT( XmNlabelString, "View" ),
                        XmNsubMenuId, ViewMenu,
                        RES_CONVERT( XmNmnemonic, "V" ),
                        NULL );
        UxPutContext( menu1_top_b3, (char *) UxKuipIoContext );


        /* Creation of OptionMenu */
        OptionMenu_shell = XtVaCreatePopupShell ("OptionMenu_shell",
                        xmMenuShellWidgetClass, menuBar,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        OptionMenu = XtVaCreateWidget( "OptionMenu",
                        xmRowColumnWidgetClass,
                        OptionMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( OptionMenu, (char *) UxKuipIoContext );


        /* Creation of clearTrButton */
        clearTrButton = XtVaCreateManagedWidget( "clearTrButton",
                        xmPushButtonWidgetClass,
                        OptionMenu,
                        RES_CONVERT( XmNlabelString, "Clear Transcript Pad" ),
                        RES_CONVERT( XmNmnemonic, "C" ),
                        NULL );
        UxPutContext( clearTrButton, (char *) UxKuipIoContext );


        /* Creation of menu1_top_b4 */
        menu1_top_b4 = XtVaCreateManagedWidget( "menu1_top_b4",
                        xmCascadeButtonWidgetClass,
                        menuBar,
                        RES_CONVERT( XmNlabelString, "Options" ),
                        RES_CONVERT( XmNmnemonic, "O" ),
                        XmNsubMenuId, OptionMenu,
                        NULL );
        UxPutContext( menu1_top_b4, (char *) UxKuipIoContext );


        /* Creation of HelpMenu */
        HelpMenu_shell = XtVaCreatePopupShell ("HelpMenu_shell",
                        xmMenuShellWidgetClass, menuBar,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        HelpMenu = XtVaCreateWidget( "HelpMenu",
                        xmRowColumnWidgetClass,
                        HelpMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( HelpMenu, (char *) UxKuipIoContext );


        /* Creation of helpKxtermButton */
        helpKxtermButton = XtVaCreateManagedWidget( "helpKxtermButton",
                        xmPushButtonWidgetClass,
                        HelpMenu,
                        RES_CONVERT( XmNlabelString, "On Kxterm" ),
                        NULL );
        UxPutContext( helpKxtermButton, (char *) UxKuipIoContext );


        /* Creation of helpResourceButton */
        helpResourceButton = XtVaCreateManagedWidget( "helpResourceButton",
                        xmPushButtonWidgetClass,
                        HelpMenu,
                        RES_CONVERT( XmNlabelString, "On Resources" ),
                        NULL );
        UxPutContext( helpResourceButton, (char *) UxKuipIoContext );


        /* Creation of helpEditButton */
        helpEditButton = XtVaCreateManagedWidget( "helpEditButton",
                        xmPushButtonWidgetClass,
                        HelpMenu,
                        RES_CONVERT( XmNlabelString, "On Edit Keys" ),
                        NULL );
        UxPutContext( helpEditButton, (char *) UxKuipIoContext );


        /* Creation of menu1_top_b5 */
        menu1_top_b5 = XtVaCreateManagedWidget( "menu1_top_b5",
                        xmCascadeButtonWidgetClass,
                        menuBar,
                        RES_CONVERT( XmNlabelString, "Help" ),
                        RES_CONVERT( XmNmnemonic, "H" ),
                        XmNsubMenuId, HelpMenu,
                        NULL );
        UxPutContext( menu1_top_b5, (char *) UxKuipIoContext );

        XtVaSetValues(menuBar,
                        XmNmenuHelpWidget, menu1_top_b5,
                        NULL );


        XtAddCallback( kuipIo, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxKuipIoContext);

        XmMainWindowSetAreas( kuipIo, menuBar, (Widget) NULL,
                        (Widget) NULL, (Widget) NULL, frame1 );

        return ( kuipIo );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_kuipIo()
{
        Widget                  rtrn;
        _UxCkuipIo              *UxContext;

        UxKuipIoContext = UxContext =
                (_UxCkuipIo *) UxNewContext( sizeof(_UxCkuipIo), False );


        rtrn = _Uxbuild_kuipIo();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

