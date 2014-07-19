/*
 * $Id: kuxbro.c,v 1.1.1.1 1996/03/08 15:33:10 mclareni Exp $
 *
 * $Log: kuxbro.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:10  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/01 05/04/94  17.59.01  by  N.Cremel*/
/*-- Author :*/
/*
 * kuipBrowser.c
 * This file has to be slightly modified by hand to add:
 * - +SEQ,KUIP_H
 * - menu entry "All Cycles" in "View" pulldown menu for VMS
 */

#include "kuip/kuip.h"

/*******************************************************************************
        kuipBrowser.c

       Associated Header file: kuipBrowser.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/CascadeB.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/SeparatoG.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/MainW.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "kuipBrowser.h" */
#include "mkuxbr.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget        _Uxbuild_kuipBrowser()
{
        Widget                _UxParent;
        Widget                FilePdMenu_shell;
        Widget                ViewPdMenu_shell;
        Widget                HelpPdMenu_shell;


        /* Creation of kuipBrowser */
        _UxParent = XtVaCreatePopupShell( "kuipBrowser_shell",
                        topLevelShellWidgetClass, UxTopLevel,
                        XmNx, 540,
                        XmNy, 80,
                        XmNwidth, 580,
                        XmNheight, 450,
                        XmNtitle, "kuipBrowser",
                        XmNiconName, "kuipBrowser",
                        NULL );

        kuipBrowser = XtVaCreateManagedWidget( "kuipBrowser",
                        xmMainWindowWidgetClass,
                        _UxParent,
                        XmNwidth, 580,
                        XmNheight, 450,
                        XmNmappedWhenManaged, TRUE,
                        NULL );
        UxPutContext( kuipBrowser, (char *) UxKuipBrowserContext );


        /* Creation of kuipBrowserFrame */
        kuipBrowserFrame = XtVaCreateManagedWidget( "kuipBrowserFrame",
                        xmFrameWidgetClass,
                        kuipBrowser,
                        NULL );
        UxPutContext( kuipBrowserFrame, (char *) UxKuipBrowserContext );


        /* Creation of browserForm */
        browserForm = XtVaCreateManagedWidget( "browserForm",
                        xmFormWidgetClass,
                        kuipBrowserFrame,
                        XmNwidth, 530,
                        XmNautoUnmanage, FALSE,
                        NULL );
        UxPutContext( browserForm, (char *) UxKuipBrowserContext );


        /* Creation of fileScWindow */
        fileScWindow = XtVaCreateManagedWidget( "fileScWindow",
                        xmScrolledWindowWidgetClass,
                        browserForm,
                        XmNx, 29,
                        XmNy, 29,
                        XmNshadowThickness, 0,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNbottomAttachment, XmATTACH_FORM,
                     /* XmNbottomOffset, 52, */
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_NONE,
                        XmNtopAttachment, XmATTACH_FORM,
                     /* XmNtopOffset, 59, */
                        NULL );
        UxPutContext( fileScWindow, (char *) UxKuipBrowserContext );


        /* Creation of fileList */
        fileList = XtVaCreateManagedWidget( "fileList",
                        xmListWidgetClass,
                        fileScWindow,
                        XmNwidth, 140,
                        XmNheight, 210,
                        XmNautomaticSelection, TRUE,
                        XmNselectionPolicy, XmBROWSE_SELECT,
                        XmNvisibleItemCount, 50,
                        XmNlistSizePolicy, XmCONSTANT,
                        NULL );
        UxPutContext( fileList, (char *) UxKuipBrowserContext );


        /* Creation of dirScWindow */
        dirScWindow = XtVaCreateManagedWidget( "dirScWindow",
                        xmScrolledWindowWidgetClass,
                        browserForm,
                        XmNshadowThickness, 2,
                        XmNbottomAttachment, XmATTACH_FORM,
                     /* XmNbottomOffset, 52, */
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, fileScWindow,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                     /* XmNtopOffset, 59, */
                        XmNwidth, 460,
                        XmNheight, 333,
                        XmNy, 30,
                        NULL );
        UxPutContext( dirScWindow, (char *) UxKuipBrowserContext );


        /* Creation of topSeparator1 */
        topSeparator1 = XtVaCreateManagedWidget( "topSeparator1",
                        xmSeparatorGadgetClass,
                        browserForm,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 5,
                        XmNbottomWidget, fileScWindow,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, -5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, -5,
                        XmNwidth, 583,
                        XmNx, 0,
                        NULL );
        UxPutContext( topSeparator1, (char *) UxKuipBrowserContext );


        /* Creation of bottomSeparator1 */
        bottomSeparator1 = XtVaCreateManagedWidget( "bottomSeparator1",
                        xmSeparatorGadgetClass,
                        browserForm,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, -5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, -5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, fileScWindow,
                        NULL );
        UxPutContext( bottomSeparator1, (char *) UxKuipBrowserContext );


        /* Creation of closeButton */
        closeButton = XtVaCreateManagedWidget( "closeButton",
                        xmPushButtonWidgetClass,
                        browserForm,
                        XmNx, 529,
                        XmNy, 349,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, " Close " ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        NULL );
        UxPutContext( closeButton, (char *) UxKuipBrowserContext );


        /* Creation of cloneButton */
        cloneButton = XtVaCreateManagedWidget( "cloneButton",
                        xmPushButtonWidgetClass,
                        browserForm,
                        XmNx, 489,
                        XmNy, 349,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, " Clone " ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 5,
                        XmNrightWidget, closeButton,
                        NULL );
        UxPutContext( cloneButton, (char *) UxKuipBrowserContext );


        /* Creation of form1 */
        form1 = XtVaCreateManagedWidget( "form1",
                        xmFormWidgetClass,
                        browserForm,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 25,
                        XmNy, 0,
                        XmNwidth, 60,
                        XmNheight, 10,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomWidget, topSeparator1,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        NULL );
        UxPutContext( form1, (char *) UxKuipBrowserContext );


        /* Creation of separatorGadget1 */
        separatorGadget1 = XtVaCreateManagedWidget( "separatorGadget1",
                        xmSeparatorGadgetClass,
                        form1,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_POSITION,
                     /* XmNtopPosition, 50, */
                        NULL );
        UxPutContext( separatorGadget1, (char *) UxKuipBrowserContext );


        /* Creation of form2 */
        form2 = XtVaCreateManagedWidget( "form2",
                        xmFormWidgetClass,
                        form1,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNheight, 25,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomWidget, separatorGadget1,
                        NULL );
        UxPutContext( form2, (char *) UxKuipBrowserContext );


        /* Creation of pathLabel */
        pathLabel = XtVaCreateManagedWidget( "pathLabel",
                        xmLabelGadgetClass,
                        form2,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Path: " ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        NULL );
        UxPutContext( pathLabel, (char *) UxKuipBrowserContext );


        /* Creation of pathText */
        pathText = XtVaCreateManagedWidget( "pathText",
                        xmTextWidgetClass,
                        form2,
                        XmNhighlightThickness, 0,
                        XmNshadowThickness, 0,
                        XmNeditable, FALSE,
                        RES_CONVERT( XmNselectionArray, "select_position" ),
                        XmNselectionArrayCount, 1,
                        XmNblinkRate, 0,
                        XmNcursorPositionVisible, FALSE,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 5,
                        XmNleftWidget, pathLabel,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        NULL );
        UxPutContext( pathText, (char *) UxKuipBrowserContext );


        /* Creation of form3 */
        form3 = XtVaCreateManagedWidget( "form3",
                        xmFormWidgetClass,
                        form1,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 0,
                        XmNy, 30,
                        XmNwidth, 70,
                        XmNheight, 20,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 0,
                        XmNtopWidget, separatorGadget1,
                        NULL );
        UxPutContext( form3, (char *) UxKuipBrowserContext );


        /* Creation of counterLabel */
        counterLabel = XtVaCreateManagedWidget( "counterLabel",
                        xmLabelWidgetClass,
                        form3,
                        XmNalignment, XmALIGNMENT_END,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        NULL );
        UxPutContext( counterLabel, (char *) UxKuipBrowserContext );


        /* Creation of form4 */
        form4 = XtVaCreateManagedWidget( "form4",
                        xmFormWidgetClass,
                        browserForm,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightWidget, cloneButton,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 0,
                        XmNtopWidget, bottomSeparator1,
                        NULL );
        UxPutContext( form4, (char *) UxKuipBrowserContext );


        /* Creation of dirNameLabel */
        dirNameLabel = XtVaCreateManagedWidget( "dirNameLabel",
                        xmLabelWidgetClass,
                        form4,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 2,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNheight, 20,
                        NULL );
        UxPutContext( dirNameLabel, (char *) UxKuipBrowserContext );


        /* Creation of fileNameLabel */
        fileNameLabel = XtVaCreateManagedWidget( "fileNameLabel",
                        xmLabelWidgetClass,
                        form4,
                        XmNheight, 20,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomWidget, dirNameLabel,
                        NULL );
        UxPutContext( fileNameLabel, (char *) UxKuipBrowserContext );


        /* Creation of browserMenu */
        browserMenu = XtVaCreateManagedWidget( "browserMenu",
                        xmRowColumnWidgetClass,
                        kuipBrowser,
                        XmNrowColumnType, XmMENU_BAR,
                        XmNmenuAccelerator, "<KeyUp>F10",
                        NULL );
        UxPutContext( browserMenu, (char *) UxKuipBrowserContext );


        /* Creation of FilePdMenu */
        FilePdMenu_shell = XtVaCreatePopupShell ("FilePdMenu_shell",
                        xmMenuShellWidgetClass, browserMenu,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        FilePdMenu = XtVaCreateWidget( "FilePdMenu",
                        xmRowColumnWidgetClass,
                        FilePdMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( FilePdMenu, (char *) UxKuipBrowserContext );


        /* Creation of FileMenu */
        FileMenu = XtVaCreateManagedWidget( "FileMenu",
                        xmCascadeButtonWidgetClass,
                        browserMenu,
                        RES_CONVERT( XmNlabelString, "File" ),
                        RES_CONVERT( XmNmnemonic, "F" ),
                        XmNsubMenuId, FilePdMenu,
                        NULL );
        UxPutContext( FileMenu, (char *) UxKuipBrowserContext );


        /* Creation of ViewPdMenu */
        ViewPdMenu_shell = XtVaCreatePopupShell ("ViewPdMenu_shell",
                        xmMenuShellWidgetClass, browserMenu,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        ViewPdMenu = XtVaCreateWidget( "ViewPdMenu",
                        xmRowColumnWidgetClass,
                        ViewPdMenu_shell,
                        XmNradioBehavior, FALSE,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( ViewPdMenu, (char *) UxKuipBrowserContext );


#ifdef ALL_CYCLE /* vms */
        /* Creation of ViewPdMenu_allcycle */
        ViewPdMenu_allcycle = XtVaCreateManagedWidget( "ViewPdMenu_allcycle",
                        xmToggleButtonGadgetClass,
                        ViewPdMenu,
                        RES_CONVERT( XmNlabelString, "All Cycle" ),
                        XmNset, FALSE,
                        NULL );
        UxPutContext( ViewPdMenu_allcycle, (char *) UxKuipBrowserContext );


        /* Creation of menu1_p2_b4 */
        menu1_p2_b4 = XtVaCreateManagedWidget( "menu1_p2_b4",
                        xmSeparatorGadgetClass,
                        ViewPdMenu,
                        NULL );
        UxPutContext( menu1_p2_b4, (char *) UxKuipBrowserContext );
#endif


        /* Creation of ViewPdMenu_icon */
        ViewPdMenu_icon = XtVaCreateManagedWidget( "ViewPdMenu_icon",
                        xmToggleButtonGadgetClass,
                        ViewPdMenu,
                        RES_CONVERT( XmNlabelString, "Icons" ),
                        XmNset, TRUE,
                        NULL );
        UxPutContext( ViewPdMenu_icon, (char *) UxKuipBrowserContext );


        /* Creation of ViewPdMenu_sicon */
        ViewPdMenu_sicon = XtVaCreateManagedWidget( "ViewPdMenu_sicon",
                        xmToggleButtonGadgetClass,
                        ViewPdMenu,
                        RES_CONVERT( XmNlabelString, "Small Icons" ),
                        NULL );
        UxPutContext( ViewPdMenu_sicon, (char *) UxKuipBrowserContext );


        /* Creation of ViewPdMenu_nicon */
        ViewPdMenu_nicon = XtVaCreateManagedWidget( "ViewPdMenu_nicon",
                        xmToggleButtonGadgetClass,
                        ViewPdMenu,
                        RES_CONVERT( XmNlabelString, "No Icons" ),
                        NULL );
        UxPutContext( ViewPdMenu_nicon, (char *) UxKuipBrowserContext );


        /* Creation of ViewPdMenu_title */
        ViewPdMenu_title = XtVaCreateManagedWidget( "ViewPdMenu_title",
                        xmToggleButtonGadgetClass,
                        ViewPdMenu,
                        RES_CONVERT( XmNlabelString, "Titles" ),
                        NULL );
        UxPutContext( ViewPdMenu_title, (char *) UxKuipBrowserContext );


        /* Creation of menu1_p2_b6 */
        menu1_p2_b6 = XtVaCreateManagedWidget( "menu1_p2_b6",
                        xmSeparatorGadgetClass,
                        ViewPdMenu,
                        NULL );
        UxPutContext( menu1_p2_b6, (char *) UxKuipBrowserContext );


        /* Creation of ViewPdMenu_selall */
        ViewPdMenu_selall = XtVaCreateManagedWidget( "ViewPdMenu_selall",
                        xmPushButtonGadgetClass,
                        ViewPdMenu,
                        RES_CONVERT( XmNlabelString, "Select All" ),
                        NULL );
        UxPutContext( ViewPdMenu_selall, (char *) UxKuipBrowserContext );


        /* Creation of menu1_p2_b8 */
        menu1_p2_b8 = XtVaCreateManagedWidget( "menu1_p2_b8",
                        xmSeparatorGadgetClass,
                        ViewPdMenu,
                        NULL );
        UxPutContext( menu1_p2_b8, (char *) UxKuipBrowserContext );


        /* Creation of ViewPdMenu_filt */
        ViewPdMenu_filt = XtVaCreateManagedWidget( "ViewPdMenu_filt",
                        xmPushButtonGadgetClass,
                        ViewPdMenu,
                        RES_CONVERT( XmNlabelString, "Filter..." ),
                        NULL );
        UxPutContext( ViewPdMenu_filt, (char *) UxKuipBrowserContext );


        /* Creation of ViewMenu */
        ViewMenu = XtVaCreateManagedWidget( "ViewMenu",
                        xmCascadeButtonWidgetClass,
                        browserMenu,
                        RES_CONVERT( XmNlabelString, "View" ),
                        RES_CONVERT( XmNmnemonic, "V" ),
                        XmNsubMenuId, ViewPdMenu,
                        NULL );
        UxPutContext( ViewMenu, (char *) UxKuipBrowserContext );


        /* Creation of HelpPdMenu */
        HelpPdMenu_shell = XtVaCreatePopupShell ("HelpPdMenu_shell",
                        xmMenuShellWidgetClass, browserMenu,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        HelpPdMenu = XtVaCreateWidget( "HelpPdMenu",
                        xmRowColumnWidgetClass,
                        HelpPdMenu_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( HelpPdMenu, (char *) UxKuipBrowserContext );


        /* Creation of HelpPdMenu_help */
        HelpPdMenu_help = XtVaCreateManagedWidget( "HelpPdMenu_help",
                        xmPushButtonGadgetClass,
                        HelpPdMenu,
                        RES_CONVERT( XmNlabelString, "Help..." ),
                        NULL );
        UxPutContext( HelpPdMenu_help, (char *) UxKuipBrowserContext );


        /* Creation of HelpMenu */
        HelpMenu = XtVaCreateManagedWidget( "HelpMenu",
                        xmCascadeButtonWidgetClass,
                        browserMenu,
                        RES_CONVERT( XmNlabelString, "Help" ),
                        RES_CONVERT( XmNmnemonic, "H" ),
                        XmNsubMenuId, HelpPdMenu,
                        NULL );
        UxPutContext( HelpMenu, (char *) UxKuipBrowserContext );

        XtVaSetValues(browserMenu,
                        XmNmenuHelpWidget, HelpMenu,
                        NULL );


        XtAddCallback( kuipBrowser, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxKuipBrowserContext);

        XmMainWindowSetAreas( kuipBrowser, browserMenu, (Widget) NULL,
                        (Widget) NULL, (Widget) NULL, kuipBrowserFrame );

        return ( kuipBrowser );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget        create_kuipBrowser()
{
        Widget                  rtrn;
        _UxCkuipBrowser         *UxContext;

        UxKuipBrowserContext = UxContext =
          (_UxCkuipBrowser *) UxNewContext( sizeof(_UxCkuipBrowser), False );


        rtrn = _Uxbuild_kuipBrowser();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

