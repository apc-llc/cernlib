/*
 * $Id: kuxpan.c,v 1.1.1.1 1996/03/08 15:33:10 mclareni Exp $
 *
 * $Log: kuxpan.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:10  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/03 29/11/94  13.55.19  by  N.Cremel*/
/*-- Author :    N.Cremel   13/08/92*/
/*
 * kuipPanel.c
 */

/*******************************************************************************
       kuipPanel.c

       Associated Header file: kuipPanel.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/CascadeB.h>
#include <Xm/Separator.h>
#include <Xm/ToggleBG.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/MainW.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "kuipPanel.h" */
#include "mkuxpa.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget       _Uxbuild_kuipPanel()
{
       Widget              _UxParent;
       Widget              FilePdMenPN_shell;
       Widget              ViewPdMenPN_shell;
       Widget              HelpPdMenPN_shell;


       /* Creation of kuipPanel */
       _UxParent = XtVaCreatePopupShell( "kuipPanel_shell",
                     topLevelShellWidgetClass, UxTopLevel,
                     XmNtitle, "kuipPanel",
                     XmNiconName, "kuipPanel",
                     NULL );

       kuipPanel = XtVaCreateManagedWidget( "kuipPanel",
                     xmMainWindowWidgetClass,
                     _UxParent,
                     NULL );
       UxPutContext( kuipPanel, (char *) UxKuipPanelContext );


       /* Creation of kuipPanelFrame */
       kuipPanelFrame = XtVaCreateManagedWidget( "kuipPanelFrame",
                     xmFrameWidgetClass,
                     kuipPanel,
                     NULL );
       UxPutContext( kuipPanelFrame, (char *) UxKuipPanelContext );


       /* Creation of kuipPanelForm */
       kuipPanelForm = XtVaCreateManagedWidget( "kuipPanelForm",
                     xmFormWidgetClass,
                     kuipPanelFrame,
                     NULL );
       UxPutContext( kuipPanelForm, (char *) UxKuipPanelContext );


       /* Creation of km_scrollPI */
       km_scrollPI = XtVaCreateManagedWidget( "km_scrollPI",
                     xmScrolledWindowWidgetClass,
                     kuipPanelForm,
                     XmNscrollingPolicy, XmAUTOMATIC,
                     XmNbottomOffset, 5,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 5,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 5,
                     XmNtopOffset, 5,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNtopAttachment, XmATTACH_FORM,
                     NULL );
       UxPutContext( km_scrollPI, (char *) UxKuipPanelContext );


       /* Creation of km_rowcolPI */
       km_rowcolPI = XtVaCreateManagedWidget( "km_rowcolPI",
                     xmRowColumnWidgetClass,
                     km_scrollPI,
                     XmNadjustLast, FALSE,
                     XmNorientation, XmHORIZONTAL,
                     XmNpacking, XmPACK_COLUMN,
                     NULL );
       UxPutContext( km_rowcolPI, (char *) UxKuipPanelContext );


       /* Creation of km_scrollPN */
       /* km_scrollPN = XtVaCreateManagedWidget( "km_scrollPN", */
       km_scrollPN = XtVaCreateWidget( "km_scrollPN",
                     xmScrolledWindowWidgetClass,
                     kuipPanelForm,
                     XmNscrollingPolicy, XmAUTOMATIC,
                     XmNbottomOffset, 5,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 5,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 5,
                     XmNtopOffset, 5,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNtopAttachment, XmATTACH_FORM,
                     NULL );
       UxPutContext( km_scrollPN, (char *) UxKuipPanelContext );


       /* Creation of km_rowcolPN */
       km_rowcolPN = XtVaCreateManagedWidget( "km_rowcolPN",
                     xmRowColumnWidgetClass,
                     km_scrollPN,
                     XmNadjustLast, FALSE,
                     XmNorientation, XmHORIZONTAL,
                     XmNpacking, XmPACK_COLUMN,
                     NULL );
       UxPutContext( km_rowcolPN, (char *) UxKuipPanelContext );


       /* Creation of km_scrollPB */
       /* km_scrollPB = XtVaCreateManagedWidget( "km_scrollPB", */
       km_scrollPB = XtVaCreateWidget( "km_scrollPB",
                     xmScrolledWindowWidgetClass,
                     kuipPanelForm,
                     XmNscrollingPolicy, XmAUTOMATIC,
                     XmNbottomOffset, 5,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 5,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 5,
                     XmNtopOffset, 5,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNtopAttachment, XmATTACH_FORM,
                     NULL );
       UxPutContext( km_scrollPB, (char *) UxKuipPanelContext );


       /* Creation of km_rowcolPB */
       km_rowcolPB = XtVaCreateManagedWidget( "km_rowcolPB",
                     xmRowColumnWidgetClass,
                     km_scrollPB,
                     XmNadjustLast, FALSE,
                     XmNorientation, XmHORIZONTAL,
                     XmNpacking, XmPACK_COLUMN,
                     NULL );
       UxPutContext( km_rowcolPB, (char *) UxKuipPanelContext );


       /* Creation of km_scrollPC */
       /* km_scrollPC = XtVaCreateManagedWidget( "km_scrollPC", */
       km_scrollPC = XtVaCreateWidget( "km_scrollPC",
                     xmScrolledWindowWidgetClass,
                     kuipPanelForm,
                     XmNscrollingPolicy, XmAUTOMATIC,
                     XmNbottomOffset, 5,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 5,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 5,
                     XmNtopOffset, 5,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNtopAttachment, XmATTACH_FORM,
                     NULL );
       UxPutContext( km_scrollPC, (char *) UxKuipPanelContext );


       /* Creation of km_rowcolPC */
       km_rowcolPC = XtVaCreateManagedWidget( "km_rowcolPC",
                     xmRowColumnWidgetClass,
                     km_scrollPC,
                     XmNadjustLast, FALSE,
                     XmNorientation, XmHORIZONTAL,
                     XmNpacking, XmPACK_COLUMN,
                     NULL );
       UxPutContext( km_rowcolPC, (char *) UxKuipPanelContext );


       /* Creation of km_scrollP1 */
       /* km_scrollP1 = XtVaCreateManagedWidget( "km_scrollP1", */
       km_scrollP1 = XtVaCreateWidget( "km_scrollP1",
                     xmScrolledWindowWidgetClass,
                     kuipPanelForm,
                     XmNscrollingPolicy, XmAUTOMATIC,
                     XmNbottomOffset, 5,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 5,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 5,
                     XmNtopOffset, 5,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNtopAttachment, XmATTACH_FORM,
                     NULL );
       UxPutContext( km_scrollP1, (char *) UxKuipPanelContext );


       /* Creation of km_rowcolP1 */
       km_rowcolP1 = XtVaCreateManagedWidget( "km_rowcolP1",
                     xmRowColumnWidgetClass,
                     km_scrollP1,
                     XmNadjustLast, FALSE,
                     XmNorientation, XmHORIZONTAL,
                     XmNpacking, XmPACK_COLUMN,
                     NULL );
       UxPutContext( km_rowcolP1, (char *) UxKuipPanelContext );


       /* Creation of kuipPanelMenu */
       kuipPanelMenu = XtVaCreateManagedWidget( "kuipPanelMenu",
                     xmRowColumnWidgetClass,
                     kuipPanel,
                     XmNrowColumnType, XmMENU_BAR,
                     XmNmenuAccelerator, "<KeyUp>F10",
                     NULL );
       UxPutContext( kuipPanelMenu, (char *) UxKuipPanelContext );


       /* Creation of FilePdMenPN */
       FilePdMenPN_shell = XtVaCreatePopupShell ("FilePdMenPN_shell",
                     xmMenuShellWidgetClass, kuipPanelMenu,
                     XmNwidth, 1,
                     XmNheight, 1,
                     XmNallowShellResize, TRUE,
                     XmNoverrideRedirect, TRUE,
                     NULL );

       FilePdMenPN = XtVaCreateWidget( "FilePdMenPN",
                     xmRowColumnWidgetClass,
                     FilePdMenPN_shell,
                     XmNrowColumnType, XmMENU_PULLDOWN,
                     NULL );
       UxPutContext( FilePdMenPN, (char *) UxKuipPanelContext );


       /* Creation of km_savePNB */
       km_savePNB = XtVaCreateManagedWidget( "km_savePNB",
                     xmPushButtonWidgetClass,
                     FilePdMenPN,
                     RES_CONVERT( XmNlabelString, "Save" ),
                     NULL );
       UxPutContext( km_savePNB, (char *) UxKuipPanelContext );


       /* Creation of km_okPNB */
       km_okPNB = XtVaCreateManagedWidget( "km_okPNB",
                     xmPushButtonWidgetClass,
                     FilePdMenPN,
                     RES_CONVERT( XmNlabelString, "Close" ),
                     NULL );
       UxPutContext( km_okPNB, (char *) UxKuipPanelContext );


       /* Creation of FileMenPN */
       FileMenPN = XtVaCreateManagedWidget( "FileMenPN",
                     xmCascadeButtonWidgetClass,
                     kuipPanelMenu,
                     RES_CONVERT( XmNlabelString, "File" ),
                     RES_CONVERT( XmNmnemonic, "F" ),
                     XmNsubMenuId, FilePdMenPN,
                     NULL );
       UxPutContext( FileMenPN, (char *) UxKuipPanelContext );


       /* Creation of ViewPdMenPN */
       ViewPdMenPN_shell = XtVaCreatePopupShell ("ViewPdMenPN_shell",
                     xmMenuShellWidgetClass, kuipPanelMenu,
                     XmNwidth, 1,
                     XmNheight, 1,
                     XmNallowShellResize, TRUE,
                     XmNoverrideRedirect, TRUE,
                     NULL );

       ViewPdMenPN = XtVaCreateWidget( "ViewPdMenPN",
                     xmRowColumnWidgetClass,
                     ViewPdMenPN_shell,
                     XmNradioBehavior, TRUE,
                     XmNrowColumnType, XmMENU_PULLDOWN,
                     NULL );
       UxPutContext( ViewPdMenPN, (char *) UxKuipPanelContext );


       /* Creation of km_viewPNB_name */
       km_viewPNB_name = XtVaCreateManagedWidget( "km_viewPNB_name",
                     xmToggleButtonGadgetClass,
                     ViewPdMenPN,
                     RES_CONVERT( XmNlabelString, "By Name" ),
                     RES_CONVERT( XmNmnemonic, "N" ),
                     NULL );
       UxPutContext( km_viewPNB_name, (char *) UxKuipPanelContext );


       /* Creation of km_viewPNB_icon */
       km_viewPNB_icon = XtVaCreateManagedWidget( "km_viewPNB_icon",
                     xmToggleButtonGadgetClass,
                     ViewPdMenPN,
                     RES_CONVERT( XmNlabelString, "By Icon" ),
                     RES_CONVERT( XmNmnemonic, "I" ),
                     XmNset, TRUE,
                     NULL );
       UxPutContext( km_viewPNB_icon, (char *) UxKuipPanelContext );


       /* Creation of km_viewPNB_both */
       km_viewPNB_both = XtVaCreateManagedWidget( "km_viewPNB_both",
                     xmToggleButtonGadgetClass,
                     ViewPdMenPN,
                     RES_CONVERT( XmNlabelString, "By Name and Icon" ),
                     RES_CONVERT( XmNmnemonic, "B" ),
                     NULL );
       UxPutContext( km_viewPNB_both, (char *) UxKuipPanelContext );


       /* Creation of km_viewPNB_sep */
       km_viewPNB_sep = XtVaCreateManagedWidget( "km_viewPNB_sep",
                     xmSeparatorWidgetClass,
                     ViewPdMenPN,
                     NULL );
       UxPutContext( km_viewPNB_sep, (char *) UxKuipPanelContext );


       /* Creation of km_viewPNB_command */
       km_viewPNB_command = XtVaCreateManagedWidget( "km_viewPNB_command",
                     xmToggleButtonGadgetClass,
                     ViewPdMenPN,
                     RES_CONVERT( XmNlabelString, "By Command (normal)" ),
                     RES_CONVERT( XmNmnemonic, "C" ),
                     NULL );
       UxPutContext( km_viewPNB_command, (char *) UxKuipPanelContext );


       /* Creation of km_viewPNB_command1 */
       km_viewPNB_command1 = XtVaCreateManagedWidget( "km_viewPNB_command1",
                     xmToggleButtonGadgetClass,
                     ViewPdMenPN,
                     RES_CONVERT( XmNlabelString, "By Command (1 col.)" ),
                     RES_CONVERT( XmNmnemonic, "1" ),
                     NULL );
       UxPutContext( km_viewPNB_command1, (char *) UxKuipPanelContext );


       /* Creation of ViewMenPN */
       ViewMenPN = XtVaCreateManagedWidget( "ViewMenPN",
                     xmCascadeButtonWidgetClass,
                     kuipPanelMenu,
                     RES_CONVERT( XmNlabelString, "View" ),
                     RES_CONVERT( XmNmnemonic, "V" ),
                     XmNsubMenuId, ViewPdMenPN,
                     NULL );
       UxPutContext( ViewMenPN, (char *) UxKuipPanelContext );


       /* Creation of HelpPdMenPN */
       HelpPdMenPN_shell = XtVaCreatePopupShell ("HelpPdMenPN_shell",
                     xmMenuShellWidgetClass, kuipPanelMenu,
                     XmNwidth, 1,
                     XmNheight, 1,
                     XmNallowShellResize, TRUE,
                     XmNoverrideRedirect, TRUE,
                     NULL );

       HelpPdMenPN = XtVaCreateWidget( "HelpPdMenPN",
                     xmRowColumnWidgetClass,
                     HelpPdMenPN_shell,
                     XmNrowColumnType, XmMENU_PULLDOWN,
                     NULL );
       UxPutContext( HelpPdMenPN, (char *) UxKuipPanelContext );


       /* Creation of km_helpPNB1 */
       km_helpPNB1 = XtVaCreateManagedWidget( "km_helpPNB1",
                     xmPushButtonWidgetClass,
                     HelpPdMenPN,
                     RES_CONVERT( XmNlabelString, "On PANEL Command" ),
                     RES_CONVERT( XmNmnemonic, "P" ),
                     NULL );
       UxPutContext( km_helpPNB1, (char *) UxKuipPanelContext );


       /* Creation of km_helpPNB2 */
       km_helpPNB2 = XtVaCreateManagedWidget( "km_helpPNB2",
                     xmPushButtonWidgetClass,
                     HelpPdMenPN,
                     RES_CONVERT( XmNlabelString, "On Panel Edition" ),
                     RES_CONVERT( XmNmnemonic, "E" ),
                     NULL );
       UxPutContext( km_helpPNB2, (char *) UxKuipPanelContext );


       /* Creation of km_helpPNB3 */
       km_helpPNB3 = XtVaCreateManagedWidget( "km_helpPNB3",
                     xmPushButtonWidgetClass,
                     HelpPdMenPN,
                     RES_CONVERT( XmNlabelString, "On Panel Saving" ),
                     RES_CONVERT( XmNmnemonic, "S" ),
                     NULL );
       UxPutContext( km_helpPNB3, (char *) UxKuipPanelContext );


       /* Creation of km_helpPNB4 */
       km_helpPNB4 = XtVaCreateManagedWidget( "km_helpPNB4",
                     xmPushButtonWidgetClass,
                     HelpPdMenPN,
                     RES_CONVERT( XmNlabelString, "On Dollar sign" ),
                     RES_CONVERT( XmNmnemonic, "D" ),
                     NULL );
       UxPutContext( km_helpPNB4, (char *) UxKuipPanelContext );


       /* Creation of km_helpPNB5 */
       km_helpPNB5 = XtVaCreateManagedWidget( "km_helpPNB5",
                     xmPushButtonWidgetClass,
                     HelpPdMenPN,
                     RES_CONVERT( XmNlabelString, "On Minus sign" ),
                     RES_CONVERT( XmNmnemonic, "M" ),
                     NULL );
       UxPutContext( km_helpPNB5, (char *) UxKuipPanelContext );


       /* Creation of km_helpPNB6 */
       km_helpPNB6 = XtVaCreateManagedWidget( "km_helpPNB6",
                     xmPushButtonWidgetClass,
                     HelpPdMenPN,
                     RES_CONVERT( XmNlabelString, "On Double Minus sign" ),
                     RES_CONVERT( XmNmnemonic, "o" ),
                     NULL );
       UxPutContext( km_helpPNB6, (char *) UxKuipPanelContext );


       /* Creation of HelpPdMenPN_b7 */
       HelpPdMenPN_b7 = XtVaCreateManagedWidget( "HelpPdMenPN_b7",
                     xmSeparatorWidgetClass,
                     HelpPdMenPN,
                     NULL );
       UxPutContext( HelpPdMenPN_b7, (char *) UxKuipPanelContext );


       /* Creation of km_helpCONTEXT */
       km_helpCONTEXT = XtVaCreateManagedWidget( "km_helpCONTEXT",
                     xmPushButtonWidgetClass,
                     HelpPdMenPN,
                     RES_CONVERT( XmNlabelString, "Select context help ..." ),
                     NULL );
       UxPutContext( km_helpCONTEXT, (char *) UxKuipPanelContext );


       /* Creation of HelpMenPN */
       HelpMenPN = XtVaCreateManagedWidget( "HelpMenPN",
                     xmCascadeButtonWidgetClass,
                     kuipPanelMenu,
                     RES_CONVERT( XmNlabelString, "Help" ),
                     RES_CONVERT( XmNmnemonic, "H" ),
                     XmNsubMenuId, HelpPdMenPN,
                     NULL );
       UxPutContext( HelpMenPN, (char *) UxKuipPanelContext );

       XtVaSetValues(kuipPanelMenu,
                     XmNmenuHelpWidget, HelpMenPN,
                     NULL );


       XtAddCallback( kuipPanel, XmNdestroyCallback,
              (XtCallbackProc) UxDestroyContextCB,
              (XtPointer) UxKuipPanelContext);

       XmMainWindowSetAreas( kuipPanel, kuipPanelMenu, (Widget) NULL,
                     (Widget) NULL, (Widget) NULL, kuipPanelFrame );

       return ( kuipPanel );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget       create_kuipPanel()
{
       Widget                  rtrn;
       _UxCkuipPanel           *UxContext;

       UxKuipPanelContext = UxContext =
              (_UxCkuipPanel *) UxNewContext( sizeof(_UxCkuipPanel), False );


       rtrn = _Uxbuild_kuipPanel();

       return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

