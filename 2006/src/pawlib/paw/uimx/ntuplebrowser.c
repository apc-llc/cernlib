/*
 * $Id: ntuplebrowser.c,v 1.1.1.1 1996/03/01 11:39:34 mclareni Exp $
 *
 * $Log: ntuplebrowser.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:34  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 09/03/94  19.27.52  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
       ntupleBrowser.c
       (Generated from interface file ntupleBrowser.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/SeparatoG.h>
#include <Xm/Form.h>



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
        Widget  UxseparatorGadget1;
        Widget  UxseparatorGadget2;
        Widget  UxrowColumn1;
        Widget  Uxform1;
        Widget  Uxform2;
        Widget  Uxform3;
        Widget  UxscrolledWindow1;
        Widget  UxrowColumn2;
        Widget  Uxform4;
        Widget  UxlabelGadget4;
        Widget  Uxform5;
        Widget  UxlabelGadget5;
        Widget  Uxform6;
        Widget  UxlabelGadget6;
        Widget  Uxframe1;
        Widget  Uxlabel1;
        Widget  UxrowColumn3;
} _UxCntupleBrowser;

static _UxCntupleBrowser       *UxNtupleBrowserContext;
#define separatorGadget1        UxNtupleBrowserContext->UxseparatorGadget1
#define separatorGadget2        UxNtupleBrowserContext->UxseparatorGadget2
#define rowColumn1              UxNtupleBrowserContext->UxrowColumn1
#define form1                   UxNtupleBrowserContext->Uxform1
#define form2                   UxNtupleBrowserContext->Uxform2
#define form3                   UxNtupleBrowserContext->Uxform3
#define scrolledWindow1         UxNtupleBrowserContext->UxscrolledWindow1
#define rowColumn2              UxNtupleBrowserContext->UxrowColumn2
#define form4                   UxNtupleBrowserContext->Uxform4
#define labelGadget4            UxNtupleBrowserContext->UxlabelGadget4
#define form5                   UxNtupleBrowserContext->Uxform5
#define labelGadget5            UxNtupleBrowserContext->UxlabelGadget5
#define form6                   UxNtupleBrowserContext->Uxform6
#define labelGadget6            UxNtupleBrowserContext->UxlabelGadget6
#define frame1                  UxNtupleBrowserContext->Uxframe1
#define label1                  UxNtupleBrowserContext->Uxlabel1
#define rowColumn3              UxNtupleBrowserContext->UxrowColumn3


Widget  ntupleBrowser;
Widget  closeNtButton;
Widget  helpNtButton;
Widget  xNtToggle;
Widget  xNtText;
Widget  yNtToggle;
Widget  yNtText;
Widget  zNtToggle;
Widget  zNtText;
Widget  varNtList;
Widget  firstNtText;
Widget  nrowNtText;
Widget  histoNtText;
Widget  cutEditorNtButton;
Widget  scanNtButton;
Widget  columnsNtLabel;
Widget  titleNtLabel;
Widget  ignoreCutNtToggle;
Widget  overlayNtToggle;
Widget  extendedNtToggle;
Widget  plotNtButton;
Widget  loopNtButton;
Widget  projectNtButton;
Widget  rebinNtButton;
Widget  optionForm;
Widget  profileNtToggle;
Widget  boxesNtToggle;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_ntupleBrowser( void );

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_ntupleBrowser()
{
        Widget          _UxParent;


        /* Creation of ntupleBrowser */
        _UxParent = XtVaCreatePopupShell( "ntupleBrowser_shell",
                        topLevelShellWidgetClass, UxTopLevel,
                        XmNwidth, 432,
                        XmNheight, 510,
                        XmNtitle, "ntupleBrowser",
                        XmNiconName, "ntupleBrowser",
                        NULL );

        ntupleBrowser = XtVaCreateManagedWidget( "ntupleBrowser",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNwidth, 432,
                        XmNheight, 510,
                        RES_CONVERT( XmNdialogTitle, "Ntuple Viewer" ),
                        NULL );
        UxPutContext( ntupleBrowser, (char *) UxNtupleBrowserContext );


        /* Creation of separatorGadget1 */
        separatorGadget1 = XtVaCreateManagedWidget( "separatorGadget1",
                        xmSeparatorGadgetClass,
                        ntupleBrowser,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 40,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        NULL );
        UxPutContext( separatorGadget1, (char *) UxNtupleBrowserContext );


        /* Creation of separatorGadget2 */
        separatorGadget2 = XtVaCreateManagedWidget( "separatorGadget2",
                        xmSeparatorGadgetClass,
                        ntupleBrowser,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 30,
                        NULL );
        UxPutContext( separatorGadget2, (char *) UxNtupleBrowserContext );


        /* Creation of closeNtButton */
        closeNtButton = XtVaCreateManagedWidget( "closeNtButton",
                        xmPushButtonWidgetClass,
                        ntupleBrowser,
                        XmNwidth, 70,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        NULL );
        UxPutContext( closeNtButton, (char *) UxNtupleBrowserContext );


        /* Creation of helpNtButton */
        helpNtButton = XtVaCreateManagedWidget( "helpNtButton",
                        xmPushButtonWidgetClass,
                        ntupleBrowser,
                        XmNx, 320,
                        XmNy, 350,
                        XmNwidth, 70,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Help..." ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        NULL );
        UxPutContext( helpNtButton, (char *) UxNtupleBrowserContext );


        /* Creation of rowColumn1 */
        rowColumn1 = XtVaCreateManagedWidget( "rowColumn1",
                        xmRowColumnWidgetClass,
                        ntupleBrowser,
                        XmNnumColumns, 1,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 40,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, separatorGadget2,
                        NULL );
        UxPutContext( rowColumn1, (char *) UxNtupleBrowserContext );


        /* Creation of form1 */
        form1 = XtVaCreateManagedWidget( "form1",
                        xmFormWidgetClass,
                        rowColumn1,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 3,
                        XmNy, 3,
                        XmNwidth, 10,
                        XmNheight, 30,
                        NULL );
        UxPutContext( form1, (char *) UxNtupleBrowserContext );


        /* Creation of xNtToggle */
        xNtToggle = XtVaCreateManagedWidget( "xNtToggle",
                        xmToggleButtonWidgetClass,
                        form1,
                        XmNwidth, 50,
                        XmNheight, 15,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "X" ),
                        XmNspacing, 15,
                        NULL );
        UxPutContext( xNtToggle, (char *) UxNtupleBrowserContext );


        /* Creation of xNtText */
        xNtText = XtVaCreateManagedWidget( "xNtText",
                        xmTextWidgetClass,
                        form1,
                        XmNwidth, 115,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNmarginHeight, 2,
                        XmNmarginWidth, 2,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 15,
                        XmNleftWidget, xNtToggle,
                        RES_CONVERT( XmNselectionArray, "select_position" ),
                        XmNselectionArrayCount, 1,
                        NULL );
        UxPutContext( xNtText, (char *) UxNtupleBrowserContext );


        /* Creation of form2 */
        form2 = XtVaCreateManagedWidget( "form2",
                        xmFormWidgetClass,
                        rowColumn1,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 13,
                        XmNy, 13,
                        XmNwidth, 10,
                        XmNheight, 30,
                        NULL );
        UxPutContext( form2, (char *) UxNtupleBrowserContext );


        /* Creation of yNtToggle */
        yNtToggle = XtVaCreateManagedWidget( "yNtToggle",
                        xmToggleButtonWidgetClass,
                        form2,
                        XmNwidth, 50,
                        XmNheight, 15,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Y" ),
                        XmNspacing, 15,
                        NULL );
        UxPutContext( yNtToggle, (char *) UxNtupleBrowserContext );


        /* Creation of yNtText */
        yNtText = XtVaCreateManagedWidget( "yNtText",
                        xmTextWidgetClass,
                        form2,
                        XmNwidth, 115,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNmarginHeight, 2,
                        XmNmarginWidth, 2,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, yNtToggle,
                        XmNleftOffset, 15,
                        RES_CONVERT( XmNselectionArray, "select_position" ),
                        XmNselectionArrayCount, 1,
                        NULL );
        UxPutContext( yNtText, (char *) UxNtupleBrowserContext );


        /* Creation of form3 */
        form3 = XtVaCreateManagedWidget( "form3",
                        xmFormWidgetClass,
                        rowColumn1,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 13,
                        XmNy, 46,
                        XmNwidth, 10,
                        XmNheight, 30,
                        NULL );
        UxPutContext( form3, (char *) UxNtupleBrowserContext );


        /* Creation of zNtToggle */
        zNtToggle = XtVaCreateManagedWidget( "zNtToggle",
                        xmToggleButtonWidgetClass,
                        form3,
                        XmNx, 0,
                        XmNy, 0,
                        XmNwidth, 50,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Z" ),
                        XmNspacing, 15,
                        NULL );
        UxPutContext( zNtToggle, (char *) UxNtupleBrowserContext );


        /* Creation of zNtText */
        zNtText = XtVaCreateManagedWidget( "zNtText",
                        xmTextWidgetClass,
                        form3,
                        XmNwidth, 115,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNmarginHeight, 2,
                        XmNmarginWidth, 2,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, zNtToggle,
                        XmNleftOffset, 15,
                        RES_CONVERT( XmNselectionArray, "select_position" ),
                        XmNselectionArrayCount, 1,
                        NULL );
        UxPutContext( zNtText, (char *) UxNtupleBrowserContext );


        /* Creation of scrolledWindow1 */
        scrolledWindow1 = XtVaCreateManagedWidget( "scrolledWindow1",
                        xmScrolledWindowWidgetClass,
                        ntupleBrowser,
                        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                        XmNvisualPolicy, XmVARIABLE,
                        XmNscrollBarDisplayPolicy, XmSTATIC,
                        XmNshadowThickness, 0,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomWidget, separatorGadget1,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, separatorGadget2,
                        XmNbottomOffset, 5,
                        XmNtopOffset, 5,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 35,
                        NULL );
        UxPutContext( scrolledWindow1, (char *) UxNtupleBrowserContext );


        /* Creation of varNtList */
        varNtList = XtVaCreateManagedWidget( "varNtList",
                        xmListWidgetClass,
                        scrolledWindow1,
                        XmNwidth, 155,
                        XmNselectionPolicy, XmEXTENDED_SELECT,
                        XmNlistSizePolicy, XmCONSTANT,
                        NULL );
        UxPutContext( varNtList, (char *) UxNtupleBrowserContext );


        /* Creation of rowColumn2 */
        rowColumn2 = XtVaCreateManagedWidget( "rowColumn2",
                        xmRowColumnWidgetClass,
                        ntupleBrowser,
                        XmNnumColumns, 1,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, rowColumn1,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 40,
                        NULL );
        UxPutContext( rowColumn2, (char *) UxNtupleBrowserContext );


        /* Creation of form4 */
        form4 = XtVaCreateManagedWidget( "form4",
                        xmFormWidgetClass,
                        rowColumn2,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNheight, 30,
                        NULL );
        UxPutContext( form4, (char *) UxNtupleBrowserContext );


        /* Creation of labelGadget4 */
        labelGadget4 = XtVaCreateManagedWidget( "labelGadget4",
                        xmLabelGadgetClass,
                        form4,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "First Row" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNwidth, 130,
                        XmNalignment, XmALIGNMENT_END,
                        NULL );
        UxPutContext( labelGadget4, (char *) UxNtupleBrowserContext );


        /* Creation of firstNtText */
        firstNtText = XtVaCreateManagedWidget( "firstNtText",
                        xmTextWidgetClass,
                        form4,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, labelGadget4,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNmarginHeight, 2,
                        XmNmarginWidth, 2,
                        NULL );
        UxPutContext( firstNtText, (char *) UxNtupleBrowserContext );


        /* Creation of form5 */
        form5 = XtVaCreateManagedWidget( "form5",
                        xmFormWidgetClass,
                        rowColumn2,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 13,
                        XmNy, 13,
                        XmNwidth, 10,
                        XmNheight, 30,
                        NULL );
        UxPutContext( form5, (char *) UxNtupleBrowserContext );


        /* Creation of labelGadget5 */
        labelGadget5 = XtVaCreateManagedWidget( "labelGadget5",
                        xmLabelGadgetClass,
                        form5,
                        RES_CONVERT( XmNlabelString, "Number of Rows" ),
                        XmNwidth, 130,
                        XmNalignment, XmALIGNMENT_END,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( labelGadget5, (char *) UxNtupleBrowserContext );


        /* Creation of nrowNtText */
        nrowNtText = XtVaCreateManagedWidget( "nrowNtText",
                        xmTextWidgetClass,
                        form5,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNmarginHeight, 2,
                        XmNmarginWidth, 2,
                        XmNleftWidget, labelGadget5,
                        XmNleftOffset, 5,
                        NULL );
        UxPutContext( nrowNtText, (char *) UxNtupleBrowserContext );


        /* Creation of form6 */
        form6 = XtVaCreateManagedWidget( "form6",
                        xmFormWidgetClass,
                        rowColumn2,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 13,
                        XmNy, 13,
                        XmNwidth, 10,
                        XmNheight, 30,
                        NULL );
        UxPutContext( form6, (char *) UxNtupleBrowserContext );


        /* Creation of labelGadget6 */
        labelGadget6 = XtVaCreateManagedWidget( "labelGadget6",
                        xmLabelGadgetClass,
                        form6,
                        RES_CONVERT( XmNlabelString, "Histogram ID" ),
                        XmNwidth, 130,
                        XmNalignment, XmALIGNMENT_END,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( labelGadget6, (char *) UxNtupleBrowserContext );


        /* Creation of histoNtText */
        histoNtText = XtVaCreateManagedWidget( "histoNtText",
                        xmTextWidgetClass,
                        form6,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNmarginHeight, 2,
                        XmNmarginWidth, 2,
                        XmNleftWidget, labelGadget6,
                        XmNleftOffset, 5,
                        XmNvalue, "1000000",
                        NULL );
        UxPutContext( histoNtText, (char *) UxNtupleBrowserContext );


        /* Creation of cutEditorNtButton */
        cutEditorNtButton = XtVaCreateManagedWidget( "cutEditorNtButton",
                        xmPushButtonWidgetClass,
                        ntupleBrowser,
                        XmNwidth, 130,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Cut Editor..." ),
                        XmNrightAttachment, XmATTACH_NONE,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 15,
                        XmNtopWidget, rowColumn2,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 40,
                        NULL );
        UxPutContext( cutEditorNtButton, (char *) UxNtupleBrowserContext );


        /* Creation of scanNtButton */
        scanNtButton = XtVaCreateManagedWidget( "scanNtButton",
                        xmPushButtonWidgetClass,
                        ntupleBrowser,
                        XmNwidth, 75,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Scan..." ),
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomWidget, separatorGadget1,
                        XmNbottomOffset, 5,
                        XmNleftPosition, 40,
                        NULL );
        UxPutContext( scanNtButton, (char *) UxNtupleBrowserContext );


        /* Creation of columnsNtLabel */
        columnsNtLabel = XtVaCreateManagedWidget( "columnsNtLabel",
                        xmLabelWidgetClass,
                        ntupleBrowser,
                        XmNheight, 25,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        RES_CONVERT( XmNlabelString, "0 R  0 C" ),
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNalignment, XmALIGNMENT_END,
                        XmNleftPosition, 75,
                        NULL );
        UxPutContext( columnsNtLabel, (char *) UxNtupleBrowserContext );


        /* Creation of titleNtLabel */
        titleNtLabel = XtVaCreateManagedWidget( "titleNtLabel",
                        xmLabelWidgetClass,
                        ntupleBrowser,
                        XmNheight, 25,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightWidget, columnsNtLabel,
                        XmNtopOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( titleNtLabel, (char *) UxNtupleBrowserContext );


        /* Creation of ignoreCutNtToggle */
        ignoreCutNtToggle = XtVaCreateManagedWidget( "ignoreCutNtToggle",
                        xmToggleButtonWidgetClass,
                        ntupleBrowser,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Ignore Cuts" ),
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 15,
                        XmNleftWidget, cutEditorNtButton,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 15,
                        XmNtopWidget, rowColumn2,
                        NULL );
        UxPutContext( ignoreCutNtToggle, (char *) UxNtupleBrowserContext );


        /* Creation of overlayNtToggle */
        overlayNtToggle = XtVaCreateManagedWidget( "overlayNtToggle",
                        xmToggleButtonWidgetClass,
                        ntupleBrowser,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Overlay" ),
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, ignoreCutNtToggle,
                        XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                        XmNleftWidget, ignoreCutNtToggle,
                        XmNtopOffset, 10,
                        NULL );
        UxPutContext( overlayNtToggle, (char *) UxNtupleBrowserContext );


        /* Creation of extendedNtToggle */
        extendedNtToggle = XtVaCreateManagedWidget( "extendedNtToggle",
                        xmToggleButtonWidgetClass,
                        ntupleBrowser,
                        XmNheight, 30,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Extended Info" ),
                        XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                        XmNleftWidget, cutEditorNtButton,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, cutEditorNtButton,
                        XmNtopOffset, 10,
                        NULL );
        UxPutContext( extendedNtToggle, (char *) UxNtupleBrowserContext );


        /* Creation of plotNtButton */
        plotNtButton = XtVaCreateManagedWidget( "plotNtButton",
                        xmPushButtonWidgetClass,
                        ntupleBrowser,
                        XmNwidth, 75,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Plot" ),
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 40,
                        XmNbottomWidget, scanNtButton,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        NULL );
        UxPutContext( plotNtButton, (char *) UxNtupleBrowserContext );


        /* Creation of loopNtButton */
        loopNtButton = XtVaCreateManagedWidget( "loopNtButton",
                        xmPushButtonWidgetClass,
                        ntupleBrowser,
                        XmNwidth, 75,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Loop" ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNbottomWidget, scanNtButton,
                        XmNleftOffset, 10,
                        XmNleftWidget, plotNtButton,
                        NULL );
        UxPutContext( loopNtButton, (char *) UxNtupleBrowserContext );


        /* Creation of projectNtButton */
        projectNtButton = XtVaCreateManagedWidget( "projectNtButton",
                        xmPushButtonWidgetClass,
                        ntupleBrowser,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, " Project " ),
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNbottomWidget, scanNtButton,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 10,
                        XmNleftWidget, loopNtButton,
                        XmNwidth, 75,
                        NULL );
        UxPutContext( projectNtButton, (char *) UxNtupleBrowserContext );


        /* Creation of rebinNtButton */
        rebinNtButton = XtVaCreateManagedWidget( "rebinNtButton",
                        xmPushButtonWidgetClass,
                        ntupleBrowser,
                        XmNwidth, 75,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Rebin..." ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 10,
                        XmNleftWidget, scanNtButton,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 5,
                        XmNbottomWidget, separatorGadget1,
                        NULL );
        UxPutContext( rebinNtButton, (char *) UxNtupleBrowserContext );


        /* Creation of frame1 */
        frame1 = XtVaCreateManagedWidget( "frame1",
                        xmFrameWidgetClass,
                        ntupleBrowser,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 40,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, extendedNtToggle,
                        XmNtopOffset, 5,
                        NULL );
        UxPutContext( frame1, (char *) UxNtupleBrowserContext );


        /* Creation of optionForm */
        optionForm = XtVaCreateManagedWidget( "optionForm",
                        xmFormWidgetClass,
                        frame1,
                        NULL );
        UxPutContext( optionForm, (char *) UxNtupleBrowserContext );


        /* Creation of label1 */
        label1 = XtVaCreateManagedWidget( "label1",
                        xmLabelWidgetClass,
                        optionForm,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "2D Options" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        NULL );
        UxPutContext( label1, (char *) UxNtupleBrowserContext );


        /* Creation of rowColumn3 */
        rowColumn3 = XtVaCreateManagedWidget( "rowColumn3",
                        xmRowColumnWidgetClass,
                        optionForm,
                        XmNnumColumns, 2,
                        XmNradioBehavior, TRUE,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, label1,
                        XmNleftOffset, 10,
                        XmNradioAlwaysOne, FALSE,
                        NULL );
        UxPutContext( rowColumn3, (char *) UxNtupleBrowserContext );


        /* Creation of profileNtToggle */
        profileNtToggle = XtVaCreateManagedWidget( "profileNtToggle",
                        xmToggleButtonWidgetClass,
                        rowColumn3,
                        XmNheight, 41,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Profile" ),
                        XmNx, -10,
                        XmNy, -10,
                        XmNwidth, 96,
                        NULL );
        UxPutContext( profileNtToggle, (char *) UxNtupleBrowserContext );


        /* Creation of boxesNtToggle */
        boxesNtToggle = XtVaCreateManagedWidget( "boxesNtToggle",
                        xmToggleButtonWidgetClass,
                        rowColumn3,
                        XmNheight, 30,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Boxes" ),
                        NULL );
        UxPutContext( boxesNtToggle, (char *) UxNtupleBrowserContext );


        XtAddCallback( ntupleBrowser, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxNtupleBrowserContext);


        return ( ntupleBrowser );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_ntupleBrowser()
{
        Widget                  rtrn;
        _UxCntupleBrowser       *UxContext;

        UxNtupleBrowserContext = UxContext =
      (_UxCntupleBrowser *) UxNewContext( sizeof(_UxCntupleBrowser), False );


        rtrn = _Uxbuild_ntupleBrowser();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

