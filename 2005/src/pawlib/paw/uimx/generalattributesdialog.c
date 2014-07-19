/*
 * $Id: generalattributesdialog.c,v 1.3 1999/01/29 15:59:48 couet Exp $
 *
 * $Log: generalattributesdialog.c,v $
 * Revision 1.3  1999/01/29 15:59:48  couet
 * - Mods to avoid the warning message telling that the file linexxx.bm
 *   and markerxxx.bm are not found
 *
 * Revision 1.2  1999/01/27 13:46:03  couet
 * - clean up: /user/../gravil/.. path removed from the marker and line
 * style bitmaps file names. not used ?
 *
 * Revision 1.1.1.1  1996/03/01 11:39:33  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.06  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        generalAttributesDialog.c
        (Generated from interface file generalAttributesDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Scale.h>
#include <Xm/RowColumn.h>
#include <Xm/SeparatoG.h>
#include <Xm/Form.h>
#include <Xm/PushBG.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>

/*******************************************************************************
        Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern swidget histoStyle;

/*******************************************************************************
        The definition of the context structure:
        If you create multiple instances of your interface, the context
        structure ensures that your callbacks use the variables for the
        correct instance.

        For each swidget in the interface, each argument to the Interface
        function, and each variable in the Instance Specific section of the
        Declarations Editor, there is an entry in the context structure.
        and a #define.  The #define makes the variable name refer to the
        corresponding entry in the context structure.
*******************************************************************************/

typedef struct
{
        Widget  Uxmarkerlabel1;
        Widget  UxEncadreForm5;
        Widget  UxTitre5;
        Widget  UxUpperLeftSep5;
        Widget  UxLeftSep5;
        Widget  UxBottomSep5;
        Widget  UxRightSep5;
        Widget  UxUpperRightSep5;
        Widget  UxMarkerRow;
        Widget  UxEncadreForm6;
        Widget  UxTitre6;
        Widget  UxUpperLeftSep6;
        Widget  UxLeftSep6;
        Widget  UxBottomSep6;
        Widget  UxRightSep6;
        Widget  UxUpperRightSep6;
        Widget  UxLineRow;
        Widget  UxseparatorGadget8;
} _UxCgeneralAttributesDialog;

#define markerlabel1          UxGeneralAttributesDialogContext->Uxmarkerlabel1
#define EncadreForm5          UxGeneralAttributesDialogContext->UxEncadreForm5
#define Titre5                  UxGeneralAttributesDialogContext->UxTitre5
#define UpperLeftSep5        UxGeneralAttributesDialogContext->UxUpperLeftSep5
#define LeftSep5                UxGeneralAttributesDialogContext->UxLeftSep5
#define BottomSep5              UxGeneralAttributesDialogContext->UxBottomSep5
#define RightSep5               UxGeneralAttributesDialogContext->UxRightSep5
#define UpperRightSep5      UxGeneralAttributesDialogContext->UxUpperRightSep5
#define MarkerRow               UxGeneralAttributesDialogContext->UxMarkerRow
#define EncadreForm6          UxGeneralAttributesDialogContext->UxEncadreForm6
#define Titre6                  UxGeneralAttributesDialogContext->UxTitre6
#define UpperLeftSep6        UxGeneralAttributesDialogContext->UxUpperLeftSep6
#define LeftSep6                UxGeneralAttributesDialogContext->UxLeftSep6
#define BottomSep6              UxGeneralAttributesDialogContext->UxBottomSep6
#define RightSep6               UxGeneralAttributesDialogContext->UxRightSep6
#define UpperRightSep6      UxGeneralAttributesDialogContext->UxUpperRightSep6
#define LineRow                 UxGeneralAttributesDialogContext->UxLineRow
#define separatorGadget8  UxGeneralAttributesDialogContext->UxseparatorGadget8

static _UxCgeneralAttributesDialog      *UxGeneralAttributesDialogContext;

Widget  generalAttributesDialog;
Widget  DefineColorButton;
Widget  Marker1;
Widget  Marker2;
Widget  Marker3;
Widget  Marker4;
Widget  Marker5;
Widget  Marker20;
Widget  Marker21;
Widget  Marker22;
Widget  Marker23;
Widget  Marker24;
Widget  Marker25;
Widget  Marker26;
Widget  Marker27;
Widget  Marker28;
Widget  Marker29;
Widget  Marker30;
Widget  Marker31;
Widget  MarkerMenu;
Widget  MarkerSizeScale;
Widget  Line1;
Widget  Line2;
Widget  Line3;
Widget  Line4;
Widget  LineMenu;
Widget  ApplyGeneralBut;
Widget  ResetGeneralBut;
Widget  CloseGeneralBut;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_generalAttributesDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_generalAttributesDialog()
{
        Widget  generalAttributesDialog_shell;
        Widget  MarkerRow_shell;
        Widget  LineRow_shell;

generalAttributesDialog_shell =
                        XtVaCreatePopupShell( "generalAttributesDialog_shell",
                        xmDialogShellWidgetClass, histoStyle,
                        XmNx, 206,
                        XmNy, 186,
                        XmNwidth, 548,
                        XmNheight, 185,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "generalAttributesDialog",
                        NULL );

        generalAttributesDialog = XtVaCreateWidget( "generalAttributesDialog",
                        xmFormWidgetClass, generalAttributesDialog_shell,
                        XmNnoResize, TRUE,
                        RES_CONVERT( XmNdialogTitle, "General Attributes" ),
                        XmNautoUnmanage, FALSE,
                        XmNheight, 185,
                        XmNwidth, 548,
                        XmNunitType, XmPIXELS,
                        NULL );

UxPutContext( generalAttributesDialog,
                     (char *) UxGeneralAttributesDialogContext );

        markerlabel1 = XtVaCreateManagedWidget( "markerlabel1",
                        xmLabelGadgetClass, generalAttributesDialog,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "Marker :" ),
                        XmNheight, 22,
                        XmNwidth, 75,
                        XmNy, 29,
                        XmNx, 18,
                        NULL );

UxPutContext( markerlabel1, (char *) UxGeneralAttributesDialogContext );

DefineColorButton = XtVaCreateManagedWidget( "DefineColorButton",
                        xmPushButtonGadgetClass, generalAttributesDialog,
                        RES_CONVERT( XmNlabelString, "Define Color..." ),
                        XmNheight, 30,
                        XmNwidth, 124,
                        XmNy, 81,
                        XmNx, 392,
                        NULL );

UxPutContext( DefineColorButton, (char *) UxGeneralAttributesDialogContext );

        EncadreForm5 = XtVaCreateManagedWidget( "EncadreForm5",
                        xmFormWidgetClass, generalAttributesDialog,
                        XmNheight, 95,
                        XmNwidth, 357,
                        XmNy, 17,
                        XmNx, 18,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

UxPutContext( EncadreForm5, (char *) UxGeneralAttributesDialogContext );

        Titre5 = XtVaCreateManagedWidget( "Titre5",
                        xmLabelGadgetClass, EncadreForm5,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNmarginWidth, 0,
                        XmNrecomputeSize, TRUE,
                        RES_CONVERT( XmNlabelString, "Marker" ),
                        XmNheight, 20,
                        XmNwidth, 60,
                        XmNy, 0,
                        XmNx, 10,
                        NULL );

        UxPutContext( Titre5, (char *) UxGeneralAttributesDialogContext );

        UpperLeftSep5 = XtVaCreateManagedWidget( "UpperLeftSep5",
                        xmSeparatorGadgetClass, EncadreForm5,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, Titre5,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 2,
                        XmNwidth, 99,
                        XmNy, 200,
                        XmNx, 87,
                        NULL );

UxPutContext( UpperLeftSep5, (char *) UxGeneralAttributesDialogContext );

        LeftSep5 = XtVaCreateManagedWidget( "LeftSep5",
                        xmSeparatorGadgetClass, EncadreForm5,
                        XmNtopOffset, 11,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNorientation, XmVERTICAL,
                        XmNheight, 116,
                        XmNwidth, 2,
                        XmNy, 201,
                        XmNx, 84,
                        NULL );

        UxPutContext( LeftSep5, (char *) UxGeneralAttributesDialogContext );

        BottomSep5 = XtVaCreateManagedWidget( "BottomSep5",
                        xmSeparatorGadgetClass, EncadreForm5,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 2,
                        XmNwidth, 292,
                        XmNy, 315,
                        XmNx, 86,
                        NULL );

        UxPutContext( BottomSep5, (char *) UxGeneralAttributesDialogContext );

        RightSep5 = XtVaCreateManagedWidget( "RightSep5",
                        xmSeparatorGadgetClass, EncadreForm5,
                        XmNtopOffset, 11,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_NONE,
                        XmNbottomOffset, 1,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNorientation, XmVERTICAL,
                        XmNheight, 116,
                        XmNwidth, 2,
                        XmNy, 201,
                        XmNx, 375,
                        NULL );

        UxPutContext( RightSep5, (char *) UxGeneralAttributesDialogContext );

        UpperRightSep5 = XtVaCreateManagedWidget( "UpperRightSep5",
                        xmSeparatorGadgetClass, EncadreForm5,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftWidget, Titre5,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNheight, 2,
                        XmNwidth, 106,
                        XmNy, 200,
                        XmNx, 274,
                        NULL );

UxPutContext( UpperRightSep5, (char *) UxGeneralAttributesDialogContext );

        MarkerRow_shell = XtVaCreatePopupShell ("MarkerRow_shell",
                        xmMenuShellWidgetClass, EncadreForm5,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        MarkerRow = XtVaCreateWidget( "MarkerRow",
                        xmRowColumnWidgetClass, MarkerRow_shell,
                        XmNresizeWidth, TRUE,
                        XmNresizeHeight, TRUE,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );

        UxPutContext( MarkerRow, (char *) UxGeneralAttributesDialogContext );

        Marker1 = XtVaCreateManagedWidget( "Marker1",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker1, (char *) UxGeneralAttributesDialogContext );

        Marker2 = XtVaCreateManagedWidget( "Marker2",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker2, (char *) UxGeneralAttributesDialogContext );

        Marker3 = XtVaCreateManagedWidget( "Marker3",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker3, (char *) UxGeneralAttributesDialogContext );

        Marker4 = XtVaCreateManagedWidget( "Marker4",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker4, (char *) UxGeneralAttributesDialogContext );

        Marker5 = XtVaCreateManagedWidget( "Marker5",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker5, (char *) UxGeneralAttributesDialogContext );

        Marker20 = XtVaCreateManagedWidget( "Marker20",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker20, (char *) UxGeneralAttributesDialogContext );

        Marker21 = XtVaCreateManagedWidget( "Marker21",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker21, (char *) UxGeneralAttributesDialogContext );

        Marker22 = XtVaCreateManagedWidget( "Marker22",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker22, (char *) UxGeneralAttributesDialogContext );

        Marker23 = XtVaCreateManagedWidget( "Marker23",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker23, (char *) UxGeneralAttributesDialogContext );

        Marker24 = XtVaCreateManagedWidget( "Marker24",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker24, (char *) UxGeneralAttributesDialogContext );

        Marker25 = XtVaCreateManagedWidget( "Marker25",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker25, (char *) UxGeneralAttributesDialogContext );

        Marker26 = XtVaCreateManagedWidget( "Marker26",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker26, (char *) UxGeneralAttributesDialogContext );

        Marker27 = XtVaCreateManagedWidget( "Marker27",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker27, (char *) UxGeneralAttributesDialogContext );

        Marker28 = XtVaCreateManagedWidget( "Marker28",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker28, (char *) UxGeneralAttributesDialogContext );

        Marker29 = XtVaCreateManagedWidget( "Marker29",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker29, (char *) UxGeneralAttributesDialogContext );

        Marker30 = XtVaCreateManagedWidget( "Marker30",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker30, (char *) UxGeneralAttributesDialogContext );

        Marker31 = XtVaCreateManagedWidget( "Marker31",
                        xmPushButtonGadgetClass, MarkerRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Marker31, (char *) UxGeneralAttributesDialogContext );

        MarkerMenu = XtVaCreateManagedWidget( "MarkerMenu",
                        xmRowColumnWidgetClass, EncadreForm5,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNresizeWidth, TRUE,
                        XmNresizeHeight, TRUE,
                        XmNy, 29,
                        XmNx, 10,
                        RES_CONVERT( XmNmnemonic, "M" ),
                        XmNsubMenuId, MarkerRow,
                        XmNrowColumnType, XmMENU_OPTION,
                        NULL );

        UxPutContext( MarkerMenu, (char *) UxGeneralAttributesDialogContext );

        MarkerSizeScale = XtVaCreateManagedWidget( "MarkerSizeScale",
                        xmScaleWidgetClass, EncadreForm5,
                        XmNvalue, 10,
                        XmNdecimalPoints, 1,
                        RES_CONVERT( XmNtitleString, "Marker Size" ),
                        XmNshowValue, TRUE,
                        XmNorientation, XmHORIZONTAL,
                        XmNheight, 69,
                        XmNwidth, 243,
                        XmNy, 14,
                        XmNx, 102,
                        NULL );

UxPutContext( MarkerSizeScale, (char *) UxGeneralAttributesDialogContext );

        EncadreForm6 = XtVaCreateManagedWidget( "EncadreForm6",
                        xmFormWidgetClass, generalAttributesDialog,
                        XmNheight, 66,
                        XmNwidth, 98,
                        XmNy, 9,
                        XmNx, 403,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( EncadreForm6, (char *) UxGeneralAttributesDialogContext );

        Titre6 = XtVaCreateManagedWidget( "Titre6",
                        xmLabelGadgetClass, EncadreForm6,
                        XmNtopOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNmarginWidth, 0,
                        XmNrecomputeSize, TRUE,
                        RES_CONVERT( XmNlabelString, "Line" ),
                        XmNheight, 20,
                        XmNwidth, 48,
                        XmNy, 0,
                        XmNx, 10,
                        NULL );

        UxPutContext( Titre6, (char *) UxGeneralAttributesDialogContext );

        UpperLeftSep6 = XtVaCreateManagedWidget( "UpperLeftSep6",
                        xmSeparatorGadgetClass, EncadreForm6,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightWidget, Titre6,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 2,
                        XmNwidth, 99,
                        XmNy, 200,
                        XmNx, 87,
                        NULL );

UxPutContext( UpperLeftSep6, (char *) UxGeneralAttributesDialogContext );

        LeftSep6 = XtVaCreateManagedWidget( "LeftSep6",
                        xmSeparatorGadgetClass, EncadreForm6,
                        XmNtopOffset, 11,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNorientation, XmVERTICAL,
                        XmNheight, 116,
                        XmNwidth, 2,
                        XmNy, 201,
                        XmNx, 84,
                        NULL );

        UxPutContext( LeftSep6, (char *) UxGeneralAttributesDialogContext );

        BottomSep6 = XtVaCreateManagedWidget( "BottomSep6",
                        xmSeparatorGadgetClass, EncadreForm6,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 2,
                        XmNwidth, 292,
                        XmNy, 315,
                        XmNx, 86,
                        NULL );

        UxPutContext( BottomSep6, (char *) UxGeneralAttributesDialogContext );

        RightSep6 = XtVaCreateManagedWidget( "RightSep6",
                        xmSeparatorGadgetClass, EncadreForm6,
                        XmNtopOffset, 11,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_NONE,
                        XmNbottomOffset, 1,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNorientation, XmVERTICAL,
                        XmNheight, 116,
                        XmNwidth, 2,
                        XmNy, 201,
                        XmNx, 375,
                        NULL );

        UxPutContext( RightSep6, (char *) UxGeneralAttributesDialogContext );

        UpperRightSep6 = XtVaCreateManagedWidget( "UpperRightSep6",
                        xmSeparatorGadgetClass, EncadreForm6,
                        XmNtopOffset, 10,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftWidget, Titre6,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNheight, 2,
                        XmNwidth, 106,
                        XmNy, 200,
                        XmNx, 274,
                        NULL );

UxPutContext( UpperRightSep6, (char *) UxGeneralAttributesDialogContext );

        LineRow_shell = XtVaCreatePopupShell ("LineRow_shell",
                        xmMenuShellWidgetClass, EncadreForm6,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        LineRow = XtVaCreateWidget( "LineRow",
                        xmRowColumnWidgetClass, LineRow_shell,
                        XmNresizeHeight, FALSE,
                        XmNheight, 90,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );

        UxPutContext( LineRow, (char *) UxGeneralAttributesDialogContext );

        Line1 = XtVaCreateManagedWidget( "Line1",
                        xmPushButtonGadgetClass, LineRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Line1, (char *) UxGeneralAttributesDialogContext );

        Line2 = XtVaCreateManagedWidget( "Line2",
                        xmPushButtonGadgetClass, LineRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Line2, (char *) UxGeneralAttributesDialogContext );

        Line3 = XtVaCreateManagedWidget( "Line3",
                        xmPushButtonGadgetClass, LineRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Line3, (char *) UxGeneralAttributesDialogContext );

        Line4 = XtVaCreateManagedWidget( "Line4",
                        xmPushButtonGadgetClass, LineRow,
                        RES_CONVERT( XmNlabelString, "pushButtonGadget" ),
                        NULL );

        UxPutContext( Line4, (char *) UxGeneralAttributesDialogContext );

        LineMenu = XtVaCreateManagedWidget( "LineMenu",
                        xmRowColumnWidgetClass, EncadreForm6,
                        RES_CONVERT( XmNlabelString, "" ),
                        XmNrightOffset, 7,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopOffset, 20,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 50,
                        XmNresizeHeight, FALSE,
                        XmNy, 18,
                        XmNx, 4,
                        XmNsubMenuId, LineRow,
                        XmNrowColumnType, XmMENU_OPTION,
                        NULL );

        UxPutContext( LineMenu, (char *) UxGeneralAttributesDialogContext );

        separatorGadget8 = XtVaCreateManagedWidget( "separatorGadget8",
                        xmSeparatorGadgetClass, generalAttributesDialog,
                        XmNtopAttachment, XmATTACH_NONE,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 50,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 20,
                        XmNwidth, 1220,
                        XmNy, 280,
                        XmNx, 0,
                        NULL );

UxPutContext( separatorGadget8, (char *) UxGeneralAttributesDialogContext );

        ApplyGeneralBut = XtVaCreateManagedWidget( "ApplyGeneralBut",
                        xmPushButtonGadgetClass, generalAttributesDialog,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Apply" ),
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 352,
                        XmNx, 23,
                        NULL );

UxPutContext( ApplyGeneralBut, (char *) UxGeneralAttributesDialogContext );

        ResetGeneralBut = XtVaCreateManagedWidget( "ResetGeneralBut",
                        xmPushButtonGadgetClass, generalAttributesDialog,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 355,
                        XmNx, 239,
                        NULL );

UxPutContext( ResetGeneralBut, (char *) UxGeneralAttributesDialogContext );

        CloseGeneralBut = XtVaCreateManagedWidget( "CloseGeneralBut",
                        xmPushButtonGadgetClass, generalAttributesDialog,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNheight, 30,
                        XmNwidth, 70,
                        XmNy, 355,
                        XmNx, 435,
                        NULL );

UxPutContext( CloseGeneralBut, (char *) UxGeneralAttributesDialogContext );

        XtAddCallback( generalAttributesDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxGeneralAttributesDialogContext );

        XtVaSetValues(generalAttributesDialog,
                        XmNdefaultButton, ApplyGeneralBut,
                        NULL );

        XtVaSetValues(Marker1,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker2,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker3,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker4,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker5,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker20,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker21,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker22,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker23,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker24,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker25,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker26,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker27,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker28,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker29,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker30,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Marker31,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Line1,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Line2,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Line3,
                        XmNlabelType, XmPIXMAP,
                        NULL );

        XtVaSetValues(Line4,
                        XmNlabelType, XmPIXMAP,
                        NULL );



        return ( generalAttributesDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_generalAttributesDialog()
{
        Widget                  rtrn;
        _UxCgeneralAttributesDialog *UxContext;

        UxGeneralAttributesDialogContext = UxContext =
(_UxCgeneralAttributesDialog *) XtMalloc( sizeof(_UxCgeneralAttributesDialog) );

        rtrn = _Uxbuild_generalAttributesDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_generalAttributesDialog()
{
        Widget                  _Uxrtrn;

        _Uxrtrn = _Ux_create_generalAttributesDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

