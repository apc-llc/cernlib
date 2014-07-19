/*
 * $Id: histostyle.c,v 1.1.1.1 1996/03/01 11:39:33 mclareni Exp $
 *
 * $Log: histostyle.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:33  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 09/03/94  18.29.27  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
       histoStyle.c
       (Generated from interface file histoStyle.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <Xm/Label.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
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
        Widget  Uxframe3;
        Widget  UxseparatorGadget2;
        Widget  UxseparatorGadget6;
        Widget  UxEncadreForm1;
        Widget  UxTitre1;
        Widget  UxUpperLeftSep1;
        Widget  UxLeftSep1;
        Widget  UxBottomSep1;
        Widget  UxRightSep1;
        Widget  UxUpperRightSep1;
        Widget  UxrowColumn5;
        Widget  UxEncadreForm2;
        Widget  UxTitre2;
        Widget  UxUpperLeftSep2;
        Widget  UxLeftSep2;
        Widget  UxBottomSep2;
        Widget  UxRightSep2;
        Widget  UxUpperRightSep2;
        Widget  Uxform4;
        Widget  UxOptionPlot_p1;
        Widget  Uxform1;
        Widget  UxCoordinateSystem_p1;
        Widget  Uxmenu1;
        Widget  Uxmenu1_p1;
        Widget  Uxmenu1_top_b1;
        Widget  Uxmenu1_p2;
        Widget  Uxmenu1_top_b2;
} _UxChistoStyle;

static _UxChistoStyle          *UxHistoStyleContext;
#define frame3                  UxHistoStyleContext->Uxframe3
#define separatorGadget2        UxHistoStyleContext->UxseparatorGadget2
#define separatorGadget6        UxHistoStyleContext->UxseparatorGadget6
#define EncadreForm1            UxHistoStyleContext->UxEncadreForm1
#define Titre1                  UxHistoStyleContext->UxTitre1
#define UpperLeftSep1           UxHistoStyleContext->UxUpperLeftSep1
#define LeftSep1                UxHistoStyleContext->UxLeftSep1
#define BottomSep1              UxHistoStyleContext->UxBottomSep1
#define RightSep1               UxHistoStyleContext->UxRightSep1
#define UpperRightSep1          UxHistoStyleContext->UxUpperRightSep1
#define rowColumn5              UxHistoStyleContext->UxrowColumn5
#define EncadreForm2            UxHistoStyleContext->UxEncadreForm2
#define Titre2                  UxHistoStyleContext->UxTitre2
#define UpperLeftSep2           UxHistoStyleContext->UxUpperLeftSep2
#define LeftSep2                UxHistoStyleContext->UxLeftSep2
#define BottomSep2              UxHistoStyleContext->UxBottomSep2
#define RightSep2               UxHistoStyleContext->UxRightSep2
#define UpperRightSep2          UxHistoStyleContext->UxUpperRightSep2
#define form4                   UxHistoStyleContext->Uxform4
#define OptionPlot_p1           UxHistoStyleContext->UxOptionPlot_p1
#define form1                   UxHistoStyleContext->Uxform1
#define CoordinateSystem_p1     UxHistoStyleContext->UxCoordinateSystem_p1
#define menu1                   UxHistoStyleContext->Uxmenu1
#define menu1_p1                UxHistoStyleContext->Uxmenu1_p1
#define menu1_top_b1            UxHistoStyleContext->Uxmenu1_top_b1
#define menu1_p2                UxHistoStyleContext->Uxmenu1_p2
#define menu1_top_b2            UxHistoStyleContext->Uxmenu1_top_b2


Widget  histoStyle;
Widget  histoStyleForm;
Widget  CloseHistoPanel;
Widget  StyleName;
Widget  HistoName;
Widget  PlotHistoPanel;
Widget  ResetHistoPanel;
Widget  StatToggleButtonGadget;
Widget  FitToggleButtonGadget;
Widget  FileToggleButtonGadget;
Widget  DateToggleButtonGadget;
Widget  AttrButton;
Widget  GeneralAttributesBut;
Widget  Angles;
Widget  AxisRanges;
Widget  GeometryButton;
Widget  AxisSettings;
Widget  ZoneButton;
Widget  FontButton;
Widget  OptionPlotD;
Widget  OptionPlotL;
Widget  OptionPlotC;
Widget  OptionPlotB;
Widget  OptionPlotP;
Widget  OptionPlotSTAR;
Widget  OptionPlotE;
Widget  OptionPlotE1;
Widget  OptionPlotE2;
Widget  OptionPlotE3;
Widget  OptionPlotE4;
Widget  OptionPlotBOX;
Widget  OptionPlotCOL;
Widget  OptionPlotSURF;
Widget  OptionPlotSURF1;
Widget  OptionPlotSURF2;
Widget  OptionPlotSURF3;
Widget  OptionPlotSURF4;
Widget  OptionPlotLEGO;
Widget  OptionPlotLEGO1;
Widget  OptionPlotLEGO2;
Widget  OptionPlotCONT;
Widget  OptionPlotCONT3;
Widget  OptionPlotTEXT;
Widget  OptionPlot;
Widget  CoordinateSystemC;
Widget  CoordinateSystemP;
Widget  CoordinateSystemR;
Widget  CoordinateSystemS;
Widget  CoordinateSystemD;
Widget  CoordinateSystem;
Widget  OpenstylePushButton;
Widget  SavestylePushButton;
Widget  SavestyleasPushButton;
Widget  ClosePushButton;
Widget  RefreshToggleButtonGadget;
Widget  SameToggleButtonGadget;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_histoStyle( void );

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_histoStyle()
{
        Widget          _UxParent;
        Widget          OptionPlot_p1_shell;
        Widget          CoordinateSystem_p1_shell;
        Widget          menu1_p1_shell;
        Widget          menu1_p2_shell;


        /* Creation of histoStyle */
        _UxParent = XtVaCreatePopupShell( "histoStyle_shell",
                        topLevelShellWidgetClass, UxTopLevel,
                        XmNx, 270,
                        XmNy, 365,
                        XmNwidth, 550,
                        XmNheight, 345,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "histoStyle",
                        XmNiconName, "histoStyle",
                        NULL );

        histoStyle = XtVaCreateManagedWidget( "histoStyle",
                        xmMainWindowWidgetClass,
                        _UxParent,
                        XmNunitType, XmPIXELS,
                        XmNwidth, 550,
                        XmNheight, 345,
                        NULL );
        UxPutContext( histoStyle, (char *) UxHistoStyleContext );


        /* Creation of frame3 */
        frame3 = XtVaCreateManagedWidget( "frame3",
                        xmFrameWidgetClass,
                        histoStyle,
                        XmNheight, 310,
                        NULL );
        UxPutContext( frame3, (char *) UxHistoStyleContext );


        /* Creation of histoStyleForm */
        histoStyleForm = XtVaCreateManagedWidget( "histoStyleForm",
                        xmFormWidgetClass,
                        frame3,
                        XmNwidth, 546,
                        XmNheight, 305,
                        RES_CONVERT( XmNdialogTitle, "Histogram Style Panel" ),
                        XmNnoResize, FALSE,
                        XmNx, 2,
                        XmNy, 2,
                        NULL );
        UxPutContext( histoStyleForm, (char *) UxHistoStyleContext );


        /* Creation of separatorGadget2 */
        separatorGadget2 = XtVaCreateManagedWidget( "separatorGadget2",
                        xmSeparatorGadgetClass,
                        histoStyleForm,
                        XmNx, 0,
                        XmNy, 300,
                        XmNwidth, 560,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 50,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, -5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        NULL );
        UxPutContext( separatorGadget2, (char *) UxHistoStyleContext );


        /* Creation of CloseHistoPanel */
        CloseHistoPanel = XtVaCreateManagedWidget( "CloseHistoPanel",
                        xmPushButtonGadgetClass,
                        histoStyleForm,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, " Close " ),
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 10,
                        XmNwidth, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 10,
                        XmNtopWidget, separatorGadget2,
                        NULL );
        UxPutContext( CloseHistoPanel, (char *) UxHistoStyleContext );


        /* Creation of StyleName */
        StyleName = XtVaCreateManagedWidget( "StyleName",
                        xmLabelWidgetClass,
                        histoStyleForm,
                        XmNx, 20,
                        XmNy, 10,
                        XmNheight, 20,
                        RES_CONVERT( XmNlabelString, "Current Style: Default" ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        NULL );
        UxPutContext( StyleName, (char *) UxHistoStyleContext );


        /* Creation of separatorGadget6 */
        separatorGadget6 = XtVaCreateManagedWidget( "separatorGadget6",
                        xmSeparatorGadgetClass,
                        histoStyleForm,
                        XmNx, 5,
                        XmNy, 414,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 0,
                        XmNtopWidget, StyleName,
                        NULL );
        UxPutContext( separatorGadget6, (char *) UxHistoStyleContext );


        /* Creation of HistoName */
        HistoName = XtVaCreateManagedWidget( "HistoName",
                        xmLabelWidgetClass,
                        histoStyleForm,
                        XmNwidth, 150,
                        XmNalignment, XmALIGNMENT_END,
                        RES_CONVERT( XmNlabelString, " " ),
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 2,
                        XmNbottomWidget, separatorGadget6,
                        NULL );
        UxPutContext( HistoName, (char *) UxHistoStyleContext );


        /* Creation of PlotHistoPanel */
        PlotHistoPanel = XtVaCreateManagedWidget( "PlotHistoPanel",
                        xmPushButtonGadgetClass,
                        histoStyleForm,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, " Plot " ),
                        XmNwidth, 100,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 10,
                        XmNtopWidget, separatorGadget2,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        NULL );
        UxPutContext( PlotHistoPanel, (char *) UxHistoStyleContext );


        /* Creation of ResetHistoPanel */
        ResetHistoPanel = XtVaCreateManagedWidget( "ResetHistoPanel",
                        xmPushButtonGadgetClass,
                        histoStyleForm,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, " Reset " ),
                        XmNwidth, 100,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftOffset, -50,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 10,
                        XmNtopWidget, separatorGadget2,
                        XmNleftPosition, 50,
                        NULL );
        UxPutContext( ResetHistoPanel, (char *) UxHistoStyleContext );


        /* Creation of EncadreForm1 */
        EncadreForm1 = XtVaCreateManagedWidget( "EncadreForm1",
                        xmFormWidgetClass,
                        histoStyleForm,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 10,
                        XmNy, 40,
                        XmNwidth, 140,
                        XmNheight, 150,
                        NULL );
        UxPutContext( EncadreForm1, (char *) UxHistoStyleContext );


        /* Creation of Titre1 */
        Titre1 = XtVaCreateManagedWidget( "Titre1",
                        xmLabelGadgetClass,
                        EncadreForm1,
                        XmNx, 10,
                        XmNy, 0,
                        XmNwidth, 80,
                        XmNheight, 20,
                        RES_CONVERT( XmNlabelString, " Plot Info " ),
                        XmNrecomputeSize, TRUE,
                        XmNmarginWidth, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        NULL );
        UxPutContext( Titre1, (char *) UxHistoStyleContext );


        /* Creation of UpperLeftSep1 */
        UpperLeftSep1 = XtVaCreateManagedWidget( "UpperLeftSep1",
                        xmSeparatorGadgetClass,
                        EncadreForm1,
                        XmNx, 87,
                        XmNy, 200,
                        XmNwidth, 99,
                        XmNheight, 2,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightWidget, Titre1,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 10,
                        NULL );
        UxPutContext( UpperLeftSep1, (char *) UxHistoStyleContext );


        /* Creation of LeftSep1 */
        LeftSep1 = XtVaCreateManagedWidget( "LeftSep1",
                        xmSeparatorGadgetClass,
                        EncadreForm1,
                        XmNx, 84,
                        XmNy, 201,
                        XmNwidth, 2,
                        XmNheight, 116,
                        XmNorientation, XmVERTICAL,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 11,
                        NULL );
        UxPutContext( LeftSep1, (char *) UxHistoStyleContext );


        /* Creation of BottomSep1 */
        BottomSep1 = XtVaCreateManagedWidget( "BottomSep1",
                        xmSeparatorGadgetClass,
                        EncadreForm1,
                        XmNx, 86,
                        XmNy, 315,
                        XmNwidth, 292,
                        XmNheight, 2,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        NULL );
        UxPutContext( BottomSep1, (char *) UxHistoStyleContext );


        /* Creation of RightSep1 */
        RightSep1 = XtVaCreateManagedWidget( "RightSep1",
                        xmSeparatorGadgetClass,
                        EncadreForm1,
                        XmNx, 375,
                        XmNy, 201,
                        XmNwidth, 2,
                        XmNheight, 116,
                        XmNorientation, XmVERTICAL,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 1,
                        XmNleftAttachment, XmATTACH_NONE,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 11,
                        NULL );
        UxPutContext( RightSep1, (char *) UxHistoStyleContext );


        /* Creation of UpperRightSep1 */
        UpperRightSep1 = XtVaCreateManagedWidget( "UpperRightSep1",
                        xmSeparatorGadgetClass,
                        EncadreForm1,
                        XmNx, 274,
                        XmNy, 200,
                        XmNwidth, 106,
                        XmNheight, 2,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftWidget, Titre1,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 10,
                        NULL );
        UxPutContext( UpperRightSep1, (char *) UxHistoStyleContext );


        /* Creation of rowColumn5 */
        rowColumn5 = XtVaCreateManagedWidget( "rowColumn5",
                        xmRowColumnWidgetClass,
                        EncadreForm1,
                        XmNx, 5,
                        XmNy, 17,
                        XmNspacing, 0,
                        XmNheight, 203,
                        XmNresizeHeight, TRUE,
                        XmNresizeWidth, TRUE,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 17,
                        XmNwidth, 135,
                        NULL );
        UxPutContext( rowColumn5, (char *) UxHistoStyleContext );


        /* Creation of StatToggleButtonGadget */
   StatToggleButtonGadget = XtVaCreateManagedWidget( "StatToggleButtonGadget",
                        xmToggleButtonGadgetClass,
                        rowColumn5,
                        XmNx, 0,
                        XmNy, 5,
                        XmNwidth, 141,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Statistics..." ),
                        XmNrecomputeSize, FALSE,
                        NULL );
        UxPutContext( StatToggleButtonGadget, (char *) UxHistoStyleContext );


        /* Creation of FitToggleButtonGadget */
    FitToggleButtonGadget = XtVaCreateManagedWidget( "FitToggleButtonGadget",
                        xmToggleButtonGadgetClass,
                        rowColumn5,
                        XmNx, 3,
                        XmNy, 33,
                        XmNwidth, 141,
                        XmNheight, 32,
                        RES_CONVERT( XmNlabelString, "Fits..." ),
                        NULL );
        UxPutContext( FitToggleButtonGadget, (char *) UxHistoStyleContext );


        /* Creation of FileToggleButtonGadget */
   FileToggleButtonGadget = XtVaCreateManagedWidget( "FileToggleButtonGadget",
                        xmToggleButtonGadgetClass,
                        rowColumn5,
                        XmNx, 3,
                        XmNy, 60,
                        XmNwidth, 117,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "File Name..." ),
                        NULL );
        UxPutContext( FileToggleButtonGadget, (char *) UxHistoStyleContext );


        /* Creation of DateToggleButtonGadget */
  DateToggleButtonGadget = XtVaCreateManagedWidget( "DateToggleButtonGadget",
                        xmToggleButtonGadgetClass,
                        rowColumn5,
                        XmNx, 13,
                        XmNy, 75,
                        XmNwidth, 120,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Date" ),
                        NULL );
        UxPutContext( DateToggleButtonGadget, (char *) UxHistoStyleContext );


        /* Creation of EncadreForm2 */
        EncadreForm2 = XtVaCreateManagedWidget( "EncadreForm2",
                        xmFormWidgetClass,
                        histoStyleForm,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNheight, 152,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 10,
                        XmNleftWidget, EncadreForm1,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 40,
                        XmNwidth, 380,
                        XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                        XmNbottomOffset, 0,
                        XmNbottomWidget, EncadreForm1,
                        NULL );
        UxPutContext( EncadreForm2, (char *) UxHistoStyleContext );


        /* Creation of Titre2 */
        Titre2 = XtVaCreateManagedWidget( "Titre2",
                        xmLabelGadgetClass,
                        EncadreForm2,
                        XmNx, 10,
                        XmNy, 0,
                        XmNwidth, 48,
                        XmNheight, 20,
                        RES_CONVERT( XmNlabelString, " Style " ),
                        XmNrecomputeSize, TRUE,
                        XmNmarginWidth, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        NULL );
        UxPutContext( Titre2, (char *) UxHistoStyleContext );


        /* Creation of UpperLeftSep2 */
        UpperLeftSep2 = XtVaCreateManagedWidget( "UpperLeftSep2",
                        xmSeparatorGadgetClass,
                        EncadreForm2,
                        XmNx, 87,
                        XmNy, 200,
                        XmNwidth, 99,
                        XmNheight, 2,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightWidget, Titre2,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 10,
                        NULL );
        UxPutContext( UpperLeftSep2, (char *) UxHistoStyleContext );


        /* Creation of LeftSep2 */
        LeftSep2 = XtVaCreateManagedWidget( "LeftSep2",
                        xmSeparatorGadgetClass,
                        EncadreForm2,
                        XmNx, 84,
                        XmNy, 201,
                        XmNwidth, 2,
                        XmNheight, 116,
                        XmNorientation, XmVERTICAL,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 11,
                        NULL );
        UxPutContext( LeftSep2, (char *) UxHistoStyleContext );


        /* Creation of BottomSep2 */
        BottomSep2 = XtVaCreateManagedWidget( "BottomSep2",
                        xmSeparatorGadgetClass,
                        EncadreForm2,
                        XmNx, 86,
                        XmNy, 315,
                        XmNwidth, 292,
                        XmNheight, 2,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        NULL );
        UxPutContext( BottomSep2, (char *) UxHistoStyleContext );


        /* Creation of RightSep2 */
        RightSep2 = XtVaCreateManagedWidget( "RightSep2",
                        xmSeparatorGadgetClass,
                        EncadreForm2,
                        XmNx, 375,
                        XmNy, 201,
                        XmNwidth, 2,
                        XmNheight, 116,
                        XmNorientation, XmVERTICAL,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 1,
                        XmNleftAttachment, XmATTACH_NONE,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 11,
                        NULL );
        UxPutContext( RightSep2, (char *) UxHistoStyleContext );


        /* Creation of UpperRightSep2 */
        UpperRightSep2 = XtVaCreateManagedWidget( "UpperRightSep2",
                        xmSeparatorGadgetClass,
                        EncadreForm2,
                        XmNx, 274,
                        XmNy, 200,
                        XmNwidth, 106,
                        XmNheight, 2,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftWidget, Titre2,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 10,
                        NULL );
        UxPutContext( UpperRightSep2, (char *) UxHistoStyleContext );


        /* Creation of AttrButton */
        AttrButton = XtVaCreateManagedWidget( "AttrButton",
                        xmPushButtonGadgetClass,
                        EncadreForm2,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Object Attributes..." ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 28,
                        XmNwidth, 140,
                        NULL );
        UxPutContext( AttrButton, (char *) UxHistoStyleContext );


        /* Creation of GeneralAttributesBut */
        GeneralAttributesBut = XtVaCreateManagedWidget( "GeneralAttributesBut",
                        xmPushButtonGadgetClass,
                        EncadreForm2,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "General Attributes..." ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 7,
                        XmNleftWidget, AttrButton,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 28,
                        XmNwidth, 140,
                        NULL );
        UxPutContext( GeneralAttributesBut, (char *) UxHistoStyleContext );


        /* Creation of Angles */
        Angles = XtVaCreateManagedWidget( "Angles",
                        xmPushButtonGadgetClass,
                        EncadreForm2,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, " Viewing Angles... " ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 7,
                        XmNtopWidget, AttrButton,
                        XmNwidth, 140,
                        NULL );
        UxPutContext( Angles, (char *) UxHistoStyleContext );


        /* Creation of AxisRanges */
        AxisRanges = XtVaCreateManagedWidget( "AxisRanges",
                        xmPushButtonGadgetClass,
                        EncadreForm2,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, " Axis Scaling... " ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 7,
                        XmNtopWidget, Angles,
                        XmNwidth, 140,
                        NULL );
        UxPutContext( AxisRanges, (char *) UxHistoStyleContext );


        /* Creation of GeometryButton */
        GeometryButton = XtVaCreateManagedWidget( "GeometryButton",
                        xmPushButtonGadgetClass,
                        EncadreForm2,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Geometry..." ),
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 7,
                        XmNleftWidget, Angles,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 7,
                        XmNtopWidget, GeneralAttributesBut,
                        XmNwidth, 140,
                        NULL );
        UxPutContext( GeometryButton, (char *) UxHistoStyleContext );


        /* Creation of AxisSettings */
        AxisSettings = XtVaCreateManagedWidget( "AxisSettings",
                        xmPushButtonGadgetClass,
                        EncadreForm2,
                        RES_CONVERT( XmNlabelString, " Axis Settings... " ),
                        XmNheight, 30,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 7,
                        XmNleftWidget, AxisRanges,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 7,
                        XmNtopWidget, GeometryButton,
                        XmNwidth, 140,
                        NULL );
        UxPutContext( AxisSettings, (char *) UxHistoStyleContext );


        /* Creation of ZoneButton */
        ZoneButton = XtVaCreateManagedWidget( "ZoneButton",
                        xmPushButtonGadgetClass,
                        EncadreForm2,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Zones..." ),
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 7,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 7,
                        XmNtopWidget, GeneralAttributesBut,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 7,
                        XmNleftWidget, GeometryButton,
                        NULL );
        UxPutContext( ZoneButton, (char *) UxHistoStyleContext );


        /* Creation of FontButton */
        FontButton = XtVaCreateManagedWidget( "FontButton",
                        xmPushButtonGadgetClass,
                        EncadreForm2,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Font..." ),
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 7,
                        XmNtopWidget, ZoneButton,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, AxisSettings,
                        XmNleftOffset, 7,
                        NULL );
        UxPutContext( FontButton, (char *) UxHistoStyleContext );


        /* Creation of form4 */
        form4 = XtVaCreateManagedWidget( "form4",
                        xmFormWidgetClass,
                        histoStyleForm,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNheight, 33,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 15,
                        XmNtopWidget, EncadreForm1,
                        NULL );
        UxPutContext( form4, (char *) UxHistoStyleContext );


        /* Creation of OptionPlot_p1 */
        OptionPlot_p1_shell = XtVaCreatePopupShell ("OptionPlot_p1_shell",
                        xmMenuShellWidgetClass, form4,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        OptionPlot_p1 = XtVaCreateWidget( "OptionPlot_p1",
                        xmRowColumnWidgetClass,
                        OptionPlot_p1_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( OptionPlot_p1, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotD */
        OptionPlotD = XtVaCreateManagedWidget( "OptionPlotD",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Default" ),
                        NULL );
        UxPutContext( OptionPlotD, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotL */
        OptionPlotL = XtVaCreateManagedWidget( "OptionPlotL",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Line" ),
                        NULL );
        UxPutContext( OptionPlotL, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotC */
        OptionPlotC = XtVaCreateManagedWidget( "OptionPlotC",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Smooth Curve" ),
                        NULL );
        UxPutContext( OptionPlotC, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotB */
        OptionPlotB = XtVaCreateManagedWidget( "OptionPlotB",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Bar Chart" ),
                        NULL );
        UxPutContext( OptionPlotB, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotP */
        OptionPlotP = XtVaCreateManagedWidget( "OptionPlotP",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Polymarker" ),
                        NULL );
        UxPutContext( OptionPlotP, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotSTAR */
        OptionPlotSTAR = XtVaCreateManagedWidget( "OptionPlotSTAR",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Star" ),
                        NULL );
        UxPutContext( OptionPlotSTAR, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotE */
        OptionPlotE = XtVaCreateManagedWidget( "OptionPlotE",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Error Bars" ),
                        NULL );
        UxPutContext( OptionPlotE, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotE1 */
        OptionPlotE1 = XtVaCreateManagedWidget( "OptionPlotE1",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Error Bars (lines)" ),
                        NULL );
        UxPutContext( OptionPlotE1, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotE2 */
        OptionPlotE2 = XtVaCreateManagedWidget( "OptionPlotE2",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Error Rectangles" ),
                        NULL );
        UxPutContext( OptionPlotE2, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotE3 */
        OptionPlotE3 = XtVaCreateManagedWidget( "OptionPlotE3",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Error: Filled Area" ),
                        NULL );
        UxPutContext( OptionPlotE3, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotE4 */
        OptionPlotE4 = XtVaCreateManagedWidget( "OptionPlotE4",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Error: Smoothed Area" ),
                        NULL );
        UxPutContext( OptionPlotE4, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotBOX */
        OptionPlotBOX = XtVaCreateManagedWidget( "OptionPlotBOX",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Boxes" ),
                        NULL );
        UxPutContext( OptionPlotBOX, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotCOL */
        OptionPlotCOL = XtVaCreateManagedWidget( "OptionPlotCOL",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Color" ),
                        NULL );
        UxPutContext( OptionPlotCOL, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotSURF */
        OptionPlotSURF = XtVaCreateManagedWidget( "OptionPlotSURF",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Hidden Lines Surface" ),
                        NULL );
        UxPutContext( OptionPlotSURF, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotSURF1 */
        OptionPlotSURF1 = XtVaCreateManagedWidget( "OptionPlotSURF1",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                     RES_CONVERT( XmNlabelString, "Color Level Surface (1)" ),
                        NULL );
        UxPutContext( OptionPlotSURF1, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotSURF2 */
        OptionPlotSURF2 = XtVaCreateManagedWidget( "OptionPlotSURF2",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                    RES_CONVERT( XmNlabelString, "Color Level Surface (2)" ),
                        NULL );
        UxPutContext( OptionPlotSURF2, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotSURF3 */
        OptionPlotSURF3 = XtVaCreateManagedWidget( "OptionPlotSURF3",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Surface and Contour" ),
                        NULL );
        UxPutContext( OptionPlotSURF3, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotSURF4 */
        OptionPlotSURF4 = XtVaCreateManagedWidget( "OptionPlotSURF4",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Gouraud Shaded Surface" ),
                        NULL );
        UxPutContext( OptionPlotSURF4, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotLEGO */
        OptionPlotLEGO = XtVaCreateManagedWidget( "OptionPlotLEGO",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Hidden Lines Lego" ),
                        NULL );
        UxPutContext( OptionPlotLEGO, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotLEGO1 */
        OptionPlotLEGO1 = XtVaCreateManagedWidget( "OptionPlotLEGO1",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Filled Lego" ),
                        NULL );
        UxPutContext( OptionPlotLEGO1, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotLEGO2 */
        OptionPlotLEGO2 = XtVaCreateManagedWidget( "OptionPlotLEGO2",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Color Level Lego" ),
                        NULL );
        UxPutContext( OptionPlotLEGO2, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotCONT */
        OptionPlotCONT = XtVaCreateManagedWidget( "OptionPlotCONT",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Contour Plot" ),
                        NULL );
        UxPutContext( OptionPlotCONT, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotCONT3 */
        OptionPlotCONT3 = XtVaCreateManagedWidget( "OptionPlotCONT3",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Filled Contour Plot" ),
                        NULL );
        UxPutContext( OptionPlotCONT3, (char *) UxHistoStyleContext );


        /* Creation of OptionPlotTEXT */
        OptionPlotTEXT = XtVaCreateManagedWidget( "OptionPlotTEXT",
                        xmPushButtonGadgetClass,
                        OptionPlot_p1,
                        RES_CONVERT( XmNlabelString, "Text" ),
                        NULL );
        UxPutContext( OptionPlotTEXT, (char *) UxHistoStyleContext );


        /* Creation of OptionPlot */
        OptionPlot = XtVaCreateManagedWidget( "OptionPlot",
                        xmRowColumnWidgetClass,
                        form4,
                        XmNmarginHeight, 0,
                        XmNmarginWidth, 0,
                        XmNnumColumns, 1,
                        XmNrowColumnType, XmMENU_OPTION,
                        XmNspacing, 0,
                        XmNsubMenuId, OptionPlot_p1,
                        XmNrightAttachment, XmATTACH_NONE,
                        XmNtopAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Plot Options:  " ),
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNheight, 30,
                        NULL );
        UxPutContext( OptionPlot, (char *) UxHistoStyleContext );


        /* Creation of form1 */
        form1 = XtVaCreateManagedWidget( "form1",
                        xmFormWidgetClass,
                        histoStyleForm,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNheight, 33,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 10,
                        XmNleftWidget, form4,
                        XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
                        XmNtopOffset, 0,
                        XmNtopWidget, form4,
                        NULL );
        UxPutContext( form1, (char *) UxHistoStyleContext );


        /* Creation of CoordinateSystem_p1 */
CoordinateSystem_p1_shell = XtVaCreatePopupShell ("CoordinateSystem_p1_shell",
                        xmMenuShellWidgetClass, form1,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        CoordinateSystem_p1 = XtVaCreateWidget( "CoordinateSystem_p1",
                        xmRowColumnWidgetClass,
                        CoordinateSystem_p1_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( CoordinateSystem_p1, (char *) UxHistoStyleContext );


        /* Creation of CoordinateSystemC */
        CoordinateSystemC = XtVaCreateManagedWidget( "CoordinateSystemC",
                        xmPushButtonGadgetClass,
                        CoordinateSystem_p1,
                        RES_CONVERT( XmNlabelString, "Cartesian" ),
                        NULL );
        UxPutContext( CoordinateSystemC, (char *) UxHistoStyleContext );


        /* Creation of CoordinateSystemP */
        CoordinateSystemP = XtVaCreateManagedWidget( "CoordinateSystemP",
                        xmPushButtonGadgetClass,
                        CoordinateSystem_p1,
                        RES_CONVERT( XmNlabelString, "Polar" ),
                        NULL );
        UxPutContext( CoordinateSystemP, (char *) UxHistoStyleContext );


        /* Creation of CoordinateSystemR */
        CoordinateSystemR = XtVaCreateManagedWidget( "CoordinateSystemR",
                        xmPushButtonGadgetClass,
                        CoordinateSystem_p1,
                        RES_CONVERT( XmNlabelString, "Cylindrical" ),
                        NULL );
        UxPutContext( CoordinateSystemR, (char *) UxHistoStyleContext );


        /* Creation of CoordinateSystemS */
        CoordinateSystemS = XtVaCreateManagedWidget( "CoordinateSystemS",
                        xmPushButtonGadgetClass,
                        CoordinateSystem_p1,
                        RES_CONVERT( XmNlabelString, "Spherical" ),
                        NULL );
        UxPutContext( CoordinateSystemS, (char *) UxHistoStyleContext );


        /* Creation of CoordinateSystemD */
        CoordinateSystemD = XtVaCreateManagedWidget( "CoordinateSystemD",
                        xmPushButtonGadgetClass,
                        CoordinateSystem_p1,
                        RES_CONVERT( XmNlabelString, "Pseudo Rapidity" ),
                        NULL );
        UxPutContext( CoordinateSystemD, (char *) UxHistoStyleContext );


        /* Creation of CoordinateSystem */
        CoordinateSystem = XtVaCreateManagedWidget( "CoordinateSystem",
                        xmRowColumnWidgetClass,
                        form1,
                        XmNmarginHeight, 0,
                        XmNmarginWidth, 0,
                        XmNnumColumns, 1,
                        XmNrowColumnType, XmMENU_OPTION,
                        XmNspacing, 0,
                        XmNsubMenuId, CoordinateSystem_p1,
                        XmNrightAttachment, XmATTACH_NONE,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        RES_CONVERT( XmNlabelString, " " ),
                        NULL );
        UxPutContext( CoordinateSystem, (char *) UxHistoStyleContext );


        /* Creation of menu1 */
        menu1 = XtVaCreateManagedWidget( "menu1",
                        xmRowColumnWidgetClass,
                        histoStyle,
                        XmNrowColumnType, XmMENU_BAR,
                        XmNmenuAccelerator, "<KeyUp>F10",
                        NULL );
        UxPutContext( menu1, (char *) UxHistoStyleContext );


        /* Creation of menu1_p1 */
        menu1_p1_shell = XtVaCreatePopupShell ("menu1_p1_shell",
                        xmMenuShellWidgetClass, menu1,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        menu1_p1 = XtVaCreateWidget( "menu1_p1",
                        xmRowColumnWidgetClass,
                        menu1_p1_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( menu1_p1, (char *) UxHistoStyleContext );


        /* Creation of OpenstylePushButton */
        OpenstylePushButton = XtVaCreateManagedWidget( "OpenstylePushButton",
                        xmPushButtonWidgetClass,
                        menu1_p1,
                        RES_CONVERT( XmNlabelString, "Open Style..." ),
                        RES_CONVERT( XmNmnemonic, "O" ),
                        NULL );
        UxPutContext( OpenstylePushButton, (char *) UxHistoStyleContext );


        /* Creation of SavestylePushButton */
        SavestylePushButton = XtVaCreateManagedWidget( "SavestylePushButton",
                        xmPushButtonWidgetClass,
                        menu1_p1,
                        RES_CONVERT( XmNlabelString, "Save Style " ),
                        RES_CONVERT( XmNmnemonic, "S" ),
                        NULL );
        UxPutContext( SavestylePushButton, (char *) UxHistoStyleContext );


        /* Creation of SavestyleasPushButton */
    SavestyleasPushButton = XtVaCreateManagedWidget( "SavestyleasPushButton",
                        xmPushButtonWidgetClass,
                        menu1_p1,
                        RES_CONVERT( XmNlabelString, "Save Style As..." ),
                        RES_CONVERT( XmNmnemonic, "S" ),
                        NULL );
        UxPutContext( SavestyleasPushButton, (char *) UxHistoStyleContext );


        /* Creation of ClosePushButton */
        ClosePushButton = XtVaCreateManagedWidget( "ClosePushButton",
                        xmPushButtonWidgetClass,
                        menu1_p1,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        RES_CONVERT( XmNmnemonic, "C" ),
                        NULL );
        UxPutContext( ClosePushButton, (char *) UxHistoStyleContext );


        /* Creation of menu1_top_b1 */
        menu1_top_b1 = XtVaCreateManagedWidget( "menu1_top_b1",
                        xmCascadeButtonWidgetClass,
                        menu1,
                        RES_CONVERT( XmNlabelString, "File" ),
                        XmNsubMenuId, menu1_p1,
                        RES_CONVERT( XmNmnemonic, "F" ),
                        NULL );
        UxPutContext( menu1_top_b1, (char *) UxHistoStyleContext );


        /* Creation of menu1_p2 */
        menu1_p2_shell = XtVaCreatePopupShell ("menu1_p2_shell",
                        xmMenuShellWidgetClass, menu1,
                        XmNwidth, 1,
                        XmNheight, 1,
                        XmNallowShellResize, TRUE,
                        XmNoverrideRedirect, TRUE,
                        NULL );

        menu1_p2 = XtVaCreateWidget( "menu1_p2",
                        xmRowColumnWidgetClass,
                        menu1_p2_shell,
                        XmNrowColumnType, XmMENU_PULLDOWN,
                        NULL );
        UxPutContext( menu1_p2, (char *) UxHistoStyleContext );


        /* Creation of RefreshToggleButtonGadget */
        RefreshToggleButtonGadget =
                        XtVaCreateManagedWidget( "RefreshToggleButtonGadget",
                        xmToggleButtonGadgetClass,
                        menu1_p2,
                        RES_CONVERT( XmNlabelString, "Automatic Refresh" ),
                        RES_CONVERT( XmNmnemonic, "A" ),
                        NULL );
       UxPutContext( RefreshToggleButtonGadget, (char *) UxHistoStyleContext );


        /* Creation of SameToggleButtonGadget */
   SameToggleButtonGadget = XtVaCreateManagedWidget( "SameToggleButtonGadget",
                        xmToggleButtonGadgetClass,
                        menu1_p2,
                        RES_CONVERT( XmNlabelString, "Overlay" ),
                        RES_CONVERT( XmNmnemonic, "O" ),
                        NULL );
        UxPutContext( SameToggleButtonGadget, (char *) UxHistoStyleContext );


        /* Creation of menu1_top_b2 */
        menu1_top_b2 = XtVaCreateManagedWidget( "menu1_top_b2",
                        xmCascadeButtonWidgetClass,
                        menu1,
                        RES_CONVERT( XmNlabelString, "Options" ),
                        XmNsubMenuId, menu1_p2,
                        RES_CONVERT( XmNmnemonic, "O" ),
                        NULL );
        UxPutContext( menu1_top_b2, (char *) UxHistoStyleContext );


        XtAddCallback( histoStyle, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxHistoStyleContext);

        XmMainWindowSetAreas( histoStyle, menu1, (Widget) NULL,
                        (Widget) NULL, (Widget) NULL, frame3 );

        return ( histoStyle );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_histoStyle()
{
        Widget                  rtrn;
        _UxChistoStyle          *UxContext;

        UxHistoStyleContext = UxContext =
             (_UxChistoStyle *) UxNewContext( sizeof(_UxChistoStyle), False );


        rtrn = _Uxbuild_histoStyle();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

