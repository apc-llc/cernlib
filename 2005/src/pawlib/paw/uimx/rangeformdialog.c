/*
 * $Id: rangeformdialog.c,v 1.1.1.1 1996/03/01 11:39:35 mclareni Exp $
 *
 * $Log: rangeformdialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:35  mclareni
 * Paw
 *
 */
/*CMZ :  2.05/01 22/03/94  17.38.27  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
       rangeFormDialog.c
       (Generated from interface file rangeFormDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/ToggleBG.h>
#include <Xm/Scale.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/Form.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern swidget histoStyle;


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
        Widget  UxseparatorGadget7;
        Widget  UxEncadreForm3;
        Widget  UxTitre3;
        Widget  UxUpperLeftSep3;
        Widget  UxLeftSep3;
        Widget  UxBottomSep3;
        Widget  UxRightSep3;
        Widget  UxUpperRightSep3;
        Widget  Uxform1;
        Widget  UxseparatorGadget8;
        Widget  UxTitre4;
        Widget  UxUpperLeftSep4;
        Widget  UxLeftSep4;
        Widget  UxBottomSep4;
        Widget  UxRightSep4;
        Widget  UxUpperRightSep4;
        Widget  Uxform2;
        Widget  Uxform3;
        Widget  UxlabelGadget2;
        Widget  Uxform4;
        Widget  UxlabelGadget3;
} _UxCrangeFormDialog;

static _UxCrangeFormDialog     *UxRangeFormDialogContext;
#define separatorGadget7        UxRangeFormDialogContext->UxseparatorGadget7
#define EncadreForm3            UxRangeFormDialogContext->UxEncadreForm3
#define Titre3                  UxRangeFormDialogContext->UxTitre3
#define UpperLeftSep3           UxRangeFormDialogContext->UxUpperLeftSep3
#define LeftSep3                UxRangeFormDialogContext->UxLeftSep3
#define BottomSep3              UxRangeFormDialogContext->UxBottomSep3
#define RightSep3               UxRangeFormDialogContext->UxRightSep3
#define UpperRightSep3          UxRangeFormDialogContext->UxUpperRightSep3
#define form1                   UxRangeFormDialogContext->Uxform1
#define separatorGadget8        UxRangeFormDialogContext->UxseparatorGadget8
#define Titre4                  UxRangeFormDialogContext->UxTitre4
#define UpperLeftSep4           UxRangeFormDialogContext->UxUpperLeftSep4
#define LeftSep4                UxRangeFormDialogContext->UxLeftSep4
#define BottomSep4              UxRangeFormDialogContext->UxBottomSep4
#define RightSep4               UxRangeFormDialogContext->UxRightSep4
#define UpperRightSep4          UxRangeFormDialogContext->UxUpperRightSep4
#define form2                   UxRangeFormDialogContext->Uxform2
#define form3                   UxRangeFormDialogContext->Uxform3
#define labelGadget2            UxRangeFormDialogContext->UxlabelGadget2
#define form4                   UxRangeFormDialogContext->Uxform4
#define labelGadget3            UxRangeFormDialogContext->UxlabelGadget3


Widget  rangeFormDialog;
Widget  CloseRangeButtonGadget;
Widget  ApplyRangeButtonGadget;
Widget  X1Scale;
Widget  X2Scale;
Widget  LockXToggleButtonGadget;
Widget  labelGadget1;
Widget  XBinNumber;
Widget  XScaleRebin;
Widget  YRangeFrame;
Widget  Y1Scale;
Widget  Y2Scale;
Widget  LockYToggleButtonGadget;
Widget  labelGadget5;
Widget  YBinNumber;
Widget  ResetRangeButtonGadget;
Widget  minimumText;
Widget  maximumText;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_rangeFormDialog( void );

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_rangeFormDialog()
{
        Widget          _UxParent;


        /* Creation of rangeFormDialog */
        _UxParent = histoStyle;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        _UxParent = XtVaCreatePopupShell( "rangeFormDialog_shell",
                        xmDialogShellWidgetClass, _UxParent,
                        XmNx, 209,
                        XmNy, 309,
                        XmNwidth, 540,
                        XmNheight, 320,
                        XmNtitle, "rangeFormDialog",
                        NULL );

        rangeFormDialog = XtVaCreateWidget( "rangeFormDialog",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNwidth, 540,
                        XmNheight, 320,
                        XmNautoUnmanage, FALSE,
                        RES_CONVERT( XmNdialogTitle, "Axis Scaling" ),
                        XmNfractionBase, 100,
                        NULL );
        UxPutContext( rangeFormDialog, (char *) UxRangeFormDialogContext );


        /* Creation of CloseRangeButtonGadget */
   CloseRangeButtonGadget = XtVaCreateManagedWidget( "CloseRangeButtonGadget",
                        xmPushButtonGadgetClass,
                        rangeFormDialog,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, " Close " ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 75,
                        XmNrightPosition, 95,
                        NULL );
    UxPutContext( CloseRangeButtonGadget, (char *) UxRangeFormDialogContext );


        /* Creation of ApplyRangeButtonGadget */
    ApplyRangeButtonGadget = XtVaCreateManagedWidget( "ApplyRangeButtonGadget",
                        xmPushButtonGadgetClass,
                        rangeFormDialog,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, " Apply " ),
                        XmNshowAsDefault, 1,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 5,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 25,
                        NULL );
    UxPutContext( ApplyRangeButtonGadget, (char *) UxRangeFormDialogContext );


        /* Creation of separatorGadget7 */
        separatorGadget7 = XtVaCreateManagedWidget( "separatorGadget7",
                        xmSeparatorGadgetClass,
                        rangeFormDialog,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 2,
                        XmNbottomWidget, CloseRangeButtonGadget,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        NULL );
        UxPutContext( separatorGadget7, (char *) UxRangeFormDialogContext );


        /* Creation of EncadreForm3 */
        EncadreForm3 = XtVaCreateManagedWidget( "EncadreForm3",
                        xmFormWidgetClass,
                        rangeFormDialog,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 250,
                        XmNy, 20,
                        XmNwidth, 290,
                        XmNheight, 280,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNbottomWidget, separatorGadget7,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNleftPosition, 48,
                        XmNtopOffset, 10,
                        NULL );
        UxPutContext( EncadreForm3, (char *) UxRangeFormDialogContext );


        /* Creation of Titre3 */
        Titre3 = XtVaCreateManagedWidget( "Titre3",
                        xmLabelGadgetClass,
                        EncadreForm3,
                        XmNx, 10,
                        XmNy, 0,
                        XmNwidth, 60,
                        XmNheight, 20,
                        RES_CONVERT( XmNlabelString, "X Axis" ),
                        XmNrecomputeSize, TRUE,
                        XmNmarginWidth, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        NULL );
        UxPutContext( Titre3, (char *) UxRangeFormDialogContext );


        /* Creation of UpperLeftSep3 */
        UpperLeftSep3 = XtVaCreateManagedWidget( "UpperLeftSep3",
                        xmSeparatorGadgetClass,
                        EncadreForm3,
                        XmNx, 87,
                        XmNy, 200,
                        XmNwidth, 99,
                        XmNheight, 2,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightWidget, Titre3,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 10,
                        NULL );
        UxPutContext( UpperLeftSep3, (char *) UxRangeFormDialogContext );


        /* Creation of LeftSep3 */
        LeftSep3 = XtVaCreateManagedWidget( "LeftSep3",
                        xmSeparatorGadgetClass,
                        EncadreForm3,
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
        UxPutContext( LeftSep3, (char *) UxRangeFormDialogContext );


        /* Creation of BottomSep3 */
        BottomSep3 = XtVaCreateManagedWidget( "BottomSep3",
                        xmSeparatorGadgetClass,
                        EncadreForm3,
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
        UxPutContext( BottomSep3, (char *) UxRangeFormDialogContext );


        /* Creation of RightSep3 */
        RightSep3 = XtVaCreateManagedWidget( "RightSep3",
                        xmSeparatorGadgetClass,
                        EncadreForm3,
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
        UxPutContext( RightSep3, (char *) UxRangeFormDialogContext );


        /* Creation of UpperRightSep3 */
        UpperRightSep3 = XtVaCreateManagedWidget( "UpperRightSep3",
                        xmSeparatorGadgetClass,
                        EncadreForm3,
                        XmNx, 274,
                        XmNy, 200,
                        XmNwidth, 106,
                        XmNheight, 2,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftWidget, Titre3,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 10,
                        NULL );
        UxPutContext( UpperRightSep3, (char *) UxRangeFormDialogContext );


        /* Creation of form1 */
        form1 = XtVaCreateManagedWidget( "form1",
                        xmFormWidgetClass,
                        EncadreForm3,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 2,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 2,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 2,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 17,
                        NULL );
        UxPutContext( form1, (char *) UxRangeFormDialogContext );


        /* Creation of X1Scale */
        X1Scale = XtVaCreateManagedWidget( "X1Scale",
                        xmScaleWidgetClass,
                        form1,
                        XmNheight, 60,
                        XmNmaximum, 999,
                        XmNminimum, 1,
                        XmNorientation, XmHORIZONTAL,
                        XmNshowValue, TRUE,
                        RES_CONVERT( XmNtitleString, "First Bin" ),
                        XmNvalue, 1,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNresizable, FALSE,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        NULL );
        UxPutContext( X1Scale, (char *) UxRangeFormDialogContext );


        /* Creation of X2Scale */
        X2Scale = XtVaCreateManagedWidget( "X2Scale",
                        xmScaleWidgetClass,
                        form1,
                        XmNx, -1,
                        XmNy, 69,
                        XmNheight, 60,
                        XmNmaximum, 999,
                        XmNminimum, 1,
                        XmNorientation, XmHORIZONTAL,
                        XmNshowValue, TRUE,
                        RES_CONVERT( XmNtitleString, "Last Bin" ),
                        XmNvalue, 100,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNresizable, FALSE,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 0,
                        XmNtopWidget, X1Scale,
                        NULL );
        UxPutContext( X2Scale, (char *) UxRangeFormDialogContext );


        /* Creation of LockXToggleButtonGadget */
LockXToggleButtonGadget = XtVaCreateManagedWidget( "LockXToggleButtonGadget",
                        xmToggleButtonGadgetClass,
                        form1,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Lock" ),
                        XmNresizable, FALSE,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 0,
                        XmNtopWidget, X2Scale,
                        NULL );
   UxPutContext( LockXToggleButtonGadget, (char *) UxRangeFormDialogContext );


        /* Creation of labelGadget1 */
        labelGadget1 = XtVaCreateManagedWidget( "labelGadget1",
                        xmLabelGadgetClass,
                        form1,
                        XmNx, 15,
                        XmNy, 135,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Number of Bins:  " ),
                        XmNmarginHeight, 0,
                        XmNmarginLeft, 0,
                        XmNmarginWidth, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNresizable, FALSE,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 0,
                        XmNtopWidget, X2Scale,
                        NULL );
        UxPutContext( labelGadget1, (char *) UxRangeFormDialogContext );


        /* Creation of XBinNumber */
        XBinNumber = XtVaCreateManagedWidget( "XBinNumber",
                        xmLabelWidgetClass,
                        form1,
                        XmNwidth, 60,
                        XmNheight, 30,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "100" ),
                        XmNrecomputeSize, FALSE,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, labelGadget1,
                        XmNresizable, FALSE,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 0,
                        XmNtopWidget, X2Scale,
                        NULL );
        UxPutContext( XBinNumber, (char *) UxRangeFormDialogContext );


        /* Creation of XScaleRebin */
        XScaleRebin = XtVaCreateManagedWidget( "XScaleRebin",
                        xmScaleWidgetClass,
                        form1,
                        XmNheight, 60,
                        XmNminimum, 1,
                        XmNorientation, XmHORIZONTAL,
                        XmNshowValue, TRUE,
                        RES_CONVERT( XmNtitleString, "Rebin Factor" ),
                        XmNvalue, 1,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 10,
                        XmNtopWidget, XBinNumber,
                        NULL );
        UxPutContext( XScaleRebin, (char *) UxRangeFormDialogContext );


        /* Creation of separatorGadget8 */
        separatorGadget8 = XtVaCreateManagedWidget( "separatorGadget8",
                        xmSeparatorGadgetClass,
                        form1,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 0,
                        XmNbottomWidget, XScaleRebin,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        NULL );
        UxPutContext( separatorGadget8, (char *) UxRangeFormDialogContext );


        /* Creation of YRangeFrame */
        YRangeFrame = XtVaCreateManagedWidget( "YRangeFrame",
                        xmFormWidgetClass,
                        rangeFormDialog,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 0,
                        XmNy, 20,
                        XmNwidth, 250,
                        XmNheight, 280,
                        XmNbottomAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 72,
                        XmNbottomWidget, separatorGadget7,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 45,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 10,
                        NULL );
        UxPutContext( YRangeFrame, (char *) UxRangeFormDialogContext );


        /* Creation of Titre4 */
        Titre4 = XtVaCreateManagedWidget( "Titre4",
                        xmLabelGadgetClass,
                        YRangeFrame,
                        XmNx, 10,
                        XmNy, 0,
                        XmNwidth, 60,
                        XmNheight, 20,
                        RES_CONVERT( XmNlabelString, "Y Axis" ),
                        XmNrecomputeSize, TRUE,
                        XmNmarginWidth, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        NULL );
        UxPutContext( Titre4, (char *) UxRangeFormDialogContext );


        /* Creation of UpperLeftSep4 */
        UpperLeftSep4 = XtVaCreateManagedWidget( "UpperLeftSep4",
                        xmSeparatorGadgetClass,
                        YRangeFrame,
                        XmNx, 87,
                        XmNy, 200,
                        XmNwidth, 99,
                        XmNheight, 2,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 0,
                        XmNrightWidget, Titre4,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 10,
                        NULL );
        UxPutContext( UpperLeftSep4, (char *) UxRangeFormDialogContext );


        /* Creation of LeftSep4 */
        LeftSep4 = XtVaCreateManagedWidget( "LeftSep4",
                        xmSeparatorGadgetClass,
                        YRangeFrame,
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
        UxPutContext( LeftSep4, (char *) UxRangeFormDialogContext );


        /* Creation of BottomSep4 */
        BottomSep4 = XtVaCreateManagedWidget( "BottomSep4",
                        xmSeparatorGadgetClass,
                        YRangeFrame,
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
        UxPutContext( BottomSep4, (char *) UxRangeFormDialogContext );


        /* Creation of RightSep4 */
        RightSep4 = XtVaCreateManagedWidget( "RightSep4",
                        xmSeparatorGadgetClass,
                        YRangeFrame,
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
        UxPutContext( RightSep4, (char *) UxRangeFormDialogContext );


        /* Creation of UpperRightSep4 */
        UpperRightSep4 = XtVaCreateManagedWidget( "UpperRightSep4",
                        xmSeparatorGadgetClass,
                        YRangeFrame,
                        XmNx, 274,
                        XmNy, 200,
                        XmNwidth, 106,
                        XmNheight, 2,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 0,
                        XmNleftWidget, Titre4,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 10,
                        NULL );
        UxPutContext( UpperRightSep4, (char *) UxRangeFormDialogContext );


        /* Creation of form2 */
        form2 = XtVaCreateManagedWidget( "form2",
                        xmFormWidgetClass,
                        YRangeFrame,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 2,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 2,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 2,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 17,
                        NULL );
        UxPutContext( form2, (char *) UxRangeFormDialogContext );


        /* Creation of Y1Scale */
        Y1Scale = XtVaCreateManagedWidget( "Y1Scale",
                        xmScaleWidgetClass,
                        form2,
                        XmNmaximum, 999,
                        XmNminimum, 1,
                        XmNshowValue, TRUE,
                        RES_CONVERT( XmNtitleString, "First Bin" ),
                        XmNvalue, 1,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 30,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 0,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 50,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        NULL );
        UxPutContext( Y1Scale, (char *) UxRangeFormDialogContext );


        /* Creation of Y2Scale */
        Y2Scale = XtVaCreateManagedWidget( "Y2Scale",
                        xmScaleWidgetClass,
                        form2,
                        XmNmaximum, 999,
                        XmNminimum, 1,
                        XmNorientation, XmVERTICAL,
                        XmNshowValue, TRUE,
                        RES_CONVERT( XmNtitleString, "Last Bin" ),
                        XmNvalue, 100,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 30,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftOffset, 0,
                        XmNleftPosition, 50,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 100,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 5,
                        NULL );
        UxPutContext( Y2Scale, (char *) UxRangeFormDialogContext );


        /* Creation of LockYToggleButtonGadget */
 LockYToggleButtonGadget = XtVaCreateManagedWidget( "LockYToggleButtonGadget",
                        xmToggleButtonGadgetClass,
                        form2,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Lock" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNresizable, FALSE,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 5,
                        NULL );
  UxPutContext( LockYToggleButtonGadget, (char *) UxRangeFormDialogContext );


        /* Creation of labelGadget5 */
        labelGadget5 = XtVaCreateManagedWidget( "labelGadget5",
                        xmLabelGadgetClass,
                        form2,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Number of Bins:  " ),
                        XmNmarginHeight, 0,
                        XmNmarginLeft, 0,
                        XmNmarginWidth, 0,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        NULL );
        UxPutContext( labelGadget5, (char *) UxRangeFormDialogContext );


        /* Creation of YBinNumber */
        YBinNumber = XtVaCreateManagedWidget( "YBinNumber",
                        xmLabelWidgetClass,
                        form2,
                        XmNwidth, 45,
                        XmNheight, 30,
                        XmNalignment, XmALIGNMENT_BEGINNING,
                        RES_CONVERT( XmNlabelString, "100" ),
                        XmNrecomputeSize, FALSE,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftWidget, labelGadget5,
                        XmNresizable, FALSE,
                        NULL );
        UxPutContext( YBinNumber, (char *) UxRangeFormDialogContext );


        /* Creation of ResetRangeButtonGadget */
  ResetRangeButtonGadget = XtVaCreateManagedWidget( "ResetRangeButtonGadget",
                        xmPushButtonGadgetClass,
                        rangeFormDialog,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        RES_CONVERT( XmNlabelString, " Reset " ),
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 40,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 60,
                        NULL );
  UxPutContext( ResetRangeButtonGadget, (char *) UxRangeFormDialogContext );


        /* Creation of form3 */
        form3 = XtVaCreateManagedWidget( "form3",
                        xmFormWidgetClass,
                        rangeFormDialog,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNheight, 30,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 16,
                        XmNrightWidget, EncadreForm3,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, YRangeFrame,
                        NULL );
        UxPutContext( form3, (char *) UxRangeFormDialogContext );


        /* Creation of labelGadget2 */
        labelGadget2 = XtVaCreateManagedWidget( "labelGadget2",
                        xmLabelGadgetClass,
                        form3,
                        RES_CONVERT( XmNlabelString, "Minimum" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        NULL );
        UxPutContext( labelGadget2, (char *) UxRangeFormDialogContext );


        /* Creation of minimumText */
        minimumText = XtVaCreateManagedWidget( "minimumText",
                        xmTextWidgetClass,
                        form3,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNwidth, 150,
                        XmNmarginHeight, 3,
                        XmNvalue, "0",
                        NULL );
        UxPutContext( minimumText, (char *) UxRangeFormDialogContext );


        /* Creation of form4 */
        form4 = XtVaCreateManagedWidget( "form4",
                        xmFormWidgetClass,
                        rangeFormDialog,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNheight, 30,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 5,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightOffset, 16,
                        XmNrightWidget, EncadreForm3,
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopOffset, 5,
                        XmNtopWidget, form3,
                        NULL );
        UxPutContext( form4, (char *) UxRangeFormDialogContext );


        /* Creation of labelGadget3 */
        labelGadget3 = XtVaCreateManagedWidget( "labelGadget3",
                        xmLabelGadgetClass,
                        form4,
                        RES_CONVERT( XmNlabelString, "Maximum" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        NULL );
        UxPutContext( labelGadget3, (char *) UxRangeFormDialogContext );


        /* Creation of maximumText */
        maximumText = XtVaCreateManagedWidget( "maximumText",
                        xmTextWidgetClass,
                        form4,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNwidth, 150,
                        XmNmarginHeight, 3,
                        XmNvalue, "0",
                        NULL );
        UxPutContext( maximumText, (char *) UxRangeFormDialogContext );

        XtVaSetValues(rangeFormDialog,
                        XmNdefaultButton, ApplyRangeButtonGadget,
                        NULL );


        XtAddCallback( rangeFormDialog, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxRangeFormDialogContext);


        return ( rangeFormDialog );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_rangeFormDialog()
{
        Widget                  rtrn;
        _UxCrangeFormDialog     *UxContext;

        UxRangeFormDialogContext = UxContext =
  (_UxCrangeFormDialog *) UxNewContext( sizeof(_UxCrangeFormDialog), False );


        rtrn = _Uxbuild_rangeFormDialog();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

