/*
 * $Id: geometrydialog.c,v 1.1.1.1 1996/03/01 11:39:33 mclareni Exp $
 *
 * $Log: geometrydialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:33  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.06  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
       geometryDialog.c
       (Generated from interface file geometryDialog.i)
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/Scale.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
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
        Widget  UxseparatorGadget4;
        Widget  Uxform3;
} _UxCgeometryDialog;

static _UxCgeometryDialog      *UxGeometryDialogContext;
#define separatorGadget4        UxGeometryDialogContext->UxseparatorGadget4
#define form3                   UxGeometryDialogContext->Uxform3


Widget  geometryDialog;
Widget  PixmapForm;
Widget  Ywin;
Widget  Gsiz;
Widget  Xmgr;
Widget  Xmgl;
Widget  Ymgu;
Widget  Ylab;
Widget  Xlab;
Widget  Ytic;
Widget  Xwin;
Widget  Ygti;
Widget  Asiz;
Widget  Vsiz;
Widget  Xtic;
Widget  Xsiz;
Widget  Ysiz;
Widget  Ymgl;
Widget  Yhti;
Widget  Xval;
Widget  Tsiz;
Widget  Errx;
Widget  Csiz;
Widget  Hmax;
Widget  Baro;
Widget  Barw;
Widget  Yval;
Widget  Smgu;
Widget  Smgr;
Widget  Scale;
Widget  ApplyGeometry;
Widget  ResetGeometry;
Widget  CloseGeometry;
Widget  Label;
Widget  Text;
Widget  ResetCurrentButton;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_geometryDialog();

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_geometryDialog()
{
        Widget          _UxParent;


        /* Creation of geometryDialog */
        _UxParent = histoStyle;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        _UxParent = XtVaCreatePopupShell( "geometryDialog_shell",
                        xmDialogShellWidgetClass, _UxParent,
                        XmNx, 730,
                        XmNy, 90,
                        XmNwidth, 400,
                        XmNheight, 760,
                        XmNallowShellResize, FALSE,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "geometryDialog",
                        NULL );

        geometryDialog = XtVaCreateWidget( "geometryDialog",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNunitType, XmPIXELS,
                        XmNwidth, 400,
                        XmNheight, 760,
                        XmNautoUnmanage, FALSE,
                        RES_CONVERT( XmNdialogTitle, "Geometry Settings" ),
                        XmNnoResize, TRUE,
                        NULL );
        UxPutContext( geometryDialog, (char *) UxGeometryDialogContext );


        /* Creation of PixmapForm */
        PixmapForm = XtVaCreateManagedWidget( "PixmapForm",
                        xmFormWidgetClass,
                        geometryDialog,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 20,
                        XmNy, 30,
                        XmNwidth, 400,
                        XmNheight, 582,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 0,
                        XmNborderWidth, 1,
                        RES_CONVERT( XmNbackground, "white" ),
                        RES_CONVERT( XmNforeground, "black" ),
                        XmNnoResize, TRUE,
                        NULL );
        UxPutContext( PixmapForm, (char *) UxGeometryDialogContext );


        /* Creation of Ywin */
        Ywin = XtVaCreateManagedWidget( "Ywin",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 209,
                        XmNy, 286,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Ywin, (char *) UxGeometryDialogContext );


        /* Creation of Gsiz */
        Gsiz = XtVaCreateManagedWidget( "Gsiz",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 83,
                        XmNy, 7,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Gsiz, (char *) UxGeometryDialogContext );


        /* Creation of Xmgr */
        Xmgr = XtVaCreateManagedWidget( "Xmgr",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 366,
                        XmNy, 120,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Xmgr, (char *) UxGeometryDialogContext );


        /* Creation of Xmgl */
        Xmgl = XtVaCreateManagedWidget( "Xmgl",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 11,
                        XmNy, 118,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Xmgl, (char *) UxGeometryDialogContext );


        /* Creation of Ymgu */
        Ymgu = XtVaCreateManagedWidget( "Ymgu",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 31,
                        XmNy, 18,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Ymgu, (char *) UxGeometryDialogContext );


        /* Creation of Ylab */
        Ylab = XtVaCreateManagedWidget( "Ylab",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 359,
                        XmNy, 553,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Ylab, (char *) UxGeometryDialogContext );


        /* Creation of Xlab */
        Xlab = XtVaCreateManagedWidget( "Xlab",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 4,
                        XmNy, 297,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Xlab, (char *) UxGeometryDialogContext );


        /* Creation of Ytic */
        Ytic = XtVaCreateManagedWidget( "Ytic",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 98,
                        XmNy, 366,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Ytic, (char *) UxGeometryDialogContext );


        /* Creation of Xwin */
        Xwin = XtVaCreateManagedWidget( "Xwin",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 186,
                        XmNy, 118,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Xwin, (char *) UxGeometryDialogContext );


        /* Creation of Ygti */
        Ygti = XtVaCreateManagedWidget( "Ygti",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 306,
                        XmNy, 24,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Ygti, (char *) UxGeometryDialogContext );


        /* Creation of Asiz */
        Asiz = XtVaCreateManagedWidget( "Asiz",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 305,
                        XmNy, 563,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Asiz, (char *) UxGeometryDialogContext );


        /* Creation of Vsiz */
        Vsiz = XtVaCreateManagedWidget( "Vsiz",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 15,
                        XmNy, 486,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Vsiz, (char *) UxGeometryDialogContext );


        /* Creation of Xtic */
        Xtic = XtVaCreateManagedWidget( "Xtic",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 149,
                        XmNy, 492,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Xtic, (char *) UxGeometryDialogContext );


        /* Creation of Xsiz */
        Xsiz = XtVaCreateManagedWidget( "Xsiz",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 215,
                        XmNy, 452,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Xsiz, (char *) UxGeometryDialogContext );


        /* Creation of Ysiz */
        Ysiz = XtVaCreateManagedWidget( "Ysiz",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 373,
                        XmNy, 306,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Ysiz, (char *) UxGeometryDialogContext );


        /* Creation of Ymgl */
        Ymgl = XtVaCreateManagedWidget( "Ymgl",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 60,
                        XmNy, 553,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Ymgl, (char *) UxGeometryDialogContext );


        /* Creation of Yhti */
        Yhti = XtVaCreateManagedWidget( "Yhti",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 265,
                        XmNy, 540,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Yhti, (char *) UxGeometryDialogContext );


        /* Creation of Xval */
        Xval = XtVaCreateManagedWidget( "Xval",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 94,
                        XmNy, 455,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Xval, (char *) UxGeometryDialogContext );


        /* Creation of Tsiz */
        Tsiz = XtVaCreateManagedWidget( "Tsiz",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 118,
                        XmNy, 558,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Tsiz, (char *) UxGeometryDialogContext );


        /* Creation of Errx */
        Errx = XtVaCreateManagedWidget( "Errx",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 85,
                        XmNy, 336,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Errx, (char *) UxGeometryDialogContext );


        /* Creation of Csiz */
        Csiz = XtVaCreateManagedWidget( "Csiz",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 288,
                        XmNy, 396,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Csiz, (char *) UxGeometryDialogContext );


        /* Creation of Hmax */
        Hmax = XtVaCreateManagedWidget( "Hmax",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 304,
                        XmNy, 55,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Hmax, (char *) UxGeometryDialogContext );


        /* Creation of Baro */
        Baro = XtVaCreateManagedWidget( "Baro",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 150,
                        XmNy, 98,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Baro, (char *) UxGeometryDialogContext );


        /* Creation of Barw */
        Barw = XtVaCreateManagedWidget( "Barw",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 109,
                        XmNy, 51,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Barw, (char *) UxGeometryDialogContext );


        /* Creation of Yval */
        Yval = XtVaCreateManagedWidget( "Yval",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 38,
                        XmNy, 545,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Yval, (char *) UxGeometryDialogContext );


        /* Creation of Smgu */
        Smgu = XtVaCreateManagedWidget( "Smgu",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 164,
                        XmNy, 316,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Smgu, (char *) UxGeometryDialogContext );


        /* Creation of Smgr */
        Smgr = XtVaCreateManagedWidget( "Smgr",
                        xmPushButtonWidgetClass,
                        PixmapForm,
                        XmNx, 338,
                        XmNy, 406,
                        XmNwidth, 10,
                        XmNheight, 10,
                        RES_CONVERT( XmNlabelString, "" ),
                        RES_CONVERT( XmNforeground, "#509f50" ),
                        XmNmarginWidth, 0,
                        XmNhighlightThickness, 0,
                        NULL );
        UxPutContext( Smgr, (char *) UxGeometryDialogContext );


        /* Creation of Scale */
        Scale = XtVaCreateManagedWidget( "Scale",
                        xmScaleWidgetClass,
                        geometryDialog,
                        XmNx, 2,
                        XmNy, 584,
                        XmNwidth, 396,
                        XmNheight, 63,
                        XmNorientation, XmHORIZONTAL,
                        XmNshowValue, TRUE,
                        RES_CONVERT( XmNtitleString, "" ),
                        XmNdecimalPoints, 2,
                        NULL );
        UxPutContext( Scale, (char *) UxGeometryDialogContext );


        /* Creation of separatorGadget4 */
        separatorGadget4 = XtVaCreateManagedWidget( "separatorGadget4",
                        xmSeparatorGadgetClass,
                        geometryDialog,
                        XmNx, 0,
                        XmNy, 280,
                        XmNwidth, 1220,
                        XmNheight, 20,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 50,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopAttachment, XmATTACH_NONE,
                        NULL );
        UxPutContext( separatorGadget4, (char *) UxGeometryDialogContext );


        /* Creation of ApplyGeometry */
        ApplyGeometry = XtVaCreateManagedWidget( "ApplyGeometry",
                        xmPushButtonGadgetClass,
                        geometryDialog,
                        XmNx, 20,
                        XmNy, 360,
                        XmNwidth, 90,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Apply" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 25,
                        NULL );
        UxPutContext( ApplyGeometry, (char *) UxGeometryDialogContext );


        /* Creation of ResetGeometry */
        ResetGeometry = XtVaCreateManagedWidget( "ResetGeometry",
                        xmPushButtonGadgetClass,
                        geometryDialog,
                        XmNx, 154,
                        XmNy, 707,
                        XmNwidth, 90,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 25,
                        XmNleftWidget, ApplyGeometry,
                        NULL );
        UxPutContext( ResetGeometry, (char *) UxGeometryDialogContext );


        /* Creation of CloseGeometry */
        CloseGeometry = XtVaCreateManagedWidget( "CloseGeometry",
                        xmPushButtonGadgetClass,
                        geometryDialog,
                        XmNx, 290,
                        XmNy, 360,
                        XmNwidth, 90,
                        XmNheight, 30,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNleftOffset, 25,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 25,
                        XmNleftWidget, ResetGeometry,
                        NULL );
        UxPutContext( CloseGeometry, (char *) UxGeometryDialogContext );


        /* Creation of form3 */
        form3 = XtVaCreateManagedWidget( "form3",
                        xmFormWidgetClass,
                        geometryDialog,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 5,
                        XmNy, 649,
                        XmNwidth, 384,
                        XmNheight, 45,
                        NULL );
        UxPutContext( form3, (char *) UxGeometryDialogContext );


        /* Creation of Label */
        Label = XtVaCreateManagedWidget( "Label",
                        xmLabelWidgetClass,
                        form3,
                        XmNx, 4,
                        XmNy, 645,
                        XmNwidth, 82,
                        XmNheight, 45,
                        RES_CONVERT( XmNlabelString, "YMGL : " ),
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( Label, (char *) UxGeometryDialogContext );


        /* Creation of Text */
        Text = XtVaCreateManagedWidget( "Text",
                        xmTextWidgetClass,
                        form3,
                        XmNx, 86,
                        XmNy, 652,
                        XmNwidth, 100,
                        XmNheight, 30,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 5,
                        XmNtopOffset, 5,
                        NULL );
        UxPutContext( Text, (char *) UxGeometryDialogContext );


        /* Creation of ResetCurrentButton */
        ResetCurrentButton = XtVaCreateManagedWidget( "ResetCurrentButton",
                        xmPushButtonGadgetClass,
                        form3,
                        XmNx, 210,
                        XmNy, 3,
                        XmNwidth, 158,
                        XmNheight, 20,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Reset WWWW" ),
                        XmNbottomOffset, 5,
                        XmNtopOffset, 5,
                        XmNrecomputeSize, FALSE,
                        NULL );
        UxPutContext( ResetCurrentButton, (char *) UxGeometryDialogContext );

        XtVaSetValues(geometryDialog,
                        XmNdefaultButton, ApplyGeometry,
                        NULL );


        XtAddCallback( geometryDialog, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxGeometryDialogContext);


        return ( geometryDialog );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_geometryDialog()
{
        Widget                  rtrn;
        _UxCgeometryDialog      *UxContext;

        UxGeometryDialogContext = UxContext =
 (_UxCgeometryDialog *) UxNewContext( sizeof(_UxCgeometryDialog), False );


        rtrn = _Uxbuild_geometryDialog();

        return(rtrn);
}
