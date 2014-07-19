/*
 * $Id: hatchstyledialog.c,v 1.1.1.1 1996/03/01 11:39:33 mclareni Exp $
 *
 * $Log: hatchstyledialog.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:33  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 05/03/94  14.59.06  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
        hatchStyleDialog.c
        (Generated from interface file hatchStyleDialog.i)
        Associated Resource file: hatchStyleDialog.rf
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>
#include <Xm/Form.h>

/*******************************************************************************
        Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern swidget attributesFormDialog;

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
        Widget  Uxform1;
        Widget  Uxlabelindex;
        Widget  UxseparatorGadget6;
} _UxChatchStyleDialog;

#define form1                   UxHatchStyleDialogContext->Uxform1
#define labelindex              UxHatchStyleDialogContext->Uxlabelindex
#define separatorGadget6        UxHatchStyleDialogContext->UxseparatorGadget6

static _UxChatchStyleDialog     *UxHatchStyleDialogContext;

Widget  hatchStyleDialog;
Widget  DistanceScale;
Widget  Angle1Scale;
Widget  Angle2Scale;
Widget  IndexText;
Widget  ApplyHatchBut;
Widget  ResetHatchBut;
Widget  CloseHatchBut;

/*******************************************************************************
        Forward declarations of functions that are defined later in this file.
*******************************************************************************/

Widget  create_hatchStyleDialog();

/*******************************************************************************
        The following are callback functions.
*******************************************************************************/

/*******************************************************************************
        The 'build_' function creates all the widgets
        using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_hatchStyleDialog()
{
        Widget  hatchStyleDialog_shell;

        hatchStyleDialog_shell = XtVaCreatePopupShell( "hatchStyleDialog_shell",
                        xmDialogShellWidgetClass, attributesFormDialog,
                        XmNx, 485,
                        XmNy, 279,
                        XmNwidth, 304,
                        XmNheight, 400,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "hatchStyleDialog",
                        NULL );

        hatchStyleDialog = XtVaCreateWidget( "hatchStyleDialog",
                        xmFormWidgetClass, hatchStyleDialog_shell,
                        XmNnoResize, TRUE,
                        XmNautoUnmanage, FALSE,
                        XmNheight, 400,
                        XmNwidth, 304,
                        XmNunitType, XmPIXELS,
                        NULL );

        UxPutContext( hatchStyleDialog, (char *) UxHatchStyleDialogContext );

        DistanceScale = XtVaCreateManagedWidget( "DistanceScale",
                        xmScaleWidgetClass, hatchStyleDialog,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNvalue, 0,
                        RES_CONVERT( XmNtitleString,
 "Distance Between each hatch" ),
                        XmNshowValue, TRUE,
                        XmNminimum, 0,
                        XmNmaximum, 9,
                        XmNorientation, XmHORIZONTAL,
                        XmNheight, 70,
                        XmNwidth, 310,
                        XmNy, 10,
                        XmNx, 10,
                        NULL );

        UxPutContext( DistanceScale, (char *) UxHatchStyleDialogContext );

        Angle1Scale = XtVaCreateManagedWidget( "Angle1Scale",
                        xmScaleWidgetClass, hatchStyleDialog,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNvalue, 0,
                        RES_CONVERT( XmNtitleString,
"Angle Between 90 and 180 degrees" ),
                        XmNshowValue, TRUE,
                        XmNminimum, 0,
                        XmNmaximum, 9,
                        XmNorientation, XmHORIZONTAL,
                        XmNheight, 70,
                        XmNwidth, 310,
                        XmNy, 80,
                        XmNx, 10,
                        NULL );

        UxPutContext( Angle1Scale, (char *) UxHatchStyleDialogContext );

        Angle2Scale = XtVaCreateManagedWidget( "Angle2Scale",
                        xmScaleWidgetClass, hatchStyleDialog,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNvalue, 0,
                        RES_CONVERT( XmNtitleString,
"Angle Between 0 and 90 degrees" ),
                        XmNshowValue, TRUE,
                        XmNminimum, 0,
                        XmNmaximum, 9,
                        XmNorientation, XmHORIZONTAL,
                        XmNheight, 70,
                        XmNwidth, 310,
                        XmNy, 150,
                        XmNx, 10,
                        NULL );

        UxPutContext( Angle2Scale, (char *) UxHatchStyleDialogContext );

        form1 = XtVaCreateManagedWidget( "form1",
                        xmFormWidgetClass, hatchStyleDialog,
                        XmNrightOffset, 10,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 10,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNheight, 50,
                        XmNwidth, 310,
                        XmNy, 230,
                        XmNx, 10,
                        XmNresizePolicy, XmRESIZE_NONE,
                        NULL );

        UxPutContext( form1, (char *) UxHatchStyleDialogContext );

        labelindex = XtVaCreateManagedWidget( "labelindex",
                        xmLabelGadgetClass, form1,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Index :" ),
                        XmNheight, 50,
                        XmNwidth, 70,
                        XmNy, 0,
                        XmNx, 0,
                        NULL );

        UxPutContext( labelindex, (char *) UxHatchStyleDialogContext );

        IndexText = XtVaCreateManagedWidget( "IndexText",
                        xmTextWidgetClass, form1,
                        XmNvalue, "000",
                        XmNtopOffset, 5,
                        XmNbottomOffset, 5,
                        XmNleftWidget, labelindex,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 50,
                        XmNwidth, 75,
                        XmNy, 0,
                        XmNx, 75,
                        NULL );

        UxPutContext( IndexText, (char *) UxHatchStyleDialogContext );

        separatorGadget6 = XtVaCreateManagedWidget( "separatorGadget6",
                        xmSeparatorGadgetClass, hatchStyleDialog,
                        XmNrightOffset, 0,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 50,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNheight, 20,
                        XmNwidth, 300,
                        XmNy, 340,
                        XmNx, 0,
                        NULL );

        UxPutContext( separatorGadget6, (char *) UxHatchStyleDialogContext );

        ApplyHatchBut = XtVaCreateManagedWidget( "ApplyHatchBut",
                        xmPushButtonGadgetClass, hatchStyleDialog,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Apply" ),
                        XmNheight, 30,
                        XmNwidth, 80,
                        XmNy, 370,
                        XmNx, 160,
                        NULL );

        UxPutContext( ApplyHatchBut, (char *) UxHatchStyleDialogContext );

        ResetHatchBut = XtVaCreateManagedWidget( "ResetHatchBut",
                        xmPushButtonGadgetClass, hatchStyleDialog,
                        XmNleftWidget, ApplyHatchBut,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Reset" ),
                        XmNheight, 30,
                        XmNwidth, 80,
                        XmNy, 360,
                        XmNx, 170,
                        NULL );

        UxPutContext( ResetHatchBut, (char *) UxHatchStyleDialogContext );

        CloseHatchBut = XtVaCreateManagedWidget( "CloseHatchBut",
                        xmPushButtonGadgetClass, hatchStyleDialog,
                        XmNleftWidget, ResetHatchBut,
                        XmNleftOffset, 5,
                        XmNleftAttachment, XmATTACH_WIDGET,
                        XmNbottomOffset, 10,
                        XmNbottomAttachment, XmATTACH_FORM,
                        RES_CONVERT( XmNlabelString, "Close" ),
                        XmNheight, 30,
                        XmNwidth, 80,
                        XmNy, 360,
                        XmNx, 20,
                        NULL );

        UxPutContext( CloseHatchBut, (char *) UxHatchStyleDialogContext );

        XtAddCallback( hatchStyleDialog, XmNdestroyCallback,
                        UxFreeClientDataCB,
                        (XtPointer) UxHatchStyleDialogContext );

        XtVaSetValues(hatchStyleDialog,
                        XmNdefaultButton, ApplyHatchBut,
                        NULL );



        return ( hatchStyleDialog );
}

/*******************************************************************************
        The following function includes the code that was entered
        in the 'Initial Code' and 'Final Code' sections of the
        Declarations Editor. This function is called from the
        'Interface function' below.
*******************************************************************************/

static Widget   _Ux_create_hatchStyleDialog()
{
        Widget                  rtrn;
        _UxChatchStyleDialog    *UxContext;

        UxHatchStyleDialogContext = UxContext =
 (_UxChatchStyleDialog *) XtMalloc( sizeof(_UxChatchStyleDialog) );

        rtrn = _Uxbuild_hatchStyleDialog();

        return(rtrn);
}

/*******************************************************************************
        The following is the 'Interface function' which is the
        external entry point for creating this interface.
        This function should be called from your application or from
        a callback function.
*******************************************************************************/

Widget  create_hatchStyleDialog()
{
        Widget                  _Uxrtrn;
        static int              _Uxinit = 0;

        if ( ! _Uxinit )
        {
                UxLoadResources( "hatchStyleDialog.rf" );
                _Uxinit = 1;
        }

        _Uxrtrn = _Ux_create_hatchStyleDialog();

        return ( _Uxrtrn );
}

/*******************************************************************************
        END OF FILE
*******************************************************************************/

