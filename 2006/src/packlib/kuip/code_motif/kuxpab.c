/*
 * $Id: kuxpab.c,v 1.1.1.1 1996/03/08 15:33:10 mclareni Exp $
 *
 * $Log: kuxpab.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:10  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/17 19/09/94  09.48.53  by  N.Cremel*/
/*-- Author :    N.Cremel   13/08/92*/
/*
 * PalButton.c
 */

/*******************************************************************************
       PalButton.c

       Associated Header file: PalButton.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/Label.h>
#include <Xm/ArrowB.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "PalButton.h" */
#include "mkuxpb.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget       _Uxbuild_PalButton()
{
       Widget              _UxParent;


       /* Creation of PalButton */
       _UxParent = PalParent;
       if ( _UxParent == NULL )
       {
              _UxParent = XtVaCreatePopupShell( "PalButton_shell",
                     topLevelShellWidgetClass, UxTopLevel,
                     XmNx, 10,
                     XmNy, 20,
                     XmNwidth, 300,
                     XmNheight, 50,
                     XmNshellUnitType, XmPIXELS,
                     XmNtitle, "PalButton",
                     XmNiconName, "PalButton",
                     NULL );

       }

       /* PalButton = XtVaCreateManagedWidget( "PalButton", */
       PalButton = XtVaCreateWidget( "PalButton",
                     xmFormWidgetClass,
                     _UxParent,
                     XmNresizePolicy, XmRESIZE_NONE,
                     XmNunitType, XmPIXELS,
                     XmNwidth, 300,
                     XmNheight, 50,
                     NULL );
       UxPutContext( PalButton, (char *) UxPalButtonContext );


       /* Creation of PNBframe1 */
       PNBframe1 = XtVaCreateManagedWidget( "PNBframe1",
                     xmFrameWidgetClass,
                     PalButton,
                     XmNshadowThickness, 4,
                     XmNheight, 30,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNbottomOffset, 0,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 0,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 0,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNtopOffset, 0,
                     NULL );
       UxPutContext( PNBframe1, (char *) UxPalButtonContext );


       /* Creation of PNBform1 */
       PNBform1 = XtVaCreateManagedWidget( "PNBform1",
                     xmFormWidgetClass,
                     PNBframe1,
                     XmNresizePolicy, XmRESIZE_NONE,
                     XmNheight, 30,
                     XmNwidth, 390,
                     NULL );
       UxPutContext( PNBform1, (char *) UxPalButtonContext );


       /* Creation of arrowButtonB */
       arrowButtonB = XtVaCreateManagedWidget( "arrowButtonB",
                     xmArrowButtonWidgetClass,
                     PNBform1,
                     XmNwidth, 20,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNbottomOffset, 2,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 5,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNtopOffset, 2,
                     XmNarrowDirection, XmARROW_RIGHT,
                     XmNhighlightThickness, 0,
                     XmNshadowThickness, 0,
                     XmNheight, 20,
                     NULL );
       UxPutContext( arrowButtonB, (char *) UxPalButtonContext );


       /* Creation of km_palLabelB */
       km_palLabelB = XtVaCreateManagedWidget( "km_palLabelB",
                     xmLabelWidgetClass,
                     PNBform1,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNbottomOffset, 2,
                     RES_CONVERT( XmNlabelString, "Logon PANEL" ),
                     XmNleftAttachment, XmATTACH_WIDGET,
                     XmNleftOffset, 20,
                     XmNleftWidget, arrowButtonB,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 5,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNtopOffset, 2,
                     NULL );
       UxPutContext( km_palLabelB, (char *) UxPalButtonContext );


       XtAddCallback( PalButton, XmNdestroyCallback,
              (XtCallbackProc) UxDestroyContextCB,
              (XtPointer) UxPalButtonContext);


       return ( PalButton );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget       create_PalButton( _UxPalParent )
       swidget       _UxPalParent;
{
       Widget                  rtrn;
       _UxCPalButton           *UxContext;

       UxPalButtonContext = UxContext =
              (_UxCPalButton *) UxNewContext( sizeof(_UxCPalButton), False );

       PalParent = _UxPalParent;

       rtrn = _Uxbuild_PalButton();

       return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

