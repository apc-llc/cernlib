/*
 * $Id: kuxpap.c,v 1.1.1.1 1996/03/08 15:33:11 mclareni Exp $
 *
 * $Log: kuxpap.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:11  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/03 29/11/94  14.13.42  by  N.Cremel*/
/*-- Author :    N.Cremel   13/08/92*/
/*
 * PalPanel.c
 */

/*******************************************************************************
       PalPanel.c

       Associated Header file: PalPanel.h
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
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "PalPanel.h" */
#include "mkuxpp.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget       _Uxbuild_PalPanel()
{
       Widget              _UxParent;


       /* Creation of PalPanel */
       _UxParent = UxParent;
       if ( _UxParent == NULL )
       {
              _UxParent = XtVaCreatePopupShell( "PalPanel_shell",
                     topLevelShellWidgetClass, UxTopLevel,
                     XmNwidth, 300,
                     XmNheight, 250,
                     XmNshellUnitType, XmPIXELS,
                     XmNtitle, "PalPanel",
                     XmNiconName, "PalPanel",
                     NULL );

       }

       /* PalPanel = XtVaCreateManagedWidget( "PalPanel", */
       PalPanel = XtVaCreateWidget( "PalPanel",
                     xmFormWidgetClass,
                     _UxParent,
                     XmNresizePolicy, XmRESIZE_NONE,
                     XmNunitType, XmPIXELS,
                     XmNwidth, 300,
                     XmNheight, 250,
                     NULL );
       UxPutContext( PalPanel, (char *) UxPalPanelContext );


       /* Creation of PNPframe1 */
       PNPframe1 = XtVaCreateManagedWidget( "PNPframe1",
                     xmFrameWidgetClass,
                     PalPanel,
                     XmNshadowThickness, 4,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNbottomOffset, 0,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 0,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 0,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNtopOffset, 0,
                     NULL );
       UxPutContext( PNPframe1, (char *) UxPalPanelContext );


       /* Creation of PNPform1 */
       PNPform1 = XtVaCreateManagedWidget( "PNPform1",
                     xmFormWidgetClass,
                     PNPframe1,
                     XmNresizePolicy, XmRESIZE_NONE,
                     XmNheight, 30,
                     NULL );
       UxPutContext( PNPform1, (char *) UxPalPanelContext );


       /* Creation of km_scrollMI */
       km_scrollMI = XtVaCreateManagedWidget( "km_scrollMI",
                     xmScrolledWindowWidgetClass,
                     PNPform1,
                     XmNscrollingPolicy, XmAUTOMATIC,
                     XmNx, 0,
                     XmNy, 40,
                     XmNwidth, 390,
                     XmNheight, 150,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNbottomOffset, 5,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 5,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 5,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNtopOffset, 40,
                     NULL );
       UxPutContext( km_scrollMI, (char *) UxPalPanelContext );


       /* Creation of km_rowcolMI */
       km_rowcolMI = XtVaCreateManagedWidget( "km_rowcolMI",
                     xmRowColumnWidgetClass,
                     km_scrollMI,
                     XmNadjustLast, FALSE,
                     XmNorientation, XmHORIZONTAL,
                     XmNpacking, XmPACK_COLUMN,
                     NULL );
       UxPutContext( km_rowcolMI, (char *) UxPalPanelContext );


       /* Creation of km_scrollMN */
       km_scrollMN = XtVaCreateManagedWidget( "km_scrollMN",
                     xmScrolledWindowWidgetClass,
                     PNPform1,
                     XmNscrollingPolicy, XmAUTOMATIC,
                     XmNx, 0,
                     XmNy, 40,
                     XmNwidth, 390,
                     XmNheight, 150,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNbottomOffset, 5,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 5,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 5,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNtopOffset, 40,
                     NULL );
       UxPutContext( km_scrollMN, (char *) UxPalPanelContext );


       /* Creation of km_rowcolMN */
       km_rowcolMN = XtVaCreateManagedWidget( "km_rowcolMN",
                     xmRowColumnWidgetClass,
                     km_scrollMN,
                     XmNadjustLast, FALSE,
                     XmNorientation, XmHORIZONTAL,
                     XmNpacking, XmPACK_COLUMN,
                     NULL );
       UxPutContext( km_rowcolMN, (char *) UxPalPanelContext );


       /* Creation of km_scrollMB */
       km_scrollMB = XtVaCreateManagedWidget( "km_scrollMB",
                     xmScrolledWindowWidgetClass,
                     PNPform1,
                     XmNscrollingPolicy, XmAUTOMATIC,
                     XmNx, 0,
                     XmNy, 40,
                     XmNwidth, 390,
                     XmNheight, 150,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNbottomOffset, 5,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 5,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 5,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNtopOffset, 40,
                     NULL );
       UxPutContext( km_scrollMB, (char *) UxPalPanelContext );


       /* Creation of km_rowcolMB */
       km_rowcolMB = XtVaCreateManagedWidget( "km_rowcolMB",
                     xmRowColumnWidgetClass,
                     km_scrollMB,
                     XmNadjustLast, FALSE,
                     XmNorientation, XmHORIZONTAL,
                     XmNpacking, XmPACK_COLUMN,
                     NULL );
       UxPutContext( km_rowcolMB, (char *) UxPalPanelContext );


       /* Creation of km_scrollMC */
       km_scrollMC = XtVaCreateManagedWidget( "km_scrollMC",
                     xmScrolledWindowWidgetClass,
                     PNPform1,
                     XmNscrollingPolicy, XmAUTOMATIC,
                     XmNx, 0,
                     XmNy, 40,
                     XmNwidth, 390,
                     XmNheight, 150,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNbottomOffset, 5,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 5,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 5,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNtopOffset, 40,
                     NULL );
       UxPutContext( km_scrollMC, (char *) UxPalPanelContext );


       /* Creation of km_rowcolMC */
       km_rowcolMC = XtVaCreateManagedWidget( "km_rowcolMC",
                     xmRowColumnWidgetClass,
                     km_scrollMC,
                     XmNadjustLast, FALSE,
                     XmNorientation, XmHORIZONTAL,
                     XmNpacking, XmPACK_COLUMN,
                     NULL );
       UxPutContext( km_rowcolMC, (char *) UxPalPanelContext );


       /* Creation of km_scrollM1 */
       km_scrollM1 = XtVaCreateManagedWidget( "km_scrollM1",
                     xmScrolledWindowWidgetClass,
                     PNPform1,
                     XmNscrollingPolicy, XmAUTOMATIC,
                     XmNx, 0,
                     XmNy, 40,
                     XmNwidth, 390,
                     XmNheight, 150,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNbottomOffset, 5,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 5,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 5,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNtopOffset, 40,
                     NULL );
       UxPutContext( km_scrollM1, (char *) UxPalPanelContext );


       /* Creation of km_rowcolM1 */
       km_rowcolM1 = XtVaCreateManagedWidget( "km_rowcolM1",
                     xmRowColumnWidgetClass,
                     km_scrollM1,
                     XmNadjustLast, FALSE,
                     XmNorientation, XmHORIZONTAL,
                     XmNpacking, XmPACK_COLUMN,
                     NULL );
       UxPutContext( km_rowcolM1, (char *) UxPalPanelContext );


       /* Creation of arrowButtonP */
       arrowButtonP = XtVaCreateManagedWidget( "arrowButtonP",
                     xmArrowButtonWidgetClass,
                     PNPform1,
                     XmNwidth, 20,
                     XmNarrowDirection, XmARROW_DOWN,
                     XmNhighlightThickness, 0,
                     XmNshadowThickness, 0,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNleftOffset, 5,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNtopOffset, 10,
                     XmNheight, 20,
                     NULL );
       UxPutContext( arrowButtonP, (char *) UxPalPanelContext );


       /* Creation of km_palLabelP */
       km_palLabelP = XtVaCreateManagedWidget( "km_palLabelP",
                     xmLabelWidgetClass,
                     PNPform1,
                     RES_CONVERT( XmNlabelString, "Logon PANEL" ),
                     XmNleftAttachment, XmATTACH_WIDGET,
                     XmNleftOffset, 10,
                     XmNleftWidget, arrowButtonP,
                     XmNrightAttachment, XmATTACH_FORM,
                     XmNrightOffset, 10,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNtopOffset, 10,
                     XmNheight, 20,
                     NULL );
       UxPutContext( km_palLabelP, (char *) UxPalPanelContext );


       XtAddCallback( PalPanel, XmNdestroyCallback,
              (XtCallbackProc) UxDestroyContextCB,
              (XtPointer) UxPalPanelContext);


       return ( PalPanel );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget       create_PalPanel( _UxUxParent )
       swidget       _UxUxParent;
{
       Widget                  rtrn;
       _UxCPalPanel            *UxContext;

       UxPalPanelContext = UxContext =
              (_UxCPalPanel *) UxNewContext( sizeof(_UxCPalPanel), False );

       UxParent = _UxUxParent;

       rtrn = _Uxbuild_PalPanel();

       return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

