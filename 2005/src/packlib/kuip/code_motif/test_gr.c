/*
 * $Id: test_gr.c,v 1.1.1.1 1996/03/08 15:33:09 mclareni Exp $
 *
 * $Log: test_gr.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:09  mclareni
 * Kuip
 *
 */
#if defined(CERNLIB_TEST)
/*CMZ :  2.05/09 06/07/94  15.45.16  by  Alfred Nathaniel*/
/*-- Author :*/
/*-- Author :    N.Cremel   10/08/92*/
/*
 * This is only to test commands with graphics area (see KUIPMCDF_TEST,
 * command DOGR).
 */

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/klink.h"

#include "mkincl.h"
#include "mkdims.h"
#include "kuip/mkutfu.h"
#include "mkutfm.h"
#include "mkutdm.h"
#define CONTEXT_MACRO_ACCESS 1
#include "mkuxpl.h"
#include "mkuxpb.h"
#include "mkuxpp.h"
#undef CONTEXT_MACRO_ACCESS
#include "kuip/browh1.h"
#include "browh2.h"


/***********************************************************************
 *                                                                     *
 *   Forward declarations of local functions.                          *
 *                                                                     *
 ***********************************************************************/


/***********************************************************************
 *                                                                     *
 *   Local datas.                                                      *
 *                                                                     *
 ***********************************************************************/

Widget draw_widget;
int wkdraw = 999; /* give special workstation identifier to external drawable */


/*******************************************************************************
        formGraph.c

       Associated Header file: formGraph.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/PushB.h>
#include <Xm/DrawingA.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "formGraph.h" */


/*******************************************************************************
       formGraph.h
       This header file is included by formGraph.c

*******************************************************************************/

#ifndef _FORMGRAPH_INCLUDED
#define _FORMGRAPH_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/PushB.h>
#include <Xm/DrawingA.h>
#include <Xm/Frame.h>
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
        Widget  UxformGraph;
        Widget  UxframeGraph;
        Widget  UxdrawingGraph;
        Widget  UxpushButton1;
        Widget  UxpushButton2;
        swidget UxUxParent;
} _UxCformGraph;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCformGraph           *UxFormGraphContext;
#define formGraph               UxFormGraphContext->UxformGraph
#define frameGraph              UxFormGraphContext->UxframeGraph
#define drawingGraph            UxFormGraphContext->UxdrawingGraph
#define pushButton1             UxFormGraphContext->UxpushButton1
#define pushButton2             UxFormGraphContext->UxpushButton2
#define UxParent                UxFormGraphContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_formGraph();

#endif  /* _FORMGRAPH_INCLUDED */
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget   _Uxbuild_formGraph()
{
        Widget          _UxParent;


        /* Creation of formGraph */
        _UxParent = UxParent;
        if ( _UxParent == NULL )
        {
                _UxParent = UxTopLevel;
        }

        _UxParent = XtVaCreatePopupShell( "formGraph_shell",
                        xmDialogShellWidgetClass, _UxParent,
                        XmNx, 530,
                        XmNy, 520,
                        XmNwidth, 400,
                        XmNheight, 390,
                        XmNshellUnitType, XmPIXELS,
                        XmNtitle, "formGraph",
                        NULL );

        formGraph = XtVaCreateWidget( "formGraph",
                        xmFormWidgetClass,
                        _UxParent,
                        XmNwidth, 400,
                        XmNheight, 390,
                        XmNunitType, XmPIXELS,
                        NULL );
        UxPutContext( formGraph, (char *) UxFormGraphContext );


        /* Creation of frameGraph */
        frameGraph = XtVaCreateManagedWidget( "frameGraph",
                        xmFrameWidgetClass,
                        formGraph,
                        XmNx, 30,
                        XmNy, 100,
                        XmNwidth, 350,
                        XmNheight, 260,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNbottomOffset, 20,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 20,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 20,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 100,
                        NULL );
        UxPutContext( frameGraph, (char *) UxFormGraphContext );


        /* Creation of drawingGraph */
        drawingGraph = XtVaCreateManagedWidget( "drawingGraph",
                        xmDrawingAreaWidgetClass,
                        frameGraph,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 10,
                        XmNy, 0,
                        XmNwidth, 330,
                        XmNheight, 250,
                        NULL );
        UxPutContext( drawingGraph, (char *) UxFormGraphContext );


        /* Creation of pushButton1 */
        pushButton1 = XtVaCreateManagedWidget( "pushButton1",
                        xmPushButtonWidgetClass,
                        formGraph,
                        XmNwidth, 100,
                        XmNheight, 40,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 20,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 20,
                        NULL );
        UxPutContext( pushButton1, (char *) UxFormGraphContext );


        /* Creation of pushButton2 */
        pushButton2 = XtVaCreateManagedWidget( "pushButton2",
                        xmPushButtonWidgetClass,
                        formGraph,
                        XmNwidth, 100,
                        XmNheight, 40,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 20,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNtopOffset, 20,
                        NULL );
        UxPutContext( pushButton2, (char *) UxFormGraphContext );


        XtAddCallback( formGraph, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxFormGraphContext);


        return ( formGraph );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget  create_formGraph( _UxUxParent )
        swidget _UxUxParent;
{
        Widget                  rtrn;
        _UxCformGraph           *UxContext;

        UxFormGraphContext = UxContext =
                (_UxCformGraph *) UxNewContext( sizeof(_UxCformGraph), False );

        UxParent = _UxUxParent;

        rtrn = _Uxbuild_formGraph();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/



/*
 * action routine for /MOTIF/DOGRAPH
 */
int kxdograph()
{
  /* Create widget with drawing area */
  draw_widget = create_formGraph (km_toplevel);
  UxFormGraphContext = (_UxCformGraph *) UxGetContext( draw_widget );
  UxPopupInterface( draw_widget, XtGrabNone );


  /* Pass drawable and display to HIGZ */
  ixsdswi( XtDisplay(draw_widget), XtWindow(drawingGraph) );


  /* Open HIGZ graphics window (special id 999) on top of this drawable */
  /* (and deactivate all other active workstations) */
  iopexw_ (&wkdraw);

  /* Initialize callbacks (Expose, Resize,  EnterWindow, LeaveWindow, ...) */
  km_graph_init_callbacks (drawingGraph, draw_widget, wkdraw , -1);

  return 0;
}

/*
 * action routine for /MOTIF/ACTGRAPH
 */
int kxactgraph()
{
  /* Activate HIGZ graphics window (id is 999 )*/
  iacwk_ (&wkdraw);

  return 0;
}

/*
 * action routine for /MOTIF/DEAGRAPH
 */
int kxdeagraph()
{
  /* Deactivate special extern HIGZ graphics window*/
  idawk_ (&wkdraw);

  return 0;
}

/*
 * action routine for /MOTIF/ENDGRAPH
 */
int kxendgraph()
{
  /* Close special extern HIGZ graphics window */
  iclwk_ (&wkdraw);

  return 0;
}
#endif
