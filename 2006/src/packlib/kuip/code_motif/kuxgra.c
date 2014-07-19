/*
 * $Id: kuxgra.c,v 1.1.1.1 1996/03/08 15:33:10 mclareni Exp $
 *
 * $Log: kuxgra.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:10  mclareni
 * Kuip
 *
 */
/*CMZ :  2.03/14 03/09/93  15.19.15  by  Nicole Cremel*/
/*-- Author :    N.Cremel   07/08/92*/
/*
 * kuipGraphics.c
 */

#include "kuip/kuip.h"

/*******************************************************************************
        kuipGraphics.c

       Associated Header file: kuipGraphics.h
*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
/* #include "UxXt.h" */
#include "mkuxxt.h"

#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/MainW.h>



/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
/* #include "kuipGraphics.h" */
#include "mkuxgr.h"
#undef CONTEXT_MACRO_ACCESS

Widget        km_graphicsDraw;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget        _Uxbuild_kuipGraphics()
{
        Widget                _UxParent;


        /* Creation of kuipGraphics */
        _UxParent = XtVaCreatePopupShell( "kuipGraphics_shell",
                        topLevelShellWidgetClass, UxTopLevel,
                        XmNx, 570,
                        XmNy, 10,
                        XmNwidth, 530,
                        XmNheight, 550,
                        XmNtitle, "kuipGraphics",
                        XmNiconName, "kuipGraphics",
                        NULL );

        kuipGraphics = XtVaCreateManagedWidget( "kuipGraphics",
                        xmMainWindowWidgetClass,
                        _UxParent,
                        XmNwidth, 530,
                        XmNheight, 550,
                        XmNmappedWhenManaged, TRUE,
                        XmNborderWidth, 0,
                        NULL );
        UxPutContext( kuipGraphics, (char *) UxKuipGraphicsContext );


        /* Creation of kuipGraphicsFrame */
        kuipGraphicsFrame = XtVaCreateManagedWidget( "kuipGraphicsFrame",
                        xmFrameWidgetClass,
                        kuipGraphics,
                        NULL );
        UxPutContext( kuipGraphicsFrame, (char *) UxKuipGraphicsContext );


        /* Creation of graphicsForm */
        graphicsForm = XtVaCreateManagedWidget( "graphicsForm",
                        xmFormWidgetClass,
                        kuipGraphicsFrame,
                        XmNautoUnmanage, FALSE,
                        XmNborderWidth, 0,
                        XmNmarginHeight, 0,
                        XmNmarginWidth, 0,
                        XmNhorizontalSpacing, 0,
                        XmNverticalSpacing, 0,
                        NULL );
        UxPutContext( graphicsForm, (char *) UxKuipGraphicsContext );


        /* Creation of km_graphicsDraw */
        km_graphicsDraw = XtVaCreateManagedWidget( "km_graphicsDraw",
                        xmDrawingAreaWidgetClass,
                        graphicsForm,
                        XmNresizePolicy, XmRESIZE_NONE,
                        XmNx, 0,
                        XmNy, 10,
                        XmNwidth, 530,
                        XmNheight, 540,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNrightOffset, 0,
                        XmNtopOffset, 0,
                        XmNbottomOffset, 0,
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNleftOffset, 0,
                        XmNtopAttachment, XmATTACH_FORM,
                        NULL );
        UxPutContext( km_graphicsDraw, (char *) UxKuipGraphicsContext );


        XtAddCallback( kuipGraphics, XmNdestroyCallback,
                (XtCallbackProc) UxDestroyContextCB,
                (XtPointer) UxKuipGraphicsContext);

        XmMainWindowSetAreas( kuipGraphics, (Widget) NULL, (Widget) NULL,
                        (Widget) NULL, (Widget) NULL, kuipGraphicsFrame );

        return ( kuipGraphics );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget        create_kuipGraphics()
{
        Widget                  rtrn;
        _UxCkuipGraphics        *UxContext;

        UxKuipGraphicsContext = UxContext =
          (_UxCkuipGraphics *) UxNewContext( sizeof(_UxCkuipGraphics), False );


        rtrn = _Uxbuild_kuipGraphics();

        return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

