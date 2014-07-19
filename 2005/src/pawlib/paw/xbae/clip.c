/*
 * $Id: clip.c,v 1.1.1.1 1996/03/01 11:39:36 mclareni Exp $
 *
 * $Log: clip.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:36  mclareni
 * Paw
 *
 */
/*CMZ :  2.05/24 01/11/94  00.38.33  by  Fons Rademakers*/
/*-- Author :*/
/*
 * Copyright(c) 1992 Bell Communications Research, Inc. (Bellcore)
 *                        All rights reserved
 * Permission to use, copy, modify and distribute this material for
 * any purpose and without fee is hereby granted, provided that the
 * above copyright notice and this permission notice appear in all
 * copies, and that the name of Bellcore not be used in advertising
 * or publicity pertaining to this material without the specific,
 * prior written permission of an authorized representative of
 * Bellcore.
 *
 * BELLCORE MAKES NO REPRESENTATIONS AND EXTENDS NO WARRANTIES, EX-
 * PRESS OR IMPLIED, WITH RESPECT TO THE SOFTWARE, INCLUDING, BUT
 * NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR ANY PARTICULAR PURPOSE, AND THE WARRANTY AGAINST IN-
 * FRINGEMENT OF PATENTS OR OTHER INTELLECTUAL PROPERTY RIGHTS.  THE
 * SOFTWARE IS PROVIDED "AS IS", AND IN NO EVENT SHALL BELLCORE OR
 * ANY OF ITS AFFILIATES BE LIABLE FOR ANY DAMAGES, INCLUDING ANY
 * LOST PROFITS OR OTHER INCIDENTAL OR CONSEQUENTIAL DAMAGES RELAT-
 * ING TO THE SOFTWARE.
 *
 * ClipWidget Author: Andrew Wason, Bellcore, aw@bae.bellcore.com
 */

/*
 * Clip.c - private child of Matrix - used to clip Matrix's textField child
 */

#include <X11/StringDefs.h>
#include <Xm/XmP.h>
/* #include <Xbae/ClipP.h> */
#include "clipp.h"

#ifndef SABER
static char sccsid[] = "@(#)Clip.c      3.4 5/13/92";
#endif

static char defaultTranslations[] =
    "<FocusIn>:                 FocusIn()";

static XtResource resources[] = {
    { XmNexposeProc, XmCFunction, XtRFunction, sizeof(XtExposeProc),
          XtOffsetOf(XbaeClipRec, clip.expose_proc),
          XtRFunction, (XtPointer) NULL },
    { XmNfocusCallback, XmCCallback, XmRCallback, sizeof(XtCallbackList),
          XtOffsetOf(XbaeClipRec, clip.focus_callback),
          XtRImmediate, (XtPointer) NULL },
};

/*
 * Declaration of methods
 */
static void ClassPartInitialize();
static void Realize();
static void Redisplay();
static void Redraw();

/*
 * Public convenience function
 */
void XbaeClipRedraw();

/*
 * Clip actions
 */
static void FocusInACT();

static XtActionsRec actions[] =
{
    {"FocusIn", FocusInACT},
};

XbaeClipClassRec xbaeClipClassRec = {
    {
    /* core_class fields */
        /* superclass           */ (WidgetClass) &xmPrimitiveClassRec,
        /* class_name           */ "XbaeClip",
        /* widget_size          */ sizeof(XbaeClipRec),
        /* class_initialize     */ NULL,
        /* class_part_initialize*/ ClassPartInitialize,
        /* class_inited         */ False,
        /* initialize           */ NULL,
        /* initialize_hook      */ NULL,
        /* realize              */ Realize,
        /* actions              */ actions,
        /* num_actions          */ XtNumber(actions),
        /* resources            */ resources,
        /* num_resources        */ XtNumber(resources),
        /* xrm_class            */ NULLQUARK,
        /* compress_motion      */ True,
        /* compress_exposure    */ XtExposeCompressSeries |
                                        XtExposeGraphicsExpose |
                                            XtExposeNoExpose,
        /* compress_enterleave  */ True,
        /* visible_interest     */ False,
        /* destroy              */ NULL,
        /* resize               */ NULL,
        /* expose               */ Redisplay,
        /* set_values           */ NULL,
        /* set_values_hook      */ NULL,
        /* set_values_almost    */ XtInheritSetValuesAlmost,
        /* get_values_hook      */ NULL,
        /* accept_focus         */ NULL,
        /* version              */ XtVersion,
        /* callback_private     */ NULL,
        /* tm_table             */ defaultTranslations,
        /* query_geometry       */ NULL,
        /* display_accelerator  */ NULL,
        /* extension            */ NULL
    },
    /* primitive_class fields */
    {
        /* border_highlight     */ NULL,
        /* border_unhighlight   */ NULL,
        /* translations         */ NULL,
        /* arm_and_activate     */ NULL,
        /* syn_resources        */ NULL,
        /* num_syn_resources    */ 0,
        /* extension            */ NULL
    },
    /* clip_class fields */
    {
        /* redraw               */ Redraw,
        /* extension            */ NULL,
    }
};

WidgetClass xbaeClipWidgetClass = (WidgetClass) & xbaeClipClassRec;


static void
ClassPartInitialize(cwc)
XbaeClipWidgetClass cwc;
{
    register XbaeClipWidgetClass super =
        (XbaeClipWidgetClass) cwc->core_class.superclass;

    /*
     * Allow subclasses to inherit our redraw method
     */
    if (cwc->clip_class.redraw == XbaeInheritRedraw)
        cwc->clip_class.redraw = super->clip_class.redraw;
}

static void
Realize(cw, valueMask, attributes)
XbaeClipWidget cw;
XtValueMask *valueMask;
XSetWindowAttributes *attributes;
{
    /*
     * Don't call our superclasses realize method, because Primitive sets
     * bit_gravity and do_not_propagate
     */
    XtCreateWindow((Widget)cw, InputOutput, CopyFromParent,
                   *valueMask, attributes);
}

/* ARGSUSED */
static void
Redisplay(cw, event, region)
XbaeClipWidget cw;
XEvent *event;
Region region;
{
    if (cw->clip.expose_proc)
        cw->clip.expose_proc((Widget)cw, event, region);
}

/*
 * Clip redraw method
 */
/* ARGSUSED */
static void
Redraw(cw)
XbaeClipWidget cw;
{
    /*
     * Clear the window generating Expose events.
     * XXX It might be more efficient to fake up an Expose event
     * and call Redisplay directly
     */
    if (XtIsRealized(cw))
        XClearArea(XtDisplay(cw), XtWindow(cw),
                   0, 0,
                   0 /*Full Width*/, 0 /*Full Height*/,
                   True);
}

/*
 * Public interface to redraw method
 */
void
XbaeClipRedraw(w)
Widget w;
{
    /*
     * Make sure w is a Clip or a subclass
     */
    XtCheckSubclass(w, xbaeClipWidgetClass, NULL);

    /*
     * Call the redraw method
     */
    if (XtIsRealized(w))
        (*((XbaeClipWidgetClass) XtClass(w))->clip_class.redraw)((Widget)w);
          /* ((XbaeClipWidget)w); */
}

/* ARGSUSED */
static void
FocusInACT(cw, event, params, nparams)
XbaeClipWidget cw;
XEvent *event;
String *params;
Cardinal *nparams;
{
    if (event->xany.type != FocusIn || !event->xfocus.send_event)
        return;

    if (cw->clip.focus_callback)
        XtCallCallbackList((Widget)cw, cw->clip.focus_callback, NULL);
}
