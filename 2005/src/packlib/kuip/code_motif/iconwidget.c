/*
 * $Id: iconwidget.c,v 1.1.1.1 1996/03/08 15:33:10 mclareni Exp $
 *
 * $Log: iconwidget.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:10  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/10 03/03/95  16.31.10  by  N.Cremel*/
/*-- Author :    Nicole Cremel   27/05/92*/
/*
 * IconWidget.c
 */

/*
 * Copyright 1991 Addison-Wesley Publishing Company
 *                (see the file "COPYRIGHTS" for details)
 */

#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <Xm/Label.h>
#include <Xm/LabelP.h>
/* #include "IconWidget.h" */
#include "icwid.h"
/* #include "IconWidgetP.h" */
#include "icwidp.h"

/* _Xm routine definitions  */
void    _XmHighlightBorder ();
void   _XmUnhighlightBorder ();
void   _XmDrawShadow ();

/* Motif1.1 ( except apollo, hpux has _XmPrimitive... with two args only
 * Gunter 30-jan-95
 */
#if XmVERSION == 1 && XmREVISION == 1
#if !defined(__hpux) && !defined(__apollo)
#define _XmPrimitive_has_two_args
#endif
#endif

/*  Static routine definitions  */

static void    Initialize ();
static Boolean SetValues ();
static void    Redisplay ();
static void    Destroy ();

static void    Arm ();
static void    Activate ();
static void    Disarm ();
static void    Enter ();
static void    Leave ();
static void    Help();

static void    GetPixmapSize ();
static void    DrawShadow ();

/*  Default translation table and action list  */

static char defaultTranslations[] =
    "<EnterWindow>:     Enter()		\n\
     <LeaveWindow>:     Leave()";

static XtActionsRec actionsList[] =
{
  { "Arm", 	   (XtActionProc) Arm	    },
  { "Activate",    (XtActionProc) Activate  },
  { "Disarm", 	   (XtActionProc) Disarm    },
  { "Enter",       (XtActionProc) Enter     },
  { "Leave",       (XtActionProc) Leave     },
  { "Help",        (XtActionProc) Help	    }
};

static XtResource resources[] =
{
   {
     XmNshadowThickness, XmCShadowThickness, XmRShort, sizeof (short),
     XtOffset (XmPrimitiveWidget, primitive.shadow_thickness),
     XmRImmediate, (caddr_t) 2
   },

   {
     XmNiconOffset, XmCIconOffset, XmRShort, sizeof(short),
     XtOffset (IconWidget, icon.offset),
     XmRImmediate, (caddr_t) 0
   },

   {
     XmNshadowType, XmCShadowType, XmRShadowType, sizeof(unsigned char),
     XtOffset (IconWidget, icon.shadow_type),
     XmRImmediate, (caddr_t) XmSHADOW_OUT
   },

   {
     XmNactivateCallback, XmCCallback, XmRCallback, sizeof(XtCallbackList),
     XtOffset (IconWidget, icon.activate_callback),
     XmRPointer, (caddr_t) NULL
   },

   {
     XmNarmCallback, XmCCallback, XmRCallback, sizeof(XtCallbackList),
     XtOffset (IconWidget, icon.arm_callback),
     XmRPointer, (caddr_t) NULL
   },

   {
     XmNdisarmCallback, XmCCallback, XmRCallback, sizeof(XtCallbackList),
     XtOffset (IconWidget, icon.disarm_callback),
     XmRPointer, (caddr_t) NULL
   },

};

/*  The IconWidget class record definition  */

IconWidgetClassRec iconWidgetClassRec =
{
   {
      (WidgetClass) &xmLabelClassRec,   /* superclass            */	
      "IconWidget",                     /* class_name	         */	
      sizeof(IconWidgetRec),            /* widget_size	         */	
      NULL,                             /* class_initialize      */
      NULL,                             /* class_part_initialize */
      FALSE,                            /* class_inited          */	
      (XtInitProc) Initialize,          /* initialize	         */	
      NULL,                             /* initialize_hook       */
      XtInheritRealize,                 /* realize	         */	
      actionsList,                      /* actions               */	
      XtNumber(actionsList),            /* num_actions    	 */	
      resources,                        /* resources	         */	
      XtNumber(resources),              /* num_resources         */	
      NULLQUARK,                        /* xrm_class	         */	
      TRUE,                             /* compress_motion       */	
      TRUE,                             /* compress_exposure     */	
      TRUE,                             /* compress_enterleave   */
      FALSE,                            /* visible_interest      */	
      Destroy,                          /* destroy               */	
      XtInheritResize,                  /* resize                */
      (XtExposeProc) Redisplay,         /* expose                */	
      (XtSetValuesFunc) SetValues,      /* set_values	         */	
      NULL,                             /* set_values_hook       */
      XtInheritSetValuesAlmost,         /* set_values_almost     */
      NULL,                             /* get_values_hook       */
      NULL,                             /* accept_focus	         */	
      XtVersion,                        /* version               */
      NULL,                             /* callback private      */
      defaultTranslations,              /* tm_table              */
      NULL,                             /* query_geometry        */
      NULL,				/* display_accelerator   */
      NULL,				/* extension             */
   },

   {
      (XtWidgetProc) _XtInherit,   	/* Primitive border_highlight   */
      (XtWidgetProc) _XtInherit,   	/* Primitive border_unhighlight */
      XtInheritTranslations,            /* translations                 */
      NULL,		                /* arm_and_activate             */
      NULL,				/* get resources      		*/
      0,				/* num get_resources  		*/
      NULL,         			/* extension                    */
   },

   {
      (XtWidgetProc) _XtInherit,        /* SetOverrideCallback     */
      (XmMenuProc)   _XtInherit,        /* SetWhichButton          */
      XtInheritTranslations,            /* menu traversal xlation  */
      NULL,                             /* extension               */
   },

   {
      NULL,         			/* extension                    */
   }
};

WidgetClass iconWidgetClass = (WidgetClass) &iconWidgetClassRec;


/************************************************************************
 *
 *  Initialize
 *     Get geometry if pixmap already there
 *
 ************************************************************************/

static void Initialize (request, new)
    IconWidget request, new;

{
    if (new -> icon.shadow_type != XmSHADOW_IN   &&
        new -> icon.shadow_type != XmSHADOW_OUT)
    {
       new -> icon.shadow_type = XmSHADOW_OUT;
    }

    GetPixmapSize (new);
    new->icon.armed = False;
}

/************************************************************************
 *
 *  GetPixmapSize
 *     Get geometry if pixmap already there
 *
 ************************************************************************/

static void GetPixmapSize (iw)
    IconWidget iw;
{
    Window     root;
    int        junk;
    unsigned int w, h, junk1;

    if (iw->label.pixmap != XmUNSPECIFIED_PIXMAP) {
        XGetGeometry (XtDisplay (iw), (Pixmap) iw->label.pixmap,
                      &root, &junk, &junk,
                      &w, &h,
                      &junk1, &junk1);
        iw->icon.width = (short) w;
        iw->icon.height = (short) h;
    }
}

/************************************************************************
 *
 *  Redisplay
 *     Handle expose events
 *
 ************************************************************************/

static void Redisplay(w, event, region)
    Widget w;
    XEvent *event;
    Region region;
{
    IconWidget iw = (IconWidget) w;
    short      shad = iw->primitive.shadow_thickness +
                      iw->primitive.highlight_thickness;
    short      avail;


    /* use the label's expose first */
    (* iw->core.widget_class->core_class.superclass->core_class.expose)
            (w, event, region);

    /* copy the pixmap into the space left by margin_top */
    if (iw->label.pixmap != XmUNSPECIFIED_PIXMAP) {
        avail = iw->core.width - 2 * shad - 2 * iw->label.margin_width -
            iw->label.margin_left - iw->label.margin_right - iw->icon.width;
        XCopyArea (XtDisplay (w), iw->label.pixmap, XtWindow (w),
                   iw->label.normal_GC, 0, 0,
                   iw->icon.width,
                   (int) iw->icon.height > (int) iw->label.margin_top ?
                                iw->label.margin_top : iw->icon.height,
                   avail/2 + shad +
                     iw->label.margin_width - iw->label.margin_left,
                   iw->label.TextRect.y - iw->icon.height -
                     iw->icon.offset);
    }

    /* draw the shadow */
    DrawShadow (iw, True);

    /* for keyboard traversal */
    if (iw->primitive.highlighted)
        _XmHighlightBorder(w);
    else if (_XmDifferentBackground (w, XtParent (w)))
        _XmUnhighlightBorder(w);
}

/************************************************************************
 *
 *  DrawShadow
 *
 ************************************************************************/

static void DrawShadow (iw, really)
    IconWidget   iw;
    Boolean      really;
{
    Boolean      in = (iw->icon.armed && really) ||
                      iw->icon.shadow_type == XmSHADOW_IN;

    if ((iw->primitive.shadow_thickness) > 0 && XtIsRealized (iw)) {
        _XmDrawShadow (XtDisplay (iw), XtWindow (iw),
                       in ? iw->primitive.bottom_shadow_GC :
                            iw->primitive.top_shadow_GC,
                       in ? iw->primitive.top_shadow_GC :
                            iw->primitive.bottom_shadow_GC,
                       iw->primitive.shadow_thickness,
                       iw->primitive.highlight_thickness,
                       iw->primitive.highlight_thickness,
               (int)iw->core.width-2*iw->primitive.highlight_thickness,
               (int)iw->core.height-2*iw->primitive.highlight_thickness);
    }
}



/************************************************************************
 *
 *  SetValues
 *     Update geometry, pixmap might have changed
 *
 ************************************************************************/

static Boolean SetValues(current, request, neww)
    Widget current, request, neww;
{
    IconWidget cur = (IconWidget) current;
    IconWidget new = (IconWidget) neww;
    IconWidget req = (IconWidget) request;

    if (new -> icon.shadow_type != XmSHADOW_IN   &&
        new -> icon.shadow_type != XmSHADOW_OUT)
    {
       new -> icon.shadow_type = XmSHADOW_OUT;
    }

    GetPixmapSize (new);

    if (new->icon.shadow_type != cur->icon.shadow_type ||
        new->primitive.highlight_thickness !=
          cur->primitive.highlight_thickness           ||
        new->primitive.shadow_thickness !=
          cur->primitive.shadow_thickness)
    {
       return True;
    }else{
       return False;
    }
}


/************************************************************************
 *
 *  Destroy
 *	Clean up allocated resources when the widget is destroyed.
 *
 ************************************************************************/

static void Destroy (iw)
    IconWidget iw;

{
    XtRemoveAllCallbacks ((Widget) iw, XmNactivateCallback);
    XtRemoveAllCallbacks ((Widget) iw, XmNarmCallback);
    XtRemoveAllCallbacks ((Widget) iw, XmNdisarmCallback);
}

/************************************************************************
 *
 *  Enter
 *
 ************************************************************************/

static void Enter (iw, event)
    IconWidget iw;
    XEvent * event;

{
    String *params;
    Cardinal num_params;

#ifdef _XmPrimitive_has_two_args
    _XmPrimitiveEnter ((XmPrimitiveWidget) iw, event);
#else
    _XmPrimitiveEnter ((Widget) iw, event, params, &num_params);
#endif

    if (iw->icon.armed)
       DrawShadow ((Widget) iw, True);
}

/************************************************************************
 *
 *  Leave
 *
 ************************************************************************/

static void Leave (iw, event)
    IconWidget iw;
    XEvent * event;

{
    String *params;
    Cardinal num_params;

#ifdef _XmPrimitive_has_two_args
    _XmPrimitiveLeave ((XmPrimitiveWidget) iw, event);
#else
    _XmPrimitiveLeave ((Widget) iw, event, params, &num_params);
#endif

    if (iw->icon.armed)
        DrawShadow (iw, False);
}

/************************************************************************
 *
 *  Arm
 *
 ************************************************************************/

static void Arm (iw, event)
    IconWidget iw;
    XEvent * event;

{
    XmAnyCallbackStruct call_value;

    iw->icon.armed = True;
    DrawShadow (iw, True);

    if (iw->icon.arm_callback) {
        XFlush(XtDisplay (iw));
        call_value.reason = XmCR_ARM;
        call_value.event = event;
        XtCallCallbackList ((Widget) iw, iw->icon.arm_callback, &call_value);
    }
}

/************************************************************************
 *
 *  Activate
 *
 ************************************************************************/

static void Activate (iw, event)
    IconWidget iw;
    XEvent * event;

{
   XmAnyCallbackStruct call_value;

   if ((event->xany.type == ButtonPress || event->xany.type == ButtonRelease)
       && ((int) event->xbutton.x > (int) iw->core.width ||
           (int) event->xbutton.y > (int) iw->core.height))
      return;

   call_value.reason = XmCR_ACTIVATE;
   call_value.event = event;
   XtCallCallbackList ((Widget) iw, iw->icon.activate_callback, &call_value);
}

/************************************************************************
 *
 *  Disarm
 *
 ************************************************************************/

static void Disarm (iw, event)
    IconWidget iw;
    XEvent * event;

{
    XmAnyCallbackStruct call_value;

    iw->icon.armed = False;
    DrawShadow (iw, True);

    if (iw->icon.disarm_callback) {
        XFlush(XtDisplay (iw));
        call_value.reason = XmCR_DISARM;
        call_value.event = event;
        XtCallCallbackList ((Widget) iw, iw->icon.disarm_callback, &call_value);
    }
}

/************************************************************************
 *
 *  Help
 *     This function processes Function Key 1 press
 *
 ************************************************************************/

static void Help (iw, event)
    IconWidget iw;
    XEvent * event;

{
   XmAnyCallbackStruct call_value;

   call_value.reason = XmCR_HELP;
   call_value.event = event;
   XtCallCallbackList ((Widget) iw, iw->primitive.help_callback, &call_value);
}

/************************************************************************
 *
 *  CreateIconLabel
 *	Create an instance of an icon as label
 *
 ************************************************************************/

Widget CreateIconLabel (parent, name, arglist, argcount, pixmap)
    Widget    parent;
    char     *name;
    ArgList   arglist;
    Cardinal  argcount;
    char     *pixmap;

{
    Widget w;
    Pixmap pix;
    Arg    args[1];

    w = XtCreateManagedWidget (name, iconWidgetClass,
                               parent, arglist, argcount);

    if (pixmap != NULL && pixmap != "") {
        pix = XmGetPixmap (((IconWidget) w)->core.screen,
                           pixmap,
                           ((IconWidget) w)->primitive.foreground,
                           ((IconWidget) w)->core.background_pixel);
        if (pix != (Pixmap) NULL) {
            XtSetArg (args[0], XmNlabelPixmap, pix);
            XtSetValues (w, args, 1);
        }
    }

    return w;
}

/************************************************************************
 *
 *  CreateIconButton
 *	Create an instance of an icon as button
 *
 ************************************************************************/

#define BUT_TRANS "#override <Btn1Down>: Arm()\n<Btn1Up>: Activate() Disarm()"

Widget CreateIconButton (parent, name, arglist, argcount, pixmap)
    Widget    parent;
    char     *name;
    ArgList   arglist;
    Cardinal  argcount;
    char     *pixmap;

{
    Widget w;
    Pixmap pix;
    Arg    args[1];
    XtTranslations   parsed = XtParseTranslationTable (BUT_TRANS);

    w = XtCreateManagedWidget (name, iconWidgetClass,
                               parent, arglist, argcount);

    if (pixmap != NULL && pixmap != "") {
        pix = XmGetPixmap (((IconWidget) w)->core.screen,
                           pixmap,
                           ((IconWidget) w)->primitive.foreground,
                           ((IconWidget) w)->core.background_pixel);
        if (pix != (Pixmap) NULL) {
            XtSetArg (args[0], XmNlabelPixmap, pix);
            XtSetValues (w, args, 1);
        }
    }

    XtAugmentTranslations (w, parsed);

    return w;
}

Widget CreateIconWidget (parent, name, arglist, argcount)
    Widget    parent;
    char     *name;
    ArgList   arglist;
    Cardinal  argcount;
{
    Widget w;

    w = XtCreateWidget (name, iconWidgetClass, parent, arglist, argcount);

    return w;
}
