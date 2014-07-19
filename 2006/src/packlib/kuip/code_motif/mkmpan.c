/*
 * $Id: mkmpan.c,v 1.3 1997/09/01 07:53:34 cremel Exp $
 *
 * $Log: mkmpan.c,v $
 * Revision 1.3  1997/09/01 07:53:34  cremel
 * Implement the possibility to "close" a palette with commands:
 * MULTI_PANEL <title> close
 * or:
 * MULTI_PANEL last close
 * Update HELP accordingly.
 *
 * Revision 1.2  1997/08/29 10:12:26  cremel
 * Increase version number to 2.07/15.
 * Bug correction for the "kuipPalette" widget (KUIP command "multi_panel"):
 * after opening 10 palettes one could not open anymore even after
 * closing one/some of them.
 *
 * Revision 1.1.1.1  1996/03/08 15:33:09  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/03 04/11/94  14.54.24  by  N.Cremel*/
/*-- Author :*/
/*-- Author :    N.Cremel   10/08/92*/
/******************************************************************************/
/*                                                                            */
/*           MOTIF-based library for KUIP - deck MKMPAN                       */
/*                                                                            */
/*           Basic Interface for "KUIP/Motif Palettes" (multi_panel)          */
/*           (connected to the files //KUIP/XMCLIB/KUXPAL, KUXPAB, KUXPAP     */
/*           for building the various palette components with Architect :     */
/*           "create_kuipPalette", "create_PalButton", "create_PalPanel"      */
/*                                                                            */
/******************************************************************************/

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
static C_PROTO_2 (void  km_display_palette,
                         char *name,
                         char *geometry);
static C_PROTO_1 (int get_pal_context,
                         Widget w);
static C_PROTO_1 (int get_pnb_context,
                         Widget w);
static C_PROTO_1 (int get_pnp_context,
                         Widget w);
static C_PROTO_3 (void add_panel_button,
                         Widget w,
                         PaletteStruct *palw,
                         XmAnyCallbackStruct *cbs);
static C_PROTO_3 (void close_palette,
                         Widget w,
                         PaletteStruct *palw,
                         XmAnyCallbackStruct *cbs);
static C_PROTO_1 (void ClosePal,
                         PaletteStruct *palw);
static void exit_appl (Widget w, caddr_t tag, XmAnyCallbackStruct *cbs);
static C_PROTO_3(void panel_set,
                         Widget w,
                         caddr_t tag,
                         XmAnyCallbackStruct *cbs);
static C_PROTO_3(void panel_enter,
                         Widget   widget,
                         char *name,
                         XEnterWindowEvent *ev);
static C_PROTO_3(void viewNPALCallback,
                         Widget  widget,
                         PaletteStruct *palst,
                         XmToggleButtonCallbackStruct *callback_data);
static C_PROTO_3(void viewIPALCallback,
                         Widget  widget,
                         PaletteStruct *palst,
                         XmToggleButtonCallbackStruct *callback_data);
static C_PROTO_3(void viewBPALCallback,
                         Widget  widget,
                         PaletteStruct *palst,
                         XmToggleButtonCallbackStruct *callback_data);
static void km_palette_view (int flag);



/***********************************************************************
 *                                                                     *
 *   Local datas.                                                      *
 *                                                                     *
 ***********************************************************************/

#define MAX_PAL 10

static PaletteStruct *cur_palette;
static npal = 0;
static Boolean addPan = True; /* Allow panel addition into a palette */
static lpal[MAX_PAL] = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1};

static PaletteStruct palette[MAX_PAL];
static Pal *pal_panel[MAX_PAL] =
           /* list of "starting panels" for a given palette */
                                 {NULL, NULL, NULL, NULL, NULL,
                                  NULL, NULL, NULL, NULL, NULL};
static int n_panel;

/*
 * C callable interface to command "multi_panel ... " (for palette)
 * (called by action routine kxpalet)
 *
 */

void km_palette (title, geometry)
  char *title, *geometry;    /* if NULL use default values */
{

  /* "multi_panel end" */
  if (title != NULL && !strcasecmp(title, "end")) {
      kc_window.palette_flag = 0;
      return;
  }


  /* "multi_panel same" */
  if (title != NULL && !strcasecmp(title, "same")) {
      kc_window.palette_flag = 1;
      return;
  }

  /* "multi_panel title close" */
  if (title != NULL && !strcasecmp(geometry, "close")) {
     /* Close palette */
     int np;
     if (!strcasecmp(title, "LAST")) {
       if (cur_palette != NULL) ClosePal( cur_palette );
     } else {
       for (np=0; np<MAX_PAL; np++) {
         char *ptit;
         PaletteStruct *pw = &palette[np];
         if (pw == NULL) break;
         XtVaGetValues(XtParent(pw->widget), XmNtitle, &ptit, NULL);
         if (!strcasecmp(title, ptit)) ClosePal( pw );
       }
     }
     return;
  }

  kc_window.palette_flag = 1;
  addPan = True;

  if (npal >= MAX_PAL) {
      km_warn_user(km_toplevel, "Cannot open more than 10 kuipPalette", "");
      addPan = False;
      return;
  } 

  km_display_palette (title, geometry);
}


/*
 * action routine for /MOTIF/MULTI_PANEL
 */
int kxpalet()
{
  char *title = ku_gets();
  char *geometry = ku_gets();

  km_palette ( title, geometry );

  return 0;
}


/******************************************************************************/
/* void km_display_palette                                                    */
/*                                                                            */
/******************************************************************************/
static void  km_display_palette(name, geometry)
    char *name, *geometry;
{
   Widget scb;
   int np;

   /* Look for a free palette number */
   for (np=0; np<MAX_PAL; np++) {
     if (lpal[np] == -1) break;
   }
   lpal[np] = npal;

   /* Interface built by Architect */
   palette[np].widget = (Widget) create_kuipPalette();

   palette[np].npal = np;
   palette[np].view_flag = by_ICON;

   palette[np].last_pan = NULL;

   XtVaSetValues(XtParent(palette[np].widget),
                 XmNtitle, name,
                 XmNgeometry, geometry,
                 NULL);

   (void)get_pal_context(palette[np].widget);

   XtVaGetValues(PalScWindow,
                 XmNhorizontalScrollBar, &scb,
                 NULL);
   XtUnmanageChild(scb);

   /* File menu */
   XtAddCallback(km_addPAL, XmNactivateCallback,
                    (XtCallbackProc)add_panel_button, &palette[np]);
   XtAddCallback(km_closePAL, XmNactivateCallback,
                    (XtCallbackProc)close_palette, &palette[np]);
   XtAddCallback(km_exitPAL, XmNactivateCallback,
                    (XtCallbackProc)exit_appl, NULL);
   /* View menu */
   XtAddCallback(km_viewPAL_name, XmNvalueChangedCallback,
                    (XtCallbackProc)viewNPALCallback, &palette[np]);
   XtAddCallback(km_viewPAL_icon, XmNvalueChangedCallback,
                    (XtCallbackProc)viewIPALCallback, &palette[np]);
   XtAddCallback(km_viewPAL_both, XmNvalueChangedCallback,
                    (XtCallbackProc)viewBPALCallback, &palette[np]);
   /* Help menu */
   XtAddCallback(km_helpPAL, XmNactivateCallback,
                    (XtCallbackProc)km_do_help, (XtPointer)"MULTI_PANEL");

   /* display palette */
   UxPopupInterface(palette[np].widget, XtGrabNone);

   /*
    * If the dialog was already open, UxPopupInterface does nothing.  In
    * this case, at least make sure the window is raised to the top
    * of the window tree (or as high as it can get).
    */
   XMapRaised(XtDisplay(palette[np].widget),
              XtWindow(XtParent(palette[np].widget)));

   cur_palette = &palette[np];
   n_panel = 0;
   npal++;
}

static int get_pal_context(w)
   Widget w;
{
   if (!w) return -1;

   /*
    *   find and restore the context
    *   (to get to the widgets belonging to this dialog)
    */
   UxKuipPaletteContext = (_UxCkuipPalette *)UxGetContext(w);

   return 1;
}

static int get_pnb_context(w)
   Widget w;
{
   if (!w) return -1;

   /*
    *   find and restore the context
    *   (to get to the widgets belonging to this dialog)
    */
   UxPalButtonContext = (_UxCPalButton *)UxGetContext(w);

   return 1;
}

static int get_pnp_context(w)
   Widget w;
{
   if (!w) return -1;

   /*
    *   find and restore the context
    *   (to get to the widgets belonging to this dialog)
    */
   UxPalPanelContext = (_UxCPalPanel *)UxGetContext(w);

   return 1;
}

static void add_panel_button(w, palw, cbs)
   Widget w;
   PaletteStruct *palw;
   XmAnyCallbackStruct *cbs;
{
   char *panel_name;
   char exstr[MAX_stringl];

   int flag = kc_window.palette_flag;

   /* reset palette_flag so that panel is not added twice ... */
   kc_window.palette_flag = -1;

   /* ask name of Kuip macro with the panel description */
   panel_name = km_wait_input ("Give KUIP macro file name",
                               "panel", "HELP_PANEL");
   if (panel_name == NULL) return;
   sprintf (exstr,"/MACRO/EXEC %s", panel_name);
   exec_cmd_string (exstr, 0, NULL);

   addPan = True;
   km_panel_add (palw, panel_name, NULL);
   kc_window.palette_flag =  flag;
}

static void close_palette(w, palw, cbs)
   Widget w;
   PaletteStruct *palw;
   XmAnyCallbackStruct *cbs;
{
   ClosePal(palw);
}

static void ClosePal(palw)
   PaletteStruct *palw;
{
   int np = palw->npal;
   int i;

   if (palw == NULL) return;

   UxDestroyInterface(palw->widget);

   /* Reset this palette */
   lpal[np] = -1;
   npal--;
   palw->widget = NULL;
   pal_panel[np] = NULL;
   palw->last_pan = NULL;
   cur_palette = NULL;
   for (i=npal; i>=0; i--) {
      /* cur_palette is the last "valid" palette in the list */
      if (lpal[i] >=0) {
          cur_palette = &palette[i];
          break;
      }
   }
}

static void exit_appl( Widget w, caddr_t tag, XmAnyCallbackStruct *cbs)
{
    exec_cmd_string ("/KUIP/EXIT", 0, NULL);
}

void km_panel_add (pw, name, geometry)
   PaletteStruct *pw;
   char *name, *geometry;
{
   int np;
   Boolean close_flag = False;

   static char *enter_tag;

   static Pal *panel[MAX_PAL] = /* list of panels for a given palette */
                                 {NULL, NULL, NULL, NULL, NULL,
                                  NULL, NULL, NULL, NULL, NULL};
   static Dimension panel_width;

   static ifirst = 0;

   if (!addPan) { /* Maximum number of kuipPalette reached */
         km_warn_user(km_toplevel, 
         "Close and open a new kuipPalette before panel execution!", "");
         return;
   }

   n_panel++;
   if (pw == NULL) pw = cur_palette;
   if (pw == NULL) return;
   np = pw->npal;
   (void)get_pal_context(pw->widget);

   if (pal_panel[np] == NULL) {
       panel[np] = pal_panel[np] = (Pal *) malloc (sizeof (Pal));
       panel[np]->npan = 0;
   } else {
       panel[np] = panel[np]->next = (Pal *) malloc (sizeof (Pal));
       panel[np]->npan = n_panel;
   }
   panel[np]->PALwidget = pw->widget;
   panel[np]->npal = np;
   panel[np]->next = NULL;

   panel[np]->Bwidget = create_PalButton(km_palForm);
   (void)get_pnb_context(panel[np]->Bwidget);
   if (kc_window.panel_title != NULL)
       XtVaSetValues(km_palLabelB,
                     RES_CONVERT( XmNlabelString,kc_window.panel_title ),
                     NULL);

   panel[np]->Pwidget = create_PalPanel(km_palForm);
   (void)get_pnp_context(panel[np]->Pwidget);
   if (kc_window.panel_title != NULL)
       XtVaSetValues(km_palLabelP,
                     RES_CONVERT( XmNlabelString,kc_window.panel_title ),
                     NULL);

   if (ifirst == 0) {
       XtVaGetValues (XtParent(km_palForm), XmNwidth, &panel_width, NULL);
       /* take off the offset on both sides */
       panel_width -= 10;
       ifirst++;
   }

   /* Create and manage panel buttons inside of RowColumn widget */
   km_panel_buttons (panel[np]->Pwidget, name, pw->view_flag, 1);


   XtVaSetValues (panel[np]->Bwidget, XmNwidth, panel_width, NULL);
   XtVaSetValues (panel[np]->Pwidget, XmNwidth, panel_width, NULL);
   if (geometry != NULL && !km_strempty(geometry)) {
       int ih, iw, ix, iy;

       iw = ih = 0;
       if (geometry[0] == 'c' || geometry[0] == 'C') { /* `Close' */
         char cf;
         close_flag = True;
         if (geometry[1] == ':')
             sscanf (geometry, "%c:%dx%d+%d+%d", &cf, &iw, &ih, &ix, &iy);
       } else {
         sscanf (geometry, "%dx%d+%d+%d", &iw, &ih, &ix, &iy);
       }
       if (iw != 0) {
           XtVaSetValues(panel[np]->Pwidget, XmNwidth, (Dimension)iw, NULL );
           XtVaSetValues (panel[np]->Bwidget, XmNwidth, (Dimension)iw, NULL);
       }
       if (ih != 0)
           XtVaSetValues(panel[np]->Pwidget, XmNheight, (Dimension)ih, NULL );
   }

   if (close_flag ) {
       panel[np]->Mwidget = panel[np]->Bwidget;
   } else {
       panel[np]->Mwidget = panel[np]->Pwidget;
   }

  /* attach next panel at end of panel list */
  if (!pw->last_pan) {
      XtVaSetValues(panel[np]->Bwidget,
                    XmNtopAttachment,  XmATTACH_FORM,
                    XmNtopOffset,      5,
                    XmNleftAttachment, XmATTACH_FORM,
                    XmNleftOffset,     5,
                    NULL);
      XtVaSetValues(panel[np]->Pwidget,
                    XmNtopAttachment,  XmATTACH_FORM,
                    XmNtopOffset,      5,
                    XmNleftAttachment, XmATTACH_FORM,
                    XmNleftOffset,     5,
                    NULL);
  } else {
      XtVaSetValues(panel[np]->Bwidget,
                    XmNtopAttachment,  XmATTACH_WIDGET,
                    XmNtopOffset,      5,
                    XmNtopWidget,      pw->last_pan->Mwidget,
                    XmNleftAttachment, XmATTACH_FORM,
                    XmNleftOffset,     5,
                    NULL);
      XtVaSetValues(panel[np]->Pwidget,
                    XmNtopAttachment,  XmATTACH_WIDGET,
                    XmNtopOffset,      5,
                    XmNtopWidget,      pw->last_pan->Mwidget,
                    XmNleftAttachment, XmATTACH_FORM,
                    XmNleftOffset,     5,
                    NULL);
  }
  pw->last_pan = panel[np];

  XtManageChild(panel[np]->Mwidget);

  XtAddCallback(arrowButtonB, XmNactivateCallback,
                (XtCallbackProc)panel_set, panel[np]);
  XtAddCallback(arrowButtonP, XmNactivateCallback,
                (XtCallbackProc)panel_set, panel[np]);
  enter_tag = NULL;
  if (kc_window.panel_name != NULL) enter_tag = strdup (kc_window.panel_name);
  XtAddEventHandler(panel[np]->Pwidget, EnterWindowMask, False,
                (XtEventHandler)panel_enter, (XtPointer)enter_tag);


  XmUpdateDisplay(pw->widget);
}


static void km_palette_view (view_flag)
   int view_flag;
{
    if ( view_flag == by_NAME ) {
         if (XtIsManaged (km_scrollMB)) XtUnmanageChild (km_scrollMB);
         if (XtIsManaged (km_scrollMI)) XtUnmanageChild (km_scrollMI);
         if (XtIsManaged (km_scrollMC)) XtUnmanageChild (km_scrollMC);
         if (XtIsManaged (km_scrollM1)) XtUnmanageChild (km_scrollM1);
         XtManageChild (km_scrollMN);
    } else if (view_flag == by_ICON) {
         if (XtIsManaged (km_scrollMN)) XtUnmanageChild (km_scrollMN);
         if (XtIsManaged (km_scrollMB)) XtUnmanageChild (km_scrollMB);
         if (XtIsManaged (km_scrollMC)) XtUnmanageChild (km_scrollMC);
         if (XtIsManaged (km_scrollM1)) XtUnmanageChild (km_scrollM1);
         XtManageChild (km_scrollMI);
    } else if (view_flag == by_BOTH) {
         if (XtIsManaged (km_scrollMN)) XtUnmanageChild (km_scrollMN);
         if (XtIsManaged (km_scrollMI)) XtUnmanageChild (km_scrollMI);
         if (XtIsManaged (km_scrollMC)) XtUnmanageChild (km_scrollMC);
         if (XtIsManaged (km_scrollM1)) XtUnmanageChild (km_scrollM1);
         XtManageChild (km_scrollMB);
    } else if (view_flag == by_COMMAND) {
         if (XtIsManaged (km_scrollMN)) XtUnmanageChild (km_scrollMN);
         if (XtIsManaged (km_scrollMI)) XtUnmanageChild (km_scrollMI);
         if (XtIsManaged (km_scrollMB)) XtUnmanageChild (km_scrollMB);
         if (XtIsManaged (km_scrollM1)) XtUnmanageChild (km_scrollM1);
         XtManageChild (km_scrollMC);
    } else if (view_flag == by_COMMAND1) {
         if (XtIsManaged (km_scrollMN)) XtUnmanageChild (km_scrollMN);
         if (XtIsManaged (km_scrollMI)) XtUnmanageChild (km_scrollMI);
         if (XtIsManaged (km_scrollMB)) XtUnmanageChild (km_scrollMB);
         if (XtIsManaged (km_scrollMC)) XtUnmanageChild (km_scrollMC);
         XtManageChild (km_scrollM1);
    }
}


static void panel_set(w, tag, cbs)
   Widget w;
   caddr_t         tag;
   XmAnyCallbackStruct *cbs;
{
   Pal *palb = (Pal *) tag;
   Pal *panel, *last_panel;

   int np = palb->npal;

   (void)get_pnb_context(palb->Bwidget);
   (void)get_pnp_context(palb->Pwidget);

   if (palb->Bwidget != NULL &&
       XtIsManaged (palb->Bwidget))  {
       palb->Mwidget = palb->Pwidget;
   } else if (palb->Pwidget != NULL &&
       XtIsManaged (palb->Pwidget))  {
       palb->Mwidget = palb->Bwidget;
   }

   for( panel = pal_panel[np]; panel != NULL; panel = panel->next ) {
      if (XtIsManaged (palb->Bwidget))
          XtUnmanageChild(panel->Bwidget);
      if (XtIsManaged (palb->Pwidget))
          XtUnmanageChild(panel->Pwidget);
  }

   /* Reset attachments of all panels */
   last_panel = pal_panel[np];
   for( panel = pal_panel[np]; panel != NULL; panel = panel->next ) {
      XtUnmanageChild(panel->Mwidget);
      if (panel->npan != 0) {
        XtVaSetValues(panel->Bwidget,
                      XmNtopAttachment,  XmATTACH_WIDGET,
                      XmNtopOffset,      5,
                      XmNtopWidget,      last_panel->Mwidget,
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNleftOffset,     5,
                      NULL);
        XtVaSetValues(panel->Pwidget,
                      XmNtopAttachment,  XmATTACH_WIDGET,
                      XmNtopOffset,      5,
                      XmNtopWidget,      last_panel->Mwidget,
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNleftOffset,     5,
                      NULL);

      }
      last_panel = panel;
   }

   for( panel = pal_panel[np]; panel != NULL; panel = panel->next ) {
      if (panel->Mwidget == panel->Pwidget) {
         /* Set correct viewing option before managing */
         Boolean set = False;
         int view_flag;

         (void)get_pal_context(panel->PALwidget);
         if (!set) {
             XtVaGetValues(km_viewPAL_name, XmNset, &set, NULL);
             if (set) view_flag = by_NAME;
         }
         if (!set) {
             XtVaGetValues(km_viewPAL_icon, XmNset, &set, NULL);
             if (set) view_flag = by_ICON;
         }
         if (!set) {
             XtVaGetValues(km_viewPAL_both, XmNset, &set, NULL);
             if (set) view_flag = by_BOTH;
         }
         (void)get_pnp_context(panel->Mwidget);
         km_palette_view (view_flag);
      }
      XtManageChild(panel->Mwidget);
   }

}

static void panel_enter(widget, name, ev)
      Widget widget;
      char *name;
      XEnterWindowEvent *ev;
{
    if (kc_window.panel_name != NULL) free (kc_window.panel_name);
    if (name != NULL) {
        kc_window.panel_name = strdup (name);
    } else {
        kc_window.panel_name = NULL;
    }
}

static void viewNPALCallback( widget, palst, callback_data )
        Widget  widget;
        PaletteStruct *palst;
        XmToggleButtonCallbackStruct *callback_data;
{
    Pal *pan;

    Widget  pal = palst->widget;
    int np = palst->npal;

    palst->view_flag = by_NAME;

   /* when a button gets unselected don't do anything */
   if (callback_data->set) return;

    /* Loop on all panels */
    for( pan = pal_panel[np]; pan != NULL; pan = pan->next ) {
         Widget panel;

         if (pan->Mwidget == pan->Bwidget) continue;
         panel = pan->Pwidget;

         (void)get_pnp_context(panel);

         km_palette_view (by_NAME);
    }

    (void)get_pal_context(pal);

    XtVaSetValues(km_viewPAL_name, XmNset, True, NULL);
    XtVaSetValues(km_viewPAL_icon, XmNset, False, NULL);
    XtVaSetValues(km_viewPAL_both, XmNset, False, NULL);
}

static void viewIPALCallback( widget, palst, callback_data )
        Widget  widget;
        PaletteStruct *palst;
        XmToggleButtonCallbackStruct *callback_data;
{
    Pal *pan;

    Widget  pal = palst->widget;
    int np = palst->npal;

    palst->view_flag = by_ICON;

   /* when a button gets unselected don't do anything */
   if (callback_data->set) return;

    /* Loop on all panels */
    for( pan = pal_panel[np]; pan != NULL; pan = pan->next ) {
         Widget panel;

         if (pan->Mwidget == pan->Bwidget) continue;
         panel = pan->Pwidget;

         (void)get_pnp_context(panel);

         km_palette_view (by_ICON);
    }

    (void)get_pal_context(pal);

    XtVaSetValues(km_viewPAL_icon, XmNset, True, NULL);
    XtVaSetValues(km_viewPAL_name, XmNset, False, NULL);
    XtVaSetValues(km_viewPAL_both, XmNset, False, NULL);
}

static void viewBPALCallback( widget, palst, callback_data )
        Widget  widget;
        PaletteStruct *palst;
        XmToggleButtonCallbackStruct *callback_data;
{
    Pal *pan;

    Widget  pal = palst->widget;
    int np = palst->npal;

    palst->view_flag = by_BOTH;

   /* when a button gets unselected don't do anything */
   if (callback_data->set) return;

    /* Loop on all panels */
    for( pan = pal_panel[np]; pan != NULL; pan = pan->next ) {
         Widget panel;

         if (pan->Mwidget == pan->Bwidget) continue;
         panel = pan->Pwidget;

         (void)get_pnp_context(panel);

         km_palette_view (by_BOTH);
    }

    (void)get_pal_context(pal);

    XtVaSetValues(km_viewPAL_both, XmNset, True, NULL);
    XtVaSetValues(km_viewPAL_name, XmNset, False, NULL);
    XtVaSetValues(km_viewPAL_icon, XmNset, False, NULL);
}
