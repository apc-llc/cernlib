/*
 * $Id: mkdgra.c,v 1.1.1.1 1996/03/08 15:33:07 mclareni Exp $
 *
 * $Log: mkdgra.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:07  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/02 31/03/95  14.30.02  by  N.Cremel*/
/*-- Author :    N.Cremel   08/09/92*/
/******************************************************************************/
/*                                                                            */
/*           MOTIF-based library for KUIP - deck MKDGRA                       */
/*                                                                            */
/*           Basic Interface for any "Graphics Windows" (based on HIGZ)       */
/*           (connected to the file //KUIP/XMCLIB/KUXGRA for building the     */
/*           window with Architect : "create_kuipGraphics").                  */
/*                                                                            */
/*           - Set all Event Handlers and call corresponding user defined     */
/*           routine.                                                         */
/*                                                                            */
/******************************************************************************/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/klink.h"
#include "kuip/kmenu.h"

#include "mkincl.h"
#include "mkdims.h"
#include "mkutfm.h"
#include "mkutdm.h"
#include "mksres.h"
#include "mksreg.h"
#define CONTEXT_MACRO_ACCESS 1
#include "mkuxgr.h"
#undef CONTEXT_MACRO_ACCESS
#include "kuip/browh1.h"
#include "browh2.h"


#define MAX_GRAPHICS_AREA   5

/* type definitions */
typedef struct {
   Widget    widget;               /* graphics area widget itself */
} GraphicsAreaStruct;


/* local data */
static Widget graph_popup = NULL;
static GraphicsAreaStruct   gr_area[MAX_GRAPHICS_AREA];
static int           n_gr_area = 0;


/***********************************************************************
 *                                                                     *
 *   Forward declarations of local functions.                          *
 *                                                                     *
 ***********************************************************************/

static C_PROTO_1(int get_ga_context,
                         Widget w);
static C_PROTO_3(void destroy_graphicsArea,
                         Widget w,
                         Widget graphics,
                         XmAnyCallbackStruct *call_data);
static C_PROTO_3(void drawing_area_callback,
                         Widget widget,
                         Widget graphics,
                         XmDrawingAreaCallbackStruct *call_data);
static C_PROTO_3(void drawing_area_callback1,
                         Widget widget,
                         int    tag,
                         XmDrawingAreaCallbackStruct *call_data);
static C_PROTO_3(void drawing_area_exr,
                         int wid,
                         int reason,
                         int count);
static C_PROTO_3(void drawing_area_ew,
                         Widget  draw_area,
                         int     tag,
                         XEnterWindowEvent *event);
static C_PROTO_3(void drawing_area_lw,
                         Widget  draw_area,
                         int     tag,
                         XLeaveWindowEvent *event);
static C_PROTO_3(void drawing_area_bp,
                         Widget  draw_area,
                         int     tag,
                         XButtonPressedEvent *event);
static C_PROTO_3(void drawing_area_br,
                         Widget  draw_area,
                         int     tag,
                         XButtonReleasedEvent *event);
static C_PROTO_3(void drawing_area_pm,
                         Widget  draw_area,
                         int     tag,
                         XPointerMovedEvent *event);


/***********************************************************************
 *                                                                     *
 *   Forward declarations of global functions.                         *
 *                                                                     *
 ***********************************************************************/
C_PROTO_5(Window km_graph_open,
                         int wkid,
                         int x_pos,
                         int y_pos,
                         int w_dim,
                         int h_dim);
C_PROTO_1(void km_graph_init,
                         int wkid);
C_PROTO_4(void km_graph_init_callbacks,
                         Widget draw,
                         Widget graph_widget,
                         int wkid,
                         int flag);
C_PROTO_1(void km_graph_close,
                         int wkid);
#if 0
C_PROTO_3(void show_kuipGraphics,
                         Widget w,
                         Widget clone_parent,
                         XmAnyCallbackStruct *call_data);
#endif
C_PROTO_0(void km_reset_graph_popup);


/***********************************************************************
 *                                                                     *
 *   Forward declarations of extern functions.                         *
 *                                                                     *
 ***********************************************************************/
extern C_PROTO_0 (Widget create_kuipGraphics);


/***********************************************************************
 *                                                                     *
 *   Forward declarations of extern functions.                         *
 *                                                                     *
 ***********************************************************************/
extern C_PROTO_3(void popup_activate,
                         Widget widget,
                         caddr_t tag,
                         XmAnyCallbackStruct *call_data);


#if 0 /* Only for testing km_graph_open and km_graph_close */
int kuipgrop( cmd_name )
     char *cmd_name;
{
  long wkid = ku_geti();
  Window gwin;

printf ("*** kuipgrop : wkid = %d\n",wkid);
  gwin = km_graph_open (wkid, 200, 200, 500, 500);
}

int kuipgrcl( cmd_name )
     char *cmd_name;
{
  long wkid = ku_geti();

printf ("*** kuipgrcl : wkid = %d\n",wkid);
  km_graph_close (wkid);
}
#endif


/***********************************************************************
 *                                                                     *
 *   Creates a Motif (Architect) graphics window (for HIGZ)            *
 *   and returns the window of the DrawingArea widget.                 *
 *                                                                     *
 *   int wkid (input) = workstation identifier                         *
 *                                                                     *
 ***********************************************************************/
Window km_graph_open (wkid, x_pos, y_pos, w_dim, h_dim)
     int wkid;
     int x_pos, y_pos;
     int w_dim, h_dim;
{
   Widget        graph_widget;
   char         *title;
   char          cval[MAX_string];


   if (n_gr_area >= MAX_GRAPHICS_AREA) {
      km_inform_user
         (gr_area[0].widget, "Cannot open more graphics windows.", "");
      return 0;
   }

   /* Create Motif graphics window and store it */
   if (gr_area[wkid-1].widget != NULL) {
       printf ("km_graph_open : workstation %d is already opened\n",wkid);
       return 0; }
   graph_widget = (Widget) create_kuipGraphics();
   gr_area[wkid-1].widget = graph_widget;
   n_gr_area++;

   get_ga_context(graph_widget);

   /* Set window title */
   title = XtCalloc(MAX_string, 1);
   sprintf(title, "%s Graphics %d", class_name, wkid);
   XtVaSetValues(XtParent(graph_widget), XmNtitle, title,
                                   XmNiconName, title,
                                   XmNiconPixmap, km_pixmap,
                                   XmNdeleteResponse, XmDO_NOTHING,
                                   NULL);
   XtFree(title);

   /* Pass widget ID to application */
   sprintf (cval,"kuipGraphics%d", wkid);
   if( klnkaddr.user_TopWid_C != NULL )
      (*klnkaddr.user_TopWid_C)(cval, graph_widget);

   /* Store this window into the list (for raise and iconify) */
   XtVaGetValues(XtParent(graph_widget), XmNtitle, &title, NULL);
   km_store_appShell (graph_widget, title);

#if 0
   km_MwmCloseCallback(XtParent(graph_widget),
                           (XtCallbackProc)destroy_graphicsArea,
                           (XtPointer)graph_widget);

   /* Add drawing area expose and resize callbacks */
   XtAddCallback(km_graphicsDraw, XmNexposeCallback,
                 (XtCallbackProc)drawing_area_callback, graph_widget);
   XtAddCallback(km_graphicsDraw, XmNresizeCallback,
                 (XtCallbackProc)drawing_area_callback, graph_widget);

   /* Add drawing area Event Handler on different kind of events */
   /* EnterWindow */
    XtAddEventHandler(XtParent(graph_widget), EnterWindowMask, False,
                      (XtEventHandler)drawing_area_ew, (XtPointer)wkid);
   /* LeaveWindow */
    XtAddEventHandler(XtParent(graph_widget), LeaveWindowMask, False,
                      (XtEventHandler)drawing_area_lw, (XtPointer)wkid);
   /* ButtonPress */
    XtAddEventHandler(km_graphicsDraw, ButtonPressMask, False,
                      (XtEventHandler)drawing_area_bp, (XtPointer)wkid);
   /* ButtonRelease */
    XtAddEventHandler(km_graphicsDraw, ButtonReleaseMask, False,
                      (XtEventHandler)drawing_area_br, (XtPointer)wkid);
   /* PointerMotion */
    XtAddEventHandler(km_graphicsDraw, PointerMotionMask, False,
                      (XtEventHandler)drawing_area_pm, (XtPointer)wkid);
#endif

   UxPopupInterface(graph_widget, XtGrabNone);

   /* Set window position and dimension */
   if (wkid > 1) {
   /* For wkid = 1 use "kuipGraphics_shell" geometry resource instead */
      XtVaSetValues(XtParent(graph_widget), XmNx, (Position) x_pos,
                                      XmNy, (Position) y_pos,
                                      XmNwidth, (Dimension) w_dim,
                                      XmNheight, (Dimension) h_dim,
                                      NULL);
      }

   km_set_cursor (graph_widget, km_cross_cursor);

   return (XtWindow(km_graphicsDraw));
}

/***********************************************************************
 *                                                                     *
 *   Install callbacks for the graphics window (created for HIGZ)      *
 *   (This has to be called after the creation of the HIGZ window)     *
 *                                                                     *
 *   int wkid (input) = workstation identifier                         *
 *                                                                     *
 ***********************************************************************/
void km_graph_init (wkid)
     int wkid;
{
   Widget graph_widget = gr_area[wkid-1].widget;

   get_ga_context(graph_widget);

   km_MwmCloseCallback(XtParent(graph_widget),
                           (XtCallbackProc)destroy_graphicsArea,
                           (XtPointer)graph_widget);
   km_graph_init_callbacks (km_graphicsDraw, graph_widget, wkid, 0);
}

void km_graph_init_callbacks (draw, graph_widget, wkid, flag)
     Widget draw, graph_widget;
     int wkid, flag;
{
   /* Add drawing area expose and resize callbacks */
   if (flag == 0) {
       XtAddCallback(draw, XmNexposeCallback,
                     (XtCallbackProc)drawing_area_callback, graph_widget);
       XtAddCallback(draw, XmNresizeCallback,
                     (XtCallbackProc)drawing_area_callback, graph_widget);
   } else {
       XtAddCallback(draw, XmNexposeCallback,
                     (XtCallbackProc)drawing_area_callback1, (XtPointer)wkid);
       XtAddCallback(draw, XmNresizeCallback,
                     (XtCallbackProc)drawing_area_callback1, (XtPointer)wkid);
   }

   /* Add drawing area Event Handler on different kind of events */
   /* EnterWindow */
    XtAddEventHandler(XtParent(graph_widget), EnterWindowMask, False,
                      (XtEventHandler)drawing_area_ew, (XtPointer)wkid);
   /* LeaveWindow */
    XtAddEventHandler(XtParent(graph_widget), LeaveWindowMask, False,
                      (XtEventHandler)drawing_area_lw, (XtPointer)wkid);

   /* ButtonPress */
    XtAddEventHandler(draw, ButtonPressMask, False,
                      (XtEventHandler)drawing_area_bp, (XtPointer)wkid);
   /* ButtonRelease */
    XtAddEventHandler(draw, ButtonReleaseMask, False,
                      (XtEventHandler)drawing_area_br, (XtPointer)wkid);
   /* PointerMotion */
    XtAddEventHandler(draw, PointerMotionMask, False,
                      (XtEventHandler)drawing_area_pm, (XtPointer)wkid);
}


/***********************************************************************
 *                                                                     *
 *   Destroy any Motif (Architect) graphics window (created for HIGZ)  *
 *                                                                     *
 *   int wkid (input) = workstation identifier                         *
 *                                                                     *
 ***********************************************************************/
void km_graph_close (wkid)
     int wkid;
{
   if (gr_area[wkid-1].widget == NULL) {
       printf ("km_graph_close : workstation %d is not opened\n",wkid);
       return; }

   UxDestroyInterface(gr_area[wkid-1].widget);
   km_reset_appShell (gr_area[wkid-1].widget);

   gr_area[wkid-1].widget       = (Widget) NULL;
   n_gr_area--;
}


/***********************************************************************
 *                                                                     *
 *   Get the context for graphics area widget.                         *
 *                                                                     *
 ***********************************************************************/
static int get_ga_context(w)
    Widget w;
{
   int           i;

   if (!w) return -1;

   /*
    *   find and restore the context
    *   (to get to the widgets belonging to this dialog)
    */
   UxKuipGraphicsContext = (_UxCkuipGraphics *)UxGetContext(w);

   /* restore the context not handled by the Architect context */
   for (i = 0; i < MAX_GRAPHICS_AREA; i++) {
      if (gr_area[i].widget == w) {
         return i;
      }
   }
   return -1;
}

/***********************************************************************
 *                                                                     *
 *   Destroy the graphics area dialog.                                 *
 *                                                                     *
 ***********************************************************************/
static void destroy_graphicsArea(w, graphics, call_data)
     Widget w, graphics;
     XmAnyCallbackStruct *call_data;
{
   int   i;

   for (i = 0; i < MAX_GRAPHICS_AREA; i++) {
      if (gr_area[i].widget == graphics) {

         if (i == 0) {
            km_inform_user(graphics, "Cannot close graphics window 1.", "");
            return; }

         /* call HIGZ routine to close the window */
         if( klnkaddr.graf_close_F != NULL ) {
           INTEGER wkid = i+1;
           (*klnkaddr.graf_close_F)(&wkid);
         }

         gr_area[i].widget       = (Widget) NULL;
         break;
      }
   }
   n_gr_area--;

   UxDestroyInterface(graphics);
   km_reset_appShell (graphics);
}


/***********************************************************************
 *                                                                     *
 *   Handle the graphics window callbacks (expose and resize events).  *
 *                                                                     *
 ***********************************************************************/
static void drawing_area_callback(widget, graphics, call_data)
       Widget widget, graphics;
       XmDrawingAreaCallbackStruct *call_data;
{
   XEvent *ev = call_data->event;
   int count = 0;

   INTEGER wid = get_ga_context(graphics) + 1;

   if (call_data->reason == XmCR_EXPOSE) count = ev->xexpose.count;
   drawing_area_exr (wid, call_data->reason, count);
}

static void drawing_area_callback1(widget, tag, call_data)
       Widget widget;
       int tag;
       XmDrawingAreaCallbackStruct *call_data;
{
   XEvent *ev = call_data->event;
   int count = 0;

   INTEGER wid = tag;

   if (call_data->reason == XmCR_EXPOSE) count = ev->xexpose.count;
   drawing_area_exr (wid, call_data->reason, count);
}

static void drawing_area_exr (wid, reason, count)
       int wid, reason, count;
{
   if (reason == XmCR_EXPOSE) {
      if (count != 0) return;
      /* Set new size */
      if( klnkaddr.graf_size_F != NULL && wid > 0) {
          (*klnkaddr.graf_size_F)(&wid); }

   }

   if (reason == XmCR_RESIZE) {
      /* Set new size */
      if( klnkaddr.graf_size_F != NULL && wid > 0) {
          (*klnkaddr.graf_size_F)(&wid); }
   }
}

/***********************************************************************
 *                                                                     *
 *   Graphics window Event Handler on EnterWindowMask                  *
 *                                                                     *
 ***********************************************************************/
static void drawing_area_ew ( draw_area, tag, event )
     Widget  draw_area;
     int     tag;
     XEnterWindowEvent *event;
{
  INTEGER ix = event->x;
  INTEGER iy = event->y;
  INTEGER iwin = tag;
  INTEGER icod;


  /* call user defined routine */
  icod = 999;
  if (kjmpaddr.user_locate_F != NULL)
    (*kjmpaddr.user_locate_F) (&ix, &iy, &icod, &iwin);
}

/***********************************************************************
 *                                                                     *
 *   Graphics window Event Handler on LeaveWindowMask                  *
 *                                                                     *
 ***********************************************************************/
static void drawing_area_lw ( draw_area, tag, event )
     Widget  draw_area;
     int     tag;
     XLeaveWindowEvent *event;
{
  INTEGER ix = event->x;
  INTEGER iy = event->y;
  INTEGER iwin = tag;
  INTEGER icod;

  /* call user defined routine */
  icod = -999;
  if (kjmpaddr.user_locate_F != NULL)
    (*kjmpaddr.user_locate_F) (&ix, &iy, &icod, &iwin);
}

/***********************************************************************
 *                                                                     *
 *   Graphics window Event Handler on ButtonPressMask                  *
 *                                                                     *
 ***********************************************************************/
static void drawing_area_bp ( draw_area, tag, event )
     Widget  draw_area;
     int     tag;
     XButtonPressedEvent *event;
{
  static poptag_str ptag[MAXLEVEL*MAXITEMS];

  Widget filemenu_default;
  MenuItem      filemenu_items[MAXITEMS];
  MenuItem      submenu_items[MAXITEMS];
  INTEGER ix = event->x;
  INTEGER iy = event->y;
  INTEGER iwin = tag;
  INTEGER icod;
  INTEGER nlev;
  INTEGER ipid[10];
  char chpid[10][8];
  char name[10][16];
  char *class;
  char title[MAX_string], stitle[MAX_string];
  KmClass  *kmcls, *kmcls1;
  BrAction *bract, *bract1;
  int i, j, k;
  int nl;

  if( event->button != 3) {
  /* call user defined routine */
    icod = event->button;
    if (kjmpaddr.user_locate_F != NULL)
      (*kjmpaddr.user_locate_F) (&ix, &iy, &icod, &iwin);
    return; }

  /* event->button = 3 : graphical object retrieving (graf_pick_F) */
  if( klnkaddr.graf_pick_F == NULL) return;

  /* graphical object retrieving */
  nlev = tag; /* give wkid to HIGZ */
  F77_CALL_x4C((*klnkaddr.graf_pick_F), &ix, &iy, &nlev, ipid, chpid[0], 8 );
  if( nlev <= 0 )
    return;

  for( nl = 0; nl < nlev; nl++ ) {
    if( ipid[nl] > 0xFFFFFF ) {
      /* if ipid is a large number assume it to be a 4-character Hollerith  */
      strncpy( name[nl], (char*)&ipid[nl], 4 );
      name[nl][4] = '\0';
    }
    else {
      long lpid = ipid[nl];
      sprintf( name[nl], "%ld", lpid );
    }
  }
  nlev --;

  class = fstr0dup( chpid[nlev], 8 );
  kmcls = find_kmclass( class );
  free( class );
  /* Look for the first level which corresponds to a valid class */
  while ( kmcls == NULL ) {
          if( nlev == 0)
              return;
          nlev --;
          class = fstr0dup( chpid[nlev], 8 );
          kmcls = find_kmclass( class );
          free( class ); }

  /* Now kmcls != NULL : Build graphics popup menu according to this class */
  sprintf( title, "%s %s", kmcls->title, name[nlev] );
  for( i = 0, bract = kmcls->graf; bract != NULL;
       bract = bract->next, i++ ) {
       if (i >= MAXITEMS) break;
       ptag[i].flag = 3;
       ptag[i].panel = (event->state == ControlMask);
       ptag[i].bract = bract;
       ptag[i].bract_next = bract->next;
       strcpy( ptag[i].item, name[nlev] );
       for( nl = nlev - 1; nl >= 0; nl-- ) {
         strcat( ptag[i].item, "," );
         strcat( ptag[i].item, name[nl] );
       }
       strcpy( ptag[i].pathname, kmcls->name );
       filemenu_items[i].accelerator    = NULL;
       filemenu_items[i].accel_text     = NULL;
       filemenu_items[i].callback       = popup_activate;
       filemenu_items[i].callback_data  = (caddr_t) &ptag[i];
       filemenu_items[i].default_button = False;
       filemenu_items[i].subitems       = NULL;
       filemenu_items[i].set            = False;
       if (bract->flags & BrActToggle) {
          filemenu_items[i].class = &xmToggleButtonGadgetClass;
          if (bract->flags & BrActToggleOn) filemenu_items[i].set = True;
       } else {
          filemenu_items[i].class = &xmPushButtonGadgetClass;
       }
       filemenu_items[i].label = bract->text;
       filemenu_items[i].mnemonic = bract->text[0];
       filemenu_items[i].separator = (bract->flags & BrActSeparator);
  } /* end for( i = 0, bract = kmcls->graf; bract != NULL; ... */
  j = 0;
  while (nlev-- > 0) {
      /* Create a pulldown menu entry for each top level in the hierarchy */
      class = fstr0dup( chpid[nlev], 8 );
      kmcls1 = find_kmclass( class );
      free( class );
      if( kmcls1 != NULL ) {
          sprintf( stitle, "%s %s", kmcls1->title, name[nlev] );
          filemenu_items[i].label = str0dup( stitle );
          filemenu_items[i].accelerator    = NULL;
          filemenu_items[i].accel_text     = NULL;
          filemenu_items[i].callback       = NULL;
          filemenu_items[i].callback_data  = NULL;
          filemenu_items[i].default_button = False;
          if (bract->flags & BrActToggle) {
             filemenu_items[i].class = &xmToggleButtonGadgetClass;
          } else {
             filemenu_items[i].class = &xmPushButtonGadgetClass;
          }
          filemenu_items[i].separator = True;
          for( k = 0, bract1 = kmcls1->graf; bract1 != NULL;
               bract1 = bract1->next, k++ ) {
               if (k >= MAXITEMS || i+j+k >= MAXLEVEL*MAXITEMS ) break;
               ptag[i+j+k].flag = 3;
               ptag[i+j+k].panel = (event->state == ControlMask);
               ptag[i+j+k].bract = bract1;
               ptag[i+j+k].bract_next = bract1->next;
               strcpy( ptag[i+j+k].item, name[nlev] );
               for( nl = nlev - 1; nl >= 0; nl-- ) {
                 strcat( ptag[i+j+k].item, "," );
                 strcat( ptag[i+j+k].item, name[nl] );
               }
               strcpy( ptag[i+j+k].pathname, kmcls1->name );
               submenu_items[k].accelerator    = NULL;
               submenu_items[k].accel_text     = NULL;
               submenu_items[k].callback       = popup_activate;
               submenu_items[k].callback_data  = (caddr_t) &ptag[i+j+k];
               submenu_items[k].default_button = False;
               submenu_items[k].subitems       = NULL;
               if (bract->flags & BrActToggle) {
                  submenu_items[k].class = &xmToggleButtonGadgetClass;
               } else {
                  submenu_items[k].class = &xmPushButtonGadgetClass;
               }
               submenu_items[k].label = bract1->text;
               submenu_items[k].mnemonic = bract1->text[0];
               submenu_items[k].separator = (bract1->flags & BrActSeparator);
               }
          submenu_items[k].label = NULL;
          filemenu_items[i].subitems = submenu_items;
          i++; j = j+k-1;
        }
  } /* end  while (nlev-- > 0) */
  filemenu_items[i].label = NULL;
  filemenu_items[0].default_button = True;

  km_reset_graph_popup();
  graph_popup = km_build_popup (draw_area, title, filemenu_items,
                                  XmMENU_POPUP, &filemenu_default);

  /*
   * position the menu at the location of the button press!  If we wanted
   * to position it elsewhere, we could change the x,y fields of the
   * event structure.
   */
  XmMenuPosition(graph_popup, event);
  XtManageChild(graph_popup);
}

/***********************************************************************
 *                                                                     *
 *   Graphics window Event Handler on ButtonReleaseMask                *
 *                                                                     *
 ***********************************************************************/
static void drawing_area_br ( draw_area, tag, event )
     Widget  draw_area;
     int     tag;
     XButtonReleasedEvent *event;
{
  INTEGER ix = event->x;
  INTEGER iy = event->y;
  INTEGER iwin = tag;
  INTEGER icod;


  if( event->button == 3) return;

  /* call user defined routine */
  icod = - event->button;
  if (kjmpaddr.user_locate_F != NULL)
    (*kjmpaddr.user_locate_F) (&ix, &iy, &icod, &iwin);
}

/***********************************************************************
 *                                                                     *
 *   Graphics window Event Handler on PointerMotionMask                *
 *                                                                     *
 ***********************************************************************/
static void drawing_area_pm ( draw_area, tag, event )
     Widget  draw_area;
     int     tag;
     XPointerMovedEvent *event;
{
  INTEGER ix = event->x;
  INTEGER iy = event->y;
  INTEGER iwin = tag;
  INTEGER icod;


  if( event->state == Button3Mask) return;

  /* call user defined routine */
  icod = 0;
  if (kjmpaddr.user_locate_F != NULL)
    (*kjmpaddr.user_locate_F) (&ix, &iy, &icod, &iwin);
}

/******************************************************************************/
/*  km_reset_graph_popup : destroy popup menu defined in drawing area         */
/******************************************************************************/
void km_reset_graph_popup ()
{
    if (graph_popup != NULL) XtDestroyWidget (XtParent(graph_popup));
    graph_popup = NULL;
}
