/*
 * $Id: iconbox.c,v 1.1.1.1 1996/03/08 15:33:09 mclareni Exp $
 *
 * $Log: iconbox.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:09  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/14 25/07/94  11.45.22  by  Unknown*/
/*-- Author :    Nicole Cremel   27/05/92*/
/*
 * IconBox.c
 */

#include "kuip/kuip.h"

/***********************************************************************
 *                                                                     *
 *   IconBox (convenience) widget.                                     *
 *                                                                     *
 *   The IconBox widget facilitates the building of a table of which   *
 *   the items consist of an icon and a text label.                    *
 *   All user callable routines are denoted by the string:             *
 *   "user callable".                                                  *
 *                                                                     *
 *   IconBox is based on ideas by Thomas Berlage as discussed in his   *
 *   book OSF/Motif Concepts and Programming, ISBN 0-201-55792-4.      *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************
 *                                                                     *
 * Copyright 1992 Alphonse A. Rademakers                               *
 *                                                                     *
 * Permission to use, copy, modify, distribute, and sell this software *
 * and its documentation for any purpose is hereby granted without     *
 * fee, provided that the above copyright notice appears in all copies *
 * and that both that copyright notice and this permission notice      *
 * appear in supporting documentation, and that the name of            *
 * Alphonse A. Rademakers or CERN not be used in advertising or        *
 * publicity pertaining to distribution of the software without        *
 * specific, written prior permission.  Alphonse A. Rademakers and     *
 * CERN make no representations about the suitability of this software *
 * for any purpose.  It is provided "as is" without express or implied *
 * warranty.                                                           *
 *                                                                     *
 * Alphonse A. Rademakers and CERN disclaim all warranties with        *
 * regard to this software, including all implied warranties of        *
 * merchantability and fitness, in no event shall                      *
 * Alphonse A. Rademakers or CERN be liable for any special, indirect  *
 * or consequential damages or any damages whatsoever resulting from   *
 * loss of use, data or profits, whether in an action of contract,     *
 * negligence or other tortious action, arising out of or in           *
 * connection with the use or performance of this software.            *
 *                                                                     *
 * Author:                                                             *
 *      Alphonse A. Rademakers                                         *
 *      CN/AS Division                                                 *
 *      CERN, European Organization for Nuclear Research               *
 *      1123 Geneve, Switzerland                                       *
 *                                                                     *
 *      rdm@cernvm.cern.ch                                             *
 *                                                                     *
 ***********************************************************************/

/* #include "IconBoxP.h" */
#include "icboxp.h"

/* #include "default_icon.bm" */
#include "icdbm.h"
/* #include "default_sicon.bm" */
#include "icsdbm.h"

#ifndef min
#define min(a,b) (((a) < (b)) ? (a) : (b))
#endif
#ifndef max
#define max(a,b) (((a) > (b)) ? (a) : (b))
#endif

#define BOX_CLASS_NAME  "IconBox"

/********************* Resource list for icon record ******************/

static XtResource icon_resources [] = {
    { XmNiconBitmap, XmCIconBitmap, XmRString, sizeof (String),
      XtOffset (IconRecPtr, pix_name), XmRString, "default_icon"},
    { XmNiconMask, XmCIconMask, XmRString, sizeof (String),
      XtOffset (IconRecPtr, mask_name), XmRString, "default_mask"},
    { XmNiconSmallBitmap, XmCIconSmallBitmap, XmRString, sizeof (String),
      XtOffset (IconRecPtr, pix_sname), XmRString, "default_sicon"},
    { XmNiconSmallMask, XmCIconSmallMask, XmRString, sizeof (String),
      XtOffset (IconRecPtr, mask_sname), XmRString, "default_smask"},
    { XmNiconForeground, XmCIconForeground, XmRPixel, sizeof (Pixel),
      XtOffset (IconRecPtr, foreground), XmRString, "black"},
    { XmNiconBackground, XmCIconBackground, XmRPixel, sizeof (Pixel),
      XtOffset (IconRecPtr, background), XmRString, "white"},
    { XmNiconLabelForeground, XmCIconLabelForeground, XmRPixel, sizeof (Pixel),
      XtOffset (IconRecPtr, label_fg), XmRString, "black"},
    { XmNiconLabelBackground, XmCIconLabelBackground, XmRPixel, sizeof (Pixel),
      XtOffset (IconRecPtr, label_bg), XmRString, "white"},
};

/******************* Resource list for box record *********************/

static XtResource box_resources[] = {
    { XmNiconWidth, XmCIconWidth, XmRDimension, sizeof (Dimension),
      XtOffset (BoxRecPtr, min_big_width), XmRImmediate, (caddr_t) 40},
    { XmNiconHeight, XmCIconHeight, XmRDimension, sizeof (Dimension),
      XtOffset (BoxRecPtr, min_big_height), XmRImmediate, (caddr_t) 40},
    { XmNiconSmallWidth, XmCIconSmallWidth, XmRDimension, sizeof (Dimension),
      XtOffset (BoxRecPtr, min_small_width), XmRImmediate, (caddr_t) 20},
    { XmNiconSmallHeight, XmCIconSmallHeight, XmRDimension, sizeof (Dimension),
      XtOffset (BoxRecPtr, min_small_height), XmRImmediate, (caddr_t) 20},
    { XmNiconHSpacing, XmCIconHSpacing, XmRInt, sizeof (int),
      XtOffset (BoxRecPtr, icon_bhspacing), XmRImmediate, (caddr_t) 40},
    { XmNiconVSpacing, XmCIconVSpacing, XmRInt, sizeof (int),
      XtOffset (BoxRecPtr, icon_bvspacing), XmRImmediate, (caddr_t) 10},
    { XmNiconSmallHSpacing, XmCIconSmallHSpacing, XmRInt, sizeof (int),
      XtOffset (BoxRecPtr, icon_shspacing), XmRImmediate, (caddr_t) 20},
    { XmNiconSmallVSpacing, XmCIconSmallVSpacing, XmRInt, sizeof (int),
      XtOffset (BoxRecPtr, icon_svspacing), XmRImmediate, (caddr_t) 10},
    { XmNiconNoHSpacing, XmCIconNoHSpacing, XmRInt, sizeof (int),
      XtOffset (BoxRecPtr, icon_nhspacing), XmRImmediate, (caddr_t) 20},
    { XmNiconNoVSpacing, XmCIconNoVSpacing, XmRInt, sizeof (int),
      XtOffset (BoxRecPtr, icon_nvspacing), XmRImmediate, (caddr_t) 10},
    { XmNiconType, XmCIconType, XmRInt, sizeof (int),
      XtOffset (BoxRecPtr, icon_type), XmRImmediate, (caddr_t) IconBoxBIG_ICON},
    { XmNshadowOffset, XmCShadowOffset, XmRInt, sizeof (int),
      XtOffset (BoxRecPtr, shadow_off), XmRImmediate, (caddr_t) 3},
    { XmNcolumns, XmCColumns, XmRInt, sizeof (int),
      XtOffset (BoxRecPtr, columns), XmRImmediate, (caddr_t) 5},
    { XmNshadowColor, XmCShadowColor, XmRPixel, sizeof (Pixel),
      XtOffset (BoxRecPtr, shadow_color), XmRString, "gray50"},
    { XmNfontList, XmCFontList, XmRFontList, sizeof (XmFontList),
      XtOffset (BoxRecPtr, fontlist), XmRString, "fixed"},
    { XmNzoomEffect, XmCZoomEffect, XmRBoolean, sizeof (Boolean),
      XtOffset (BoxRecPtr, zoom_effect), XmRImmediate, (caddr_t) True},
    { XmNzoomSpeed, XmCZoomSpeed, XmRInt, sizeof (int),
      XtOffset (BoxRecPtr, zoom_speed), XmRImmediate, (caddr_t) 10},
    { XmNdoubleClickInterval, XmCDoubleClickInterval, XmRInt, sizeof (int),
      XtOffset (BoxRecPtr, dc_interval), XmRImmediate, (caddr_t) 250},
};


/***********************************************************************
 *                                                                     *
 *   Forward declarations of local functions.                          *
 *                                                                     *
 ***********************************************************************/

static C_PROTO_3(void ExposeBox,
                         Widget widget,
                         BoxRec *rec,
                         XmDrawingAreaCallbackStruct *call_data);
static C_PROTO_3(void HandleButtons,
                         Widget widget,
                         BoxRec *rec,
                         XmDrawingAreaCallbackStruct *call_data);
static C_PROTO_3(void HandleGraphicsExposures,
                         Widget widget,
                         BoxRec *rec,
                         XEvent *ev);
static C_PROTO_3(void HandleMove,
                         Widget widget,
                         BoxRec *rec,
                         XEvent *ev);
static C_PROTO_3(void HandleResize,
                         Widget widget,
                         BoxRec *rec,
                         XmDrawingAreaCallbackStruct *call_data);
static C_PROTO_3(void DestroyIconBox,
                         Widget widget,
                         BoxRec *rec,
                         caddr_t call_data);
static C_PROTO_3(void ScrollBarMoved,
                         Widget widget,
                         BoxRec *rec,
                         XmScrollBarCallbackStruct *call_data);
static C_PROTO_1(void SendExpose,
                         Widget widget);
static C_PROTO_6(void RedrawRegion,
                         Widget widget,
                         BoxRec *rec,
                         int x,
                         int y,
                         Dimension width,
                         Dimension height);
static C_PROTO_1(void UpdatePositions,
                         BoxRec *rec);
static C_PROTO_1(void RepositionIcons,
                         BoxRec *rec);
static C_PROTO_6(void CenterPixmap,
                         BoxRec *rec,
                         IconRec *icon,
                         Cardinal big_width,
                         Cardinal big_height,
                         Cardinal small_width,
                         Cardinal small_height);
static C_PROTO_3(void RecalculateIconOffset,
                         BoxRec *rec,
                         Boolean recalc_offset,
                         Boolean recalc_soffset);


/***********************************************************************
 *                                                                     *
 *   Initialize one icon record.                                       *
 *                                                                     *
 ***********************************************************************/
static void InitIcon (widget, rec, icon, item, class, position)
       Widget widget;
       BoxRec *rec;
       IconRec *icon;
       XmString item;
       char *class;
       int position;
{
   int         pos;
   IconRec    *tmp;
   Boolean     new_pixmap;
   Display    *display;

   rec->no_icons++;
   rec->last_id++;
   icon->id = rec->last_id;

   /* get resources for icon record */
   XtGetSubresources (widget, icon, "name", class,
                      icon_resources, XtNumber(icon_resources), NULL, 0);

   /*
    * make copies of the pix_name, pix_sname, pix_mask and pix_smask so
    * we can modify its values without disturbing the resource list
    */
   icon->pix_name   = XtNewString(icon->pix_name);
   icon->mask_name  = XtNewString(icon->mask_name);
   icon->pix_sname  = XtNewString(icon->pix_sname);
   icon->mask_sname = XtNewString(icon->mask_sname);

   /* set label and its offset to the left of icon */
   icon->label = XmStringCopy(item);
   icon->string_left = ((int) XmStringWidth (rec->fontlist, icon->label) -
                        (int) rec->icon_width + 1) / 2;
   if (icon->string_left < 0) icon->string_left = 0;
   icon->class = XtMalloc(strlen(class) + 1);
   strcpy(icon->class, class);

   /* calculate initial position */
   if (position == 0)
      pos = rec->no_icons;
   else
      pos = position;

   pos--;
   icon->x = (pos % rec->columns) *
      (rec->icon_width + rec->icon_hspacing) +
      rec->icon_hspacing/2;
   icon->y = (pos / rec->columns) *
      (rec->icon_height + rec->string_height + rec->icon_vspacing) +
      rec->icon_vspacing/2;

   /* icons are unselected */
   icon->selected = False;

   new_pixmap = False;

   display = XtDisplay(widget);

   /*
    *  Get cached big and small icon pixmaps from Motif.
    *  Before calling XmGetPixmap check if the icon name or the class
    *  has already been used. In that case we may skip the XmGetPixmap call
    *  which can be extremely expensive when the user's file system is NFS
    *  mounted (the user's filesystem is in the XmGetPixmap search path).
    */
   tmp = rec->icons;
   while (tmp) {
      if ((tmp->pix_name && !strcmp(tmp->pix_name, icon->pix_name)) ||
          (tmp->class    && !strcmp(tmp->class, icon->class))) {
         icon->pix    = tmp->pix;
         icon->hi_pix = tmp->hi_pix;
         icon->x_off  = tmp->x_off;
         icon->y_off  = tmp->y_off;
         icon->pix_h  = tmp->pix_h;
         icon->pix_w  = tmp->pix_w;
         break;
      }
      tmp = tmp->link;
   }
   if (!tmp) {
      icon->pix = XmGetPixmap(XDefaultScreenOfDisplay(display),
                              icon->pix_name,
                              icon->foreground, icon->background);

      icon->hi_pix = XmGetPixmap(XDefaultScreenOfDisplay(display),
                                 icon->pix_name,
                                 icon->background, icon->foreground);

      /* big icon pixmap not found use built-in default */
      if (icon->pix == XmUNSPECIFIED_PIXMAP) {
         if (!rec->box_icon) {
            rec->box_icon = XCreatePixmapFromBitmapData(display,
                         DefaultRootWindow(display),
                         (char*)default_icon_bits,
                         default_icon_width, default_icon_height,
                         icon->foreground, icon->background,
                         DefaultDepthOfScreen(XtScreen(widget)));

            rec->box_hi_icon = XCreatePixmapFromBitmapData(display,
                         DefaultRootWindow(display),
                         (char*)default_icon_bits,
                         default_icon_width, default_icon_height,
                         icon->background, icon->foreground,
                         DefaultDepthOfScreen(XtScreen(widget)));
         }
         icon->pix    = rec->box_icon;
         icon->hi_pix = rec->box_hi_icon;
      }

      new_pixmap = True;
   }

   /* idem for small icons */
   tmp = rec->icons;
   while (tmp) {
      if ((tmp->pix_sname && !strcmp(tmp->pix_sname, icon->pix_sname)) ||
          (tmp->class     && !strcmp(tmp->class, icon->class))) {
         icon->spix    = tmp->spix;
         icon->hi_spix = tmp->hi_spix;
         icon->x_soff  = tmp->x_soff;
         icon->y_soff  = tmp->y_soff;
         icon->spix_w  = tmp->spix_w;
         icon->spix_h  = tmp->spix_h;
         break;
      }
      tmp = tmp->link;
   }
   if (!tmp) {
      icon->spix = XmGetPixmap(XDefaultScreenOfDisplay(display),
                               icon->pix_sname,
                               icon->foreground, icon->background);

      icon->hi_spix = XmGetPixmap(XDefaultScreenOfDisplay(display),
                                  icon->pix_sname,
                                  icon->background, icon->foreground);

      /* small icon pixmap not found, use built in default */
      if (icon->spix == XmUNSPECIFIED_PIXMAP) {
         if (!rec->box_sicon) {
            rec->box_sicon = XCreatePixmapFromBitmapData(display,
                         DefaultRootWindow(display),
                         (char*)default_sicon_bits,
                         default_sicon_width, default_sicon_height,
                         icon->foreground, icon->background,
                         DefaultDepthOfScreen(XtScreen(widget)));

            rec->box_hi_sicon = XCreatePixmapFromBitmapData(display,
                         DefaultRootWindow(display),
                         (char*)default_sicon_bits,
                         default_sicon_width, default_sicon_height,
                         icon->background, icon->foreground,
                         DefaultDepthOfScreen(XtScreen(widget)));
         }
         icon->spix    = rec->box_sicon;
         icon->hi_spix = rec->box_hi_sicon;
      }

      new_pixmap = True;
   }

   if (new_pixmap)
      CenterPixmap(rec, icon,
                   rec->icon_big_width, rec->icon_big_height,
                   rec->icon_small_width, rec->icon_small_height);

   /* big icon mask must be bitmap, so get and cache it ourself */
   /********** use no mask for the time being
   tmp = rec->icons;
   while (tmp) {
      if (tmp->mask_name != NULL &&
          strcmp(tmp->mask_name, icon->mask_name) == 0) {
         icon->mask = tmp->mask;
         break;
      }
      tmp = tmp->link;
   }
   if (!tmp)
       int         dummy;
       Cardinal    dum;
      if (XReadBitmapFile (XtDisplay (widget),
                           XDefaultRootWindow (XtDisplay (widget)),
                           icon->mask_name, &dum, &dum, &icon->mask,
                           &dummy, &dummy) != BitmapSuccess)
         icon->mask = XCreateBitmapFromData(display, DefaultRootWindow(display),
                                  default_mask_bits,
                                  default_mask_width, default_mask_height);
   ***********/

   icon->mask = None;

   /* small icon mask must be bitmap, so get and cache it ourself */
   /********** use no mask for the time being
   tmp = rec->icons;
   while (tmp) {
      if (tmp->mask_sname != NULL &&
          strcmp(tmp->mask_sname, icon->mask_sname) == 0) {
         icon->smask = tmp->smask;
         break;
      }
      tmp = tmp->link;
   }
   if (!tmp)
       int         dummy;
       Cardinal    dum;
      if (XReadBitmapFile (XtDisplay (widget),
                           XDefaultRootWindow (XtDisplay (widget)),
                           icon->mask_sname, &dum, &dum, &icon->smask,
                           &dummy, &dummy) != BitmapSuccess)
         icon->smask = XCreateBitmapFromData(display,DefaultRootWindow(display),
                                  default_smask_bits,
                                  default_smask_width, default_smask_height);
   ************/

   icon->smask = None;
}

/***********************************************************************
 *                                                                     *
 *   Set the scrollbar resources.                                      *
 *                                                                     *
 ***********************************************************************/
static void SetScrollBar(widget, rec)
       Widget widget;
       BoxRec *rec;
{
   Arg        args[4];
   Widget     parent;
   Dimension  w, h;
   int        value, size;

   parent = XtParent(widget);

   /* get the size of the parent */
   XtSetArg(args[0], XmNwidth, &w);
   XtSetArg(args[1], XmNheight, &h);
   XtGetValues(parent, args, 2);

   XtSetArg(args[0], XmNvalue, &value);
   XtGetValues(rec->scrollbar, args, 1);

   size = (int)(h*h) / (int) (rec->virt_height);
   if (size < 1) size = 1;
   if (size > (int) (rec->virt_height)) size = rec->virt_height;
   if (value > (int) (rec->virt_height-size)) {
      value = rec->virt_height-size;
      rec->top = -value;
   }
   XtSetArg(args[0], XmNmaximum, rec->virt_height);
   XtSetArg(args[1], XmNsliderSize, size);
   XtSetArg(args[2], XmNvalue, value);
   XtSetArg(args[3], XmNincrement, 5);
   XtSetValues(rec->scrollbar, args, 4);

   /****************
   if (size < rec->virt_height)
      XmScrolledWindowSetAreas(parent, NULL, rec->scrollbar, widget);
   else
      XmScrolledWindowSetAreas(parent, NULL, NULL, widget);
   *****************/
}

/***********************************************************************
 *                                                                     *
 *   Calculate the virtual height of the icon box.                     *
 *                                                                     *
 ***********************************************************************/
static int GetVirtHeight(rec)
       BoxRec *rec;
{
   int h;

   h = (((rec->no_icons - 1)/ rec->columns) + 1) *
      (rec->icon_height + rec->string_height + rec->icon_vspacing);
   if (h < (int) (rec->height)) h = rec->height;

   return h;
}

/***********************************************************************
 *                                                                     *
 *   Get current IconBox scrollbar position (user callable).           *
 *                                                                     *
 ***********************************************************************/
int IconBoxGetPos(widget)
    Widget widget;
{
   BoxRec     *rec;
   int         value;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);
   if (!rec) return 0;

   XtVaGetValues(rec->scrollbar, XmNvalue, &value, NULL);

   return value;
}

/***********************************************************************
 *                                                                     *
 *   Set IconBox scrollbar position (user callable).                   *
 *                                                                     *
 ***********************************************************************/
void IconBoxSetPos(widget, value)
     Widget widget;
     int value;
{
   BoxRec     *rec;
   int         maxi, mini, val, val1, slz, inc, pinc;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);
   if (!rec) return;

   XmScrollBarGetValues(rec->scrollbar, &val, &slz, &inc, &pinc);
   XtVaGetValues(rec->scrollbar, XmNminimum, &mini, XmNmaximum, &maxi, NULL);
   val1 = maxi - mini - slz;
   if (value > val1) value = val1;

   if (value != val)
      XmScrollBarSetValues(rec->scrollbar, value, slz, inc, pinc, True);
}

/***********************************************************************
 *                                                                     *
 *   Makes row containing item the first visible row (user callable).  *
 *                                                                     *
 ***********************************************************************/
void IconBoxSetItem(widget, item)
     Widget widget;
     XmString item;
{
   BoxRec     *rec;
   IconRec    *icon;
   int         top = 0;
   int         i;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);
   if (!rec || !rec->icons) return;

   /*
    * if refresh has never been called before the scrollbar is not
    * yet set properly and the icon_height may also not be up-to-date
    */
   IconBoxRefresh(widget);

   icon = rec->icons;

   i = 0;
   while (icon) {
      i++;
      if (XmStringCompare(icon->label, item)) {
         top = ((i - 1)/ rec->columns) *
            (rec->icon_height + rec->string_height + rec->icon_vspacing);
         break;
      }
      icon = icon->link;
   }

   if (icon)
      IconBoxSetPos(widget, top);
}

/***********************************************************************
 *                                                                     *
 *   Set icon and string dimensions as function of the icon type.      *
 *                                                                     *
 ***********************************************************************/
static void SetIconDimensions(rec)
       BoxRec *rec;
{
   XmString  xms;
   IconRec  *icon;
   int       maxl, strlft;

   /* find longest icon label, minimum length is maxl */
   xms = XmStringCreateLtoR("WWWWWWWWWWW", XmSTRING_DEFAULT_CHARSET);
   maxl = (int)XmStringWidth(rec->fontlist, xms);
   XmStringFree(xms);
   icon = rec->icons;
   while (icon) {
      maxl = max(maxl, (int)XmStringWidth(rec->fontlist, icon->label));
      icon = icon->link;
   }

   /*
    * set current icon height and width, vert. and horz. spacing
    * depending on the default icon type
    */
   switch (rec->icon_type) {
      case IconBoxBIG_ICON:
         rec->string_height = rec->string_bheight;
         rec->icon_width    = rec->icon_big_width;
         rec->icon_height   = rec->icon_big_height + rec->ilsp;
         strlft = (maxl - (int) rec->icon_width +1) / 2;
         if (strlft > rec->icon_bhspacing/2)
            rec->icon_hspacing = 2 * strlft + 10;
         else
            rec->icon_hspacing = rec->icon_bhspacing;
         rec->icon_vspacing = rec->icon_bvspacing;
         icon = rec->icons;
         while (icon) {
            icon->string_left = ((int)XmStringWidth(rec->fontlist, icon->label)-
                                 (int)rec->icon_width + 1) / 2;
            if (icon->string_left < 0) icon->string_left = 0;
            icon = icon->link;
         }
         break;
      case IconBoxSMALL_ICON:
      case IconBoxTITLES:
         rec->string_height = 0;
         rec->icon_width    = rec->icon_small_width + rec->silsp + maxl;
         rec->icon_height   = rec->icon_small_height;
         rec->icon_hspacing = rec->icon_shspacing;
         rec->icon_vspacing = rec->icon_svspacing;
         icon = rec->icons;
         while (icon) {
            icon->string_left = 0;
            icon = icon->link;
         }
         break;
      case IconBoxNO_ICON:
         rec->string_height = 0;
         rec->icon_width    = maxl;
         rec->icon_height   = rec->string_bheight;
         rec->icon_hspacing = rec->icon_nhspacing;
         rec->icon_vspacing = rec->icon_nvspacing;
         icon = rec->icons;
         while (icon) {
            icon->string_left = 0;
            icon = icon->link;
         }
   }
}

/***********************************************************************
 *                                                                     *
 *   Recalculate the offsets of the pixmaps if the size of the minimum *
 *   pixmap changes.                                                   *
 *   Default for the big icons is 40x40 and for the small icons        *
 *   20x20.                                                            *
 *                                                                     *
 ***********************************************************************/
static C_DECL_3(void RecalculateIconOffset,
       BoxRec *, rec,
       Boolean, recalc_offset,
       Boolean, recalc_soffset)
{
   IconRec    *icon;
   int         dummy;
   Cardinal    dum, wp, hp;
   Display    *display;
   Window      root;

   display = XtDisplay(rec->area);

   icon = rec->icons;
   while (icon) {
      if (recalc_offset) {
         XGetGeometry(display, icon->pix, &root, &dummy, &dummy,
                      &wp, &hp, &dum, &dum);
         icon->x_off = (rec->icon_big_width - wp) / 2;
         icon->y_off = (rec->icon_big_height - hp) / 2;
         if (icon->x_off < 0) icon->x_off = 0;
         if (icon->y_off < 0) icon->y_off = 0;
      }
      if (recalc_soffset) {
         XGetGeometry(display, icon->spix, &root, &dummy, &dummy,
                      &wp, &hp, &dum, &dum);
         icon->x_soff = (rec->icon_small_width - wp) / 2;
         icon->y_soff = (rec->icon_small_height - hp) / 2;
         if (icon->x_soff < 0) icon->x_soff = 0;
         if (icon->y_soff < 0) icon->y_soff = 0;
      }
      icon = icon->link;
   }
}

/***********************************************************************
 *                                                                     *
 *   Center the big and small pixmaps in the minimum (by default the   *
 *   min. big pixmap = 40x40 and small pixmap is 20x20) pixmap. If the *
 *   new pixmaps are larger then the min. pixmaps they become the new  *
 *   min. pixmaps. In this case all pixmaps of the existing items will *
 *   be re-centered in the new min. pixmaps.                           *
 *                                                                     *
 *   If icon=NULL then scan the pixmaps of all icons to find the a     *
 *   new min. pixmap. The default min. pixmaps is given by the         *
 *   arguments big_width, ...                                          *
 *                                                                     *
 ***********************************************************************/
static void CenterPixmap(rec, icon,
                         big_width, big_height, small_width, small_height)
       BoxRec *rec;
       IconRec *icon;
       Cardinal big_width,  big_height, small_width, small_height;
{
   Display    *display;
   int         dummy;
   Cardinal    dum, wp, hp, wt, ht;
   Window      root;
   Boolean     recalc_offset, recalc_soffset;

   recalc_offset  = False;
   recalc_soffset = False;

   display = XtDisplay(rec->area);

   if (icon && big_width > 0 && big_height > 0) {
      /*
       * get size of pixmap and calculate the offsets from the full pixmap
       * (default 40x40) to center the pixmap in the full pixmap
       */
      XGetGeometry(display, icon->pix, &root, &dummy, &dummy,
                   &wp, &hp, &dum, &dum);
      wt = max(big_width, wp);
      ht = max(big_height, hp);
      if (wt != big_width) {
         rec->icon_big_width = (Dimension)wt;
         recalc_offset = True;
      }
      if (ht != big_height) {
         rec->icon_big_height = (Dimension)ht;
         recalc_offset = True;
      }
      icon->x_off = (rec->icon_big_width - wp) / 2;
      icon->y_off = (rec->icon_big_height - hp) / 2;
      if (icon->x_off < 0) icon->x_off = 0;
      if (icon->y_off < 0) icon->y_off = 0;
      icon->pix_w = (Dimension)wp;
      icon->pix_h = (Dimension)hp;
   }

   if (icon && small_width > 0 && small_height > 0) {
      /*
       * get size of small pixmap and calculate the offsets from the full
       * pixmap (default 20x20) to center the pixmap in the full pixmap
       */
      XGetGeometry(display, icon->spix, &root, &dummy, &dummy,
                   &wp, &hp, &dum, &dum);
      wt = max(small_width, wp);
      ht = max(small_height, hp);
      if (wt != small_width) {
         rec->icon_small_width = (Dimension)wt;
         recalc_soffset = True;
      }
      if (ht != small_height) {
         rec->icon_small_height = (Dimension)ht;
         recalc_soffset = True;
      }
      icon->x_soff = (rec->icon_small_width - wp) / 2;
      icon->y_soff = (rec->icon_small_height - hp) / 2;
      if (icon->x_soff < 0) icon->x_soff = 0;
      if (icon->y_soff < 0) icon->y_soff = 0;
      icon->spix_w = (Dimension)wp;
      icon->spix_h = (Dimension)hp;
   }

   if (!icon) {
      rec->icon_big_width    = big_width;
      rec->icon_big_height   = big_height;
      rec->icon_small_width  = small_width;
      rec->icon_small_height = small_height;
      recalc_offset  = True;
      recalc_soffset = True;
      icon = rec->icons;
      while (icon) {
         XGetGeometry(display, icon->pix, &root, &dummy, &dummy,
                      &wp, &hp, &dum, &dum);
         rec->icon_big_width  = max(rec->icon_big_width, wp);
         rec->icon_big_height = max(rec->icon_big_height, hp);

         XGetGeometry(display, icon->spix, &root, &dummy, &dummy,
                      &wp, &hp, &dum, &dum);
         rec->icon_small_width  = max(rec->icon_small_width, wp);
         rec->icon_small_height = max(rec->icon_small_height, hp);

         icon = icon->link;
      }
   }

   /* if size of pixmap increased recalculate offsets for all icons */
   if (recalc_offset || recalc_soffset)
      RecalculateIconOffset(rec, recalc_offset, recalc_soffset);
}

/***********************************************************************
 *                                                                     *
 *   Set IconBox icon type, either IconBoxBIG_ICON, IconBoxSMALL_ICON  *
 *   or IconBoxNO_ICON (user callable) or IconBoxTITLES.               *
 *                                                                     *
 ***********************************************************************/
void IconBoxSetIconType(widget, type)
     Widget widget;
     int type;
{
   BoxRec     *rec;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);
   if (rec == NULL) return;

   if (rec->icon_type == type) return;

   rec->icon_type = type;

   IconBoxRefresh(widget);
}

/***********************************************************************
 *                                                                     *
 *   Get IconBox icon type, either IconBoxBIG_ICON, IconBoxSMALL_ICON  *
 *   or IconBoxNO_ICON (user callable) or IconBoxTITLES.               *
 *                                                                     *
 ***********************************************************************/
int IconBoxGetIconType(widget)
    Widget widget;
{
   BoxRec     *rec;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);
   if (rec == NULL) return IconBoxNO_ICON;

   return rec->icon_type;
}

/***********************************************************************
 *                                                                     *
 *   Add icon to the IconBox.                                          *
 *                                                                     *
 ***********************************************************************/
IconRec *AddItem(rec, item, class, position)
        BoxRec *rec;
        XmString item;
        char *class;
        int position;
{
   IconRec    *icon;
   IconRec    *tmp;
   int         i;

   /* allocate icon records and initialize to 0 */
   icon = (IconRec*) XtCalloc (1, sizeof (IconRec));

   InitIcon(rec->area, rec, icon, item, class, position);

   /* attach next icon at end of icon list if position = 0, or at position */
   if (!rec->icons) {
      rec->icons     = icon;
      rec->last_icon = icon;
   } else {
      if (position == 0) {
         rec->last_icon->link = icon;
         rec->last_icon       = icon;
      } else if (position == 1) {
         icon->link = rec->icons;
         rec->icons = icon;
      } else {
         i   = 1;
         tmp = rec->icons;
         while (i < position-1) {
            i++;
            tmp = tmp->link;
            if (!tmp) {
               rec->last_icon->link = icon;
               rec->last_icon       = icon;
               goto out;
            }
         }
         icon->link = tmp->link;
         tmp->link  = icon;
      }
   }

out:
   return(icon);
}

/***********************************************************************
 *                                                                     *
 *   Add icon to the IconBox (user callable).                          *
 *                                                                     *
 ***********************************************************************/
C_DECL_5(void IconBoxAddItem,
     Widget, widget,
     XmString, item,
     char *, class,
     int, position,
     Boolean, refresh)
{
   Arg         args[1];
   BoxRec     *rec;

   XtSetArg (args[0], XmNuserData, &rec);
   XtGetValues (widget, args, 1);

   if (rec == NULL) return;

   AddItem(rec, item, class, position);

   if (refresh)
      IconBoxRefresh(widget);
}

/***********************************************************************
 *                                                                     *
 *   Add icon with predfined pixmaps to the IconBox (user callable).   *
 *   The user may define 4 Pixmaps: big_icon (40x40), highlighted      *
 *   big_icon, small_icon (20x20) and highlighted small_icon.          *
 *                                                                     *
 ***********************************************************************/
C_DECL_9(void IconBoxAddItemPixmap,
     Widget, widget,
     XmString, item,
     char *, class,
     Pixmap, bpix,
     Pixmap, bhpix,
     Pixmap, spix,
     Pixmap, shpix,
     int, position,
     Boolean, refresh)
{
   Arg         args[1];
   BoxRec     *rec;
   IconRec    *icon;

   XtSetArg(args[0], XmNuserData, &rec);
   XtGetValues(widget, args, 1);

   if (rec == NULL) return;

   icon = AddItem(rec, item, class, position);

   if (bpix)  icon->pix     = bpix;
   if (bhpix) icon->hi_pix  = bhpix;
   if (spix)  icon->spix    = spix;
   if (shpix) icon->hi_spix = shpix;

   if (bpix || bhpix || spix || shpix)
      CenterPixmap(rec, icon,
                   rec->icon_big_width, rec->icon_big_height,
                   rec->icon_small_width, rec->icon_small_height);

   if (refresh)
      IconBoxRefresh(widget);
}

/***********************************************************************
 *                                                                     *
 *   Add icon with predfined bitmap data to the IconBox                *
 *   (user callable).                                                  *
 *   The user must define the name, bits, width and height of the big  *
 *   and small icons.                                                  *
 *                                                                     *
 ***********************************************************************/
C_DECL_13(void IconBoxAddItemBitmapData,
     Widget,   widget,
     XmString, item,
     char *,   class,
     char *,   name,
     char *,   bits,
     int,      wd,
     int,      ht,
     char *,   sname,
     char *,   sbits,
     int,      swd,
     int,      sht,
     int,      position,
     Boolean,  refresh)
{
   BoxRec     *rec;
   IconRec    *icon;
   IconRec    *tmp = NULL;
   Display    *display;
   Boolean     new_pixmap = False;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);

   if (!rec) return;

   icon = AddItem(rec, item, class, position);

   display = XtDisplay(widget);

   if (bits && wd > 0 && ht >0) {
      if (name && strlen(name) > 0) {
         /*
          *  To prevent un-necessary calls to XCreatePixmapFromBitmapData,
          *  get pixmap from first icon with the same pixmap name.
          */
         tmp = rec->icons;
         while (tmp) {
            if (strcmp(tmp->pix_name, name) == 0) {
               icon->pix    = tmp->pix;
               icon->hi_pix = tmp->hi_pix;
               icon->x_off  = tmp->x_off;
               icon->y_off  = tmp->y_off;
               icon->pix_h  = tmp->pix_h;
               icon->pix_w  = tmp->pix_w;
               break;
            }
            tmp = tmp->link;
         }
         XtFree(icon->pix_name);
         icon->pix_name = XtNewString(name);
      }
      if (!tmp) {
         icon->pix = XCreatePixmapFromBitmapData(display,
                                DefaultRootWindow(display),
                                bits, wd, ht,
                                icon->foreground, icon->background,
                                DefaultDepthOfScreen(XtScreen(rec->area)));

         icon->hi_pix = XCreatePixmapFromBitmapData(display,
                                DefaultRootWindow(display),
                                bits, wd, ht,
                                icon->background, icon->foreground,
                                DefaultDepthOfScreen(XtScreen(rec->area)));

         new_pixmap = True;
      }
   }

   if (sbits && swd > 0 && sht >0) {
      if (sname && strlen(sname) > 0) {
         /* idem for the small icons */
         tmp = rec->icons;
         while (tmp) {
            if (strcmp(tmp->pix_sname, sname) == 0) {
               icon->spix    = tmp->spix;
               icon->hi_spix = tmp->hi_spix;
               icon->x_soff  = tmp->x_soff;
               icon->y_soff  = tmp->y_soff;
               icon->spix_w  = tmp->spix_w;
               icon->spix_h  = tmp->spix_h;
               break;
            }
            tmp = tmp->link;
         }
         XtFree(icon->pix_sname);
         icon->pix_sname = XtNewString(sname);
      }
      if (!tmp) {
         icon->spix = XCreatePixmapFromBitmapData(display,
                                DefaultRootWindow(display),
                                sbits, swd, sht,
                                icon->foreground, icon->background,
                                DefaultDepthOfScreen(XtScreen(rec->area)));

         icon->hi_spix = XCreatePixmapFromBitmapData(display,
                                DefaultRootWindow(display),
                                sbits, swd, sht,
                                icon->background, icon->foreground,
                                DefaultDepthOfScreen(XtScreen(rec->area)));

         new_pixmap = True;
      }
   }

   if (new_pixmap)
      CenterPixmap(rec, icon,
                   rec->icon_big_width, rec->icon_big_height,
                   rec->icon_small_width, rec->icon_small_height);

   if (refresh)
      IconBoxRefresh(widget);
}

/***********************************************************************
 *                                                                     *
 *   Change pixmaps of icon with label item (user callable).           *
 *                                                                     *
 ***********************************************************************/
C_DECL_7(void IconBoxChangeItemPixmap,
     Widget, widget,
     XmString, item,
     Pixmap, bpix,
     Pixmap, bhpix,
     Pixmap, spix,
     Pixmap, shpix,
     Boolean, refresh)
{
   Arg         args[1];
   BoxRec     *rec;
   IconRec    *icon;

   XtSetArg(args[0], XmNuserData, &rec);
   XtGetValues(widget, args, 1);

   if (!rec || !rec->icons) return;

   icon = rec->icons;

   while (icon) {
      if (XmStringCompare(icon->label, item)) {
         if (bpix)  icon->pix     = bpix;
         if (bhpix) icon->hi_pix  = bhpix;
         if (spix)  icon->spix    = spix;
         if (shpix) icon->hi_spix = shpix;
         break;
      }
      icon = icon->link;
   }

   if (icon) {
      CenterPixmap(rec, icon,
                   rec->icon_big_width, rec->icon_big_height,
                   rec->icon_small_width, rec->icon_small_height);

      if (refresh)
         IconBoxRefresh(widget);
   }
}

/***********************************************************************
 *                                                                     *
 *   Change pixmaps of icon in position pos (user callable).           *
 *                                                                     *
 ***********************************************************************/
C_DECL_7(void IconBoxChangePosPixmap,
     Widget, widget,
     int, pos,
     Pixmap, bpix,
     Pixmap, bhpix,
     Pixmap, spix,
     Pixmap, shpix,
     Boolean, refresh)
{
   Arg         args[1];
   BoxRec     *rec;
   IconRec    *icon;
   int         ipos;

   XtSetArg(args[0], XmNuserData, &rec);
   XtGetValues(widget, args, 1);

   if (!rec || !rec->icons) return;

   ipos = 0;
   icon = rec->icons;

   while (icon) {
      ipos++;
      if (pos == ipos) {
         if (bpix)  icon->pix     = bpix;
         if (bhpix) icon->hi_pix  = bhpix;
         if (spix)  icon->spix    = spix;
         if (shpix) icon->hi_spix = shpix;
         break;
      }
      icon = icon->link;
   }

   if (icon) {
      CenterPixmap(rec, icon,
                   rec->icon_big_width, rec->icon_big_height,
                   rec->icon_small_width, rec->icon_small_height);

      if (refresh)
         IconBoxRefresh(widget);
   }
}

/***********************************************************************
 *                                                                     *
 *  Update icon spacing, repostion the icons, and if necessary update  *
 *  the scrollbar.                                                     *
 *                                                                     *
 ***********************************************************************/
static void UpdatePositions(rec)
   BoxRec *rec;
{
   int h;

   /*
    *  update the icon spacing depending on the label
    *  lenght and icon size
    */
   SetIconDimensions(rec);

   /* repostion the icons */
   RepositionIcons(rec);

   /* if the virtual height of the box changed adjust scrollbar slider */
   h = GetVirtHeight(rec);
   if (h != rec->virt_height) {
      rec->virt_height = h;
      SetScrollBar(rec->area, rec);
   }
}

/***********************************************************************
 *                                                                     *
 *   Refresh the IconBox (user callable).                              *
 *                                                                     *
 ***********************************************************************/
void IconBoxRefresh(widget)
     Widget widget;
{
   Arg         args[1];
   BoxRec     *rec;

   XtSetArg (args[0], XmNuserData, &rec);
   XtGetValues (widget, args, 1);

   if (rec == NULL) return;

   UpdatePositions(rec);

   if (rec->exposed)
      RedrawRegion(widget, rec, 0, 0, rec->width, rec->height);
}

/***********************************************************************
 *                                                                     *
 *   Create a scrollbar to scroll the iconbox window.                  *
 *                                                                     *
 ***********************************************************************/
static void CreateScrollBar(parent, rec, work_area)
       Widget parent;
       BoxRec *rec;
       Widget work_area;
{
   Widget  sb;
   Arg     args[3];

   XtSetArg(args[0], XmNminimum, 0);
   XtSetArg(args[1], XmNincrement, 1);
   XtSetArg(args[2], XmNpageIncrement,
            rec->icon_height + rec->string_height + rec->icon_vspacing);
   sb = XtCreateManagedWidget("scrollbar", xmScrollBarWidgetClass,
                              parent, args, 3);

   XtAddCallback(sb, XmNvalueChangedCallback,
                 (XtCallbackProc)ScrollBarMoved, rec);
   XtAddCallback(sb, XmNdragCallback, (XtCallbackProc)ScrollBarMoved, rec);

   rec->scrollbar = sb;
   rec->scrolled  = False;

   XmScrolledWindowSetAreas(parent, NULL, sb, work_area);

   SetScrollBar(work_area, rec);
}

/***********************************************************************
 *                                                                     *
 *   Add callbacks (user callable).                                    *
 *                                                                     *
 ***********************************************************************/
void IconBoxAddCallback(widget, cb_type, cb, client_data)
     Widget widget;
     char *cb_type;
     XtCallbackProc cb;
     caddr_t client_data;
{
   BoxRec     *rec;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);
   if (!rec) return;

   if (!strcmp(cb_type, XmNdefaultActionCallback)) {
      rec->default_cb = cb;
      rec->default_cd = client_data;
   }
   if (!strcmp(cb_type, XmNsingleSelectionCallback)) {
      rec->single_cb = cb;
      rec->single_cd = client_data;
   }
   if (!strcmp(cb_type, XmNmultipleSelectionCallback)) {
      rec->multiple_cb = cb;
      rec->multiple_cd = client_data;
   }
   if (!strcmp(cb_type, XmNextendedSelectionCallback)) {
      rec->extended_cb = cb;
      rec->extended_cd = client_data;
   }
}

/***********************************************************************
 *                                                                     *
 *   Call callbacks.                                                   *
 *                                                                     *
 ***********************************************************************/
static void CallCallback(rec, event, cb_type)
       BoxRec *rec;
       XEvent *event;
       char *cb_type;
{
   IconBoxCallbackStruct cbs;
   XtCallbackProc        proc = NULL;
   caddr_t               cd = NULL;
   IconRec              *icon;
   int                   cnt, ipos;
   XmStringTable         tt;
   int                  *tp;
   char                **tc;

   if (!strcmp(cb_type, XmNdefaultActionCallback)) {
      proc = rec->default_cb;
      cd = rec->default_cd;
      cbs.reason = XmCR_DEFAULT_ACTION;
   }
   if (!strcmp(cb_type, XmNsingleSelectionCallback)) {
      proc = rec->single_cb;
      cd = rec->single_cd;
      cbs.reason = XmCR_SINGLE_SELECT;
   }
   if (!strcmp(cb_type, XmNmultipleSelectionCallback)) {
      proc = rec->multiple_cb;
      cd = rec->multiple_cd;
      cbs.reason = XmCR_MULTIPLE_SELECT;
   }
   if (!strcmp(cb_type, XmNextendedSelectionCallback)) {
      proc = rec->extended_cb;
      cd = rec->extended_cd;
      cbs.reason = XmCR_EXTENDED_SELECT;
   }

   if (proc) {
      if (!rec->no_icons)
         return;

      tt = (XmStringTable)XtMalloc(rec->no_icons * sizeof(XmString));
      tp = (int *)XtMalloc(rec->no_icons * sizeof(int));
      tc = (char **)XtMalloc(rec->no_icons * sizeof(char *));

      cbs.event                   = event;
      cbs.selected_items          = tt;
      cbs.selected_item_positions = tp;
      cbs.selected_item_class     = tc;
      cbs.selection_type          = 0;

      cnt  = 0;
      ipos = 0;
      icon = rec->icons;
      while (icon) {
         ipos++;
         if (icon->selected) {
            *tt++ = icon->label;
            *tp++ = ipos;
            *tc++ = icon->class;
            cnt++;
         }
         icon = icon->link;
      }
      if (!cnt)
         goto clean;

      cbs.selected_item_count = cnt;

      /* in case one item selected always do single select callback */
      if (cbs.reason == XmCR_MULTIPLE_SELECT && cnt == 1) {
         proc = rec->single_cb;
         cd = rec->single_cd;
         cbs.reason = XmCR_SINGLE_SELECT;
      }

      /* flush display before calling callback routine */
      XFlush(XtDisplay(rec->area));

      /* call the callback */
      (*proc)(rec->area, cd, &cbs);

clean:
      XtFree((char *)cbs.selected_items);
      XtFree((char *)cbs.selected_item_positions);
      XtFree((char *)cbs.selected_item_class);
   }
}

/***********************************************************************
 *                                                                     *
 *   Create an IconBox (user callable).                                *
 *                                                                     *
 ***********************************************************************/
Widget CreateIconBox(parent, name, add_args, num_add_args)
       Widget parent;
       char *name;
       Arg add_args[];
       Cardinal num_add_args;
{
   Widget          area;
   Arg             args[3];
   BoxRec         *rec;
   XmString        xms;

   /* space for rec is initialized to 0 */
   rec = (BoxRec *)XtCalloc (1, sizeof (BoxRec));

   area = XtCreateManagedWidget(name,
                                xmDrawingAreaWidgetClass,
                                parent,
                                add_args, num_add_args);

   XtGetSubresources(parent, rec, name, BOX_CLASS_NAME,
                     box_resources, XtNumber (box_resources),
                     add_args, num_add_args);

   xms = XmStringCreateLtoR("hg", XmSTRING_DEFAULT_CHARSET);
   rec->string_bheight = XmStringHeight(rec->fontlist, xms) + 2;
   XmStringFree(xms);

   /* set default size to the size of the parent */
   XtSetArg(args[0], XmNwidth, &rec->width);
   XtSetArg(args[1], XmNheight, &rec->height);
   XtGetValues(parent, args, 2);

   XtSetArg(args[0], XmNwidth, rec->width);
   XtSetArg(args[1], XmNheight, rec->height);
   XtSetArg(args[2], XmNuserData, rec);
   XtSetValues(area, args, 3);

   /* set minimum icon pixmap sizes */
   rec->icon_big_width    = rec->min_big_width;
   rec->icon_big_height   = rec->min_big_height;
   rec->icon_small_width  = rec->min_small_width;
   rec->icon_small_height = rec->min_small_height;

   /* set spacing between icon and label (should be setable in resource) */
   rec->ilsp  = 1;
   rec->silsp = 2;

   SetIconDimensions(rec);

   /* calculate the number of columns that fit in the window */
   rec->columns =
        (int) (rec->width) / (int) (rec->icon_width + rec->icon_hspacing);
   if (rec->columns <= 0) rec->columns = 1;

   /* set the virtual height to the actual height */
   rec->virt_height = rec->height;

   XtAddCallback(area, XmNexposeCallback, (XtCallbackProc)ExposeBox, rec);
   XtAddCallback(area, XmNinputCallback, (XtCallbackProc)HandleButtons, rec);
   XtAddEventHandler(area, ButtonMotionMask, False,
                     (XtEventHandler)HandleMove, rec);
   XtAddEventHandler(area, 0, True,
                     (XtEventHandler)HandleGraphicsExposures, rec);
   XtAddCallback(area, XmNresizeCallback, (XtCallbackProc)HandleResize, rec);
   XtAddCallback(area, XmNdestroyCallback, (XtCallbackProc)DestroyIconBox, rec);

   CreateScrollBar(parent, rec, area);

   rec->area = area;

   return area;
}

/******************** Handling expose through back pixmap *************/

/***********************************************************************
 *                                                                     *
 *   Redraw one icon to back pixmap.                                   *
 *                                                                     *
 ***********************************************************************/
static void RedrawIcon(widget, rec, icon)
       Widget widget;
       BoxRec *rec;
       IconRec *icon;
{
   int yoff;
   Pixel bgr;

   XtVaGetValues(widget, XmNbackground, &bgr, NULL);

   switch (rec->icon_type) {

      case IconBoxBIG_ICON:
         /* set clip mask in gc */
         XSetClipMask(XtDisplay(widget), rec->draw_gc, icon->mask);

         /* fill shadow */
         /***********  no icon shadows for the time being cosmetic
            ***********  problem when scrolling
            XSetClipOrigin (XtDisplay (widget), rec->draw_gc,
            icon->x + rec->shadow_off, icon->y + rec->shadow_off + rec->top);
            XSetForeground(XtDisplay(widget), rec->draw_gc, rec->shadow_color);
            XFillRectangle(XtDisplay(widget), rec->back, rec->draw_gc,
            icon->x + rec->shadow_off, icon->y + rec->shadow_off + rec->top,
            rec->icon_width, rec->icon_height);
            **************/

         /* draw icon */
         XSetClipOrigin(XtDisplay(widget), rec->draw_gc,
                        icon->x, icon->y + rec->top);
         XSetForeground(XtDisplay(widget), rec->draw_gc, icon->foreground);
         XSetBackground(XtDisplay(widget), rec->draw_gc, icon->background);
         if (icon->pix != XmUNSPECIFIED_PIXMAP)
            if (icon->selected)
               XCopyArea(XtDisplay(widget), icon->hi_pix, rec->back,
                         rec->draw_gc, 0, 0, rec->icon_width,
                         rec->icon_big_height,
                         icon->x + icon->x_off,
                         icon->y + icon->y_off + rec->top);
            else
               XCopyArea(XtDisplay(widget), icon->pix, rec->back,
                         rec->draw_gc, 0, 0, rec->icon_width,
                         rec->icon_big_height,
                         icon->x + icon->x_off,
                         icon->y + icon->y_off + rec->top);

         /* draw label */
         XSetClipMask(XtDisplay(widget), rec->draw_gc, None);
         if (icon->selected) {
            XSetForeground(XtDisplay(widget), rec->draw_gc, icon->label_bg);
            XSetBackground(XtDisplay(widget), rec->draw_gc, icon->label_fg);
            XmStringDrawImage(XtDisplay (widget), rec->back,
                       rec->fontlist, icon->label, rec->draw_gc,
                       icon->x - icon->string_left,
                       icon->y + rec->icon_height + rec->top + rec->ilsp,
                       rec->icon_width + 2 * icon->string_left,
                       XmALIGNMENT_CENTER, XmSTRING_DIRECTION_L_TO_R,
                       NULL);
         } else {
            XSetForeground(XtDisplay(widget), rec->draw_gc, icon->label_fg);
            XSetBackground(XtDisplay(widget), rec->draw_gc, icon->label_bg);
            if (icon->label_fg == bgr) {
                XmStringDrawImage(XtDisplay (widget), rec->back,
                      rec->fontlist, icon->label, rec->draw_gc,
                      icon->x - icon->string_left,
                      icon->y + rec->icon_height + rec->top + rec->ilsp,
                      rec->icon_width + 2 * icon->string_left,
                      XmALIGNMENT_CENTER, XmSTRING_DIRECTION_L_TO_R, NULL);
            } else {
                XmStringDraw(XtDisplay (widget), rec->back,
                      rec->fontlist, icon->label, rec->draw_gc,
                      icon->x - icon->string_left,
                      icon->y + rec->icon_height + rec->top + rec->ilsp,
                      rec->icon_width + 2 * icon->string_left,
                      XmALIGNMENT_CENTER, XmSTRING_DIRECTION_L_TO_R, NULL);
            }
         }
         break;

      case IconBoxSMALL_ICON:
      case IconBoxTITLES:
         /* set clip mask in gc */
         XSetClipMask(XtDisplay(widget), rec->draw_gc, icon->smask);

         /* draw small icon */
         XSetClipOrigin(XtDisplay(widget), rec->draw_gc,
                         icon->x, icon->y + rec->top);
         XSetForeground(XtDisplay(widget), rec->draw_gc, icon->foreground);
         XSetBackground(XtDisplay(widget), rec->draw_gc, icon->background);
         if (icon->spix != XmUNSPECIFIED_PIXMAP)
            if (icon->selected)
               XCopyArea(XtDisplay(widget), icon->hi_spix, rec->back,
                         rec->draw_gc, 0, 0, rec->icon_small_width,
                         rec->icon_height,
                         icon->x + icon->x_soff,
                         icon->y + icon->y_soff + rec->top);
            else
               XCopyArea(XtDisplay(widget), icon->spix, rec->back,
                         rec->draw_gc, 0, 0, rec->icon_small_width,
                         rec->icon_height,
                         icon->x + icon->x_soff,
                         icon->y + icon->y_soff + rec->top);

         /* draw label */
         XSetClipMask(XtDisplay(widget), rec->draw_gc, None);
         /* place label to the right and in the middle of the icon */
         yoff = rec->icon_small_height / 2 - rec->string_bheight / 2;
         if (icon->selected) {
            XSetForeground(XtDisplay(widget), rec->draw_gc, icon->label_bg);
            XSetBackground(XtDisplay(widget), rec->draw_gc, icon->label_fg);
            XmStringDrawImage(XtDisplay(widget), rec->back,
                              rec->fontlist, icon->label, rec->draw_gc,
                              icon->x + rec->icon_small_width + rec->silsp,
                              icon->y + rec->top + yoff,
                              rec->icon_width - rec->icon_small_width,
                              XmALIGNMENT_BEGINNING, XmSTRING_DIRECTION_L_TO_R,
                              NULL);
         } else {
            XSetForeground(XtDisplay(widget), rec->draw_gc, icon->label_fg);
            XSetBackground(XtDisplay(widget), rec->draw_gc, icon->label_bg);
            if (icon->label_fg == bgr) {
                XmStringDrawImage(XtDisplay(widget), rec->back,
                             rec->fontlist, icon->label, rec->draw_gc,
                             icon->x + rec->icon_small_width + rec->silsp,
                             icon->y + rec->top + yoff,
                             rec->icon_width - rec->icon_small_width,
                             XmALIGNMENT_BEGINNING, XmSTRING_DIRECTION_L_TO_R,
                             NULL);
            } else {
                XmStringDraw(XtDisplay(widget), rec->back,
                             rec->fontlist, icon->label, rec->draw_gc,
                             icon->x + rec->icon_small_width + rec->silsp,
                             icon->y + rec->top + yoff,
                             rec->icon_width - rec->icon_small_width,
                             XmALIGNMENT_BEGINNING, XmSTRING_DIRECTION_L_TO_R,
                             NULL);
            }
         }
         break;

      case IconBoxNO_ICON:
         /* draw label */
         XSetClipMask(XtDisplay(widget), rec->draw_gc, None);
         if (icon->selected) {
            XSetForeground(XtDisplay(widget), rec->draw_gc, icon->label_bg);
            XSetBackground(XtDisplay(widget), rec->draw_gc, icon->label_fg);
            XmStringDrawImage(XtDisplay(widget), rec->back,
                              rec->fontlist, icon->label, rec->draw_gc,
                              icon->x,
                              icon->y + rec->top,
                              rec->icon_width,
                              XmALIGNMENT_BEGINNING, XmSTRING_DIRECTION_L_TO_R,
                              NULL);
         } else {
            XSetForeground(XtDisplay(widget), rec->draw_gc, icon->label_fg);
            XSetBackground(XtDisplay(widget), rec->draw_gc, icon->label_bg);
            if (icon->label_fg == bgr) {
                XmStringDrawImage(XtDisplay(widget), rec->back,
                             rec->fontlist, icon->label, rec->draw_gc,
                             icon->x,
                             icon->y + rec->top,
                             rec->icon_width,
                             XmALIGNMENT_BEGINNING, XmSTRING_DIRECTION_L_TO_R,
                             NULL);
            } else {
                XmStringDraw(XtDisplay(widget), rec->back,
                             rec->fontlist, icon->label, rec->draw_gc,
                             icon->x,
                             icon->y + rec->top,
                             rec->icon_width,
                             XmALIGNMENT_BEGINNING, XmSTRING_DIRECTION_L_TO_R,
                             NULL);
            }
         }
   }
}

/***********************************************************************
 *                                                                     *
 *   Test whether two rectangles intersect.                            *
 *                                                                     *
 ***********************************************************************/
static Boolean Intersect(x1, y1, w1, h1, x2, y2, w2, h2)
       int x1, y1, x2, y2;
       Dimension w1, h1, w2, h2;
{
   if (x1 + (int)w1 < x2 || y1 + (int)h1 < y2) return False;
   if (x1 > x2 + (int)w2 || y1 > y2 + (int)h2) return False;
   return True;
}

/***********************************************************************
 *                                                                     *
 *   Create back pixmap on first expose.                               *
 *                                                                     *
 ***********************************************************************/
static void InitializeBoxWindow(widget, rec)
       Widget widget;
       BoxRec *rec;
{
   Arg         args[3];
   Cardinal    depth;
   Pixel       window_foreground;
   XGCValues   gc_values;
   IconRec    *icon;
   Boolean     got_intersection = False;

   /* get values from drawing area */
   XtSetArg(args[0], XmNdepth, &depth);
   XtSetArg(args[1], XmNbackground, &rec->background);
   XtSetArg(args[2], XmNforeground, &window_foreground);
   XtGetValues(widget, args, 3);

   /* delete backing pixmap if it already exists */
   if (rec->back) {
      XFreePixmap(XtDisplay(widget), rec->back);
      XFreeGC(XtDisplay(widget), rec->draw_gc);
      XtReleaseGC(widget, rec->normal_gc);
      XtReleaseGC(widget, rec->xor_gc);
   }

   /* create backing pixmap */
   rec->back = XCreatePixmap(XtDisplay(widget), XtWindow(widget),
                             rec->width, rec->height, depth);
   rec->exposed = True;

   /* create writable GC used to draw into backing pixmap */
   gc_values.foreground = window_foreground;
   gc_values.background = rec->background;
   gc_values.graphics_exposures = False;
   gc_values.font = XLoadFont(XtDisplay(widget), "fixed");
   rec->draw_gc = XCreateGC(XtDisplay(widget), XtWindow(widget),
                            GCForeground | GCBackground |
                            GCFont       | GCGraphicsExposures,
                            &gc_values);

   /* get shareable GC used to draw to the screen */
   gc_values.graphics_exposures = True;
   rec->normal_gc = XtGetGC(widget,
                            GCForeground | GCBackground |
                            GCFont       | GCGraphicsExposures,
                            &gc_values);

   /* get shareable GC used to draw the rubberband box */
   /*
    * Set the fg to the XOR of the fg and bg, so if it is XOR'ed
    * with the bg, the result will be fg and vice-versa.
    * This effectively achieves inverse video for the box.
    */
   gc_values.foreground = window_foreground ^ rec->background;
   gc_values.background = rec->background;
   gc_values.line_style = LineSolid;
   gc_values.function   = GXxor;
   rec->xor_gc = XtGetGC(widget,
                         GCForeground | GCBackground |
                         GCFunction | GCLineStyle,
                         &gc_values);

   /*
    * Establish a passive grab, for any button press.
    * Force the sprite to stay within the area window.
    */
   XGrabButton(XtDisplay(widget), AnyButton, AnyModifier,
               XtWindow(widget), True,
               ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
               GrabModeAsync, GrabModeAsync, XtWindow(widget),
               XCreateFontCursor(XtDisplay(widget), XC_left_ptr));

   /* initialize pixmap contents */
   XSetForeground(XtDisplay (widget), rec->draw_gc, rec->background);
   XFillRectangle(XtDisplay (widget), rec->back, rec->draw_gc,
                  0, 0, rec->width, rec->height);

   icon = rec->icons;
   while (icon) {
      if (Intersect(0, 0, rec->width, rec->height,
                    icon->x - icon->string_left,
                    icon->y + rec->top,
                    rec->icon_width + 2 * icon->string_left,
                    rec->icon_height + rec->string_height)) {
         RedrawIcon(widget, rec, icon);
         got_intersection = True;
      } else if (got_intersection)
         break;
      icon = icon->link;
   }
}

/***********************************************************************
 *                                                                     *
 *   Redraw partial region of back pixmap and refresh window.          *
 *                                                                     *
 ***********************************************************************/
static C_DECL_6(void RedrawRegion,
       Widget, widget,
       BoxRec *, rec,
       int, x,
       int, y,
       Dimension, width,
       Dimension, height)
{
   IconRec     *icon;
   Boolean      got_intersection = False;

   /* erase invalid rectangle in back pixmap */
   XSetForeground(XtDisplay(widget), rec->draw_gc, rec->background);
   XSetClipMask(XtDisplay(widget), rec->draw_gc, None);
   XFillRectangle(XtDisplay(widget), rec->back, rec->draw_gc,
                  x, y, width, height);

   /* redraw affected icons to back pixmap */
   icon = rec->icons;
   while (icon) {
      if (Intersect(x, y, width, height,
                    icon->x - icon->string_left,
                    icon->y + rec->top,
                    rec->icon_width + 2 * icon->string_left,
                    rec->icon_height + rec->string_height)) {
         RedrawIcon(widget, rec, icon);
         got_intersection = True;
      } else if (got_intersection)
         break;
      icon = icon->link;
   }

   /* refresh window of drawing area */
   XCopyArea(XtDisplay(widget), rec->back, XtWindow(widget),
             rec->normal_gc, x, y, width, height, x, y);
}

/***********************************************************************
 *                                                                     *
 *   Simple expose procedure.                                          *
 *                                                                     *
 ***********************************************************************/
static void ExposeBox(widget, rec, call_data)
       Widget widget;
       BoxRec *rec;
       XmDrawingAreaCallbackStruct *call_data;
{
   XEvent *ev = call_data->event;

   /*
    * If widget has not yet been displayed then window is not set
    * and in that case InitializeBoxWindow will not work because
    * it cannot create the backing pixmap.
    */
   if (!XtWindow(widget)) return;

   if (ev == NULL && !rec->redraw_all) return;

   /*
    * After scrolling the backing pixmap is not up-to-date,
    * RedrawRegion will update it
    */
   if (rec->scrolled && rec->exposed) {

      RedrawRegion(widget, rec, 0, 0, rec->width, rec->height);

   } else {

      /* create back pixmap if first time */
      if (!rec->exposed)
         InitializeBoxWindow(widget, rec);

      /* copy backing pixmap to screen */
      if (rec->redraw_all) {
         XCopyArea(XtDisplay(widget), rec->back, XtWindow(widget),
                   rec->normal_gc, 0, 0, rec->width, rec->height, 0, 0);
         rec->redraw_all = False;
      } else
         XCopyArea(XtDisplay(widget), rec->back, XtWindow(widget),
                   rec->normal_gc,
                   ev->xexpose.x, ev->xexpose.y,
                   ev->xexpose.width, ev->xexpose.height,
                   ev->xexpose.x, ev->xexpose.y);
   }

   rec->scrolled = False;
}

/************************ Handle button-down **************************/

/***********************************************************************
 *                                                                     *
 *   Test whether point in icon or label part.                         *
 *                                                                     *
 ***********************************************************************/
Boolean PointInIcon(x, y, icon, ww, hh, sh, top)
        int x, y;
        IconRec *icon;
        Dimension ww, hh;
        int sh, top;
{
   int w, h;

   w = ww; h = hh;

   /* is in icon part ? */
   if (x >= icon->x && x <= icon->x + w &&
       y >= icon->y + top && y <= icon->y + top + h) {
      return True;
   }
   /* is in label part (only for big icons since sh=0 for small icons)? */
   if (x >= icon->x - icon->string_left &&
       x <= icon->x + w + icon->string_left &&
       y >= icon->y + top + h &&
       y <= icon->y + top + h + sh) {
      return True;
   }
   /* else false */
   return False;
}

/***********************************************************************
 *                                                                     *
 *   Which icon was hit?                                               *
 *                                                                     *
 ***********************************************************************/
static int Hit(rec, x, y)
       BoxRec *rec;
       int x, y;
{
   int        found = 0;
   IconRec   *icon;

   icon = rec->icons;
   while (icon) {
      if (PointInIcon (x, y, icon,
                       rec->icon_width, rec->icon_height,
                       rec->string_height, rec->top)) {
         found = icon->id;
         break;
      }
      icon = icon->link;
   }
   return found;
}

/***********************************************************************
 *                                                                     *
 *   Find IconRec belonging to id.                                     *
 *                                                                     *
 ***********************************************************************/
static IconRec *FindIcon(rec, id)
       BoxRec *rec;
       int id;
{
   IconRec   *icon;

   icon = rec->icons;
   while (icon) {
      if (icon->id == id) break;
      icon = icon->link;
   }
   return icon;
}

/***********************************************************************
 *                                                                     *
 *   Select or deselect one or all icons.                              *
 *                                                                     *
 ***********************************************************************/
static C_DECL_3(void SelectIcons,
       BoxRec *, rec,
       IconRec *, icon,
       Boolean, select)
{
   if (icon) {
      icon->selected = select;
   } else {
      icon = rec->icons;
      while (icon) {
         icon->selected = select;
         icon = icon->link;
      }
   }
}

/***********************************************************************
 *                                                                     *
 *   Select all items (user callable).                                 *
 *                                                                     *
 ***********************************************************************/
C_DECL_3(void IconBoxSelectAllItems,
     Widget, widget,
     Boolean, refresh,
     Boolean, notify)
{
   BoxRec     *rec;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);
   if (!rec || !rec->icons) return;

   SelectIcons(rec, NULL, True);

   if (refresh && rec->exposed)
      RedrawRegion(widget, rec, 0, 0, rec->width, rec->height);

   if (notify)
      CallCallback(rec, NULL, XmNmultipleSelectionCallback);
}

/***********************************************************************
 *                                                                     *
 *   Deselect all items (user callable).                               *
 *                                                                     *
 ***********************************************************************/
C_DECL_2(void IconBoxDeselectAllItems,
     Widget, widget,
     Boolean, refresh)
{
   BoxRec     *rec;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);
   if (!rec || !rec->icons) return;

   SelectIcons(rec, NULL, False);

   if (refresh && rec->exposed)
      RedrawRegion(widget, rec, 0, 0, rec->width, rec->height);
}

/***********************************************************************
 *                                                                     *
 *   Change the characters in str into lower case.                     *
 *                                                                     *
 ***********************************************************************/
static void lower(str)
       char *str;
{
   char *sav = str;

   while (*sav) {
      *sav = tolower(*sav);
      sav++;
   }
}

/***********************************************************************
 *                                                                     *
 *   This function returns a pointer to an array of XmString's         *
 *   containing the icon labels and the number of icons.               *
 *   When nitems<>0 the XmString array should be freed by the user     *
 *   after usage (user callable).                                      *
 *                                                                     *
 ***********************************************************************/
void IconBoxGetItems(widget, items, nitems)
     Widget widget;
     XmString **items;
     int *nitems;
{
   BoxRec     *rec;
   IconRec    *icon;
   XmString   *tmp;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);

   if (!rec) return;

   *nitems = rec->no_icons;
   if (*nitems == 0) {
      *items = (XmString *) NULL;
      return;
   }

   *items = (XmString *)XtMalloc(rec->no_icons * sizeof(XmString));
   tmp    = *items;

   icon = rec->icons;
   while (icon) {
      *tmp++ = icon->label;
      icon = icon->link;
   }
}

/***********************************************************************
 *                                                                     *
 *   This function returns a pointer to an array of XmString's         *
 *   containing the icon labels and the number of selected icons.      *
 *   When nitems<>0 the XmString array should be freed by the user     *
 *   after usage (user callable).                                      *
 *                                                                     *
 ***********************************************************************/
void IconBoxGetSelectedItems(widget, items, nitems)
     Widget widget;
     XmString **items;
     int *nitems;
{
   BoxRec     *rec;
   IconRec    *icon;
   XmString   *tmp;
   int         cnt;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);

   if (!rec) return;

   *nitems = rec->no_icons;
   if (*nitems == 0) {
      *items = (XmString *) NULL;
      return;
   }

   *items = (XmString *)XtMalloc(rec->no_icons * sizeof(XmString));
   tmp    = *items;

   cnt  = 0;
   icon = rec->icons;
   while (icon) {
      if (icon->selected) {
         *tmp++ = icon->label;
         cnt++;
      }
      icon = icon->link;
   }
   *nitems = cnt;

   /* get rid of reserved space when no selected icons were found */
   if (cnt == 0) {
      XtFree((char *)*items);
      *items = (XmString *) NULL;
   }
}

/***********************************************************************
 *                                                                     *
 *   This function returns a XmString containing the last selected     *
 *   item. The XmString should be freed by the user after usage        *
 *   (user callable).                                                  *
 *                                                                     *
 ***********************************************************************/
int IconBoxGetLastSelectedItem(widget, item)
    Widget widget;
    XmString *item;
{
   BoxRec     *rec;
   IconRec    *icon;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);

   if (!rec || !rec->icons || !rec->hit) goto end;

   icon = rec->icons;
   while (icon) {
      if (rec->hit == icon->id) {
         *item = XmStringCopy(icon->label);
         return 1;
      }
      icon = icon->link;
   }

end:
   *item = XmStringCreate(" ", XmSTRING_DEFAULT_CHARSET);
   return 0;
}

/***********************************************************************
 *                                                                     *
 *   This function returns some of the attributes of item, like its    *
 *   x, y, width, height, icon class and selected attributes.          *
 *   The function allocates space for the class string, the caller has *
 *   to free this space using XtFree to prevent a memory leak.         *
 *   When the item does not exist the function returns 0               *
 *   (user callable).                                                  *
 *                                                                     *
 ***********************************************************************/
C_DECL_8(int IconBoxGetItemAttr,
    Widget, widget,
    XmString, item,
    int *, x,
    int *, y,
    int *, width,
    int *, height,
    char **, class,
    Boolean *, selected)
{
   BoxRec     *rec;
   IconRec    *icon;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);

   *class = XtCalloc(1, 1);

   if (!rec || !rec->icons) return 0;
   if (rec->no_icons == 0)  return 0;

   UpdatePositions(rec);

   icon = rec->icons;
   while (icon) {
      if (XmStringCompare(item, icon->label)) {
         switch (rec->icon_type) {
            case IconBoxBIG_ICON:
               *x        = (int) (icon->x + icon->x_off);
               *y        = (int) (icon->y + icon->y_off + rec->top);
               *width    = (int) icon->pix_w;
               *height   = (int) icon->pix_h;
               break;
            case IconBoxSMALL_ICON:
            case IconBoxTITLES:
               *x        = (int) (icon->x + icon->x_soff);
               *y        = (int) (icon->y + icon->y_soff + rec->top);
               *width    = (int) icon->spix_w;
               *height   = (int) icon->spix_h;
               break;
            case IconBoxNO_ICON:
               *x        = (int) 0;
               *y        = (int) 0;
               *width    = (int) 0;
               *height   = (int) 0;
               break;
         }
         XtFree(*class);
         *class = XtCalloc(strlen(icon->class)+1, 1);
         strcpy(*class, icon->class);
         *selected = icon->selected;
         return 1;
      }
      icon = icon->link;
   }
   return 0;
}

/***********************************************************************
 *                                                                     *
 *   This function selects the icons of which the names are included   *
 *   in the array of XmString's (user callable).                       *
 *                                                                     *
 ***********************************************************************/
C_DECL_4(void IconBoxSelectItems,
     Widget, widget,
     XmString *, items,
     int, nitems,
     Boolean, notify)
{
   BoxRec     *rec;
   IconRec    *icon;
   XmString   *tmp;
   int         i;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);

   if (!rec || !rec->icons) return;

   if (items == NULL) return;
   if (nitems == 0)   return;

   tmp = items;

   for (i = 0; i < nitems; i++) {
      icon = rec->icons;
      while (icon) {
         if (XmStringCompare(*tmp, icon->label)) {
            icon->selected = True;
            break;
         }
         icon = icon->link;
      }
      tmp++;
   }

   if (rec->exposed)
      RedrawRegion(widget, rec, 0, 0, rec->width, rec->height);

   if (notify)
      CallCallback(rec, NULL, XmNmultipleSelectionCallback);
}

/***********************************************************************
 *                                                                     *
 *   This function selects the icons of which the names (minus the     *
 *   cycle number) are included in the array of XmString's. For CMZ    *
 *   only (user callable).                                             *
 *                                                                     *
 ***********************************************************************/
C_DECL_5(void IconBoxSelectDecks,
     Widget, widget,
     XmString *, decks,
     int, ndecks,
     Boolean, refresh,
     Boolean, notify)
{
   BoxRec     *rec;
   IconRec    *icon;
   XmString   *tmp;
   int         i;
   char       *deck1,  *deck2, *s;
   char       *label1, *label2;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);

   if (!rec || !rec->icons) return;

   if (decks == NULL) return;
   if (ndecks == 0)   return;

   tmp = decks;

   for (i = 0; i < ndecks; i++) {
      XmStringGetLtoR(*tmp, XmSTRING_DEFAULT_CHARSET, &deck1);
      deck2 = XtCalloc(strlen(deck1)+1, 1);
      s = strchr(deck1,';');
      if (s)
         strncpy(deck2, deck1, (size_t) (s - deck1));
      else
         strcpy(deck2, deck1);
      lower(deck2);

      icon = rec->icons;
      while (icon) {
         XmStringGetLtoR(icon->label, XmSTRING_DEFAULT_CHARSET, &label1);
         label2 = XtCalloc(strlen(label1)+1, 1);
         strncpy(label2, label1, (size_t) (strchr(label1,';') - label1));
         lower(label2);
         if (!strcmp(label2, deck2)) {
            icon->selected = True;
            XtFree(label1); XtFree(label2);
            break;
         }
         icon = icon->link;
         XtFree(label1); XtFree(label2);
      }
      tmp++;
      XtFree(deck1); XtFree(deck2);
   }

   if (refresh && rec->exposed)
      RedrawRegion(widget, rec, 0, 0, rec->width, rec->height);

   if (notify)
      CallCallback(rec, NULL, XmNmultipleSelectionCallback);
}

/***********************************************************************
 *                                                                     *
 *   This function returns a pointer to an array of integer's          *
 *   containing the position and the number of selected icons.         *
 *   When npos<>0 the int array should be freed by the user            *
 *   after usage (user callable).                                      *
 *                                                                     *
 ***********************************************************************/
void IconBoxGetSelectedPos(widget, pos, npos)
     Widget widget;
     int **pos, *npos;
{
   BoxRec     *rec;
   IconRec    *icon;
   int        *tmp;
   int         cnt, ipos;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);

   if (rec == NULL) return;

   *npos = rec->no_icons;
   if (*npos == 0) {
      *pos = (int *) NULL;
      return;
   }

   *pos = (int *)XtMalloc(rec->no_icons * sizeof(int));
   tmp  = *pos;

   cnt  = 0;
   ipos = 0;
   icon = rec->icons;
   while (icon) {
      ipos++;
      if (icon->selected) {
         *tmp++ = ipos;
         cnt++;
      }
      icon = icon->link;
   }
   *npos = cnt;

   /* get rid of reserved space when no selected icons were found */
   if (cnt == 0) {
      XtFree((char *)*pos);
      *pos = (int *) NULL;
   }
}

/***********************************************************************
 *                                                                     *
 *   This function selects the icons of which the positions are        *
 *   included in the array of integers's (user callable).              *
 *                                                                     *
 ***********************************************************************/
C_DECL_4(void IconBoxSelectPos,
     Widget, widget,
     int *, pos,
     int, npos,
     Boolean, notify)
{
   BoxRec     *rec;
   IconRec    *icon;
   int        *tmp;
   int         i, ipos;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);

   if (!rec || !rec->icons) return;

   if (pos == NULL) return;
   if (npos == 0)   return;

   tmp = pos;

   for (i = 0; i < npos; i++) {
      ipos = 0;
      icon = rec->icons;
      while (icon) {
         ipos++;
         if (*tmp == ipos) {
            icon->selected = True;
            break;
         }
         icon = icon->link;
      }
      tmp++;
   }

   if (rec->exposed)
      RedrawRegion(widget, rec, 0, 0, rec->width, rec->height);

   if (notify)
      CallCallback(rec, NULL, XmNmultipleSelectionCallback);
}

/***********************************************************************
 *                                                                     *
 *   This function checks if a specified item is in the IconBox (user  *
 *   callable).                                                        *
 *                                                                     *
 ***********************************************************************/
Boolean IconBoxItemExists(widget, item)
        Widget widget;
        XmString item;
{
   Arg         args[1];
   BoxRec     *rec;
   IconRec    *icon;

   XtSetArg(args[0], XmNuserData, &rec);
   XtGetValues(widget, args, 1);

   if (!rec || !rec->icons) return False;

   icon = rec->icons;
   while (icon) {
      if (XmStringCompare(icon->label, item)) return True;
      icon = icon->link;
   }
   return False;
}

/***********************************************************************
 *                                                                     *
 *   This function checks if a specified deck is in the IconBox. For   *
 *   CMZ only (user callable).                                         *
 *                                                                     *
 ***********************************************************************/
Boolean IconBoxDeckExists(widget, deck)
        Widget widget;
        XmString deck;
{
   Arg         args[1];
   BoxRec     *rec;
   IconRec    *icon;
   char       *deck1, *deck2, *s;
   char       *label1, *label2;

   XtSetArg(args[0], XmNuserData, &rec);
   XtGetValues(widget, args, 1);

   if (!rec || !rec->icons) return False;

   XmStringGetLtoR(deck, XmSTRING_DEFAULT_CHARSET, &deck1);
   deck2 = XtCalloc(strlen(deck1)+1, 1);
   s = strchr(deck1,';');
   if (s)
      strncpy(deck2, deck1, (size_t) (s - deck1));
   else
      strcpy(deck2, deck1);
   lower(deck2);

   icon = rec->icons;
   while (icon) {
      XmStringGetLtoR(icon->label, XmSTRING_DEFAULT_CHARSET, &label1);
      label2 = XtCalloc(strlen(label1)+1, 1);
      strncpy(label2, label1, (size_t) (strchr(label1,';') - label1));
      lower(label2);
      if (!strcmp(label2, deck2)) {
         XtFree(deck1); XtFree(deck2); XtFree(label1); XtFree(label2);
         return True;
      }
      icon = icon->link;
      XtFree(label1); XtFree(label2);
   }
   XtFree(deck1); XtFree(deck2);

   return False;
}

/***********************************************************************
 *                                                                     *
 *  ShowIcons redraws one or all icons.                                *
 *                                                                     *
 ***********************************************************************/
static void ShowIcons(widget, rec, icon)
       Widget widget;
       BoxRec *rec;
       IconRec *icon;
{
   if (icon)
      RedrawRegion (widget, rec,
                    icon->x - icon->string_left,
                    icon->y + rec->top,
                    rec->icon_width + 2 * icon->string_left,
                    rec->icon_height + rec->string_height);
   else
      RedrawRegion (widget, rec, 0, 0, rec->width, rec->height);
}

/***********************************************************************
 *                                                                     *
 *   Update the rubberband box in the rec structure.                   *
 *                                                                     *
 ***********************************************************************/
static void SetRectangle(x, y, rec)
       int x, y;
       BoxRec *rec;
{
   rec->x_rub = min(rec->x_rubs, x);
   rec->y_rub = min(rec->y_rubs, y);
   rec->w_rub = abs(rec->x_rubs - x);
   rec->h_rub = abs(rec->y_rubs - y);
}

/***********************************************************************
 *                                                                     *
 *   Check if icon is in rubberband box.                               *
 *                                                                     *
 ***********************************************************************/
static Boolean IconInRectangle(rec, icon)
       BoxRec *rec;
       IconRec *icon;
{
   if (Intersect(icon->x - icon->string_left,
                 icon->y + rec->top,
                 rec->icon_width + 2 * icon->string_left,
                 rec->icon_height + rec->string_height,
                 rec->x_rub, rec->y_rub, rec->w_rub, rec->h_rub))
      return True;
   else
      return False;
}

/***********************************************************************
 *                                                                     *
 *   Check if doubleclick happened.                                    *
 *                                                                     *
 ***********************************************************************/
static Boolean DoubleClick(ev, rec)
       XEvent *ev;
       BoxRec *rec;
{
   static Time  click1 = 0;
   Boolean      rtn;

   rtn = False;

   /* in case of time wrap no double click */
   if (ev->xbutton.time < click1) return(rtn);

   if (ev->xbutton.time - click1 <= rec->dc_interval)
      rtn = True;
   click1 = ev->xbutton.time;

   return(rtn);
}

/***********************************************************************
 *                                                                     *
 *   Handle scrollbar changes.                                         *
 *                                                                     *
 ***********************************************************************/
static void ScrollBarMoved(widget, rec, call_data)
       Widget widget;
       BoxRec *rec;
       XmScrollBarCallbackStruct *call_data;
{
   int    d, da;
   int    old_top = rec->top;

   rec->top = -call_data->value;

   if (!rec->exposed) return;

   switch (call_data->reason) {
      case XmCR_DRAG:
      case XmCR_VALUE_CHANGED:
         /*
          * first copy part of area that is still visible to new place,
          * next redraw only the (small) new region
          */
         d  = rec->top - old_top;
         da = abs(d);
         if (da < (int) rec->height) {
            if (d > 0) {
               XCopyArea(XtDisplay(rec->area), XtWindow(rec->area),
                         XtWindow(rec->area), rec->normal_gc,
                         0, 0, rec->width, rec->height-da, 0, da);
               RedrawRegion(rec->area, rec, 0, 0, rec->width, da);
            } else if (d < 0) {
               XCopyArea(XtDisplay(rec->area), XtWindow(rec->area),
                         XtWindow(rec->area), rec->normal_gc,
                         0, da, rec->width, rec->height-da, 0, 0);
               RedrawRegion(rec->area, rec, 0, rec->height-da, rec->width, da);
            }
            rec->scrolled = True;
         } else
            RedrawRegion(rec->area, rec, 0, 0, rec->width, rec->height);
   }
}

/***********************************************************************
 *                                                                     *
 *   Handle input callback.                                            *
 *                                                                     *
 ***********************************************************************/
static void HandleButtons(widget, rec, call_data)
       Widget widget;
       BoxRec *rec;
       XmDrawingAreaCallbackStruct *call_data;
{
   XEvent       *ev = call_data->event;
   IconRec      *icon;
   static unsigned int state;
   static Boolean      db_click = False;

   if (ev->xany.type == ButtonPress) {
      state = ev->xbutton.state;
      /*
       *   double click events
       */
      if (DoubleClick(ev, rec)) {
         if (ev->xbutton.button == 1) {
            db_click = True;
         } else if (ev->xbutton.button == 2) {

         } else if (ev->xbutton.button == 3) {

         }
      /*
       *   mouse button 1 is used to select icons
       */
      } else if (ev->xbutton.button == 1) {
         /*
          * first un-highlight all selected icons except when
          * the control button has been pressed
          */
         if (ev->xbutton.state != ControlMask) {
            SelectIcons(rec, NULL, False);
            ShowIcons(widget, rec, NULL);
         }

         rec->hit = Hit(rec, ev->xbutton.x, ev->xbutton.y);

         /* start drawing rubberband box */
         rec->rubber = True;
         rec->x_rubs = rec->x_rub  = ev->xbutton.x;
         rec->y_rubs = rec->y_rub  = ev->xbutton.y;
         rec->w_rub  = 0;
         rec->h_rub  = 0;
         XDrawRectangle(XtDisplay(widget), XtWindow(widget),
                        rec->xor_gc, rec->x_rub, rec->y_rub,
                        rec->w_rub, rec->h_rub);
         /*
          *   mouse button 2 is used to move icons
          */
      } else if (ev->xbutton.button == 2) {

         /*
          *   mouse button 3 is not yet used
          */
      } else if (ev->xbutton.button == 3) {

      }

      /*********************************************************
       * we don't allow any moving for the time being
       *
      if (!rec->hit) return;
      icon = FindIcon(rec, rec->hit);
      rec->x_off = ev->xbutton.x - icon->x;
      rec->y_off = ev->xbutton.y - icon->y;
      *********************************************************/
   } else if (ev->xany.type == ButtonRelease) {
      if (db_click) {
         CallCallback(rec, ev, XmNdefaultActionCallback);
         db_click = False;
      } else if (ev->xbutton.button == 1) {
         if (rec->rubber) {
            /*
            * erase the current rubberband box and update the box info
            */
            XDrawRectangle(XtDisplay(widget), XtWindow(widget),
                           rec->xor_gc, rec->x_rub, rec->y_rub,
                           rec->w_rub, rec->h_rub);
            SetRectangle(ev->xbutton.x, ev->xbutton.y, rec);
            rec->rubber = False;
         }
         if (rec->w_rub == 0 && rec->h_rub == 0) {
            /* highlight hit icon */
            icon = FindIcon(rec, rec->hit);
            if (icon) {
               if (state != ControlMask) {
                  SelectIcons(rec, icon, True);
                  ShowIcons(widget, rec, icon);
                  CallCallback(rec, ev, XmNsingleSelectionCallback);
               } else {
                  if (icon->selected)
                      SelectIcons(rec, icon, False);
                  else
                      SelectIcons(rec, icon, True);
                  ShowIcons(widget, rec, icon);
                  CallCallback(rec, ev, XmNextendedSelectionCallback);
               }
            }
         } else {
            /* find and highlight icons inside rubberband box */
            icon = rec->icons;
            while (icon) {
               if (IconInRectangle(rec, icon)) {
                  if (state != ControlMask) {
                     SelectIcons(rec, icon, True);
                  } else {
                     if (icon->selected)
                         SelectIcons(rec, icon, False);
                     else
                         SelectIcons(rec, icon, True);
                  }
                  ShowIcons(widget, rec, icon);
               }
               icon = icon->link;
            }
            if (state != ControlMask)
               CallCallback(rec, ev, XmNmultipleSelectionCallback);
            else
               CallCallback(rec, ev, XmNextendedSelectionCallback);
         }
      }
   }
}

/***********************************************************************
 *                                                                     *
 *   Reposition icons in function of the new width of the box.         *
 *                                                                     *
 ***********************************************************************/
static void RepositionIcons(rec)
       BoxRec *rec;
{
   IconRec     *icon;
   int          pos;

   /* calculate the number of columns */
   rec->columns =
        (int) (rec->width) / (int) (rec->icon_width + rec->icon_hspacing);
   if (rec->columns <= 0) rec->columns = 1;

   icon = rec->icons;
   pos  = 0;
   while (icon) {
      icon->x = (pos % rec->columns) *
         (rec->icon_width + rec->icon_hspacing) +
         rec->icon_hspacing/2;
      icon->y = (pos / rec->columns) *
         (rec->icon_height + rec->string_height + rec->icon_vspacing) +
         rec->icon_vspacing/2;
      icon = icon->link;
      pos++;
   }
}

/***********************************************************************
 *                                                                     *
 *   Handle resize callback.                                           *
 *                                                                     *
 ***********************************************************************/
static void HandleResize (widget, rec, call_data)
            Widget widget;
            BoxRec *rec;
            XmDrawingAreaCallbackStruct *call_data;
{
   Arg                   args[2];
   Dimension             oldw;
   int                   value, ovalue;

   oldw = rec->width;
   rec->exposed = False;

   XtSetArg (args[0], XmNwidth, &rec->width);
   XtSetArg (args[1], XmNheight, &rec->height);
   XtGetValues (widget, args, 2);

   XtSetArg(args[0], XmNvalue, &ovalue);
   XtGetValues(rec->scrollbar, args, 1);

   /* only when width changes reposition the icons */
   if (rec->width != oldw) {
      RepositionIcons(rec);
      rec->virt_height = GetVirtHeight(rec);
   }

   SetScrollBar(widget, rec);

   XtSetArg(args[0], XmNvalue, &value);
   XtGetValues(rec->scrollbar, args, 1);

   if (ovalue != value)
      RedrawRegion(widget, rec, 0, 0, rec->width, rec->height);
   else {
      /*
       * explicitely call the expose callback because sometimes when
       * shrinking the window it is not called automatically
       */
      SendExpose(widget);
   }
}

/***********************************************************************
 *                                                                     *
 *   Send an expose event to a widget.                                 *
 *                                                                     *
 ***********************************************************************/
static void SendExpose(widget)
       Widget widget;
{
   Arg         args[1];
   BoxRec     *rec;
   XmDrawingAreaCallbackStruct  da_struct;

   XtSetArg (args[0], XmNuserData, &rec);
   XtGetValues (widget, args, 1);

   rec->redraw_all = True;

   /* explicitely call the expose callback */
   da_struct.reason = XmCR_EXPOSE;
   da_struct.event  = (XEvent *) NULL;
   da_struct.window = XtWindow(widget);
   XtCallCallbacks(widget, XmNexposeCallback, &da_struct);
}

/***********************************************************************
 *                                                                     *
 *   Wait for ms milliseconds. Used by IconBoxZoom.                    *
 *                                                                     *
 ***********************************************************************/
static void delay(ms)
       int ms;
{
#if defined(UNIX) && !defined(APOLLO)
   struct timeval t_start, t_end;
   unsigned int  elapsed;

   gettimeofday(&t_start, NULL);

   do {
      gettimeofday(&t_end, NULL);
      elapsed = (t_end.tv_sec * 1000000 + t_end.tv_usec) -
                (t_start.tv_sec * 1000000 + t_start.tv_usec);
   } while (elapsed <= ms*1000);
#endif
}


/***********************************************************************
 *                                                                     *
 *   Make a zoom effect starting from or going to rect (x, y, w, h).   *
 *   The zoom is always to or from the full IconBox area               *
 *   (user callable).                                                  *
 *                                                                     *
 ***********************************************************************/
C_DECL_6(void IconBoxZoom,
     Widget, widget,
     int, x,
     int, y,
     int, width,
     int, height,
     Boolean, expand)
{
   BoxRec     *rec;
   int         i, unit;
   struct { int left, top, right, bottom; } r[17], sr, dr;

   XtVaGetValues(widget, XmNuserData, &rec, NULL);

   if (!rec) return;
   if (!rec->zoom_effect) return;
   if (!rec->exposed) return;

   if (expand) {
      sr.left   = x;
      sr.top    = y;
      sr.right  = x + width;
      sr.bottom = y + height;

      dr.left   = 0;
      dr.top    = 0;
      dr.right  = rec->width;
      dr.bottom = rec->height;
   } else {
      sr.left   = 0;
      sr.top    = 0;
      sr.right  = rec->width;
      sr.bottom = rec->height;

      dr.left   = x;
      dr.top    = y;
      dr.right  = x + width;
      dr.bottom = y + height;
   }

   for (i = 0; i <= 22; i++) {
      if (i <= 16) {
         r[i] = sr;
         unit = (expand?i+1:33-i)*i/2;

         r[i].top    += (unit*(dr.top-sr.top))/136;
         r[i].left   += (unit*(dr.left-sr.left))/136;
         r[i].bottom += (unit*(dr.bottom-sr.bottom))/136;
         r[i].right  += (unit*(dr.right-sr.right))/136;

         XDrawRectangle(XtDisplay(widget), XtWindow(widget),
                        rec->xor_gc, r[i].left, r[i].top,
                        (Dimension)(r[i].right-r[i].left),
                        (Dimension)(r[i].bottom-r[i].top));

         XSync(XtDisplay(widget), False);
      }
      if (i >= 6) {
         XDrawRectangle(XtDisplay(widget), XtWindow(widget),
                        rec->xor_gc, r[i-6].left, r[i-6].top,
                        (Dimension)(r[i-6].right-r[i-6].left),
                        (Dimension)(r[i-6].bottom-r[i-6].top));

         XSync(XtDisplay(widget), False);
      }

      /* slow down factor */
      delay(rec->zoom_speed);
   }
}

/************************** Handle movement ****************************/

/***********************************************************************
 *                                                                     *
 *   Find rectangle invalidated by move.                               *
 *                                                                     *
 ***********************************************************************/
#ifdef UNREF
static void FindInvalidRect(rec, new_x, new_y, x, y, width, height)
       BoxRec *rec;
       int new_x, new_y;
       int *x, *y;
       Dimension *width, *height;
{
   int            xmove, ymove;
   IconRec       *icon = FindIcon(rec, rec->hit);

   /* check movement in X direction */
   xmove = new_x - icon->x;
   if (xmove > 0) {
      *x = icon->x - icon->string_left;
      *width = rec->icon_width + xmove + 2 * icon->string_left;
   } else {
      *x = icon->x + xmove - icon->string_left;
      *width = rec->icon_width - xmove + 2 * icon->string_left;
   }

   /* check movement in Y direction */
   ymove = new_y - icon->y;
   if (ymove > 0) {
      *y = icon->y;
      *height = rec->icon_height + rec->string_height + ymove;
   } else {
      *y = icon->y + ymove;
      *height = rec->icon_height + rec->string_height - ymove;
   }
}
#endif /* UNREF */

/***********************************************************************
 *                                                                     *
 *   Handle motion events.                                             *
 *                                                                     *
 ***********************************************************************/
static void HandleMove (widget, rec, ev)
       Widget widget;
       BoxRec *rec;
       XEvent *ev;
{
   if (rec->rubber) {
      /* draw once to clear the previous rubberband box */
      XDrawRectangle(XtDisplay(widget), XtWindow(widget),
                     rec->xor_gc, rec->x_rub, rec->y_rub,
                     rec->w_rub, rec->h_rub);

      /* update the rectangle */
      SetRectangle(ev->xbutton.x, ev->xbutton.y, rec);

      /* draw the new box */
      XDrawRectangle(XtDisplay(widget), XtWindow(widget),
                     rec->xor_gc, rec->x_rub, rec->y_rub,
                     rec->w_rub, rec->h_rub);
   }
#if 0
   /**********************************************
    *  No moving icons for the time being

   IconRec       *icon;
   Dimension      width, height;
   int            new_x, new_y;
   int            x, y;

   if (!rec->hit) return;

   * constrain motion *
   new_x = ev->xmotion.x - rec->x_off;
   if (new_x < 0)
      new_x = 0;
   if (new_x > rec->width - rec->icon_width)
      new_x = rec->width - rec->icon_width;
   new_y = ev->xmotion.y - rec->y_off;
   if (new_y < 0)
      new_y = 0;
   if (new_y > rec->height - rec->icon_height - rec->string_height)
      new_y = rec->height - rec->icon_height - rec->string_height;

   * find affected region *
   FindInvalidRect (rec, new_x, new_y, &x, &y, &width, &height);

   * set new position... *
   icon = FindIcon(rec, rec->hit);
   icon->x = new_x;
   icon->y = new_y;

   * ...and redraw region *
   RedrawRegion (widget, rec, x, y, width, height);
   ***********************************************************/
#endif
}

/***********************************************************************
 *                                                                     *
 *   Handle GraphicsExpose events.                                     *
 *                                                                     *
 *   Workaround for DrawingArea widget deficiency:                     *
 *   If a GraphicsExpose is recieved, redraw the window by calling the *
 *   DrawingArea widget's XmNexposeCallback list.                      *
 *                                                                     *
 ***********************************************************************/
static void HandleGraphicsExposures(widget, rec, ev)
   Widget  widget;
   BoxRec *rec;
   XEvent *ev;
{
   /*
    * This routine will be called for all non-maskable events. Make sure
    * it's the type we want.
    */
   if (ev->type == GraphicsExpose) {

      XmDrawingAreaCallbackStruct cb;
      /*
       * Fill out a call data struct.
       */
      cb.reason = XmCR_EXPOSE;
      cb.event  = ev;
      cb.window = XtWindow(widget);
      /*
       * Invoke all handlers on the XmNexposeCallback list.
       */
      XtCallCallbacks(widget, XmNexposeCallback, &cb);
   }
}

/***********************************************************************
 *                                                                     *
 *   Delete icon from the IconBox (user callable).                     *
 *                                                                     *
 ***********************************************************************/
C_DECL_3(void IconBoxDeleteItem,
     Widget, widget,
     XmString, item,
     Boolean, refresh)
{
   Arg         args[1];
   BoxRec     *rec;
   IconRec    *tmp1, *tmp2;

   XtSetArg (args[0], XmNuserData, &rec);
   XtGetValues (widget, args, 1);

   if (!rec || !rec->icons) return;

   tmp1 = rec->icons;
   tmp2 = NULL;

   while (tmp1) {
      if (XmStringCompare(tmp1->label, item)) break;
      tmp2 = tmp1;
      tmp1 = tmp1->link;
   }

   /* item not found */
   if (!tmp1) return;

   if (tmp1) {
      XtFree(tmp1->mask_name);
      XtFree(tmp1->pix_name);
      XtFree(tmp1->mask_sname);
      XtFree(tmp1->pix_sname);
      XmStringFree(tmp1->label);
      if (tmp2) {
         tmp2->link = tmp1->link;
         if (rec->last_icon == tmp1) rec->last_icon = tmp2;
      } else {
         rec->icons = tmp1->link;
         if (rec->last_icon == tmp1) rec->last_icon = NULL;
      }
      XtFree((char *)tmp1);
   }
   rec->no_icons--;

   CenterPixmap(rec, NULL,
                rec->min_big_width, rec->min_big_height,
                rec->min_small_width, rec->min_small_height);

   if (refresh)
      IconBoxRefresh(widget);
}

/***********************************************************************
 *                                                                     *
 *   Delete all icons from the IconBox (user callable).                *
 *                                                                     *
 ***********************************************************************/
C_DECL_2(void IconBoxDeleteAllItems,
     Widget, widget,
     Boolean, refresh)
{
   Arg         args[1];
   BoxRec     *rec;
   IconRec    *tmp1, *tmp2;

   XtSetArg (args[0], XmNuserData, &rec);
   XtGetValues (widget, args, 1);

   if (!rec || !rec->icons) return;

   /* free records */
   tmp1 = rec->icons;
   while (tmp1) {
      XtFree(tmp1->mask_name);
      XtFree(tmp1->pix_name);
      XtFree(tmp1->mask_sname);
      XtFree(tmp1->pix_sname);
      XmStringFree(tmp1->label);
      tmp2 = tmp1;
      tmp1 = tmp1->link;
      XtFree((char *)tmp2);
   }
   rec->icons     = NULL;
   rec->last_icon = NULL;
   rec->no_icons  = 0;
   rec->last_id   = 0;

   /* reset minimum pixmap sizes */
   rec->icon_big_width    = rec->min_big_width;
   rec->icon_big_height   = rec->min_big_height;
   rec->icon_small_width  = rec->min_small_width;
   rec->icon_small_height = rec->min_small_height;

   if (refresh)
      IconBoxRefresh(widget);
}

/***********************************************************************
 *                                                                     *
 *   Handle destroy callback.                                          *
 *                                                                     *
 ***********************************************************************/
static void DestroyIconBox(widget, rec, call_data)
       Widget widget;
       BoxRec *rec;
       caddr_t call_data;
{
   IconRec      *tmp1, *tmp2;

   /* free records */
   tmp1 = rec->icons;
   while (tmp1) {
      XtFree(tmp1->pix_name);
      XtFree(tmp1->mask_name);
      XtFree(tmp1->pix_sname);
      XtFree(tmp1->mask_sname);
      XmStringFree(tmp1->label);
      tmp2 = tmp1;
      tmp1 = tmp1->link;
      XtFree((char *)tmp2);
   }
   if (rec->back) {
      XFreePixmap(XtDisplay(widget), rec->back);
      XFreeGC(XtDisplay(widget), rec->draw_gc);
      XtReleaseGC(widget, rec->normal_gc);
      XtReleaseGC(widget, rec->xor_gc);
   }
   if (rec->box_icon) {
      XFreePixmap(XtDisplay(widget), rec->box_icon);
      XFreePixmap(XtDisplay(widget), rec->box_hi_icon);
   }
   if (rec->box_sicon) {
      XFreePixmap(XtDisplay(widget), rec->box_sicon);
      XFreePixmap(XtDisplay(widget), rec->box_hi_sicon);
   }
   XtFree((char *)rec);
}
