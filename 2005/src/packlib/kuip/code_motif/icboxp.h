/*
 * $Id: icboxp.h,v 1.1.1.1 1996/03/08 15:33:11 mclareni Exp $
 *
 * $Log: icboxp.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:11  mclareni
 * Kuip
 *
 */
/* IconBoxP.h */
/***********************************************************************
 *                                                                     *
 *   Private header file for the IconBox (convenience) widget.         *
 *                                                                     *
 *   The IconBox widget facilitates the building of a table of which   *
 *   the items consist of an icon and a text label.                    *
 *   All user callable routines are denoted by the string:             *
 *   "user callable".                                                  *
 *                                                                     *
 *   IconBox is based on ideas by Thomas Berlage.                      *
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

#ifndef _IconBoxP_h
#define _IconBoxP_h

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>

/* #include "IconBox.h" */
#include "icbox.h"


/************************ Record for each icon ************************/

typedef struct IconRec IconRec;
typedef struct IconRec {
   Position   x, y;            /* left upper edge of icon            */
   Position   x_off, y_off;    /* offset to center of big and small  */
   Position   x_soff, y_soff;  /* pixmaps in the respective max. bitmaps */
   Dimension  pix_w, pix_h;    /* width and height of the big pixmap */
   Dimension  spix_w, spix_h;  /* w. and h. of the small pixmap      */
   int        string_left;     /* how much strings extends to left   */
   int        id;              /* icon id                            */
   Boolean    selected;        /* is icon selected (high lighted)?   */
   XmString   label;           /* label string                       */
   String     class;           /* for selection of pixmap            */
   String     pix_name;        /* name of icon bitmap                */
   String     pix_sname;       /* name of small icon bitmap          */
   Pixmap     pix;             /* icon pixmap                        */
   Pixmap     hi_pix;          /* highlighted icon pixmap            */
   Pixmap     spix;            /* small icon pixmap                  */
   Pixmap     hi_spix;         /* highlighted small icon pixmap      */
   String     mask_name;       /* name of mask bitmap                */
   String     mask_sname;      /* name of small mask bitmap          */
   Pixmap     mask;            /* mask bitmap                        */
   Pixmap     smask;           /* small mask bitmap                  */
   Pixel      foreground;      /* foreground for icon pixmap         */
   Pixel      background;      /* background for icon pixmap         */
   Pixel      label_fg;        /* label foreground color             */
   Pixel      label_bg;        /* label background color             */
   IconRec   *link;            /* link to next icon                  */
} *IconRecPtr;

/************************ Icon box data record ************************/

typedef struct {
   Dimension  width, height;   /* size of box                        */
   Dimension  virt_height;     /* height of box with all icons       */
   Dimension  icon_width;      /* width of current icon type         */
   Dimension  icon_height;     /* height of current icon type        */
   Dimension  icon_big_width;  /* width of big icon                  */
   Dimension  icon_big_height; /* height of big icon                 */
   Dimension  icon_small_width;  /* width of small icon              */
   Dimension  icon_small_height; /* height of small icon             */
   Dimension  min_big_width;   /* min width of big icon              */
   Dimension  min_big_height;  /* min height of big icon             */
   Dimension  min_small_width;   /* min width of small icon          */
   Dimension  min_small_height;  /* min height of small icon         */
   int        string_height;   /* current height of string box       */
   int        string_bheight;  /* height of string box with big icon */
   int        icon_hspacing;   /* current hor. spacing between icons */
   int        icon_vspacing;   /* current ver. spacing between icons */
   int        icon_bhspacing;  /* horz. spacing between big icons    */
   int        icon_bvspacing;  /* vert. spacing between big icons    */
   int        icon_shspacing;  /* horz. spacing between small icons  */
   int        icon_svspacing;  /* vert. spacing between small icons  */
   int        icon_nhspacing;  /* horz. spacing between no icons     */
   int        icon_nvspacing;  /* vert. spacing between no icons     */
   int        ilsp;            /* icon-label spacing (big icon)      */
   int        silsp;           /* icon-label spacing (small icon)    */
   int        icon_type;       /* either big, small or no icon       */
   int        shadow_off;      /* offset of shadow effect            */
   int        columns;         /* no of icon columns                 */
   int        top;             /* pixel at top of window             */
   IconRec   *icons;           /* first icon in the list of icons    */
   IconRec   *last_icon;       /* last icon in list of icons         */
   int        no_icons;        /* no of icons in list                */
   int        last_id;         /* id of last icon                    */
   Pixel      shadow_color;    /* color for shadow effect            */
   Pixel      background;      /* pixmap background                  */
   Boolean    exposed;         /* is the window exposed              */
   Boolean    redraw_all;      /* need to redraw the whole window    */
   Boolean    scrolled;        /* has the window just been scrolled  */
   GC         draw_gc;         /* to draw into backing pixmap        */
   GC         xor_gc;          /* to draw rubberband box             */
   GC         normal_gc;       /* to copy to screen                  */
   Pixmap     back;            /* background storage                 */
   Pixmap     box_icon;        /* big icon                           */
   Pixmap     box_hi_icon;     /* big hi-lighted icon                */
   Pixmap     box_sicon;       /* small icon                         */
   Pixmap     box_hi_sicon;    /* small hi-lighted icon              */
   XmFontList fontlist;        /* for strings                        */
   int        dc_interval;     /* double-click interval              */
   Boolean    rubber;          /* drawing the rubberband box         */
   int        x_off, y_off;    /* mouse offset when drag-moving      */
   int        x_rubs, y_rubs;  /* start position of rubberband box   */
   int        x_rub, y_rub;    /* top left pos of rubberband box     */
   Dimension  w_rub, h_rub;    /* width and height of rubberband box */
   Boolean    zoom_effect;     /* making zoom effect or not          */
   int        zoom_speed;      /* speed of zoom effect               */
   int        hit;             /* index of icon currently pointed at */
   Widget     area;            /* drawing area in the scrollwindow   */
   Widget     scrollbar;       /* scrollbar of the scrollwindow      */
   XtCallbackProc default_cb;  /* default action callback            */
   caddr_t    default_cd;      /* default action client data         */
   XtCallbackProc single_cb;   /* single selection callback          */
   caddr_t    single_cd;       /* single selection client data       */
   XtCallbackProc multiple_cb; /* multiple selection callback        */
   caddr_t    multiple_cd;     /* multiple selection clinet data     */
   XtCallbackProc extended_cb; /* extended selection callback        */
   caddr_t    extended_cd;     /* extended selection client data     */
} BoxRec, *BoxRecPtr;


#endif /* _IconBoxP_h */

