/*
 * $Id: icbox.h,v 1.1.1.1 1996/03/08 15:33:11 mclareni Exp $
 *
 * $Log: icbox.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:11  mclareni
 * Kuip
 *
 */

/* IconBox.h */
/***********************************************************************
 *                                                                     *
 *   Header file for the IconBox (convenience) widget.                 *
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

#ifndef _IconBox_h
#define _IconBox_h

/* new resource fields */

#define XmNiconBitmap           "iconBitmap"
#define XmNiconSmallBitmap      "iconSmallBitmap"
/* #define XmNiconMask             "iconMask" */
#define XmNiconSmallMask        "iconSmallMask"
#define XmNiconForeground       "iconForeground"
#define XmNiconBackground       "iconBackground"
#define XmNiconLabelForeground  "iconLabelForeground"
#define XmNiconLabelBackground  "iconLabelBackground"
#define XmNiconWidth            "iconWidth"
#define XmNiconHeight           "iconHeight"
#define XmNiconSmallWidth       "iconSmallWidth"
#define XmNiconSmallHeight      "iconSmallHeight"
#define XmNiconHSpacing         "iconHSpacing"
#define XmNiconVSpacing         "iconVSpacing"
#define XmNiconSmallHSpacing    "iconSmallHSpacing"
#define XmNiconSmallVSpacing    "iconSmallVSpacing"
#define XmNiconNoHSpacing       "iconNoHSpacing"
#define XmNiconNoVSpacing       "iconNoVSpacing"
#define XmNiconType             "iconType"
#define XmNshadowOffset         "shadowOffset"
#define XmNshadowColor          "shadowColor"
#define XmNzoomEffect           "zoomEffect"
#define XmNzoomSpeed            "zoomSpeed"
#ifndef XmNdoubleClickInterval
#define XmNdoubleClickInterval  "doubleClickInterval"
#endif

#define XmCIconBitmap           "IconBitmap"
#define XmCIconSmallBitmap      "IconSmallBitmap"
/* #define XmCIconMask             "IconMask" */
#define XmCIconSmallMask        "IconSmallMask"
#define XmCIconForeground       "IconForeground"
#define XmCIconBackground       "IconBackground"
#define XmCIconLabelForeground  "IconLabelForeground"
#define XmCIconLabelBackground  "IconLabelBackground"
#define XmCIconWidth            "IconWidth"
#define XmCIconHeight           "IconHeight"
#define XmCIconSmallWidth       "IconSmallWidth"
#define XmCIconSmallHeight      "IconSmallHeight"
#define XmCIconHSpacing         "IconHSpacing"
#define XmCIconVSpacing         "IconVSpacing"
#define XmCIconSmallHSpacing    "IconSmallHSpacing"
#define XmCIconSmallVSpacing    "IconSmallVSpacing"
#define XmCIconNoHSpacing       "IconNoHSpacing"
#define XmCIconNoVSpacing       "IconNoVSpacing"
#define XmCIconType             "IconType"
#define XmCShadowOffset         "ShadowOffset"
#define XmCShadowColor          "ShadowColor"
#define XmCZoomEffect           "ZoomEffect"
#define XmCZoomSpeed            "ZoomSpeed"
#ifndef XmCDoubleClickInterval
#define XmCDoubleClickInterval  "DoubleClickInterval"
#endif

#define IconBoxBIG_ICON    1
#define IconBoxSMALL_ICON  2
#define IconBoxNO_ICON     3
#define IconBoxTITLES      4   /* list with small icons and full titles */

/* definition of callback structure */
typedef struct {
   int             reason;
   XEvent         *event;
   XmStringTable   selected_items;
   int             selected_item_count;
   int            *selected_item_positions;
   char          **selected_item_class;
   int             selection_type;
} IconBoxCallbackStruct;

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

/* user callable routines */
extern C_PROTO_4(Widget  CreateIconBox,
                            Widget      parent,
                            char       *name,
                            Arg         add_args[],
                            Cardinal    num_add_args);
extern C_PROTO_4(void    IconBoxAddCallback,
                            Widget          widget,
                            char           *cb_type,
                            XtCallbackProc  cb,
                            caddr_t         client_data);
extern C_PROTO_5(void    IconBoxAddItem,
                            Widget      widget,
                            XmString    item,
                            char       *class,
                            int         position,
                            Boolean     refresh);
extern C_PROTO_9(void    IconBoxAddItemPixmap,
                            Widget      widget,
                            XmString    item,
                            char       *class,
                            Pixmap      big_pix,
                            Pixmap      hi_big_pix,
                            Pixmap      small_pix,
                            Pixmap      hi_small_pix,
                            int         position,
                            Boolean     refresh);
extern C_PROTO_13(void   IconBoxAddItemBitmapData,
                            Widget      widget,
                            XmString    item,
                            char       *class,
                            char       *name,
                            char       *bits,
                            int         width,
                            int         height,
                            char       *sname,
                            char       *sbits,
                            int         swidth,
                            int         sheight,
                            int         position,
                            Boolean     refresh);
extern C_PROTO_3(void    IconBoxDeleteItem,
                            Widget      widget,
                            XmString    item,
                            Boolean     refresh);
extern C_PROTO_2(void    IconBoxDeleteAllItems,
                            Widget      widget,
                            Boolean     refresh);
extern C_PROTO_1(void    IconBoxRefresh,
                            Widget      widget);
extern C_PROTO_2(Boolean IconBoxItemExists,
                            Widget      widget,
                            XmString    item);
extern C_PROTO_2(Boolean IconBoxDeckExists,
                            Widget      widget,
                            XmString    item);
extern C_PROTO_3(void    IconBoxGetItems,
                            Widget      widget,
                            XmString  **items,
                            int        *nitems);
extern C_PROTO_3(void    IconBoxGetSelectedItems,
                            Widget      widget,
                            XmString  **items,
                            int        *nitems);
extern C_PROTO_2(int     IconBoxGetLastSelectedItem,
                            Widget      widget,
                            XmString   *item);
extern C_PROTO_8(int     IconBoxGetItemAttr,
                            Widget      widget,
                            XmString    item,
                            int        *x,
                            int        *y,
                            int        *width,
                            int        *height,
                            char      **class,
                            Boolean    *selected);
extern C_PROTO_4(void    IconBoxSelectItems,
                            Widget      widget,
                            XmString   *items,
                            int         nitems,
                            Boolean     notify);
extern C_PROTO_5(void    IconBoxSelectDecks,
                            Widget      widget,
                            XmString   *decks,
                            int         ndecks,
                            Boolean     refresh,
                            Boolean     notify);
extern C_PROTO_3(void    IconBoxGetSelectedPos,
                            Widget      widget,
                            int       **pos,
                            int        *npos);
extern C_PROTO_4(void    IconBoxSelectPos,
                            Widget      widget,
                            int        *pos,
                            int         npos,
                            Boolean     notify);
extern C_PROTO_3(void    IconBoxSelectAllItems,
                            Widget      widget,
                            Boolean     refresh,
                            Boolean     notify);
extern C_PROTO_2(void    IconBoxDeselectAllItems,
                            Widget      widget,
                            Boolean     refresh);
extern C_PROTO_1(int     IconBoxGetPos,
                            Widget      widget);
extern C_PROTO_2(void    IconBoxSetPos,
                            Widget      widget,
                            int         value);
extern C_PROTO_2(void    IconBoxSetItem,
                            Widget      widget,
                            XmString    item);
extern C_PROTO_1(int     IconBoxGetIconType,
                            Widget      widget);
extern C_PROTO_2(void    IconBoxSetIconType,
                            Widget      widget,
                            int         type);
extern C_PROTO_7(void    IconBoxChangeItemPixmap,
                            Widget      widget,
                            XmString    item,
                            Pixmap      big_pix,
                            Pixmap      hi_big_pix,
                            Pixmap      small_pix,
                            Pixmap      hi_small_pix,
                            Boolean     refresh);
extern C_PROTO_7(void    IconBoxChangePosPixmap,
                            Widget      widget,
                            int         position,
                            Pixmap      big_pix,
                            Pixmap      hi_big_pix,
                            Pixmap      small_pix,
                            Pixmap      hi_small_pix,
                            Boolean     refresh);
extern C_PROTO_6(void    IconBoxZoom,
                            Widget      widget,
                            int         x,
                            int         y,
                            int         width,
                            int         height,
                            Boolean     expand);

#if defined(__cplusplus) || defined(c_plusplus)
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _IconBox_h */

