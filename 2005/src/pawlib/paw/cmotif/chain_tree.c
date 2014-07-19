/*
 * $Id: chain_tree.c,v 1.1.1.1 1996/03/01 11:38:54 mclareni Exp $
 *
 * $Log: chain_tree.c,v $
 * Revision 1.1.1.1  1996/03/01 11:38:54  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.04/15 14/03/94  01.31.33  by  Fons Rademakers*/
/*-- Author :    Fons Rademakers   28/10/92*/
#include <stdio.h>
#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>

#include "hmotif/pawm.h"
#include "hpaw/chain.h"
#include "paw/tree.h"

#if defined(CERNLIB_QX_SC)
#define  show_chain_tree     show_chain_tree_
#endif

/* chain tree window icon */
#define chain_tree_width 50
#define chain_tree_height 50
static char chain_tree_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x38, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1c, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x1e, 0x00, 0x00, 0x3e, 0x60, 0x00, 0x00, 0x1e,
   0x00, 0xc0, 0xff, 0x79, 0x3c, 0x00, 0x0e, 0x00, 0xc0, 0xc1, 0xf9, 0x78,
   0x00, 0x0e, 0x00, 0xc0, 0xc0, 0xd9, 0x60, 0x00, 0x0e, 0x00, 0xc0, 0xc0,
   0xd9, 0x60, 0x00, 0x0e, 0x00, 0xc0, 0xe0, 0xd9, 0x40, 0x00, 0x0e, 0x00,
   0xc0, 0xe0, 0xdd, 0xc1, 0x00, 0x0e, 0x00, 0xc0, 0x61, 0x9c, 0xc1, 0x00,
   0x0e, 0x00, 0xc0, 0x3b, 0x9c, 0xc1, 0x1c, 0x0e, 0x00, 0xc0, 0x1f, 0x8e,
   0xc1, 0x1d, 0x0e, 0x00, 0xc0, 0x00, 0x0e, 0x83, 0x3d, 0x0e, 0x00, 0xc0,
   0x00, 0x8f, 0x83, 0x3d, 0x07, 0x00, 0xc0, 0x00, 0xff, 0x87, 0xef, 0x03,
   0x00, 0xc0, 0x80, 0x7f, 0x07, 0xef, 0x01, 0x00, 0xcc, 0x01, 0x06, 0x0e,
   0xe7, 0x01, 0x00, 0xfc, 0x01, 0x06, 0x1e, 0xc7, 0x00, 0x00, 0xf8, 0x01,
   0x07, 0x1c, 0xc6, 0x00, 0x00, 0xf0, 0x83, 0x07, 0x3c, 0x86, 0x00, 0x00,
   0x80, 0x87, 0x07, 0x7c, 0x04, 0x00, 0x00, 0x00, 0x07, 0x07, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00,
   0x00, 0x00, 0x00, 0x00, 0xe0, 0x33, 0x00, 0x30, 0x00, 0x00, 0x00, 0x30,
   0x36, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1c, 0x30, 0x00, 0x00, 0x18, 0x00,
   0x00, 0x1c, 0x20, 0x00, 0x20, 0xff, 0x00, 0x00, 0x0e, 0xe0, 0xe1, 0x23,
   0x8c, 0x01, 0x00, 0x0e, 0x60, 0x32, 0x66, 0x8c, 0x01, 0x00, 0x06, 0x30,
   0x36, 0x66, 0x88, 0x01, 0x00, 0x06, 0x30, 0x36, 0x66, 0x08, 0x03, 0x00,
   0x0e, 0x30, 0x36, 0x66, 0x08, 0x03, 0x00, 0x0c, 0x30, 0x36, 0x6e, 0x08,
   0x0f, 0x00, 0x1c, 0x32, 0xe2, 0x4b, 0x08, 0x06, 0x00, 0x38, 0x13, 0x06,
   0x00, 0x00, 0x00, 0x00, 0xe0, 0x01, 0x00, 0x0f, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xfe, 0x0f, 0x00, 0x30, 0x00, 0x00, 0x00, 0x7f, 0x00, 0x18, 0xfc,
   0x00, 0x00, 0x00, 0x60, 0xe0, 0x7e, 0xcc, 0x00, 0x00, 0x00, 0x40, 0xb8,
   0x66, 0xfc, 0x00, 0x00, 0x00, 0x40, 0x18, 0x3e, 0x1c, 0x00, 0x00, 0x00,
   0xc0, 0x08, 0x1c, 0x98, 0x01, 0x00, 0x00, 0xc0, 0x08, 0xfc, 0xf8, 0x00,
   0x00, 0x00, 0xc0, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00};

/* extern functions */
extern Widget create_chainTree(void);

/* extern widgets */
extern Widget  treeChWindow;
extern Widget  closeChButton;

/* global data */


/* local functions */
static void show_tree(Widget parent, char *chain_name, Widget super_node);
static void activate_cb(Widget w, Chain *chain, XmAnyCallbackStruct *cbs);

/* local data */
static Widget    chainTree  = NULL;
static Widget    tree       = NULL;

static Chain    *top_chain  = NULL;
static Widget    top_widget = NULL;


/***********************************************************************
 *                                                                     *
 *   Show (manage) the chain tree viewer.                              *
 *                                                                     *
 ***********************************************************************/
void show_chain_tree(char *tchain_name, int *lc)
{
   Pixmap        pixmap;
   Display      *display;
   Chain        *chain;
   char         *chain_name;

   chain_name = XtCalloc(*lc+1, 1);
   strncpy(chain_name, tchain_name, *lc);

   chain = find_chain(chain_name);
   if (!chain) {
      XtFree(chain_name);
      return;
   }

   if (!chainTree) {
      chainTree = create_chainTree();

      /*
       *   Set icon for chain tree viewer
       */
      display = XtDisplay(chainTree);
      pixmap = XCreateBitmapFromData(display, DefaultRootWindow(display),
                                     chain_tree_bits, chain_tree_width,
                                     chain_tree_height);
      XtVaSetValues(XtParent(chainTree), XmNtitle, "Chain Tree",
                    XmNiconName, "Chain Tree",
                    XmNiconPixmap, pixmap,
                    XmNdeleteResponse, XmUNMAP,
                    NULL);

      /* make the chain_tree known to KUIP's Exec window */
      km_store_appShell(chainTree, "Chain Tree");

      /*
       *   Create the tree widget
       */
      tree = XtVaCreateManagedWidget("tree", treeWidgetClass,
                                     treeChWindow, XmNborderWidth, 0,
                                     NULL);

      /*
       *   Install close callback
       */
      XtAddCallback(closeChButton, XmNactivateCallback,
                    (XtCallbackProc)close_cb, chainTree);

   } else {

      /*
       * destroy tree widget and make a new one
       */
      XtDestroyWidget(tree);
      top_widget = NULL;
      top_chain  = NULL;
      tree = XtVaCreateManagedWidget("tree", treeWidgetClass,
                                     treeChWindow, XmNborderWidth, 0,
                                     NULL);
   }

   /*
    *   Create the tree and popup the tree viewer
    */
   show_tree(tree, chain_name, NULL);

   UxPopupInterface(chainTree, XtGrabNone);

   /*
    * If the dialog was already open, UxPopupInterface does nothing.  In
    * this case, at least make sure the window is raised to the top
    * of the window tree (or as high as it can get).
    */
   XMapRaised(XtDisplay(chainTree), XtWindow(XtParent(chainTree)));

   XtFree(chain_name);
}

/***********************************************************************
 *                                                                     *
 *   Display the chain tree.                                           *
 *                                                                     *
 ***********************************************************************/
static void show_tree(Widget parent, char *chain_name, Widget super_node)
{
   Widget    w;
   XmString  xmstr;
   Chain    *chain;
   Entry    *entry;

   chain = find_chain(chain_name);
   if (!chain) return;

   if (!super_node) {
      xmstr = XmStringLtoRCreate(chain_name, XmSTRING_DEFAULT_CHARSET);
      super_node = XtVaCreateManagedWidget("chain", xmPushButtonWidgetClass,
                                        parent, XtNtreeParent, super_node,
                                        XmNlabelString, xmstr,
                                        XmNhighlightThickness, 0,
                                        NULL);
      chain->widget = (void *) super_node;
      XtAddCallback(super_node, XmNactivateCallback,
                    (XtCallbackProc)activate_cb, chain);
      top_chain = chain;
      XmStringFree(xmstr);
   }

   entry = chain->entries;
   while (entry) {
      xmstr = XmStringLtoRCreate(entry->name, XmSTRING_DEFAULT_CHARSET);
      switch (entry->type) {
         case CHAIN:
            /* if necessary resolve reference to chain */
            if (!entry->chain) {
               entry->chain = find_chain(entry->name);
               if (!entry->chain)
                  break;
            }

            w = XtVaCreateManagedWidget("chain", xmPushButtonWidgetClass,
                                        parent, XtNtreeParent, super_node,
                                        XmNlabelString, xmstr,
                                        XmNhighlightThickness, 0,
                                        NULL);
            entry->widget =        (void *) w;
            entry->chain->widget = (void *) w;
            XtAddCallback(w, XmNactivateCallback,
                          (XtCallbackProc)activate_cb, entry->chain);
            XmStringFree(xmstr);
            show_tree(parent, entry->name, w);
            break;
         case LUN:
         case FILE:
            w = XtVaCreateManagedWidget("file", xmPushButtonWidgetClass,
                                        parent, XtNtreeParent, super_node,
                                        XmNlabelString, xmstr,
                                        XmNshadowThickness, 0,
                                        XmNhighlightThickness, 0,
                                        NULL);
            entry->widget = (void *) w;
            XtAddCallback(w, XmNactivateCallback,
                          (XtCallbackProc)activate_cb, NULL);
            XmStringFree(xmstr);
            break;
      }
      entry = entry->link;
   }
}

/***********************************************************************
 *                                                                     *
 *   Toggle a pushbutton widget (switching shadow colors).             *
 *                                                                     *
 ***********************************************************************/
static void highlight_button(Widget button, Boolean set)
{
   static Pixel  ts, bs, arm, fg, bg;
   Dimension     st;

   if (!bg) {
      XtVaGetValues(button, XmNbottomShadowColor, &bs, XmNtopShadowColor, &ts,
                    XmNbackground, &bg, XmNforeground, &fg,
                    XmNarmColor, &arm,
                    NULL);
   }

   /*
    * if shadowthichness (st) is 0 then we have a leave
    * in whixch case we do not have to set the top and bottom shadow colors
    */
   XtVaGetValues(button, XmNshadowThickness, &st, NULL);

   if (set) {
      if (st)
         XtVaSetValues(button, XmNbottomShadowColor, ts, XmNtopShadowColor, bs,
                       RES_CONVERT(XmNbackground, "#00007f7e0000"),
                       /**XmNbackground, arm,**/
                       NULL);
      else
         XtVaSetValues(button,
                       RES_CONVERT(XmNforeground, "red"),
                       NULL);
   } else {
      if (st)
         XtVaSetValues(button, XmNbottomShadowColor, bs, XmNtopShadowColor, ts,
                       XmNbackground, bg,
                       NULL);
      else
         XtVaSetValues(button, XmNforeground, fg, NULL);
   }
}

/***********************************************************************
 *                                                                     *
 *   Highlight tree.                                                   *
 *                                                                     *
 ***********************************************************************/
static void highlight_tree(Chain *chain, Boolean set)
{
   Entry  *entry;

   entry = chain->entries;
   while (entry) {
      switch (entry->type) {
         case CHAIN:
            highlight_button(entry->widget, set);
            highlight_tree(entry->chain, set);
            break;
         case LUN:
         case FILE:
            highlight_button(entry->widget, set);
            break;
      }
      entry = entry->link;
   }
}

/***********************************************************************
 *                                                                     *
 *   Button in chain selected. Traverse tree and highlight path.       *
 *                                                                     *
 ***********************************************************************/
static void activate_cb(Widget w, Chain *chain, XmAnyCallbackStruct *cbs)
{
   if (top_widget) {
      /*
       *  unhighlight the complete tree
       */
      highlight_button(top_widget, False);
      if (top_chain)
         highlight_tree(top_chain, False);

      if (top_widget == w) {
         top_widget = NULL;
         top_chain  = NULL;
         return;
      }
   }

   /*
    *  highlight the selected tree and Cdir to the chain
    */
   highlight_button(w, True);
   if (chain) {
      char *cmd = XtCalloc(strlen(chain->name) + 8, 1);
      sprintf(cmd, "Cdir //%s", chain->name);
      ku_exec(cmd);
      XtFree(cmd);

      /* update browsers when they are entered */
      km_reset_allbrowser(True);

      highlight_tree(chain, True);
   }

   top_widget = w;
   top_chain  = chain;
}

/***********************************************************************
 *                                                                     *
 *   Delete chain entry via the KUIP Browser.                          *
 *                                                                     *
 ***********************************************************************/
void bdelete_entry(Widget w, BrClientdata *cd, XmAnyCallbackStruct *cbs)
{
    char *s;

    s = strrchr(cd->path, '/');
    delete_chain_entry(++s, cd->kmobj);
}
