/*
 * $Id: kmbrow.c,v 1.1.1.1 1996/03/08 15:33:08 mclareni Exp $
 *
 * $Log: kmbrow.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:08  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/09 20/02/95  12.14.20  by  N.Cremel*/
/*-- Author : Nicole Cremel*/
/******************************************************************************/
/*                                                                            */
/*           MOTIF-based library for KUIP - deck KMBROW                       */
/*                                                                            */
/*           Basic Interface for any "KUIP Browsers"                          */
/*           (connected to the file //KUIP/XMCLIB/KUXBRO for building the     */
/*           panels with Architect : "create_kuipBrowser").                   */
/*                                                                            */
/******************************************************************************/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/klink.h"
#include "kuip/kmenu.h"

#include <stdio.h>
#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/PushBGP.h>

#include "mkdims.h"
#include "kuip/mkutfu.h"
#include "mkutfm.h"
#include "mkutda.h"
#include "mkutdm.h"
#include "kuip/mkterm.h"
#include "icbox.h"
#include "kuip/browh1.h"
#include "browh2.h"
#include "browbm.h"
#define CONTEXT_MACRO_ACCESS 1
#include "mkuxbr.h"
#undef CONTEXT_MACRO_ACCESS


/* structure definition */
typedef struct {
        Widget browser;
        BrAction *bract;
               } otag_str;  /* tag structure for FilePdMenu_o_activate */

/* local data */
static Widget        last_browser;
static Widget        file_popup;
static Widget        dir_popup;
static int           n_fbrowser              = 0;
static int           hili_left               = 0;
static int           hili_right              = 0;
static Boolean       no_recursion            = False;
static Boolean       path_edit               = False;
static int           current_browser;
static Widget        class_popup = NULL;
static Widget        object_popup = NULL;
static char          br_filter[MAX_stringl];
static char          *br_init                = NULL;
static char          *path_init              = NULL;
static int           init_br                 = 0;


/* extern data */
extern XmAnyCallbackStruct *current_callback_data;

/***********************************************************************
 *                                                                     *
 *   Forward declarations of extern functions.                         *
 *                                                                     *
 ***********************************************************************/
extern C_PROTO_1(Widget create_kuipBrowser,
                         void);
extern C_PROTO_1(void create_menu_help,
                         Widget parent);
extern C_PROTO_3(void popup_activate,
                         Widget widget,
                         caddr_t tag,
                         XmAnyCallbackStruct *call_data);


/***********************************************************************
 *                                                                     *
 *   Forward declarations of global functions.                         *
 *                                                                     *
 ***********************************************************************/
C_PROTO_3(void km_show_Browser,
                         Widget w,
                         Widget clone_parent,
                         XmAnyCallbackStruct *call_data);
C_PROTO_2(void kxterm_show_browser, char **, int);
C_PROTO_3(Widget km_create_Browser,
                         Widget w,
                         Widget clone_parent,
                         XmAnyCallbackStruct *call_data);
C_PROTO_1( void file_execute,
                         char *lun);
C_PROTO_3( void dir_execute,
                         Widget browser,
                         KmObject *kmobj,
                         char *lun);
C_PROTO_3(void update_browser,
                         Widget widget,
                         int list,
                         Boolean refresh);
C_PROTO_0(void km_reset_class_popup);
C_PROTO_0(void km_reset_object_popup);
C_PROTO_0(char *km_get_br_filter);

/* C-callable entry to set browser in a given state */
void km_browser_set ( const char *file, const char *path );

/***********************************************************************
 *                                                                     *
 *   Forward declarations of local functions.                          *
 *                                                                     *
 ***********************************************************************/
static C_PROTO_1(Widget find_kuipBrowser,
                         Widget child);
static C_PROTO_6(void list_delete_item,
                         int     *lst,
                         char    *item,
                         int     *litem,
                         int     *all1,
                         int     *refresh,
                         int     *all_dir);
static C_PROTO_2(void list_refresh,
                         int *list,
                         int *all_dir);
static C_PROTO_1(void file_list_refresh,
                         Widget widget);
static C_PROTO_1(int  get_fb_context,
                         Widget w);
static C_PROTO_1(void save_fb_context,
                         Widget w);
static C_PROTO_1(void focus_allbrowser,
                         Boolean set);
static C_PROTO_3(void destroy_kuipBrowser,
                         Widget w,
                         Widget browser,
                         XmAnyCallbackStruct *call_data);
static C_PROTO_0(char *get_path);
static C_PROTO_1(void set_path,
                         char *path);
static C_PROTO_3(void reset_cursor,
                         Widget w,
                         Widget browser,
                         XButtonPressedEvent *event);
static C_PROTO_3(void change_path,
                         Widget w,
                         Widget browser,
                         XmTextVerifyCallbackStruct *cbs);
static C_PROTO_3(void edit_path,
                         Widget w,
                         Widget browser,
                         XmTextVerifyCallbackStruct *cbs);
static C_PROTO_3(void do_file_list,
                         Widget w,
                         Widget browser,
                         XmListCallbackStruct *call_data);
static C_PROTO_1(KmObject *get_dir_data,
                         Widget browser);
static C_PROTO_3(void do_dir_list,
                         Widget w,
                         Widget browser,
                         IconBoxCallbackStruct *call_data);
static C_PROTO_3(void enter_browser,
                         Widget widget,
                         int tag,
                         XEnterWindowEvent *ev);
static C_PROTO_3(void post_class_menu,
                         Widget w,
                         Widget browser,
                         XButtonPressedEvent *event);
static C_PROTO_3(void post_object_menu,
                         Widget w,
                         Widget browser,
                         XButtonPressedEvent *event);
static C_PROTO_5(void list_addc_item,
                         KmObject *kmobj,
                         int ltype,
                         int pos,
                         Boolean refresh,
                         Boolean all_dir);
#if UNREF
static C_PROTO_3(void list_addp_item,
                         char *item,
                         int pos,
                         Boolean all_dir);
#endif
static C_PROTO_2 (void do_list_content,
                         char *path,
                         Boolean refresh);
static C_PROTO_1(void init_fb_menu,
                         Widget browser);
static C_PROTO_1(void set_icon_tb_state,
                         int icon_type);
static C_PROTO_3(void change_icon_type,
                         Widget w,
                         int icon_type,
                         XmToggleButtonCallbackStruct *call_data);
#ifdef ALL_CYCLE /* vms */
static C_PROTO_3(void change_cycle,
                         Widget w,
                         caddr_t tag,
                         XmToggleButtonCallbackStruct *call_data);
#endif
static C_PROTO_3(void select_all_decks,
                         Widget w,
                         Widget browser,
                         XmAnyCallbackStruct *call_data);
static C_PROTO_3(void show_filtDialog,
                         Widget w,
                         Widget browser,
                         XmAnyCallbackStruct *call_data);
static C_PROTO_3(void FilePdMenu_o_activate,
                         Widget widget,
                         caddr_t tag,
                         XmAnyCallbackStruct *call_data);
static C_PROTO_3(void FilePdMenu_exit,
                         Widget widget,
                         caddr_t tag,
                         XmAnyCallbackStruct *call_data);
static void browser_init();
static void set_browser ( char *file );



/***********************************************************************
 *                                                                     *
 *   Show (manage) the file browser.                                   *
 *                                                                     *
 ***********************************************************************/
void km_show_Browser(w, clone_parent, call_data)
     Widget w, clone_parent;
     XmAnyCallbackStruct *call_data;
{
     km_create_Browser(w, clone_parent, call_data);
}

/***********************************************************************
 *                                                                     *
 *   Action routine which will be called by handle_kxterm_actions to   *
 *   popup a new browser.                                              *
 *                                                                     *
 ***********************************************************************/
void kxterm_show_browser( argv, argc )
     char **argv;
     int argc;
{
    km_show_Browser((Widget)NULL, (Widget)NULL,
                    (XmAnyCallbackStruct *)NULL);
}

/***********************************************************************
 *                                                                     *
 *   Create the file browser.                                          *
 *                                                                     *
 ***********************************************************************/
Widget km_create_Browser(w, clone_parent, call_data)
     Widget w, clone_parent;
     XmAnyCallbackStruct *call_data;
{
   Widget        widget;
   Pixmap        pixmap;
   Pixel         fgc;
   XmString     *xfsel, *xdsel;
   Display      *display;
   char         *path, *title;
   char          cval[MAX_string], cval1[MAX_string];
   int           i, fitem, fcnt, dcnt, icon_type;
   int           browser_count = 0;

   BrObject *brobj;

   if (n_fbrowser >= MAX_FILE_BROWSER) {
      km_inform_user(w, "Cannot open more file browsers.", "");
      return(NULL);
   }

   widget = create_kuipBrowser();
   last_browser = widget;

   get_fb_context(widget);


   /* set icon for file browser popup */
   display = XtDisplay(widget);
   pixmap = XCreateBitmapFromData(display, DefaultRootWindow(display),
                 (char*)browser_bits, browser_width, browser_height);

   for (i = 0; i < MAX_FILE_BROWSER; i++) {
      if (!fbrowser[i].widget) {
         browser_count = i;
         break; }
      }

   /*
    * First browser is called the so called Main browser.
    */
   if (!browser_count)
      sprintf (cval,"%s Main Browser", class_name);
   else
      sprintf (cval,"%s Browser %d", class_name,browser_count+1);
   XtVaSetValues(XtParent(widget), XmNtitle, cval,
                  XmNiconName, cval,
                  XmNiconPixmap, pixmap,
                  NULL);

   /*
    * Register browser with application via application defined routine.
    */
   sprintf (cval1,"kuipBrowser%d", browser_count+1);
   if( klnkaddr.user_TopWid_C != NULL ) {
     /* pass widget ID to application */
     (*klnkaddr.user_TopWid_C)(cval1, widget);
     XtVaGetValues(XtParent(widget), XmNtitle, &title, NULL);
     km_store_appShell (widget, title);
   } else {
     km_store_appShell (widget, cval);
   }

   XtVaGetValues(widget, XmNbackground, &fgc, NULL);
   XtVaSetValues(pathText, XmNbackground, fgc, NULL);

   /*
    *   install callbacks on browser buttons
    */
   if (!browser_count) {
      XtVaSetValues(XtParent(widget), XmNdeleteResponse, XmDO_NOTHING, NULL);
      XtSetSensitive(closeButton, False);
   } else {
      km_MwmCloseCallback(XtParent(widget),
                          (XtCallbackProc)destroy_kuipBrowser,
                          (XtPointer)widget);
      XtAddCallback(closeButton, XmNactivateCallback,
                    (XtCallbackProc)destroy_kuipBrowser, widget);
   }
   XtAddCallback(cloneButton, XmNactivateCallback,
                 (XtCallbackProc)km_show_Browser, widget);

   /*
    *    install callbacks on the lists
    */
   XtAddCallback(fileList, XmNbrowseSelectionCallback,
          (XtCallbackProc)do_file_list, widget);
   XtAddCallback(fileList, XmNsingleSelectionCallback,
          (XtCallbackProc)do_file_list, widget);
   XtAddCallback(fileList, XmNdefaultActionCallback,
          (XtCallbackProc)do_file_list, widget);

   /*
    *     install event handler on the pathText widget
    */
   XtAddEventHandler(pathText, ButtonPressMask, False,
                     (XtEventHandler)reset_cursor, widget);
   XtAddCallback(pathText, XmNmotionVerifyCallback,
                     (XtCallbackProc)change_path, widget);

   dirList = CreateIconBox(dirScWindow, "dirlist", NULL, 0);

   /*
    *    install callbacks on the dirList
    */
   IconBoxAddCallback(dirList, XmNdefaultActionCallback,
               (XtCallbackProc)do_dir_list, (caddr_t)widget);
   IconBoxAddCallback(dirList, XmNsingleSelectionCallback,
               (XtCallbackProc)do_dir_list, (caddr_t)widget);
   IconBoxAddCallback(dirList, XmNextendedSelectionCallback,
               (XtCallbackProc)do_dir_list, (caddr_t)widget);
   IconBoxAddCallback(dirList, XmNmultipleSelectionCallback,
               (XtCallbackProc)do_dir_list, (caddr_t)widget);

   /*
    *    install event handler on EnterNotify events
    */
   current_browser =  browser_count;
   XtAddEventHandler(widget, EnterWindowMask, False,
              (XtEventHandler)enter_browser, (XtPointer)browser_count);


   /*
    *    initialize the View menu
    */
   init_fb_menu(widget);

   /*
    *    initialize the Help menu
    */
   create_menu_help(widget);

   /*
    *     install the popup menu's and event handlers
    */
   XtAddEventHandler(fileList, ButtonPressMask, False,
                     (XtEventHandler)post_class_menu, widget);
   XtAddEventHandler(dirList, ButtonPressMask, False,
                     (XtEventHandler)post_object_menu, widget);


   /* display the file browser */
   UxPopupInterface(widget, XtGrabNone);

   /* get icon_type and titles state */
   icon_type = IconBoxGetIconType(dirList);
   if (icon_type == IconBoxBIG_ICON)   titles = 0;
   else if (icon_type == IconBoxSMALL_ICON) titles = 1;
   else if (icon_type == IconBoxNO_ICON)    titles = 2;
   else if (icon_type == IconBoxTITLES) titles = 3;


   n_fbrowser++;
   for (i = 0; i < MAX_FILE_BROWSER; i++) {
      if (!fbrowser[i].widget) {
       fbrowser[i].widget     = widget;
       fbrowser[i].dirlist    = dirList;
       fbrowser[i].file_popup = file_popup;
       fbrowser[i].dir_popup  = dir_popup;
       fbrowser[i].pref_dialog = (Widget) NULL;
       fbrowser[i].pref_menu  = (MenuCbStruct *) NULL;
       fbrowser[i].cd[0]      = '\0';
       fbrowser[i].hili_left  = 0;
       fbrowser[i].hili_right = 0;
       fbrowser[i].titles     = titles;
       fbrowser[i].order      = 0;
       strcpy (fbrowser[i].filter, "*");
       fbrowser[i].focus      = False;
       fbrowser[i].reset      = False;
       fbrowser[i].path_edit  = False;
       break;
      }
   }

   /* Show all connected files (class window) */
   for ( brobj = scan_brobjects(); brobj != NULL; brobj = brobj->next ) {
     /* Get total number of classes and for each one get and
      * add short_name in class window
      */
     XmString xmstr = XmStringCreate( brobj->name, XmSTRING_DEFAULT_CHARSET );
     XmListAddItem( fileList, xmstr, 0 );
     XmStringFree( xmstr );
   }


   /* is it a clone ? */
   if (clone_parent) {

      /* set context to the parent */
      get_fb_context(clone_parent);

      /* get selected item in the parent's file list */
      XtVaGetValues(fileList, XmNitemCount, &fitem,
             XmNselectedItemCount,   &fcnt,
             XmNselectedItems,       &xfsel, NULL);
      if (!fcnt) return(widget);

      IconBoxGetSelectedItems(dirList, &xdsel, &dcnt);
      icon_type = IconBoxGetIconType(dirList);

      /* get current path in the parent's file list */
      path = get_path();
      strcpy (browser_dir, path);

      /*
       * save parent's context as clone context (to save titles, order and
       * filter settings)
       */
      save_fb_context(widget);

      /* set context back to the clone */
      get_fb_context(widget);

      XmListSelectItem(fileList, xfsel[0], True);
      IconBoxSetIconType(dirList, icon_type);
      set_path(path);

      if (titles)
         set_icon_tb_state(IconBoxTITLES);
      else
         set_icon_tb_state(icon_type);

      /* list content of selected_path for this new current_browser */
      do_list_content(path, True);


      /* highlight the icons that were selected */
      IconBoxSelectItems(dirList, xdsel, dcnt, False);


      if (dcnt > 0) XtFree((char *)xdsel);
      XtFree(path);
   } else {
      set_path("");
   }

   return (widget);
}

/***********************************************************************
 *                                                                     *
 *   Get the context for kuipBrowser widget.                           *
 *                                                                     *
 ***********************************************************************/
static int get_fb_context(w)
    Widget w;
{
   int           i;

   if (!w) return -2;

   /*
    *   find and restore the context
    *   (to get to the widgets belonging to this dialog)
    */
   UxKuipBrowserContext = (_UxCkuipBrowser *)UxGetContext(w);


   /* restore dirList context by hand (not in Architect context) */
   for (i = 0; i < MAX_FILE_BROWSER; i++) {
      if (fbrowser[i].widget == w) {
       dirList    = fbrowser[i].dirlist;
       file_popup = fbrowser[i].file_popup;
       dir_popup  = fbrowser[i].dir_popup;
       pref_dialog = fbrowser[i].pref_dialog;
       pref_menu = fbrowser[i].pref_menu;
       strcpy(browser_dir, fbrowser[i].cd);
       hili_left  = fbrowser[i].hili_left;
       hili_right = fbrowser[i].hili_right;
       titles     = fbrowser[i].titles;
       order      = fbrowser[i].order;
       strcpy(browser_filter, fbrowser[i].filter);
       browser_focus = fbrowser[i].focus;
       browser_reset = fbrowser[i].reset;
       path_edit     = fbrowser[i].path_edit;
       return i;
      }
   }
   return -1;
}

/***********************************************************************
 *                                                                     *
 *   Get current value of br_filter.                                   *
 *                                                                     *
 ***********************************************************************/
char *km_get_br_filter()
{
    return br_filter;
}

/***********************************************************************
 *                                                                     *
 *   Save the context for kuipBrowser widget.                          *
 *                                                                     *
 ***********************************************************************/
static void save_fb_context(w)
     Widget w;
{
   int i;

   for (i = 0; i < MAX_FILE_BROWSER; i++) {
      if (fbrowser[i].widget == w) {
       strcpy(fbrowser[i].cd, browser_dir);
       fbrowser[i].hili_left  = hili_left;
       fbrowser[i].hili_right = hili_right;
       fbrowser[i].titles     = titles;
       fbrowser[i].order      = order;
       strcpy(fbrowser[i].filter, browser_filter);
       fbrowser[i].focus      = browser_focus;
       fbrowser[i].reset      = browser_reset;
       fbrowser[i].path_edit  = path_edit;
       break;
      }
   }
}

/***********************************************************************
 *                                                                     *
 *   Set or reset the file browser update flag.                        *
 *                                                                     *
 ***********************************************************************/
C_DECL_1(void km_reset_allbrowser,
       Boolean, set)
{
   int i;

   for (i = 0; i < MAX_FILE_BROWSER; i++) {
       fbrowser[i].reset = set;
   }
   browser_reset = set;
}

/***********************************************************************
 *                                                                     *
 *   Set or reset the file browser focus flag.                         *
 *                                                                     *
 ***********************************************************************/
static C_DECL_1(void focus_allbrowser,
       Boolean, set)
{
   int i;

   for (i = 0; i < MAX_FILE_BROWSER; i++) {
       fbrowser[i].focus = set;
   }
   browser_focus = set;
}

/***********************************************************************
 *                                                                     *
 *   Destroy the file browser dialog.                                  *
 *                                                                     *
 ***********************************************************************/
static void destroy_kuipBrowser(w, browser, call_data)
     Widget w, browser;
     XmAnyCallbackStruct *call_data;
{
   int   i;

   n_fbrowser--;
   for (i = 0; i < MAX_FILE_BROWSER; i++) {
      if (fbrowser[i].widget == browser) {
       fbrowser[i].widget  = (Widget) NULL;
       fbrowser[i].dirlist = (Widget) NULL;
       fbrowser[i].file_popup = (Widget) NULL;
       fbrowser[i].dir_popup  = (Widget) NULL;
       fbrowser[i].pref_dialog = (Widget) NULL;
       XtFree((char *)fbrowser[i].pref_menu);
       fbrowser[i].pref_menu  = (MenuCbStruct *) NULL;
       fbrowser[i].cd[0]      = '\0';
       fbrowser[i].hili_left  = 0;
       fbrowser[i].hili_right = 0;
       fbrowser[i].titles     = 0;
       fbrowser[i].order      = 0;
       strcpy (fbrowser[i].filter, "*");
       fbrowser[i].focus      = False;
       fbrowser[i].reset      = False;
       fbrowser[i].path_edit  = False;
       break;
      }
   }

   UxDestroyInterface(browser);
   km_reset_appShell (browser);
}

/***********************************************************************
 *                                                                     *
 *   Find the file browser dialog.                                     *
 *                                                                     *
 ***********************************************************************/
static Widget find_kuipBrowser(child)
       Widget child;
{
   String  name;
   Widget  browser;

   /*
    * starting from a child widget find back the
    * parent file browser widget to set the right context.
    */
   browser = child;
   do {
      browser = XtParent(browser);
      name = XtName(browser);
   } while (browser && strcmp(name, "kuipBrowser"));

   return browser;
}

/***********************************************************************
 *                                                                     *
 *   Set the current path in kxterm.                                   *
 *                                                                     *
 ***********************************************************************/
static void set_path_in_kxterm(path)
       char *path;
{
   char *s = XtMalloc(strlen(path)+6);
   sprintf(s,"cwd(%s)", path);
   send_single_kxterm_cmd(s);
   XtFree(s);
}

/***********************************************************************
 *                                                                     *
 *   Handle the browser window enter and leave events.                 *
 *                                                                     *
 ***********************************************************************/
static void enter_browser(widget, tag, ev)
       Widget widget;
       int tag;
       XEnterWindowEvent *ev;
{
   extern Boolean  km_browser_refresh;
   int       fcnt;
   XmString *xfsel;
   char     *path, *selected_path;

   get_fb_context(fbrowser[tag].widget);
   if( current_browser != tag ) {
      /* When moving from one browser into another we have to refresh */
      current_browser = tag;
      browser_focus = False;
   }

   /* Test flag reset for command like HISTO/FILE */
   if (browser_reset) km_browser_refresh = True;

   if (km_browser_refresh) {
      /* after command execution : */
      /* update all browsers the 1st time one is entered */
      update_browser (NULL, 0, False);
      km_browser_refresh = False;
      km_reset_allbrowser(False);

      get_fb_context(fbrowser[current_browser].widget);
      browser_focus = False;
   }

   if (!browser_focus) {

      path = get_path();

      set_path_in_kxterm(path);

      /*
       * scan object for this filebrowser class
       * This code may be removed once each browser has its own kmobj
       * structure
       */
      XtVaGetValues(fileList, XmNselectedItemCount, &fcnt,
                              XmNselectedItems,     &xfsel, NULL);
      if (fcnt) {
         XmStringGetLtoR(xfsel[0], XmSTRING_DEFAULT_CHARSET, &selected_path);
         scan_kmobjects(selected_path, path, current_browser);
         XtFree (selected_path);
      }

      /* execute default action for this browser (e.g. cd ...) */
      update_browser (fbrowser[current_browser].widget, DIR_LIST, False);

      XtFree (path);
      focus_allbrowser(False);
      browser_focus = True;
   }

   /* always update a browser which shows the //PAWC */
   path = get_path();
   if (!strncasecmp(path, "//PAWC", 6)) do_list_content(path, True);
   XtFree (path);

   save_fb_context(fbrowser[current_browser].widget);
}


/***********************************************************************
 *                                                                     *
 *   Update browser : both class and object window.                    *
 *                                                                     *
 *   + Execute default action for the current path (change directory)  *
 *                                                                     *
 *   list = DIR_LIST, FILE_LIST or 0 (both)                            *
 *   if browser = NULL then update all browsers (class and/or object)  *
 *   if list = DIR_LIST and refresh = False then execute default       *
 *          command for this browser selected path (e.g. cd)           *
 *   if list = DIR_LIST and refresh = True then update browser content *
 *          and execute default command for this browser selected path *
 *                                                                     *
 ***********************************************************************/
C_DECL_3(void update_browser,
       Widget, browser,
       int, list,
       Boolean, refresh)
{
   XmString     *xfsel;
   char         *path;
   int           i, fitem, fcnt, oldcur;

   BrObject *brobj;
   BrClass  *brcls;
   BrAction *bract;


   if (browser != NULL) get_fb_context(browser);


   /* Update the "class window" with list of connected files */
   if (list == 0 || list == FILE_LIST) {
      if (browser == NULL) {
         /* update all browsers */
         int do_all = 1;
         int lt = FILE_LIST;

         list_refresh(&lt, &do_all);
      } else {
         file_list_refresh (fileList);
         save_fb_context(browser);
      }
   }

   if (list == FILE_LIST) return;

   if (browser == NULL) {
      /* Update all browser object windows */
       oldcur = current_browser;
       for (i = 0; i < MAX_FILE_BROWSER; i++) {
          if (fbrowser[i].widget) {
              current_browser = get_fb_context(fbrowser[i].widget);
              /* Update the content window */
              path = get_path();
              if (path == NULL) {
                 IconBoxDeleteAllItems(dirList, True);
                 continue;
              }
              do_list_content (path, True);
              XtFree (path);
          }
       }
       current_browser = oldcur;
   } else {
       /* Get selected class (lun) for this browser */
       XtVaGetValues(fileList, XmNitemCount, &fitem,
                     XmNselectedItemCount,   &fcnt,
                     XmNselectedItems,       &xfsel, NULL);
       /* Execute default action for this selected class (change directory) */
       if (fcnt) {
           char *lun;

           XmStringGetLtoR(xfsel[0], XmSTRING_DEFAULT_CHARSET, &lun);
           /* Get object structure for this lun */
           if ( (brobj = find_brobject( lun )) != NULL ) {
                brcls = brobj->class;
                bract = brcls->root;
                /* execute the class window default action for lun */
                current_callback_data = NULL;
                exec_action (bract, lun, NULL, False, NULL, NULL);
           }
           XtFree (lun);
       }
       if (refresh) {
          /* refresh content window */
          path = get_path();
          if (path == NULL) {
               IconBoxDeleteAllItems(dirList, True);
               return;
          }
          /* Store new value for "browser_dir" */
          strcpy(browser_dir, path);
          /* Update the content window */
          do_list_content (path, True);
          XtFree (path);
       }
   }
}

/***********************************************************************
 *                                                                     *
 *   Make sure that the correct file is highlighted.                   *
 *   The user may have changed the file selection.                     *
 *                                                                     *
 ***********************************************************************/
static void check_file_selection()
{
}

/***********************************************************************
 *                                                                     *
 *   Get the path in the pathText widget.                              *
 *   It removes the blank appended at the end of the path by set_path. *
 *                                                                     *
 ***********************************************************************/
static char *get_path()
{
   char *path, *s;
   int   ls;

   s   = strtrim( XmTextGetString(pathText) );
   ls  = strlen(s);

   if (ls)
      path = XtNewString(s);
   else
      path = XtCalloc(1, 1);

   free(s);

   return path;
}

/***********************************************************************
 *                                                                     *
 *   Set the path in the pathText widget.                              *
 *                                                                     *
 ***********************************************************************/
static void set_path(path)
       char *path;
{
   no_recursion = True;
   XmTextSetString(pathText, path);
   XmTextInsert(pathText, strlen(path), " ");
   no_recursion = False;

   hili_left  = 0;
   hili_right = 0;

   set_path_in_kxterm(path);
}

/***********************************************************************
 *                                                                     *
 *   Event handler which resets the cursor position to 1 so            *
 *   change_path will always be called (because the current pos        *
 *   will always be different from the new pos.                        *
 *                                                                     *
 ***********************************************************************/
static void reset_cursor(w, browser, event)
   Widget w, browser;
   XButtonPressedEvent *event;
{
   XmTextPosition  pos;

   /* only handle 1st and 3rd mouse button */
   if (event->button == 2)
      return;

   no_recursion = True;

   if (event->button == 1) {
      pos = 0;
      XmTextSetInsertionPosition(w, pos);
   }

   if (event->button == 3) {
      /* set the context */
      get_fb_context(browser);

      XmTextSetHighlight(pathText, hili_left, hili_right, XmHIGHLIGHT_NORMAL);
      hili_left  = 0;
      hili_right = 0;

      path_edit  = True;
      save_fb_context(browser);

      XtVaSetValues(pathText,
                    XmNeditable, True,
                    XmNcursorPositionVisible, True,
                    XmNshadowThickness, 2,
                    XmNmarginHeight, 3,
                    NULL);
      XtOverrideTranslations (pathText,
                           XtParseTranslationTable("<Key>Return:activate()"));
      XtAddCallback(pathText, XmNactivateCallback,
                     (XtCallbackProc)edit_path, browser);
   }

   no_recursion = False;
}

/***********************************************************************
 *                                                                     *
 *   Callback for manually editing the pathtext.                       *
 *                                                                     *
 ***********************************************************************/
static void edit_path(w, browser, cbs)
       Widget w, browser;
       XmTextVerifyCallbackStruct *cbs;
{
   char           *path, *selected_path, *lun;
   int             zero, all, list;
   XmString       *xfsel;
   int             fitem, fcnt;

   no_recursion = True;

   get_fb_context(browser);

   XtRemoveCallback(pathText, XmNactivateCallback,
                 (XtCallbackProc)edit_path, browser);

   path  = get_path();

   /* Store new value for "browser_dir" and update path */
   strcpy(browser_dir, path);
   set_path(browser_dir);
   /* make sure the values changed in set_path are saved */
   save_fb_context(browser);
   km_set_cursor (browser, km_wait_cursor);
   zero = 0;
   all  = 1;
   list = DIR_LIST;
   list_delete_item(&list, NULL, &zero, &all, &zero, &zero);
   selected_path = (char *) XtMalloc (MAX_string+1);
   strcpy (selected_path, browser_dir);
   do_list_content (selected_path, True);

   /* Get selected class (lun) for this browser */
   XtVaGetValues(fileList, XmNitemCount, &fitem,
                 XmNselectedItemCount,   &fcnt,
                 XmNselectedItems,       &xfsel, NULL);
   /* Execute default action for this selected class */
   if (fcnt) {
       BrObject     *brobj;

       XmStringGetLtoR(xfsel[0], XmSTRING_DEFAULT_CHARSET, &lun);
       /* Get object structure for this lun */
       if ( (brobj = find_brobject( lun )) != NULL ) {
             BrClass      *brcls;
             BrAction     *bract;

             brcls = brobj->class;
             bract = brcls->root;
             /* execute the class window default action for lun */
             current_callback_data = NULL;
             exec_action (bract, lun, NULL, False, NULL, NULL);

             /* Update all browsers */
             if (bract->flags & BrActUpdate)
                 update_browser(NULL, FILE_LIST, False);
       }
       XtFree (lun);
   }

   km_set_cursor (browser, km_main_cursor);
   XtFree (selected_path);
   check_file_selection();

   path_edit = False;

   save_fb_context(browser);

   XtFree(path);

   XtVaSetValues(pathText,
                 XmNeditable, False,
                 XmNcursorPositionVisible, False,
                 XmNshadowThickness, 0,
                 XmNmarginHeight, 5,
                 NULL);

   no_recursion = False;
}

/***********************************************************************
 *                                                                     *
 *   Event handler for the pressing of the 1st mouse button in the     *
 *   pathText widget.                                                  *
 *                                                                     *
 ***********************************************************************/
static void change_path(w, browser, cbs)
       Widget w, browser;
       XmTextVerifyCallbackStruct *cbs;
{
   XmTextPosition  pos;
   int             end_pos, zero, all, x, y, width, height, list;
   char           *path, *prev, *s, *s2, *class, *lun;
   char           *selected_path;
   Boolean         selected;
   XmString        xmstr;
   int             is_vms_dir = 0;
   int             slash = '/';

   if (no_recursion || path_edit) return;

   /* get the click position */
   pos = cbs->newInsert;

   if (pos <= 0) return;

   /* set the context */
   get_fb_context(browser);

   path = get_path();

   /* check if it is not a VMS directory name "disk:[dir]" */
   if( path[strlen(path)-1] == ']' ) {
     is_vms_dir = 1;
     slash = '.';
   }

   /* special case for path's starting with // (i.e. RZ files) */
   if (!strncmp(path, "//", 2) && pos <= 2)
      pos = 3;

   /* search from the click position to the first / */
   s = strchr(path+(pos-1), slash);

   if (s) {
      end_pos = s - path;
      if (end_pos == 0) end_pos = 1;
   } else
      end_pos = strlen(path);

   /* if clicked twice in the same path segment change path */
   if (end_pos == hili_right) {

      /* just rescan when full path selected */
      if (end_pos == strlen(path)) {

         XmTextSetHighlight(pathText, hili_left, hili_right,
                            XmHIGHLIGHT_NORMAL);
         hili_left  = 0;
         hili_right = 0;
         save_fb_context(browser);  /* make sure the above values are saved */

         km_set_cursor (browser, km_wait_cursor);
         zero = 0;
         all  = 1;
         list = DIR_LIST;
         list_delete_item(&list, NULL, &zero, &all, &zero, &zero);
         selected_path = (char *) XtMalloc (MAX_string+1);
         strcpy (selected_path, browser_dir);
         do_list_content (selected_path, False);
         list = DIR_LIST;
         list_refresh(&list, &zero);
         XtFree (selected_path);
         km_set_cursor (browser, km_main_cursor);

      } else {
         /* new path selected: execute action for "change directory" */
         XmString     *xfsel;
         int           fitem, fcnt;
         BrObject     *brobj;
         BrClass      *brcls;
         BrAction     *bract;

         /* Store new value for "browser_dir" and update path */
         strncpy(browser_dir, path, end_pos);
         if( is_vms_dir ) {
           browser_dir[end_pos] = ']';
           browser_dir[end_pos+1] = '\0';
         }
         else {
           browser_dir[end_pos] = '\0';
         }

         set_path(browser_dir);
         /* make sure the values changed in set_path are saved */
         save_fb_context(browser);

         s2 = strchr(s+1, slash);

         if (s2) {
            prev = XtCalloc(s2-s, 1);
            strncpy(prev, s+1, s2-s-1);
         } else {
            prev = XtCalloc(strlen(path)-end_pos, 1);
            strcpy(prev, s+1);
         }

         km_set_cursor (browser, km_wait_cursor);
         zero = 0;
         all  = 1;
         list = DIR_LIST;
         list_delete_item(&list, NULL, &zero, &all, &zero, &zero);
         selected_path = (char *) XtMalloc (MAX_string+1);
         strcpy (selected_path, browser_dir);
         do_list_content (selected_path, False);

         xmstr = XmStringCreate(prev, XmSTRING_DEFAULT_CHARSET);
         if (IconBoxGetItemAttr(dirList, xmstr, &x, &y, &width, &height,
                                &class, &selected))
            IconBoxZoom(dirList, x, y, width, height, False);
         list = DIR_LIST;
         list_refresh(&list, &zero);

         /* Get selected class (lun) for this browser */
         XtVaGetValues(fileList, XmNitemCount, &fitem,
                       XmNselectedItemCount,   &fcnt,
                       XmNselectedItems,       &xfsel, NULL);
         /* Execute default action for this selected class */
         if (fcnt) {
             XmStringGetLtoR(xfsel[0], XmSTRING_DEFAULT_CHARSET, &lun);
             /* Get object structure for this lun */
             if ( (brobj = find_brobject( lun )) != NULL ) {
                 brcls = brobj->class;
                 bract = brcls->root;
                 /* execute the class window default action for lun */
                 current_callback_data = NULL;
                 exec_action (bract, lun, NULL, False, NULL, NULL);

                 /* Update all browsers */
                 if (bract->flags & BrActUpdate)
                    update_browser(NULL, FILE_LIST, False);
             }
             XtFree (lun);
         }

         km_set_cursor (browser, km_main_cursor);
         XtFree (selected_path);
         XtFree(prev);
         XtFree(class);
         XmStringFree(xmstr);
      }
   } else {
      /* if clicked in new path segment, just highlight the segment */
      XmTextSetHighlight(pathText, hili_left, hili_right, XmHIGHLIGHT_NORMAL);
      hili_right = end_pos;
      XmTextSetHighlight(pathText, hili_left, hili_right, XmHIGHLIGHT_SELECTED);
   }

   check_file_selection();

   save_fb_context(browser);

   XtFree(path);
}


/***********************************************************************
 *                                                                     *
 *   Event handler for the pressing of the 3rd mouse button            *
 *   in the class window.                                              *
 *                                                                     *
 ***********************************************************************/
static void post_class_menu(w, browser, event)
       Widget w, browser;
       XButtonPressedEvent *event;
{
   Widget        wtmp;
   MenuItem      filemenu_items[MAXITEMS];
   char          *selected_path;
   XmString      *xfsel;
   int           fitem, fcnt;
   int i;
   BrObject *brobj;
   BrClass  *brcls;
   BrAction *bract;
   char title[MAX_string];

   static poptag_str ptag[MAXITEMS];
   unsigned int state;


   /* only handle 3rd mouse button */
   if (event->button != 3) return;
   state = event->state;


   get_fb_context(browser);

   /* Get selected_path for this browser */
   XtVaGetValues(fileList, XmNitemCount, &fitem,
                 XmNselectedItemCount,   &fcnt,
                 XmNselectedItems,       &xfsel, NULL);
   if (!fcnt)
      return;

   XmStringGetLtoR(xfsel[0], XmSTRING_DEFAULT_CHARSET, &selected_path);

   /* Get Browser class according to the selection : selected_path */
   if( (brobj = find_brobject( selected_path )) == NULL ) {
     XtFree (selected_path);
     return;
   }
   brcls = brobj->class;
   bract = brcls->root;
   /* Build the "class window" popup menu : */
   for( i = 0; bract != NULL; bract = bract->next, i++ ) {
     if (i > MAXITEMS) break;
     ptag[i].command_num = i+1;
     ptag[i].flag = 1;
     ptag[i].browser = browser;
     ptag[i].panel = (state == ControlMask);
     ptag[i].bract = bract;
     ptag[i].bract_next = bract->next;
     ptag[i].kmobj = NULL;
     strcpy (ptag[i].pathname, selected_path);
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
   }
   /* Add auromatically a "Help" item at the end of the menu */
   filemenu_items[i].accelerator    = NULL;
   filemenu_items[i].accel_text     = NULL;
   filemenu_items[i].callback       = km_do_help;
   filemenu_items[i].callback_data  = str2dup("HELP_", selected_path) ;
   filemenu_items[i].subitems       = NULL;
   filemenu_items[i].set            = False;
   filemenu_items[i].class = &xmPushButtonGadgetClass;
   filemenu_items[i].label = strdup("Help");
   filemenu_items[i].mnemonic = 'H';
   filemenu_items[i].separator = True;
   i++;
   filemenu_items[i].label = NULL;
   filemenu_items[0].default_button = True;

   /* create menu title */
   if( brcls->title != NULL )
     sprintf( title, "%s %s", brcls->title, brobj->name );
   else
     sprintf( title, "%s", brobj->name );

   km_reset_class_popup();
   class_popup = km_build_popup (fileList, title, filemenu_items,
                                 XmMENU_POPUP, &wtmp);


   /*
    * position the menu at the location of the button press!  If we wanted
    * to position it elsewhere, we could change the x,y fields of the
    * event structure.
    */
   XmMenuPosition(class_popup, event);
   XtManageChild(class_popup);
   XtFree (selected_path);
}

/***********************************************************************
 *                                                                     *
 *   Destroy menu defined in the class window (class_popup).           *
 *                                                                     *
 ***********************************************************************/
void km_reset_class_popup()
{
    if (class_popup != NULL) XtDestroyWidget (XtParent(class_popup));
    class_popup = NULL;
}

/***********************************************************************
 *                                                                     *
 *   Event handler for the pressing of the 3rd mouse button            *
 *   in the object window.                                             *
 *                                                                     *
 ***********************************************************************/
static void post_object_menu(w, browser, event)
       Widget w, browser;
       XButtonPressedEvent *event;
{
   Widget        wtmp;
   MenuItem      filemenu_items[MAXITEMS];
   int           i, npos, *pos;
   char          *selected_path;
   XmString      *xfsel;
   int           fitem, fcnt;

   static poptag_str ptag[MAXITEMS];
   static KmObject   tag_kmobj;
   unsigned int state;
   KmObject *kmobj;
   KmClass  *kmcls;
   BrAction *bract;
   char title[MAX_string];


   /* only handle 3rd mouse button */
   if (event->button != 3) return;
   state = event->state;


   get_fb_context(browser);

   /* Get selected_path for this browser */
   XtVaGetValues(fileList, XmNitemCount, &fitem,
                 XmNselectedItemCount,   &fcnt,
                 XmNselectedItems,       &xfsel, NULL);
   if (!fcnt)
      return;

   XmStringGetLtoR(xfsel[0], XmSTRING_DEFAULT_CHARSET, &selected_path);

   /* Get selected_item for this browser */
   IconBoxGetSelectedPos (dirList, &pos, &npos);
   if (npos != 1) {
     XtFree((char *)pos);
     XtFree (selected_path);
     return;
   }
   kmobj = kmobjects;
   for( i = pos[npos-1] - 1; kmobj != NULL && i > 0; i-- )
     kmobj = kmobj->next;
   XtFree((char *)pos);
   if( kmobj == NULL ) return;

   /* save kmobj in local copy */
   tag_kmobj.next = NULL;
   if (tag_kmobj.name) free(tag_kmobj.name);
   if (kmobj->name) tag_kmobj.name = strdup(kmobj->name);
   if (tag_kmobj.stext) free(tag_kmobj.stext);
   if (kmobj->stext) tag_kmobj.stext = strdup(kmobj->stext);
   else tag_kmobj.stext = NULL;
   if (tag_kmobj.ltext) free(tag_kmobj.ltext);
   if (kmobj->ltext) tag_kmobj.ltext = strdup(kmobj->ltext);
   else tag_kmobj.ltext = NULL;
   tag_kmobj.class = kmobj->class;

   /* Get item class according to the selection */
   kmcls = kmobj->class;
   bract = kmcls->cont;
   /* Build object window popup menu according to this class */
   if( kmcls->title != NULL ) {
     /* create menu title */
   }
   for( i = 0; bract != NULL; bract = bract->next, i++ ) {
     if (i > MAXITEMS) break;
     ptag[i].command_num = i+1;
     ptag[i].flag = 2;
     ptag[i].browser = browser;
     ptag[i].panel = (state == ControlMask);
     ptag[i].bract = bract;
     ptag[i].bract_next = bract->next;
     ptag[i].kmobj = &tag_kmobj;
     strcpy (ptag[i].pathname, selected_path);
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
   }
   filemenu_items[i].label = NULL;
   filemenu_items[0].default_button = True;

   sprintf( title, "%s %s", kmcls->title, kmobj->name );
   km_reset_object_popup();
   object_popup = km_build_popup (dirList, title, filemenu_items,
                                  XmMENU_POPUP, &wtmp);

   /*
    * position the menu at the location of the button press!  If we wanted
    * to position it elsewhere, we could change the x,y fields of the
    * event structure.
    */
   XmMenuPosition(object_popup, event);
   XtManageChild(object_popup);
   XtFree (selected_path);
}

/***********************************************************************
 *                                                                     *
 *   Destroy menu defined in the object window (object_popup).         *
 *                                                                     *
 ***********************************************************************/
void km_reset_object_popup()
{
    if (object_popup != NULL) XtDestroyWidget (XtParent(object_popup));
    object_popup = NULL;
}

#if UNREF
/***********************************************************************
 *                                                                     *
 *   Add an item to the Class Window                                   *
 *   FORTRAN callable                                                  *
 *                                                                     *
 *   If all_dir<>0 then add the item to all lists in all file          *
 *   browsers.                                                         *
 *                                                                     *
 ***********************************************************************/
static C_DECL_3(void list_addp_item,
   char *, item,
   int, pos,
   Boolean, all_dir)
{
   XmString   xmstr;
   int        i;

   if (!km_toplevel)  return;
   if (!n_fbrowser) return;

   xmstr = XmStringCreate(item, XmSTRING_DEFAULT_CHARSET);

   if (all_dir) {
       for (i = 0; i < MAX_FILE_BROWSER; i++) {
          if (fbrowser[i].widget) {
              get_fb_context(fbrowser[i].widget);
              XmListAddItem(fileList, xmstr, pos); }
          }
       } else
          XmListAddItem(fileList, xmstr, pos);

   XmStringFree(xmstr);
}
#endif

/***********************************************************************
 *                                                                     *
 *   Add an item to the Object Window                                  *
 *   FORTRAN callable                                                  *
 *                                                                     *
 *   If all_dir<>0 then add the item to all lists in all file          *
 *   browsers.                                                         *
 *                                                                     *
 ***********************************************************************/
static C_DECL_5(void list_addc_item,
     KmObject *, kmobj,
     int, ltype,
     int, pos,
     Boolean, refresh,
     Boolean, all_dir)
{
   XmString   xmstr;
   Boolean    pixmap;
   char      *title;
   int        i;

   KmIcon   *kmicon_big, *kmicon_sml;


   if (!km_toplevel) return;
   if (!n_fbrowser)  return;

   title = XtCalloc(MAX_stringl, 1);

   if (ltype == 0) {
      /* Big icons */
      strcpy( title, kmobj->name );
   } else if (ltype == 1) {
      /* Small icons */
      strcpy( title, kmobj->name );
   } else if (ltype == 2) {
      /* no icons */
      sprintf (title, "%s", kmobj->name );
      if( kmobj->stext != NULL ) {
          strcat( title, "  " );
          strcat( title, kmobj->stext );
      }
   } else if (ltype == 3) {
      /* Titles */
      sprintf (title, "%-10s%-8s",
             kmobj->name, (kmobj->stext != NULL) ? kmobj->stext : "" );
      if ( kmobj->ltext != NULL ) {
          strcat( title, " - " );
          strcat( title, kmobj->ltext );
      }
   }
   xmstr = XmStringCreate(title, XmSTRING_DEFAULT_CHARSET);

   pixmap = True;

   /* Retrieve bitmap data (corresponding to this class) from the CDF */
   /* Create big pixmaps (first time only) */
      if ((kmicon_big = kmobj->class->bicon) == NULL)
           kmicon_big = kmobj->class->bicon = find_kmicon(
                                                   kmobj->class->big_icon);

   /* Create small pixmaps (first time only) */
   if ((kmicon_sml = kmobj->class->sicon) == NULL)
        kmicon_sml = kmobj->class->sicon = find_kmicon(
                                                   kmobj->class->sm_icon);

   if (kmicon_big == NULL && kmicon_sml == NULL) {
       pixmap = False;
   } else {
       if (kmicon_big == NULL) kmicon_big = kmicon_sml;
       if (kmicon_sml == NULL) kmicon_sml = kmicon_big;
   }


   if (all_dir) {
       for (i = 0; i < MAX_FILE_BROWSER; i++) {
            if (fbrowser[i].widget) {
                get_fb_context(fbrowser[i].widget);
                if (pixmap) {
                    IconBoxAddItemBitmapData (dirList, xmstr,
                                  kmobj->class->name,
                                  kmicon_big->name,
                                  kmicon_big->bitmap,
                                  kmicon_big->width, kmicon_big->height,
                                  kmicon_sml->name,
                                  kmicon_sml->bitmap,
                                  kmicon_sml->width, kmicon_sml->height,
                                  pos, refresh);
                } else {
                    IconBoxAddItem(dirList, xmstr, kmobj->class->name,
                                  pos, refresh);
                }
            }
       }
   } else {
       if (pixmap) {
           IconBoxAddItemBitmapData (dirList, xmstr,
                                  kmobj->class->name,
                                  kmicon_big->name,
                                  kmicon_big->bitmap,
                                  kmicon_big->width, kmicon_big->height,
                                  kmicon_sml->name,
                                  kmicon_sml->bitmap,
                                  kmicon_sml->width, kmicon_sml->height,
                                  pos, refresh);
       } else {
           IconBoxAddItem(dirList, xmstr, kmobj->class->name,
                          pos, refresh);
       }
   }

   XmStringFree(xmstr);
   XtFree(title);
}


/***********************************************************************
 *                                                                     *
 *   Delete one or all items from one of the two lists (file or dir    *
 *   list).                                                            *
 *   If a selected item is deleted from the file list then delete also *
 *   all the items from the dir list. If all1 is <>0 then delete all   *
 *   items. If refresh=0 then one needs to call list_delete_item at    *
 *   least once more with refresh<>0 or call list_refresh. If          *
 *   all_dir<>0 then delete item from all file browsers.               *
 *                                                                     *
 ***********************************************************************/
static void list_delete_item(lst, item, litem, all1, refresh, all_dir)
   int     *lst;
   char    *item;
   int     *litem;
   int     *all1;
   int     *refresh;
   int     *all_dir;
{
}


/***********************************************************************
 *                                                                     *
 *   Refresh the lists                                                 *
 *                                                                     *
 ***********************************************************************/
static void list_refresh(list, all_dir)
     int *list, *all_dir;
{
   Boolean    all_brow;
   int        i;


   if (!km_toplevel) return;
   if (!n_fbrowser)  return;

   all_brow = (Boolean) *all_dir;

   if (*list == DIR_LIST) {
      if (all_brow) {
         for (i = 0; i < MAX_FILE_BROWSER; i++) {
            if (fbrowser[i].widget) {
                get_fb_context(fbrowser[i].widget);
                IconBoxRefresh(dirList);
            }
         }
      } else
         IconBoxRefresh(dirList);
   } else if (*list == FILE_LIST) {
      if (all_brow) {
         for (i = 0; i < MAX_FILE_BROWSER; i++) {
            if (fbrowser[i].widget) {
                get_fb_context(fbrowser[i].widget);
                file_list_refresh (fileList);
                save_fb_context(fbrowser[i].widget);
            }
         }
      } else
         file_list_refresh (fileList);
   }

}

/***********************************************************************
 *                                                                     *
 *   Refresh File_list (class window)                                  *
 *                                                                     *
 ***********************************************************************/
static void file_list_refresh(widget)
     Widget widget;
{
   BrObject  *brobj;
   int        fcnt;
   XmString  *xfsel;
   char      *fsel = NULL;
   Boolean    found = False;

   XtVaGetValues(fileList, XmNselectedItemCount, &fcnt,
                           XmNselectedItems,     &xfsel, NULL);
   if (fcnt) fsel = km_normal_string(xfsel[0]);

   XmListSetPos(widget, 1);
   XmListDeselectAllItems(widget);
   XmListDeleteAllItems (widget);

   /* Get list of root pathname and update the list widget */
   for( brobj = scan_brobjects(); brobj != NULL; brobj = brobj->next ) {
     XmString xmstr = XmStringCreate( brobj->name, XmSTRING_DEFAULT_CHARSET );
     XmListAddItem( widget, xmstr, 0 );
     if (fsel != NULL && !strcmp(fsel, brobj->name)) {
        XmListSelectItem(widget, xmstr, False);
        found = True;
     }
     XmStringFree( xmstr );
   }

   /* Set pre-defined browser (user has to call km_browser_set) */
   browser_init ();

   if (!found && init_br != 0) {
      IconBoxDeleteAllItems(dirList, True);
      browser_dir[0] = '\0';
      set_path("");
   }
   if (fsel != NULL) XtFree(fsel);

   init_br = 1;
}

static void browser_init()
{
   if( br_init != NULL && init_br == 0 ) {
       XmString xmsel;
       char  path_n[MAX_string];
       char *path, *rpath;

       xmsel = XmStringCreate(br_init, XmSTRING_DEFAULT_CHARSET);
       XmListSelectItem(fileList, xmsel, True);

       /* Store new value of "browser_dir" */
       if (path_init != NULL) {
           path = path_init;
       } else {
           sprintf (path_n, "path_%d", current_browser);
           rpath = get_variable( br_init, "root");
           if ( (path = get_variable( br_init, path_n )) == NULL) {
                path = rpath; }
           if( path == NULL || path[0] == '\0' )
               path = "/";
       }
       strcpy(browser_dir, path);
       set_path(browser_dir);

       XmStringFree(xmsel);
       br_init = NULL;
       free (br_init);
   }
}


/***********************************************************************
 *                                                                     *
 *   Callback for the file list                                        *
 *                                                                     *
 *   (Execution when clicking on one item in the class window)         *
 *                                                                     *
 ***********************************************************************/
static void do_file_list(w, browser, call_data)
       Widget w, browser;
       XmListCallbackStruct *call_data;
{
      char *lun;

      get_fb_context(browser);
      lun = km_normal_string(call_data->item);

   switch (call_data->reason) {
      case XmCR_DEFAULT_ACTION:

         break;

      case XmCR_SINGLE_SELECT:
      case XmCR_BROWSE_SELECT:

         km_set_cursor (browser, km_wait_cursor);

         set_browser (lun);
         current_callback_data = (XmAnyCallbackStruct *)call_data;

         km_set_cursor (browser, km_main_cursor);

         break;
      }

      XtFree(lun);
      save_fb_context(browser);
}

void km_browser_set ( const char *file, const char *path )
{
      BrObject *brobj;

      /* Take right value in the list (case sensitive) */
      br_init = strdup (file);
      for( brobj = scan_brobjects(); brobj != NULL; brobj = brobj->next ) {
           if ( strcasecmp( file, brobj->name ) == 0 )
                br_init = strdup (brobj->name);
      }

      if (path != NULL) path_init = strdup (path);
}

void F77_ENTRY_CC(Kmbrset,chfil,chpath)
/* { */
     char *file = fstr0dup( chfil, len_chfil);
     char *path = fstr0dup( chfil, len_chpath);
     km_browser_set( file, path );
     if (file != NULL) free( file );
     if (path != NULL) free( path );
}

static void set_browser ( file )
       char *file;
{
      char ff[512];
      char *value, *vf;
      XmString xmfil, xmstr;
      BrObject *brobj;

      int      ic = current_browser;

      /* Check if item is in the list */
      vf = strdup(file);
      for( brobj = scan_brobjects(); brobj != NULL; brobj = brobj->next ) {
           if ( strcasecmp( file, brobj->name ) == 0 )
                vf = strdup(brobj->name);
      }

      get_fb_context(fbrowser[ic].widget);
      xmfil = XmStringCreate(vf, XmSTRING_DEFAULT_CHARSET);
      if ( !XmListItemExists( fileList, xmfil ) ) {
         printf ("set_browser : cannot find %s in list of browsables\n", file);
         XmStringFree(xmfil);
         return; }
      XmStringFree(xmfil);

      /* Get filename corresponding to path selected */
      strcpy( ff, "File: " );
      if( (value = get_variable( vf, "file" )) != NULL )
            strcat( ff, value );
      xmstr = XmStringCreate(ff, XmSTRING_DEFAULT_CHARSET);
      /* Print fileNameLabel in the labelString area */
      XtVaSetValues(fileNameLabel, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);

      file_execute (vf);
      free (vf);
}

/*
 * action routine for /MOTIF/BROWSER
 */
int kxbrset()
{
  char *file = ku_gets();
  int npar = ku_npar();

  char *vf = NULL;
  XmString xmsel;
  BrObject *brobj;

  get_fb_context(last_browser);

  if (file == NULL) return 0;

  for( brobj = scan_brobjects(); brobj != NULL; brobj = brobj->next ) {
       if ( strcasecmp( file, brobj->name ) == 0 ) vf = strdup (brobj->name);
  }

  if (vf == NULL) {
      printf ("kxbrset : cannot find %s in list of browsables\n", file);
      return 0;
  }

  xmsel = XmStringCreate(vf, XmSTRING_DEFAULT_CHARSET);
  XmListSelectItem(fileList, xmsel, True);
  XmStringFree(xmsel);

  if (npar > 1) {
      extern Boolean  km_browser_refresh;
      char *path = ku_gets();

      if (path == NULL) {
          free (vf);
          return 0;
      } else {
          char *full_path = fexpand(path, NULL);
          strcpy(browser_dir, full_path);
          set_path(browser_dir);
          km_browser_refresh = True;
          browser_focus = False;
          enter_browser(NULL, current_browser, NULL);
          free (full_path);
      }
  } /* end ... if (npar > 1) */

  free (vf);
  return 0;
}



/***********************************************************************
 *                                                                     *
 *   Execute default action (for list) when clicking on one item in    *
 *   the class window.                                                 *
 *                                                                     *
 ***********************************************************************/
void file_execute (lun)
   char *lun;
{
   char *path, *rpath;
   char  path_n[MAX_string];

   BrObject *brobj;
   BrClass  *brcls;
   BrAction *bract;

   /* Store new value of "browser_dir" */
   sprintf (path_n, "path_%d", current_browser);
   rpath = get_variable( lun, "root");
   if ( (path = get_variable( lun, path_n )) == NULL) {
         path = rpath; }
   if( path == NULL || path[0] == '\0' )
       path = "/";

   strcpy(browser_dir, path);
   set_path(browser_dir);

  /* Update object window with the list of objects for this path */
  do_list_content(browser_dir, True);

  /* execute default action (first command) */
  if (init_br == 0) return;
  if ( (brobj = find_brobject( lun )) == NULL )
       return;
  brcls = brobj->class;
  bract = brcls->root;

  exec_action (bract, lun, NULL, False, NULL, NULL);

  /* Update all browsers */
  if (bract->flags & BrActUpdate)
{
     update_browser(NULL, FILE_LIST, False);
}

}

/***********************************************************************
 *                                                                     *
 *   Get data (value, class, title) on item selected in object window  *
 *                                                                     *
 ***********************************************************************/
static KmObject *get_dir_data (browser)
       Widget browser;
{
   int       i, npos, *pos;
   KmObject *kmobj;

   get_fb_context(browser);

   IconBoxGetSelectedPos (dirList, &pos, &npos);
   if (npos != 1) {
     XtFree((char *)pos);
     return(NULL);
   }
   kmobj = kmobjects;
   for( i = pos[npos-1] - 1; kmobj != NULL && i > 0; i-- )
     kmobj = kmobj->next;
   XtFree((char *)pos);
   if( kmobj == NULL ) return(NULL);

   /* Set value of "dirNameLabel" (bottom right) */
   if (kmobj->name != NULL) {
       XmString  xmstr;
       char      str[512];

       if (kmobj->ltext != NULL)
           sprintf(str, "%s: %s",kmobj->name, kmobj->ltext);
       else
           sprintf(str, "%s",kmobj->name);
       xmstr = XmStringCreate(str, XmSTRING_DEFAULT_CHARSET);
       XtVaSetValues(dirNameLabel, XmNlabelString, xmstr, NULL);
       XmStringFree(xmstr);
   }

   return (kmobj);
}

/***********************************************************************
 *                                                                     *
 *   Callback for the dir list                                         *
 *                                                                     *
 *   (Execution when clicking on one item in the object window)        *
 *                                                                     *
 ***********************************************************************/
static void do_dir_list(w, browser, call_data)
       Widget w, browser;
       IconBoxCallbackStruct *call_data;
{
   XmString  *xfsel;
   int       fitem, fcnt;
   char      *lun; /* name in the root window, e.g. PAWC */

   KmObject *kmobj;

   get_fb_context(browser);

   /* Get selected_path (lun) for this browser */
   XtVaGetValues(fileList, XmNitemCount, &fitem,
                 XmNselectedItemCount,   &fcnt,
                 XmNselectedItems,       &xfsel, NULL);
   if (!fcnt) return;
   XmStringGetLtoR(xfsel[0], XmSTRING_DEFAULT_CHARSET, &lun);

   switch (call_data->reason) {
      case XmCR_SINGLE_SELECT:
      case XmCR_MULTIPLE_SELECT:
      case XmCR_EXTENDED_SELECT:

           get_dir_data (browser);
           break;


      case XmCR_DEFAULT_ACTION:

           km_set_cursor (browser, km_wait_cursor);

           kmobj = get_dir_data (browser);

           current_callback_data = (XmAnyCallbackStruct *)call_data;

           if (kmobj->class->is_dir) {
           /* This is a directory ... */
              /* execute default action for directory */
              dir_execute (browser, kmobj, lun);
           } else {
              /* execute default action (first command) */
              km_get_curpos (call_data->event, &km_xcur_pos, &km_ycur_pos);
              exec_action (kmobj->class->cont, lun, kmobj->name, False,
                           w, call_data);
              /* Update all browsers */
              if (kmobj->class->cont->flags & BrActUpdate)
                 update_browser (NULL, 0, False);
           }
           km_set_cursor (browser, km_main_cursor);
           break;
   }

   XtFree(lun);
   save_fb_context(browser);
}


/***********************************************************************
 *                                                                     *
 *   Execute default action and make zoom effect                       *
 *   when clicking on one directory in the object window               *
 *                                                                     *
 ***********************************************************************/
void dir_execute (browser, kmobj, lun)
   Widget browser;
   KmObject *kmobj;
   char *lun;
{
   XmString  xmstr;
   char     *path, *selected_path, *class, *dirname, *title;
   Boolean   selected;
   int       list, zero, all, x, y, width, height;
   BrAction *brasav;

   get_fb_context(browser);

   /* Store new value of "browser_dir" */
   path = get_path();
   strcpy( browser_dir, path );
   XtFree (path);

   if( strcmp( kmobj->name, ".." ) == 0 ) {
     /* go up one level in directory hierarchy */
     if( browser_dir[1] != '\0' ) {
       char *p = strrchr( browser_dir, '/' );
       if( p != NULL ) {
         if( p == browser_dir )
           p[1] = '\0';
         else
           p[0] = '\0';
       }
     }
   }
   else if( strcmp( kmobj->name, "[-]" ) == 0 ) {
     /* VMS */
     char *p = strrchr( browser_dir, '.' );
     if( p != NULL )
       strcpy( p, "]" );
   }
   else if( strncmp( kmobj->name, "[.", 2 ) == 0 ) {
     /* VMS */
     char *p = strrchr( browser_dir, ']' );
     if( p != NULL )
       strcpy( p, kmobj->name + 1 );
   }
   else {
     if( browser_dir[1] != '\0' || browser_dir[0] == '~')
       strcat( browser_dir, "/" );
     strcat( browser_dir, kmobj->name );
   }

   /* save new browser_dir value */
   save_fb_context(browser);

   /* Set new path with "browser_dir" */
   selected_path = str0dup (browser_dir);
   set_path(selected_path);

   /* delete previous contents and fill directory list again */
   zero = 0;
   all  = 1;
   list = DIR_LIST;

   /* delete the items from the iconbox */
   list_delete_item(&list, NULL, &zero, &all, &zero, &zero);

   /*
    * store the items from the new directory in the iconbox,
    * but do not yet update the screen
    */
   brasav  = kmobj->class->cont;
   dirname = str0dup (kmobj->name);

   title = XtCalloc(MAX_stringl, 1);

   if (titles == 0 || titles == 1) {
      /* big and small icons */
      strcpy(title, kmobj->name);
   } else if (titles == 2) {
      /* no icons */
      sprintf (title, "%s", kmobj->name );
      if ( kmobj->stext != NULL ) {
          strcat( title, "  " );
          strcat( title, kmobj->stext );
      }
   } else if (titles == 3) {
      /* Titles */
      sprintf (title, "%-10s%-8s",
             kmobj->name, (kmobj->stext != NULL) ? kmobj->stext : "" );
      if ( kmobj->ltext ) {
          strcat( title, " - " );
          strcat( title, kmobj->ltext );
      }
   }

   /* make zoom effect and update the screen */
   xmstr = XmStringCreate(title, XmSTRING_DEFAULT_CHARSET);
   if (IconBoxGetItemAttr(dirList, xmstr,
                          &x, &y, &width, &height,
                          &class, &selected)) {
      do_list_content (selected_path, False);
      IconBoxZoom(dirList, x, y, width, height, True);
      IconBoxDeselectAllItems(dirList, False);
      list = DIR_LIST;
      list_refresh(&list, &zero);
   }

   free(selected_path);
   XtFree(class);
   XtFree(title);
   XmStringFree(xmstr);

   /* execute default action (first command) */
   exec_action (brasav, lun, dirname, False, NULL, NULL);

   /*
    * Update all browsers
    * Here we assume that the default action does not add new items
    * in the just refreshed object window, only in the class window
    * (i.e. FILE_LIST). Best would be if there would be a separate
    * flag for class and object window refreshes.
    */
   if (brasav->flags & BrActUpdate)
      update_browser (NULL, FILE_LIST, False);

   free(dirname);

   save_fb_context(browser);
}


/***********************************************************************
 *                                                                     *
 *   do_list_content (path, refresh)                                   *
 *                                                                     *
 *           char *path (input)  :  complete directory pathname        *
 *                                  (e.g. //PAWC/LUN20/DIR1)           *
 *           Boolean refresh (input) : refresh the iconbox             *
 *                                                                     *
 *   Default action for do_file_list                                   *
 *                                                                     *
 *   List of objects should be obtained by calling the "next_object"   *
 *   routine corresponding to the class of the directory pathname      *
 *   (HBOOK, Picture, ZEBRA file, ...)                                 *
 *                                                                     *
 ***********************************************************************/
static C_DECL_2(void do_list_content,
     char *, path,
     Boolean, refresh)
{
     char *pathname;
     XmString   xmstr;

     int      ic = current_browser;
     KmObject *kmobj;
     XmString *xfsel;
     int       fitem, fcnt;
     char     *selected_path; /* name in the root window, e.g. PAWC */


     /* make a local copy of path (which can be overwritten by browser_dir) */
     pathname = (char *) XtMalloc(strlen(path)+1);
     strcpy (pathname, path);

     get_fb_context(fbrowser[ic].widget);
     /* get selected item in the parent's file list */
     XtVaGetValues(fileList, XmNitemCount, &fitem,
                             XmNselectedItemCount,   &fcnt,
                             XmNselectedItems,       &xfsel, NULL);
     if (!fcnt) {
         XtFree (pathname);
         return;
     }
     XmStringGetLtoR(xfsel[0], XmSTRING_DEFAULT_CHARSET, &selected_path);

     if (!get_variable(selected_path, "root") && !strcmp(pathname, "/"))
        pathname[0] = '\0';

     if (strlen(pathname) == 0)
        strcpy(browser_dir, "/");
     else
        strcpy(browser_dir, pathname);

     IconBoxDeleteAllItems(dirList, False);

     strcpy (br_filter,  fbrowser[ic].filter);
     for( kmobj = scan_kmobjects( selected_path, pathname, ic);
       kmobj != NULL; kmobj = kmobj->next ) {
       int  i1 = 0;
       int  i2 = 0;
       int  i3 = 0;

       /* Add item */
       list_addc_item(kmobj,fbrowser[ic].titles,i1,i2,i3);
     }  /* end for( kmobj = scan_kmobjects ... */

     /* Set value of counterLabel */
     xmstr = XmStringCreate( get_variable( selected_path, "summary" ),
                            XmSTRING_DEFAULT_CHARSET);
     XtVaSetValues(counterLabel, XmNlabelString, xmstr, NULL);
     XmStringFree(xmstr);

     if (refresh) IconBoxRefresh(dirList);

     save_fb_context(fbrowser[ic].widget);
     XtFree (pathname);
     XtFree (selected_path);
}


/***********************************************************************
 *                                                                     *
 *   Install callbacks file browser menu's.                            *
 *                                                                     *
 ***********************************************************************/
static void init_fb_menu(browser)
     Widget browser;
{
   int narg;
   Arg arglist[15];
   XmString mgXmStrings[1];
   Widget FilePdMenu_button, FilePdMenu_sep;
   int icon_type;

   BrClass *brcls;

   Boolean open_button = False;

   /*
    *     Set up list of buttons in OPEN menu
    *     and install callbacks on OPEN men buttons
    */
   /* Get the list of "Open statements" for all "browsers" */
   for( brcls = brclasses; brcls != NULL; brcls = brcls->next ) {
        BrAction *bract;
        for( bract = brcls->open; bract != NULL; bract = bract->next ) {
             otag_str *otag = (otag_str*)malloc(sizeof(otag_str));

             narg = 0;
             mgXmStrings[0] =
               XmStringCreateLtoR( bract->text, XmSTRING_DEFAULT_CHARSET);
             XtSetArg(arglist[narg], XmNlabelString, mgXmStrings[0]); narg++;
             if (bract->flags & BrActSeparator) {
                 FilePdMenu_sep = XmCreateSeparatorGadget
                                  (FilePdMenu,"FilePdMenu_sep",arglist,0);
                 XtManageChild(FilePdMenu_sep); }
             FilePdMenu_button = XmCreatePushButtonGadget
                                (FilePdMenu,"FilePdMenu_button",arglist,narg);
             open_button = True;
             otag->browser = browser;
             otag->bract = bract;
             XtAddCallback(FilePdMenu_button, XmNactivateCallback,
                           (XtCallbackProc)FilePdMenu_o_activate,otag );
             XtManageChild(FilePdMenu_button );
             XmStringFree(mgXmStrings[0]);
        }
   }

   /*
    *     add "Exit" button at the end of "File" menu (to exit application)
    */
    if (open_button)
        XtVaCreateManagedWidget( "FilePdMenu_sep",
                             xmSeparatorGadgetClass, FilePdMenu, NULL );
    FilePdMenu_button = XtVaCreateManagedWidget( "FilePdMenu_button",
                                xmPushButtonGadgetClass, FilePdMenu,
                                RES_CONVERT( XmNmnemonic, "E" ),
                                RES_CONVERT( XmNlabelString, "Exit" ),
                                NULL );
    XtAddCallback(FilePdMenu_button, XmNactivateCallback,
                           (XtCallbackProc)FilePdMenu_exit, NULL );


   /*
    *     install callbacks on VIEW men buttons
    */
   XtAddCallback(ViewPdMenu_icon, XmNvalueChangedCallback,
          (XtCallbackProc)change_icon_type, (XtPointer)IconBoxBIG_ICON);
   XtAddCallback(ViewPdMenu_sicon, XmNvalueChangedCallback,
          (XtCallbackProc)change_icon_type, (XtPointer)IconBoxSMALL_ICON);
   XtAddCallback(ViewPdMenu_nicon, XmNvalueChangedCallback,
          (XtCallbackProc)change_icon_type, (XtPointer)IconBoxNO_ICON);
   XtAddCallback(ViewPdMenu_title, XmNvalueChangedCallback,
          (XtCallbackProc)change_icon_type, (XtPointer)IconBoxTITLES);
#ifdef ALL_CYCLE /* vms */
   km_all_cycles = 0;
   XtAddCallback(ViewPdMenu_allcycle,  XmNvalueChangedCallback,
          (XtCallbackProc)change_cycle, NULL);
#endif

   /*
    *     install callbacks on HELP men button
    */
   XtAddCallback(HelpPdMenu_help, XmNactivateCallback,
          (XtCallbackProc)km_do_help,(XtPointer)"HELP_BROWSER");

   /*
    * SELECT ALL command
    */
   XtAddCallback(ViewPdMenu_selall, XmNactivateCallback,
          (XtCallbackProc)select_all_decks, browser);
   /*
    * PREFERENCES command
    */
   XtAddCallback(ViewPdMenu_filt, XmNactivateCallback,
          (XtCallbackProc)show_filtDialog, browser);

   /* get icon_type set in the .Xdefaults file */
   icon_type = IconBoxGetIconType(dirList);
   set_icon_tb_state(icon_type);
}

/***********************************************************************
 *                                                                     *
 *   Set the icon toggle button state.                                 *
 *                                                                     *
 ***********************************************************************/
static void set_icon_tb_state(icon_type)
     int icon_type;
{
   /* turn all buttons off except the one currently active */
   XmToggleButtonGadgetSetState(ViewPdMenu_icon,  False, False);
   XmToggleButtonGadgetSetState(ViewPdMenu_sicon, False, False);
   XmToggleButtonGadgetSetState(ViewPdMenu_nicon, False, False);
   XmToggleButtonGadgetSetState(ViewPdMenu_title, False, False);

   switch (icon_type) {
      case IconBoxBIG_ICON:
       XmToggleButtonGadgetSetState(ViewPdMenu_icon,  True, False);
       break;
      case IconBoxSMALL_ICON:
       XmToggleButtonGadgetSetState(ViewPdMenu_sicon, True, False);
       break;
      case IconBoxNO_ICON:
       XmToggleButtonGadgetSetState(ViewPdMenu_nicon, True, False);
       break;
      case IconBoxTITLES:
       XmToggleButtonGadgetSetState(ViewPdMenu_title, True, False);
   }
}

/***********************************************************************
 *                                                                     *
 *   Get the new icon_type and change the dirList accordingly.         *
 *                                                                     *
 ***********************************************************************/
static void change_icon_type(w, icon_type, call_data)
       Widget w;
       int icon_type;
       XmToggleButtonCallbackStruct *call_data;
{
   Widget    browser;
   XmString *xmlist;
   int       dcnt;

   browser = find_kuipBrowser(w);
   get_fb_context(browser);

   set_icon_tb_state(icon_type);

   /* when a button gets unselected don't do anything */
   if (!call_data->set) return;

   km_set_cursor (browser, km_wait_cursor);

   IconBoxGetItems(dirList, &xmlist, &dcnt);

   /*
    * Delete all items and reset list with current path (browser_dir) and
    * icon_type.
    */
   if (icon_type == IconBoxTITLES) {
      titles = 3;
      if (dcnt) IconBoxDeleteAllItems(dirList, False);
      IconBoxSetIconType(dirList, IconBoxSMALL_ICON);
   } else {
      if (icon_type == IconBoxBIG_ICON)   titles = 0;
      if (icon_type == IconBoxSMALL_ICON) titles = 1;
      if (icon_type == IconBoxNO_ICON)    titles = 2;
      if (dcnt) IconBoxDeleteAllItems(dirList, False);
      IconBoxSetIconType(dirList, icon_type);
   }
   save_fb_context(browser);

   if (dcnt) {
       char *selected_path;

       selected_path = (char *) XtMalloc (MAX_string+1);
       strcpy (selected_path, browser_dir);
       do_list_content (selected_path, True);
       XtFree (selected_path);
   }

   km_set_cursor (browser, km_main_cursor);

   /* cleanup XmString list */
   if (dcnt) XtFree((char *)xmlist);
}

#ifdef ALL_CYCLE /* vms */
/***********************************************************************
 *                                                                     *
 *   Get cycle flag ("All Cycles" or not)                              *
 *                         and change the dirList accordingly.         *
 *                                                                     *
 ***********************************************************************/
static void change_cycle(w, tag, call_data)
       Widget w;
       caddr_t tag;
       XmToggleButtonCallbackStruct *call_data;
{
   Widget    browser;
   XmString *xmlist;
   int       list, zero, all, ref, ll, dcnt, icon_type;

   if (call_data->set )
       km_all_cycles = 1;
   else
       km_all_cycles = 0;

   browser = find_kuipBrowser(w);
   get_fb_context(browser);

   list = DIR_LIST;
   zero = 0;
   all  = 1;
   ref  = 0;
   ll   = strlen(browser_dir);

   km_set_cursor (browser, km_wait_cursor);

   IconBoxGetItems(dirList, &xmlist, &dcnt);

   /*
    * Delete all items and reset list according to km_all_cycles
    */
   if (XmToggleButtonGadgetGetState(ViewPdMenu_icon))
        icon_type = IconBoxBIG_ICON;
   else if (XmToggleButtonGadgetGetState(ViewPdMenu_sicon))
        icon_type = IconBoxSMALL_ICON;
   else if (XmToggleButtonGadgetGetState(ViewPdMenu_nicon))
        icon_type = IconBoxNO_ICON;
   else if (XmToggleButtonGadgetGetState(ViewPdMenu_title))
        icon_type = IconBoxSMALL_ICON;
   IconBoxSetIconType(dirList, icon_type);
   save_fb_context(browser);

   if (dcnt) {
       char *selected_path;

       selected_path = (char *) XtMalloc (MAX_string+1);
       strcpy (selected_path, browser_dir);
       do_list_content (selected_path, True);
       XtFree (selected_path);
   }

   km_set_cursor (browser, km_main_cursor);

   /* cleanup XmString list */
   if (dcnt) XtFree((char *)xmlist);
}
#endif

/***********************************************************************
 *                                                                     *
 *   Select all decks.                                                 *
 *                                                                     *
 ***********************************************************************/
static void select_all_decks(w, browser, call_data)
       Widget w, browser;
       XmAnyCallbackStruct *call_data;
{
   get_fb_context(browser);

   IconBoxSelectAllItems(dirList, True, False);
}

/***********************************************************************
 *                                                                     *
 *   Show (manage) PREFERENCES dialog.                                 *
 *                                                                     *
 ***********************************************************************/
static void show_filtDialog(w, browser, call_data)
       Widget w, browser;
       XmAnyCallbackStruct *call_data;
{
   XmString *xmlist;
   int       ibr, dcnt;
   char *filter;

   ibr = get_fb_context(browser);
   filter = km_prompt ("Filter (on object names)",
                                fbrowser[ibr].filter);
   if (filter != NULL) strcpy(fbrowser[ibr].filter, filter);

   km_set_cursor (browser, km_wait_cursor);

   IconBoxGetItems(dirList, &xmlist, &dcnt);

/* Delete all items and reset list with current filter */
   if (dcnt) {
       char *selected_path;

       IconBoxDeleteAllItems(dirList, False);
       selected_path = (char *) XtMalloc (MAX_string+1);
       strcpy (selected_path, browser_dir);
       do_list_content (selected_path, True);
       XtFree (selected_path);
   }

   km_set_cursor (browser, km_main_cursor);

   /* cleanup XmString list */
   if (dcnt) XtFree((char *)xmlist);
   XtFree((char *)filter);
}

/***********************************************************************
 *                                                                     *
 *   Callbacks for "Open" menu items.                                  *
 *                                                                     *
 ***********************************************************************/
static void FilePdMenu_o_activate (widget, tag, call_data)
     Widget widget;
     caddr_t tag;
     XmAnyCallbackStruct *call_data;
{
     otag_str *stag = (otag_str *)tag;

     current_callback_data = call_data;
     km_get_curpos (call_data->event, &km_xcur_pos, &km_ycur_pos);
     exec_action( stag->bract, NULL, NULL, 1, widget, call_data);
}

/***********************************************************************
 *                                                                     *
 *   Callbacks for "Exit".                                             *
 *                                                                     *
 ***********************************************************************/
static void FilePdMenu_exit (widget, tag, call_data)
     Widget widget;
     caddr_t tag;
     XmAnyCallbackStruct *call_data;
{
    exec_cmd_string ("/KUIP/EXIT", 0, NULL);
}
