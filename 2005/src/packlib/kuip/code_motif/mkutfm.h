/*
 * $Id: mkutfm.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkutfm.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */
/*** Utility functions which refer some Motif/X11 include files ***/
#ifndef _mkutfm_
#define _mkutfm_

/* km_do_help : Display help corresponding to a "Help_item" entry in the CDF. */
extern C_PROTO_3(void          km_do_help,
                                  Widget w,
                                  char *cval,
                                  XmAnyCallbackStruct *call_data);
/* km_help_activate : Display help text at the current cursor position */
extern C_PROTO_3(void km_help_activate,
                         Widget  widget,
                         caddr_t tag,
                         XmAnyCallbackStruct *callback_data);
/* km_GetTrueShell : Return the true toplevel shell of the interface */
extern C_PROTO_1(Widget        km_GetTrueShell,
                                  Widget);
/* km_normal_string : Get char string from XmString */
extern C_PROTO_1(char         *km_normal_string,
                                  XmString);
/* km_insert_string : Insert a string at the end of a XmText widget */
extern C_PROTO_3(void          km_insert_string,
                                  Widget,
                                  char *,
                                  Boolean);
/* km_all_cursor : Set cursor shape for all widgets/windows */
extern C_PROTO_1(void          km_all_cursor,
                                  int);
/*  km_double_click : Check if doubleclick happened */
extern C_PROTO_1(Boolean       km_double_click,
                                  Time);
/* km_quit_confirm : ask for confirmation before quit (return 1 or 0) */
extern C_PROTO_0(int           km_quit_confirm);
/* km_exit_confirm : ask for confirmation before exit (return 1 or 0) */
extern C_PROTO_0(int           km_exit_confirm);
/* km_cmd_list : display selection box with list of possible commands */
extern C_PROTO_3(void         km_cmd_list,
                                  char *command,
                                  KmCommand **cmd_list,
                                  int flag);
/* km_display_list : display selection box with list of items */
extern C_PROTO_3(void km_display_list,
                                  char **items,
                                  char **help_items,
                                  ListData data);
/* km_ask_user : Ask the user a question and wait for the answer (2 choices) */
extern C_PROTO_5(Boolean       km_ask_user,
                                  Widget,
                                  char *,
                                  char *,
                                  char *,
                                  int);
/* km_warn_user : Warn the user with a message and wait till he acknowledges */
extern C_PROTO_3(void          km_warn_user,
                                  Widget,
                                  char *,
                                  char *);
/* km_inform_user : Inform the user with a message and wait till acknowledge */
extern C_PROTO_3(void          km_inform_user,
                                  Widget,
                                  char *,
                                  char *);
/* km_MwmCloseCallback : Install MwmCloseCallback */
extern C_PROTO_3(void          km_MwmCloseCallback,
                                  Widget,
                                  XtCallbackProc,
                                  XtPointer);
/* km_IsShellIconic : Is shell in iconic state? */
extern C_PROTO_1(Boolean       km_IsShellIconic,
                                  Widget);
/* km_IconifyShell : Iconify a shell */
extern C_PROTO_1(void          km_IconifyShell,
                                  Widget);
/* km_DeIconifyShell : De-Iconify a shell */
extern C_PROTO_1(void          km_DeIconifyShell,
                                  Widget);
/* km_FlushEvents : equivalent to XtAppMainLoop */
extern C_PROTO_0(void          km_FlushEvents);
/* km_wait_input : Ask user for input and wait till she responds (C callable) */
extern C_PROTO_3 (char        *km_wait_input,
                                  char *prompt,
                                  char *default_line,
                                  char *help_item);
/* km_toggle_selection : another taste of the "SelectionBox" (with toggles)   */
extern C_PROTO_3 (int          km_toggle_selection,
                                  char *prompt,
                                  char **list,
                                  int defsel);
/* km_set_cursor : Set the cursor shape for one specific widget/window */
extern C_PROTO_2(void          km_set_cursor,
                                  Widget,
                                  Cursor);
/* km_get_curpos : Get cursor positions xc and yc */
extern C_PROTO_3(void          km_get_curpos,
                                  XEvent *event,
                                  Position *x,
                                  Position *y);
/* km_display_cmdpan : display Kuip/Motif panel corresponding to a command */
extern C_PROTO_3(int           km_display_cmdpan,
                                  KmCommand *,
                                  char *,
                                  int);
/*  km_create_sctext : Display text in a scrollable window and return Widget */
extern C_PROTO_5(Widget        km_create_sctext,
                                  char *title,
                                  char *text,
                                  Position xr,
                                  Position yr,
                                  int flag);
/*  km_display_sctext : same as km_create_sctext without returning Widget */
extern C_PROTO_5(void          km_display_sctext,
                                  char *title,
                                  char *text,
                                  Position xr,
                                  Position yr,
                                  int flag);
/* km_reset_allbrowser : Set or reset the file browser update flag */
extern C_PROTO_1(void          km_reset_allbrowser,
                                  Boolean set);
/* km_panel_buttons : fill panel (RowColumn) with buttons */
extern C_PROTO_4(void          km_panel_buttons,
                                  Widget panel_widget,
                                  char *name,
                                  int view_flag,
                                  int flag);
/* km_panel_execute : panel button execution (callback) */
extern C_PROTO_3(void          km_panel_execute,
                                  Widget   pbut_widget,
                                  caddr_t tag,
                                  XmAnyCallbackStruct *callback_data);

typedef struct _Pal {
   struct _Pal          *next;      /* link to next panel */
   int                   npan;      /* panel number */
   int                   npal;      /* palette number */
   Widget                PALwidget; /* palette widget */
   Widget                Bwidget;
   Widget                Pwidget;
   Widget                Mwidget;   /* widget currently managed */
} Pal;

typedef struct _PaletteStruct {
   Widget                widget;
   int                   npal;      /* palette number */
   int                   view_flag;
   Pal                  *last_pan;  /* last panel in the list */
} PaletteStruct;
/* km_panel_add : add button/panel in multi-panel widget */
extern C_PROTO_3 (void         km_panel_add,
                                  PaletteStruct *pw,
                                  char *name,
                                  char *geometry);
/* Interface for building menus */
typedef struct _menu_item {
   char         *label;
   WidgetClass  *class;
   Boolean       set;     /* default setting in case of toggle button */
   char          mnemonic;
   char         *accelerator;
   char         *accel_text;
   void        (*callback)();
   caddr_t       callback_data;
   Boolean       default_button;
   Boolean       separator;
   struct _menu_item *subitems;
} MenuItem;
/* km_build_popup : Build a popup menu and return Widget and default button */
extern C_PROTO_5(Widget        km_build_popup,
                                  Widget parent,
                                  char *menu_title,
                                  MenuItem *items,
                                  int menu_type,
                                  Widget *default_button);
/* km_add_item: Fill MenuItem array and returns number of items */
extern C_PROTO_5(int           km_add_item,
                                  char *item_string,
                                  void (*push_cb)(),
                                  void (*toggle_cb)(),
                                  MenuItem *items,
                                  int max_items);
/* km_build_menu: Build popup/option/pulldown menus, depending on menu_type. */
extern C_PROTO_6(Widget        km_build_menu,
                                  Widget parent,
                                  int menu_type,
                                  char *menu_title,
                                  int menu_mnemonic,
                                  MenuItem *items,
                                  int add_to_menu);
/* km_destroy_all_popup : destroy all KUIP/Motif popup menus (button 3 use) */
extern C_PROTO_0(void          km_destroy_all_popup);
/* km_make_menuStruct : Assign the space for a MenuCbStruct and fill it */
typedef struct {
   Widget    button;        /* menu button to be set (in)sensitive */
   Widget    dialog;        /* associated argument popup dialog */
} MenuCbStruct;
extern C_PROTO_2(MenuCbStruct *km_make_menuStruct,
                                  Widget,
                                  Widget);
/* km_help_cb : Display (scrolled text) HELP on command passed as client_data */
extern C_PROTO_3 (void         km_help_cb,
                                  Widget w,
                                  char *help_cmd,
                                  XmAnyCallbackStruct *call_data);
/* km_close_cb : Close (popdown) a popup dial. (/shell) passed as client_data */
extern C_PROTO_3(void          km_close_cb,
                                  Widget,
                                  Widget,
                                  XmAnyCallbackStruct *);
/* km_cancel_cb : Unmanage popup passed as client_data (in MenuCbStruct). */
extern C_PROTO_3(void          km_cancel_cb,
                                  Widget,
                                  MenuCbStruct *,
                                  XmAnyCallbackStruct *);
/* km_reset_sensitive_cb : Reset sensitivity on widget passed as client_data */
extern C_PROTO_3(void          km_reset_sensitive_cb,
                                  Widget,
                                  Widget,
                                  XmAnyCallbackStruct *);
/* km_show_cb : Show (manage) the widget passed as the client_data */
extern C_PROTO_3(void          km_show_cb,
                                  Widget w,
                                  Widget dialog,
                                  XmAnyCallbackStruct *call_data);
/* km_popdown_MenuCbdialog : Popdown dialog (in MenuCbStruct) and make menu
item sensitive. */
extern C_PROTO_1(void          km_popdown_MenuCbdialog,
                                  MenuCbStruct *);
/* km_popup_MenuCbdialog : Popup dialog (in MenuCbStruct) and make menu
item insensitive. */
extern C_PROTO_1(void          km_popup_MenuCbdialog,
                                  MenuCbStruct *);

extern C_PROTO_5(void          km_print_box,
                                  Widget pw,
                                  Position x,
                                  Position y,
                                  char *message,
                                  int itype);

extern C_PROTO_2(void          km_store_appShell,
                                  Widget w,
                                  char *title);
extern C_PROTO_1(void          km_reset_appShell,
                                  Widget w);

extern C_PROTO_0(Widget        create_kuipPanel);

/* km_add_menu: to add a pulldown menu in main browser */
extern C_PROTO_1(void          km_add_menu,
                                  char **menu);

/* km_ContextHelp : context help for panels */
extern C_PROTO_0(void          km_ContextHelp);

/* km_focus_window : to avoid the window to disappear under kxterm */
extern C_PROTO_3(void          km_focus_window,
                                  Widget widget,
                                  caddr_t tag,
                                  XEnterWindowEvent *ev);

/* km_destroy_list : destroy current list (km_list_id) */
extern C_PROTO_0(void          km_destroy_list);

/* km_list_data : Set data information for a user defined list */
extern void  km_list_data( const char *list_label,
                          const char *selection_label,
                          const char *help_text,
                          int (*callback)()) ;

/* km_show_list : display a user defined list */
extern C_PROTO_1(void          km_show_list,
                                  char **items);

/* km_show_filSel : display a user defined FileSelectionBox */
extern void  km_show_filSel( const char *title,
                            const char *dir,
                            const char *def,
                            const char *help,
                            int (*okCallback)());

#endif /* _mkutfm_ */

