/*
 * $Id: pawm.h,v 1.3 1996/03/19 12:52:51 cremel Exp $
 *
 * $Log: pawm.h,v $
 * Revision 1.3  1996/03/19 12:52:51  cremel
 * BUG correction in Paw++ cut editor (happened for a long cut expression when
 * the number of cuts was > 11). The problem was that cut_expression had
 * dimension MAX_CMD_LENGTH (512) whereas local variable cut was MAX_STRING (80).
 * * cut editor BUG in Paw++: cut[MAX_CMD_LENGTH] instead of cut[MAX_STRING].
 * * add reference to KUIP utility routine mstrcat (extern).
 *
 * Revision 1.2  1996/03/12 16:59:29  cremel
 * cpp run over the CDFs
 * Add new resource "hbookfile" to specify the extension for HBOOK files.
 * This can now be specify by the user in .Xdefaults, e.g.:
 *
 * Paw++*hbookfile: dat
 *
 * (default is "hbook").
 * definition for new resource "hbookfile".
 *
 * Revision 1.1.1.1  1996/03/01 11:39:14  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 13/03/94  13.30.56  by  Fons Rademakers*/
/*-- Author :*/
/***********************************************************************
 *
 * pawm.h: declarations and definitions for paw++
 *
 **********************************************************************/
#ifndef _pawm_h
#define _pawm_h

#include "uxxt.h"
#include "kuip.h"

#include "fortran.h"


#define FILE_LIST          1
#define DIR_LIST           2
#define MAX_BROWSERS       5
#define MAX_GRAPHICS       5
#define MAX_STRING         80
#define MAX_CMD_LENGTH     512 /* max length of a PAW command */
#define MAXITEMS           50  /* max number of items in a popup menu */

/* histogram types */
#define HISTO_1D           1
#define HISTO_2D           2
#define HISTO_3D           3

typedef struct {
   Boolean  auto_refresh;   /* automatic refresh when changing style options */
   Boolean  echo_kuip_cmd;  /* echo kuip commands */
   Boolean  double_buffer;  /* use double buffering */
   Boolean  same_zone;      /* plot current histogram in same zone */
   String   hbook_file;     /* extension for hbook files */
} AppRes, *AppResPtr;

typedef struct {
   Widget    button;        /* menu button to be set (in)sensitive */
   Widget    dialog;        /* associated argument popup dialog */
} MenuCbStruct;

typedef struct {
   int       id;                     /* last selected histogram */
   char      dir[MAX_STRING];        /* histogram directory */
   int       type;                   /* 1: 1D histo, 2: 2D histo, 3: 3D histo */
   unsigned int bits;                /* status word */
   int       nx;                     /* number of channels in X */
   float     xmin;                   /* lower edge of first X channel */
   float     xmax;                   /* upper edge of last X channel */
   int       ny;                     /* number of channels in Y */
   float     ymin;                   /* lower edge of first Y channel */
   float     ymax;                   /* upper edge of last Y channel */
   int       x1;                     /* first bin to be plotted on X */
   int       x2;                     /* last bin to be plotted on X */
   int       y1;                     /* first bin to be plotted on Y */
   int       y2;                     /* last bin to be plotted on Y */
   char      option_1d[8];           /* plotting option for 1d histos */
   char      option_2d[8];           /* plotting option for 2d histos */
   char      option_3d[8];           /* plotting option for 3d histos */
   char      coordinate[8];          /* coordinate system */
   char      *XAxisTitle;            /* XAxis Title */
   char      *YAxisTitle;            /* YAxis Title */
} HistoStruct;

typedef struct {
   Widget     widget;                /* browser widget */
   Widget     open_dialog;           /* widget of open dialog */
   Widget     close_dialog;          /* widget of close dialog */
} BrowserStruct;

typedef struct {
   Widget     widget;                /* graphics window widget */
} GraphicsStruct;

typedef struct {
   int       id;                     /* id of Ntuple in Ntuple viewer */
   char      dir[MAX_STRING];        /* Ntuple directory */
   unsigned int bits;                /* status word */
   int       ncols;                  /* number of columns in Ntuple */
   int       nrows;                  /* number of rows in Ntuple */
   char      xyz[MAX_STRING];        /* variables selected in the NT browser */
   char      cut[MAX_CMD_LENGTH];    /* cut selected in the NT browser */
                                     /* (same dim. as cut_expression) */
   int       nvars;                  /* number variables in the NT browser */
   int       first;                  /* first event */
   int       nevent;                 /* number of events */
   Boolean   chain;                  /* are we dealing with a chain? */
} NtupleStruct;

typedef struct _menu_item {
   char         *label;
   WidgetClass  *class;
   char          mnemonic;
   char         *accelerator;
   char         *accel_text;
   void        (*callback)();
   caddr_t       callback_data;
   Boolean       default_button;
   struct _menu_item *subitems;
} MenuItem;

/* global data */
extern Widget          UxTopLevel;      /* parent widget of topLevelShell's */
extern Widget          topWidget;       /* executive window */
extern BrowserStruct   browser[];       /* file browsers (max=5) */
extern GraphicsStruct  graphics[];      /* graphics windows (max=5) */
extern Display        *UxDisplay;
extern AppRes          appres;          /* application resources */
extern HistoStruct     histogram;       /* style panel/file browser comm. */
extern NtupleStruct    ntuple;          /* set in Ntuple viewer */

/* interface to PAWLUN common block (defined in PAW) */
#include "pawlunc.h"

/* utility functions */
extern char          *extract_normal_string(XmString);
extern void           execute_kuip_cmd(char *);
extern void           lower(char *);
extern void           upper(char *);
extern Boolean        strempty(char *);
extern char          *compress(char *);
extern char          *get_selected_items(int);
extern MenuCbStruct  *make_menuStruct(Widget, Widget);
extern void           popup_argument_dialog(MenuCbStruct *);
extern void           popdown_argument_dialog(MenuCbStruct *);
extern void           close_cb(Widget, Widget, XmAnyCallbackStruct *);
extern void           cancel_cb(Widget, MenuCbStruct *, XmAnyCallbackStruct *);
extern void           help_cb(Widget, char *, XmAnyCallbackStruct *);
extern Boolean        ask_user(Widget, char *, char *, char *, int);
extern void           warn_user(Widget, char *, char *);
extern void           inform_user(Widget, char *, char *);
extern void           input_from_user(Widget, char *, int *,
                                      char *, int *,
                                      char *, int *,
                                      char *, int *);
extern void           InstallMwmCloseCallback(Widget, XtCallbackProc,
                                              XtPointer);
extern void           IconifyShell(Widget);
extern void           DeIconifyShell(Widget);
extern Boolean        IsShellIconic(Widget);
extern void           TogglePushButton(Widget, Boolean);
extern void           ForceUpdate(Widget);
extern void           set_pixmap_to_button(Widget, char *, int, int, Boolean);
extern void           plot_current_histo(void);

extern void           FindRgb(int Num,XColor *RgbColor);
extern void           show_attributes(Widget w);
extern void           show_fontSettings(Widget w);
extern void           show_geometry (Widget w);
extern void           axis_settings_dialog(Widget);
extern void           open_style_dialog(Widget);
extern void           save_style_as_dialog(Widget);
extern void           show_generalAttributes(Widget w);
extern void           ixsdswi(Display*, Window);
extern void           set_histo_id(Boolean);
extern void           set_environment(void);
extern void           set_ntuple_id(void);
extern char          *km_strip(char *);
extern int            ku_exec(char*);

/* utility functions from KUIP */
extern char          *mstrcat( char *str1, const char *str2 );

#endif  /* _pawm_h */

