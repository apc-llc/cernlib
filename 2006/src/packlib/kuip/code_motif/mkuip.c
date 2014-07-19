/*
 * $Id: mkuip.c,v 1.2 1996/10/24 12:13:41 cremel Exp $
 *
 * $Log: mkuip.c,v $
 * Revision 1.2  1996/10/24 12:13:41  cremel
 * Version 2.07/10
 * Add resource higzMessGeometry to have control on the size and position of
 * the "HigzOutput" window (e.g. for the locator). To change the size
 * and position of this window one must add in his .Xdefaults (or .Xresources)
 * file the 2 following lines:
 *
 * Paw++*kuipScroll1.defaultPosition: False
 * Paw++*higzMessGeometry: 200x250+650+650
 *
 * Revision 1.1.1.1  1996/03/08 15:33:06  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/05 01/06/95  15.55.18  by  N.Cremel*/
/*-- Author :    N.Cremel   06/05/92*/
/******************************************************************************/
/*                                                                            */
/*                MOTIF-based library for KUIP - deck MKUIP                   */
/*                                                                            */
/*                Basic Interface for the "Executive Window" :                */
/*                (connected to the file //KUIP/XMCLIB/KUXEXE for building    */
/*                the window with Architect : "create_kuipExec")              */
/*                                                                            */
/*                - Motif / Xt Initialization                                 */
/*                - Menus "Control",  "Commands" (command panels) and "Help"  */
/*                - Command widget manipulation                               */
/*                - KUIP Panels manipulation (style GP) / connected to the    */
/*                  file //KUIP/XMCLIB/KUXPAN for building the panels with    */
/*                  Architect ("create_kuipPanel")                            */
/*                - User defined buttons and Menus                            */
/*                - Application Main Loop.                                    */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
#define Extern                  /* allocate storage for globals */

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/klink.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/mkterm.h"

#include <X11/keysym.h>

/* Global SEQUENCES (defined in MKCDES) */
#include "mkincl.h"
#include "mkdims.h"
#include "mkfont.h"
#include "mksres.h"
#include "mkutfm.h"
#include "mkutdm.h"
#include "kuip/mkutfu.h"
#define CONTEXT_MACRO_ACCESS 1
#include "mkuxbr.h"
#include "mkuxpa.h"
#include "mkuxli.h"
#include "mkuxlihlp.h"
#undef CONTEXT_MACRO_ACCESS
#include "kuip/browh1.h"
#include "browh2.h"

/* Start hourglass bitmaps (for km_wait_cursor) */
#define hourglass_width 32
#define hourglass_height 32
#define hourglass_x_hot 15
#define hourglass_y_hot 15
static unsigned char hourglass_bits[] = {
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0x40, 0x00, 0x00, 0x01,
   0xe0, 0xff, 0xff, 0x03, 0x60, 0x00, 0x00, 0x03, 0x60, 0x00, 0x00, 0x03,
   0x60, 0x00, 0x00, 0x03, 0x60, 0xc0, 0x01, 0x03, 0xc0, 0xa8, 0x8e, 0x01,
   0x80, 0x55, 0xdd, 0x00, 0x00, 0xab, 0x6f, 0x00, 0x00, 0x56, 0x37, 0x00,
   0x00, 0xec, 0x1b, 0x00, 0x00, 0xd8, 0x0d, 0x00, 0x00, 0xb0, 0x06, 0x00,
   0x00, 0xa0, 0x02, 0x00, 0x00, 0x20, 0x02, 0x00, 0x00, 0xb0, 0x06, 0x00,
   0x00, 0x98, 0x0c, 0x00, 0x00, 0x8c, 0x18, 0x00, 0x00, 0x06, 0x30, 0x00,
   0x00, 0x83, 0x60, 0x00, 0x80, 0xc1, 0xc3, 0x00, 0xc0, 0xb0, 0x86, 0x01,
   0x60, 0x5c, 0x1d, 0x03, 0x60, 0x2a, 0x39, 0x03, 0x60, 0xdd, 0x7f, 0x03,
   0x60, 0x00, 0x00, 0x03, 0xe0, 0xff, 0xff, 0x03, 0x40, 0x00, 0x00, 0x01,
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03};

#define hourglass_mask_width 32
#define hourglass_mask_height 32
static unsigned char hourglass_mask_bits []= {
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0xc0, 0xff, 0xff, 0x01,
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03,
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0xc0, 0xff, 0xff, 0x01,
   0x80, 0xff, 0xff, 0x00, 0x00, 0xff, 0x7f, 0x00, 0x00, 0xfe, 0x3f, 0x00,
   0x00, 0xfc, 0x1f, 0x00, 0x00, 0xf8, 0x0f, 0x00, 0x00, 0xf0, 0x07, 0x00,
   0x00, 0xe0, 0x03, 0x00, 0x00, 0xe0, 0x03, 0x00, 0x00, 0xf0, 0x07, 0x00,
   0x00, 0xf8, 0x0f, 0x00, 0x00, 0xfc, 0x1f, 0x00, 0x00, 0xfe, 0x3f, 0x00,
   0x00, 0xff, 0x7f, 0x00, 0x80, 0xff, 0xff, 0x00, 0xc0, 0xff, 0xff, 0x01,
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03,
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0xc0, 0xff, 0xff, 0x01,
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03};
/* ... end hourglass bitmaps */

/* application resource list */
static XtResource kuip_resources[] = {
   { XmNdoubleClickInterval, XmCDoubleClickInterval, XmRInt, sizeof(int),
     XtOffset(SresRecPtr, dc_interval), XmRImmediate, (caddr_t) 250},
   { XmNhelpFont, XmCHelpFont, XmRString, sizeof (String),
     XtOffset (SresRecPtr,Help_font), XmRString,
     "*-courier-medium-r-normal*-120-*"},
   { XmNdoubleBuffer, XmCDoubleBuffer, XmRBoolean, sizeof(Boolean),
     XtOffset (SresRecPtr,double_buffer), XmRImmediate, (caddr_t) True},
   { XmNgBell, XmCGBell, XmRInt, sizeof (int),
     XtOffset (SresRecPtr,bell_vol), XmRImmediate, (caddr_t) 0},
   { XmNcenterCommand, XmCCenterCommand, XmRInt, sizeof(int),
     XtOffset (SresRecPtr,center_command), XmRImmediate, (caddr_t) 0},
   { XmNpanelInteractive, XmCPanelInteractive, XmRBoolean, sizeof(Boolean),
     XtOffset (SresRecPtr,panel_interactive), XmRImmediate, (caddr_t) True},
   { XmNnoKxterm, XmCNoKxterm, XmRBoolean, sizeof(Boolean),
     XtOffset (SresRecPtr,no_kxterm), XmRImmediate, (caddr_t) False},
   { XmNnoMBrowser, XmCNoMBrowser, XmRBoolean, sizeof(Boolean),
     XtOffset (SresRecPtr,no_mbrowser), XmRImmediate, (caddr_t) False},
   { XmNhigzMessGeometry, XmCHigzMessGeometry, XmRString, sizeof (String),
     XtOffset (SresRecPtr,higzmess_geometry), XmRString, "0x0+0+0"},
   { XmNkxtermGeometry, XmCKxtermGeometry, XmRString, sizeof (String),
     XtOffset (SresRecPtr,kxterm_geometry), XmRString, "650x450+0+0"},
   { XmNkxtermTextFont, XmCKxtermTextFont, XmRString, sizeof (String),
     XtOffset (SresRecPtr,kxterm_text_font), XmRString,
     "*-courier-medium-r-normal*-120-*"},
   { XmNkxtermFont, XmCKxtermFont, XmRString, sizeof (String),
     XtOffset (SresRecPtr,kxterm_font), XmRString,
     "*-helvetica-bold-r-normal*-120-*"},
   { XmNmainCursor, XmCMainCursor, XmRInt, sizeof (int),
     XtOffset (SresRecPtr,main_cs), XmRImmediate, (caddr_t) -1},
   { XmNgraphCursor, XmCGraphCursor, XmRInt, sizeof (int),
     XtOffset (SresRecPtr,graph_cs), XmRImmediate, (caddr_t) -1},
   { XmNinputCursor, XmCInputCursor, XmRInt, sizeof (int),
     XtOffset (SresRecPtr,input_cs), XmRImmediate, (caddr_t) -1},
   { XmNwaitCursor, XmCWaitCursor, XmRInt, sizeof (int),
     XtOffset (SresRecPtr,wait_cs), XmRImmediate, (caddr_t) -1},
   { XmNhelpCursor, XmCHelpCursor, XmRInt, sizeof (int),
     XtOffset (SresRecPtr,help_cs), XmRImmediate, (caddr_t) -1},
   { XmNbusyCursor, XmCBusyCursor, XmRInt, sizeof (int),
     XtOffset (SresRecPtr,busy_cs), XmRImmediate, (caddr_t) -1},
   { XmNblckCursor, XmCBlckCursor, XmRInt, sizeof (int),
     XtOffset (SresRecPtr,blck_cs), XmRImmediate, (caddr_t) -1},
   { XmNselcCursor, XmCSelcCursor, XmRInt, sizeof (int),
     XtOffset (SresRecPtr,selc_cs), XmRImmediate, (caddr_t) -1},
   { XmNscrolledCmdPanel, XmCScrolledCmdPanel, XmRString, sizeof (String),
     XtOffset (SresRecPtr,scrolled_cmdpanel), XmRString, "auto"},
};

/* No graphics window */
static String k_fallbacks[] = {
   "*dirlist*fontList: *-courier-bold-r-normal*-120-*",
   "*fontList: *-helvetica-bold-r-normal*-120-*",
   "*kuipBrowser*fileScWindow.width: 80",
   "*kuipBrowser*fileScWindow.bottomOffset: 52",
   "*kuipBrowser*fileScWindow.topOffset: 59",
   "*kuipBrowser*dirScWindow.bottomOffset: 52",
   "*kuipBrowser*dirScWindow.topOffset: 59",
   "*kuipBrowser*separatorGadget1.topPosition: 50",
   "*kuipBrowser_shell.geometry: -0+0",
   NULL};

/* With graphics window */
static String kg_fallbacks[] = {
   "*dirlist*fontList: *-courier-bold-r-normal*-120-*",
   "*fontList: *-helvetica-bold-r-normal*-120-*",
   "*kuipBrowser*fileScWindow.width: 80",
   "*kuipBrowser*fileScWindow.bottomOffset: 52",
   "*kuipBrowser*fileScWindow.topOffset: 59",
   "*kuipBrowser*dirScWindow.bottomOffset: 52",
   "*kuipBrowser*dirScWindow.topOffset: 59",
   "*kuipBrowser*separatorGadget1.topPosition: 50",
   "*kuipGraphics_shell.geometry: 600x600-0+0",
   "*kuipBrowser_shell.geometry: +0+485",
   NULL};


/***********************************************************************
 *                                                                     *
 *   Local data.                                                       *
 *                                                                     *
 ***********************************************************************/
static Widget   appShell[MAX_APP_SHELL];
static char    *appShell_title[MAX_APP_SHELL];
static Widget   print_box_widget;
static int      fl_decwindows; /* flag if the server is DecWindows */
static int      do_bell;
static Widget   kuipControlM;
static Widget   kuipControlShell, kuipControlShellM;

static Widget  km_list_id = NULL;  /* Widget id. for kuipList */

#define MAX_ITEMS          30

/***********************************************************************
 *                                                                     *
 *   GLOBAL data.                                                      *
 *                                                                     *
 ***********************************************************************/
char          class_name[MAX_string];
char          main_prompt[MAX_string];
XmFontList    help_font = NULL;
XFontStruct  *help_font_struct;
Colormap      km_colormap;
SresRec       srec;             /* application resources */
XmAnyCallbackStruct *current_callback_data = NULL;
Boolean       km_browser_refresh;
int           do_exec, km_all_cycles;

void  (*user_logo_C)( /* Widget */ );      /* should be in a kuip structure */
void  (*user_mail_C)( /* Widget */ );      /* should be in a kuip structure */

/* For HPArchitect */
XtAppContext    UxAppContext;
Widget          UxTopLevel;
Display         *UxDisplay;
int             UxScreen, UxScreenWidth, UxScreenHeight;


/***********************************************************************
 *                                                                     *
 *   Forward declarations of local functions.                          *
 *                                                                     *
 ***********************************************************************/
static C_PROTO_0(void init_data);
static void set_HIGZ();
static C_PROTO_1(Widget create_option_menu,
                         Widget parent);
static C_PROTO_1(void create_command_menu,
                         Widget parent);
static C_PROTO_2(void set_grab,
                         Widget widget_id,
                         int flgrab);
static C_PROTO_4(void display_widget,
                         KmCommand *cmd,
                         Position xr,
                         Position yr,
                         int flag);
static C_PROTO_4(void show_widget,
                         char *command,
                         Position xr,
                         Position yr,
                         int flag);

/* List of Callbacks */
static void C_PROTO_3(destroy_print_box,
                         Widget   widget,
                         caddr_t tag,
                         XmAnyCallbackStruct *callback_data);
static C_PROTO_3(void get_command_panel,
                         Widget   widget,
                         int      tag,
                         XmAnyCallbackStruct *callback_data);
#if 0
static C_PROTO_3(void help_list_items,
                         Widget w,
                         char   **help,
                         XmAnyCallbackStruct *call_data);
#endif
static C_PROTO_3(void destroy_list_id,
                         Widget w,
                         caddr_t tag,
                         XmAnyCallbackStruct *call_data);
static C_PROTO_3(void command_panel_activate,
                         Widget   widget,
                         char     *cmd,
                         XmAnyCallbackStruct *data);
static C_PROTO_3(void appShell_activate,
                         Widget  widget,
                         int     tag,
                         XmAnyCallbackStruct *callback_data);
static C_PROTO_3(void iconify_appShell,
                         Widget w,
                         XtPointer client_data,
                         XmAnyCallbackStruct *call_data);
static C_PROTO_3(void kuipList_OK,
                         Widget  widget,
                         int     tag,
                         XmAnyCallbackStruct *callback_data);
static C_PROTO_3(void kxterm_raise_OK,
                         Widget  widget,
                         int     tag,
                         XmAnyCallbackStruct *callback_data);
static C_PROTO_3(void km_do_mail,
                         Widget w,
                         XtPointer client_data,
                         XmAnyCallbackStruct *call_data);
C_PROTO_3(void km_kmenu_OK,
                         Widget  widget,
                         caddr_t tag,
                         XmAnyCallbackStruct *callback_data);
static C_PROTO_3(void ListHelp,
                         Widget w,
                         char **help,
                         XmAnyCallbackStruct *call_data);


/***********************************************************************
 *                                                                     *
 *   Forward declarations of global functions.                         *
 *                                                                     *
 ***********************************************************************/
C_PROTO_3(void km_deiconify_appShell,
                         Widget w,
                         XtPointer client_data,
                         XmAnyCallbackStruct *call_data);
C_PROTO_1(void km_create_windows,
                         char *cname);
C_PROTO_0(void define_buttons);
C_PROTO_2(void km_buttons_callb, char **, int);
C_PROTO_0(int  km_enter_loop);
C_PROTO_0(void km_realize_windows);
C_PROTO_3(void km_do_help,
                         Widget w,
                         char *cval,
                         XmAnyCallbackStruct *call_data);
C_PROTO_3(void km_help_activate,
                         Widget  widget,
                         caddr_t tag,
                         XmAnyCallbackStruct *callback_data);
C_PROTO_3(void          km_focus_window,
                         Widget widget,
                         caddr_t tag,
                         XEnterWindowEvent *ev);
C_PROTO_1(void create_menu_help,
                         Widget parent);
C_PROTO_2(void kxterm_show_panel, char **, int);
C_PROTO_2(void kxterm_set_panel, char **, int);
C_PROTO_2(void kxterm_set_echo_mode, char **, int);
C_PROTO_2(void kxterm_iconify, char **, int);
C_PROTO_2(void kxterm_raise, char **, int);
C_PROTO_2(void kxterm_help, char **, int);
C_PROTO_2(void kxterm_geometry, char **, int);
C_PROTO_2(void kxterm_about, char **, int);
C_PROTO_2(void kxterm_mail, char **, int);


/***********************************************************************
 *                                                                     *
 *   Forward declarations of extern functions.                         *
 *                                                                     *
 ***********************************************************************/
extern C_PROTO_3(Widget km_create_Browser,
                         Widget w,
                         Widget clone_parent,
                         XmAnyCallbackStruct *call_data);
extern C_PROTO_1(Widget create_kuipList, Widget);
extern C_PROTO_5(int start_kxterm,
                         char *title,
                         char *geometry,
                         char *font,
                         char *text_font,
                         char *main_prompt);
extern C_PROTO_1(void km_WarnCB,
                         String message);


/***********************************************************************
 *                                                                     *
 *   X Window error handler.                                           *
 *                                                                     *
 ***********************************************************************/
int error_handler (dp, error_event)
    Display *dp;
    XErrorEvent *error_event;
{
   char text[1000];
   int il = 1000;
   unsigned char errc = error_event->error_code;    /* error code */

   if (errc == Success)                             /* everything's okay */
       return (0);
   else if (errc == BadRequest)
        printf (" *** X Protocol Error: bad request code \n");
   else if (errc == BadValue)
        printf (" *** X Protocol Error: BadValue \n");
   else if (errc == BadWindow)
        printf (" *** X Protocol Error: BadWindow \n");
   else if (errc == BadPixmap)
        printf (" *** X Protocol Error: BadPixmap \n");
   else if (errc == BadAtom)
        printf (" *** X Protocol Error: BadAtom \n");
   else if (errc == BadCursor)
        printf (" *** X Protocol Error: BadCursor \n");
   else if (errc == BadFont)
        printf (" *** X Protocol Error: BadFont \n");
   else if (errc == BadMatch)
        printf (" *** X Protocol Error: BadMatch \n");
   else if (errc == BadDrawable)
        printf (" *** X Protocol Error: BadDrawable \n");
   else if (errc == BadAccess)
        printf (" *** X Protocol Error: BadAccess \n");
   else if (errc == BadAlloc)
        printf (" *** X Protocol Error: BadAlloc \n");
   else if (errc == BadColor)
        printf (" *** X Protocol Error: BadColor \n");
   else if (errc == BadGC)
        printf (" *** X Protocol Error: BadGC \n");
   else if (errc == BadIDChoice)
        printf (" *** X Protocol Error: BadIDChoice \n");
   else if (errc == BadName)
        printf (" *** X Protocol Error: BadName \n");
   else if (errc == BadLength)
        printf (" *** X Protocol Error: BadLength \n");
   else if (errc == BadImplementation)
        printf (" *** X Protocol Error: BadImplementation \n");

   XGetErrorText (dp, errc, text, il);
   printf ("%s \n", text);

   return 0;
}



/******************************************************************************/
/* km_create_windows (class) : MOTIF initialization (has to be called first)  */
/*             class (input) : class name (if "" then "Mkuip")                */
/******************************************************************************/
void km_create_windows(cname)
    char *cname;
{
    IntC     argc;
    int      i, iterm;
    char    *argv[15];
    char     title[MAX_string];

    String  *fallbacks, *kuip_fallbacks;

    Boolean km_kxterm = True;
    Boolean nombr = False;

    if (klnkaddr.graf_info_C == NULL) {
       /* no graphics window */
        kuip_fallbacks = k_fallbacks;
    } else {
        kuip_fallbacks = kg_fallbacks;
    }

    fallbacks = kuip_fallbacks;

    if( klnkaddr.user_FallBk_C != NULL ) {
      /* allow application to specify its own fallbacks */

      String *user_fallbacks = (*klnkaddr.user_FallBk_C)();

      /* concatenate Kuip and user fallbacks */
      if( user_fallbacks != NULL ) {

        int  fbcnt = 0;

        for( i = 0; kuip_fallbacks[i] != NULL; i++ )
          fbcnt++;
        for( i = 0; user_fallbacks[i] != NULL; i++ )
          fbcnt++;
        fallbacks = (String*)XtMalloc( (fbcnt+1) * sizeof(String) );

        fbcnt = 0;
        for( i = 0; kuip_fallbacks[i] != NULL; i++ )
          fallbacks[fbcnt++] = kuip_fallbacks[i];
        for( i = 0; user_fallbacks[i] != NULL; i++ )
          fallbacks[fbcnt++] = user_fallbacks[i];
        fallbacks[fbcnt] = NULL;
      }
    }

    /*
     *  Initialize toolkit
     */
    if (cname != NULL) {
        char *cargv[15];
        int cargc;
        k_parse_args(cname, cargv, &cargc);
        /* Parse arguments */
        for( i = 0; i < cargc; i++) {
           if (strcmp(cargv[i], "-nokxterm") == 0) km_kxterm = False;
           if (strcmp(cargv[i], "-nombrowser") == 0) nombr = True;
           free (cargv[i]);
        }
        if (cargc > 0) {
           /* Keep only the 1st word */
           char *tmp;
           if ( (tmp = strstr( cname, " " )) != NULL ) tmp[0] = '\0';
        }
    }

    strcpy (class_name, cname != NULL ? cname : "Mkuip");

    sprintf (title,"%s Executive Window", class_name);
    argc       = 0;
    argv[argc] = NULL;

    UxTopLevel = XtAppInitialize(&UxAppContext, class_name,
                                 NULL, 0, &argc, argv, fallbacks, NULL, 0);

    UxDisplay      = XtDisplay(UxTopLevel);
    UxScreen       = XDefaultScreen(UxDisplay);
    UxScreenWidth  = DisplayWidth(UxDisplay, UxScreen);
    UxScreenHeight = DisplayHeight(UxDisplay, UxScreen);

    /*
     * We set the geometry of UxTopLevel so that dialogShells
     * that are parented on it will get centered on the screen
     * (if defaultPosition is true).
     */
    XtVaSetValues(UxTopLevel,
                  XmNmappedWhenManaged, False,
                  XmNallowShellResize, True,
                  XmNx, DisplayWidth(UxDisplay, UxScreen)/2,
                  XmNy, DisplayHeight(UxDisplay, UxScreen)/2,
                  XmNwidth, 1,
                  XmNheight, 1,
                  NULL);

    /* XSynchronize(UxDisplay, TRUE); */ /* for debugging only */

    /*
     * Set default colormap and get application resources.
     */
    km_Display = UxDisplay;
    km_colormap = DefaultColormap(km_Display,DefaultScreen(km_Display));

    XtGetApplicationResources(UxTopLevel, &srec,
                              kuip_resources, XtNumber(kuip_resources),
                              NULL, 0);

    if (srec.no_kxterm) km_kxterm = False;
    if (srec.no_mbrowser) nombr = True;

    /*
     * Create a shadow kxterm topLevelShell. This widget is not mapped
     * but is used to popup dialogs as if kxterm were the parent.
     * When kxterm is moved or resized it sends a message (#@geometry...)
     * to the application which in turn calls the routine kxterm_geometry
     * to move and resize also the shadow kxterm toplevel widget.
     */
    if (km_kxterm) {
        /* to have the same resources (colours, fonts, ...) as Main Browser : */
#if EXEC_RES
    /* to have the same resources (colours, fonts, ...) as Executive Window : */
    km_toplevel = XtAppCreateShell(NULL, "KXterm",
                                   applicationShellWidgetClass,
                                   km_Display, NULL, 0);
#else
        km_toplevel = XtAppCreateShell(NULL, class_name,
                                       applicationShellWidgetClass,
                                       km_Display, NULL, 0);
#endif
    } else {
        km_toplevel = XtAppCreateShell(NULL, "kxterm_shadow",
                                       topLevelShellWidgetClass,
                                       km_Display, NULL, 0);
        /* unbuffered I/O */
        setbuf(stdin,  NULL);
        setbuf(stdout, NULL);
        kc_flags.use_kxterm = 0;
#if 0
        fprintf(stderr,"\
You have selected the \"noKxterm\" resource.\n\
Your present terminal window will handle I/O.\n");
#endif
    }

    /* don't free geometry it is not copied?! */
    XtVaSetValues(km_toplevel,
                  XmNmappedWhenManaged, False,
                  XmNgeometry, srec.kxterm_geometry,
                  NULL);
    XtRealizeWidget(km_toplevel);

    /*
     * Create Main Browser.
     */
    if (!nombr) {

    km_main_browser = (Widget)km_create_Browser((Widget)NULL, (Widget)NULL,
                                                (XmAnyCallbackStruct *)NULL);
    XmUpdateDisplay(km_main_browser);

    /* Initialize global data */
    init_data();

    /* Create menu "Option" (general entries) */
    kuipControlM = create_option_menu(km_main_browser);

    /* Create "Commands" menu (with all commands) */
    create_command_menu(km_main_browser);

    } /* ... if (!nombr) */

    /*
     * Start kxterm and redirect stdin, stdout and stderr.
     */
    if (km_kxterm) {
      iterm = start_kxterm(title, srec.kxterm_geometry, srec.kxterm_font,
                           srec.kxterm_text_font, kc_flags.echo_prompt);
    } else { /* no_kxterm */
      iterm = -1;
    }

    if (klnkaddr.user_TopWid_C != NULL)
       /* tell application that kxterm can now be initialized */
       (*klnkaddr.user_TopWid_C)("kxterm", (Widget)NULL);

    /* user defined buttons/menus */
    if (kmbuttons != NULL) define_buttons();

    /* set system current working directory in kxterm */
    if (kc_flags.use_kxterm) {
       char cwd[256];
       if (getcwd(cwd, (sizeof cwd))) {
          char *s = malloc(strlen(cwd) + 6);
          sprintf(s, "lcd(%s)", cwd);
          send_single_kxterm_cmd(s);
          free(s);
       }
    }

    /* Store kxterm widget (if any) */
    if (iterm >= 0) km_store_appShell(NULL, title);

    /* Store Main Browser widget */
    if (!nombr)
    km_store_appShell(km_main_browser, NULL);

    /* Set up information requested by HIGZ to open the Motif graphics window */
    set_HIGZ();
}

static void set_HIGZ()
{
extern C_PROTO_5(Window km_graph_open,
                         int wkid,
                         int x_pos,
                         int y_pos,
                         int w_dim,
                         int h_dim);
extern C_PROTO_1(void km_graph_init,
                         int wkid);
extern C_PROTO_1(void km_graph_close,
                         int wkid);

    /* Set up information requested by HIGZ to open the Motif graphics window */
    if (klnkaddr.graf_info_C != NULL)
    /* if NULL -> no graphics window */
        (*klnkaddr.graf_info_C)
                  (km_Display, km_graph_open, km_graph_init, km_graph_close);
}

/******************************************************************************/
/* User defined buttons and menus (in the CDF)                                */
/******************************************************************************/
void define_buttons()
{
     KmButton *kmbut;
     char line0[512], line1[512];
     Boolean exist;
     int i, num_actions, nkm, nbm;

     KxtermActionList actions = (KxtermActionList) malloc(
                                                     sizeof(KxtermActionsRec) );
     char **kxterm_menu = (char **) malloc ( sizeof (char *) );
     char **browser_menu = (char **) malloc ( sizeof (char *) );
     int na = 0;
     int nk = 0;
     int nb = 0;

     for (kmbut = kmbuttons; kmbut != NULL; kmbut = kmbut->next) {
          /* add buttons/menus (kxterm and browser) */
          line0[0] = line1[0] = '\0';
          if ( (kmbut->flags & KmButtSeparator) != 0) {
             sprintf (line0, "M:%s: Sep :                      : separator",
                      kmbut->menu);
          }
          sprintf (line1, "M:%s: %s : %s%s : button : %s: %s: %s",
                   kmbut->menu, kmbut->label, ESCAPE, kmbut->action,
                   kmbut->mnemo, kmbut->accel, kmbut->accel_text);
          if ( (kmbut->flags & KmButtBrowser) != 0) {
            /* fill browser_menu array */
            if (line0[0] != '\0') {
                browser_menu = (char **) realloc ( (char *)browser_menu,
                                                   (nb+1) * sizeof (char *) );
                browser_menu[nb++] = strdup (line0);
            }
            browser_menu = (char **) realloc ( (char *)browser_menu,
                                               (nb+1) * sizeof (char *) );
            browser_menu[nb++] = strdup (line1);
          } else {
            /* fill kxterm_menu array (default) */
            if (line0[0] != '\0') {
                kxterm_menu = (char **) realloc ( (char *)kxterm_menu,
                                                  (nk+1) * sizeof (char *) );
                kxterm_menu[nk++] = strdup (line0);
            }
            kxterm_menu = (char **) realloc ( (char *)kxterm_menu,
                                              (nk+1) * sizeof (char *) );
            kxterm_menu[nk++] = strdup (line1);
          }
          /* add corresponding actions */
          exist = False;
          for (i=0; i<na; i++) {
              /* check if new actions */
              if (strcasecmp( kmbut->action, actions[i].string ) == 0 )
                  exist = True;
          }
          if (!exist) {
              actions = (KxtermActionList) realloc ( actions,
                                         (na+1) * sizeof (KxtermActionsRec) );
              actions[na].string = strdup (kmbut->action);
              if (kmbut->call_C != NULL)
                  actions[na].proc = (KxtermActionProc)kmbut->call_C;
              else
                  actions[na].proc  = km_buttons_callb;
              na++;
          }
     }

     /* terminate all arrays with NULL */
     actions = (KxtermActionList) realloc ( actions,
                                  (na+1) * sizeof (KxtermActionsRec) );
     actions[na].string = NULL;
     actions[na].proc = NULL;
     browser_menu = (char **) realloc ( (char *)browser_menu,
                                         (nb+1) * sizeof (char *) );
     browser_menu[nb++] = NULL;
     kxterm_menu = (char **) realloc ( (char *)kxterm_menu,
                                        (nk+1) * sizeof (char *) );
     kxterm_menu[nk++] = NULL;
     num_actions = na;
     nkm = nk;
     nbm = nb;

     send_kxterm_cmd (kxterm_menu);
     km_add_menu (browser_menu);
     kxterm_add_actions (actions);

     for (na=0; na<num_actions; na++) {
         free (actions[na].string);
     }
     free ( (KxtermActionList) actions );
     for (nk=0; nk<nkm; nk++)
         free (kxterm_menu[nk]);
     for (nb=0; nb<nbm; nb++)
         free (browser_menu[nb]);
     free ( (char*) kxterm_menu);
     free ( (char*) browser_menu);
}


/***********************************************************************
 *                                                                     *
 *   Action routine for user defined buttons when action is defined as *
 *   a command.                                                        *
 *                                                                     *
 ***********************************************************************/
void km_buttons_callb( argv, argc )
     char **argv;
     int argc;
{
     char *button_cmd;
     int i;

     if (argc) {
         button_cmd = strdup (argv[0]);
         for (i = 1; i < argc; i++)
              button_cmd = mstr2cat( button_cmd, " ", argv[i]);
         exec_cmd_string (button_cmd, 1, km_display_cmdpan);
     }
}

/***********************************************************************
 *                                                                     *
 *   Initialize global data.                                           *
 *                                                                     *
 ***********************************************************************/
static void init_data ()
{
    int k;

    km_charset = XmSTRING_DEFAULT_CHARSET;

   /* Get foreground and background values for main-browser */
   XtVaGetValues(km_main_browser, XmNforeground, &km_foreground,
                                   XmNbackground, &km_background,
                                   NULL);
   /*
    * Load help_font (only once) for HELP (use a mono spaced font).
    */
   if (help_font == NULL) {
       help_font_struct = XLoadQueryFont( km_Display, srec.Help_font);
       if (help_font_struct == NULL)
           printf ("%s Warning: Font loaded for HELP (%s) is NULL \n",
                   main_prompt, srec.Help_font);
       else
           help_font = XmFontListCreate (help_font_struct,km_charset);
   }

   /*
    * Initialize the application data structures
    * and the application GLOBAL variables
    */
   for (k = 0; k < MAX_APP_SHELL; k++) {
        appShell[k] = NULL;
        appShell_title[k] = NULL;
   }
   km_xcur_pos = 0;
   km_ycur_pos = 0;
   print_box_widget = NULL;
   do_exec = True;  /* do not execute command at <CR> (push EXECUTE button) */
   do_bell = True;  /* ring bell when grabbing (set_grab) */
   if (ServerVendor(km_Display) != NULL &&
       strncmp(ServerVendor(km_Display),"DECWINDOWS",10) == 0) {
       fl_decwindows = 1;
   } else {
       fl_decwindows = 0;
   }
   sprintf (main_prompt, "%s > ", class_name);
   km_browser_refresh = False;
}

/***********************************************************************
 *                                                                     *
 *   Set up help menu's.                                               *
 *                                                                     *
 ***********************************************************************/
void create_menu_help(parent)
     Widget parent;
{
    Widget   helpB1, helpB2, helpB3, helpB4, helpB5, helpB6;
    char     hlab[128];

    /*
     *   find and restore the context for Browser
     *   (to get to the widgets belonging to this dialog)
     */
    UxKuipBrowserContext = (_UxCkuipBrowser *)UxGetContext(parent);

    /* delete default help button in HelpPdMenu */
    XtDestroyWidget(HelpPdMenu_help);

    /* add new help button's */
    sprintf(hlab,"On %s", class_name);
    helpB1 = XtVaCreateManagedWidget( "helpB1",
                        xmPushButtonGadgetClass, HelpPdMenu,
                        RES_CONVERT( XmNmnemonic, "O" ),
                        RES_CONVERT( XmNlabelString, hlab ),
                        NULL );
    XtAddCallback (helpB1,XmNactivateCallback,
                   (XtCallbackProc)km_do_help,(XtPointer)"HELP_EXE");

    sprintf(hlab,"On %s Resources", class_name);
    helpB6 = XtVaCreateManagedWidget( "helpB6",
                        xmPushButtonGadgetClass, HelpPdMenu,
                        RES_CONVERT( XmNmnemonic, "R" ),
                        RES_CONVERT( XmNlabelString, hlab ),
                        NULL );
    XtAddCallback (helpB6,XmNactivateCallback,
                   (XtCallbackProc)km_do_help,(XtPointer)"HELP_EXE_RESOURCES");

    helpB2 = XtVaCreateManagedWidget( "helpB2",
                        xmPushButtonGadgetClass, HelpPdMenu,
                        RES_CONVERT( XmNmnemonic, "K" ),
                        RES_CONVERT( XmNlabelString, "On Kuip Resources"),
                        NULL );
    XtAddCallback (helpB2,XmNactivateCallback,
                   (XtCallbackProc)km_do_help,(XtPointer)"HELP_RESOURCES");

    helpB4 = XtVaCreateManagedWidget( "helpB4",
                        xmPushButtonGadgetClass, HelpPdMenu,
                        RES_CONVERT( XmNmnemonic, "B" ),
                        RES_CONVERT( XmNlabelString, "On Browser" ),
                        NULL );
    XtAddCallback (helpB4,XmNactivateCallback,
                   (XtCallbackProc)km_do_help,(XtPointer)"HELP_BROWSER");

    helpB3 = XtVaCreateManagedWidget( "helpB3",
                        xmPushButtonGadgetClass, HelpPdMenu,
                        RES_CONVERT( XmNmnemonic, "P" ),
                        RES_CONVERT( XmNlabelString, "On Panel" ),
                        NULL );
    XtAddCallback (helpB3,XmNactivateCallback,
                   (XtCallbackProc)km_do_help,(XtPointer)"HELP_PANEL");

    helpB5 = XtVaCreateManagedWidget( "helpB5",
                        xmPushButtonGadgetClass, HelpPdMenu,
                        RES_CONVERT( XmNmnemonic, "F" ),
                        RES_CONVERT( XmNlabelString, "On System Functions" ),
                        NULL );
    XtAddCallback (helpB5,XmNactivateCallback,
                   (XtCallbackProc)km_do_help,(XtPointer)"FUNCTIONS");

    if (user_mail_C) {
       Widget   button;

       button = XtVaCreateManagedWidget( "helpB6",
                        xmSeparatorGadgetClass, HelpPdMenu,
                        NULL );

       sprintf(hlab, "Mail %s Developers", class_name);
       button = XtVaCreateManagedWidget( "helpB7",
                        xmPushButtonGadgetClass, HelpPdMenu,
                        RES_CONVERT( XmNmnemonic, "M" ),
                        RES_CONVERT( XmNlabelString, hlab ),
                        NULL );
       XtAddCallback(button, XmNactivateCallback,
                     (XtCallbackProc)km_do_mail, NULL);
    }
}

/******************************************************************************/
/* kireal : MOTIF realization (has to be called last)                         */
/******************************************************************************/
void km_realize_windows()
{
    XEvent event;
    XColor    color_defs[2];
    Pixmap    hourglass_pixmap, hourglass_mask_pixmap;


   /* initialize cursors */
   if (srec.main_cs == -1)
       km_main_cursor = XCreateFontCursor(km_Display, XC_left_ptr);
   else
       km_main_cursor = XCreateFontCursor(km_Display, srec.main_cs);
   if (srec.graph_cs == -1)
       km_cross_cursor = XCreateFontCursor(km_Display, XC_crosshair);
   else
       km_cross_cursor = XCreateFontCursor(km_Display, srec.graph_cs);
   if (srec.input_cs == -1)
       km_input_cursor = km_main_cursor;
   else
       km_input_cursor = XCreateFontCursor(km_Display, srec.input_cs);
   if (srec.help_cs == -1)
       km_help_cursor = XCreateFontCursor(km_Display, XC_question_arrow);
   else
       km_help_cursor = XCreateFontCursor(km_Display, srec.help_cs);
   if (srec.busy_cs == -1)
       km_busy_cursor = XCreateFontCursor(km_Display, XC_watch);
   else
       km_busy_cursor = XCreateFontCursor(km_Display, srec.busy_cs);
   if (srec.blck_cs == -1)
       km_blck_cursor = XCreateFontCursor(km_Display, XC_dot);
   else
       km_blck_cursor = XCreateFontCursor(km_Display, srec.blck_cs);
   if (srec.selc_cs == -1)
       km_selc_cursor = XCreateFontCursor(km_Display, XC_hand2);
   else
       km_selc_cursor = XCreateFontCursor(km_Display, srec.selc_cs);

   /* build wait cursor with hourglass pixmap */
   color_defs[0].pixel = BlackPixel(km_Display, UxScreen);
   color_defs[1].pixel = WhitePixel(km_Display, UxScreen);
   XQueryColors(km_Display, km_colormap, color_defs, 2);

   hourglass_pixmap = XCreatePixmapFromBitmapData(km_Display,
                         DefaultRootWindow(km_Display), (char*)hourglass_bits,
                         hourglass_width, hourglass_height,
                         WhitePixel(km_Display, UxScreen),
                         BlackPixel(km_Display, UxScreen), 1);
   hourglass_mask_pixmap = XCreatePixmapFromBitmapData(km_Display,
                         DefaultRootWindow(km_Display),
                         (char*)hourglass_mask_bits,
                         hourglass_mask_width, hourglass_mask_height,
                         WhitePixel(km_Display, UxScreen),
                         BlackPixel(km_Display, UxScreen), 1);
   if (srec.wait_cs == -1)
       km_wait_cursor = XCreatePixmapCursor(km_Display, hourglass_pixmap,
                                   hourglass_mask_pixmap,
                                   &color_defs[0], &color_defs[1],
                                   hourglass_x_hot, hourglass_y_hot);
   else
       km_wait_cursor = XCreateFontCursor(km_Display, srec.wait_cs);

   /*
    *   register a procedure to be called on warning messages
    *   to discard the passive grab warnings generated by Motif
    */
   (void)XtAppSetWarningHandler(UxAppContext, (XtErrorHandler)km_WarnCB);

    /*
     *  Realize the widget tree.
     */
    XtRealizeWidget(UxTopLevel);

    km_all_cursor(KM_WAIT_CURSOR);

    /* Flush events to display the browser (and not wait for XtMainLoop) */
    while ( XEventsQueued( km_Display, QueuedAlready) > 1) {
            XtAppNextEvent( UxAppContext, &event );
            XtDispatchEvent (&event); }
    XSync(km_Display, False);

}


/*
 * execute IDLE command if no other activity
 */
static XtIntervalId idle_timer = 0;

static
void idle_callback( XtPointer client_data, XtIntervalId *id )
{
  /*
   * If "IDLE 0" was typed in Kxterm without any X-event on the application
   * side then the timer was not removed.  Therefore we better check here
   * again that there is still an idle command active.
   */
  if( kc_value.idle_time > 0 ) {
    ku_exel( kc_value.idle_cmd );
    idle_timer = XtAppAddTimeOut( UxAppContext, kc_value.idle_time * 1000,
                                 idle_callback, kc_value.idle_cmd );
  }
  else
    idle_timer = 0;
}


/******************************************************************************/
/* kiloop : give control to MOTIF event loop (has to be called last)          */
/******************************************************************************/
int km_enter_loop()
{
#ifdef vms

  /*
   * The signal handler calls LIB$SIG_TO_RET which lets km_enter_loop return
   * the error number ( 1 == SS$_NORMAL )
   */
  VAXC$ESTABLISH( signal_handler );

#endif

   km_all_cursor (KM_MAIN_CURSOR);
   while( 1 ) {
     XEvent event;

     if( kc_value.idle_time > 0 )
       idle_timer = XtAppAddTimeOut( UxAppContext, kc_value.idle_time * 1000,
                                    idle_callback, kc_value.idle_cmd );
     else
       idle_timer = 0;

     XtAppNextEvent( UxAppContext, &event );

     if( idle_timer != 0 )
       XtRemoveTimeOut( idle_timer );

     XtDispatchEvent( &event );
     if (km_panel_context) km_ContextHelp();
   }
   return 1;
}


/******************************************************************************/
/* Create "Options" menu for all optional settings.                           */
/******************************************************************************/
static Widget create_option_menu(parent)
    Widget parent;
{
    int narg;
    Arg arglist[2];
    Widget id_pdm;
    Widget PdMenu[15];

    /*
     *   find and restore the context for Browser
     *   (to get to the widgets belonging to this dialog)
     */
    UxKuipBrowserContext = (_UxCkuipBrowser *)UxGetContext(parent);

    /*  create pulldown_menu */
    id_pdm = XmCreatePulldownMenu (browserMenu,"OptionsPdMenu",arglist,0);


    /* create pulldown_menu_entry "Options" */
    XtVaCreateManagedWidget( "optionsMenu",
                                xmCascadeButtonGadgetClass, browserMenu,
                                XmNsubMenuId, id_pdm,
                                XmNmnemonic, XK_O,
                                RES_CONVERT( XmNlabelString, "Options" ),
                                NULL );


/*  create list of push_buttons and separators for menu "Option"             */
/*  create pulldown_menu for "Raise Window" (with list of appl. shells)       */
    narg = 0;
#if XmREVISION == 99 /* problem with REVISION 2 ! */
    XtSetArg(arglist[narg], XmNtearOffModel, XmTEAR_OFF_ENABLED); narg++;
#endif
    kuipControlShellM = XmCreatePulldownMenu
                        (id_pdm,"ControlShellM",arglist,narg);
/*  create pulldown_menu_entry                                               */
    kuipControlShell = XtVaCreateManagedWidget( "ControlShell",
                                xmCascadeButtonGadgetClass, id_pdm,
                                XmNsubMenuId, kuipControlShellM,
                                XmNmnemonic, XK_R,
                                RES_CONVERT( XmNlabelString, "Raise Window" ),
                                NULL );

    XtVaCreateManagedWidget("PdSep0",
                                xmSeparatorGadgetClass, id_pdm,
                                NULL);

    PdMenu[3] =  XtVaCreateManagedWidget("PdMenu3",
                                 xmPushButtonGadgetClass, id_pdm,
                                 RES_CONVERT( XmNlabelString,
                                              "Command Argument Panel... " ),
                                 XmNmnemonic,XK_C,
                                 NULL );
    XtAddCallback (PdMenu[3],XmNactivateCallback,
                   (XtCallbackProc)get_command_panel, (XtPointer)0);

    return (id_pdm);
}

/************************************************************************
 *                                                                      *
 * Create "Commands" menu for all commands defined in the CDF.          *
 *                                                                      *
 ************************************************************************/
static void scan_menu_list(w, pdm, call_data)
   Widget w, pdm;
   XmAnyCallbackStruct *call_data;
{
    static Boolean init = False;

    if (!init) {
       Widget widget_menu[MAX_MENU_LEVEL];
       KmMenu **menu_list = root_menu_list("");
       KmMenu *menu;
       int i;

       init = True;

       widget_menu[0] = pdm;
       for (i = 0; (menu = menu_list[i]) != NULL; i++) {
          int narg;
          Arg arglist[5];
          Widget id_pdme, id_pdm;

          Widget pw = widget_menu[menu->level-1];
          char *name = str0dup(menu->name);
          KmCommand *cmd;
          int nbut;

          Widget *id_pushb = (Widget *) malloc ( sizeof (Widget));


          strlower (name+1);

          /* Do not display menu "Motif" (contains helps only) */
          if (name != NULL && strcmp(name, "Motif") == 0) continue;

          /* create pulldown_menu */
          narg = 0;
#if XmVERSION > 1 || XmREVISION >= 2
          XtSetArg(arglist[narg], XmNtearOffModel, XmTEAR_OFF_ENABLED); narg++;
#endif
          id_pdm = XmCreatePulldownMenu (pw,"id_pdmC",arglist,narg);
          id_pdme = XtVaCreateWidget( "id_pdmeC",
                        xmCascadeButtonGadgetClass, pw,
                        XmNsubMenuId, id_pdm,
                        RES_CONVERT( XmNlabelString, name ),
                        NULL );
          widget_menu[menu->level] = id_pdm;

          free (name);

          /*  create and manage push_button(s) */
          nbut = 0;
          for (cmd = menu->cmds; cmd != NULL; cmd = cmd->next) {
             char *cmd_name = str0dup(cmd->name);

             strlower (cmd_name+1);

             id_pushb = (Widget *) realloc ( id_pushb,
                                                 (nbut+1) * sizeof (Widget) );


             id_pushb[nbut] = XtVaCreateWidget( "id_pushb",
                                xmPushButtonGadgetClass, id_pdm,
                                RES_CONVERT( XmNlabelString, cmd_name ),
                                NULL );
             XtAddCallback (id_pushb[nbut],XmNactivateCallback,
                                    (XtCallbackProc)command_panel_activate,
                                    (XtPointer)cmd->path);
             free (cmd_name);
             nbut++;
          }
          XtManageChild (id_pdme);
          if (nbut > 0) XtManageChildren (id_pushb, nbut);
          free ((Widget *) id_pushb);
       }
       free( menu_list );
    }
}

/***********************************************************************
 *                                                                     *
 *   Execute an application defined menu button.                       *
 *                                                                     *
 ***********************************************************************/
static void application_cmd_cb( w, cmd, cbs )
     Widget w;
     char *cmd;
     XmAnyCallbackStruct *cbs;
{
   handle_kxterm_action(cmd);
}

/***********************************************************************
 *                                                                     *
 *   Execute an application defined toggle menu button. These commands *
 *   must recognize the 'on' and 'off' options.                        *
 *                                                                     *
 ***********************************************************************/
static void application_toggle_cb( w, cmd, cbs )
     Widget w;
     char *cmd;
     XmToggleButtonCallbackStruct *cbs;
{
   char  *s;

   s = XtMalloc(strlen(cmd) + 10);
   if (cbs->set)
      sprintf(s, "%s(ON)", cmd);
   else
      sprintf(s, "%s(OFF)", cmd);

   handle_kxterm_action(s);

}


/***********************************************************************
 *                                                                     *
 *   Handle menu building escape commands.                             *
 *                                                                     *
 ***********************************************************************/
static void add_entry (entry)
     char *entry;
{
   char      *cmd, *s, *menu, *menu_name;
   Widget     pulldown;
   MenuItem  *menu_item;

    /*
     *   find and restore the context for Browser
     *   (to get to the widgets belonging to this dialog)
     */
    UxKuipBrowserContext = (_UxCkuipBrowser *)UxGetContext(km_main_browser);

    /* Get menu label */
    cmd = entry;
    if (strncasecmp(cmd, "M:", 2)) return;
    cmd += 2;
    if ((s = strchr(cmd, ':'))) {
       menu      = XtCalloc(s-cmd+1, 1);
       menu_name = XtCalloc(s-cmd+7+1, 1);
       strncpy(menu, cmd, s-cmd);
       sprintf(menu_name, "*%sPdMenu", menu);

      menu_item = (MenuItem *)XtMalloc(MAX_ITEMS*sizeof(MenuItem));
      if (km_add_item(s+1, application_cmd_cb, application_toggle_cb,
                         menu_item, MAX_ITEMS)) {

         /* does menu already exist? */
         pulldown = XtNameToWidget(browserMenu, menu_name);
         if (pulldown) {
            /* add to existing pulldown menu */
            km_build_menu(pulldown, XmMENU_PULLDOWN, menu, menu[0],
                      menu_item, True);
         } else {
            /* create new pulldown menu */
            km_build_menu(browserMenu, XmMENU_PULLDOWN, menu, menu[0],
                     menu_item, False);
         }
      }
      XtFree((char *)menu_item);
      XtFree(menu);
      XtFree(menu_name);
   }
}


/***********************************************************************
 *                                                                     *
 *   Handle application defined menu for the main browser.             *
 *                                                                     *
 ***********************************************************************/
void km_add_menu (menu)
     char **menu;
{
    int    i;

   for (i = 0; menu[i]; i++)
       add_entry (menu[i]);
}

/************************************************************************
 *                                                                      *
 * Create "Commands" menu for all commands defined in the CDF.          *
 *                                                                      *
 ************************************************************************/
static void create_command_menu(parent)
   Widget parent;
{
    Widget id_pdme, id_pdm;
    Arg    arglist[2];
    int    narg;

    /*
     *   find and restore the context for Browser
     *   (to get to the widgets belonging to this dialog)
     */
    UxKuipBrowserContext = (_UxCkuipBrowser *)UxGetContext(parent);

    /* create pulldown_menu_entry "Commands" for all commands */
    narg = 0;
#if XmVERSION > 1 || XmREVISION >= 2
    XtSetArg(arglist[narg], XmNtearOffModel, XmTEAR_OFF_ENABLED); narg++;
#endif
    id_pdm = XmCreatePulldownMenu
                     (browserMenu, "CommandsPdMenu", arglist, narg);

    id_pdme = XtVaCreateManagedWidget( "id_pdme",
                        xmCascadeButtonGadgetClass, browserMenu,
                        RES_CONVERT( XmNmnemonic, "C" ),
                        XmNsubMenuId, id_pdm,
                        RES_CONVERT( XmNlabelString, "Commands" ),
                        NULL );

    /*
     * Delay creation of the KUIP menu structure till the user selects the
     * Command menu
     */
    XtAddCallback(id_pdme, XmNcascadingCallback,
                  (XtCallbackProc) scan_menu_list, id_pdm);
}

/******************************************************************************/
/* void   Function km_print_box (pw, x, y, message, itype)                    */
/*        MessageBox widget                                                   */
/*                   pw             (input)  : parent widget                  */
/*                 x, y             (input)  : Position                       */
/*              message             (input)  : message string                 */
/*                itype             (input)  : dialogType                     */
/*                                             0  -->  DIALOG_ERROR           */
/*                                             1  -->  DIALOG_INFORMATION     */
/*                                             2  -->  DIALOG_MESSAGE         */
/*                                             3  -->  DIALOG_QUESTION        */
/*                                             4  -->  DIALOG_WARNING         */
/*                                             5  -->  DIALOG_WORKING         */
/* return Widget widget_id                   : widget identifier              */
/******************************************************************************/
C_DECL_5(void km_print_box,
    Widget, pw,
    Position, x,
    Position, y,
    char *, message,
    int, itype)
{
    Widget kid[6];
    int narg;
    Arg arglist[20];
    XmString      xmstr;
    char cval[MAX_string];

/*  If parent widget is not yet realized just put a print message             */
    if (!XtIsRealized (pw)) {
       printf ("%s %s\n", main_prompt, message);
       return; }

/*  If not yet done destroy previous print box widget                         */
    if ( print_box_widget != NULL ) XtDestroyWidget (print_box_widget);
    km_reset_appShell (print_box_widget);
    print_box_widget = NULL;


/*  create MessageDialog                                                      */
    narg = 0;
/*  Set position                                                              */
    if (x != 0 || y != 0)  {
        XtSetArg(arglist[narg],XmNdefaultPosition,False); narg++;
        XtSetArg(arglist[narg],XmNx,x); narg++;
        XtSetArg(arglist[narg],XmNy,y); narg++; }
    XtSetArg(arglist[narg],XmNmessageString,
             XmStringCreateLtoR(message, km_charset)); narg++;
    if (itype == 0) {
       XtSetArg(arglist[narg],XmNdialogType,XmDIALOG_ERROR); narg++; }
    else if (itype == 1) {
       XtSetArg(arglist[narg],XmNdialogType,XmDIALOG_INFORMATION); narg++; }
    else if (itype == 2) {
       XtSetArg(arglist[narg],XmNdialogType,XmDIALOG_MESSAGE); narg++; }
    else if (itype == 3) {
       XtSetArg(arglist[narg],XmNdialogType,XmDIALOG_QUESTION); narg++; }
    else if (itype == 4) {
       XtSetArg(arglist[narg],XmNdialogType,XmDIALOG_WARNING); narg++; }
    else if (itype == 5) {
       XtSetArg(arglist[narg],XmNdialogType,XmDIALOG_WORKING); narg++; }
/*  Add following line if you want to change the title (can be a new argument)*/
    sprintf (cval, "%s Message", class_name);
    xmstr = XmStringCreateLtoR(cval, XmSTRING_DEFAULT_CHARSET);
    XtSetArg(arglist[narg], XmNdialogTitle, xmstr); narg++;
    print_box_widget = XmCreateMessageDialog (pw,"print_box",arglist,narg);
    XmStringFree(xmstr);


/*  Unmanage unneeded children.                                               */
    kid[0] = XmMessageBoxGetChild (print_box_widget, XmDIALOG_CANCEL_BUTTON);
    kid[1] = XmMessageBoxGetChild (print_box_widget, XmDIALOG_HELP_BUTTON);
    XtUnmanageChildren (kid, 2);


/*  Add callback(s) and manage print_box                                      */
    XtAddCallback (print_box_widget,XmNokCallback,
                   (XtCallbackProc)destroy_print_box,NULL);
    XtManageChild (print_box_widget);
    set_grab(print_box_widget, 1);
}


/******************************************************************************/
/* void set_grab (widget_id, flgrab)                                          */
/******************************************************************************/
/*
 * Set grab on widget and ring the bell
*/
static void set_grab (widget_id, flgrab)
       Widget widget_id;
       int flgrab;  /* flgrab = 0 -> change only cursor shape to input        */
{
    XKeyboardState kbstate;
    int percent;

    km_set_cursor (widget_id, km_input_cursor);
    if (flgrab == 0) return;

/*  grab widget (do not accept any event until user does acknowledge)         */
/*  XtAddGrab (widget_id,True,True); */

/*  Set Bell if requested  (default)                                          */
    if (!do_bell) return;

/*  Set subresources for : gBell (bell volume when grabbing)                  */
    percent = srec.bell_vol;

    if (percent < 0) {
/*      Use some reasonnable default                                          */
        XGetKeyboardControl( km_Display, &kbstate );
        percent = kbstate.bell_percent; }
    if (percent > 0) XBell( km_Display, percent );


    return;
}


/******************************************************************************/
/*                                                                            */
/***                      CALLBACKS ROUTINES                                ***/
/*                                                                            */
/******************************************************************************/


/***********************************************************************
 *                                                                     *
 *  Display command panel if command is valid                          *
 *  or propose list of commands if it is ambiguous                     *
 *                                                                     *
 *  flag = 0 --> command panel                                         *
 *         1 --> command panel help                                    *
 *                                                                     *
 ***********************************************************************/
static C_DECL_4(void show_widget,
      char *, command,
      Position, xr,
      Position, yr,
      int, flag)
{
    KmCommand **last_cmd_list;
    KmCommand *cmd;



    /* Get command path according to command string */
    if (command == NULL || command[0] == '\0') return;
    cmd = search_command (command, &last_cmd_list);

    if (cmd != NULL) {
        /* Command is ok */
        display_widget (cmd, xr, yr, flag);
    } else {
        km_cmd_list (command, last_cmd_list, flag);
    } /* end if (cmd != NULL) ... */

}

/***********************************************************************
 *                                                                     *
 *  Display list.                                                      *
 *                                                                     *
 ***********************************************************************/
void km_display_list (items, help_items, data)
    char **items, **help_items;
    ListData data;
{
    char *item;
    char cval[MAX_string];
    int nitems;

    static char **help = NULL;

    if (km_list_id != NULL) {
        XtDestroyWidget (XtParent(km_list_id));
        km_reset_appShell (km_list_id);
    }
    km_list_id = NULL;
    if (help != NULL) free ((char **)help );
    help = NULL;

    if (items != NULL) {
       Widget kuipListSelection;

       /* command is ambiguous ... */
       if (help_items == NULL)
           km_list_id = (Widget) create_kuipList(km_toplevel);
       else
           km_list_id = (Widget) create_kuipListHelp(km_toplevel);
       /* Set title */
       sprintf (cval, "%s List", class_name);
       XtVaSetValues(XtParent(km_list_id), XmNtitle, cval, NULL);
       km_store_appShell (km_list_id, cval);
       XtVaSetValues(km_listSBox,
                     RES_CONVERT( XmNlistLabelString, data.listLabel ),
                     RES_CONVERT( XmNselectionLabelString, data.label ),
                     NULL);
       /* Fill list */
       kuipListSelection = XmSelectionBoxGetChild (km_listSBox, XmDIALOG_LIST);
       for (nitems = 0; (item = items[nitems]) != NULL; nitems++) {
            XmString   xmstr;

            xmstr = XmStringCreate(item, XmSTRING_DEFAULT_CHARSET);
            XmListAddItem(kuipListSelection, xmstr, 0);
            XmStringFree(xmstr);
       }

       if (help_items != NULL) {
           help = (char**)malloc( sizeof(char*) );
           for (nitems = 0; help_items[nitems] != NULL; nitems++) {
                help = (char**)realloc( (char*)help,
                                        (nitems+1) * sizeof(char*) );
                help[nitems] = strdup (help_items[nitems]);
           }
           help = (char**)realloc( (char*)help, (nitems+1) * sizeof(char*) );
           help[nitems] = NULL;
#if 0
Unfortunately the following does not seem to work ...
--> replace it with "helpCallback".
           XtAddCallback (kuipListSelection, XmNsingleSelectionCallback,
                          (XtCallbackProc)help_list_items, help);
#endif
       } /* end ... if (help_items != NULL) */

       if (data.OKcallback)
           XtAddCallback (km_listSBox,XmNokCallback,
                          (XtCallbackProc)data.OKcallback,
                          data.OKcallback_data);
       XtAddCallback (km_listSBox,XmNcancelCallback,
                      (XtCallbackProc)destroy_list_id, NULL);
/*
       XtAddCallback (km_listSBox,XmNhelpCallback,
                      (XtCallbackProc)km_help_activate, data.help);
*/
       XtAddCallback (km_listSBox,XmNhelpCallback,
                      (XtCallbackProc)ListHelp, help);
       /* This is to avoid the window to disappear under kxterm */
       XtAddEventHandler (km_list_id, FocusChangeMask, False,
                          (XtEventHandler)km_focus_window, NULL);
       if (nitems <= 3) XtVaSetValues(km_list_id,
                                      XmNheight, (Dimension)280, NULL);
       UxPopupInterface(km_list_id, XtGrabNone);
    }
}

static void ListHelp ( w, help, call_data)
    Widget w;
    char **help;
    XmAnyCallbackStruct *call_data;
{
    Widget kuipListSelection;
    int *pos;
    int ipos;
    int i;

    int npos = 0;

    /*
     *   find and restore the context for ListHelp
     *   (to get to the widgets belonging to this dialog)
     */
    UxKuipListHelpContext = (_UxCkuipListHelp *)UxGetContext(w);


    kuipListSelection = XmSelectionBoxGetChild (w, XmDIALOG_LIST);
    XmListGetSelectedPos (kuipListSelection , &pos, &npos);
    ipos = pos[0] - 1;

    if ( help != NULL &&
         help[ipos] != NULL ) XmTextSetString (km_listText, help[ipos]);
}

/***********************************************************************
 *                                                                     *
 *  Display list of possible commands if it is ambiguous               *
 *                                                                     *
 *  flag = 0 --> command panel                                         *
 *         1 --> command panel help                                    *
 *                                                                     *
 ***********************************************************************/
void km_cmd_list (command, cmd_list, flag)
    char *command;
    KmCommand **cmd_list;
    int flag;
{
    Widget kuipListSelection;
    KmCommand *cmd;
    char cval[MAX_string];
    int i;

    if (km_list_id != NULL) {
        XtDestroyWidget (XtParent(km_list_id));
        km_reset_appShell (km_list_id);
    }
    km_list_id = NULL;

    if (cmd_list == NULL) {
       /* command does not exist ... */
       sprintf (cval, "Cannot find command : %s", command);
       km_warn_user(km_toplevel, cval, "");
    } else {
       char *help = (char *) "Select item in the List !";

       /* command is ambiguous ... */
       km_list_id = (Widget) create_kuipList(km_toplevel);
       /* Set title */
       sprintf (cval, "%s List", class_name);
       XtVaSetValues(XtParent(km_list_id), XmNtitle, cval, NULL);
       km_store_appShell (km_list_id, cval);
       /* Fill list */
       kuipListSelection = XmSelectionBoxGetChild (km_listSBox, XmDIALOG_LIST);
       for (i = 0; (cmd = cmd_list[i]) != NULL; i++) {
            XmString   xmstr;

            xmstr = XmStringCreate(cmd->path, XmSTRING_DEFAULT_CHARSET);
            XmListAddItem(kuipListSelection, xmstr, 0);
            XmStringFree(xmstr);
       } /* end for ... */
       XtAddCallback (km_listSBox,XmNokCallback,
                      (XtCallbackProc)kuipList_OK, (XtPointer)flag);
       XtAddCallback (km_listSBox,XmNcancelCallback,
                      (XtCallbackProc)destroy_list_id, NULL);
       XtAddCallback (km_listSBox,XmNhelpCallback,
                      (XtCallbackProc)km_help_activate, help);
       UxPopupInterface(km_list_id, XtGrabNone);
    } /* end if (cmd_list == NULL) ... */
}

static C_DECL_4(void display_widget,
      KmCommand *, cmd,
      Position, xr,
      Position, yr,
      int, flag)
{
      if (flag == 0) {
          /* Enter Command ... (force display of panel) */
          km_xcur_pos = xr; km_ycur_pos = yr;
          exec_cmd_string (cmd->path, -1, km_display_cmdpan);
      } else if (flag == 1) {
          /* Command Panel Help ... */
          char *helptext = fmt_cmd_help( cmd, 0 );
          km_display_sctext (cmd->path, helptext, xr, yr, 1);
          free( helptext );
      }
}


static void destroy_print_box( widget, tag, callback_data )
      Widget   widget;
      caddr_t tag;
      XmAnyCallbackStruct *callback_data;
{
    if (print_box_widget != NULL ) XtDestroyWidget(print_box_widget);
    km_reset_appShell (print_box_widget);
    print_box_widget = NULL;
}


/***********************************************************************
 *                                                                     *
 *   Action routine which will be called by handle_kxterm_actions to   *
 *   popup a command panel.                                            *
 *                                                                     *
 ***********************************************************************/
void kxterm_show_panel( argv, argc )
     char **argv;
     int argc;
{
    FILE *ppf;
    char fname[MAX_stringl], message[MAX_stringl];
    char *panel_name;
    char exstr[MAX_stringl];


    panel_name = km_wait_input ("Give KUIP macro file name",
                                "panel", "HELP_PANEL");
    if (panel_name == NULL) return;
/*  check existence of macro file                                        */
    sprintf (fname, "%s.kumac", panel_name);
    if ((ppf = fopen (fname,"r")) == NULL) {
         sprintf (message,"Cannot find file %s", fname);
         km_print_box (km_toplevel, 0, 0, message,0);
         fclose (ppf);
         return; }
    free (panel_name);
    sprintf (exstr,"/MACRO/EXEC %s", fname);
    exec_cmd_string (exstr, 0, NULL);
}


/***********************************************************************
 *                                                                     *
 *   Action routine which will be called by handle_kxterm_actions to   *
 *   popup a new command panel.                                        *
 *                                                                     *
 ***********************************************************************/
void kxterm_set_panel( argv, argc )
     char **argv;
     int argc;
{
    ku_exel ("/KUIP/SET_SHOW/NEWPANEL");
}


static void get_command_panel( widget, tag, callback_data )
      Widget   widget;
      int      tag;
      XmAnyCallbackStruct *callback_data;
{
    char *command;
    Position xr, yr;
    int flag = tag;

    command = km_wait_input ("Give Command Name", "", "HELP_GET_COMMAND_PANEL");
    if (command == NULL) return;
    km_get_curpos (callback_data->event,&xr,&yr);
    show_widget (command, xr, yr, flag);

    free (command);
}

/***********************************************************************
 *                                                                     *
 *   Action routine which will be called by handle_kxterm_actions to   *
 *   turn on/off command echoing.                                      *
 *                                                                     *
 ***********************************************************************/
void kxterm_set_echo_mode( argv, argc )
     char **argv;
     int argc;
{
   if (!strcasecmp(argv[0], "on"))
      kc_flags.echo_command = 1;
   else
      kc_flags.echo_command = 0;
}

#if 0
/***********************************************************************
 *                                                                     *
 *   Add callback on each list item for "singleSelection"              *
 *                                                                     *
 ***********************************************************************/
static void help_list_items(w, help, call_data)
    Widget w;
    char **help;
    XmAnyCallbackStruct *call_data;
{
    Widget kuipListSelection;
    int *pos;
    int ipos;
    int npos = 0;

    /*
     *   find and restore the context for ListHelp
     *   (to get to the widgets belonging to this dialog)
     */
    UxKuipListHelpContext = (_UxCkuipListHelp *)UxGetContext(w);


    kuipListSelection = XmSelectionBoxGetChild (w, XmDIALOG_LIST);
    XmListGetSelectedPos (kuipListSelection , &pos, &npos);
    ipos = pos[0] - 1;

    if (help[ipos] != NULL XmTextSetString (km_listText, help[ipos]);
}
#endif

/***********************************************************************
 *                                                                     *
 *   Destroy km_list_id (widget issued from kuipList)                  *
 *                                                                     *
 ***********************************************************************/
static void destroy_list_id(w, tag, call_data)
    Widget w;
    caddr_t tag;
    XmAnyCallbackStruct *call_data;
{
    km_destroy_list();
}

void km_destroy_list()
{
    if (km_list_id != NULL) {
        UxDestroyInterface(km_list_id);
        km_reset_appShell (km_list_id);
        km_list_id = NULL;
    }
}


/*
 *      Display panel corresponding to command
 */
static void command_panel_activate( widget, cmd, data )
      Widget   widget;
      char     *cmd;
      XmAnyCallbackStruct *data;
{
    /* Force display of panel */
    km_get_curpos (data->event,&km_xcur_pos,&km_ycur_pos);
    exec_cmd_string (cmd, -1, km_display_cmdpan);
}



/******************************************************************************/
/*  km_store_appShell : store application shell (for Iconify)                 */
/******************************************************************************/
void km_store_appShell(w, title)
   Widget w;
   char *title;
{
   Arg       arglist[2];
   Widget    PdMenu;
   char     *tit;
   int       i;


   if (w == NULL && (title)) { /* Executive window */
      /* Create entry in pulldown menu "Application Shell" */
      appShell_title[0] = strdup(title);
      if (!kuipControlM) return;
      PdMenu = XmCreatePushButtonGadget
                   (kuipControlShellM,appShell_title[0],arglist,0);
      XtAddCallback(PdMenu,XmNactivateCallback,
                    (XtCallbackProc)appShell_activate, (XtPointer)-1);
      XtManageChild(PdMenu);
      return;
   } else {
      for (i = 1; i < MAX_APP_SHELL; i++) {
         if (appShell[i] == NULL) {
            appShell[i] = w;

            if (title)
               appShell_title[i] = strdup(title);
            else {
               XtVaGetValues(km_GetTrueShell(w), XmNtitle, &tit, NULL);
               appShell_title[i] = strdup(tit);
            }

            /* Create entry in pulldown menu "Application Shell" */
            if (!kuipControlM) break;
            PdMenu = XmCreatePushButtonGadget
                         (kuipControlShellM,appShell_title[i],arglist,0);
            XtAddCallback(PdMenu,XmNactivateCallback,
                          (XtCallbackProc)appShell_activate, (XtPointer)i);
            XtManageChild(PdMenu);
            break;
         } /* end ... if (appShell[i] == NULL) */
      } /* end ... for (i = 1; i < MAX_APP_SHELL; i++) */
      if (i == MAX_APP_SHELL-1)
         km_print_box (km_toplevel, 0, 0, "Please erase some windows ...",4);
   }
}

/******************************************************************************/
/*  km_reset_appShell : retrieve application shell from the list (for Iconify)*/
/******************************************************************************/
void km_reset_appShell(w)
   Widget w;
{
   int       narg;
   Arg       arglist[2];
   Widget    PdMenu;
   int       i;


   for (i = 1; i < MAX_APP_SHELL; i++) {
      if (w == appShell[i]) {
         appShell[i] = NULL;
         appShell_title[i] = NULL;
         break;
      }
   }

   if (!kuipControlM) return;

   /* Reset pulldown_menu "Application Shell" */
   if (kuipControlShellM != NULL) XtDestroyWidget (kuipControlShellM);

   narg = 0;
#if XmREVISION == 99 /* problem with REVISION 2 ! */
   XtSetArg(arglist[narg], XmNtearOffModel, XmTEAR_OFF_ENABLED); narg++;
#endif
   kuipControlShellM = XmCreatePulldownMenu
                       (kuipControlM,"ControlShellM",arglist,narg);
   /* set XmNsubMenuId attribute for pulldown menu entry */
   XtVaSetValues(kuipControlShell, XmNsubMenuId, kuipControlShellM, NULL);

   if (appShell_title[0] != NULL) {
      /* Executive window */
      PdMenu = XmCreatePushButtonGadget
                   (kuipControlShellM,appShell_title[0],arglist,0);
      XtAddCallback (PdMenu,XmNactivateCallback,
                     (XtCallbackProc)appShell_activate, (XtPointer)-1);
      XtManageChild(PdMenu);
   }
   for( i = 1; i < MAX_APP_SHELL; i++) {
        if (appShell[i] != NULL) {
           /* reset entries in pulldown menu "Application Shell" */
           PdMenu = XmCreatePushButtonGadget
                        (kuipControlShellM,appShell_title[i],arglist,0);
           XtAddCallback (PdMenu,XmNactivateCallback,
                          (XtCallbackProc)appShell_activate, (XtPointer)i);
           XtManageChild(PdMenu);
        }
    }
}

/***********************************************************************
 *                                                                     *
 *   Iconify all TopLevelShells.                                       *
 *                                                                     *
 ***********************************************************************/
static void iconify_appShell(w, client_data, call_data)
   Widget w;
   XtPointer client_data;
   XmAnyCallbackStruct *call_data;
{
   int i;

   for( i = 0; i < MAX_APP_SHELL; i++) {
      if (appShell[i])
         km_IconifyShell(appShell[i]);
   }
}

/***********************************************************************
 *                                                                     *
 *   DeIconify all TopLevelShells.                                     *
 *                                                                     *
 ***********************************************************************/
void km_deiconify_appShell(w, client_data, call_data)
   Widget w;
   XtPointer client_data;
   XmAnyCallbackStruct *call_data;
{
   int i;

   for (i = 0; i < MAX_APP_SHELL; i++) {
      if (appShell[i] && km_IsShellIconic(appShell[i]))
         km_DeIconifyShell(appShell[i]);
   }
}

/***********************************************************************
 *                                                                     *
 *   Action routine which will be called by handle_kxterm_actions to   *
 *   iconify or de-iconify all top level shells.                       *
 *                                                                     *
 ***********************************************************************/
void kxterm_iconify( argv, argc )
     char **argv;
     int argc;
{
    if (!strcasecmp(argv[0], "unmap"))
       iconify_appShell((Widget)NULL, (XtPointer)0,
                           (XmAnyCallbackStruct *)NULL);
    else
       km_deiconify_appShell((Widget)NULL, (XtPointer)0,
                           (XmAnyCallbackStruct *)NULL);
}

/***********************************************************************
 *                                                                     *
 *   Action routine which will be called by handle_kxterm_actions to   *
 *   raise any top level shells.                                       *
 *                                                                     *
 ***********************************************************************/
void kxterm_raise( argv, argc )
     char **argv;
     int argc;
{
     Widget kuipListSelection;
     char cval[MAX_string];
     int i;

     char *help = (char *) "Select the window you want to popup !";


     if (km_list_id != NULL) {
         XtDestroyWidget (XtParent(km_list_id));
         km_reset_appShell (km_list_id);
     }
     km_list_id = NULL;

     km_list_id = (Widget) create_kuipList(km_toplevel);
     /* Set title */
     sprintf (cval, "%s Window List", class_name);
     km_store_appShell (km_list_id, cval);
     XtVaSetValues(XtParent(km_list_id), XmNtitle, cval, NULL);
     XtVaSetValues(km_listSBox,
            RES_CONVERT( XmNlistLabelString, "List of Windows" ),
            RES_CONVERT( XmNselectionLabelString, "Window to be raised" ),
            NULL);
     /* Fill list */
     kuipListSelection = XmSelectionBoxGetChild (km_listSBox, XmDIALOG_LIST);
     for (i = 0; appShell_title[i] != NULL; i++) {
          XmString   xmstr;

          xmstr = XmStringCreate(appShell_title[i], XmSTRING_DEFAULT_CHARSET);
          XmListAddItem(kuipListSelection, xmstr, 0);
          XmStringFree(xmstr);
     } /* end for ... */
     XtAddCallback (km_listSBox,XmNokCallback,
                    (XtCallbackProc)kxterm_raise_OK, NULL);
     XtAddCallback (km_listSBox,XmNcancelCallback,
                    (XtCallbackProc)destroy_list_id, NULL);
     XtAddCallback (km_listSBox,XmNhelpCallback,
                    (XtCallbackProc)km_help_activate, help);
     UxPopupInterface(km_list_id, XtGrabNone);
}

/***********************************************************************
 *                                                                     *
 *   Action routine which will be called by handle_kxterm_actions to   *
 *   show help.                                                        *
 *                                                                     *
 ***********************************************************************/
void kxterm_help( argv, argc )
     char **argv;
     int argc;
{
    if (argc == 1) {
       char *cmd = XtMalloc(strlen(argv[0]) + 12);

       sprintf(cmd, "/KUIP/HELP %s", argv[0]);
       ku_exel (cmd);

       XtFree(cmd);
    }
}

/***********************************************************************
 *                                                                     *
 *   Action routine which will be called by handle_kxterm_actions to   *
 *   change the geometry of the km_toplevel shell. This action will    *
 *   be called whenever kxterm is moved or resized, so that            *
 *   km_toplevel can follow kxterm.                                    *
 *                                                                     *
 ***********************************************************************/
void kxterm_geometry( argv, argc )
     char **argv;
     int argc;
{
    Position    x, y;
    Dimension   width, height;

    if (argc == 4) {
       x      = (Position)  atoi(argv[0]);
       y      = (Position)  atoi(argv[1]);
       width  = (Dimension) atoi(argv[2]);
       height = (Dimension) atoi(argv[3]);

       XtVaSetValues(km_toplevel, XmNx, x, XmNy, y,
                     XmNwidth, width, XmNheight, height, NULL);
    }
}

/***********************************************************************
 *                                                                     *
 *   Action routine which will be called by handle_kxterm_actions to   *
 *   show the about... (logo) off the application.                     *
 *                                                                     *
 ***********************************************************************/
void kxterm_about( argv, argc )
     char **argv;
     int argc;
{
    if (user_logo_C) {
       (user_logo_C)(UxTopLevel);
    }
}

/***********************************************************************
 *                                                                     *
 *   Action routine which will be called by handle_kxterm_actions to   *
 *   show the "Mail Developers" dialog of the application.             *
 *                                                                     *
 ***********************************************************************/
void kxterm_mail( argv, argc )
     char **argv;
     int argc;
{
    if (user_mail_C) {
       (user_mail_C)(UxTopLevel);
    }
}

/***********************************************************************
 *                                                                     *
 *   Callback routine which will be called to                          *
 *   show the "Mail Developers" dialog of the application.             *
 *                                                                     *
 ***********************************************************************/
static void km_do_mail(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
    if (user_mail_C) {
       (user_mail_C)(UxTopLevel);
    }
}

/***********************************************************************
 *                                                                     *
 *   Set cursor shape for all TopLevelShells (except Graphics).        *
 *                                                                     *
 ***********************************************************************/
void km_all_cursor (icurs)
   int icurs;
{
   int i;
   Cursor kursor, gkursor;

   if (icurs == KM_MAIN_CURSOR) {
       kursor = km_main_cursor;
       gkursor = km_cross_cursor; }
   else if (icurs == KM_WAIT_CURSOR) {
       kursor = km_wait_cursor;
       gkursor = kursor; }
   else if (icurs == KM_CROSS_CURSOR) {
       kursor = km_cross_cursor;
       gkursor = kursor; }
   else if (icurs == KM_INPUT_CURSOR) {
       kursor = km_input_cursor;
       gkursor = kursor; }
   else if (icurs == KM_HELP_CURSOR) {
       kursor = km_help_cursor;
       gkursor = kursor; }
   else if (icurs == KM_BUSY_CURSOR) {
       kursor = km_busy_cursor;
       gkursor = kursor; }
   else if (icurs == KM_BLCK_CURSOR) {
       kursor = km_blck_cursor;
       gkursor = kursor; }
   else if (icurs == KM_SELC_CURSOR) {
       kursor = km_selc_cursor;
       gkursor = kursor; }

   for (i = 0; i < MAX_APP_SHELL; i++) {
      if (appShell[i] != NULL) {
         char *title = str0dup(appShell_title[i]);
         if( title == NULL || strstr(strupper(title), "GRAPHICS") == NULL )
            km_set_cursor (appShell[i], kursor);
         else
            km_set_cursor (appShell[i], gkursor);
         if( title != NULL )
            free (title);
      }
   }

   /* Kxterm */
   if (icurs == KM_MAIN_CURSOR)
       send_single_kxterm_cmd ("cursor(main)");
   else if (icurs == KM_WAIT_CURSOR)
       send_single_kxterm_cmd ("cursor(wait)");

}

/******************************************************************************/
/* void appShell_activate( widget, tag, callback_data )                       */
/******************************************************************************/
static void appShell_activate( widget, tag, callback_data )
        Widget  widget;
        int     tag;
        XmAnyCallbackStruct *callback_data;
{
   int i = tag;

   if (tag >= 0) {
     if (appShell[i] == NULL) return;
     UxPopupInterface(appShell[i], XtGrabNone);
     XMapRaised(km_Display, XtWindow(km_GetTrueShell(appShell[i])));
     XFlush(km_Display);
   } else {
     /* Executive window managed by kxterm */
     send_single_kxterm_cmd("raise");
   }
}

/******************************************************************************/
/* void kuipList_OK ( widget, tag, callback_data )                            */
/******************************************************************************/
static void kuipList_OK( widget, tag, callback_data )
        Widget  widget;
        int     tag;
        XmAnyCallbackStruct *callback_data;
{
    Widget       kuipListText;
    char         *command;
    char         cval[MAX_string];
    KmCommand    *cmd;
    Position     xr, yr;

    int flag = tag;


    /* Get selected value */
    kuipListText = XmSelectionBoxGetChild
                       (km_listSBox, XmDIALOG_TEXT);
    command = (char *) XmTextGetString (kuipListText);
    if (command != NULL) {
        /* Get command path according to command string */
        cmd = search_command (command, NULL);
        if (cmd != NULL) {
            /* Command is ok */
            km_get_curpos (callback_data->event,&xr,&yr);
            display_widget (cmd, xr, yr, flag);
        } else {
            /* command does not exist ... */
            sprintf (cval, "Cannot find command : %s", command);
            km_warn_user(km_toplevel, cval, "");
            XtFree (command);
            return;
        }
        XtFree (command);
    }

    km_destroy_list();
}

/******************************************************************************/
/* void km_kmenu_OK ( widget, tag, callback_data )                            */
/******************************************************************************/
void km_kmenu_OK( widget, tag, callback_data )
        Widget  widget;
        caddr_t tag;
        XmAnyCallbackStruct *callback_data;
{
    Widget       kuipListText;
    char *menu;

    char *path = (char *) tag;

    /* Get selected value */
    kuipListText = XmSelectionBoxGetChild (km_listSBox, XmDIALOG_TEXT);
    menu = (char *) XmTextGetString (kuipListText);
    /* Destroy list */
    km_destroy_list();
    if (menu != NULL) {
        char cmd[MAX_stringl];

        if (path != NULL)
            sprintf (cmd, "/KUIP/HELP %s/%s", path, menu);
        else
            sprintf (cmd, "/KUIP/HELP %s", menu);
        ku_exel (cmd);
        XtFree (menu);
    }
}

/******************************************************************************/
/* void kxterm_raise_OK ( widget, tag, callback_data )                        */
/******************************************************************************/
static void kxterm_raise_OK( widget, tag, callback_data )
        Widget  widget;
        int     tag;
        XmAnyCallbackStruct *callback_data;
{
    Widget       kuipListText;
    char         *title;
    int          k, index;

    /* Get selected value */
    kuipListText = XmSelectionBoxGetChild
                       (km_listSBox, XmDIALOG_TEXT);
    title = (char *) XmTextGetString (kuipListText);
    index = -1;
    if (title != NULL) {
       for (k = 0; k < MAX_APP_SHELL; k++) {
            if (appShell_title[k] == NULL) break;
            if (strcmp(title, appShell_title[k])  == 0) {
                index = k;
                break; }
       }
       XtFree (title);
    } /* end ...  if (title != NULL) */

    if (index >= 0) {
        if (index == 0)
            /* Executive window */
            appShell_activate (NULL, -1, NULL);
        else
            appShell_activate (NULL, index, NULL);
    }
}
