/*
 * $Id: mkdpan.c,v 1.3 2001/05/23 15:16:33 couet Exp $
 *
 * $Log: mkdpan.c,v $
 * Revision 1.3  2001/05/23 15:16:33  couet
 * - Closing KUIP panels with the command "pannel" crashed on LINUX. The
 *   following macro demonstrate the problem:
 *
 *   panel 0
 *   panel 1.01 'hello'
 *   panel 0 d 'first' 120x100+0+0
 *   panel 0 c 'first'
 *
 * Revision 1.2  2000/09/28 16:13:09  couet
 * - On LINUX: Mods to allow the close of KUIP panels. Closing KUIP panels
 *   crashed previously.
 *
 * Revision 1.1.1.1  1996/03/08 15:33:06  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/08 06/11/95  16.33.31  by  N.Cremel*/
/*-- Author :    N.Cremel   10/08/92*/
/******************************************************************************/
/*                                                                            */
/*           MOTIF-based library for KUIP - deck MKDPAN                       */
/*                                                                            */
/*           Basic Interface for "KUIP Panels " (like in "STYLE GP" )         */
/*           (connected to the file //KUIP/XMCLIB/KUXPAN for building the     */
/*           panels with Architect : "create_kuipPanel").                     */
/*                                                                            */
/******************************************************************************/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/klink.h"
#include "kuip/khash.h"
#include "kuip/kalias.h"

/* Global SEQUENCES (defined in MKCDES)                                       */
#include "mkincl.h"
#include "mkdims.h"
#include "kuip/mkutfu.h"
#include "mkutfm.h"
#include "mkutdm.h"
#define CONTEXT_MACRO_ACCESS 1
#include "mkuxpa.h"
#include "mkuxpp.h"
#include "mkuxsc.h"
#undef CONTEXT_MACRO_ACCESS
#include "kuip/mkterm.h"
#include "mksres.h"
#include "mksreg.h"
#include "kuip/browh1.h"
#include "browh2.h"
#include "icwid.h"
#include "icwidp.h"

/***********************************************************************
 *                                                                     *
 *   Forward declarations of local functions.                          *
 *                                                                     *
 ***********************************************************************/
#if 0
static C_PROTO_3(void panelfocusCallback,
                         Widget   widget,
                         caddr_t tag,
                         XmAnyCallbackStruct *callback_data);
#endif
static C_PROTO_3(void focus_panel,
                         Widget   widget,
                         char *name,
                         XEnterWindowEvent *ev);
static C_PROTO_2(void panel_save,
                         Widget pwidget,
                         char *name);
static C_PROTO_3(void okPNBCallback,
                         Widget widget,
                         Widget parent,
                         XmAnyCallbackStruct *data);
static C_PROTO_3(void savePNBCallback,
                         Widget  widget,
                         Widget  panel,
                         XmAnyCallbackStruct *callback_data);
static C_PROTO_3(void set_context_help,
                         Widget  widget,
                         Widget  panel,
                         XmAnyCallbackStruct *callback_data);
static C_PROTO_1(void display_ContextHelp,
                         char *label);
static C_PROTO_3(void destroy_dialog,
                         Widget w,
                         Widget dialog,
                         XmAnyCallbackStruct *call_data);
static C_PROTO_3(void viewPNBCallback,
                         Widget  widget,
                         Widget  panel,
                         XmToggleButtonCallbackStruct *callback_data);
static C_PROTO_1(char *find_hlp,
                         char *name);
static C_PROTO_1(void strconv,
                         char *str);
static void destroy_ptag(
            Widget w, caddr_t tag, XmAnyCallbackStruct *call_data);
#if 0
static void button_arm(
            Widget w, caddr_t tag, XmAnyCallbackStruct *call_data);
#endif
static void km_panel_view (int flag);


/***********************************************************************
 *                                                                     *
 *   Local datas.                                                      *
 *                                                                     *
 ***********************************************************************/

/* structure definition */
typedef struct {
        Widget buttonn;
        Widget buttoni;
        Widget buttonb;
        Widget buttonc;
        Widget button1;
        int row;
        int col;
        int rep;        /* ICON, NO_ICON */
        int flag;       /* panel button option (T=1) */
        KmPixmap pix;   /* icon pixmap */
        KmPixmap hi_pix; /* icon highlight pixmap */
               } ptag_str;  /* tag structure for km_panel_execute */

#define PAHLP_TABLE_SIZE 97     /* should be a prime */
static HashTable *kmpahlp;      /* panel helps (for button) table */

#define PAWID_TABLE_SIZE 97
static HashTable *kmpawid;      /* list of panels with their name */

static Widget panel_widget = NULL;  /* Widget id of the last created panel */

static int WM_x = 0;   /* width of Window Manager decoration */
static int WM_y = 0;   /* height of Window Manager decoration */

Widget context_panel;

#define BUT_TRANS "#override <Btn1Down>: Arm()\n<Btn1Up>: Activate() Disarm()"

/******************************************************************************/
/* int km_display_kpanel                                                      */
/*                                                                            */
/******************************************************************************/
int km_display_kpanel (name, geometry)
    char *name, *geometry;
{
    char title[MAX_string];
    char *panel_name, *panel_htitle;

    static char *focus_tag;
    static np = 0;

    if (name != NULL && name[0] != '\0') {
        panel_name = strdup (name);
    } else {
        char cnp[3];
        np++;
        sprintf (cnp,"%d",np);
        panel_name = str2dup ("kuipPanel", cnp);
    }
    panel_htitle = strdup (panel_name);
    strconv (panel_htitle);

    panel_widget = (Widget) create_kuipPanel();

    /* insert panel name in table kmpawid */
     if (panel_htitle != NULL) {
         if (kmpawid == NULL) {
             kmpawid = hash_create( PAWID_TABLE_SIZE );
             hash_config( kmpawid, "struct" );
         }
         hash_insert( kmpawid, panel_htitle, panel_widget, 0);
     }


    /*
     *   find and restore the context
     *   (to get to the widgets belonging to this dialog)
     */
    UxKuipPanelContext = (_UxCkuipPanel *)UxGetContext( panel_widget );


    /* Set title, icon name and pixmap */
    if (strstr(panel_name, class_name) == NULL)
        sprintf (title, "%s %s", class_name, panel_name);
    else
        strcpy (title, panel_name);
    if (geometry != NULL)
         XtVaSetValues(XtParent(panel_widget), XmNtitle, title,
                                       XmNiconName, title,
                                       XmNiconPixmap, km_pixmap,
                                       XmNdefaultPosition, False,
                                       XmNgeometry, geometry,
                                       NULL);
    else
         XtVaSetValues(XtParent(panel_widget), XmNtitle, title,
                                       XmNiconName, title,
                                       XmNiconPixmap, km_pixmap,
                                       NULL);

    if( klnkaddr.user_TopWid_C != NULL ) {
      /* pass widget ID to application */
       (*klnkaddr.user_TopWid_C)(panel_name, panel_widget);
    }

    km_store_appShell (panel_widget, title);

    /* Inhibit "Close" button" (non sensitive) when it is requested */
    if (!srec.panel_interactive) XtSetSensitive(km_okPNB, False);

#if defined(CERNLIB_LINUX)
    /* Inhibit the Close from the WM */
    XtVaSetValues(XtParent(panel_widget), XmNdeleteResponse, 
                  XmDO_NOTHING, NULL);
#endif
		  
    /* Create and manage panel buttons inside of RowColumn widget */
    km_panel_buttons (panel_widget, panel_name, by_ICON, 0);

    XtAddCallback (km_okPNB,XmNactivateCallback,
                   (XtCallbackProc)okPNBCallback, panel_widget);
    XtAddCallback (km_savePNB,XmNactivateCallback,
                   (XtCallbackProc)savePNBCallback, panel_widget);
    XtAddCallback (km_viewPNB_name,XmNvalueChangedCallback,
                   (XtCallbackProc)viewPNBCallback, panel_widget);
    XtAddCallback (km_viewPNB_icon,XmNvalueChangedCallback,
                   (XtCallbackProc)viewPNBCallback, panel_widget);
    XtAddCallback (km_viewPNB_both,XmNvalueChangedCallback,
                   (XtCallbackProc)viewPNBCallback, panel_widget);
    XtAddCallback (km_viewPNB_command,XmNvalueChangedCallback,
                   (XtCallbackProc)viewPNBCallback, panel_widget);
    XtAddCallback (km_viewPNB_command1,XmNvalueChangedCallback,
                   (XtCallbackProc)viewPNBCallback, panel_widget);
    XtAddCallback (km_helpPNB1,XmNactivateCallback,
                   (XtCallbackProc)km_do_help, (XtPointer)"HELP_PANEL");
    XtAddCallback (km_helpPNB2,XmNactivateCallback,
                   (XtCallbackProc)km_do_help, (XtPointer)"HELP_EDIT_PANEL");
    XtAddCallback (km_helpPNB3,XmNactivateCallback,
                   (XtCallbackProc)km_do_help, (XtPointer)"HELP_SAVE_PANEL");
    XtAddCallback (km_helpPNB4,XmNactivateCallback,
                   (XtCallbackProc)km_do_help, (XtPointer)"HELP_DOLLAR_PANEL");
    XtAddCallback (km_helpPNB5,XmNactivateCallback,
                   (XtCallbackProc)km_do_help, (XtPointer)"HELP_MINUS_PANEL");
    XtAddCallback (km_helpPNB6,XmNactivateCallback,
                   (XtCallbackProc)km_do_help, (XtPointer)"HELP_MINUS2_PANEL");
    XtAddCallback (km_helpCONTEXT,XmNactivateCallback,
                   (XtCallbackProc)set_context_help, panel_widget);
#if 0
    XtAddCallback (panel_widget,XmNfocusCallback,
                   (XtCallbackProc)panelfocusCallback, NULL);
#endif
    focus_tag = NULL;
    if (kc_window.panel_name != NULL) focus_tag = strdup (kc_window.panel_name);
    XtAddEventHandler(panel_widget, FocusChangeMask, False,
              (XtEventHandler)focus_panel, (XtPointer)focus_tag);


    UxPopupInterface(panel_widget, XtGrabNone);

    return 0;
}


/******************************************************************************/
/* int km_close_kpanel                                                        */
/*                                                                            */
/******************************************************************************/
int km_close_kpanel (name)
    char *name;
{
    Widget panel;

    if (name != NULL && name[0] != '\0' ) {
        char *panel_htitle = strdup (name);
        strconv (panel_htitle);

        panel = (Widget)hash_lookup (kmpawid, panel_htitle, NULL);
        hash_remove( kmpawid,  panel_htitle );
    } else
        panel =  panel_widget;

#if defined(CERNLIB_LINUX)
    UxPopdownInterface(panel);
#else
    UxDestroyInterface(panel);
#endif
    km_reset_appShell (panel);

    return 0;
}

#if 0
static void panelfocusCallback( widget, tag, callback_data )
      Widget   widget;
      caddr_t tag;
      XmAnyCallbackStruct *callback_data;
#endif
static void focus_panel(widget, name, ev)
      Widget widget;
      char *name;
      XEnterWindowEvent *ev;
{
    String new_geometry;
    Position xpos, ypos;
    Dimension dh, dw;
    int ih, iw, ix, iy;

    static int ifirst = 0;

    if (ifirst == 0) {
        /* First time only we get WM decoration size */
        ifirst = 1;
        XtVaGetValues(XtParent(widget), XmNgeometry, &new_geometry,
                                        XmNx, &xpos, XmNy, &ypos,
                                        XmNheight, &dh, XmNwidth, &dw,
                                        NULL);
        if (new_geometry != NULL) {
            int delta = 50;

            sscanf (new_geometry, "%dx%d+%d+%d", &iw, &ih, &ix, &iy);
            WM_x = xpos - ix;
            WM_y = ypos - iy;
            if (WM_x < 0 || WM_x > delta ) WM_x = 0;
            if (WM_y < 0 || WM_y > delta ) WM_y = 0;
       }
    }
    if (kc_window.panel_name != NULL) free (kc_window.panel_name);
    if (name != NULL) {
        kc_window.panel_name = strdup (name);
    } else {
        kc_window.panel_name = NULL;
    }
}

/*
 * Display panel buttons : in normal panels the display is done "by_ICON"
 * when panel is described with icons, otherwise it is "by_NAME".
 * Inside of a palette the display depends on view_flag
 * flag = 0 --> normal panel
 * flag = 1 --> palette
 *
 * Row Column / buttons widgets for panels:
 * View by name             --> km_rowcolPN / buttonn
 *      by icon             --> km_rowcolPI (managed at the beginning) / buttoni
 *      by icon & label     --> km_rowcolPB / buttonb
 *      by command          --> km_rowcolPC / buttonc
 *      by command (1 col.) --> km_rowcolP1 / button1
 *
 * Row Column / buttons widgets for palettes (multi_panel):
 * View by name             --> km_rowcolMN / buttonn
 *      by icon             --> km_rowcolMI (managed at the beginning) / buttoni
 *      by icon & label     --> km_rowcolMB / buttonb
 *      by command          --> km_rowcolMC / buttonc
 *      by command (1 col.) --> km_rowcolM1 / button1
 */
void km_panel_buttons (pwidget, name, view_flag, flag)
   Widget pwidget;
   char *name;
   int view_flag, flag;
{
    Widget RowColN, RowColI, RowColB, RowColC, RowCol1;
    Widget buttonn[MAX_button], buttoni[MAX_button], buttonb[MAX_button],
           buttonc[MAX_button], button1[MAX_button];
    Pixel arm_pixel;
    char wname[MAX_stringl];
    int i, j, ncol, nbut, nbut1;
    short nrow;
    Arg arglist[10];
    Cardinal narg;

    int margin = 0;

    /*
     *   find and restore the context
     *   (to get to the widgets belonging to this dialog)
     */
    if (flag == 0) {
        UxKuipPanelContext = (_UxCkuipPanel *)UxGetContext( pwidget );
        RowColN = km_rowcolPN;
        RowColI = km_rowcolPI;
        RowColB = km_rowcolPB;
        RowColC = km_rowcolPC;
        RowCol1 = km_rowcolP1;
    } else if (flag == 1) {
        UxPalPanelContext = (_UxCPalPanel *)UxGetContext( pwidget );
        RowColN = km_rowcolMN;
        RowColI = km_rowcolMI;
        RowColB = km_rowcolMB;
        RowColC = km_rowcolMC;
        RowCol1 = km_rowcolM1;
    }

    /* Get maximum number of colums and total nb. of buttons */
    nrow = kc_window.panel_rows;
    ncol = 0;
    nbut = 0;
    nbut1 = 0;
    for (i = 0; i < nrow; i++) {
        int nc = kc_window.panel_cols[i];
        if (kc_window.panel_cols[i] > ncol) ncol = kc_window.panel_cols[i];
        /* get "marginTop" for iconwidget (maximum icon height) */
        for (j = 0; j < nc; j++) {
         if ( kc_window.panel_icons[i][j] != NULL &&
              !km_strempty (kc_window.panel_icons[i][j]) ) {
                KmIcon *kmicon = find_kmicon(kc_window.panel_icons[i][j]);
                if (kmicon != NULL &&
                    kmicon->height > margin) margin = kmicon->height;
         }
         if (kc_window.panel_keys[i][j] != NULL &&
             !km_strempty (kc_window.panel_keys[i][j]) &&
             strncmp (kc_window.panel_keys[i][j], ".", 1) != 0 )
             nbut1++;

         nbut++;
        } /* end ... for (j = 0 ...) */
    } /* end ... for(i = 0 ...) */

    /* Set RowColumn attributes */
    XtVaSetValues(RowColN, XmNnumColumns, nrow, NULL);
    XtVaSetValues(RowColI, XmNnumColumns, nrow, NULL);
    XtVaSetValues(RowColB, XmNnumColumns, nrow, NULL);
    XtVaSetValues(RowColC, XmNnumColumns, nrow, NULL);
    if (nbut1 > 0) XtVaSetValues(RowCol1, XmNnumColumns, nbut1, NULL);

    /* Create buttons */
    nbut = 0;
    nbut1 = 0;
    for (i = 0; i < nrow; i++) {
        int nc = kc_window.panel_cols[i];
        XtTranslations   parsed = XtParseTranslationTable (BUT_TRANS);

        if (nbut >= MAX_button - 1) break;
        for (j = 0; j < nc; j++) {
         char *user_data;

         ptag_str *ptag = (ptag_str*)malloc(sizeof(ptag_str));
         ptag->row = i;
         ptag->col = j;
         ptag->pix = 0;
         ptag->hi_pix = 0;
         ptag->buttonn = NULL;
         ptag->buttoni = NULL;
         ptag->buttonb = NULL;
         ptag->buttonc = NULL;
         ptag->button1 = NULL;

         sprintf (wname, "NP%sB%d", name,j);
         if (kc_window.panel_keys[i][j] != NULL &&
             !km_strempty (kc_window.panel_keys[i][j]) &&
             strncmp (kc_window.panel_keys[i][j], ".", 1) != 0 ) {
             char *cmd = (char*)hash_lookup( kc_alias.cmd_table,
                                             kc_window.panel_keys[i][j], NULL);

            /*
             * XmNuserData --> label[#%pix]
             */
             user_data = strdup(kc_window.panel_keys[i][j]);
             buttoni[nbut] = XtVaCreateWidget (wname,
                       xmPushButtonWidgetClass, RowColI,
                       RES_CONVERT( XmNlabelString, kc_window.panel_keys[i][j]),
                       XmNuserData, strdup ( user_data ),
                       XmNlabelPixmap, XmUNSPECIFIED_PIXMAP,
                       NULL);
             buttonn[nbut] = XtVaCreateWidget (wname,
                       xmPushButtonWidgetClass, RowColN,
                       RES_CONVERT( XmNlabelString, kc_window.panel_keys[i][j]),
                       XmNuserData, strdup ( user_data ),
                       XmNlabelPixmap, XmUNSPECIFIED_PIXMAP,
                       NULL);
             narg = 0;
             XtSetArg (arglist[narg], XmNiconOffset, 1); narg++;
             XtSetArg (arglist[narg], XmNmarginTop, margin); narg++;
             XtSetArg (arglist[narg], XmNlabelString,
                      XmStringCreateSimple(kc_window.panel_keys[i][j])); narg++;
             XtSetArg (arglist[narg], XmNlabelPixmap, XmUNSPECIFIED_PIXMAP);
             narg++;
             XtSetArg (arglist[narg], XmNuserData, strdup ( user_data ));
             narg++;
             buttonb[nbut] = CreateIconWidget (RowColB, wname, arglist, narg);
             if (cmd == NULL) cmd =  kc_window.panel_keys[i][j];
             buttonc[nbut] = XtVaCreateWidget (wname,
                       xmPushButtonWidgetClass, RowColC,
                       RES_CONVERT( XmNlabelString, cmd),
                       XmNuserData, strdup ( user_data ),
                       XmNlabelPixmap, XmUNSPECIFIED_PIXMAP,
                       NULL);
             button1[nbut1] = XtVaCreateWidget (wname,
                       xmPushButtonWidgetClass, RowCol1,
                       RES_CONVERT( XmNlabelString, cmd),
                       XmNuserData, strdup ( user_data ),
                       XmNlabelPixmap, XmUNSPECIFIED_PIXMAP,
                       NULL);
             ptag->button1 = button1[nbut1];
             XtAddCallback (button1[nbut1],XmNactivateCallback,
                        (XtCallbackProc)km_panel_execute,(XtPointer)ptag);
             XtAddCallback (button1[nbut1], XmNdestroyCallback,
                  (XtCallbackProc)destroy_ptag, (XtPointer)ptag);
             nbut1 ++;
         } else {
             user_data = strdup("");
             buttoni[nbut] = XtVaCreateWidget (wname,
                       xmPushButtonWidgetClass, RowColI,
                       RES_CONVERT( XmNlabelString, ""),
                       XmNuserData, strdup ( user_data ),
                       XmNlabelPixmap, XmUNSPECIFIED_PIXMAP,
                       NULL);
             buttonn[nbut] = XtVaCreateWidget (wname,
                       xmPushButtonWidgetClass, RowColN,
                       RES_CONVERT( XmNlabelString, ""),
                       XmNuserData, strdup ( user_data ),
                       XmNlabelPixmap, XmUNSPECIFIED_PIXMAP,
                       NULL);
             narg = 0;
             XtSetArg (arglist[narg], XmNiconOffset, 1);  narg++;
             XtSetArg (arglist[narg], XmNmarginTop, margin);  narg++;
             XtSetArg (arglist[narg], XmNlabelString,
                                      XmStringCreateSimple("")); narg++;
             XtSetArg (arglist[narg], XmNlabelPixmap, XmUNSPECIFIED_PIXMAP);
             narg++;
             XtSetArg (arglist[narg], XmNuserData, strdup ( user_data ));
             narg++;
             buttonb[nbut] = CreateIconWidget (RowColB, wname, arglist, narg);
             buttonc[nbut] = XtVaCreateWidget (wname,
                       xmPushButtonWidgetClass, RowColC,
                       RES_CONVERT( XmNlabelString, ""),
                       XmNuserData, strdup ( user_data ),
                       XmNlabelPixmap, XmUNSPECIFIED_PIXMAP,
                       NULL);
         }

         XtOverrideTranslations (buttoni[nbut],
                     XtParseTranslationTable("<Btn3Down>:Arm()"));
         XtOverrideTranslations (buttoni[nbut],
                     XtParseTranslationTable("<Btn3Up>:Activate()"));

         if ( kc_window.panel_icons[i][j] != NULL &&
              !km_strempty (kc_window.panel_icons[i][j]) ) {
             KmIcon *kmicon = find_kmicon(kc_window.panel_icons[i][j]);
             char *label = strdup(kc_window.panel_keys[i][j]);

             if (strncmp (label, ".", 1) == 0 ) label[0] ='\0';

             XtVaGetValues (buttoni[nbut],
                             XmNarmColor, &arm_pixel, NULL);
             if (kmicon != NULL) {
                ptag->pix =
                kmicon->pix = XCreatePixmapFromBitmapData( km_Display,
                                DefaultRootWindow(km_Display),
                                kmicon->bitmap, kmicon->width, kmicon->height,
                                km_foreground, km_background,
                                DefaultDepthOfScreen(XtScreen(pwidget)) );
                ptag->hi_pix =
                kmicon->hi_pix = XCreatePixmapFromBitmapData( km_Display,
                                DefaultRootWindow(km_Display),
                                kmicon->bitmap, kmicon->width, kmicon->height,
                                km_foreground, arm_pixel,
                                DefaultDepthOfScreen(XtScreen(pwidget)) );
                /*
                 * [pix] in XmNuserData (label[#%pix] is used for panel_save
                 */
                user_data = mstr2cat (user_data,
                                      "#%", kc_window.panel_icons[i][j]);
                XtVaSetValues (buttoni[nbut],
                               XmNlabelPixmap, kmicon->pix,
                               XmNuserData, strdup ( user_data ),
                               NULL);
                if (view_flag == by_ICON)
                    XtVaSetValues (buttoni[nbut],
                                   XmNlabelType, XmPIXMAP,
                                   RES_CONVERT( XmNlabelString, label),
                                   NULL);
             } else {
                char message[MAX_stringl];

                sprintf (message,"cannot find bitmaps description for %s",
                         kc_window.panel_icons[i][j]);
                km_warn_user(km_toplevel, message, "");
                kmicon = find_kmicon("pixpan");
                if (kmicon != NULL) {
                    ptag->pix =
                    kmicon->pix = XCreatePixmapFromBitmapData( km_Display,
                                DefaultRootWindow(km_Display),
                                kmicon->bitmap, kmicon->width, kmicon->height,
                                km_foreground, km_background,
                                DefaultDepthOfScreen(XtScreen(pwidget)) );
                    ptag->hi_pix =
                    kmicon->hi_pix = XCreatePixmapFromBitmapData( km_Display,
                                DefaultRootWindow(km_Display),
                                kmicon->bitmap, kmicon->width, kmicon->height,
                                km_foreground, arm_pixel,
                                DefaultDepthOfScreen(XtScreen(pwidget)) );
                    /*
                     * [pix] in userData (label[#%pix] is used for panel_save
                     */
                    user_data = mstrcat (user_data, "#%pixpan");
                    XtVaSetValues (buttoni[nbut],
                                   XmNlabelPixmap, kmicon->pix,
                                   XmNuserData, strdup ( user_data ),
                                   NULL);
                    if (view_flag == by_ICON)
                        XtVaSetValues (buttoni[nbut],
                                       XmNlabelType, XmPIXMAP,
                                       RES_CONVERT( XmNlabelString, label),
                                       NULL);
                }
             }
             ptag->rep = ICON;
             narg = 0;
             XtSetArg (arglist[narg], XmNiconOffset, 1); narg++;
             /* XtSetArg (arglist[narg], XmNshadowThickness, 4); narg++; */
             XtSetArg (arglist[narg], XmNmarginTop, margin); narg++;
             XtSetArg (arglist[narg], XmNlabelString,
                      XmStringCreateSimple(kc_window.panel_keys[i][j])); narg++;
             XtSetArg (arglist[narg], XmNlabelPixmap, kmicon->pix); narg++;
             XtSetArg (arglist[narg], XmNuserData, strdup ( user_data ));
             narg++;
             buttonb[nbut] = CreateIconWidget (RowColB, wname, arglist, narg);

         } else {
             ptag->rep = NO_ICON;
         }
         if ( kc_window.panel_opts[i][j] != NULL &&
              strstr( kc_window.panel_opts[i][j], "T" ) != NULL ) {
             /* Toggle behavior */
             ptag->flag = 1;
         } else {
             ptag->flag = 0;
         }
         XtAugmentTranslations (buttonb[nbut], parsed);

         ptag->buttonn = buttonn[nbut];
         ptag->buttoni = buttoni[nbut];
         ptag->buttonb = buttonb[nbut];
         ptag->buttonc = buttonc[nbut];

#if 0
         XtAddCallback (buttoni[nbut],XmNarmCallback,
                        (XtCallbackProc)button_arm,(XtPointer)ptag);
#endif
         XtAddCallback (buttoni[nbut],XmNactivateCallback,
                        (XtCallbackProc)km_panel_execute,(XtPointer)ptag);
         XtAddCallback (buttoni[nbut], XmNdestroyCallback,
                  (XtCallbackProc)destroy_ptag, (XtPointer)ptag);
         XtAddCallback (buttonn[nbut],XmNactivateCallback,
                        (XtCallbackProc)km_panel_execute,(XtPointer)ptag);
         XtAddCallback (buttonn[nbut], XmNdestroyCallback,
                  (XtCallbackProc)destroy_ptag, (XtPointer)ptag);
         XtAddCallback (buttonb[nbut],XmNactivateCallback,
                        (XtCallbackProc)km_panel_execute,(XtPointer)ptag);
         XtAddCallback (buttonb[nbut], XmNdestroyCallback,
                  (XtCallbackProc)destroy_ptag, (XtPointer)ptag);
         XtAddCallback (buttonc[nbut],XmNactivateCallback,
                        (XtCallbackProc)km_panel_execute,(XtPointer)ptag);
         XtAddCallback (buttonc[nbut], XmNdestroyCallback,
                  (XtCallbackProc)destroy_ptag, (XtPointer)ptag);

         nbut ++;
        } /* end ... for (j = 0; j < nc; j++) */
        for (j = nc; j < ncol; j++) {

         ptag_str *ptag = (ptag_str*)malloc(sizeof(ptag_str));
         ptag->row = i;
         ptag->col = j;
         ptag->pix = 0;
         ptag->hi_pix = 0;
         ptag->buttonn = NULL;
         ptag->buttoni = NULL;
         ptag->buttonb = NULL;
         ptag->buttonc = NULL;
         ptag->button1 = NULL;

         sprintf (wname, "NP%sB%d", name,j);
         buttoni[nbut] = XtVaCreateWidget (wname,
                       xmPushButtonWidgetClass, RowColI,
                       RES_CONVERT( XmNlabelString, ""),
                       XmNuserData, strdup ( "" ),
                       NULL);
         buttonn[nbut] = XtVaCreateWidget (wname,
                       xmPushButtonWidgetClass, RowColN,
                       RES_CONVERT( XmNlabelString, ""),
                       XmNuserData, strdup ( "" ),
                       NULL);
         narg = 0;
         XtSetArg (arglist[narg], XmNiconOffset, 1);  narg++;
         XtSetArg (arglist[narg], XmNmarginTop, margin);  narg++;
         XtSetArg (arglist[narg], XmNlabelString, XmStringCreateSimple(""));
         narg++;
         XtSetArg (arglist[narg], XmNlabelPixmap, XmUNSPECIFIED_PIXMAP);
         narg++;
         XtSetArg (arglist[narg], XmNuserData, strdup ( "" ));  narg++;
         buttonb[nbut] = CreateIconWidget (RowColB, wname, arglist, narg);

         buttonc[nbut] = XtVaCreateWidget (wname,
                       xmPushButtonWidgetClass, RowColC,
                       RES_CONVERT( XmNlabelString, ""),
                       XmNuserData, strdup ( "" ),
                       NULL);
         XtAugmentTranslations (buttonb[nbut], parsed);
         XtOverrideTranslations (buttoni[nbut],
                     XtParseTranslationTable("<Btn3Down>:Arm()"));
         XtOverrideTranslations (buttoni[nbut],
                     XtParseTranslationTable("<Btn3Up>:Activate()"));

         ptag->buttonn = buttonn[nbut];
         ptag->buttoni = buttoni[nbut];
         ptag->buttonb = buttonb[nbut];
         ptag->buttonc = buttonc[nbut];
         ptag->rep = NO_ICON;
         ptag->flag = 0;
         XtAddCallback (buttoni[nbut],XmNactivateCallback,
                        (XtCallbackProc)km_panel_execute,(XtPointer)ptag);
         XtAddCallback (buttoni[nbut], XmNdestroyCallback,
                  (XtCallbackProc)destroy_ptag, (XtPointer)ptag);
         XtAddCallback (buttonn[nbut],XmNactivateCallback,
                        (XtCallbackProc)km_panel_execute,(XtPointer)ptag);
         XtAddCallback (buttonn[nbut], XmNdestroyCallback,
                  (XtCallbackProc)destroy_ptag, (XtPointer)ptag);
         XtAddCallback (buttonb[nbut],XmNactivateCallback,
                        (XtCallbackProc)km_panel_execute,(XtPointer)ptag);
         XtAddCallback (buttonb[nbut], XmNdestroyCallback,
                  (XtCallbackProc)destroy_ptag, (XtPointer)ptag);
         XtAddCallback (buttonc[nbut],XmNactivateCallback,
                        (XtCallbackProc)km_panel_execute,(XtPointer)ptag);
         XtAddCallback (buttonc[nbut], XmNdestroyCallback,
                  (XtCallbackProc)destroy_ptag, (XtPointer)ptag);
         nbut ++;
        } /* end ... for (j = nc; j < ncol; j++) */
    } /* end ... for (i = 0; i < nrow; i++) */

    XtManageChildren (buttoni, nbut);
    XtManageChildren (buttonn, nbut);
    XtManageChildren (buttonb, nbut);
    XtManageChildren (buttonc, nbut);
    if (nbut1 > 0) XtManageChildren (button1, nbut1);
}

static void destroy_ptag(Widget w,caddr_t tag,XmAnyCallbackStruct *call_data)
{
   ptag_str *stag = (ptag_str *)tag;
   free (stag);
}

#if 0
static void button_arm(
       Widget pbut_widget, caddr_t tag, XmAnyCallbackStruct *callback_data )
{
   ptag_str *stag = (ptag_str *)tag;

   if (stag->hi_pix != 0)
       XtVaSetValues (pbut_widget, XmNlabelPixmap, stag->hi_pix, NULL);
}
#endif

void km_panel_execute( pbut_widget, tag, callback_data )
      Widget   pbut_widget;
      caddr_t tag;
      XmAnyCallbackStruct *callback_data;
{
   Widget button[5];
   Pixel arm_pixel, bg_pixel, bs_col, ts_col;
   char *g_string, *ex_string;
   char *value = NULL;
   char cmd_start[MAXCMD];
   int i,ilen;
   int dollar_pos = -1;

   ptag_str *stag = (ptag_str *)tag;
   int row = stag->row;
   int col = stag->col;


   static Widget button_pressed = NULL;
   static Widget bpressed[5];
   static KmPixmap hpix_pressed = 0;

   /* Push Button with labels (0,1,2) */
   button[0] = stag->buttonn;
   button[1] = stag->buttonc;
   button[2] = stag->button1;
   /* Push Button with icon */
   button[3] = stag->buttoni;
   /* IconWidget */
   button[4] = stag->buttonb;


   /* Get armColor and background attributes for push button */
    XtVaGetValues (pbut_widget,
                   XmNarmColor, &arm_pixel,
                   XmNbackground, &bg_pixel,
                   XmNbottomShadowColor, &bs_col,
                   XmNtopShadowColor, &ts_col,
                   NULL);

    if (button_pressed != NULL) { /* Toggle behavior */
        for (i = 0; i < 4; i++)   /* xmPushButtonWidgetClass */
        XtVaSetValues (bpressed[i],
                       XmNbackground, bg_pixel,
                       XmNbottomShadowColor, bs_col,
                       XmNtopShadowColor, ts_col,
                       NULL);
        if ( hpix_pressed != 0 ) { /* button with icons */
            XtVaSetValues (bpressed[3],
                           XmNlabelPixmap, hpix_pressed, NULL);
            XtVaSetValues (bpressed[4],
                           XmNlabelPixmap, hpix_pressed, NULL);
        }
        /* iconwidget : just change XmNshadowType value */
        XtVaSetValues (bpressed[4],
                       XmNshadowType, XmSHADOW_OUT,  NULL);
    }

    /*
     *  Special case / selection with button 3
     */
    if (callback_data->event->xbutton.button == 3 && stag->rep == NO_ICON) {
        if (!srec.panel_interactive) return;
        /* erase label (new button) */
        for (i = 0; i < 5; i++ )
             if (button[i] != NULL) XtVaSetValues (button[i],
                            RES_CONVERT( XmNlabelString, "" ),
                            XmNuserData, strdup ( "" ),
                            NULL);
        return;
    }

    /*
     * We have to retrieve the label from XmNuserData
     * because XmNlabelString can contain
     *  - either [label]
     *  - or [command]
     * according to the "view" setting
     * N.B. XmNuserData = label[#%pixlab]
    */
    XtVaGetValues (pbut_widget,
                   XmNuserData, &g_string,
                   NULL);

    if (g_string != NULL) { /* retrieve real label */
        char *tmp;
        if ( (tmp = strstr( g_string, "#%" )) != NULL ) {
             tmp[0] = '\0';
        }
    }
    ilen = strlen(g_string);

    if (callback_data->event->xbutton.button == 3 && stag->rep == ICON) {
        char *blab;

        if (!srec.panel_interactive) return;
        /* ask for new button definition */
        blab = km_wait_input ("Give (new) text to map in this button", "",
                              "HELP_EDIT_PANEL");
        if (blab == NULL) return;
        for (i = 0; i < 5; i++)
             if (button[i] != NULL) XtVaSetValues (button[i],
                                    RES_CONVERT( XmNlabelString, blab ),
                                    XmNuserData, strdup ( blab ),
                                    NULL);
        return;
    }

    if (g_string != NULL) {
       char *blank_string = "                              ";

    /*
     *  Special case / button selected is blank : ask for label
     */
       if ( ilen == 0 || strncmp(g_string,blank_string,ilen) == 0 ) {
           char *blab;

           if (!srec.panel_interactive) return;
           /* ask for new button label */
           blab = km_wait_input ("Give text to map in this button",
                                 "", "HELP_EDIT_PANEL");
           if (blab != NULL) {
               for (i = 0; i < 5; i++)
                     if (button[i] != NULL) XtVaSetValues (button[i],
                                            RES_CONVERT( XmNlabelString, blab ),
                                            XmNuserData, strdup ( blab ),
                                            NULL);
           }
           return;
       }
    }

    /* Execute command ... */

    /* Get real command (ex_string) from the label (if any) */
    ex_string = strdup (g_string);
    if (kc_window.panel_name != NULL) {
           /* if cmd is an alias concatenate with panel name */
           ex_string = mstr2cat (ex_string, "_", kc_window.panel_name);
           value = (char*)hash_lookup( kc_alias.cmd_table, ex_string, NULL);
    }
    if (value == NULL) {
           char *strim = strdup (g_string);
           free (ex_string);
           strim = strblk0 (strim);
           /* check if "strim" value is an alias */
           value = (char*)hash_lookup( kc_alias.cmd_table, strim, NULL);
           if (value != NULL)
               ex_string = strdup (value);
           else
               ex_string = strdup (g_string);
           free (strim);
    }

    /* Parse "ex_string" (for - or $ characters) */
    ilen = strlen(ex_string);
    for (i=0; i<ilen; i++) {
         if (ex_string[i] == '$') dollar_pos = i; }
    if (dollar_pos >= 0) {
       /*  character "$" inside a key */

       char *command;
       char cmd_string[MAX_stringl];
       int lc;

       command = km_wait_input ("Enter value which should replace $", "",
                 "HELP_DOLLAR_PANEL");
       if (command == NULL) return;
       lc = strlen(command);
       for (i=0; i<dollar_pos; i++) {
            cmd_string[i] = ex_string[i]; }
       for (i=dollar_pos; i<dollar_pos+lc; i++) {
            cmd_string[i] = command[i-dollar_pos]; }
       for (i=dollar_pos+lc; i<ilen-1+lc; i++) {
            /* skip "$" */
            cmd_string[i] = ex_string[i+1-lc]; }
       cmd_string[i] = '\0';
       exec_cmd_string (cmd_string, 1, km_display_cmdpan);
       free (command);

    } else if (ex_string[ilen-1] == '-') {
       /*  key ending with minus */

       char *command;
       char cmd_string[MAX_stringl];

       if   (ex_string[ilen-2] == '-') {
            /*  key ending with double minus */
            strcpy (cmd_start,ex_string);
            cmd_start[ilen-2] = '\0';
            command = km_wait_input ("Give Missing Value","",
                      "HELP_MINUS2_PANEL");
            if (command == NULL) return;
            strcat (cmd_start, command);
            exec_cmd_string (cmd_start, 1, km_display_cmdpan);
       } else {
            /*  key ending with single minus */
            strcpy (cmd_start,ex_string);
            cmd_start[ilen-1] = '\0';
            command = km_wait_input ("Give Missing Value","",
                      "HELP_MINUS_PANEL");
            if (command == NULL) return;
            sprintf (cmd_string,"%s %s", cmd_start, command);
            exec_cmd_string (cmd_string, 1, km_display_cmdpan); }

    } else {

       /* set background color to arm color */
       for (i = 0; i < 4; i++)  /* PushButtonWidgetClass */
             if (button[i] != NULL) XtVaSetValues (button[i],
                      XmNbackground, arm_pixel,
                      XmNbottomShadowColor, ts_col,
                      XmNtopShadowColor, bs_col,
                      NULL);
       /* button with icons */
       if (stag->hi_pix != 0) {
           XtVaSetValues (button[3], XmNlabelPixmap, stag->hi_pix, NULL);
           XtVaSetValues (button[4], XmNlabelPixmap, stag->hi_pix, NULL);
       }
       /* for iconwidget just change XmNshadowType value */
       XtVaSetValues (button[4], XmNshadowType, XmSHADOW_IN,  NULL);
       /*
        * Wait and flush Motif event queue to see the button "pressed in"
        * during the command execution.
        */
       for (i= 0; i < 1000; i++) km_FlushEvents();

       /* Execute command */
       exec_cmd_string (ex_string, 1, km_display_cmdpan);
       free (ex_string);

       /* Reset background color */
       if (stag->flag != 1) {
           button_pressed = NULL;
           hpix_pressed = 0;
           for (i = 0; i < 4; i++)  /* PushButtonWidgetClass */
                 if (button[i] != NULL) XtVaSetValues (button[i],
                          XmNbackground, bg_pixel,
                          XmNbottomShadowColor, bs_col,
                          XmNtopShadowColor, ts_col,
                          NULL);
           if (stag->pix != 0) { /* icon */
               XtVaSetValues (button[3], XmNlabelPixmap, stag->pix, NULL);
               XtVaSetValues (button[4], XmNlabelPixmap, stag->pix, NULL);
           }
           /* for iconwidget just change XmNshadowType value */
           XtVaSetValues (button[4],
                          XmNshadowType, XmSHADOW_OUT,  NULL);
       } else {
           button_pressed = pbut_widget;
           for (i = 0; i < 5; i++)
                bpressed[i] = button[i];
           hpix_pressed = stag->pix;
       }

    }  /* end if (dollar_pos >= 0) */

    return;
}

static void km_panel_view (view_flag)
   int view_flag;
{
    if ( view_flag == by_NAME ) {
         if (XtIsManaged (km_scrollPB)) XtUnmanageChild (km_scrollPB);
         if (XtIsManaged (km_scrollPI)) XtUnmanageChild (km_scrollPI);
         if (XtIsManaged (km_scrollPC)) XtUnmanageChild (km_scrollPC);
         if (XtIsManaged (km_scrollP1)) XtUnmanageChild (km_scrollP1);
         XtManageChild (km_scrollPN);
    } else if (view_flag == by_ICON) {
         if (XtIsManaged (km_scrollPN)) XtUnmanageChild (km_scrollPN);
         if (XtIsManaged (km_scrollPB)) XtUnmanageChild (km_scrollPB);
         if (XtIsManaged (km_scrollPC)) XtUnmanageChild (km_scrollPC);
         if (XtIsManaged (km_scrollP1)) XtUnmanageChild (km_scrollP1);
         XtManageChild (km_scrollPI);
    } else if (view_flag == by_BOTH) {
         if (XtIsManaged (km_scrollPN)) XtUnmanageChild (km_scrollPN);
         if (XtIsManaged (km_scrollPI)) XtUnmanageChild (km_scrollPI);
         if (XtIsManaged (km_scrollPC)) XtUnmanageChild (km_scrollPC);
         if (XtIsManaged (km_scrollP1)) XtUnmanageChild (km_scrollP1);
         XtManageChild (km_scrollPB);
    } else if (view_flag == by_COMMAND) {
         if (XtIsManaged (km_scrollPN)) XtUnmanageChild (km_scrollPN);
         if (XtIsManaged (km_scrollPI)) XtUnmanageChild (km_scrollPI);
         if (XtIsManaged (km_scrollPB)) XtUnmanageChild (km_scrollPB);
         if (XtIsManaged (km_scrollP1)) XtUnmanageChild (km_scrollP1);
         XtManageChild (km_scrollPC);
    } else if (view_flag == by_COMMAND1) {
         if (XtIsManaged (km_scrollPN)) XtUnmanageChild (km_scrollPN);
         if (XtIsManaged (km_scrollPI)) XtUnmanageChild (km_scrollPI);
         if (XtIsManaged (km_scrollPB)) XtUnmanageChild (km_scrollPB);
         if (XtIsManaged (km_scrollPC)) XtUnmanageChild (km_scrollPC);
         XtManageChild (km_scrollP1);
    }
}

static void panel_save (pwidget, name)
    Widget pwidget;
    char *name;
{
    WidgetList bchild;
    FILE *ppf;
    char *fstr;
    String title;
    char display_string[MAX_stringl];
    Cardinal nbut;
    short nrow, ncol;
    Position xpos, ypos;
    Dimension dh, dw;
    int i, k, nbutton;

    /*
     *   find and restore the context
     *   (to get to the widgets belonging to this dialog)
     */
    UxKuipPanelContext = (_UxCkuipPanel *)UxGetContext( pwidget );


    /* Get values for panel geometry */
    XtVaGetValues (XtParent(pwidget), XmNtitle, &title,
                                      XmNx, &xpos, XmNy, &ypos,
                                      XmNheight, &dh, XmNwidth, &dw,
                                      NULL);
    /* We have to extract WM decoration from xpos and ypos */
    xpos = xpos - WM_x;
    ypos = ypos - WM_y;
    sprintf (display_string,"\npanel 0 d \'%s\' %dx%d+%d+%d\n",
                                         title, dw, dh, xpos, ypos);

    /* Get number and list of the rowcolumn push buttons children */
    XtVaGetValues (km_rowcolPI, XmNnumChildren, &nbut,
                                 XmNchildren, &bchild,
                                 XmNnumColumns, &nrow, NULL);

    fstr = strdup ("*\n* MOTIF_PANEL ");
    fstr = mstrcat (fstr, name);
    fstr = mstrcat (fstr, "\n*\npanel 0");
    ncol = (short) (nbut / nrow);
    nbutton = 0;
    for (k = 0; k < nrow;  k++) {
        for (i = 0; i < ncol; i++) {
            char cval[MAX_string];
            int ilen;

            char *label = NULL;
            char *pixlab = NULL;
            char *value = NULL;

            if (i < 10)
               sprintf (cval,"\npanel %d.0%d ", k+1,i+1);
            else
               sprintf (cval,"\npanel %d.%d ", k+1,i+1);
            fstr = mstrcat (fstr, cval);
            /*
             * We have to retrieve the label from XmNuserData
             * because XmNlabelString can contain
             *  - either [label]
             *  - or [command]
             * according to the "view" setting
             * N.B. XmNuserData = label[#%pixlab]
             */
            XtVaGetValues (bchild[nbutton], XmNuserData, &label,
                                            NULL);
            if (label != NULL && !km_strempty (label) ) {
               /* retrieve real label */
                char *tmp;

                if ( (tmp = strstr( label, "#%" )) != NULL ) {
                     pixlab = strdup (tmp);
                     pixlab += 2;
                     tmp[0] = '\0';
                }
            }

            if (label != NULL && !km_strempty (label) ) {
               value = (char*)hash_lookup( kc_alias.cmd_table, label, NULL);
               if( value != NULL)
                    fstr = mstr3cat  (fstr, "\'", value, "\' ");
               else
                    fstr = mstrcat (fstr, ". ");

               if ( (ilen = strlen(label)) == 0 )
                    fstr = mstrcat (fstr, ". ");
               else
                    fstr = mstr3cat (fstr, "\'", label, "\' ");
               if ( pixlab != NULL && strlen(pixlab) != 0 )
                    fstr = mstrcat  (fstr, pixlab);
               XtFree (label);
            } else {
               fstr = mstrcat (fstr, ". ");
            }
            nbutton++;
        }
    }
    fstr = mstrcat  (fstr, display_string);

    if ((ppf = fopen(name, "w")) == NULL) {
         char message[MAX_stringl];

         sprintf (message,"Unable to open file %s for write ", name);
         km_print_box (km_toplevel, 0, 0, message,0);
         return; }
    fwrite(fstr, sizeof(char), strlen(fstr), ppf);
    fclose(ppf);

}

static void okPNBCallback( widget, parent, data )
      Widget          widget;
      Widget          parent;
      XmAnyCallbackStruct   *data;
{
    String name = XtName(parent);
    char *panel_htitle = strdup (name);
    strconv (panel_htitle);

#if defined(CERNLIB_LINUX)
    UxPopdownInterface(parent);
#else
    UxDestroyInterface(parent);
#endif
    km_reset_appShell (parent);
    hash_remove( kmpawid,  panel_htitle );
}

static void savePNBCallback( widget, panel, callback_data )
        Widget  widget;
        Widget  panel;
        XmAnyCallbackStruct *callback_data;
{
   FILE *tfp;                              /* Pointer to open file            */
   char *macro_name;


/* Get kuip macro name (panel definition)                                    */
   macro_name = km_wait_input ("Give KUIP macro file name", "",
                               "HELP_SAVE_PANEL");
   if (macro_name == NULL) return;


/* Check if file already exists and put a warning message                    */
   if ((tfp = fopen(macro_name, "r")) != NULL) {
        char cmess[MAX_stringl];
        int answ;

        static char *clist[] = {"Yes", "No"};

        sprintf (cmess,"File %s already exists: overwrite it ? (Y/N)",
                 macro_name);
        answ = km_select (XmDIALOG_QUESTION, cmess, 2, clist, 1);
        if (answ != 1) {
            fclose(tfp);
            return;
        }
        fclose(tfp);
   }

   /* Save file */
   panel_save (panel, macro_name);

   free (macro_name);
   return;
}

static void set_context_help( widget, panel, callback_data )
        Widget  widget;
        Widget  panel;
        XmAnyCallbackStruct *callback_data;
{
   context_panel = panel;

   km_panel_context = True;
}

void km_ContextHelp()
{
   Widget          context_widget ;
   XEvent          event_return;

   static Cursor   context_cursor;
   static int ifirst = 0;

   if ( ifirst == 0 ) {
        ifirst = 1;
        context_cursor = XCreateFontCursor( XtDisplay(context_panel),
                                            XC_question_arrow ) ;
   }

#if XmVERSION > 1 || XmREVISION >= 2
   context_widget = XmTrackingEvent( context_panel, context_cursor,
                                      True, &event_return ) ;

   if (event_return.xbutton.button == 3 || event_return.xbutton.button == 2) {
       km_panel_context = False;
       return;
   }
#else
   context_widget = XmTrackingLocate( context_panel, context_cursor, True ) ;
#endif

   if ( context_widget != NULL ) {
        /* In panels context_widget are the push buttons */
        char *label, *user_data;

        /*
         * We have to retrieve the label from XmNuserData
         * because XmNlabelString can contain
         *  - either [label]
         *  - or [command]
         * according to the "view" setting
         * N.B. XmNuserData = label[#%pixlab]
        */
        XtVaGetValues (context_widget,
                       XmNuserData, &user_data,
                       NULL);
        label = strdup (user_data);
        if (label != NULL && !km_strempty (label) ) { /* retrieve real label */
            char *tmp;
            if ( (tmp = strstr( label, "#%" )) != NULL ) {
                  tmp[0] = '\0';
            }
        }

        km_panel_context = False;

        display_ContextHelp (label);
        free (label);
   }
}

static void display_ContextHelp (label)
   char *label;
{
extern C_PROTO_1(Widget create_kuipScroll, Widget);
extern XmFontList    help_font;

   Widget dialog;
   char title[MAX_string];

   static char file_help[] =
"        *** Menu \"File\" *** \n\n\
You can select \"Save\" to save your panel\n\
configuration into a file (and be able to retrieve\n\
it later), or \"Close\" to close the panel\n\
(erase from screen)";
   static char view_help[] =
"        *** Menu \"View\" *** \n\n\
This gives you access to 3 viewing options for the panel:\n\
     - By Name (with string labels),\n\
     - By Icon (with pixmap labels) \n\
     - or both (By Name and Icon).";
   static char help_help[] =
"        *** Menu \"Help\" *** \n\n\
This menu gives access on various help  items\n\
concerning panels handling inside KUIP/Motif";

   dialog = create_kuipScroll(km_toplevel);
   UxKuipScrollContext = (_UxCkuipScroll *)UxGetContext(dialog);
   sprintf (title, "%s Panel Help", class_name);
   XtVaSetValues(dialog, XmNtitle, title, NULL);
   /* set help_font (proportional) if defined */
   if (help_font != NULL )
       XtVaSetValues(scrollText, XmNfontList, help_font, NULL);
   XmTextSetAddMode(scrollText, True);

   if (!strcasecmp (label, "File")) {
       km_insert_string(scrollText, file_help, False);
   } else if (!strcasecmp (label, "View")) {
       km_insert_string(scrollText, view_help, False);
   } else if (!strcasecmp (label, "Help")) {
       km_insert_string(scrollText, help_help, False);
   } else {
       Boolean found = False;

       if (kmpahlp != NULL) {
           /* Look in hash table for panel helps */
           char *hlp = (char*)hash_lookup (kmpahlp, label, NULL);

           if (hlp != NULL) {
               km_insert_string(scrollText, hlp, False);
               found = True;
           }
       }
       if (!found) {
           char mess[512];

           sprintf (mess, "Cannot find help for \"%s\"\n", label);
           km_insert_string(scrollText, mess, False);
       }
   }

   UxPopupInterface(dialog, XtGrabNone);
   XtAddCallback(km_scrollOK, XmNactivateCallback,
                 (XtCallbackProc)destroy_dialog, dialog);
   XtAddCallback(dialog, XmNdestroyCallback,
                 (XtCallbackProc)destroy_dialog, dialog);
}

static void destroy_dialog(w, dialog, call_data)
   Widget w;
   Widget dialog;
   XmAnyCallbackStruct *call_data;
{
    UxDestroyInterface(dialog);
}

static void viewPNBCallback( widget, panel, callback_data )
        Widget  widget;
        Widget  panel;
        XmToggleButtonCallbackStruct *callback_data;
{
    /* when a button gets unselected don't do anything */
    if (!callback_data->set) return;

    /*
     *   find and restore the context
     *   (to get to the widgets belonging to this dialog)
     */
    UxKuipPanelContext = (_UxCkuipPanel *)UxGetContext( panel );

    XtVaSetValues(widget, XmNset, True, NULL);

    if (widget == km_viewPNB_name) {
       km_panel_view (by_NAME);
       XtVaSetValues(km_viewPNB_icon, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_both, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_command, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_command1, XmNset, False, NULL);
    } else if (widget == km_viewPNB_icon) {
       km_panel_view (by_ICON);
       XtVaSetValues(km_viewPNB_name, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_both, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_command, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_command1, XmNset, False, NULL);
    } else if (widget == km_viewPNB_both) {
       km_panel_view (by_BOTH);
       XtVaSetValues(km_viewPNB_icon, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_name, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_command, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_command1, XmNset, False, NULL);
    } else if (widget == km_viewPNB_command) {
       km_panel_view (by_COMMAND);
       XtVaSetValues(km_viewPNB_icon, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_name, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_both, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_command1, XmNset, False, NULL);
    } else if (widget == km_viewPNB_command1) {
       km_panel_view (by_COMMAND1);
       XtVaSetValues(km_viewPNB_icon, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_name, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_both, XmNset, False, NULL);
       XtVaSetValues(km_viewPNB_command, XmNset, False, NULL);
    }
}

static char *find_hlp( name )
     char *name;
{
  if (kmpahlp == NULL) {
      kmpahlp = hash_create( PAHLP_TABLE_SIZE );
      hash_config( kmpahlp, "struct" );
  }

  return (char*)hash_lookup( kmpahlp, name, NULL );
}

/*
 * action routine for /MOTIF/PANEL_HELP/CREATE
 */
int kxphlpc()
{
  char *label = ku_getc();
  char *file = ku_gets();
  char *hlp;
  FILE *fp;

  if ( (hlp = find_hlp( label )) != NULL )
     hash_remove( kmpahlp, label );

  if ((fp = fopen(file, "r")) != NULL) {
     char line[1024];
     char *buf = strdup( "" );

     while( fgets( line, (sizeof line), fp ) != NULL )
        buf = mstrcat( buf, line );

     if (kmpahlp == NULL) {
         kmpahlp = hash_create( PAHLP_TABLE_SIZE );
         hash_config( kmpahlp, "struct" );
     }
     hash_insert( kmpahlp, label, buf, 0);

  } else {
     char message[MAX_stringl];

     sprintf (message,"cannot find help corresponding to button %s", label);
     km_warn_user(km_toplevel, message, "");
  }

  free (file);

  return 0;
}

/*
 * action routine for /MOTIF/PANEL_HELP/LIST
 */
int kxphlpl()
{
  int n = hash_entries( kmpahlp );

  if( n == 0 ) {
      printf( " No help defined for user panels.\n");
  } else {
      HashArray *hlp = hash_array( kmpahlp );
      int i;

      printf( " Panel(s) help items:\n" );
      for( i = 0; i < n; i++ )
        printf( " %-10s => %s\n", hlp[i].name, (char*)hlp[i].value );

      free( (char*)hlp );
  }

  return 0;
}

/*
 * action routine for /MOTIF/PANEL_HELP/DELETE
 */
int kxphlpd()
{
  int npar = ku_npar();

  if (npar >= 1) {
      char *label = ku_getc();

      if (label == NULL ||
          strcmp( label, "*" ) == 0 || km_strempty (label) ) {
         /* Delete all user defined help items */
         hash_clear( kmpahlp );
      } else {
         hash_remove( kmpahlp, label );
      }
  } else {
      /* Delete all entries */
      hash_clear( kmpahlp );
  }

  return 0;
}

/*
 * action routine for /MOTIF/PANEL_HELP/PRINT
 */
int kxphlpp()
{
  char *label = ku_getc();

  if (label == NULL ||
      strcmp( label, "*" ) == 0 || km_strempty (label) ) {
     kxphlpl();
  } else {
       Boolean found = False;

       if (kmpahlp != NULL) {
           /* Look in hash table for panel helps */
           char *hlp = (char*)hash_lookup (kmpahlp, label, NULL);

           if (hlp != NULL) {
               printf( " %-10s => %s\n", label, (char*)hlp );
               found = True;
           }
       }
       if (!found) printf( " No help defined for user panels.\n");
  }

  return 0;
}

/*
 * Replace all blanks inside string by "-" (minus)
 */
static void strconv( str )
  char *str;
{
  if( str != NULL ) {
    char *p;
    for( p = str; *p != '\0'; p++ ) {
         if( *p == ' ') *p = '-';
    }
  }
}
