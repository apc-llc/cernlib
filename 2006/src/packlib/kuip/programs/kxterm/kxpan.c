/*
 * $Id: kxpan.c,v 1.1.1.1 1996/03/08 15:33:04 mclareni Exp $
 *
 * $Log: kxpan.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:04  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/03 09/05/95  11.54.44  by  N.Cremel*/
/*-- Author :    N.Cremel   10/08/92*/
/******************************************************************************/
/*                                                                            */
/*           Basic Interface for "KXTERM Panels".                             */
/*           (connected to the file "panels" for building                     */
/*           panels with Architect : "create_kuipPanel").                     */
/*                                                                            */
/******************************************************************************/

#include "kuip/kuip.h"

#include <Xm/Xm.h>
#include <X11/IntrinsicP.h>
#if defined(__hpux) && XtSpecificationRelease == 4
#include <X11/Protocols.h>
#else
#include <Xm/Protocols.h>
#endif

#include "utils.h"
#include "uxxt.h"
#define CONTEXT_MACRO_ACCESS 1
/* #include "kuipPanel.h" */
#include "uxpan.h"
#undef CONTEXT_MACRO_ACCESS

/***********************************************************************
 *                                                                     *
 *   Forward declarations of extern functions.                         *
 *                                                                     *
 ***********************************************************************/

extern Widget create_kuipPanel();

extern void warn_user(          /* from dialog.c */
#ifndef _NO_PROTO
                      Widget parent, char *warning
#endif
                      );
extern void inform_user(
#ifndef _NO_PROTO
                        Widget parent, char *warning
#endif
                        );
extern Boolean ask_user(
#ifndef _NO_PROTO
                        Widget parent, char *question, char *ans1, char *ans2,
                        int default_ans
#endif
                        );
extern char *prompt_user(
#ifndef _NO_PROTO
                        Widget parent,
                        char *prompt, char *default_line, char *help_item
#endif
                        );
extern void send_cmd(
#ifndef _NO_PROTO
                        char *
#endif
                        );

/***********************************************************************
 *                                                                     *
 *   Forward declarations of local functions.                          *
 *                                                                     *
 ***********************************************************************/
static void panelfocusCallback(
#ifndef _NO_PROTO
                        Widget, caddr_t, XmAnyCallbackStruct *
#endif
                       );
static void panel_execute(
#ifndef _NO_PROTO
                        Widget, caddr_t, XmAnyCallbackStruct *
#endif
                       );
static void panel_save(
#ifndef _NO_PROTO
                        Widget , char *
#endif
                       );
static void okPNBCallback(
#ifndef _NO_PROTO
                        Widget, Widget, XmAnyCallbackStruct *
#endif
                       );
static void savePNBCallback(
#ifndef _NO_PROTO
                        Widget, Widget, XmAnyCallbackStruct *
#endif
                       );
void helpPNBCallback(
#ifndef _NO_PROTO
                        Widget, char *, XmAnyCallbackStruct *
#endif
                       );

static char *Mstrcat(
#ifndef _NO_PROTO
                        char *, const char *
#endif
                       );

/***********************************************************************
 *                                                                     *
 *   Local datas.                                                      *
 *                                                                     *
 ***********************************************************************/
static int WM_x = 0;   /* width of Window Manager decoration */
static int WM_y = 0;   /* height of Window Manager decoration */
static char panel_geometry[512];

static struct kxpanel {
  int         panel_rows;       /* number of rows */
  int        *panel_cols;       /* number of columns in each row */
  char     ***panel_keys;       /* key labels */
} kx_panel = { 0, NULL, NULL };

#define  MAXCMD          512  /* max length of a command line                 */
#define MAX_button       600  /*  max. number of buttons in a panel           */
                              /*  cf. KUIP : 30 lin. X 20 col.                */
#define MAX_string        80  /*  max. length for string                      */
#define MAX_stringl      240  /*  max. length for long string                 */


/******************************************************************************/
/* void set_kxtpanel_rows (nrow, ncol, last)                                  */
/******************************************************************************/
void set_kxtpanel_rows (nrow, ncol, last)
    int nrow, ncol, last;
{
    int i, j;

    if (last == -1) {

        /* Reset old panel datas */
        for( i = 0; i < kx_panel.panel_rows; i++ ) {
             for( j = 0; j < kx_panel.panel_cols[i]; j++ ) {
                if (kx_panel.panel_keys[i][j] != NULL )
                   free( kx_panel.panel_keys[i][j] );
             }
             if (kx_panel.panel_keys[i] != NULL ) {
                free( (char*)kx_panel.panel_keys[i] );
                kx_panel.panel_keys[i] = NULL;
             }
             kx_panel.panel_cols[i] = 0;
        }

        if( kx_panel.panel_rows != 0 ) {
            free( (char*)kx_panel.panel_cols );
            kx_panel.panel_cols = NULL;
            free( (char*)kx_panel.panel_keys );
            kx_panel.panel_keys = NULL;
        }

        /* new space allocation */
        kx_panel.panel_rows = nrow;
        kx_panel.panel_cols = (int*)malloc( nrow * sizeof(int) );
        kx_panel.panel_keys = (char***)malloc( nrow * sizeof(char**) );
        return;
    }

    kx_panel.panel_cols[nrow] = ncol;
    kx_panel.panel_keys[nrow] = (char**)malloc( ncol * sizeof(char**) );
    for( i = 0; i < ncol; i++ ) {
         kx_panel.panel_keys[nrow][i] = NULL;
    }

}

/******************************************************************************/
/* void set_kxtpanel_keys (row, col, key)                                     */
/******************************************************************************/
void set_kxtpanel_keys (row, col, key)
     int row, col;
     char *key;
{
     if( kx_panel.panel_keys[row][col] != NULL )
       free( kx_panel.panel_keys[row][col] );
     if (key != NULL)
         kx_panel.panel_keys[row][col] = strdup( key );
     else
         kx_panel.panel_keys[row][col] = strdup( "" );
}

/******************************************************************************/
/* void display_kxtpanel                                                      */
/******************************************************************************/
void display_kxtpanel (title, geometry)
    char *title, *geometry;
{
    Widget panel_widget;
    Widget npbutton[MAX_button];
    char wname[MAX_stringl];
    int i, j, ncol, nbutton;
    short nrow;

    panel_widget = (Widget) create_kuipPanel();

    /*
     *   find and restore the context
     *   (to get to the widgets belonging to this dialog)
     */
    UxKuipPanelContext = (_UxCkuipPanel *)UxGetContext( panel_widget );

    strcpy (panel_geometry, geometry);

    /* Set title and geometry */
    if (geometry != NULL)
         XtVaSetValues(XtParent(panel_widget), XmNtitle, title,
                                       XmNiconName, title,
                                       XmNdefaultPosition, False,
                                       XmNgeometry, geometry,
                                       NULL);
    else
         XtVaSetValues(XtParent(panel_widget), XmNtitle, title,
                                       XmNiconName, title,
                                       NULL);

    /* Create RowColumn with push buttons child of Scrolled Window */
    nrow = kx_panel.panel_rows;
    XtVaSetValues(km_rowcolPNW, XmNnumColumns, nrow, NULL);


    /* Get maximum number of colums */
    ncol = 0;
    for (i = 0; i < nrow; i++) {
        if (kx_panel.panel_cols[i] > ncol) ncol = kx_panel.panel_cols[i]; }

    /* Create buttons */
    nbutton = 0;
    for (i = 0; i < nrow; i++) {
        int nc = kx_panel.panel_cols[i];

        if (nbutton >= MAX_button - 1) break;
        for (j = 0; j < nc; j++) {
         sprintf (wname, "NP%sB%d", title,j);
         if (kx_panel.panel_keys[i][j] != NULL)
             npbutton[nbutton] = XtVaCreateWidget (wname,
                       xmPushButtonWidgetClass, km_rowcolPNW,
                       RES_CONVERT( XmNlabelString, kx_panel.panel_keys[i][j]),
                       NULL);
         else
             npbutton[nbutton] = XtVaCreateWidget (wname,
                       xmPushButtonWidgetClass, km_rowcolPNW,
                       RES_CONVERT( XmNlabelString, ""),
                       NULL);
         XtOverrideTranslations (npbutton[nbutton],
                     XtParseTranslationTable("<Btn3Down>:Arm()"));
         XtOverrideTranslations (npbutton[nbutton],
                     XtParseTranslationTable("<Btn3Up>:Activate()"));
         XtAddCallback (npbutton[nbutton],XmNactivateCallback,
                        (XtCallbackProc)panel_execute,NULL);
         nbutton ++;
        }
        for (j = nc; j < ncol; j++) {
         sprintf (wname, "NP%sB%d", title,j);
         npbutton[nbutton] = XtVaCreateWidget (wname,
                       xmPushButtonWidgetClass, km_rowcolPNW,
                       RES_CONVERT( XmNlabelString, ""),
                       NULL);
         XtOverrideTranslations (npbutton[nbutton],
                     XtParseTranslationTable("<Btn3Down>:Arm()"));
         XtOverrideTranslations (npbutton[nbutton],
                     XtParseTranslationTable("<Btn3Up>:Activate()"));
         XtAddCallback (npbutton[nbutton],XmNactivateCallback,
                        (XtCallbackProc)panel_execute,NULL);
         nbutton ++;
        }
    }
    XtManageChildren (npbutton, nbutton);

    XtAddCallback (km_okPNB,XmNactivateCallback,
                   (XtCallbackProc)okPNBCallback, panel_widget);
    XtAddCallback (km_savePNB,XmNactivateCallback,
                   (XtCallbackProc)savePNBCallback, panel_widget);
    XtAddCallback (km_helpPNB,XmNactivateCallback,
                   (XtCallbackProc)helpPNBCallback,
                   (XtPointer)"HELP_EDIT_PANEL");
    XtAddCallback (panel_widget,XmNfocusCallback,
                   (XtCallbackProc)panelfocusCallback, NULL);


    UxPopupInterface(panel_widget, XtGrabNone);

    return;
}

static void panelfocusCallback( widget, tag, callback_data )
      Widget   widget;
      caddr_t tag;
      XmAnyCallbackStruct *callback_data;
{
    Position xpos, ypos;
    int ih, iw, ix, iy;

    static int ifirst = 0;

    if (ifirst == 0) {
        /* First time only we get WM decoration size */
        ifirst = 1;
        XtVaGetValues(XtParent(widget), XmNx, &xpos, XmNy, &ypos, NULL);
        sscanf (panel_geometry, "%dx%d+%d+%d", &iw, &ih, &ix, &iy);
        WM_x = xpos - ix;
        WM_y = ypos - iy;
    }
}


static void panel_execute( pbut_widget, tag, callback_data )
      Widget   pbut_widget;
      caddr_t tag;
      XmAnyCallbackStruct *callback_data;
{
   XmString xmstr_label;
   char *g_string;
   char cmd_start[MAXCMD];
   int i,ilen;
   int dollar_pos = -1;


    /*
     *  Special case / selection with button 3 : erase label (new button)
     */
    if (callback_data->event->xbutton.button == 3) {
        XtVaSetValues (pbut_widget,
                       RES_CONVERT( XmNlabelString, "" ),
                       NULL);
        return;
    }

    XtVaGetValues (pbut_widget,
                   XmNlabelString, &xmstr_label,
                   NULL);
    XmStringGetLtoR (xmstr_label, XmSTRING_DEFAULT_CHARSET, &g_string);
    ilen = strlen(g_string);
    if (g_string != NULL) {
       char *blank_string = "                              ";

    /*
     *  Special case / button selected is blank : ask for label
     */
       if ( ilen == 0 || strncmp(g_string,blank_string,ilen) == 0 ) {
           /* ask for new button label */
           char *blab;
           blab = prompt_user (UxTopLevel, "Give text to map in this button",
                               "", "HELP_EDIT_PANEL");
           if (blab != NULL) XtVaSetValues (pbut_widget,
                                 RES_CONVERT( XmNlabelString, blab ), NULL);
           free (blab);
           return;
       }
    }

    /* Execute command ... */
    for (i=0; i<ilen; i++) {
         if (g_string[i] == '$') dollar_pos = i; }
    if (dollar_pos >= 0) {
       /*  character "$" inside a key */
       char *command;
       char cmd_string[MAX_stringl];
       int lc;

       command = prompt_user (UxTopLevel,
                              "Enter value which should replace $", "",
                              "HELP_DOLLAR_PANEL");
       if (command == NULL) return;
       lc = strlen(command);
       for (i=0; i<dollar_pos; i++) {
            cmd_string[i] = g_string[i]; }
       for (i=dollar_pos; i<dollar_pos+lc; i++) {
            cmd_string[i] = command[i-dollar_pos]; }
       for (i=dollar_pos+lc; i<ilen-1+lc; i++) {
            /* skip "$" */
            cmd_string[i] = g_string[i+1-lc]; }
       cmd_string[i] = '\0';
       send_cmd (cmd_string);
       free (command);
    } else if (g_string[ilen-1] == '-') {
       /*  key ending with minus */
       char *command;
       char cmd_string[MAX_stringl];

       if   (g_string[ilen-2] == '-') {
            /*  key ending with double minus */
            strcpy (cmd_start,g_string);
            cmd_start[ilen-2] = '\0';
            command = prompt_user (UxTopLevel,  "Give Missing Value", "",
                                  "HELP_MINUS2_PANEL");
            if (command == NULL) return;
            strcat (cmd_start, command);
            send_cmd (cmd_start);
       } else {
            /*  key ending with single minus */
            strcpy (cmd_start,g_string);
            cmd_start[ilen-1] = '\0';
            command = prompt_user (UxTopLevel, "Give Missing Value", "",
                                   "HELP_MINUS_PANEL");
            if (command == NULL) return;
            sprintf (cmd_string,"%s %s", cmd_start, command);
            send_cmd (cmd_string);
       } /* end ... if   (g_string[ilen-2] == '-')  */
    } else {
       /*  Normal case : execute command directly */
       send_cmd (g_string);
    }  /* end if (dollar_pos >= 0) */

    return;
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
    sprintf (display_string,"\npanel 0 T \'%s\' %dx%d+%d+%d\n",
                                         title, dw, dh, xpos, ypos);

    /* Get number and list of the rowcolumn push buttons children */
    XtVaGetValues (km_rowcolPNW, XmNnumChildren, &nbut,
                                 XmNchildren, &bchild,
                                 XmNnumColumns, &nrow, NULL);

    fstr = strdup ("*\n* MOTIF_PANEL ");
    fstr = Mstrcat (fstr, name);
    fstr = Mstrcat (fstr, "\n*\npanel 0");
    ncol = (short) (nbut / nrow);
    nbutton = 0;
    for (k = 0; k < nrow;  k++) {
        for (i = 0; i < ncol; i++) {
            char cval[MAX_string];
            XmString xmstr_label;
            char *label;
            int ilen;

            if (ncol < 10)
               sprintf (cval,"\npanel %d.0%d \'", k+1,i+1);
            else
               sprintf (cval,"\npanel %d.%d \'", k+1,i+1);
            fstr = Mstrcat (fstr, cval);
            XtVaGetValues (bchild[nbutton], XmNlabelString, &xmstr_label, NULL);
            XmStringGetLtoR (xmstr_label, XmSTRING_DEFAULT_CHARSET, &label);
            if ( (ilen = strlen(label)) == 0 )
                 fstr = Mstrcat (fstr, " ");
            else
                 fstr = Mstrcat (fstr, label);
            fstr = Mstrcat  (fstr, "\'");
            XtFree (label);
            nbutton++;
        }
    }
    fstr = Mstrcat  (fstr, display_string);

    if ((ppf = fopen(name, "w")) == NULL) {
         char message[MAX_stringl];

         sprintf (message,"Unable to open file %s for write ", name);
         warn_user (UxTopLevel, message);
         return; }
    fwrite(fstr, sizeof(char), strlen(fstr), ppf);
    fclose(ppf);

}

static void okPNBCallback( widget, parent, data )
      Widget          widget;
      Widget          parent;
      XmAnyCallbackStruct   *data;
{
    UxDestroyInterface(parent);
}

static void savePNBCallback( widget, panel, callback_data )
        Widget  widget;
        Widget  panel;
        XmAnyCallbackStruct *callback_data;
{
   FILE *tfp;            /* Pointer to open file */
   char *macro_name;


/* Get kuip macro name (panel definition)                                    */
   macro_name = prompt_user (UxTopLevel, "Give KUIP macro file name", "",
                             "HELP_SAVE_PANEL");
   if (macro_name == NULL) return;


/* Check if file already exists and put a warning message                    */
   if ((tfp = fopen(macro_name, "r")) != NULL) {
        char cmess[MAX_stringl];
        int answ;

        sprintf (cmess,"File %s already exists: overwrite it ? (Y/N)",
                 macro_name);
        answ = ask_user (UxTopLevel, cmess, "Yes", "No", 1);
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

void helpPNBCallback( widget, hlpstr, callback_data )
        Widget  widget;
        char    *hlpstr;
        XmAnyCallbackStruct *callback_data;
{
   char cmd_help[MAXCMD];

   sprintf (cmd_help, "HELP %s", hlpstr);
   send_cmd (cmd_help);
}

/*
 * concatenate two strings if the first one is allocated
 */
static char *Mstrcat( str1, str2 )
     char *str1;
     const char *str2;
{
  size_t len1 = strlen( str1 );
  size_t len2 = strlen( str2 );
  str1 = realloc( str1, len1 + len2 + 1 );
  strcpy( &str1[len1], str2 );
  return str1;
}
