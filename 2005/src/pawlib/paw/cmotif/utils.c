/*
 * $Id: utils.c,v 1.4 2000/01/13 15:43:11 couet Exp $
 *
 * $Log: utils.c,v $
 * Revision 1.4  2000/01/13 15:43:11  couet
 * - mods to make the fitting panel works on LINUX
 *
 * Revision 1.3  1997/07/18 09:34:15  couet
 * - kulast is called in execute_kuip_cmd in order to store the "panel generated
 * commands" in the last.kumac file.
 *
 * Revision 1.2  1996/10/18 08:22:15  couet
 * -   #include <Xm/AtomMgr.h> was missing
 *
 * Revision 1.1.1.1  1996/03/01 11:38:57  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.07/03 21/06/95  17.45.09  by  O.Couet*/
/*-- Author :*/
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>

#include "hmotif/fortran.h"
#include "hmotif/pawm.h"
#if defined(CERNLIB_FPANELS)
#include "fpanelsh/fpanels.h"
#endif

/* extern functions */
extern void restart_input_timer(void);

/* extern widgets */
extern Widget cwdLabel;

extern Widget OptionPlot;
extern Widget OptionPlotD;
extern Widget OptionPlotL;
extern Widget OptionPlotC;
extern Widget OptionPlotB;
extern Widget OptionPlotP;
extern Widget OptionPlotSTAR;
extern Widget OptionPlotE;
extern Widget OptionPlotE1;
extern Widget OptionPlotE2;
extern Widget OptionPlotE3;
extern Widget OptionPlotE4;
extern Widget OptionPlotBOX;
extern Widget OptionPlotCOL;
extern Widget OptionPlotSURF;
extern Widget OptionPlotSURF1;
extern Widget OptionPlotSURF2;
extern Widget OptionPlotSURF3;
extern Widget OptionPlotSURF4;
extern Widget OptionPlotLEGO;
extern Widget OptionPlotLEGO1;
extern Widget OptionPlotLEGO2;
extern Widget OptionPlotCONT;
extern Widget OptionPlotCONT3;
extern Widget OptionPlotTEXT;


/* global used by libPW.a */
int Error;

/***********************************************************************
 *                                                                     *
 *   Get char string from XmString.                                    *
 *   Returned string must be freed afterwards.                         *
 *                                                                     *
 ***********************************************************************/
char *extract_normal_string(XmString cs)
{
  char  *primitive_string;

  XmStringGetLtoR(cs, XmSTRING_DEFAULT_CHARSET, &primitive_string);

  return ((char *)primitive_string);
}

#if 0
/***********************************************************************
 *                                                                     *
 *   Strip blanks from both sides of a string. Space for the new       *
 *   string is allocated and a pointer to it is returned.              *
 *                                                                     *
 ***********************************************************************/
char *strip(char *s)
{
   char *r, *t1, *t2;
   int   l;

   l = strlen(s);
   r = XtCalloc(l+1, 1);

   if (l == 0) {
      *r = '\0';
      return r;
   }

   /* get rid of leading blanks */
   t1 = s;
   while (*t1 == ' ')
      t1++;

   t2 = s + l - 1;
   while (*t2 == ' ' && t2 > s)
      t2--;

   if (t1 > t2) {
      *r = '\0';
      return r;
   }
   strncpy(r, t1, (size_t) (t2-t1+1));

   return r;
}
#endif

/***********************************************************************
 *                                                                     *
 *   Removes all blanks from a string. Space for the new               *
 *   string is allocated and a pointer to it is returned.              *
 *                                                                     *
 ***********************************************************************/
char *compress(char *s)
{
   char *r, *t;
   int   l;

   l = strlen(s);
   r = XtCalloc(l+1, 1);

   if (l == 0) {
      *r = '\0';
      return r;
   }

   t = r;
   while (*s) {
      if (*s != ' ') {
         *t = *s;
         t++;
      }
      s++;
   }
   return r;
}

/***********************************************************************
 *                                                                     *
 *   Returns true if string is empty (only containing blanks).         *
 *                                                                     *
 ***********************************************************************/
Boolean strempty(char *str)
{
   char    *sav  = str;
   Boolean  rtrn = True;

   if (!sav) return rtrn;
   if (strlen(sav) == 0) return rtrn;

   while (*sav) {
      if (*sav != ' ') {
       rtrn = False;
       break;
      }
      sav++;
   }
   return rtrn ;
}

/***********************************************************************
 *                                                                     *
 *   Change the characters in str into lower case.                     *
 *                                                                     *
 ***********************************************************************/
void lower(char *str)
{
   if (str) {
      char *p = str;
      while (*p) {
         if (isupper(*p))
            *p = tolower(*p);
         p++;
      }
   }
}

/***********************************************************************
 *                                                                     *
 *   Change the characters in str into upper case.                     *
 *                                                                     *
 ***********************************************************************/
void upper(char *str)
{
   if (str) {
      char *p = str;
      while (*p) {
         if (islower(*p))
            *p = toupper(*p);
         p++;
      }
   }
}

/***********************************************************************
 *                                                                     *
 *   Assign the space for a MenuCbStruct and fill it.                  *
 *                                                                     *
 ***********************************************************************/
MenuCbStruct *make_menuStruct(Widget button, Widget dialog)
{
   MenuCbStruct  *tmp;

   tmp = XtNew(MenuCbStruct);
   tmp->button = button;
   tmp->dialog = dialog;

   return tmp;
}

/***********************************************************************
 *                                                                     *
 *   Popup argument dialog and make menu item insensitive.             *
 *                                                                     *
 ***********************************************************************/
void popup_argument_dialog(MenuCbStruct *menu_item)
{
   XtSetSensitive(menu_item->button, False);
   XtManageChild(menu_item->dialog);
}

/***********************************************************************
 *                                                                     *
 *   Popdown argument dialog and make menu item sensitive.             *
 *                                                                     *
 ***********************************************************************/
void popdown_argument_dialog(MenuCbStruct *menu_item)
{
   XtSetSensitive(menu_item->button, True);
   XtUnmanageChild(menu_item->dialog);
}

/***********************************************************************
 *                                                                     *
 *   Returns the true shell widget.                                    *
 *                                                                     *
 ***********************************************************************/
static Widget GetTrueShell(w)
     Widget w;
{
   Widget wgt = w;

   if (wgt && !XtIsShell(wgt)) {
      wgt = XtParent(wgt);
   }
   return wgt;
}

/***********************************************************************
 *                                                                     *
 *   Install MwmCloseCallback.                                         *
 *                                                                     *
 ***********************************************************************/
void InstallMwmCloseCallback(Widget w, XtCallbackProc callback,
                             XtPointer client_data)
{
   static Atom a = (Atom) NULL;
   Widget shell  = GetTrueShell(w);

   if (!a)
      a = XmInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW", False);

   if (XmIsVendorShell(shell)) {
      XmAddWMProtocols(shell, &a, 1);
      XmAddWMProtocolCallback(shell, a, callback, client_data);
   }
}

/***********************************************************************
 *                                                                     *
 *   Iconify a shell.                                                  *
 *                                                                     *
 ***********************************************************************/
void IconifyShell(Widget shell)
{
   static Atom          a = (Atom) NULL;
   Display             *dpy = XtDisplay(shell);
   XClientMessageEvent  ev;

   if (!a)
      a = XmInternAtom(dpy, "WM_CHANGE_STATE", False);

   ev.type         = ClientMessage;
   ev.display      = dpy;
   ev.message_type = a;
   ev.format       = 32;
   ev.data.l[0]    = IconicState;
   ev.window       = XtWindow(GetTrueShell(shell));

   XSendEvent(dpy,
            RootWindow(dpy, DefaultScreen(dpy)),
            True,
            (SubstructureRedirectMask | SubstructureNotifyMask),
            (XEvent *)&ev);
   XFlush(dpy);
}

/***********************************************************************
 *                                                                     *
 *   De-Iconify a shell.                                               *
 *                                                                     *
 ***********************************************************************/
void DeIconifyShell(Widget w)
{
   Widget shell = GetTrueShell(w);
   Display *dpy = XtDisplay(shell);

   XMapWindow(dpy, XtWindow(shell));
   XFlush(dpy);
}

/***********************************************************************
 *                                                                     *
 *   Execute a command (via KUIP).                                     *
 *                                                                     *
 ***********************************************************************/
void execute_kuip_cmd(char *cmd_text)
{
   km_all_cursor(KM_WAIT_CURSOR);

   ku_last(cmd_text);

   ku_exec(cmd_text);

   km_all_cursor(KM_MAIN_CURSOR);
}

/***********************************************************************
 *                                                                     *
 *   Show (manage) argument popup for the different commands.          *
 *                                                                     *
 ***********************************************************************/
void show_cb(Widget w, Widget dialog, XmAnyCallbackStruct *call_data)
{
   XtManageChild(dialog);
}

/***********************************************************************
 *                                                                     *
 *   Close (popdown) an interface (popup dialog or shell).             *
 *                                                                     *
 ***********************************************************************/
void close_cb(Widget w, Widget dialog, XmAnyCallbackStruct *call_data)
{
   UxPopdownInterface(dialog);
}

/***********************************************************************
 *                                                                     *
 *   Remove (unmanage) popup in case the cancel button was pressed.    *
 *                                                                     *
 ***********************************************************************/
void cancel_cb(Widget w, MenuCbStruct *item, XmAnyCallbackStruct *call_data)
{
   popdown_argument_dialog(item);
}

/***********************************************************************
 *                                                                     *
 *   Get HELP on the commands.                                         *
 *                                                                     *
 ***********************************************************************/
void help_cb(Widget w, char *help_cmd, XmAnyCallbackStruct *call_data)
{
   char *cmd;

   cmd = XtCalloc(MAX_CMD_LENGTH, 1);
   sprintf(cmd, "Help %s", help_cmd);

   execute_kuip_cmd(cmd);

   XtFree(cmd);
}

/***********************************************************************
 *                                                                     *
 *   Is shell in IconicState?                                          *
 *                                                                     *
 ***********************************************************************/
Boolean IsShellIconic(Widget w)
{
   static Atom    a      = (Atom) NULL;
   Widget         shell  = GetTrueShell(w);
   Display       *dpy    = XtDisplay(shell);
   Window         window = XtWindow(shell);
   unsigned long *property = NULL;
   unsigned long  nitems;
   unsigned long  leftover;
   Atom           actual_type;
   int            actual_format;
   int            status;
   Boolean        state;

   if (!a)
      a = XmInternAtom(dpy, "WM_STATE", False);

   status = XGetWindowProperty(dpy, window,
                               a, 0L, 1L,
                               False, a, &actual_type, &actual_format,
                               &nitems, &leftover, (unsigned char **)&property);

   if ( !((status == Success) &&
          (actual_type == a) && (nitems == 1L))) {
      if (property) {
         XFree((char *)property);
         return(False);
      }
   }

   state = (*property == IconicState) ? True : False;

   XFree((char *)property);
   return(state);
}

/***********************************************************************
 *                                                                     *
 *   This function is a superset of XmUpdateDisplay() in that it will  *
 *   ensure that a window's contents is visible before returning.      *
 *   The monitoring of window states is necessary because attempts to  *
 *   map a window is subject to the whim of the window manager -- this *
 *   introduces a significant delay before the window is actually      *
 *   mapped and exposed.                                               *
 *                                                                     *
 *   This function is intended to be called after XtPopup(),           *
 *   XtManageChild() or XMapRaised() on a widget (or window, for       *
 *   XMapRaised()). Don't use this for other situations as it might    *
 *   sit and process other unrelated events till the widget becomes    *
 *   visible.                                                          *
 *                                                                     *
 ***********************************************************************/
void ForceUpdate(Widget w)
{
   Widget        diashell, topshell;

   /* Locate shell we are interested in */
   for (diashell = w; !XtIsShell(diashell); diashell = XtParent(diashell))
      ;

   /* Locate its primary window's shell (which may be the same) */
   for (topshell = diashell; !XtIsTopLevelShell(topshell);
        topshell = XtParent(topshell))
      ;

   /*
    * If the dialog shell (or its primary shell window) is not realized,
    * don't bother... nothing can possibly happen.
    */
   if (XtIsRealized(diashell) && XtIsRealized(topshell)) {
      XEvent        event;
      XWindowAttributes xwa;
      Display      *dpy = XtDisplay(topshell);
      Window        diawindow = XtWindow(diashell);
      Window        topwindow = XtWindow(topshell);
      XtAppContext  cxt = XtWidgetToApplicationContext(w);

      /* Wait for the dialog to be mapped. It is guaranteed to become so */
      while (XGetWindowAttributes(dpy, diawindow, &xwa) &&
             xwa.map_state != IsViewable) {

         /*
          * ... if the primary is (or becomes) unviewable or unmapped,
          * it's probably iconic, and nothing will happen.
          */
         if (XGetWindowAttributes(dpy, topwindow, &xwa) &&
             xwa.map_state != IsViewable)
            break;

         /* We are guaranteed there will be an event of sme kind */
         XtAppNextEvent(cxt, &event);
         XtDispatchEvent(&event);
      }
   }

   /* The next XSync() will get an expose event */
   XmUpdateDisplay(topshell);
}

/***********************************************************************
 *                                                                     *
 *   Toggle a pushbutton widget (switching shadow colors).             *
 *                                                                     *
 ***********************************************************************/
void TogglePushButton(Widget button, Boolean set)
{
   Pixel  ts, bs;

   if (set) {
      XtVaGetValues(button, XmNbottomShadowColor, &bs, XmNtopShadowColor, &ts,
                    NULL);
      XtVaSetValues(button, XmNbottomShadowColor, ts, XmNtopShadowColor, bs,
                    NULL);
   } else {
      XtVaGetValues(button, XmNbottomShadowColor, &ts, XmNtopShadowColor, &bs,
                    NULL);
      XtVaSetValues(button, XmNbottomShadowColor, bs, XmNtopShadowColor, ts,
                    NULL);
   }
}

/***********************************************************************
 *                                                                     *
 *   Set a label pixmap.                                               *
 *                                                                     *
 ***********************************************************************/
void set_pixmap_to_button(Widget w, char *bits, int width, int height,
                          Boolean gadget)
{
   Widget        parent;
   Pixel         fg, bg;
   Pixmap        pixmap;
   Display      *disp;

   parent = w;
   if (gadget)
      parent = XtParent(w);

   XtVaGetValues(parent, XmNforeground, &fg, XmNbackground, &bg, NULL);

   disp = XtDisplay(topWidget);

   pixmap = XCreatePixmapFromBitmapData(disp, DefaultRootWindow(disp),
                        bits, width, height, fg, bg,
                        DefaultDepthOfScreen(XtScreen(topWidget)));

   XtVaSetValues(w, XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap, NULL);
}

#if defined(CERNLIB_FPANELS)
/***********************************************************************
 *                                                                     *
 *   Open the fitting panel on the current 1D histogram                *
 *                                                                     *
 ***********************************************************************/
void fitting_action(Widget w, BrClientdata *client_data,
                    XmAnyCallbackStruct *cbs)
{

/*
 *              1D histograms ....
 */
   if ( strcmp(client_data->kmcls,"1d") == 0 ) {
      histogram.id   = atoi(client_data->kmobj);
      histogram.type = HISTO_1D;
/* This line makes the fit panel crash on LINUX because */
/* client_data->path is not properly set before but in  */
/* any case it seems useless                            */
/*      strcpy(histogram.dir, client_data->path);       */
      set_histo_id(False);
      if (fitPanelContext==NULL) fphbrspanel();
      fp_hsetid(histogram.id, fitPanelContext);
   } else {
   }
}
#endif

/***********************************************************************
 *                                                                     *
 *   Perform the default action on the current histogram or ntuple     *
 *   in the KUIP browser                                               *
 *                                                                     *
 ***********************************************************************/
void default_action(Widget w, BrClientdata *client_data,
                    XmAnyCallbackStruct *cbs)
{

/*
 *              1D histograms ....
 */
   if ( strcmp(client_data->kmcls,"1d") == 0 ) {
      histogram.id   = atoi(client_data->kmobj);
      histogram.type = HISTO_1D;
      strcpy(histogram.dir, client_data->path);
      set_histo_id(False);
#if defined(CERNLIB_FPANELS)
      if (fitPanelContext!=NULL)
         fp_hsetid(histogram.id, fitPanelContext);
#endif
      plot_current_histo();
   }

/*
 *              2D histograms ....
 */
   if ( strcmp(client_data->kmcls,"2d") == 0 ) {
      histogram.id   = atoi(client_data->kmobj);
      histogram.type = HISTO_2D;
      strcpy(histogram.dir, client_data->path);
      set_histo_id(False);
#if defined(CERNLIB_FPANELS)
      if (fitPanelContext!=NULL)
         fp_hsetid(histogram.id, fitPanelContext);
#endif
      plot_current_histo();
   }

/*
 *              3D histograms ....
 */
   if ( strcmp(client_data->kmcls,"3d") == 0 ) {
      histogram.id   = atoi(client_data->kmobj);
      histogram.type = HISTO_3D;
      strcpy(histogram.dir, client_data->path);
      set_histo_id(False);
   }

/*
 *              Ntuples ....
 */
   if ( strcmp(client_data->kmcls,"ntuple") == 0 ) {
      ntuple.id = atoi(client_data->kmobj);
      strcpy(ntuple.dir, client_data->path);
      set_ntuple_id();
   }
}

/***********************************************************************
 *                                                                     *
 *   Perform the default action on the current histogram or ntuple     *
 *   in the HIGZ window                                                *
 *                                                                     *
 ***********************************************************************/
void default_G_action(Widget w, BrClientdata *client_data,
                      XmAnyCallbackStruct *cbs)
{
   if( !OptionPlot ) return;
   histogram.type = 0;
   if ( strcmp(client_data->kmcls,"1d") == 0 ) histogram.type = HISTO_1D;
   if ( strcmp(client_data->kmcls,"2d") == 0 ) histogram.type = HISTO_2D;

   if ( histogram.type == HISTO_1D || histogram.type == HISTO_2D ) {
      histogram.id   = atoi(client_data->kmobj);
      strcpy(histogram.dir, "//PAWC");

      if ( strcmp(client_data->mtext,"Line") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotL, NULL);
      if ( strcmp(client_data->mtext,"Curve") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotC, NULL);
      if ( strcmp(client_data->mtext,"Bar Chart") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotB, NULL);
      if ( strcmp(client_data->mtext,"Marker") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotP, NULL);
      if ( strcmp(client_data->mtext,"Stars") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotSTAR, NULL);
      if ( strcmp(client_data->mtext,"Error Bars") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotE, NULL);
      if ( strcmp(client_data->mtext,"Error Bars (lines)") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotE1, NULL);
      if ( strcmp(client_data->mtext,"Error Rectangles") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotE2, NULL);
      if ( strcmp(client_data->mtext,"Error: Filled Area") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotE3, NULL);
      if ( strcmp(client_data->mtext,"Error: Smoothed Area") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotE4, NULL);
      if ( strcmp(client_data->mtext,"Boxes") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotBOX, NULL);
      if ( strcmp(client_data->mtext,"Color") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotCOL, NULL);
      if ( strcmp(client_data->mtext,"Hidden Lines Surface") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotSURF, NULL);
      if ( strcmp(client_data->mtext,"Color Level Surface (1)") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotSURF1, NULL);
      if ( strcmp(client_data->mtext,"Color Level Surface (2)") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotSURF2, NULL);
      if ( strcmp(client_data->mtext,"Surface and Contour") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotSURF3, NULL);
      if ( strcmp(client_data->mtext,"Gouraud Shaded Surface") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotSURF4, NULL);
      if ( strcmp(client_data->mtext,"Hidden Lines Lego") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotLEGO, NULL);
      if ( strcmp(client_data->mtext,"Filled Lego") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotLEGO1, NULL);
      if ( strcmp(client_data->mtext,"Color Level Lego") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotLEGO2, NULL);
      if ( strcmp(client_data->mtext,"Contour Plot") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotCONT, NULL);
      if ( strcmp(client_data->mtext,"Filled Contour Plot") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotCONT3, NULL);
      if ( strcmp(client_data->mtext,"Text") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotTEXT, NULL);
      if ( strcmp(client_data->mtext,"Default") == 0 )
         XtVaSetValues(OptionPlot, XmNmenuHistory, OptionPlotD, NULL);

      set_histo_id(True);
   }
}

/***********************************************************************
 *                                                                     *
 *   Plot the current histogram.                                       *
 *                                                                     *
 * Note that in this procedure, HISTO_3D is (for the time being)       *
 * consider as an Ntuple in which 3 variables have to be plotted.      *
 *                                                                     *
 ***********************************************************************/
void plot_current_histo()
{
   if ( histogram.type == HISTO_1D ||
        histogram.type == HISTO_2D ||
        histogram.type == HISTO_3D ) {

      char *comstr = XtCalloc(MAX_CMD_LENGTH, 1);
      char *same   = XtCalloc(MAX_CMD_LENGTH, 1);
      char *chid   = XtCalloc(MAX_CMD_LENGTH, 1);
      char *chopt  = XtCalloc(MAX_CMD_LENGTH, 1);

      int ilen1, ilen2;
      if ( appres.same_zone ) {
         sprintf(same, "S");
      } else {
         sprintf(same, " ");
      }

      switch (histogram.type) {
         case HISTO_1D:
            sprintf(chid, "%s/%d", histogram.dir, histogram.id);
            sprintf(chopt, "%s,%s,%s", histogram.option_1d,
                                       histogram.coordinate, same);
            break;

         case HISTO_2D:
            sprintf(chid, "%s/%d", histogram.dir, histogram.id);
            sprintf(chopt, "%s,%s,%s", histogram.option_2d,
                                       histogram.coordinate, same);
            break;

         case HISTO_3D:
            sprintf(comstr, "Ntuple/Plot %s/%d%s %s %d %d ! '%s%s'",
                    ntuple.dir, ntuple.id, ntuple.xyz, ntuple.cut,
                    ntuple.nevent, ntuple.first,
                    histogram.option_3d, same);
            execute_kuip_cmd(comstr);
            break;
      }

      if (histogram.type == HISTO_1D || histogram.type == HISTO_2D) {
         ilen1 = strlen(chid);
         ilen2 = strlen(chopt);
         km_all_cursor(KM_WAIT_CURSOR);
         Plot_histo(chid, ilen1, chopt, ilen2,
                    &histogram.x1, &histogram.x2,
                    &histogram.y1, &histogram.y2);
         km_all_cursor(KM_MAIN_CURSOR);
      }

      if ((histogram.XAxisTitle) || (histogram.YAxisTitle)) {
         if (!histogram.XAxisTitle) {
            sprintf(comstr,"ATitle '  ' '%s'", histogram.YAxisTitle);
         } else if (!histogram.YAxisTitle) {
            sprintf(comstr,"ATitle '%s' '  '", histogram.XAxisTitle);
         } else
            sprintf(comstr,"ATitle '%s' '%s'", histogram.XAxisTitle,
                                               histogram.YAxisTitle);
         execute_kuip_cmd(comstr);
      }

      XtFree(same);
      XtFree(chid);
      XtFree(chopt);
      XtFree(comstr);
   }
}
