/*
 * $Id: ntwork.c,v 1.1.1.1 1996/03/01 11:38:56 mclareni Exp $
 *
 * $Log: ntwork.c,v $
 * Revision 1.1.1.1  1996/03/01 11:38:56  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.04/15 11/03/94  19.25.08  by  Fons Rademakers*/
/*-- Author :    Fons Rademakers   03/05/93*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/ScrollBar.h>

#include "hmotif/pawm.h"

/* extern functions */
extern Widget create_ntupleWork(Widget);

/* extern widgets */
extern Widget   km_toplevel;
extern Widget   messNtWLabel;
extern Widget   passNtWLabel;
extern Widget   minNtWLabel;
extern Widget   maxNtWLabel;
extern Widget   progressNtWScrollbar;
extern Widget   chainNtWLabel;
extern Widget   eventNtWLabel;
extern Widget   fileNtWLabel;
extern Widget   hostNtWLabel;
extern Widget   stopNtWButton;
extern Widget   stopNtWLabel;

/* local data */
static Widget ntWorkDialog;
static int    interrupt;
static int    n_pass;
static int    i_pass;
static int    imin;
static int    imax;
static int    events_done;
static int    nchains;
static int    ichain;

/*static char mess1[] = "Making 2 passes   (lacking xx MB of buffer space).";*/
static char mess2[] = "Making 1 pass.";
static char stop1[] = "at next update   (no plotting will be done).";
static char stop2[] = "at next update.";


/***********************************************************************
 *                                                                     *
 *   User pressed the stop button.                                     *
 *                                                                     *
 ***********************************************************************/
static void stop_cb(Widget dialog, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   interrupt = 1;
}

/***********************************************************************
 *                                                                     *
 *   Create, init and show the Ntuple Work dialog.                     *
 *                                                                     *
 ***********************************************************************/
#ifdef vms
void ntwork_init(int *npass, int *ipass, int *ifrom, int *ito, int *misbyt,
                 int *nchain, struct dsc$descriptor_s *thost_dsc, int *lh)
{
   char *thost = thost_dsc->dsc$a_pointer;
#else
void ntwork_init(int *npass, int *ipass, int *ifrom, int *ito, int *misbyt,
                 int *nchain, char *thost, int *lh)
{
#endif
   char    mess[128], host[80];
   float   mb;

   if (!ntWorkDialog) {
      ntWorkDialog = create_ntupleWork(km_toplevel);

      XtAddCallback(stopNtWButton, XmNactivateCallback,
                    (XtCallbackProc)stop_cb, NULL);
   }

   /* initialize dialog */
   if (*npass == 1) {
      XtVaSetValues(messNtWLabel, RES_CONVERT(XmNlabelString, mess2), NULL);
      XtVaSetValues(passNtWLabel, RES_CONVERT(XmNlabelString, "Pass 1"), NULL);
      XtVaSetValues(stopNtWLabel, RES_CONVERT(XmNlabelString, stop2), NULL);
   } else {
      if (*ipass == 1) {
         mb = (float)(*misbyt)/(1024.0*1024.0) + 1.0;
         sprintf(mess,
            "Making 2 passes   (lacking %d MB of buffer space for one pass).",
            (int) mb);
         XtVaSetValues(messNtWLabel, RES_CONVERT(XmNlabelString, mess), NULL);
         XtVaSetValues(passNtWLabel, RES_CONVERT(XmNlabelString, "Pass 1"),
                       NULL);
         XtVaSetValues(stopNtWLabel, RES_CONVERT(XmNlabelString, stop1), NULL);
      } else {
         XtVaSetValues(passNtWLabel, RES_CONVERT(XmNlabelString, "Pass 2"),
                       NULL);
         XtVaSetValues(stopNtWLabel, RES_CONVERT(XmNlabelString, stop2), NULL);
      }
   }

   strncpy(host, thost, *lh); host[*lh] = '\0';
   XtVaSetValues(hostNtWLabel, RES_CONVERT(XmNlabelString, host), NULL);
   XtVaSetValues(chainNtWLabel, RES_CONVERT(XmNlabelString, " "), NULL);
   XtVaSetValues(eventNtWLabel, RES_CONVERT(XmNlabelString, " "), NULL);
   XtVaSetValues(fileNtWLabel, RES_CONVERT(XmNlabelString, " "), NULL);
   XmScrollBarSetValues(progressNtWScrollbar, 0, 1, 0, 0, False);

   n_pass      = *npass;
   i_pass      = *ipass;
   imin        = *ifrom;
   imax        = *ito;
   nchains     = *nchain;
   events_done = 0;
   ichain      = 0;
   interrupt   = 0;

   XtManageChild(ntWorkDialog);

   /* make sure updates in work dialog are visible */
   ForceUpdate(ntWorkDialog);
}

/***********************************************************************
 *                                                                     *
 *   Check Stop button and show progressin work dialog.                *
 *                                                                     *
 ***********************************************************************/
#ifdef vms
void ntwork_check(int *iedone, struct dsc$descriptor *tfile_dsc, int *lf,
                               struct dsc$descriptor *thost_dsc, int *lh,
                  int *icont)
{
   char *tfile = tfile_dsc->dsc$a_pointer;
   char *thost = thost_dsc->dsc$a_pointer;
#else
void ntwork_check(int *iedone, char *tfile, int *lf, char *thost, int *lh,
                  int *icont)
{
#endif
   Display  *dpy = XtDisplay(ntWorkDialog);
   Window    win = XtWindow(stopNtWButton);
   XEvent    event;
   char      file[128], s[160], host[80];
   float     per;
   int       iper;

   /* Make sure all our requests get to the server */
   XFlush(dpy);

   /* Let Motif process all pending exporsure events for us */
   XmUpdateDisplay(ntWorkDialog);

   /* Check the event loop for events in the dialog ("Stop"?) */
   while (XCheckMaskEvent(dpy,
          ButtonPressMask | ButtonReleaseMask | ButtonMotionMask |
          PointerMotionMask | KeyPressMask | KeyReleaseMask,
          &event)) {
      /* got an interesting event */
      if (event.xany.window == win)
         XtDispatchEvent(&event);        /* it's in our dialog */
      else
         XBell(dpy, 50);                 /* throw away and sound bell */
   }

   events_done += *iedone;

   /*
    * when all files processed update progress scrollbar to 100%
    * and popdown workdialog
    */
   if (*icont) {
      interrupt = *icont;
      XmScrollBarSetValues(progressNtWScrollbar, 0, 100, 0, 0, False);
      /* make sure updates in work dialog are visible */
      XFlush(dpy);
      XmUpdateDisplay(ntWorkDialog);

      /* do not popdown when whe have to do second pass */
      if (n_pass == 2 && i_pass == 1)
         return;
   }

   if (!interrupt) {
      per = (float) events_done / ((float)(imax-imin+1)) * 100.0;
      iper = (int) per;
      if (iper <= 0)  iper = 1;
      if (iper > 100) iper = 100;
      XmScrollBarSetValues(progressNtWScrollbar, 0, iper, 0, 0, False);

      strncpy(file, tfile, *lf); file[*lf] = '\0';
      XtVaSetValues(chainNtWLabel, RES_CONVERT(XmNlabelString, file), NULL);

      strncpy(host, thost, *lh); host[*lh] = '\0';
      XtVaSetValues(hostNtWLabel, RES_CONVERT(XmNlabelString, host), NULL);

      sprintf(s, "%d  (%d)", ichain, nchains);
      XtVaSetValues(fileNtWLabel, RES_CONVERT(XmNlabelString, s), NULL);
      ichain++;

      sprintf(s, "%d  (%d)", events_done, imax-imin+1);
      XtVaSetValues(eventNtWLabel, RES_CONVERT(XmNlabelString, s), NULL);

      /* make sure updates in work dialog are visible */
      ForceUpdate(ntWorkDialog);
   } else {
      UxPopdownInterface(ntWorkDialog);
      XFlush(dpy);
   }

   *icont = interrupt;
}
