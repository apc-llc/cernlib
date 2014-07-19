/*
 * $Id: utils.c,v 1.1.1.1 1996/03/08 15:33:04 mclareni Exp $
 *
 * $Log: utils.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:04  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/03 10/01/95  09.20.57  by  N.Cremel*/
/*-- Author :    Fons Rademakers   8/01/93*/
/* includes */
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <Xm/Xm.h>
#include <Xm/SashP.h>
#include <X11/IntrinsicP.h>
#if defined(__hpux) && XtSpecificationRelease == 4
#include <X11/Protocols.h>
#else
#include <Xm/Protocols.h>
#endif
#include <Xm/AtomMgr.h>

#include "utils.h"


/***********************************************************************
 *                                                                     *
 *   Returns pointer to last character when last character is c,       *
 *   otherwise 0. Ignore ' ' and \n.                                   *
 *                                                                     *
 ***********************************************************************/
char *my_strrstr( s1, s2 )
     const char *s1;
     const char *s2;
{
   int    i;
   int    ls1 = strlen(s1);
   int    ls2 = strlen(s2);

   if (!s1 || !ls1 || !s2 || !ls2) return NULL;

   for (i = ls1; i > 0; i--) {
      if (s1[i-1] == *s2) {
         if (!strncmp(&s1[i-1], s2, ls2))
            return (char *)&s1[i-1];
      }
   }
   return NULL;
}

/***********************************************************************
 *                                                                     *
 *   Returns true if string is empty (only containing blanks).         *
 *                                                                     *
 ***********************************************************************/
Boolean strempty( str )
     char *str;
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
 *   Strip blanks from both sides of a string. Space for the new       *
 *   string is allocated and a pointer to it is returned.              *
 *   The user must free the space.                                     *
 *                                                                     *
 ***********************************************************************/
char *strip( s )
     char *s;
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

/***********************************************************************
 *                                                                     *
 *   Returns pointer to last character when last character is c,       *
 *   otherwise 0. Ignore ' ' and \n.                                   *
 *                                                                     *
 ***********************************************************************/
char *strend( str, c )
     char *str;
     int c;
{
   int i;

   if (!str || !strlen(str)) return NULL;

   for (i = strlen(str); i > 0; i--) {
      if (str[i-1] == ' ' || str[i-1] == '\n')
         continue;

      if (str[i-1] != c)
         return NULL;

      return str+i-1;
   }
   return NULL;
}

/***********************************************************************
 *                                                                     *
 *   Change the characters in str into lower case.                     *
 *                                                                     *
 ***********************************************************************/
void lower( str )
     char *str;
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
 *   Popdown dialog and make menu item sensitive.                      *
 *                                                                     *
 ***********************************************************************/
void popdown_dialog( dialog )
     Widget dialog;
{
   Widget  button;

   XtVaGetValues(dialog, XmNuserData, &button, NULL);

   XtSetSensitive(button, True);
   XtUnmanageChild(dialog);
}

/***********************************************************************
 *                                                                     *
 *   Remove (unmanage) popup in case the cancel button was pressed,    *
 *   and make the menu button sensitive again (via popdown_dialog).    *
 *                                                                     *
 ***********************************************************************/
void cancel_cb( w, dialog, call_data )
     Widget w;
     Widget dialog;
     XmAnyCallbackStruct *call_data;
{
   popdown_dialog(dialog);
}

/***********************************************************************
 *                                                                     *
 *   Remove (unmanage) popup in case the cancel button was pressed.    *
 *                                                                     *
 ***********************************************************************/
void unmanage_cb( w, dialog, call_data )
     Widget w;
     Widget dialog;
     XmAnyCallbackStruct *call_data;
{
   XtUnmanageChild(dialog);
}

/***********************************************************************
 *                                                                     *
 *   Returns the true shell widget.                                    *
 *                                                                     *
 ***********************************************************************/
static Widget GetTrueShell( w )
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
void InstallMwmCloseCallback( w, callback, client_data )
     Widget w;
     XtCallbackProc callback;
     XtPointer client_data;
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
void IconifyShell( shell )
     Widget shell;
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
 *   Resetting XmNtraversalOn on all the sashes in a paned window.     *
 *                                                                     *
 ***********************************************************************/
void TurnOffSashTraversal( pane )
     Widget pane;
{
   Widget  *children;
   int      num_children;

   XtVaGetValues(pane, XmNchildren,    &children,
                       XmNnumChildren, &num_children,
                       NULL);

   /*
    * Instead of using the private function XmIsSash one could also
    * check for the widget name being "sash" (using XtName).
    */
   while (num_children--)
      if (XmIsSash(children[num_children]))
         XtVaSetValues(children[num_children], XmNtraversalOn, False, NULL);
}
