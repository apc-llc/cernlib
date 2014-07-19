/*
 * $Id: strtograv.c,v 1.1.1.1 1996/03/01 11:39:31 mclareni Exp $
 *
 * $Log: strtograv.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:31  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 14/03/94  01.33.00  by  Fons Rademakers*/
/*-- Author :     Fons Rademakers   23/10/92*/

#include <string.h>

#if !defined(__convexc__) /* to avoid multiply definition strcasecmp */
extern int strcasecmp(const char *, const char *);
#endif /* __convexc__ */

#include <X11/Intrinsic.h>

#include "converter.h"


#define done(address, type) \
        { (*toVal).size = sizeof(type); (*toVal).addr = (caddr_t) address; }

static struct _namepair {
    XrmQuark quark;
    char *name;
    XtGravity gravity;
} names[] = {
    { NULLQUARK, XtEForget, ForgetGravity },
    { NULLQUARK, XtENorthWest, NorthWestGravity },
    { NULLQUARK, XtENorth, NorthGravity },
    { NULLQUARK, XtENorthEast, NorthEastGravity },
    { NULLQUARK, XtEWest, WestGravity },
    { NULLQUARK, XtECenter, CenterGravity },
    { NULLQUARK, XtEEast, EastGravity },
    { NULLQUARK, XtESouthWest, SouthWestGravity },
    { NULLQUARK, XtESouth, SouthGravity },
    { NULLQUARK, XtESouthEast, SouthEastGravity },
    { NULLQUARK, XtEStatic, StaticGravity },
    { NULLQUARK, XtEUnmap, UnmapGravity },
    { NULLQUARK, XtEleft, WestGravity },
    { NULLQUARK, XtEtop, NorthGravity },
    { NULLQUARK, XtEright, EastGravity },
    { NULLQUARK, XtEbottom, SouthGravity },
    { NULLQUARK, NULL, ForgetGravity }
};

/***********************************************************************
 *                                                                     *
 *   String To Gravity type converter used by the tree widget.         *
 *   Code based on StrToGrav.c from the Xaw X11R5 release.             *
 *                                                                     *
 ***********************************************************************/
void XrdmCvtStringToGravity (args, num_args, fromVal, toVal)
   XrmValuePtr args;
   Cardinal    *num_args;
   XrmValuePtr fromVal;
   XrmValuePtr toVal;
{
   char    lowerName[40];
   char   *s;
   struct _namepair *np;

   if (*num_args != 0)
      XtWarningMsg("wrongParameters","cvtStringToGravity","XtToolkitError",
                   "String to Gravity conversion needs no extra arguments",
                   (String *) NULL, (Cardinal *)NULL);

   s = (char *) fromVal->addr;
   if (strlen(s) < sizeof lowerName) {
      strcpy(lowerName, s);

      for (np = names; np->name; np++) {
         if (!strcasecmp(np->name, s)) {
            done (&np->gravity, XtGravity);
            return;
         }
      }
   }
   XtStringConversionWarning((char *) fromVal->addr, XtRGravity);
}
