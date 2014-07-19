/*
 * $Id: readfs.c,v 1.2 1996/03/12 16:58:52 cremel Exp $
 *
 * $Log: readfs.c,v $
 * Revision 1.2  1996/03/12 16:58:52  cremel
 * cpp run over the CDFs
 * Add new resource "hbookfile" to specify the extension for HBOOK files.
 * This can now be specify by the user in .Xdefaults, e.g.:
 *
 * Paw++*hbookfile: dat
 *
 * (default is "hbook").
 *
 * Revision 1.1.1.1  1996/03/01 11:38:56  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.04/15 05/03/94  14.53.31  by  Fons Rademakers*/
/*-- Author :    Fons Rademakers   17/11/92*/

#if defined(CERNLIB_QMALPH)
#ifdef TGV
#define getcwd DECC$GETCWD
#endif /* TGV */
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <Xm/Xm.h>       /* only to define _NO_PROTO in $KUIP */
#ifdef vms
#include <unixlib.h>
#endif

#include "hmotif/kuip.h"
#include "hmotif/pawm.h"

#define MAX_PATH   256

/***********************************************************************
 *                                                                     *
 *   Return one by one the *.hbook files. This routine is called by    *
 *   the Kuip Browser.                                                 *
 *                                                                     *
 ***********************************************************************/
char **blist_hbook_files(char *brobj_name, char *brcls_name,
                         char *bpath, int n)
{
   static int init = 0;
   char *filetype = XtCalloc(MAX_STRING, 1);
   char *file = XtCalloc(MAX_STRING, 1);
   char **scan;

   strcpy(filetype,".");
   strcat(filetype, appres.hbook_file);

   if (!init) {
      km_file_type(filetype, "hbook", "(Hbook File)", 0);
      init = 1;
   }

   strcpy(file,"*");
   strcat(file, filetype);
   scan = km_scan_dir(bpath, file, n, 1);

   XtFree(filetype);
   XtFree(file);

   return scan;
}

/***********************************************************************
 *                                                                     *
 *   Return the CWD for the *.hbook files. This routine is called by   *
 *   the Kuip Browser.                                                 *
 *                                                                     *
 ***********************************************************************/
char **bset_hbook_dir(char *class_name, int first)
{
   static char *path_desc[2];
   static char  root[MAX_PATH+6];
   char         path[MAX_PATH+1];

   path_desc[0] = NULL;
   path_desc[1] = NULL;

#ifdef NeXT
   if (first && getwd(path)) {
#else
   if (first && getcwd(path, MAX_PATH)) {
#endif
      sprintf(root, "root=%s", path);
      path_desc[0] = "Hbook";
      path_desc[1] = root;
   }
   return path_desc;
}
