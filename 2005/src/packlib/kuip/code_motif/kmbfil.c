/*
 * $Id: kmbfil.c,v 1.1.1.1 1996/03/08 15:33:08 mclareni Exp $
 *
 * $Log: kmbfil.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:08  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/09 19/05/94  17.22.57  by  Unknown*/
/*-- Author :    N.Cremel   27/10/92*/

#include "kuip/kuip.h"
#include "kuip/mkutfu.h"



/***********************************************************************
 *                                                                     *
 *   This routine is called by the Kuip Browser for Files.             *
 *                                                                     *
 ***********************************************************************/
char **kmbfil( brobj_name, brcls_name, bpath, n )
     char *brobj_name;
     char *brcls_name;
     char *bpath;
     int n;
{
  char       **fdesc;
  static char *expath;

  if (n == 0)
     expath = fexpand(bpath, NULL);

  if (!(fdesc = km_scan_dir( expath, "*", n, 0 )))
     free(expath);

  return fdesc;
}


