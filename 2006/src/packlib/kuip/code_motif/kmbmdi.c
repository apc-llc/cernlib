/*
 * $Id: kmbmdi.c,v 1.1.1.1 1996/03/08 15:33:08 mclareni Exp $
 *
 * $Log: kmbmdi.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:08  mclareni
 * Kuip
 *
 */
/*CMZ :  2.01/00 18/02/93  11.15.03  by  Alfred Nathaniel*/
/*-- Author :    N.Cremel   27/10/92*/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/klink.h"
#include "kuip/kmenu.h"
#include "mkdims.h"


/***********************************************************************
 *                                                                     *
 *   Return the CWD for the *.kumac files.                             *
 *   This routine is called by the Kuip Browser for Macro.             *
 *                                                                     *
 ***********************************************************************/
char **kmbmdi( class_name, first )
     char *class_name;
     int first;
{
   static char *path_desc[2];
   static char  root[MAX_stringl+31];
   char         path[MAX_stringl+1];

   path_desc[0] = NULL;
   path_desc[1] = NULL;

   if (first && getcwd(path, MAX_stringl)) {
      sprintf(root, "root=%s file=\'List of KUIP macros\'", path);
      path_desc[0] = "Macro";
      path_desc[1] = root;
   }
   return path_desc;
}
