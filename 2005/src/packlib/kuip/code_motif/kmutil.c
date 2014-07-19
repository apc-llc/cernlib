/*
 * $Id: kmutil.c,v 1.1.1.1 1996/03/08 15:33:09 mclareni Exp $
 *
 * $Log: kmutil.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:09  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/09 06/07/94  15.26.00  by  Alfred Nathaniel*/
/*-- Author :    N.Cremel   03/04/92*/
/******************************************************************************/
/*                                                                            */
/*           MOTIF-based library for KUIP - deck KMDIAL                       */
/*                                                                            */
/*           General utilities which do NOT refer any Motif/X11 include files */
/*                                                                            */
/*           - km_strip : Strip blanks from both sides of a string            */
/*           - km_strempty : Returns true (1) if string is empty              */
/*                                                                            */
/******************************************************************************/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kmenu.h"

#include "mkutda.h"
#include "kuip/mkutfu.h"


/***********************************************************************
 *                                                                     *
 *   Strip blanks from both sides of a string. Space for the new       *
 *   string is allocated and a pointer to it is returned.              *
 *                                                                     *
 ***********************************************************************/
char *km_strip(s)
     char *s;
{
  return strtrim( strdup( s ) );
}


/***********************************************************************
 *                                                                     *
 *   Returns true (1) if string is empty (only containing blanks).     *
 *                                                                     *
 ***********************************************************************/
int km_strempty(str)
   char *str;
{
   char    *sav  = str;
   int      rtrn = 1;

   if (!sav) return rtrn;
   if (strlen(sav) == 0) return rtrn;

   while (*sav) {
      if (*sav != ' ') {
       rtrn = 0;
       break;
      }
      sav++;
   }
   return rtrn ;
}

/***********************************************************************
 *                                                                     *
 *   Searches backwards for character c while ignoring ' ' and '\n'.   *
 *   If c not found return NULL otherwise pointer to c.                *
 *                                                                     *
 ***********************************************************************/
char *km_strend(str, c)
   char *str;
   int   c;
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
