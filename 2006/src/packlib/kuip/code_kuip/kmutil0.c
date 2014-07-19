/*
 * $Id: kmutil0.c,v 1.2 1997/11/28 17:24:46 mclareni Exp $
 *
 * $Log: kmutil0.c,v $
 * Revision 1.2  1997/11/28 17:24:46  mclareni
 * Add Id and Log for CVS
 *
 */
/******************************************************************************/
/*                                                                            */
/*           - km_strempty : Returns true (1) if string is empty              */
/*                                                                            */
/******************************************************************************/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kmenu.h"

#include "kuip/mkutfu.h"

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

