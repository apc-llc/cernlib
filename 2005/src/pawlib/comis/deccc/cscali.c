/*
 * $Id: cscali.c,v 1.4 2004/10/22 12:42:40 couet Exp $
 *
 * $Log: cscali.c,v $
 * Revision 1.4  2004/10/22 12:42:40  couet
 * - for consistency with cscalr.c, this function has been modified the same way
 *
 * Revision 1.3  1997/03/14 12:02:24  mclareni
 * WNT mods
 *
 * Revision 1.2.2.1  1997/01/21 11:34:43  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.2  1996/03/14 13:44:11  berezhno
 * mods for WINNT
 *
 * Revision 1.1.1.1  1996/02/26 17:16:55  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"
#if !defined(CERNLIB_ALPHA_OSF)
/*CMZ :          22/11/95  10.39.30  by  Julian Bunn*/
/*-- Author :*/

#define cscali
#undef  cscali

#ifdef CERNLIB_WINNT
# include <stdio.h>
#endif

#if (defined(CERNLIB_QX_SC))&&(!defined(CERNLIB_WINNT))
int cscali_ (name,n,p)
#endif
#if defined(CERNLIB_QXNO_SC)
int cscali (name,n,p)
#endif
#if defined(CERNLIB_QXCAPT)
int type_of_call CSCALI (name,n,p)
#endif
 int (type_of_call *(*name)) ();
 int *n;
 int *p[15];
{
   int i;
   switch (*n)
   {
      case 0:
         i = ((*(*name))());
         break;
      case 1:
         i = ((*(*name))(p[0]));
         break;
      case 2:
         i = ((*(*name))(p[0],p[1]));
         break;
      case 3:
         i = ((*(*name))(p[0],p[1],p[2]));
         break;
      case 4:
         i = ((*(*name))(p[0],p[1],p[2],p[3]));
         break;
      case 5:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4]));
         break;
      case 6:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5]));
         break;
      case 7:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]));
         break;
      case 8:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7]));
         break;
      case 9:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8]));
         break;
      case 10:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9]));
         break;
      case 11:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10]));
         break;
      case 12:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11]));
         break;
      case 13:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12]));
         break;
      case 14:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13]));
         break;
      case 15:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14]));
         break;
      case 16:
         i = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14],p[15]));
         break;
      default:
         printf("\n More then 16 arguments in call users routine");
   }
   return i;
}
#endif
