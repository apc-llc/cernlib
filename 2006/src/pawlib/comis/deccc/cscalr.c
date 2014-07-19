/*
 * $Id: cscalr.c,v 1.5 2004/10/22 12:03:16 couet Exp $
 *
 * $Log: cscalr.c,v $
 * Revision 1.5  2004/10/22 12:03:16  couet
 * - The previous version of this routine returned NAN on the lxplus machine
 *   lxslc3
 *
 * Revision 1.4  1997/09/02 15:50:42  mclareni
 * WINNT corrections
 *
 * Revision 1.3  1997/03/14 12:02:24  mclareni
 * WNT mods
 *
 * Revision 1.2.2.1  1997/01/21 11:34:43  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.2  1996/03/14 13:44:12  berezhno
 * mods for WINNT
 *
 * Revision 1.1.1.1  1996/02/26 17:16:55  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"
#if !defined(CERNLIB_ALPHA_OSF)
/*CMZ :          22/11/95  10.40.22  by  Julian Bunn*/
/*-- Author :*/

#define cscalr
#undef  cscalr

#ifdef CERNLIB_WINNT
# include <stdio.h>
#endif


#if (defined(CERNLIB_QX_SC))&&(!defined(CERNLIB_WINNT))
float cscalr_ (name,n,p)
#endif

#if defined(CERNLIB_QXNO_SC)
float cscalr (name,n,p)
#endif

#if defined(CERNLIB_QXCAPT)
# if defined(CERNLIB_MSSTDCALL)
   float type_of_call CSCALR(name,n,p)
# else
   int CSCALR (name,n,p)
# endif
#endif

 float (type_of_call *(*name)) ();
 int *n;
 int *p[15];
{
   float r;
   switch (*n)
   {
      case 0:
         r = ((*(*name))());
         break;
      case 1:
         r = ((*(*name))(p[0]));
         break;
      case 2:
         r = ((*(*name))(p[0],p[1]));
         break;
      case 3:
         r = ((*(*name))(p[0],p[1],p[2]));
         break;
      case 4:
         r = ((*(*name))(p[0],p[1],p[2],p[3]));
         break;
      case 5:
         r = ((*(*name))(p[0],p[1],p[2],p[3],p[4]));
         break;
      case 6:
         r = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5]));
         break;
      case 7:
         r = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]));
         break;
      case 8:
         r = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7]));
         break;
      case 9:
         r = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8]));
         break;
      case 10:
         r = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9]));
         break;
      case 11:
         r = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10]));
         break;
      case 12:
         r = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11]));
         break;
      case 13:
         r = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12]));
         break;
      case 14:
         r = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13]));
         break;
      case 15:
         r = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14]));
         break;
      case 16:
         r = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14],p[15]));
         break;
      default:
         printf("\n More then 16 arguments in call users routine");
   }
   return r;
}
#endif
