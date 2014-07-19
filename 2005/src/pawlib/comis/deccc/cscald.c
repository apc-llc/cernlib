/*
 * $Id: cscald.c,v 1.5 2004/10/27 09:01:54 couet Exp $
 *
 * $Log: cscald.c,v $
 * Revision 1.5  2004/10/27 09:01:54  couet
 * - switch rewritten
 *
 * Revision 1.4  1997/09/02 15:50:40  mclareni
 * WINNT corrections
 *
 * Revision 1.3  1997/03/14 12:02:23  mclareni
 * WNT mods
 *
 * Revision 1.2.2.1  1997/01/21 11:34:42  mclareni
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
/*CMZ :          22/11/95  10.40.58  by  Julian Bunn*/
/*-- Author :*/

#define cscald   
#undef  cscald

#ifdef CERNLIB_WINNT
# include <stdio.h>
#endif

#if (defined(CERNLIB_QX_SC))&&(!defined(CERNLIB_WINNT))
double cscald_ (name,n,p)
#endif
#if defined(CERNLIB_QXNO_SC)
double cscald (name,n,p)
#endif
#if defined(CERNLIB_QXCAPT)
#  ifdef CERNLIB_MSSTDCALL
    double type_of_call CSCALD(name,n,p)
#  else
    int CSCALD (name,n,p)
#  endif
#endif
 double (type_of_call *(*name)) ();
 int *n;
 int *p[15];
{
   double d;
   switch (*n)
   {
      case 0:
         d = ((*(*name))());
	 break;
      case 1:
         d = ((*(*name))(p[0]));
	 break;
      case 2:
         d = ((*(*name))(p[0],p[1]));
	 break;
      case 3:
         d = ((*(*name))(p[0],p[1],p[2]));
	 break;
      case 4:
         d = ((*(*name))(p[0],p[1],p[2],p[3]));
	 break;
      case 5:
         d = ((*(*name))(p[0],p[1],p[2],p[3],p[4]));
	 break;
      case 6:
         d = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5]));
	 break;
      case 7:
         d = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6]));
	 break;
      case 8:
         d = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7]));
	 break;
      case 9:
         d = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8]));
	 break;
      case 10:
         d = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9]));
	 break;
      case 11:
         d = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10]));
	 break;
      case 12:
         d = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11]));
	 break;
      case 13:
         d = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12]));
	 break;
      case 14:
         d = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13]));
	 break;
      case 15:
         d = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14]));
	 break;
      case 16:
         d = ((*(*name))(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],p[12],p[13],p[14],p[15]));
	 break;
      default:
         printf("\n More then 16 arguments in call users routine");
   }
   return d;
}
#endif
