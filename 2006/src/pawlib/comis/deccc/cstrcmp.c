/*
 * $Id: cstrcmp.c,v 1.4 1997/09/02 15:50:44 mclareni Exp $
 *
 * $Log: cstrcmp.c,v $
 * Revision 1.4  1997/09/02 15:50:44  mclareni
 * WINNT corrections
 *
 * Revision 1.3  1997/03/14 12:02:24  mclareni
 * WNT mods
 *
 * Revision 1.2.2.1  1997/01/21 11:34:44  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.2  1996/03/14 13:44:13  berezhno
 * mods for WINNT
 *
 * Revision 1.1.1.1  1996/02/26 17:16:55  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"
#if !defined(CERNLIB_ALPHA_OSF)
/*CMZ :  1.16/13 16/09/93  16.09.32  by  Rene Brun*/
/*-- Author :*/

#ifdef CERNLIB_WINNT
# include <stdio.h>
#endif

#if defined(CERNLIB_QXCAPT) && defined(CERNLIB_MSSTDCALL)
/*
 int type_of_call CSTRCMP(ja,len_ja, na,jb, len_jb, nb)
    int len_ja, len_jb;
*/
 int type_of_call CSTRCMP(ja, na,jb, nb)
#else
int cstrcmp_(ja,na,jb,nb)
#endif
     char **ja, **jb;
     int *na, *nb;
{
  int i,la,lb,k; char *a,*b;
  if ( *na >= *nb )
     {
      a=*ja; b=*jb; la=*na; lb=*nb; k=1;
      }
  else
    {
      a=*jb; b=*ja; la=*nb; lb=*na; k=-1;
     }
  for ( i=0; i<lb; i++ )
    { if(a[i] != b[i])
      { return ( (a[i] > b[i]) ? k : -k ); }
     }
  while( i < la )
    {
        if(a[i] > ' ')        return (k);
        else if( a[i] < ' ') return (-k);
        i++;
    }
    return(0);
}
#endif
