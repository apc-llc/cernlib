/*
 * $Id: ccopys.c,v 1.4 1997/09/02 15:50:38 mclareni Exp $
 *
 * $Log: ccopys.c,v $
 * Revision 1.4  1997/09/02 15:50:38  mclareni
 * WINNT corrections
 *
 * Revision 1.3  1997/03/14 12:02:22  mclareni
 * WNT mods
 *
 * Revision 1.2.2.1  1997/01/21 11:34:41  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.2  1996/03/14 13:44:09  berezhno
 * mods for WINNT
 *
 * Revision 1.1.1.1  1996/02/26 17:16:55  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"
#if !defined(CERNLIB_ALPHA_OSF)
/*CMZ :  1.16/13 16/09/93  16.08.50  by  Rene Brun*/
/*-- Author :*/
/*-- Author :*/
 

#if defined(CERNLIB_QXCAPT) && defined(CERNLIB_MSSTDCALL)
/* void type_of_call CCOPYS(ja,len_ja, jb, len_jb, nn)
     int len_ja, len_jb;
*/
void type_of_call CCOPYS(ja, jb, nn)
#else
void ccopys_(ja,jb,nn)
#endif
     char **ja, **jb;
     int *nn;
{
  int i,n; char *a,*b;
  n=*nn; a=*ja; b=*jb;
  if ( a >= b )
     for ( i=0; i<n; i++ )
          b[i]=a[i];
  else
    { if(a+n > b)
       for ( i=n-1; i>=0; i-- )
           b[i]=a[i];
      else
        for ( i=0; i<n; i++ )
           b[i]=a[i];

     }
}
#endif
