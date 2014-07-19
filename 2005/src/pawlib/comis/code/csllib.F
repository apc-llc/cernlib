*
* $Id: csllib.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: csllib.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/02 08/07/93  18.23.56  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSLLIB(J,N)
********************************************
*                                          *
*   FIND THE MODULE (J,N) IN THE LIBRARY.  *
*                                          *
*   LSCLIB = 1  - SUCCESS                  *
*          = 0  - THE MODULE ISN'T FOUND   *
*                                          *
*   N MUST NOT BE >40                      *
*                                          *
********************************************
#if defined(CERNLIB_VAX)
      CHARACTER*40 MODULE
      COMMON /CSLBRW/ LIBIND,LIBST,LEOF,LIBRD,LIBTXT
      DIMENSION IRFA(2)
*
      CALL CCOPYS(J,MJSCHA(MODULE),N)
      CSLLIB=0
*
      LIBST=LBR$LOOKUP_KEY(LIBIND,MODULE(1:N),IRFA)
*
      IF(.NOT.LIBST) RETURN
      CSLLIB=1
#endif
#if !defined(CERNLIB_VAX)
      CSLLIB=0
#endif
      END
