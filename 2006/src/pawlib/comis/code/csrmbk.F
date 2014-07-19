*
* $Id: csrmbk.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: csrmbk.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_OLD)
*CMZ :  1.18/00 11/02/94  09.59.26  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSRMBK(T)
      CHARACTER *(*)T, ST*80
      J=1
      ST=' '
      DO 10 K=1,LEN(T)
        IF(T(K:K).NE.' ')THEN
          ST(J:J)=T(K:K)
          J=J+1
        ENDIF
  10  CONTINUE
      T=ST
      END
#endif
