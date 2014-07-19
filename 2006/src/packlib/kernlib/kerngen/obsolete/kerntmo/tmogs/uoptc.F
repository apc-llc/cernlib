*
* $Id: uoptc.F,v 1.1.1.1 1996/02/15 17:53:50 mclareni Exp $
*
* $Log: uoptc.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:50  mclareni
* Kernlib
*
*
#include "kerntmo/pilot.h"
      SUBROUTINE UOPTC (DATA,POSS,IOPTV)
C
C CERN PROGLIB# M502    UOPTC           .VERSION KERNTMO  1.02  891211
C ORIG. 11/12/89, Mik Storr
C
      INTEGER      IOPTV(9)
      CHARACTER*(*)  DATA, POSS
 
 
      NP = LEN(POSS)
      ND = LEN(DATA)
      DO 25 J=1,NP
      IOPTV(J) = 0
      DO 24 K=1,ND
      IF (DATA(K:K).EQ.POSS(J:J)) IOPTV(J)=1
   24 CONTINUE
   25 CONTINUE
      RETURN
      END
#ifdef CERNLIB_TCGEN_UOPTC
#undef CERNLIB_TCGEN_UOPTC
#endif
