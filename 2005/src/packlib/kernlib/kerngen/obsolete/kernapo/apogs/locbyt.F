*
* $Id: locbyt.F,v 1.1.1.1 1996/02/15 17:51:03 mclareni Exp $
*
* $Log: locbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:03  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION LOCBYT (IT,IA,N,NW,LA,NBITS)
C
C CERN PROGLIB# M428    LOCBYT          .VERSION KERNAPO  1.01  831216
C ORIG. 14/12/83  JZ
C
      DIMENSION IA(9)
C
C
      IF (N.EQ.0)  GO TO 19
      MASK = LSHFT (RSHFT(INT4(-1),32-NBITS),LA-1)
      ITSH = LSHFT (IT,LA-1)

      DO 18 J=1,N,NW
      IF (AND(IA(J),MASK).EQ.ITSH)  GO TO 21
   18 CONTINUE
   19 LOCBYT= 0
      RETURN
C
   21 LOCBYT= J
      RETURN
      END
#ifdef CERNLIB_TCGEN_LOCBYT
#undef CERNLIB_TCGEN_LOCBYT
#endif
