*
* $Id: iuforw.F,v 1.1.1.1 1996/02/15 17:51:02 mclareni Exp $
*
* $Log: iuforw.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:02  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION IUFORW (IC,JL,JR)
C
C CERN PROGLIB# M410    IUFORW          .VERSION KERNAPO  1.00  831212
C ORIG. 12/12/83  JZ
C
      DIMENSION    IC(99)
      COMMON /SLATE/ ND,JEND, DUMMY(38)

      ND   = 0
      JEND = JL
      IVAL = 0

   12 IF (JEND.GT.JR)        GO TO 20
      K = RSHFT (IC(JEND),24)
      IF (K.EQ.32)           GO TO 19
      IF (K.GE.58)           GO TO 20
      K = K - 48
      IF (K.LT.0)            GO TO 20
      IVAL = 10*IVAL + K
      ND   = ND + 1
   19 JEND = JEND + 1
      GO TO 12

   20 IUFORW = IVAL
      RETURN
      END
#ifdef CERNLIB_TCGEN_IUFORW
#undef CERNLIB_TCGEN_IUFORW
#endif
