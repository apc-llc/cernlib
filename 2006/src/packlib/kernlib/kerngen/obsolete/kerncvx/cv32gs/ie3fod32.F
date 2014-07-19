*
* $Id: ie3fod32.F,v 1.1.1.1 1996/02/15 17:52:21 mclareni Exp $
*
* $Log: ie3fod32.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:21  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE IE3FOD (MS,MT,NDPN,JBAD)
C
C CERN PROGLIB# M220    IE3FOD          .VERSION KERNCVX  1.09  910815
C Orig. 31/05/89  JZ

C-    Convert REAL*8 to double precision for output with copy
C-    from source in native to target in IEEE data format

      DIMENSION    MS(99), MT(99)

      INTEGER*4    MTHIS(2), ITHA, ITHB
      EQUIVALENCE (ITHA,MTHIS(1)), (ITHB,MTHIS(2))

      PARAMETER    (MSKEXP = '7FF00000'X)
      PARAMETER    (IBADCD = '7FF01FC0'X)
      PARAMETER    (IOVPCD = '7FF00000'X)
      PARAMETER    (IOVNCD = 'FFF00000'X)


      JFAI = 0
      JMS  = 0
      JMT  = 0

      DO 449  JL=1,NDPN
      ITHA = MS(JMS+1)
      IF (ITHA.EQ.0)               GO TO 442
      JSIGN = ishft (ITHA, -31)
      JEXP  = ishft (ITHA.AND.MSKEXP, -20)
      IF (JEXP.EQ.0)               GO TO 435
      IF (JEXP.EQ.2047)            GO TO 433
      JEXP  = JEXP - 2
      IF (JEXP.LE.0)               GO TO 441
      ITHA  = (ITHA .AND. .NOT.MSKEXP) .OR. ISHFT(JEXP,20)
      ITHB  = MS(JMS+2)
      GO TO 447

C--       overflow
  433 IF (JSIGN.EQ.0)  THEN
          ITHA = IOVPCD
        ELSE
          ITHA = IOVNCD
        ENDIF
      GO TO 437

C--       NaN
  435 IF (JSIGN.EQ.0)              GO TO 441
      ITHA = IBADCD
  437 JFAI = JL
      GO TO 442

  441 ITHA = 0
  442 ITHB = 0
  447 MT(JMT+1) = ITHA
      MT(JMT+2) = ITHB
      JMT = JMT + 2
  449 JMS = JMS + 2

      JBAD = JFAI
      RETURN
      END
#ifdef CERNLIB_TCGEN_IE3FOD
#undef CERNLIB_TCGEN_IE3FOD
#endif
