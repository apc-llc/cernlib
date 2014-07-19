*
* $Id: ie3fod.F,v 1.1.1.1 1996/02/15 17:52:36 mclareni Exp $
*
* $Log: ie3fod.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:36  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE IE3FOD (MS,MT,NDPN,JBAD)
C
C CERN PROGLIB# M220    IE3FOD          .VERSION KERNCRY  1.10  890322

C-    Convert double precision for output with copy
C-    from source in native to target in IEEE data format

      DIMENSION    MS(99), MT(99)

      DOUBLE PRECISION   THDB
      DIMENSION    THIS(2)
      EQUIVALENCE  (THDB,THIS)
      EQUIVALENCE  (ITHA,THA,THIS(1)), (ITHB,THB,THIS(2))

      PARAMETER    (IOVPCS = 17740000000 B)
      PARAMETER    (IOVPCD = 17774000000 B)
      PARAMETER    (IOVNCS = 37740000000 B)
      PARAMETER    (IOVNCD = 37774000000 B)


      JFAI = 0
      JMS  = 0
      JMT  = 0

C-       Cray single-pr. to IEEE double, ignoring 2nd word
C-                   for the moment (loss of 5 bits)
      DO 449  JL=1,NDPN
      ITHA = MS(JMS+1)
      IF (ITHA.EQ.0)         GO TO 442
      JSIGN = SHIFTR (MASK(1).AND.ITHA, 32)
      JEXP  = SHIFTR (SHIFTL(ITHA,1), 49)
      JEXP  = JEXP - 40000B + 1022
      IF (JEXP.LE.0)         GO TO 441
      IF (JEXP.GE.2047)      GO TO 433
      JMANT = MASK(128-20) .AND. SHIFTR (ITHA,27)
      ITHB  = SHIFTL (ITHA, 5) .AND. MASK(128-32)
      ITHA  = JSIGN .OR. SHIFTL(JEXP,20) .OR. JMANT
      GO TO 447

C--      overflow
  433 JFAI = JL
      IF (JSIGN.EQ.0)    THEN
          ITHA = IOVPCD
        ELSE
          ITHA = IOVNCD
        ENDIF
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
