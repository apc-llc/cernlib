*
* $Id: ie3fos.F,v 1.1.1.1 1996/02/15 17:52:36 mclareni Exp $
*
* $Log: ie3fos.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:36  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE IE3FOS (MS,MT,NWDO,JBAD)
C
C CERN PROGLIB# M220    IE3FOS          .VERSION KERNCRY  1.05  861204

C-    Convert single precision for output with copy
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


      JBAD = 0
      JMS  = 0
      JMT  = 0

  301 DO 349  JL=1,NWDO
      ITHA = MS(JMS+1)
      IF (ITHA.EQ.0)         GO TO 347
      JSIGN = SHIFTR (MASK(1).AND.ITHA, 32)
      JEXP  = SHIFTR (SHIFTL(ITHA,1), 49)
      JEXP  = JEXP - 40000B + 126
      JMANT = MASK(128-24) .AND. SHIFTR(ITHA,23)
      IF (JMANT.EQ.MASK(128-24))  THEN
          JMANT = 0
          JEXP  = JEXP + 1
        ELSE
          JMANT = SHIFTR(JMANT+1,1)
        ENDIF
      IF (JEXP.LE.0)         GO TO 331
      IF (JEXP.GE.255)       GO TO 333
      ITHA  = JSIGN .OR. SHIFTL(JEXP,23) .OR. JMANT
      GO TO 347

C--      make de-normalized number
  331 IF (JEXP.LT.-23)       GO TO 341
      JMANT = SHIFTR (JMANT+40000000B,1-JEXP)
      ITHA  = JSIGN .OR. JMANT
      GO TO 347

C--      overflow
  333 JBAD = JL
      IF (JSIGN.EQ.0)    THEN
          ITHA = IOVPCS
        ELSE
          ITHA = IOVNCS
        ENDIF
      GO TO 347

  341 ITHA = 0
  347 MT(JMT+1) = ITHA
      JMT = JMT + 1
  349 JMS = JMS + 1

      RETURN
      END
#ifdef CERNLIB_TCGEN_IE3FOS
#undef CERNLIB_TCGEN_IE3FOS
#endif
