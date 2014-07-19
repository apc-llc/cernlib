*
* $Id: ie3fod.F,v 1.1.1.1 1996/02/15 17:54:47 mclareni Exp $
*
* $Log: ie3fod.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:47  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      SUBROUTINE IE3FOD (MS,MT,NDPN,JBAD)
C
C CERN PROGLIB# M220    IE3FOD          .VERSION KERNNOR  2.03  861204
 
C-    Convert double precision for output with copy
C-    from source in native to target in IEEE data format
 
      DIMENSION    MS(99), MT(99)
 
      DOUBLE PRECISION   THDB
      DIMENSION    THIS(2)
      EQUIVALENCE  (THDB,THIS)
      EQUIVALENCE  (ITHA,THA,THIS(1)), (ITHB,THB,THIS(2))
 
      PARAMETER    (IBADMS = 17777777777 B)
      PARAMETER    (IBADCS = 17740177000 B)
      PARAMETER    (IBADCD = 17774017700 B)
      PARAMETER    (IOVPCS = 17740000000 B)
      PARAMETER    (IOVPCD = 17774000000 B)
      PARAMETER    (IOVNCS = 37740000000 B)
      PARAMETER    (IOVNCD = 37774000000 B)
 
 
      JFAI = 0
      JMS  = 0
      JMT  = 0
 
      DO 449  JL=1,NDPN
      ITHA = MS(JMS+1)
      IF (ITHA.EQ.0)         GO TO 442
      ITHB  = MS(JMS+2)
      JSIGN = ISHFT (ITHA,-31)
      JEXP  = ISHFT (ISHFT(ITHA,1), -23)
      IF (JEXP.EQ.0)         GO TO 441
      IF (JEXP.EQ.511)       GO TO 433
      JMANT = ISHFT (ISHFT(ITHA,10), -12)
      ITHB  = ISHFT (ITHA,30) .OR. ISHFT (ITHB,-2)
      ITHA  = ISHFT(JSIGN,31) .OR. ISHFT(JEXP+766,20) .OR. JMANT
      GO TO 447
 
C--      overflow / NaN
  433 JFAI = JL
      IF (ITHA.NE.IBADMS)  THEN
          IF (JSIGN.EQ.0)    THEN
              ITHA = IOVPCD
            ELSE
              ITHA = IOVNCD
            ENDIF
        ELSE
          ITHA = IBADCD
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
