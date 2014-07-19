*
* $Id: ie3fos.F,v 1.1.1.1 1996/02/15 17:54:47 mclareni Exp $
*
* $Log: ie3fos.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:47  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      SUBROUTINE IE3FOS (MS,MT,NWDO,JBAD)
C
C CERN PROGLIB# M220    IE3FOS          .VERSION KERNNOR  2.03  861204
 
C-    Convert single precision for output with copy
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
 
 
      JBAD = 0
      JMS  = 0
      JMT  = 0
 
  301 DO 349  JL=1,NWDO
      ITHA = MS(JMS+1)
      IF (ITHA.EQ.0)         GO TO 347
      JSIGN = ISHFT (ITHA,-31)
      JEXP  = ISHFT (ISHFT(ITHA,1), -23)
      JEXP  = JEXP - 130
      IF (JEXP.LE.0)         GO TO 331
      IF (JEXP.GE.255)       GO TO 333
      JMANT = ISHFT (ISHFT(ITHA,10), -9)
      ITHA  = ISHFT(JSIGN,31) .OR. ISHFT(JEXP,23) .OR. JMANT
      GO TO 347
 
C--      make de-normalized number
  331 IF (JEXP.LE.-23)       GO TO 341
      ITHA = ITHA .OR. 20000000 B
      ITHA = ISHFT (ISHFT(ITHA,9),JEXP-9)
      ITHA = ITHA .OR. ISHFT(JSIGN,31)
      GO TO 347
 
C--      overflow
  333 JBAD = JL
      IF (ITHA.NE.IBADMS)  THEN
          IF (JSIGN.EQ.0)    THEN
              ITHA = IOVPCS
            ELSE
              ITHA = IOVNCS
            ENDIF
        ELSE
          ITHA = IBADCS
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
