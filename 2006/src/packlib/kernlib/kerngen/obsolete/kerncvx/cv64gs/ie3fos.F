*
* $Id: ie3fos.F,v 1.1.1.1 1996/02/15 17:52:15 mclareni Exp $
*
* $Log: ie3fos.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:15  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE IE3FOS (MS,MT,NWDO,JBAD)
C
C CERN PROGLIB# M220    IE3FOS          .VERSION KERNCVX  1.09  910815
C Orig. 31/05/89  JZ

C-    Convert REAL*8 to single precision for output with copy
C-    from source in native to target in IEEE data format

      DIMENSION    MS(99), MT(99)

      INTEGER*8    ITHA

      PARAMETER    (MSK32R = '00000000FFFFFFFF'X)
      PARAMETER    (MSK24R = '0000000000FFFFFF'X)
      PARAMETER    (MSKEXP = '7FF0000000000000'X)
      PARAMETER    (IBADCS = '000000007F80FE00'X)
      PARAMETER    (IOVPCS = '000000007F800000'X)
      PARAMETER    (IOVNCS = '00000000FF800000'X)


      JFAI = 0
      JMS  = 0
      JMT  = 0

      DO 349  JL=1,NWDO
      ITHA = MS(JMS+1)
      IF (ITHA.EQ.0)               GO TO 347
      JSIGN = ishft (ITHA, -63)
      JEXP  = ishft (ITHA.AND.MSKEXP, -52)
      IF (JEXP.EQ.0)               GO TO 335
      JEXP  = JEXP - 1025 + 127
      JMANT = MSK24R .AND. ishft(ITHA,-28)
      IF (JMANT.EQ.MSK24R)  THEN
          JMANT = 0
          JEXP  = JEXP + 1
        ELSE
          JMANT = ishft(JMANT+1,-1)
        ENDIF
      IF (JEXP.LE.0)               GO TO 331
      IF (JEXP.GE.255)             GO TO 333
      ITHA  = ISHFT(JSIGN,31) .OR. ISHFT(JEXP,23) .OR. JMANT
      GO TO 347

C--      make de-normalized number
  331 IF (JEXP.LT.-23)             GO TO 341
      JMANT = ishft (JMANT+'800000'X,JEXP-1)
      ITHA  = ISHFT(JSIGN,31) .OR. JMANT
      GO TO 347

C--       overflow
  333 IF (JSIGN.EQ.0)  THEN
          ITHA = IOVPCS
        ELSE
          ITHA = IOVNCS
        ENDIF
      GO TO 337

C--       NaN
  335 IF (JSIGN.EQ.0)              GO TO 341
      ITHA = IBADCS
  337 JFAI = JL
      GO TO 347

  341 ITHA = 0
  347 MT(JMT+1) = ITHA
      JMT = JMT + 1
  349 JMS = JMS + 1

      JBAD = JFAI
      RETURN
      END
#ifdef CERNLIB_TCGEN_IE3FOS
#undef CERNLIB_TCGEN_IE3FOS
#endif
