*
* $Id: ie3fos32.F,v 1.1.1.1 1996/02/15 17:52:21 mclareni Exp $
*
* $Log: ie3fos32.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:21  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE IE3FOS (MS,MT,NWDO,JBAD)
C
C CERN PROGLIB# M220    IE3FOS          .VERSION KERNCVX  1.09  910815
C Orig. 06/08/91  JZ

C-    Convert REAL*4 to single precision for output with copy
C-    from source in native to target in IEEE data format

      DIMENSION    MS(99), MT(99)

      INTEGER*4    ITHA

      PARAMETER    (MSKEXP = '7F800000'X)
      PARAMETER    (MSKMAN = '007FFFFF'X)
      PARAMETER    (IBADCS = '7F80FE00'X)
      PARAMETER    (IOVPCS = '7F800000'X)
      PARAMETER    (IOVNCS = 'FF800000'X)


      JFAI = 0
      JMS  = 0
      JMT  = 0

      DO 349  JL=1,NWDO
      ITHA = MS(JMS+1)
      IF (ITHA.EQ.0)               GO TO 347
      JSIGN = ishft (ITHA, -31)
      JEXP  = ishft (ITHA.AND.MSKEXP, -23)
      IF (JEXP.EQ.0)               GO TO 335
      IF (JEXP.EQ.255)             GO TO 333
      JEXP  = JEXP - 2
      JMANT = MSKMAN .AND. ITHA
      IF (JEXP.LE.0)               GO TO 331
      ITHA  = ISHFT(JSIGN,31) .OR. ISHFT(JEXP,23) .OR. JMANT
      GO TO 347

C--      make de-normalized number
  331 JMANT = ishft (JMANT+'800000'X,JEXP-1)
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
