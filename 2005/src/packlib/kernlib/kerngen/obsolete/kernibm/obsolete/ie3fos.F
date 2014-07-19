*
* $Id: ie3fos.F,v 1.1.1.1 1996/02/15 17:53:24 mclareni Exp $
*
* $Log: ie3fos.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:24  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE IE3FOS (MS,MT,NWDO,JBAD)
C
C CERN PROGLIB# M220    IE3FOS          .VERSION KERNIBM  2.20  870601

C-    Convert single precision for output with copy
C-    from source in native to target in IEEE data format

      DIMENSION    MS(99), MT(99)

      DOUBLE PRECISION   THDB
      DIMENSION    THIS(2)
      EQUIVALENCE  (THDB,THIS)
      EQUIVALENCE  (ITHA,THA,THIS(1)), (ITHB,THB,THIS(2))

C     PARAMETER    (IBADMS =   Z 7FFFFFFF)
      PARAMETER    (IBADMS = 2147 483 647)
C     PARAMETER    (MSKA24 =   Z 00FFFFFF)
      PARAMETER    (MSKA24 =   16 777 215)
C     PARAMETER    (MSKB24 =   Z 00800000)
      PARAMETER    (MSKB24 =    8 388 608)
C     PARAMETER    (MN2131 =   Z 800FFFFF)
      PARAMETER    (MN2131 =-2146 435 073)
C     PARAMETER    (IBADCS =   Z 7F80FE00)
      PARAMETER    (IBADCS = 2139 160 064)
C     PARAMETER    (IBADCD =   Z 7FF01FC0)
      PARAMETER    (IBADCD = 2146 443 200)
C     PARAMETER    (IOVPCS =   Z 7F800000)
      PARAMETER    (IOVPCS = 2139 095 040)
C     PARAMETER    (IOVPCD =   Z 7FF00000)
      PARAMETER    (IOVPCD = 2146 435 072)
C     PARAMETER    (IOVNCS =   Z FF800000)
      PARAMETER    (IOVNCS =   -8 388 608)
C     PARAMETER    (IOVNCD =   Z FFF00000)
      PARAMETER    (IOVNCD =   -1 048 576)

#include "q_andor.inc"
#include "q_shift.inc"

      JBAD = 0
      JMS  = 0
      JMT  = 0

  301 DO 349  JL=1,NWDO
      ITHA = MS(JMS+1)
      IF (ITHA.EQ.0)         GO TO 347
      JSIGN = ISHFTR (ITHA,31)
      JEXP  = ISHFTR (ISHFTL(ITHA,1), 25)
      JMANT = IAND (ITHA,MSKA24)
      IF (JMANT.EQ.0)        GO TO 341
      JEXP = 4*(JEXP-64)
  322 IF (IAND(JMANT,MSKB24).NE.0)    GO TO 324
      JMANT = ISHFTL(JMANT,1)
      JEXP  = JEXP - 1
      GO TO 322

  324 JEXP = JEXP + 126
      IF (JEXP.LE.0)         GO TO 331
      IF (JEXP.GE.255)       GO TO 333
      ITHA  = IAND (NOT(MSKB24),JMANT)
      ITHA  = IOR  ( IOR(ITHA,ISHFTL(JEXP,23)), ISHFTL(JSIGN,31))
      GO TO 347

C--      make de-normalized number
  331 IF (JEXP.LE.-23)       GO TO 341
      ITHA = ISHFTR (JMANT,1-JEXP)
      ITHA = IOR (ITHA, ISHFTL(JSIGN,31))
      GO TO 347

C--      overflow / NaN
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
