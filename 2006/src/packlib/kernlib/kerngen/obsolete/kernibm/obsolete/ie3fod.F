*
* $Id: ie3fod.F,v 1.1.1.1 1996/02/15 17:53:24 mclareni Exp $
*
* $Log: ie3fod.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:24  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE IE3FOD (MS,MT,NDPN,JBAD)
C
C CERN PROGLIB# M220    IE3FOD          .VERSION KERNIBM  2.20  870601

C-    Convert double precision for output with copy
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

      JFAI = 0
      JMS  = 0
      JMT  = 0

      DO 449  JL=1,NDPN
      ITHA = MS(JMS+1)
      IF (ITHA.EQ.0)         GO TO 442
      ITHB  = MS(JMS+2)
      JEXP  = ISHFTR (ISHFTL(ITHA,1), 25)
      IF (JEXP.EQ.127)       GO TO 433
      JSHF  = 0
      JMOST = ISHFTR (ISHFTL(ITHA,8), 28)
      IF (JMOST.EQ.0)        GO TO 441
  422 IF (JMOST.EQ.1)        GO TO 424
      THDB  = THDB / 2.
      JMOST = ISHFTR (JMOST,1)
      JSHF  = JSHF + 1
      GO TO 422

  424 JEXP  = 4*(JEXP-65) + JSHF + 1023
      ITHA  = IOR ( IAND(ITHA,MN2131), ISHFTL(JEXP,20))
      GO TO 447

C--      overflow / NaN
  433 JFAI = JL
      IF (ITHA.NE.IBADMS)  THEN
          JSIGN = ISHFTR (ITHA,31)
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
