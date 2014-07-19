*
* $Id: ie3tod32.F,v 1.1.1.1 1996/02/15 17:52:22 mclareni Exp $
*
* $Log: ie3tod32.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:22  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE IE3TOD (MS,MT,NDPN,JBAD)
C
C CERN PROGLIB# M220    IE3TOD          .VERSION KERNCVX  1.09  910815
C Orig. 31/05/89  JZ

C-    Convert double precision from input with copy
C-    from source in IEEE to target in native REAL*8 format

      DIMENSION    MS(99), MT(99)

      INTEGER*8    ITHL,    IOVPMD, IOVNMD, IBADMD
      INTEGER*4    ITHV(2), ITHA, ITHB
      EQUIVALENCE (ITHL,ITHV)
      EQUIVALENCE (ITHA,ITHV(1)), (ITHB,ITHV(2))

      PARAMETER    (MSK20R = '000FFFFF'X)
      PARAMETER    (MSKEXP = '7FF00000'X)
      PARAMETER    (IOVPMD = '7FF7FFFFFFFFFFFF'X)
      PARAMETER    (IOVNMD = 'FFF7FFFFFFFFFFFF'X)
      PARAMETER    (IBADMD = '80007FFFFFFFFFFF'X)


      JFAI = 0
      JMS  = 0
      JMT  = 0

      DO 449  JL=1,NDPN
      ITHA  = MS(JMS+1)
      IF (ITHA.EQ.0)               GO TO 441
      ITHB  = MS(JMS+2)
      JSIGN = ishft (ITHA, -31)
      JEXP  = ishft (ITHA.AND.MSKEXP, -20)
      JMANT = ITHA .AND. MSK20R
      IF (JEXP.EQ.0)               GO TO 441
      IF (JEXP.EQ.2047)            GO TO 433
      IF (JEXP.EQ.2046)            GO TO 434

      JEXP  = JEXP + 2
      ITHA  = ISHFT (JSIGN,31) .OR.
     +        ISHFT (JEXP,20)  .OR.  JMANT
      GO TO 447

C--       overflow
  433 IF (JMANT.NE.0)              GO TO 435
  434 IF (JSIGN.EQ.0)  THEN
          ITHL = IOVPMD
        ELSE
          ITHL = IOVNMD
        ENDIF
      GO TO 437

C--       NaN
  435 ITHL = IBADMD
  437 JFAI = JL
      GO TO 447

  441 ITHL = 0
  447 MT(JMT+1) = ITHA
      MT(JMT+2) = ITHB
      JMT = JMT + 2
  449 JMS = JMS + 2

      JBAD = JFAI
      RETURN
      END
#ifdef CERNLIB_TCGEN_IE3TOD
#undef CERNLIB_TCGEN_IE3TOD
#endif
