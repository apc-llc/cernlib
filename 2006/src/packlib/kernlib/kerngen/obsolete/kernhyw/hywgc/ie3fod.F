*
* $Id: ie3fod.F,v 1.1.1.1 1996/02/15 17:52:50 mclareni Exp $
*
* $Log: ie3fod.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:50  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE IE3FOD (MS,MT,NDPN,JBAD)
C
C CERN PROGLIB# M220    IE3FOD          .VERSION KERNHYW  1.06  870612
C ORIG. 12-june-87 JZ
 
C-    Convert double precision for output with copy
C-    from source in native to target in IEEE data format
 
      DIMENSION    MS(99), MT(99)
 
      DOUBLE PRECISION   THDB
      DIMENSION    THIS(2)
      EQUIVALENCE  (THDB,THIS)
      EQUIVALENCE  (ITHA,THA,THIS(1)), (ITHB,THB,THIS(2))
 
      PARAMETER    (IBADCD = O'17774017700')
      PARAMETER    (IOVPCD = O'17774000000')
      PARAMETER    (IOVNCD = O'37774000000')
 
      PARAMETER    (IBADMS = O'376777777776')
      PARAMETER    (IOVPMS = O'376777777777')
      PARAMETER    (IOVNMS = O'377000000000')
 
C-        IBAD : Not-a-number
C-        IOV :  overflow  P +ve / N -ve
C-               letter  M machine internal / C converted
C-               letter  S single / D double
 
#include "q_shift.inc"
 
      JFAI = 0
      JMS  = 0
      JMT  = 0
 
      DO 449  JL=1,NDPN
      ITHA = MS(JMS+1)
      IF  (THA.EQ.0.)        GO TO 441
      IF (ITHA.EQ.0)         GO TO 442
      IF (ITHA.EQ.IBADMS)    GO TO 437
      IF (ITHA.EQ.IOVPMS)    GO TO 438
      IF (ITHA.EQ.IOVNMS)    GO TO 438
      ITHB  = MS(JMS+2)
      JSIGN = 0
      IF (THIS.LT.0.)  THEN
          THIS  = -THIS
          JSIGN = 2048
        ENDIF
      JEXP  = ISHFTR (ITHA, 28)
      IF (JEXP.GE.128)  JEXP= JEXP-256
      JEXPC = JEXP + 1022
 
C--                Check mantissa normalized
 
      JMANT = ISHFTL (ITHA,9)
      IF (JMANT.EQ.0)        GO TO 441
      IF (JMANT.GT.0)        GO TO 437
 
C--           Zero the hidden bit,
C-            Keep the next 20 bits on the right in word 1,
C-            and the remaining 6 bits on the left in word 2,
C-            with the last 26 bits taken from ITHB
 
  421 JMANT = ISHFTL (JMANT,1)
      ITHB  = IOR (ISHFTR(ISHFTL(JMANT,20),4), ISHFTR(ITHB,10))
      ITHA  = IOR (ISHFTL(JSIGN+JEXPC,20), ISHFTR(JMANT,16))
      GO TO 447
 
C--      Not-a-Number / Overflows
  437 JFAI = JL
      ITHA = IBADCD
      GO TO 442
  438 JFAI = JL
      ITHA = IOVPCD
      GO TO 442
  439 JFAI = JL
      ITHA = IOVNCD
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
*      ==================================================
#include "qcardl.inc"
