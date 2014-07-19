*
* $Id: ie3fos.F,v 1.1.1.1 1996/02/15 17:52:50 mclareni Exp $
*
* $Log: ie3fos.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:50  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE IE3FOS (MS,MT,NWDO,JBAD)
C
C CERN PROGLIB# M220    IE3FOS          .VERSION KERNHYW  1.06  870612
C ORIG. 11-june-87 JZ
 
C-    Convert single precision for output with copy
C-    from source in native to target in IEEE data format
 
      DIMENSION    MS(99), MT(99)
 
      EQUIVALENCE  (ITHA,THA)
 
      PARAMETER    (IBADCS = O'17740177000')
      PARAMETER    (IOVPCS = O'17740000000')
      PARAMETER    (IOVNCS = O'37740000000')
 
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
 
  301 DO 349  JL=1,NWDO
      ITHA = MS(JMS+1)
      IF  (THA.EQ.0.)        GO TO 341
      IF (ITHA.EQ.0)         GO TO 347
      IF (ITHA.EQ.IBADMS)    GO TO 337
      IF (ITHA.EQ.IOVPMS)    GO TO 338
      IF (ITHA.EQ.IOVNMS)    GO TO 338
      JSIGN = 0
      IF (THA.LT.0.)  THEN
          THA   = -THA
          JSIGN = 256
        ENDIF
      JEXP  = ISHFTR (ITHA, 28)
      IF (JEXP.GE.128)  JEXP= JEXP-256
      JEXPC = JEXP + 126
 
C--                Check mantissa normalized
 
      JMANT = ISHFTL (ITHA,9)
      IF (JMANT.EQ.0)                GO TO 341
  317 IF (JEXPC.LE.0)        GO TO 331
      IF (JMANT.LT.0)        GO TO 321
      JMANT = ISHFTL (JMANT,1)
      JEXPC = JEXPC - 1
      GO TO 317
 
C--           Zero the hidden bit,
C-            Keep the next 23 signf. bits, plus 1 for rounding
 
  321 JMANT = ISHFTR (ISHFTL(JMANT,1), 12)
      IF (JMANT.EQ.O'77777777')  THEN
          JMANT = 0
          JEXPC = JEXPC + 1
        ELSE
          JMANT = ISHFTR(JMANT+1,1)
        ENDIF
      ITHA  = IOR (ISHFTL(JSIGN+JEXPC,23), JMANT)
      GO TO 347
 
C--      make de-normalized number
  331 JMANT = ISHFTR (JMANT,13-JEXPC)
      IF (JMANT.EQ.0)        GO TO 341
      ITHA  = IOR (LSHFT(JSIGN,23), JMANT)
      GO TO 347
 
C--      Not-a-Number / Overflows
  337 JFAI = JL
      ITHA = IBADCS
      GO TO 347
  338 JFAI = JL
      ITHA = IOVPCS
      GO TO 347
  339 JFAI = JL
      ITHA = IOVNCS
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
*      ==================================================
#include "qcardl.inc"
