*
* $Id: ie3tos.F,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: ie3tos.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      SUBROUTINE IE3TOS (MS,MT,NWDO,JBAD)
C
C CERN PROGLIB# M220    IE3TOS          .VERSION KERNNOR  2.03  861204
 
C-    Convert single precision for input with copy
C-    from source in IEEE to target in native data format
 
      DIMENSION    MS(99), MT(99)
 
      DOUBLE PRECISION   THDB
      DIMENSION    THIS(2)
      EQUIVALENCE  (THDB,THIS)
      EQUIVALENCE  (ITHA,THA,THIS(1)), (ITHB,THB,THIS(2))
 
      PARAMETER    (IBADMS = 17777777777 B)
      PARAMETER    (JEXMIN = -255,  JEXMAX=253)
      PARAMETER    (IOVPMS = 17777777770 B)
      PARAMETER    (IOVNMS = 37777777770 B)
      PARAMETER    (IBADMD = IBADMS, IBADME = 0)
      PARAMETER    (IOVPMD = IOVPMS, IOVNMD = IOVNMS)
 
#include "kernnor/q_jbit.inc"
* Ignoring t=pass
 
      JBAD = 0
      JMS  = 0
      JMT  = 0
 
  301 DO 349  JL=1,NWDO
      ITHA = MS(JMS+1)
      IF (ITHA.EQ.0)         GO TO 347
      JSIGN = JBIT (ITHA,32)
      JEXP  = JBYT (ITHA,24,8)
      JMANT = JBYT (ITHA,1,23)
      IF (JEXP.EQ.0)         GO TO 331
      IF (JEXP.EQ.255)       GO TO 333
      JMANT = JMANT + 8 388 608
      JEXP  = JEXP - 127
      JPRE  = ISIGN (23,JEXP)
      THA   = FLOAT(JMANT) * 2.**(JPRE-23)
      THA   = THA * 2.**(JEXP-JPRE)
      IF (JSIGN.EQ.0)        GO TO 347
      THA  = -THA
      GO TO 347
 
  331 IF (JMANT.EQ.0)        GO TO 341
      THA  = FLOAT(JMANT) * 2.**(-63)
      THA  = THA * 2.**(-86)
      IF (ITHA .EQ.0)        GO TO 347
      IF (JSIGN.EQ.0)        GO TO 347
      THA  = -THA
      GO TO 347
 
  333 IF (JMANT.EQ.0)    THEN
          IF (JSIGN.EQ.0)  THEN
              ITHB = IOVPMS
            ELSE
              ITHB = IOVNMS
            ENDIF
        ELSE
          ITHB = IBADMS
        ENDIF
      JBAD = JL
      ITHA = ITHB
      GO TO 347
 
  341 ITHA = 0
  347 MT(JMT+1) = ITHA
      JMT = JMT + 1
  349 JMS = JMS + 1
 
      RETURN
      END
#ifdef CERNLIB_TCGEN_IE3TOS
#undef CERNLIB_TCGEN_IE3TOS
#endif
