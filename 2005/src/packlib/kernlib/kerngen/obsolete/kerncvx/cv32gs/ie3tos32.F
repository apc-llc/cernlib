*
* $Id: ie3tos32.F,v 1.1.1.1 1996/02/15 17:52:22 mclareni Exp $
*
* $Log: ie3tos32.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:22  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE IE3TOS (MS,MT,NWDO,JBAD)
C
C CERN PROGLIB# M220    IE3TOS          .VERSION KERNCVX  1.09  910815
C Orig. 31/05/89  JZ

C-    Convert single precision for input with copy
C-    from source in IEEE to target in native REAL*4 format

      DIMENSION    MS(99), MT(99)

      REAL*4       VALUE
      INTEGER*4    ITHA
      EQUIVALENCE (VALUE,ITHA)

      PARAMETER    (MSK23R = '007FFFFF'X)
      PARAMETER    (MSKEXP = '7F800000'X)
      PARAMETER    (IOVPMS = '7FBFFFFF'X)
      PARAMETER    (IOVNMS = 'FFBFFFFF'X)
      PARAMETER    (IBADMS = '80007FFF'X)


      JFAI = 0
      JMS  = 0
      JMT  = 0

      DO 349  JL=1,NWDO
      ITHA = MS(JMS+1)
      IF (ITHA.EQ.0)               GO TO 347
      JSIGN = ishft (ITHA, -31)
      JEXP  = ishft (ITHA.AND.MSKEXP, -23)
      JMANT = ITHA .AND. MSK23R
      IF (JEXP.EQ.0)               GO TO 331
      IF (JEXP.EQ.255)             GO TO 333
      IF (JEXP.EQ.254)             GO TO 334
      JEXP  = JEXP + 2
      ITHA  = ISHFT (JSIGN,31) .OR. ISHFT (JEXP,23) .OR. JMANT
      GO TO 347

C--       de-normalized number
  331 IF (JMANT.EQ.0)              GO TO 341
      VALUE  = FLOAT(JMANT) * 2.**(-63)
      VALUE  = VALUE * 2.**(-86)
      IF (ITHA .EQ.0)              GO TO 347
      IF (JSIGN.EQ.0)              GO TO 347
      VALUE  = -VALUE
      GO TO 347

C--       overflow / NaN
  333 IF (JMANT.NE.0)              GO TO 336
  334 IF (JSIGN.EQ.0)  THEN
          ITHA = IOVPMS
        ELSE
          ITHA = IOVNMS
        ENDIF
      GO TO 337

  336 ITHA = IBADMS
  337 JFAI = JL
      GO TO 347

  341 ITHA = 0
  347 MT(JMT+1) = ITHA
      JMT = JMT + 1
  349 JMS = JMS + 1

      JBAD = JFAI
      RETURN
      END
#ifdef CERNLIB_TCGEN_IE3TOS
#undef CERNLIB_TCGEN_IE3TOS
#endif
