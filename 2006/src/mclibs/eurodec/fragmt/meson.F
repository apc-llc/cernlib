*
* $Id: meson.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: meson.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION MESON(IQ1,IQ2)
C.----------------------------------------------------------------------
C.
C.    DETERMINE THE MESON COMPOSED OF QUARK IQ1 AND ANTIQUARK IQ2.
C.    RETURNS PARTICLE CODE: MESON.
C.    LAST UPDATE: 14/02/89
C.
C.----------------------------------------------------------------------
#include "eurodec/frgfix.inc"
C--
C--   THE PROBABILITY FOR A PSEUDO-SCALAR IS GIVEN BY 'PROPSE'
      ISPIN=0
      IF (MAX(IQ1,-IQ2).LT.4) THEN
        IF (EURRAN(IPRDUM).GT.PROPSE(1)) ISPIN=1
      ELSE
        IF (EURRAN(IPRDUM).GT.PROPSE(2)) ISPIN=1
      ENDIF
      IF (ISPIN.EQ.1) THEN
C--
C--   VECTOR MESON...
        IF (IQ1.EQ.IQ2) THEN
          IF (IQ1.EQ.3) THEN
            MESON=331
          ELSE
            MESON=111
            IF (EURRAN(IRDUM).GT.0.5) MESON=221
          ENDIF
        ELSE
          IF (IQ1.GT.IQ2) THEN
            MESON=IQ1*100+IQ2*10+1
          ELSE
            MESON=-(IQ2*100+IQ1*10+1)
          ENDIF
        ENDIF
      ELSE
C--
C--   ... PSEUDO-SCALAR MESON, THE DEFINITION OF THE INDEX IS CONFORM
C--   THE SU(3) PROBABILITY FOR PICKING A PI, ETA AND ETA'
        IF (IQ1.EQ.IQ2) THEN
          RAT=EURRAN(IRDUM)
          IF (RAT.LE.0.5) THEN
            MESON=110
            IF (IQ1.EQ.3) MESON=220
          ELSEIF (RAT.LE.0.75) THEN
            MESON=220
            IF (IQ1.EQ.3) MESON=330
          ELSE
            MESON=330
          ENDIF
        ELSE
          IF (IQ1.GT.IQ2) THEN
            MESON=IQ1*100+IQ2*10
          ELSE
            MESON=-(IQ2*100+IQ1*10)
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
