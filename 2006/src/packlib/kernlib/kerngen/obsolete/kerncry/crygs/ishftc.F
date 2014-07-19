*
* $Id: ishftc.F,v 1.1.1.1 1996/02/15 17:52:36 mclareni Exp $
*
* $Log: ishftc.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:36  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION ISHFTC(I,K,L)
C
C CERN PROGLIB# M441    ISHFTC          .VERSION KERNCRY  1.06  870527
C ORIG. 17/11/86  CNAF, BOLOGNA
C
C      CRAY VERSION OF MIL-STD-1753 ISHFTC FUNCTION
C      K IS THE SHIFT COUNT - NEGATIVE TO THE RIGHT
C                             POSITIVE TO THE LEFT
C      L IS THE NUMBER OF THE RIGHTMOST BITS TO BE SHIFTED, ALL
C         OTHER BITS ARE LEFT UNCHANGED
C
      IF (K.GT.0) THEN
         IF (L.EQ.64) THEN
            ISHFTC = SHIFT(I,K)
         ELSE
            ISHFTC=OR(SHIFTR(AND(MASK(128-L),I),L-K),
     +                AND(MASK(128-L),SHIFTL(AND(MASK(128-L),I),K)))
            ISHFTC=OR(ISHFTC,AND(MASK(64-L),I))
         ENDIF
      ELSEIF (K.LT.0) THEN
            ISHFTC=OR(AND(MASK(128-L),SHIFTL(AND(MASK(128-L),I),L+K)),
     +                SHIFTR(AND(MASK(128-L),I),-K))
            ISHFTC=OR(ISHFTC,AND(MASK(64-L),I))
      ELSE
         ISHFTC = I
      ENDIF
      END
#ifdef CERNLIB_TCGEN_ISHFTC
#undef CERNLIB_TCGEN_ISHFTC
#endif
