*
* $Id: ishft.F,v 1.1.1.1 1996/02/15 17:51:02 mclareni Exp $
*
* $Log: ishft.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:02  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION ISHFT (I,K)
C
C CERN PROGLIB# M441    ISHFT           .VERSION KERNAPO  1.10  870524
C ORIG. 01/10/85  T.LINDELOF
C
C      APOLLO VERSION OF ANSI/ISA FUNCTION ISHFT
C      K IS THE SHIFT COUNT - NEGATIVE TO THE RIGHT
C                             POSITIVE TO THE LEFT
C
      IF (K.GT.0) THEN
         ISHFT = LSHFT(I, K)
      ELSEIF (K.LT.0) THEN
         ISHFT = RSHFT(I, -K)
      ELSE
         ISHFT = I
      ENDIF
      END
#ifdef CERNLIB_TCGEN_ISHFT
#undef CERNLIB_TCGEN_ISHFT
#endif
