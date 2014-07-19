*
* $Id: ishft.F,v 1.1.1.1 1996/02/15 17:52:36 mclareni Exp $
*
* $Log: ishft.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:36  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION ISHFT(I,K)
C
C CERN PROGLIB# M441    ISHFT           .VERSION KERNCRY  1.06  870527
C ORIG. 17/11/86  CNAF, BOLOGNA
C
C      CRAY VERSION OF ANSI/ISA FUNCTION ISHFT
C      K IS THE SHIFT COUNT - NEGATIVE TO THE RIGHT
C                             POSITIVE TO THE LEFT
C
      IF (K.GT.0) THEN
         ISHFT = SHIFTL(I,K)
      ELSEIF (K.LT.0) THEN
         ISHFT = SHIFTR(I,-K)
      ELSE
         ISHFT = I
      ENDIF
      END
#ifdef CERNLIB_TCGEN_ISHFT
#undef CERNLIB_TCGEN_ISHFT
#endif
