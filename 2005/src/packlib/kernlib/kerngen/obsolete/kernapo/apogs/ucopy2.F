*
* $Id: ucopy2.F,v 1.1.1.1 1996/02/15 17:51:06 mclareni Exp $
*
* $Log: ucopy2.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:06  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE UCOPY2 (A,B,N)
C
C CERN PROGLIB# V301    UCOPY2          .VERSION KERNAPO  1.04  850321
C ORIG. 01/03/85  R.BRUN
C
      DIMENSION A(*),B(*)

      IF (N.LE.0)            RETURN
      IA=IADDR (A)
      IB=IADDR (B)
      IF (IA-IB)             11, 99, 21

C--     REVERSE LOOP
   11 CALL VEC_$COPY_I (A (N),-1,B (N),-1,N)
      RETURN

C--     USUAL LOOP
   21 CALL VEC_$COPY (A,B,N)
   99 RETURN
      END
#ifdef CERNLIB_TCGEN_UCOPY2
#undef CERNLIB_TCGEN_UCOPY2
#endif
