*
* $Id: lvmaxa.F,v 1.1.1.1 1996/02/15 17:51:03 mclareni Exp $
*
* $Log: lvmaxa.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:03  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION LVMAXA (A,N)
C
C CERN PROGLIB# F121    LVMAXA          .VERSION KERNAPO  1.17  890626
C ORIG. 29/05/89  F.Carminati
C
      DIMENSION A(*)

      IF (N.GT.0)  THEN
          CALL VEC_$MAX (A,N,HMAX,IMAX)
          LVMAXA = IMAX
          RETURN
        ENDIF
      LVMAXA = 0
      RETURN
      END
#ifdef CERNLIB_TCGEN_LVMAXA
#undef CERNLIB_TCGEN_LVMAXA
#endif
