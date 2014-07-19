*
* $Id: datime.F,v 1.1.1.1 1996/02/15 17:51:08 mclareni Exp $
*
* $Log: datime.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:08  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
#if defined(CERNLIB_QF_APO)
      SUBROUTINE DATIME (ND,NT)
C
C CERN PROGLIB# Z007    DATIME          .VERSION KERNAPO  1.18  890929
C ORIG. 13/12/83  TL + JZ, CERN
C MOD.  27/07/89  add /SLATE/, JZ
C
C-    CALL DATIME (ND,NT)   RETURNS INTEGER DATE   ND = YYMMDD
C-                                  INTEGER TIME   NT =   HHMM
C-    returns also ISL  (1) y  (2) m  (3) d  (4) h  (5) m  (6) s
C
      COMMON /SLATE/ ISL(40)
      INTEGER*2    MM(6)
      EQUIVALENCE (MM(1),ISL(31))


      CALL CAL_$DECODE_LOCAL_TIME (MM)

      DO 24  J=1,6
   24 ISL(J) = MM(J)

      MM(1) = MOD (MM(1),100)

      ND = (INT4(MM(1))*100+INT4(MM(2)))*100 + INT4(MM(3))
      NT =                  INT4(MM(4)) *100 + INT4(MM(5))
      RETURN
      END
#ifdef CERNLIB_CCGEN_DATIME
#undef CERNLIB_CCGEN_DATIME
#endif
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
#endif
