*
* $Id: datimh.F,v 1.1.1.1 1996/02/15 17:51:09 mclareni Exp $
*
* $Log: datimh.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:09  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
#if defined(CERNLIB_QF_APO)
      SUBROUTINE DATIMH (ND,NT)
C
C CERN PROGLIB# Z007    DATIMH          .VERSION KERNAPO  1.00  831212
C ORIG. 13/12/83  TL + JZ, CERN
C
C
C-    CALL DATIMH (ND,NT)   RETURNS HOLLERITH DATE   ND = DD/MM/YY
C-                                  HOLLERITH TIME   NT = HH.MM.SS
C
      COMMON /SLATE/ ISAFE(10), M(27), MM
C
      DIMENSION    ND(2), NT(2)
      INTEGER*2    MM(6)


      CALL UBLOW (16H0 /0 /000 .0 .00, M, 16)
C
C-                 D D / M M / Y Y   H H . M M . S S
C-                 1 2 3 4 5 6 7 8   9 0 1 2 3 4 5 6
C
      CALL CAL_$DECODE_LOCAL_TIME (MM)

      MM(1) = MOD (MM(1),100)

      CALL USET (INT4(MM(1)), M, 7, 8)
      CALL USET (INT4(MM(2)), M, 4, 5)
      CALL USET (INT4(MM(3)), M, 1, 2)
      CALL USET (INT4(MM(4)), M, 9,10)
      CALL USET (INT4(MM(5)), M,12,13)
      CALL USET (INT4(MM(6)), M,15,16)

      CALL UBUNCH (M(1),ND,8)
      CALL UBUNCH (M(9),NT,8)
      RETURN
      END
#ifdef CERNLIB_CCGEN_DATIMH
#undef CERNLIB_CCGEN_DATIMH
#endif
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
#endif
