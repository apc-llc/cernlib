*
* $Id: datimh.F,v 1.1.1.1 1996/02/15 17:50:56 mclareni Exp $
*
* $Log: datimh.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:56  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE DATIMH (ND,NT)
C
C CERN PROGLIB# Z007    DATIMH          .VERSION KERNALT  1.00  880212
C ORIG. 15/02/88  JZ
C
 
C-    CALL DATIMH (ND,NT)   RETURNS HOLLERITH DATE   ND = DD/MM/YY
C-                                  HOLLERITH TIME   NT = HH.MM.SS
 
      COMMON /SLATE/ ISAFE(10), M(24), MM(6)
 
      DIMENSION    ND(2), NT(2)
 
 
      CALL UBLOW (16H0 /0 /000 .0 .00, M, 16)
 
C-                 D D / M M / Y Y   H H . M M . S S
C-                 1 2 3 4 5 6 7 8   9 0 1 2 3 4 5 6
 
      CALL IDATE (MM)
      CALL ITIME (MM(4))
 
      MM(3) = MOD (MM(3),100)
 
      CALL USET (MM(3), M, 7, 8)
      CALL USET (MM(2), M, 4, 5)
      CALL USET (MM(1), M, 1, 2)
      CALL USET (MM(4), M, 9,10)
      CALL USET (MM(5), M,12,13)
      CALL USET (MM(6), M,15,16)
 
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
