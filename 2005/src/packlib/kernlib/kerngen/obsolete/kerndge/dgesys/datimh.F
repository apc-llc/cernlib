*
* $Id: datimh.F,v 1.1.1.1 1996/02/15 17:54:27 mclareni Exp $
*
* $Log: datimh.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:27  mclareni
* Kernlib
*
*
#include "kerndge/pilot.h"
      SUBROUTINE DATIMH (ND,NT)
C
C CERN PROGLIB# Z007    DATIMH          .VERSION KERNDGE  1.01  860217
C ORIG. 03/02/86  JZ
C
C
C-    CALL DATIMH (ND,NT)   RETURNS HOLLERITH DATE   ND = DD/MM/YY
C-                                  HOLLERITH TIME   NT = HH.MM.SS
C
      COMMON /SLATE/ ISAFE(10), M(24), MM(6)
C
      DIMENSION    ND(2), NT(2)
 
 
      CALL UBLOW (16H0 /0 /000 .0 .00, M, 16)
C
C-                 D D / M M / Y Y   H H . M M . S S
C-                 1 2 3 4 5 6 7 8   9 0 1 2 3 4 5 6
C
      CALL DATE (MM)
      CALL TIME (MM(4))
 
      MM(1) = MOD (MM(1),100)
 
      CALL USET (MM(1), M, 7, 8)
      CALL USET (MM(2), M, 4, 5)
      CALL USET (MM(3), M, 1, 2)
      CALL USET (MM(4), M, 9,10)
      CALL USET (MM(5), M,12,13)
      CALL USET (MM(6), M,15,16)
 
      CALL UBUNCH (M(1),ND,8)
      CALL UBUNCH (M(9),NT,8)
      RETURN
      END
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
