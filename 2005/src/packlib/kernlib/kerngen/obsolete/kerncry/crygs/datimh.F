*
* $Id: datimh.F,v 1.1.1.1 1996/02/15 17:52:35 mclareni Exp $
*
* $Log: datimh.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:35  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE DATIMH (AID,AIT)
C
C CERN PROGLIB# Z007    DATIMH          .VERSION KERNCRY  1.00  830612
C ORIG. 12/06/83 JZ
C
C
C-    CALL DATIMH (ND,NT)   RETURNS HOLLERITH DATE   ND = DD/MM/YY
C-                                  HOLLERITH TIME   NT = HH:MM:SS
C
      COMMON /SLATE/ SAFE(10), RM(4), L(3), M(23)
C
C
      RM(1) = DATE ()
      RM(2) = CLOCK ()
C
C-       -US       D D / M M / Y Y   H H . M M . S S
C-                 1 2 3 4 5 6 7 8   9 0 1 2 3 4 5 6
C-        US       M M / D D / Y Y   H H . M M . S S
C
#if defined(CERNLIB_USADATE)
      CALL UBLOW (RM,M,8)
      L(1) = M(4)
      L(2) = M(5)
      L(3) = M(6)
      M(4) = M(7)
      M(5) = M(8)
      CALL UBUNCH (L,RM,8)
#endif
      AID = RM(1)
      AIT = RM(2)
      RETURN
      END
#ifdef CERNLIB_CCGEN_DATIMH
#undef CERNLIB_CCGEN_DATIMH
#endif
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
