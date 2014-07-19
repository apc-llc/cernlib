*
* $Id: datime.F,v 1.1.1.1 1996/02/15 17:52:35 mclareni Exp $
*
* $Log: datime.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:35  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE DATIME (ND,NT)
C
C CERN PROGLIB# Z007    DATIME          .VERSION KERNCRY  1.12  890913
C ORIG. 12/06/83 JZ
C MOD.  27/07/89  add /SLATE/, JZ
C
      COMMON /SLATE/ ISL(10), MM(6), RM(4), M(20)
C
C
      RM(1) = DATE ()
      RM(2) = CLOCK ()
      CALL UBLOW (RM,M,16)
C
C-       -US       D D / M M / Y Y   H H . M M . S S
C-                 1 2 3 4 5 6 7 8   9 0 1 2 3 4 5 6
C-        US       M M / D D / Y Y   H H . M M . S S
C
#if !defined(CERNLIB_USADATE)
      MM(1) = IUFORW (M,7,8)
      MM(2) = IUFORW (M,4,5)
      MM(3) = IUFORW (M,1,2)
#endif
#if defined(CERNLIB_USADATE)
      MM(1) = IUFORW (M,7,8)
      MM(2) = IUFORW (M,1,2)
      MM(3) = IUFORW (M,4,5)
#endif
      MM(4) = IUFORW (M,9,10)
      MM(5) = IUFORW (M,12,13)
      MM(6) = IUFORW (M,15,16)

      ND = (MM(1)*100+MM(2))*100 + MM(3)
      NT =            MM(4) *100 + MM(5)

      MM(1) = MM(1) + 1900
      DO 24  J=1,6
   24 ISL(J) = MM(J)
      RETURN
      END
#ifdef CERNLIB_CCGEN_DATIME
#undef CERNLIB_CCGEN_DATIME
#endif
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
