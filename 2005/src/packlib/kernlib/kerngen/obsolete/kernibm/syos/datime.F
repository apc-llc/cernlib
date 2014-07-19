*
* $Id: datime.F,v 1.1.1.1 1996/02/15 17:53:20 mclareni Exp $
*
* $Log: datime.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:20  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE DATIME (ND,NT)
C
C CERN PROGLIB# Z007    DATIME          .VERSION KERNIBM  2.28  891004
C MOD.  27/07/89  add /SLATE/, JZ
C
C
C-    CALL DATIME (ND,NT)   RETURNS INTEGER DATE   ND = YYMMDD
C-                                  INTEGER TIME   NT =   HHMM
C-    returns also ISL  (1) y  (2) m  (3) d  (4) h  (5) m  (6) s
C
      COMMON /SLATE/ ISL(10), MM(10), M(20)
C
C
      CALL DTZ007  (MM)
      CALL UBLOW (MM,M,16)
C
C-                 D D / M M / Y Y   H H . M M . S S
C-                 1 2 3 4 5 6 7 8   9 0 1 2 3 4 5 6
C
      MM(1) = IUFORW (M,7,8)
      MM(2) = IUFORW (M,4,5)
      MM(3) = IUFORW (M,1,2)
C
      MM(4) = IUFORW (M,9,10)
      MM(5) = IUFORW (M,12,13)
      MM(6) = IUFORW (M,15,16)
C
      ND = (MM(1)*100+MM(2))*100 + MM(3)
      NT =            MM(4) *100 + MM(5)
C
      MM(1) = MM(1) + 1900
      DO 24 J=1,6
   24 ISL(J) = MM(J)
      RETURN
      END
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
