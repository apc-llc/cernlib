*
* $Id: datime.F,v 1.1.1.1 1996/02/15 17:50:56 mclareni Exp $
*
* $Log: datime.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:56  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE DATIME (ND,NT)
C
C CERN PROGLIB# Z007    DATIME          .VERSION KERNALT  1.07  890913
C ORIG. 15/02/88  JZ
C MOD.  11/09/89  add /SLATE/, JZ
C
C-    CALL DATIME (ND,NT)   RETURNS INTEGER DATE   ND = YYMMDD
C-                                  INTEGER TIME   NT =   HHMM
C-    returns also ISL  (1) y  (2) m  (3) d  (4) h  (5) m  (6) s
C
      COMMON /SLATE/ ISL(30), MM(10)
 
 
      CALL IDATE (MM)
      CALL ITIME (MM(4))
 
      DO 24  J=1,6
   24 ISL(J) = MM(J)
      ISL(1) = MM(3)
      ISL(3) = MM(1)
 
      MM(3) = MOD (MM(3),100)
 
      ND = (MM(3)*100+MM(2))*100 + MM(1)
      NT =            MM(4) *100 + MM(5)
      RETURN
      END
#ifdef CERNLIB_CCGEN_DATIME
#undef CERNLIB_CCGEN_DATIME
#endif
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
