*
* $Id: datime.F,v 1.1.1.1 1996/02/15 17:54:44 mclareni Exp $
*
* $Log: datime.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:44  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE DATIME (ND,NT)
C
C CERN PROGLIB# Z007    DATIME          .VERSION KERNIBX  1.00  900307
C ORIG. 13/10/89  JZ
C
C-    CALL DATIME (ND,NT)   RETURNS INTEGER DATE   ND = YYMMDD
C-                                  INTEGER TIME   NT =   HHMM
C
      COMMON /SLATE/ MM(10), INF(30)


      CALL DATIM (INF)

      DO 24  J=1,6
   24 MM(J) = INF(9-J)

      ND = (MOD(MM(1),100)*100+MM(2))*100 + MM(3)
      NT =                     MM(4) *100 + MM(5)
      RETURN
      END
#ifdef CERNLIB_CCGEN_DATIME
#undef CERNLIB_CCGEN_DATIME
#endif
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
