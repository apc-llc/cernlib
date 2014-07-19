*
* $Id: datime.F,v 1.1.1.1 1996/02/15 17:52:23 mclareni Exp $
*
* $Log: datime.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:23  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE DATIME (ND,NT)
C
C CERN PROGLIB# Z007    DATIME          .VERSION KERNCVX  1.09  910815
C ORIG. 15/02/88  JZ
C MOD.  27/07/89  add /SLATE/, JZ
C
C-    CALL DATIME (ND,NT)   RETURNS INTEGER DATE   ND = YYMMDD
C-                                  INTEGER TIME   NT =   HHMM
C
      COMMON /SLATE/ ISLATE(40)
      DIMENSION      MM(6)


      CALL IDATE (MM(2),MM(3),MM(1))
      CALL ITIME (MM(4))

      DO 24  J=1,6
   24 ISLATE(J) = MM(J)

      MM(1) = MOD (MM(1),100)

      ND = (MM(1)*100+MM(2))*100 + MM(3)
      NT =            MM(4) *100 + MM(5)
      RETURN
      END
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
#ifdef CERNLIB_CCGEN_DATIME
#undef CERNLIB_CCGEN_DATIME
#endif
