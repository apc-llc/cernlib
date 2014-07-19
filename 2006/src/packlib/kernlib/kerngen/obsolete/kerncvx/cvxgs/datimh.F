*
* $Id: datimh.F,v 1.1.1.1 1996/02/15 17:52:24 mclareni Exp $
*
* $Log: datimh.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:24  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE DATIMH (ND,NT)
C
C CERN PROGLIB# Z007    DATIMH          .VERSION KERNCVX  1.09  910815
C ORIG. 15/06/89  JZ
C

C-    CALL DATIMH (ND,NT)   RETURNS HOLLERITH DATE   ND = DD/MM/YY
C-                                  HOLLERITH TIME   NT = HH.MM.SS

      COMMON /SLATE/ ISL(10),M(20),MM(10)

      CHARACTER    ND*8, NT*8, CHV*16


C-                 D D / M M / Y Y   H H . M M . S S
C-                 1 2 3 4 5 6 7 8   9 0 1 2 3 4 5 6

C-                   m     d     y
      CALL IDATE (MM(2),MM(3),MM(1))
      CALL ITIME (MM(4))

      DO 24  J=1,6
   24 M(J) = MM(J)

      M(1) = MOD (M(1),100)

      CHV ='0 /0 /000 .0 .00'

      CALL CSETDI (M(1), CHV, 7, 8)
      CALL CSETDI (M(2), CHV, 4, 5)
      CALL CSETDI (M(3), CHV, 1, 2)
      CALL CSETDI (M(4), CHV, 9,10)
      CALL CSETDI (M(5), CHV,12,13)
      CALL CSETDI (M(6), CHV,15,16)

      ND(1:8) = CHV(1:8)
      NT(1:8) = CHV(9:16)
      RETURN
      END
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
#ifdef CERNLIB_CCGEN_DATIMH
#undef CERNLIB_CCGEN_DATIMH
#endif
