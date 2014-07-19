*
* $Id: datimh.F,v 1.1.1.1 1996/02/15 17:54:44 mclareni Exp $
*
* $Log: datimh.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:44  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE DATIMH(ND,NT)
C
C CERN PROGLIB# Z007    DATIMH          .VERSION KERNIBX  1.00  900123
C
C  use VS FORTRAN Version 2 Library routine DATIM
C
      DIMENSION ND(2),NT(2)
      DIMENSION I(8)
      DIMENSION L(4)
      EQUIVALENCE(L(1),STRG)
      CHARACTER*16 STRG

      CALL DATIM(I)
      I(8) = MOD(I(8),100)
      WRITE(STRG,1) I(6),I(7),I(8),I(5),I(4),I(3)
1     FORMAT(I2.2,'/',I2.2,'/',I2.2,I2.2,'.',I2.2,'.',I2.2)
      ND(1) = L(1)
      ND(2) = L(2)
      NT(1) = L(3)
      NT(2) = L(4)
      RETURN
      END
#ifdef CERNLIB_CCGEN_DATIMH
#undef CERNLIB_CCGEN_DATIMH
#endif
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
