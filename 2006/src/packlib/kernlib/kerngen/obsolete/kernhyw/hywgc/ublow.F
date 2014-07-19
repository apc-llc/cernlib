*
* $Id: ublow.F,v 1.1.1.1 1996/02/15 17:52:53 mclareni Exp $
*
* $Log: ublow.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:53  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE UBLOW (IV4,IV,NCH)
C
C CERN PROGLIB# M409    UBLOW           .VERSION KERNHYW  1.03  841013
C ORIG. 13/10/84 JZ
C
      DIMENSION    IV4(99),IV(99)
      DATA IBLANK  /4H    /
 
      IF (NCH.EQ.0)          RETURN
      NW = NCH/4
      NC = NCH - 4*NW
      J1 = 0
      M  = IBLANK
      IF (NW.EQ.0)           GO TO 41
 
      DO 24 J=1,NW
      MW = IV4(J)
      FLD(0,9,M) = FLD(0,9,MW)
      IV(J1+1)   = M
      FLD(0,9,M) = FLD(9,9,MW)
      IV(J1+2)   = M
      FLD(0,9,M) = FLD(18,9,MW)
      IV(J1+3)   = M
      FLD(0,9,M) = FLD(27,9,MW)
      IV(J1+4)   = M
   24 J1 = J1 + 4
      IF (NC.EQ.0)           RETURN
 
   41 JP = 0
      MW = IV4(NW+1)
 
      DO 44 J=1,NC
      FLD(0,9,M) = FLD(JP,9,MW)
      IV(J1+1) = M
      J1 = J1 + 1
   44 JP = JP + 9
      RETURN
      END
#ifdef CERNLIB_TCGEN_UBLOW
#undef CERNLIB_TCGEN_UBLOW
#endif
*      ==================================================
#include "qcardl.inc"
