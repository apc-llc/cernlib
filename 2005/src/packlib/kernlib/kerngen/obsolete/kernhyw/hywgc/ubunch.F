*
* $Id: ubunch.F,v 1.1.1.1 1996/02/15 17:52:53 mclareni Exp $
*
* $Log: ubunch.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:53  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE UBUNCH (IV,IV4,NCH)
C
C CERN PROGLIB# M409    UBUNCH          .VERSION KERNHYW  1.03  841013
C ORIG. 13/10/84 JZ
C
      DIMENSION    IV(99),IV4(99)
      DATA IBLANK  /4H    /
 
      IF (NCH.EQ.0)          RETURN
      NW = NCH/4
      NC = NCH - 4*NW
      J1 = 0
      IF (NW.EQ.0)           GO TO 41
 
      DO 24 J4=1,NW
      FLD(0,9,M)  = FLD(0,9,IV(J1+1))
      FLD(9,9,M)  = FLD(0,9,IV(J1+2))
      FLD(18,9,M) = FLD(0,9,IV(J1+3))
      FLD(27,9,M) = FLD(0,9,IV(J1+4))
      IV4(J4) = M
   24 J1 = J1 + 4
      IF (NC.EQ.0)           RETURN
 
   41 M  = IBLANK
      JP = 0
 
      DO 44 J=1,NC
      FLD(JP,9,M) = FLD(0,9,IV(J1+1))
      JP = JP + 9
   44 J1 = J1 + 1
      IV4(NW+1) = M
      RETURN
      END
#ifdef CERNLIB_TCGEN_UBUNCH
#undef CERNLIB_TCGEN_UBUNCH
#endif
*      ==================================================
#include "qcardl.inc"
