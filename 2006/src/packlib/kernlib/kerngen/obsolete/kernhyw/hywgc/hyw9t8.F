*
* $Id: hyw9t8.F,v 1.1.1.1 1996/02/15 17:52:49 mclareni Exp $
*
* $Log: hyw9t8.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:49  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE HYW9T8 (IV9,IV8,NW)
C
C CERN PROGLIB#         HYW9T8          .VERSION KERNHYW  1.07  870624
C ORIG. 22/06/87 JZ
C     Contract 9-bit bytes to 8-bit bytes
C
      DIMENSION    IV9(99),IV8(99)
 
 
      DO 24 JL=1,NW
      M9 = IV9(JL)
      M8           = FLD(28,8,M9)
      FLD(20,8,M8) = FLD(19,8,M9)
      FLD(12,8,M8) = FLD(10,8,M9)
      FLD (4,8,M8) = FLD (1,8,M9)
   24 IV8(JL) = M8
      RETURN
      END
#ifdef CERNLIB_TCGEN_HYW9T8
#undef CERNLIB_TCGEN_HYW9T8
#endif
*      ==================================================
#include "qcardl.inc"
