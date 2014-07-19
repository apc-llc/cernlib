*
* $Id: hyw8t9.F,v 1.1.1.1 1996/02/15 17:52:48 mclareni Exp $
*
* $Log: hyw8t9.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:48  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE HYW8T9 (IV8,IV9,NW)
C
C CERN PROGLIB#         HYW8T9          .VERSION KERNHYW  1.07  870624
C ORIG. 22/06/87 JZ
C     Expand 8-bit bytes to 9-bit bytes
C
      DIMENSION    IV8(99),IV9(99)
 
C-       0             9             18            27          35
C-      |             |             |             |             |
C-      | | | | | ||||| | | | | ||||| | | | | ||||| | | | | |||||
C-              |           |           |           |
C-               4           12          20          28        35
 
      DO 24 JL=1,NW
      M8 = IV8(JL)
      M9           = FLD(28,8,M8)
      FLD(19,8,M9) = FLD(20,8,M8)
      FLD(10,8,M9) = FLD(12,8,M8)
      FLD (1,8,M9) = FLD (4,8,M8)
   24 IV9(JL) = M9
      RETURN
      END
#ifdef CERNLIB_TCGEN_HYW8T9
#undef CERNLIB_TCGEN_HYW8T9
#endif
*      ==================================================
#include "qcardl.inc"
