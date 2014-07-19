*
* $Id: xinb.F,v 1.1.1.1 1996/02/15 17:52:42 mclareni Exp $
*
* $Log: xinb.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:42  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE XINB  (LUN,XBV,NXB)
C
C CERN PROGLIB# Z203    XINB            .VERSION KERNCRY  1.00  830612
C ORIG. 12/06/83 DL + JZ
C
      DIMENSION    XBV(999)
C

      BUFFERIN (LUN,-1) (N,N)
      IF   (UNIT(LUN))       22,61,64

   22 N = MIN0 (N,NXB)
      BUFFERIN (LUN,0)  (XBV,XBV(N))

      IF   (UNIT(LUN))       24,61,64
   24 NXB = LENGTH (LUN)
      RETURN
C
   61 NXB = 0
      RETURN
C
   64 NXB = -7
      RETURN
      END
#ifdef CERNLIB_TCGEN_XINB
#undef CERNLIB_TCGEN_XINB
#endif
#ifdef CERNLIB_XVECT_XINB
#undef CERNLIB_XVECT_XINB
#endif
