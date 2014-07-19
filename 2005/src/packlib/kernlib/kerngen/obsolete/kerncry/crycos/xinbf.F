*
* $Id: xinbf.F,v 1.1.1.1 1996/02/15 17:52:42 mclareni Exp $
*
* $Log: xinbf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:42  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE XINBF (LUN,XBV,NXB)
C
C CERN PROGLIB# Z203    XINBF           .VERSION KERNCRY  1.00  830612
C ORIG. 12/06/83 DL + JZ
C
      DIMENSION    XBV(999)
C

      BUFFERIN (LUN,0)  (XBV,XBV(NXB))

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
#ifdef CERNLIB_TCGEN_XINBF
#undef CERNLIB_TCGEN_XINBF
#endif
#ifdef CERNLIB_XVECT_XINBF
#undef CERNLIB_XVECT_XINBF
#endif
