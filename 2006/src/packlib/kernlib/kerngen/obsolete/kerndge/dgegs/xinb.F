*
* $Id: xinb.F,v 1.1.1.1 1996/02/15 17:54:27 mclareni Exp $
*
* $Log: xinb.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:27  mclareni
* Kernlib
*
*
#include "kerndge/pilot.h"
      SUBROUTINE XINB  (LUN,XBV,NXB)
C
C CERN PROGLIB# Z203    XINB            .VERSION KERNDGE  1.01  860217
C ORIG. 03/02/86  JZ
C
      DIMENSION    XBV(99)
C
      READ (LUN,END=61,ERR=64) N, (XBV(J),J=1,MIN0(N,NXB))
      N   = MIN0 (N,NXB)
      NXB = MAX0 (N,1)
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
