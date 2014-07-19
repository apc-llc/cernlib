*
* $Id: xinb.F,v 1.1.1.1 1996/02/15 17:54:51 mclareni Exp $
*
* $Log: xinb.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:51  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      SUBROUTINE XINB  (LUN,XBV,NXB)
C
C CERN PROGLIB# Z203    XINB            .VERSION KERNNOR  0.99  790518
C
      DIMENSION    XBV(99)
 
      READ (LUN,END=61,ERR=64) N, (XBV(J),J=1,MIN0(N,NXB))
      N   = MIN0 (N,NXB)
      NXB = MAX0 (N,1)
      RETURN
 
   61 NXB = 0
      RETURN
 
   64 NXB = -7
      RETURN
      END
#ifdef CERNLIB_TCGEN_XINB
#undef CERNLIB_TCGEN_XINB
#endif
#ifdef CERNLIB_XVECT_XINB
#undef CERNLIB_XVECT_XINB
#endif
