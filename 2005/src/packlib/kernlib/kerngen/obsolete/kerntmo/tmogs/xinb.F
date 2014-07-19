*
* $Id: xinb.F,v 1.1.1.1 1996/02/15 17:53:51 mclareni Exp $
*
* $Log: xinb.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:51  mclareni
* Kernlib
*
*
#include "kerntmo/pilot.h"
      SUBROUTINE XINB  (LUN,XBV,NXB)
C
C CERN PROGLIB# Z203    XINB            .VERSION KERNTMO  1.00  890208
C ORIG.  4/01/89  JZ
C
      DIMENSION    XBV(99)
 
      READ (LUN,IOSTAT=JSTAT) N, (XBV(J),J=1,MIN(N,NXB))
      IF   (JSTAT)           61, 24, 64
   24 N   = MIN (N,NXB)
      NXB = MAX (N,1)
      RETURN
 
   61 NXB = 0
      RETURN
 
   64 NXB = -JSTAT
      RETURN
      END
#ifdef CERNLIB_TCGEN_XINB
#undef CERNLIB_TCGEN_XINB
#endif
#ifdef CERNLIB_XVECT_XINB
#undef CERNLIB_XVECT_XINB
#endif
