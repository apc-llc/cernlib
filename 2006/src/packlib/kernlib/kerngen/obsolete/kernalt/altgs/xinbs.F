*
* $Id: xinbs.F,v 1.1.1.1 1996/02/15 17:50:56 mclareni Exp $
*
* $Log: xinbs.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:56  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE XINBS (LUN,XAV,NA,XBV,NXB)
C
C CERN PROGLIB# Z203    XINBS           .VERSION KERNALT  1.00  880212
C ORIG.  8/02/88  JZ
C
      DIMENSION    XBV(99), XAV(NA)
 
      READ (LUN,IOSTAT=JSTAT) N,XAV,(XBV(J),J=1,MIN(N,NXB))
      IF   (JSTAT)           61, 24, 64
   24 N   = MIN (N,NXB)
      NXB = MAX (N,1)
      RETURN
 
   61 NXB = 0
      RETURN
 
   64 NXB = -JSTAT
      RETURN
      END
#ifdef CERNLIB_TCGEN_XINBS
#undef CERNLIB_TCGEN_XINBS
#endif
#ifdef CERNLIB_XVECT_XINBS
#undef CERNLIB_XVECT_XINBS
#endif
