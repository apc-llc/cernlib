*
* $Id: xinbs.F,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: xinbs.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINBS (LUN,XAV,NA,XBV,NXB)
C
C CERN PROGLIB# Z203    XINBS           .VERSION KERNUNI  1.07  830120
C
      DIMENSION    XBV(99), XAV(NA)
 
#if defined(CERNLIB_F77)
      READ (LUN,END=61,ERR=64) N,XAV,(XBV(J),J=1,MIN0(N,NXB))
      N   = MIN0 (N,NXB)
#endif
#if !defined(CERNLIB_F77)
      READ (LUN,END=61,ERR=64) N,XAV,(XBV(J),J=1,N)
#endif
      NXB = MAX0 (N,1)
      RETURN
 
   61 NXB = 0
      RETURN
 
   64 NXB = -7
      RETURN
      END
#ifdef CERNLIB_XVECT_XINBS
#undef CERNLIB_XVECT_XINBS
#endif
