*
* $Id: xinbs.F,v 1.1.1.1 1996/02/15 17:51:16 mclareni Exp $
*
* $Log: xinbs.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:16  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINBS (LUN,XAV,NA,XBV,NXB)
 
C CERN PROGLIB# Z203    XINBS           .VERSION KERNCDC  2.09  840618
 
      DIMENSION    XBV(99), XAV(NA)
 
      READ (LUN,IOSTAT=NSTAT) N,XAV,(XBV(J),J=1,MIN0(N,NXB))
      IF (NSTAT)    61,24,64
   24 NXB = MIN0 (N,NXB)
      RETURN
 
   61 NXB = 0
      RETURN
 
   64 IF (NSTAT.EQ.63)       GO TO 61
      IF (NSTAT.EQ.89)       GO TO 24
      NXB = -7
      RETURN
      END
#ifdef CERNLIB_XVECT_XINBS
#undef CERNLIB_XVECT_XINBS
#endif
