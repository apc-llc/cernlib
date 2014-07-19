*
* $Id: xinb.F,v 1.1.1.1 1996/02/15 17:51:16 mclareni Exp $
*
* $Log: xinb.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:16  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINB  (LUN,XBV,NXB)
 
C CERN PROGLIB# Z203    XINB            .VERSION KERNCDC  2.09  840618
 
      DIMENSION    XBV(99)
 
      READ (LUN,IOSTAT=NSTAT) N,(XBV(J),J=1,MIN0(N,NXB))
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
#ifdef CERNLIB_XVECT_XINB
#undef CERNLIB_XVECT_XINB
#endif
