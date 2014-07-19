*
* $Id: xinbf.F,v 1.1.1.1 1996/02/15 17:51:16 mclareni Exp $
*
* $Log: xinbf.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:16  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINBF (LUN,XBV,NXB)
 
C CERN PROGLIB# Z203    XINBF           .VERSION KERNCDC  2.09  840618
 
      DIMENSION    XBV(NXB)
 
      READ (LUN,IOSTAT=NSTAT) XBV
      IF (NSTAT)    61,24,64
   24 CONTINUE
      RETURN
 
   61 NXB = 0
      RETURN
 
   64 IF (NSTAT.EQ.63)       GO TO 61
      IF (NSTAT.EQ.89)       GO TO 24
      NXB = -7
      RETURN
      END
#ifdef CERNLIB_XVECT_XINBF
#undef CERNLIB_XVECT_XINBF
#endif
