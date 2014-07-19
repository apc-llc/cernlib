*
* $Id: xinbf.F,v 1.1.1.1 1996/02/15 17:54:44 mclareni Exp $
*
* $Log: xinbf.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:44  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINBF (LUN,XBV,NXB)
C
C CERN PROGLIB# Z203    XINBF           .VERSION KERNIBX  1.00  900101
C
      DIMENSION    XBV(NXB)
C
      READ (LUN,IOSTAT=JSTAT,NUM=NBYTES) XBV
      IF   (JSTAT)                 61, 24, 64
   24 NXB = MAX (1, NBYTES/4)
      RETURN
C
   61 NXB = 0
      RETURN
C
   64 NXB = -JSTAT
      READ (LUN) N
      RETURN
      END
#ifdef CERNLIB_XVECT_XINBF
#undef CERNLIB_XVECT_XINBF
#endif
