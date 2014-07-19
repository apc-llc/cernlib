*
* $Id: xinbs.F,v 1.1.1.1 1996/02/15 17:54:44 mclareni Exp $
*
* $Log: xinbs.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:44  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINBS (LUN,XAV,NA,XBV,NXB)
C
C CERN PROGLIB# Z203    XINBS           .VERSION KERNIBX  1.00  900101
C
      COMMON /SLATE/ NXR, DUMMY(39)
      DIMENSION    XBV(NXB), XAV(NA)
C
      READ (LUN,IOSTAT=JSTAT,NUM=NBYTES) NR,XAV,XBV
      IF   (JSTAT)                 61, 24, 64
   24 NXR = NR
      NR  = MIN (NR,NXB)
      NXB = MAX (NR,1)
      RETURN
C
   61 NXB = 0
      RETURN
C
   64 NXB = -JSTAT
      READ (LUN) N
      RETURN
      END
#ifdef CERNLIB_XVECT_XINBS
#undef CERNLIB_XVECT_XINBS
#endif
