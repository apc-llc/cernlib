*
* $Id: xinbs.F,v 1.1.1.1 1996/02/15 17:52:56 mclareni Exp $
*
* $Log: xinbs.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:56  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINBS (LUN,XAV,NA,XBV,NXB)
C
C CERN PROGLIB# Z203    XINBS           .VERSION KERNHYW  1.02  840921
C ORIG. 9/10/79 JZ
C
      DIMENSION    XBV(999), XAV(NA)
C
      NIN = NXB
      READ (LUN,END=61,ERR=64) NR,XAV,(XBV(J),J=1,MIN0(NR,NIN))
      NXB = MIN0(NR,NIN)
      RETURN
C
   61 NXB = 0
      RETURN
C
   64 NXB = -7
      RETURN
      END
#ifdef CERNLIB_XVECT_XINBS
#undef CERNLIB_XVECT_XINBS
#endif
