*
* $Id: xinbf.F,v 1.1.1.1 1996/02/15 17:53:08 mclareni Exp $
*
* $Log: xinbf.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:08  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINBF (LUN,XBV,NXB)
C
C CERN PROGLIB# Z203    XINBF           .VERSION KERNIBM  1.06  790518
C
      DIMENSION    XBV(NXB)
C
      READ (LUN,END=61,ERR=64) XBV
      RETURN
C
   61 NXB = 0
      RETURN
C
   64 NXB = -7
      READ (LUN) N
      RETURN
      END
#ifdef CERNLIB_XVECTIBM_XINBF
#undef CERNLIB_XVECTIBM_XINBF
#endif
#ifdef CERNLIB_XVECT_XINBF
#undef CERNLIB_XVECT_XINBF
#endif
