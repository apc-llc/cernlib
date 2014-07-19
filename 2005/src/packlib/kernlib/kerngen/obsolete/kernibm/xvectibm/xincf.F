*
* $Id: xincf.F,v 1.1.1.1 1996/02/15 17:53:09 mclareni Exp $
*
* $Log: xincf.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:09  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINCF (LUN,XBV,NXB)
C
C CERN PROGLIB# Z203    XINCF           .VERSION KERNIBM  1.12  820329
C
      DIMENSION    XBV(NXB)
C
      READ (LUN,8000,END=61,ERR=64) XBV
      RETURN
C
   61 NXB = 0
      RETURN
C
   64 NXB = -7
      READ (LUN,8000) N
      RETURN
C
 8000 FORMAT (200(90A4))
      END
#ifdef CERNLIB_XVECT_XINCF
#undef CERNLIB_XVECT_XINCF
#endif
