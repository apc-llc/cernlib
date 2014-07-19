*
* $Id: xinbf.F,v 1.1.1.1 1996/02/15 17:52:54 mclareni Exp $
*
* $Log: xinbf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:54  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINBF (LUN,XBV,NXB)
C
C CERN PROGLIB# Z203    XINBF           .VERSION KERNHYW  1.07  870624
C ORIG. 22/06/87 JZ
C
      DIMENSION    XBV(NXB)
C
      READ (LUN,IOSTAT=JSTAT) XBV
      IF   (JSTAT)           61, 17, 64
C
   61 NXB = 0
   17 RETURN
C
   64 NXB = -JSTAT
      RETURN
      END
#ifdef CERNLIB_XVECT_XINBF
#undef CERNLIB_XVECT_XINBF
#endif
*      ==================================================
#include "qcardl.inc"
