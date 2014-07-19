*
* $Id: xinb.F,v 1.1.1.1 1996/02/15 17:52:53 mclareni Exp $
*
* $Log: xinb.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:53  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINB  (LUN,XBV,NXB)
C
C CERN PROGLIB# Z203    XINB            .VERSION KERNHYW  1.07  870624
C ORIG. 1/09/84 JZ, changed to use IOSTAT 22/06/87
C
      DIMENSION    XBV(999)
C
      NIN = NXB
      READ (LUN,IOSTAT=JSTAT) NR,(XBV(J),J=1,MIN0(NR,NIN))
      IF   (JSTAT)           61, 17, 64
   17 NXB = MIN0(NR,NIN)
      RETURN
C
   61 NXB = 0
      RETURN
C
   64 NXB = -JSTAT
      RETURN
      END
#ifdef CERNLIB_XVECT_XINB
#undef CERNLIB_XVECT_XINB
#endif
*      ==================================================
#include "qcardl.inc"
