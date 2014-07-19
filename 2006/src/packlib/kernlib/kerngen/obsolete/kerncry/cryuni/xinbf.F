*
* $Id: xinbf.F,v 1.1.1.1 1996/02/15 17:52:45 mclareni Exp $
*
* $Log: xinbf.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:45  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE XINBF (LUN,XBV,NXB)
C
C CERN PROGLIB# Z203    XINBF           .VERSION KERNCRY  1.09  881102
C ORIG. 02/11/88 Judy Richards
C
      DIMENSION    XBV(NXB)

C   : READ (LUN,END=61,ERR=64) XBV
   11 NXU = NXB
      CALL READ (LUN,XBV,NXU,ISTAT,IUBC)
      IF (ISTAT .GE. 4)      GO TO 64
      IF (ISTAT .GE. 2)      GO TO 61
      IF (ISTAT .EQ. 1)      GO TO 11
      NXB = NXU
      RETURN

   61 NXB = 0
      RETURN

   64 NXB = -ISTAT
      CALL SKIPBAD (LUN,NBLOCK,ISTAT)
      PRINT 9064, LUN,NBLOCK,ISTAT
      IF (ISTAT.GT.0)        GO TO 61
      RETURN

 9064 FORMAT (' !!!!!    Read error in XINBF for LUN=',I3,
     F', SKIPBAD discards'/1X,I15,
     F' bad blocks and returns status=',Z17,' hex.')
      END
#ifdef CERNLIB_TCGEN_XINBF
#undef CERNLIB_TCGEN_XINBF
#endif
#ifdef CERNLIB_XVECT_XINBF
#undef CERNLIB_XVECT_XINBF
#endif
