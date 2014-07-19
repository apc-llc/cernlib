*
* $Id: ifromc.F,v 1.1.1.1 1996/02/15 17:51:20 mclareni Exp $
*
* $Log: ifromc.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:20  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if defined(CERNLIB_F77)
      FUNCTION IFROMC(CH)
C
C CERN PROGLIB# M431    IFROMC          .VERSION KERNCDC  2.16  860514
C ORIG. 10/02/86 M.METCALF
C
C   TO CONVERT 1 TO 4 CHARACTERS TO A PACKED ASCII INTEGER FORM
C   CDC ONLY
C
      CHARACTER*(*) CH
      PARAMETER (IBLANK = O"10020040")
      IFROMC = IBLANK
      LENCH = LEN(CH)
      DO 1 IC = 1, MIN(LENCH,4)
         CALL CHTOI(CH(IC:IC), IVAL, *999)
         IFROMC = SHIFT(IFROMC, 8) + IVAL
    1 CONTINUE
      IF (LENCH.LT.4) IFROMC = ISHFTC(IFROMC, 8*(4-LENCH), 32)
  999 END
#ifdef CERNLIB_TCGEN_IFROMC
#undef CERNLIB_TCGEN_IFROMC
#endif
#endif
