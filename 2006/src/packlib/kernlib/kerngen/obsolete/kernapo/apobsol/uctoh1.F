*
* $Id: uctoh1.F,v 1.1.1.1 1996/02/15 17:51:10 mclareni Exp $
*
* $Log: uctoh1.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:10  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE UCTOH1 (KCHAR,KHOLL,NCH)
C
C CERN PROGLIB# M409    UCTOH1          .VERSION KERNAPO  1.07  860212
C ORIG. 11/02/86 A. PETRILLI, CERN
C
      PARAMETER (MAXCPW=4)

      CHARACTER*(9) KCHAR,KHOLL

      IF (NCH.LE.0)          RETURN

      J = 1
      KHOLL(1:4*NCH) = ' '
      DO 14 L=1,NCH
         KHOLL(J:J) = KCHAR(L:L)
         J = J+MAXCPW
   14 CONTINUE
      RETURN
      END
#ifdef CERNLIB_TCGEN_UCTOH1
#undef CERNLIB_TCGEN_UCTOH1
#endif
