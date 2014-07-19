*
* $Id: cditoc.F,v 1.1.1.1 1996/02/28 16:24:37 mclareni Exp $
*
* $Log: cditoc.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:37  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDITOC (IAR, CH, NWD, NCH)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDITOC (IAR, CH*, NWD, NCH)                             *
*                                                                      *
*   Converts integer array IAR (of length NWD) to an output character  *
*   string CH (of type character and NCH long) via the look-up         *
*   table stored in CSTRCA                                             *
*                                                                      *
*   Called by CDLIND                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/caopti.inc"
      CHARACTER       CH*(*), CH1*1
      DIMENSION       IAR(9), IB(4)
*     ------------------------------------------------------------------
*
      DO 10 J = 1, NWD
        K  = 4*J - 4
        CALL UPKBYT (IAR(J), 1, IB, 4, MPAKCA)
        DO 5 I = 1, 4
          IF (K+I.GT.NCH) GO TO 999
          CH1 = CSTRCA(63:63)
          IF (IB(I).GE.32.AND.IB(I).LE.127) CH1 = CSTRCA(IB(I):IB(I))
          CH(K+I:K+I) = CH1
    5   CONTINUE
   10 CONTINUE
*                                                             END CDITOC
  999 END
