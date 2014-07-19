*
* $Id: cdifrc.F,v 1.1.1.1 1996/02/28 16:24:20 mclareni Exp $
*
* $Log: cdifrc.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:20  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDIFRC (CH, IAR, NWD, NCH)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDIFRC (CH, IAR*, NWD, NCH)                             *
*                                                                      *
*   Converts character string CH (of type character and NCH long)      *
*   to an output integer array IAR (of length NWD) via the look-up     *
*   table stored in CSTRCA                                             *
*                                                                      *
*   Called by CDLINC                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/caopti.inc"
      CHARACTER       CH*(*)
      DIMENSION       IAR(9), IB(4)
*     ------------------------------------------------------------------
*
      DO 10 J = 1, NWD
        K   = 4*J - 4
        DO 5 L = 1, 4
          IF (K+L.LE.NCH) THEN
            I = INDEX (CSTRCA(32:127), CH(K+L:K+L))
            IB(L) = I + 31
          ELSE
            IB(L) = 31
          ENDIF
    5   CONTINUE
        CALL PKBYT (IB, IAR(J), 1, 4, MPAKCA)
   10 CONTINUE
*                                                             END CDIFRC
  999 END
