*
* $Id: cdupck.F,v 1.1.1.1 1996/02/28 16:24:36 mclareni Exp $
*
* $Log: cdupck.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:36  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUPCK (IAIN, LIN, LOU, LAUX, LBITL, IAOU)
*     =====================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDUPCK (IAIN, LIN, LOU*, LAUX, LBITL, IAOU*)            *
*                                                                      *
*   Uncompresses data from LBITL to 32 bit size                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IAIN     Input array                                             *
*     LIN      Length of the input array                               *
*     LOU      Length of the output array                              *
*     LAUX     Number of words exceeding the length LBITL and therefore*
*              are stored in 32 bits with locations given              *
*     LBITL    Number of bits to be used for storing                   *
*     IAOU     Output array                                            *
*                                                                      *
*   Called by CDUCMP                                                   *
*                                                                      *
************************************************************************
*
      DIMENSION       IAIN(LIN), IAOU(LIN)
      LOGICAL         AUXI, AUXIL
#include "zebra/q_jbit.inc"
* Ignoring t=pass
#include "zebra/q_sbyt.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      CALL VZERO (IAOU, LOU)
*
      AUXI   = LAUX.GT.0
*
      LAUX   = LIN - LAUX + 1
      IN     = 1
      IB     = 1
      IO     = 1
   10 CONTINUE
      AUXIL  = (AUXI.AND.(IAIN(LAUX).EQ.IO))
      IF (AUXIL) THEN
        LAUX   = LAUX + 1
        IAOU(IO) = IAIN(LAUX)
        LAUX   = LAUX + 1
      ENDIF
      AUXIL  = .NOT.AUXIL
      IBA    = IB + LBITL
      IF (IBA.LE.32) THEN
        IF (AUXIL.AND.LBITL.GT.0) IAOU(IO) = JBYT (IAIN(IN), IB, LBITL)
        IB     = IBA
      ELSE
        LBIT1  = 32 - IB + 1
        IF (AUXIL.AND.LBIT1.GT.0) IAOU(IO) = JBYT (IAIN(IN), IB, LBIT1)
        IB     = IBA - 32
        IN     = IN + 1
        IF (AUXIL.AND.IB.GT.1)
     +  IAOU(IO) = MSBYT (IAIN(IN), IAOU(IO), LBIT1+1, IB-1)
      ENDIF
      IF (IO.LT.LOU) THEN
        IO     = IO + 1
        GO TO 10
      ENDIF
*                                                             END CDUPCK
      END
