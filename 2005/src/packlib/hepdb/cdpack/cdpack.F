*
* $Id: cdpack.F,v 1.2 1996/05/01 13:47:10 cernlib Exp $
*
* $Log: cdpack.F,v $
* Revision 1.2  1996/05/01 13:47:10  cernlib
* do not try to include zebra/q_cbyt.inc. This no longer exists
*
* Revision 1.1.1.1  1996/02/28 16:24:21  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPACK (IAIN, LIN, LOU, LAUX, LBITL, IAOU, IAUX)
*     ===========================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPACK (IAIN, LIN, LOU*, LAUX*, LBITL, IAOU*, IAUX)     *
*                                                                      *
*   Compresses data from 32 to LBITL bit size                          *
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
*     IAUX     Auxiliary array for internal working space              *
*                                                                      *
*   Called by CDCMPR                                                   *
*                                                                      *
************************************************************************
*
      DIMENSION       IAIN(LIN), IAOU(LIN), IAUX(LIN)
      DIMENSION       IHB(33), LHB(32), AHB(32)
      DATA            MINBIT /2/  , IBIG /9999999/
#include "zebra/q_jbit.inc"
* Ignoring t=pass
#include "zebra/q_sbyt.inc"
* Ignoring t=pass
*#include "zebra/q_cbyt.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      CALL VZERO (IHB, 33)
      CALL VZERO (IAOU, LIN)
      CALL VFILL (LHB, 32, IBIG)
*
* *** Histogram of the input stream bit-length
*
      DO 3 I = 1, LIN
        DO 1 J = 1, 32
          J1    = 33 - J
          IF (JBIT(IAIN(I),J1).EQ.1) GO TO 2
    1   CONTINUE
    2   IHB(J1) = IHB(J1) + 1
    3 CONTINUE
*
* *** Look for the minimum storage length
*
      NW    = 0
      JJ    = 33 - MINBIT
      DO 4 J = 1, JJ
        J1    = 33 - J
        NW    = NW + IHB(J1+1)
        LHB(J1) = (LIN*J1-1)/32 + 2*NW
    4 CONTINUE
*
      CALL VFLOAT (LHB, AHB, 32)
      LBITL = LVMIN (AHB, 32)
      IF (LHB(LBITL)+1.GE.LIN)       GO TO 991
*
      ICOMP = LBITL + 1
      NCOMP = 32 - LBITL
*
* *** Pack the input with LBITL byte size
* ***    (If input is longer then LBITL, write it in a separate
* ***     output word at the end of the buffer)
*
      LAUX  = 0
      LOU   = 1
      IN    = 1
      IB    = 1
   11 CONTINUE
      IF (NCOMP.GT.0) THEN
        ICHECK = JBYT (IAIN(IN), ICOMP, NCOMP)
      ELSE
        ICHECK = 0
      ENDIF
      IBA   = IB + LBITL
      IF (IBA.LE.32) THEN
        IF (LBITL.GT.0)
     +  IAOU(LOU) = MSBYT (IAIN(IN), IAOU(LOU), IB, LBITL)
        IB = IBA
      ELSE
        LBIT1 = 32 - IB + 1
        IF (LBIT1.GT.0)
     +  IAOU(LOU) = MSBYT (IAIN(IN), IAOU(LOU), IB, LBIT1)
*       ITEST = JBYT (IAOU(LOU), IB, LBIT1)
        IB    = IBA - 32
        LOU   = LOU + 1
        IF (IB.GT.1)
     +    IAOU(LOU) = MCBYT (IAIN(IN), LBIT1+1, IAOU(LOU), 1, IB-1)
*       ITEST = MSBYT (IAOU(LOU), ITEST, LBIT1+1, IB-1)
      ENDIF
      IF (ICHECK.NE.0) THEN
        IF (LAUX+2.GT.LIN)           GO TO 991
        IAUX(LAUX+1) = IN
        IAUX(LAUX+2) = IAIN(IN)
        LAUX  = LAUX + 2
      ENDIF
      IF (IN.LT.LIN) THEN
        IN    = IN + 1
        GO TO 11
      ENDIF
*
      ICAR  = 0
      IF (IAOU(LOU+1).NE.0) ICAR = IAOU(LOU+1)
      IF (LAUX.GT.0) THEN
        IF (LOU+LAUX.GT.LIN)         GO TO 991
        DO 21 I = 1, LAUX
   21   IAOU(LOU+I) = IAUX(I)
        IF (ICAR.NE.0) IAOU(LIN) = ICAR
        LOU   = LOU + LAUX
      ENDIF
      GO TO 999
*
  991 CONTINUE
      CALL UCOPY (IAIN, IAOU, LIN)
      LOU   = LIN
      LBITL = 32
      LAUX  = 0
*                                                             END CDPACK
  999 END
