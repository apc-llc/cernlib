*
* $Id: cddckv.F,v 1.1.1.1 1996/02/28 16:24:47 mclareni Exp $
*
* $Log: cddckv.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDDCKV (LUN, IC, KY, IRC)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDDCKV (LUN, IC, KY*, IRC*)                             *
*                                                                      *
*   Decodes values of a key vector (KY) stored in Vertical mode        *
*   (in NL lines on a file on unit LUN)                                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Logical Unit number containing the file to be decoded   *
*     IC       Number of keys concatenated to the present one          *
*     KY       Vector containing the decoded elements                  *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by  CDEDAS, CDEDKY                                          *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/ckkeys.inc"
      CHARACTER       LINE*80, CHFOR*1, CHVAL*80
      DIMENSION       KY(9), IC(9)
*
*     ------------------------------------------------------------------
*
      IRC   = 0
      J     = 0
    5 READ (LUN, '(A80)', END=999) LINE
*
      NCH   = LENOCC (LINE)
      IF (NCH.LE.0)    GO TO 30
      CHVAL = ' '
      I     = 13
      CHFOR = LINE(I:I)
      DO 6 K = I+1, NCH
        IF (LINE(K:K).NE.' ') THEN
          CHVAL = LINE(K:NCH)
          GO TO 20
        ENDIF
    6 CONTINUE
*
   20 J = J + 1
      IF (CHFOR.EQ.'I') THEN
        CALL CDCHTI (CHVAL, KY(J), IRC0)
        IF (IRC0.NE.0) IRC  = IRC0
      ELSE IF (CHFOR.EQ.'F') THEN
        CALL CDCTOR (CHVAL, KY(J), IRC0)
        IF (IRC0.NE.0) IRC  = IRC0
      ELSE IF (CHFOR.EQ.'B') THEN
        CALL CDCTOB (CHVAL, KY(J), IRC0)
        IF (IRC0.NE.0) IRC  = IRC0
      ELSE IF (CHFOR.EQ.'H') THEN
        NREP = IC(J)
        LCDAT = 4*(NREP+1)
        CALL UCTOH (CHVAL, KY(J), 4, LCDAT)
        J = J + NREP
      ELSE IF (CHFOR.EQ.'S') THEN
        NC = LENOCC (CHVAL)
        DO 21 K = 1, NC
          IF (CHVAL(K:K).EQ.' ') THEN
            KK = K
            GO TO 22
          ENDIF
   21   CONTINUE
        KY(J) = 0
        GO TO 25
   22   CONTINUE
        CALL CDCHTI (CHVAL(1:KK-1), IDATE, IRC0)
        IF (IRC0.NE.0) IRC  = IRC0
        CALL CDCHTI (CHVAL(KK:NC),  ITIME, IRC0)
        IF (IRC0.NE.0) IRC  = IRC0
        CALL CDPKTS (IDATE, ITIME, KY(J), IRC0)
        IF (IRC0.NE.0) IRC  = IRC0
      ELSE IF (CHFOR.EQ.'M') THEN
        NC = LENOCC (CHVAL)
        DO 23 K = 1, NC
          IF (CHVAL(K:K).EQ.' ') THEN
            KK = K
            GO TO 24
          ENDIF
   23   CONTINUE
        KY(J) = 0
        GO TO 25
   24   CONTINUE
        CALL CDCHTI (CHVAL(1:KK-1), IDATE, IRC0)
        IF (IRC0.NE.0) IRC  = IRC0
        CALL CDCHTI (CHVAL(KK:NC),  ITIME, IRC0)
        IF (IRC0.NE.0) IRC  = IRC0
        CALL CDPKTM (IDATE, ITIME, KY(J), IRC0)
        IF (IRC0.NE.0) IRC  = IRC0
      ENDIF
   25 IF (J.LT.NWKYCK) GO TO 5
   30 CONTINUE
*                                                             END CDDCKV
  999 END
