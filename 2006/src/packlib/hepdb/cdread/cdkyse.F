*
* $Id: cdkyse.F,v 1.1.1.1 1996/02/28 16:24:26 mclareni Exp $
*
* $Log: cdkyse.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:26  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKYSE (KEYS, ITIME, IRC)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDKYSE (KEYS, ITIME, IRC*)                              *
*                                                                      *
*   Creates or completes the Key banks supported as next of same type  *
*   to the Node bank for 'S' option                                    *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     KEYS     Vector of keys                                          *
*     ITIME    Time for which the valid keys are required              *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDBKKS                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                      Could be set to nonzero by some routines called *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
      PARAMETER       (NZ=0)
      DIMENSION       KEYS(9), ITIME(9)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      IRC    = 0
      IF (LFIXCD.EQ.LBNOCD) THEN
        JBIAS  = -KLKYCD
      ELSE
        JBIAS  = 0
      ENDIF
      LBKYCD = LSAVCD
      ND     = IQ(KOFUCD+LBNOCD+MNDNWD)
      DO 25 JK = 1, NKEYCK
*
*  *    Load the keys for this serial number
*
        IK     = NKEYCK + 1 - JK
        CALL CDKEYR (IK, NWKYCK, KEYNCK)
*
*  *    Use the selection
*
        CALL CDKSEL (ITIME, KEYS, KEYNCK, -1, ISEL, INBR)
        IF (ISEL.NE.0)                                GO TO 25
        IF (IPRBCA.EQ.0.AND.IPRECA.EQ.0) THEN
*
*  **     S option with time validity (Last inserted ones only)
*  **     See if such Key bank already exists
*
          LBKYCD = LFIXCD
   10     LBKYCD = LQ(KOFUCD+LBKYCD)
          IF (LBKYCD.NE.0)  THEN
            DO 15 I = NOF3CK, NWKYCK
              IF (I.GT.NOF1CK.AND.I.LE.NOF1CK+2*NPARCD) THEN
                GO TO 15
              ELSE IF (I.EQ.IDHFLG.OR.I.EQ.IDHINS)  THEN
                GO TO 15
              ELSE
                IF (KEYNCK(I).NE.IQ(KOFUCD+LBKYCD+I)) GO TO 10
              ENDIF
   15       CONTINUE
            IF (IQ(KOFUCD+LBKYCD+IDHKSN).LT.KEYNCK(IDHKSN))  THEN
              CALL UCOPY (KEYNCK, IQ(KOFUCD+LBKYCD+1), NWKYCK)
            ENDIF
            GO TO 25
          ENDIF
*
        ELSE
          IF (JBIT(KEYNCK(IDHFLG),JIGNCD).NE.0)       GO TO 25
        ENDIF
*
*  *    Create a new Key bank
*
        CALL UCOPY  (IQ(KOFUCD+LBNOCD+MNDIOF), IOKYCD, NWNOCD)
        CALL CDBANK (IDIVCD, LBKYCD, LFIXCD, JBIAS, 'KYCD', NLKYCD,
     +               NSKYCD, ND, IOKYCD, NZ, IRC)
        IF (IRC.NE.0)                                 GO TO 999
*
        LQ(KOFUCD+LBKYCD-KLNOCD) = LBNOCD
        LQ(KOFUCD+LBKYCD-KLUPCD) = LBUPCD
        CALL UCOPY (KEYNCK, IQ(KOFUCD+LBKYCD+1), NWKYCK)
        IF (IPRBCA.EQ.0.AND.IPRECA.EQ.0) THEN
        ELSE
          DO 20 I = 1, NPARCD-1
            IQ(KOFUCD+LBKYCD+NWKYCK+I) = IQ(KOFUCD+LBKYCD+NOF1CK+2*I-1)
   20     CONTINUE
          IQ(KOFUCD+LBKYCD+NWKYCK+NPARCD) = KEYNCK(NOF1CK+2*NPARCD-1) +1
        ENDIF
*
   25 CONTINUE
*                                                             END CDKYSE
  999 END
