*
* $Id: cdkeyb.F,v 1.1.1.1 1996/02/28 16:24:27 mclareni Exp $
*
* $Log: cdkeyb.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKEYB (KEYS, LBK, ITIME, IRC)
*     =========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDKEYB (KEYS, LBK*, ITIME, IRC*)                        *
*                                                                      *
*   Creates or completes the Key banks supported as next of same type  *
*   to the Node bank                                                   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     KEYS     Vector of keys                                          *
*     LBK      Address(es) of Keys bank(s) KYCD                        *
*     ITIME    Time for which the valid keys are required              *
*              (in option S)                                           *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDUSEM                                                   *
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
      DIMENSION       KEYS(9), LBK(9), ITIME(9)
*
*     ------------------------------------------------------------------
*
      IRC    = 0
      IF (IOPMCA.NE.0) THEN
*
* ***   For M option no action to be taken
*
      ELSE IF (IOPSCA.NE.0) THEN
*
* ***   For S-Option call CDBKKS
*
        CALL CDBKKS (KEYS, LBK, ITIME, IRC)
*
      ELSE
*
* ***   Book a single bank if does not exist
*
        LSAVCD = LBNOCD
        JBIAS  = -KLKYCD
        ND     = IQ(KOFUCD+LSAVCD+MNDNWD)
        LBKYCD = LQ(KOFUCD+LSAVCD-KLKYCD)
   10   IF (LBKYCD.NE.0)  THEN
          IF (NWKYCK.GT.NSYSCK)  THEN
            DO 15 I = NSYSCK+1, NWKYCK
              IF (IOKYCA(I).NE.0)  THEN
                IF (KEYS(I).NE.IQ(KOFUCD+LBKYCD+I)) GO TO 20
              ENDIF
   15       CONTINUE
            GO TO 25
          ELSE
            GO TO 25
          ENDIF
   20     LSAVCD = LBKYCD
          LBKYCD = LQ(KOFUCD+LSAVCD)
          JBIAS  = 0
          GO TO 10
        ELSE
*
*  *      The Key bank does not exist, create it
*
          CALL UCOPY  (IQ(KOFUCD+LBNOCD+MNDIOF), IOKYCD, NWNOCD)
          CALL CDBANK (IDIVCD, LBKYCD, LSAVCD, JBIAS, 'KYCD', NLKYCD,
     +                 NSKYCD, ND, IOKYCD, NZ, IRC)
          IF (IRC.NE.0)                             GO TO 999
          LQ(KOFUCD+LBKYCD-KLNOCD) = LBNOCD
          LQ(KOFUCD+LBKYCD-KLUPCD) = LBUPCD
        ENDIF
*
   25   LBK(1) = LBKYCD
        DO 30 I = 1, NWKYCK
          IF (IOKYCA(I).NE.0) IQ(KOFUCD+LBKYCD+I) = KEYS(I)
   30   CONTINUE
      ENDIF
*                                                             END CDKEYB
  999 END
