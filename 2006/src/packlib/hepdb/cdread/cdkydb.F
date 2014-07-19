*
* $Id: cdkydb.F,v 1.1.1.1 1996/02/28 16:24:26 mclareni Exp $
*
* $Log: cdkydb.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:26  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKYDB (KEYS, LBK, ITIME, IRC)
*     =========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDKYDB (KEYS, LBK*, ITIME, IRC*)                        *
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
*   Called by CDUSEDB                                                  *
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
#include "hepdb/cinitl.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/cmulop.inc"
#include "hepdb/ctkxin.inc"
      PARAMETER       (NZ=0)
      DIMENSION       KEYS(9), LBK(9), ITIME(9)
*
*     ------------------------------------------------------------------
*
* *** Get rid of S option first
*
      IF (IOPSCA.NE.0) THEN
        CALL CDBKKS (KEYS, LBK, ITIME, IRC)
        GO TO 999
      ENDIF
*
* *** Get number of key banks needed
*
      CALL CDKMUL (KEYS, NKYMX, IRC)
      IF (IRC.NE.0)                                  GO TO 999
*
* *** Loop over all possible Key Banks
*
      NK     = 0
   10 NK     = NK + 1
*
*  ** Load the key values to be compared
*
      CALL CDKMLD (NK, KEYS)
*
*  ** Check if this Key bank already exists
*
      LSAVCD = LBNOCD
      JBIAS  = -KLKYCD
      ND     = IQ(KOFUCD+LSAVCD+MNDNWD)
      LBKYCD = LQ(KOFUCD+LSAVCD-KLKYCD)
   15 IF (LBKYCD.NE.0)  THEN
        IF (NWKYCK.GT.NSYSCK)  THEN
          DO 20 I = NSYSCK+1, NWKYCK
            IF (IOKYCA(I).NE.0)  THEN
              IF (KYVMCK(I).NE.IQ(KOFUCD+LBKYCD+I))  GO TO 25
            ENDIF
   20     CONTINUE
        ENDIF
        GO TO 30
   25   LSAVCD = LBKYCD
        LBKYCD = LQ(KOFUCD+LSAVCD)
        JBIAS  = 0
        GO TO 15
      ELSE
*
*  *    The Key bank does not exist, create it
*
        CALL UCOPY  (IQ(KOFUCD+LBNOCD+MNDIOF), IOKYCD, NWNOCD)
        CALL CDBANK (IDIVCD, LBKYCD, LSAVCD, JBIAS, 'KYCD', NLKYCD,
     +               NSKYCD, ND, IOKYCD, NZ, IRC)
        IF (IRC.NE.0)                                GO TO 999
        LQ(KOFUCD+LBKYCD-KLNOCD) = LBNOCD
        LQ(KOFUCD+LBKYCD-KLUPCD) = LBUPCD
      ENDIF
*
* *** Copy the key vector into Key bank
*
   30 LBK(NK) = LBKYCD
      DO 35 I = 1, NWKYCK
        IF (IOKYCA(I).NE.0) IQ(KOFUCD+LBKYCD+I) = KYVMCK(I)
   35 CONTINUE
*
      IF (NK.LT.NKYMX)                               GO TO 10
*                                                             END CDKYDB
  999 END
