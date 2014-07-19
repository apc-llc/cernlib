*
* $Id: cdbook.F,v 1.1.1.1 1996/02/28 16:24:25 mclareni Exp $
*
* $Log: cdbook.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:25  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDBOOK (PATHN, LBK, NBANK, CHOPT, IRC)
*     =================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDBOOK (PATHN, LBK*, NBANK, CHOPT, IRC*)                *
*                                                                      *
*   Creates a chain of NBANK Key banks supported as next of same type  *
*   to the Node bank for a given pathname                              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LBK      Address(es) of Keys bank(s) KYCD                        *
*     NBANK    Number of banks to be created                           *
*     CHOPT    Character string with any of the following characters   *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user,   CDREPL, CDSTOR                                   *
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
      PARAMETER       (NZ=0)
      DIMENSION       LBK(9)
      CHARACTER       PATHN*(*), CHOPT*(*)
*
*     ------------------------------------------------------------------
*
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)    GO TO 999
*
* *** Find the address of the node bank
*
      CALL CDNODE (PATHN, IRC)
      IF (IRC.NE.0)    GO TO 999
*
* *** Loop to create the Key Banks
*
      IF (LQ(KOFUCD+LBNOCD-KLKYCD).NE.0) THEN
        LFIXCD = LZLAST (IDIVCD, LQ(KOFUCD+LBNOCD-KLKYCD))
        JBINI  = 0
      ELSE
        LFIXCD = LBNOCD
        JBINI  = -KLKYCD
      ENDIF
      LBKYCD = LFIXCD
      JBIAS  = JBINI
      ND     = IQ(KOFUCD+LBNOCD+MNDNWD)
*
      DO 10 NK = 1, NBANK
        CALL UCOPY  (IQ(KOFUCD+LBNOCD+MNDIOF), IOKYCD, NWNOCD)
        LSAVCD = LBKYCD
        CALL CDBANK (IDIVCD, LBKYCD, LSAVCD, JBIAS, 'KYCD', NLKYCD,
     +               NSKYCD, ND, IOKYCD, NZ, IRC)
        JBIAS  = 0
        IF (IRC.NE.0)  GO TO 999
        LQ(KOFUCD+LBKYCD-KLNOCD) = LBNOCD
        LQ(KOFUCD+LBKYCD-KLUPCD) = LBUPCD
   10 CONTINUE
*
      LBK(1) = LQ(KOFUCD+LFIXCD+JBINI)
*                                                             END CDBOOK
  999 END
