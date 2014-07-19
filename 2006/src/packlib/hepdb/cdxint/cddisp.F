*
* $Id: cddisp.F,v 1.1.1.1 1996/02/28 16:24:48 mclareni Exp $
*
* $Log: cddisp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:48  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDDISP (LUN, PATHN, CHOPT, IRC)
*     ==========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDDISP (LUN, PATHN, *CHOPT*, IRC*)                      *
*                                                                      *
*   Displays objects attached to the directory PATHN                   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Unit number of file for display                         *
*     PATHN    Character string describing the pathname                *
*     CHOPT    Character with any of the follwing characters           *
*          K   Display all the keys (Default)                          *
*          U   Display only user keys and validities                   *
*          I   Display template to type in new keys (Return value)     *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDAUXI, CDEDKY, CDPEEK                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =101 : Illegal path name                               *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ccdisp.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHN*(*), CHOPT*(*), PATHY*80, CFORM*67
      DATA            MAXLN /1000/
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Set the current directory
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)           GO TO 999
      PATHY  = PAT1CT
      CALL CDKYTG
      CALL CDCONC (IOTYCK(1), NWKYCK, ICONCK(1), NSKPCK)
      CALL UCOPY  (IOTYCK(1), IOTYCC(1), NWKYCK)
*
* *** Check the number of keys
*
      IF (NKEYCK.EQ.0) THEN
        IFLG   = 1
      ELSE
        IFLG   = 0
        MINKY  = 1
        IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
        IF (IOPTP.EQ.0) THEN
          MAXKY  = NKEYCK
        ELSE
          KST    = NWKYCK + 1
          KPNT   = IUHUNT (NKEYCK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                     NKEYCK*KST, KST)
          IF (KPNT.GT.0) THEN
            IPNT   = KOFSCD + LCDRCD + IKDRCD + KPNT - MPSRCD
          ELSE
            IPNT   = KOFSCD + LCDRCD + IKDRCD + (NKEYCK - 1) * KST
          ENDIF
          KOBJ   = IQ(IPNT+MOBJCD)
          CALL CDPATH (TOP2CT, NKEYCK)
          CALL RZCDIR (TOP2CT, ' ')
          IF (IQUEST(1).NE.0) GO TO 991
          NKEYS  = IQUEST(7)
          MAXKY  = KOBJ + NKEYS
          IF (MAXKY.LE.0) IFLG = 1
          CALL RZCDIR (PATHY, ' ')
          IF (IQUEST(1).NE.0) GO TO 991
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
        ENDIF
        IF (IOPHCC.NE.0) THEN
          LENOB  = MAXKY
        ELSE
          LENOB  = MAXKY * NWKYCK
        ENDIF
        IF (LENOB.GT.MAXLN) THEN
          WRITE (CFORM, 1000) MAXKY
          CALL KUPROI (CFORM, MINKY)
          IF (IOPHCC.NE.0) THEN
            MAXK   = MINKY + MAXLN
          ELSE
            MAXK   = MINKY + MAXLN/NWKYCK
          ENDIF
          MAXKY = MIN0 (MAXKY, MAXK)
        ENDIF
      ENDIF
*
* *** Now display the keys
*
      IF (IOPHCC.NE.0) THEN
        CALL CDDISH (LUN, PATHN, MINKY, MAXKY, IFLG, CHOPT, IRC)
      ELSE
        CALL CDDISV (LUN, PATHN, MINKY, MAXKY, IFLG, CHOPT, IRC)
      ENDIF
      IRC    = 0
      IF (IFLG.EQ.1) CHOPT = 'I'
      GO TO 999
*
* *** Error messages
*
  991 IRC    = 101
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.0) THEN
        CALL CDPRNT (LPRTCD, '(/,'' CDDISP : Illegal Path Name '//PATHY
     +       //''')', IARGCD, 0)
      ENDIF
#endif
*
 1000 FORMAT ('Directory too Long --',I10,' Objects - First Object to'
     +,' Display ?')
*                                                             END CDDISP
  999 END
