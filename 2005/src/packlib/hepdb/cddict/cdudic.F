*
* $Id: cdudic.F,v 1.1.1.1 1996/02/28 16:24:10 mclareni Exp $
*
* $Log: cdudic.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:10  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDUDIC (IRC)
*     =======================
*
************************************************************************
*                                                                      *
*        SUBR. CDUDIC (IRC*)                                           *
*                                                                      *
*   Updates the dictionary information if not yet available in the     *
*   disk file                                                          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDINIT                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =140 : Illegal Top directory name                      *
*               =141 : Error in creating the DICTIONARY/HELP directory *
*               =142 : Error in RZ in writing the dictionary object    *
*               =143 : Error in RZ in purging the dictionary directory *
*                                                                      *
************************************************************************
*
#include "hepdb/caopti.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHD*80, CFOR*32
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*     ------------------------------------------------------------------
*
      IRC    = 0
      IOUT   = IOUTCD
      NSYSCK = NOF2CK + 2*NPARCD
      IF (IOPPCD.NE.0) IOUT = 0
      PAT1CT = '//'//TOPNCD
      NCHR   = NCHRCD + 2
      PATHD  = PAT1CT(1:NCHR)//'/DICTIONARY'
      CALL RZCDIR (PAT1CT, ' ')
      IF (IQUEST(1).NE.0) THEN
        IRC    = 140
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUDIC : Illegal'//
     +  ' top directory '//PAT1CT//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
#if !defined(CERNLIB__P3CHILD)
*
* *** Create the help file if it does not exist
*
      IF (IOUT.NE.0) THEN
        PAT2CT = PAT1CT(1:NCHR)//'/HELP'
        CALL RZCDIR (PAT2CT, 'Q')
        IF (IQUEST(1).NE.0) THEN
          CFOR   = CHFTCK(1:NSYSCK)
          DO I = 1, NSYSCK
            CTAGCK(I) = CHTGCK(I)
          ENDDO
          DO I = 1, NPARCD
            CFOR(NOF1CK+2*I-1:NOF1CK+2*I-1) = 'I'
            CFOR(NOF1CK+2*I  :NOF1CK+2*I  ) = 'I'
            CTAGCK(NOF1CK+2*I-1) = 'STR_VAL'//CALFCA(27+I)
            CTAGCK(NOF1CK+2*I)   = 'END_VAL'//CALFCA(27+I)
          ENDDO
          CALL RZCDIR (PAT1CT, ' ')
          IF (IOPSCD.NE.0) CALL RZLOCK ('CDUDIC')
          CALL RZMDIR ('HELP', NSYSCK, CFOR, CTAGCK)
          IERR   = IQUEST(1)
          IF (IOPSCD.NE.0) THEN
            CALL RZCDIR (PAT1CT, ' ')
            CALL RZFREE ('CDUDIC')
          ENDIF
          IF (IERR.NE.0) THEN
            IRC    = 141
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUDIC : RZM'//
     +      'DIR error for path name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
            GO TO 999
          ENDIF
        ENDIF
      ENDIF
#endif
*
* *** Try to load the dictionary information if it exists
*
      CALL RZCDIR (PATHD, 'Q')
      IF (IQUEST(1).NE.0) THEN
        IQUEST(1) = 0
#if !defined(CERNLIB__P3CHILD)
        IF (IOUT.NE.0) THEN
          CFOR   = CHFTCK(1:NSYSCK)
          DO I = 1, NSYSCK
            CTAGCK(I) = CHTGCK(I)
          ENDDO
          DO I = 1, NPARCD
            CFOR(NOF1CK+2*I-1:NOF1CK+2*I-1) = 'I'
            CFOR(NOF1CK+2*I  :NOF1CK+2*I  ) = 'I'
            CTAGCK(NOF1CK+2*I-1) = 'STR_VAL'//CALFCA(27+I)
            CTAGCK(NOF1CK+2*I)   = 'END_VAL'//CALFCA(27+I)
          ENDDO
          CALL RZCDIR (PAT1CT, ' ')
          IF (IOPSCD.NE.0) CALL RZLOCK ('CDUDIC')
          CALL RZMDIR ('DICTIONARY', NSYSCK, CFOR, CTAGCK)
          IERR   = IQUEST(1)
          IF (IOPSCD.NE.0) THEN
            CALL RZCDIR (PAT1CT, ' ')
            CALL RZFREE ('CDUDIC')
          ENDIF
          IF (IERR.NE.0) THEN
            IRC    = 141
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUDIC : RZM'//
     +      'DIR error for path name '//PATHD//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
            GO TO 999
          ENDIF
        ENDIF
#endif
        GO TO 5
      ENDIF
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      ISTP   = NWKYCK + 1
      IF (NKEYCK.GT.0) THEN
        IPNT   = KOFSCD + LCDRCD + IKDRCD
        IMIN   = IUHUNT (-1, IQ(IPNT+IDHKSN), NKEYCK*ISTP, ISTP)
        IF (IMIN.GT.0) THEN
          IMIN   = (IMIN - IDHKSN) / ISTP + 1
          CALL CDKEYT
          CALL CDKEYR (IMIN, NWKYCK, KEYNCK)
          LFIXCD = LQ(KOFUCD+LBUPCD-KLDICD)
          IF (LFIXCD.NE.0) CALL MZDROP (IDIVCD, LFIXCD, ' ')
          CALL CDRZIN (IDIVCD, LBUPCD, -KLDICD, IMIN, ICYCL, PATHD, IRC)
          IF (IRC.EQ.0)                                    GO TO 999
          IOUT   = 0
        ENDIF
      ENDIF
*
    5 CALL VZERO (KEYNCK, NSYSCK)
      KEYNCK(IDHKSN) = -1
      KEYNCK(IDHFLG) = 1
      CALL DATIME (IDATE, ITIME)
      CALL CDPKTM (IDATE, ITIME, KEYNCK(IDHINS), IRC)
      CALL CDMDIC (TOPNCD, LBUPCD, -KLDICD, IRC)
      IF (IRC.NE.0)                                        GO TO 999
      LFIXCD = LQ(KOFUCD+LBUPCD-KLDICD)
#if !defined(CERNLIB__P3CHILD)
*
* *** All subdirectories looked at; now store dictionary if permitted
*
      IF (IOUT.NE.0) THEN
        CALL RZCDIR (PATHD, ' ')
        LCDRCD = IQUEST(11)
        IF (IOPSCD.NE.0) CALL RZLOCK ('CDUDIC')
        CALL RZOUT (IDIVCD, LFIXCD, KEYNCK, ICYCLE, 'S')
        IKDRCD = IQ(KOFSCD+LCDRCD+KLKDCD)
        NKEYCK = IQ(KOFSCD+LCDRCD+KNKDCD)
        IF (IQUEST(1).NE.0) THEN
          IF (IOPSCD.NE.0) CALL RZFREE ('CDUDIC')
          IRC    = 142
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUDIC : RZOUT'//
     +    ' error for path name '//PATHD//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 999
        ENDIF
        CALL RZPURG (0)
        IERR   = IQUEST(1)
        IF (IOPSCD.NE.0) CALL RZFREE ('CDUDIC')
        IF (IERR.NE.0) THEN
          IRC    = 143
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUDIC : RZPUR'//
     +    'G error for path name '//PATHD//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 999
        ENDIF
      ENDIF
#endif
*
  100 IRC    = 0
*                                                             END CDUDIC
  999 END
