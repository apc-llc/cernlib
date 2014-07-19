*
* $Id: cdlast.F,v 1.1.1.1 1996/02/28 16:24:41 mclareni Exp $
*
* $Log: cdlast.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:41  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      LOGICAL FUNCTION CDLAST (LBK, IRC)
*     ==================================
*
************************************************************************
*                                                                      *
*        FUNC. CDLAST (LBK, IRC*)                                      *
*                                                                      *
*   Checks if the Key bank corresponds to the last inserted object     *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LBK      Address of the key bank                                 *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =131 : Illegal pathname in the Key bank                *
*               =132 : Illegal number of keys in the directory         *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHN*255
      DIMENSION       LBK(9)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Get the path name from the key bank
*
      LREFCD(1) = LBK(1)
      CDLAST = .FALSE.
      LBNOCD = LQ(KOFUCD+LREFCD(1)-KLNOCD)
      NCH    = IQ(KOFUCD+LBNOCD+MNDNCH)
      CALL UHTOC (IQ(KOFUCD+LBNOCD+MNDNAM), 4, PATHN, NCH)
      PATHN  = PATHN(1:NCH)
*
* *** Set the current directory
*
      CALL RZCDIR (PATHN, ' ')
      IF (IQUEST(1).NE.0) THEN
        IRC    = 131
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDLAST : Ill'//
     +  'egal path name '//PATHN//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      IF (IQUEST(7).LE.0) THEN
        IRC    = 132
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDLAST : Illegal'//
     +  ' number of keys for '//PATHN//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      IPNT   = KOFSCD + LCDRCD + IKDRCD
      ISTP   = NWKYCK + 1
      IOPTP  = JBIT (IQ(IPNT+IDHFLG), JPRTCD)
*
* *** Take different action for partitioned and non-partitioned cases
*
      IF (IOPTP.EQ.0) THEN
        KPNT   = IPNT + (NKEYCK-1) * ISTP
        KMAX   = IQ(KPNT+IDHKSN)
        DO 10 I = 2, NKEYCK
          KPNT   = KPNT - ISTP
          IF (IQ(KPNT+IDHKSN).GT.KMAX) KMAX = IQ(KPNT+IDHKSN)
   10   CONTINUE
      ELSE
*
        KMAX   = 0
        DO 20 IK = 1, NKEYCK
          JK     = NKEYCK + 1 - IK
          CALL CDPATH (TOP1CT, JK)
          PATHN  = PATHN(1:NCH)//'/'//TOP1CT
          CALL RZCDIR (PATHN, ' ')
          IF (IQUEST(1).NE.0) THEN
            IRC    = 131
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDLAST : Ill'//
     +      'egal path name '//PATHN//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          NKEYS  = IQUEST(7)
          IF (NKEYS.GT.0) THEN
            KPNT   = KOFSCD + IQUEST(11) + IQUEST(13) + (NKEYS-1) * ISTP
            KMAX   = IQ(KPNT+IDHKSN)
            DO 15 I = 2, NKEYS
              KPNT   = KPNT - ISTP
              IF (IQ(KPNT+IDHKSN).GT.KMAX) KMAX = IQ(KPNT+IDHKSN)
   15       CONTINUE
            GO TO 30
          ENDIF
   20   CONTINUE
      ENDIF
*
* *** Now compare last Key 1 with that in the Key bank
*
   30 IF (IQ(KOFUCD+LREFCD(1)+IDHKSN).EQ.KMAX) CDLAST = .TRUE.
      IRC    = 0
*                                                             END CDLAST
  999 END
