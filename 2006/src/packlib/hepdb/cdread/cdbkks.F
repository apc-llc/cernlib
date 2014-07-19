*
* $Id: cdbkks.F,v 1.1.1.1 1996/02/28 16:24:25 mclareni Exp $
*
* $Log: cdbkks.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:25  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDBKKS (KEYS, LBK, ITIME, IRC)
*     =========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDBKKS (KEYS, LBK*, ITIME, IRC*)                        *
*                                                                      *
*   Creates or completes the Key banks supported as next of same type  *
*   to the Node bank with S option                                     *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     KEYS     Vector of keys                                          *
*     LBK      Address(es) of Keys bank(s) KYCD                        *
*     ITIME    Time for which the valid keys are required              *
*              (in option S)                                           *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDKEYB, CDKYDB, CDUSEDB,CDUSEM                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 24 : No Key bank created satisfying key options for  *
*                      option S                                        *
*               = 25 : Illegal Path Name                               *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctkxin.inc"
#include "hepdb/ctpath.inc"
      PARAMETER       (NZ=0)
      DIMENSION       KEYS(9), LBK(9), ITIME(9)
      CHARACTER       PATHN*80
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      IRC    = 0
*
*  ** Start from the end of the existing chain
*
      IF (LQ(KOFUCD+LBNOCD-KLKYCD).NE.0) THEN
        LFIXCD = LZLAST (IDIVCD, LQ(KOFUCD+LBNOCD-KLKYCD))
        IF (LFIXCD.EQ.0) LFIXCD = LQ(KOFUCD+LBNOCD-KLKYCD)
      ELSE
        LFIXCD = LBNOCD
      ENDIF
*
*  ** Loop over all keys
*
      IF (NKEYCK.LE.0)  THEN
        IRC    = 24
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDBKKS : No comp'//
     +  'atible object for specified options'')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      LSAVCD = LFIXCD
      CALL CDKEYT
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
*
      IF (IOPTP.EQ.0)  THEN
        CALL CDKYSE (KEYS, ITIME, IRC)
      ELSE
        CALL RZCDIR (PATHN, 'R')
        MAXL    = LENOCC (PATHN)
        NKEYS   = NKEYCK
        KST     = NWKYCK + 1
*
*  **   Loop over all subdirectories
*
        DO 10 IK = 1, NKEYS
          ICURCT = NKEYS + 1 - IK
*
*  *      Fast selection
*
          IF (IK.NE.1) THEN
            CALL RZCDIR (PATHN, ' ')
            IF (IQUEST(1).NE.0)  THEN
              IRC    = 25
#if defined(CERNLIB__DEBUG)
              IF (IDEBCD.NE.0) CALL CDPRNT (LPRTCD, '(/,'' CDBKKS : I'//
     +        'llegal Path name '//PATHN//''')', IARGCD, 0)
#endif
              GO TO 999
            ENDIF
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            NWKYCK = IQUEST(8)
          ENDIF
*
          KPNT   = IUHUNT (ICURCT, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                     NKEYS*KST, KST)
          IF (KPNT.NE.0) THEN
            IPNT   = KOFSCD + LCDRCD + IKDRCD + KPNT - MPSRCD
          ELSE
            IPNT   = KOFSCD + LCDRCD + IKDRCD + (ICURCT - 1) * KST
          ENDIF
          CALL CDPSEL (ITIME, KEYS, IQ(IPNT+1), 0, ISEL)
          IF (ISEL.NE.0)                                    GO TO 10
*
          CALL CDPATH (TOP2CT, ICURCT)
          PAT2CT = PATHN(1:MAXL)//'/'//TOP2CT
          CALL RZCDIR (PAT2CT, ' ')
          IF (IQUEST(1).NE.0)  THEN
            IQUEST(1) = 25
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.NE.0) CALL CDPRNT (LPRTCD, '(/,'' CDBKKS : Ill'//
     +      'egal Path name '//PAT2CT//''')', IARGCD, 0)
#endif
            GO TO 30
          ENDIF
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          NKEYCK = IQUEST(7)
          NWKYCK = IQUEST(8)
          CALL CDKEYT
          CALL CDKYSE (KEYS, ITIME, IRC)
          IF (IRC.NE.0)                                     GO TO 30
   10   CONTINUE
      ENDIF
*
* *** Discard all keys with discard bit set
*
      LSAVCD = LFIXCD
      IF (LFIXCD.EQ.LBNOCD) THEN
        LBKYCD = LQ(KOFUCD+LSAVCD-KLKYCD)
      ELSE
        LBKYCD = LQ(KOFUCD+LSAVCD)
      ENDIF
   15 IF (LBKYCD.NE.0)  THEN
        IF (JBIT(IQ(KOFUCD+LBKYCD+IDHFLG),JIGNCD).NE.0)  THEN
          CALL MZDROP (IDIVCD, LBKYCD, ' ')
        ELSE
          LSAVCD = LBKYCD
          IF (IPRBCA.EQ.0.AND.IPRECA.EQ.0) THEN
            IF (IHFLCD.EQ.0) THEN
              KYEN   = 0
              DO 20 I = 1, NPARCD
                IF (KYEN.EQ.0) THEN
                  IF (IQ(KOFUCD+LBKYCD+NOF1CK+2*I).LT.KYENCD(I)) KYEN=-1
                  IF (IQ(KOFUCD+LBKYCD+NOF1CK+2*I).GT.KYENCD(I)) KYEN= 1
                ENDIF
   20         CONTINUE
              DO I = 1, NPARCD
                IF (KYEN.LT.0) THEN
                  IQ(KOFUCD+LBKYCD+NWKYCK+I) =
     +                      IQ(KOFUCD+LBKYCD+NOF1CK+2*I)
                ELSE
                  IQ(KOFUCD+LBKYCD+NWKYCK+I) = KYENCD(I)
                ENDIF
              ENDDO
            ELSE
              DO 25 I = 1, NPARCD
                IQ(KOFUCD+LBKYCD+NWKYCK+I) =
     +          MIN (IQ(KOFUCD+LBKYCD+NOF1CK+2*I), KYENCD(I))
   25         CONTINUE
            ENDIF
          ENDIF
        ENDIF
        LBKYCD = LQ(KOFUCD+LSAVCD)
        GO TO 15
      ENDIF
*
* *** Reset the current directory if needed
*
   30 IF (IOPTP.NE.0)  THEN
        CALL RZCDIR (PATHN, ' ')
        IF (IQUEST(1).NE.0)  THEN
          IF (IRC.EQ.0)  THEN
            IRC    = 25
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.NE.0) CALL CDPRNT (LPRTCD, '(/,'' CDBKKS : Ill'//
     +      'egal Path name '//PATHN//''')', IARGCD, 0)
#endif
          ENDIF
        ELSE
          NKEYCK = IQUEST(7)
          NWKYCK = IQUEST(8)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          CALL CDKEYT
        ENDIF
      ENDIF
*
      IF (LFIXCD.EQ.LBNOCD) THEN
        LBK(1) = LQ(KOFUCD+LFIXCD-KLKYCD)
      ELSE
        LBK(1) = LQ(KOFUCD+LFIXCD)
      ENDIF
      IF (LBK(1).EQ.0)  THEN
        IRC    = 24
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDBKKS : No comp'//
     +  'atible object for specified options'')', IARGCD, 0)
#endif
      ENDIF
*                                                             END CDBKKS
  999 END
