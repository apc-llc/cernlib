*
* $Id: cdrtfz.F,v 1.1.1.1 1996/02/28 16:24:12 mclareni Exp $
*
* $Log: cdrtfz.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:12  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRTFZ (PATHI, LUNFZ, K1MIN, K1MAX, CHOPT, IRC)
*     ==========================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDRTFZ (PATHI, LUNFZ, K1MIN, K1MAX, CHOPT, IRC*)        *
*                                                                      *
*   Copies a part of the directory (in the range K1MIN-K1MAX or with   *
*   insertion time > K1MIN for specified CHOPT) into a sequential file *
*   in the format of the journal file                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHI    Character string describing the input  pathname         *
*     LUNFZ    Logical unit number of the output FZ file               *
*     K1MIN    Minumum serial number (Key 1 value) to be copied, or    *
*              Minimum date (YYMMDD) for insertion time to be copied   *
*              (Depends on CHOPT supplied)                             *
*     K1MAX    Maximum serial number (Key 1 value) to be copied, or    *
*              Minimum time (HHMM) for insertion time to be copied     *
*              (Depends on CHOPT supplied)                             *
*     CHOPT    Character string with any of the following characters   *
*          F   Updates with a fully matched data object (in user keys) *
*          I   Copies objects with insertion time > (K1MIN/K1MAX)      *
*          R   Copied objects with RZ insertion time > (K1MIN/K1MAX)   *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user,   CDEXTR                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 71 : Illegal path name                               *
*                                                                      *
*     If IRC = 0, IQUEST(2) carries information on number of data      *
*     objects transfered to the FZ file                                *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/csavbk.inc"
#include "hepdb/ctpath.inc"
#include "hepdb/czpack.inc"
      PARAMETER       (JBIAS=2)
      CHARACTER       PATHI*(*), CHOPT*(*), PATHN*80, CHOPS*4
      DIMENSION       ITIME(MXPACD)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Suppress blanks from the path name
*
      ICOPY  = 0
      CALL CDOPTS (' ', IRC)
      IF (IRC.NE.0)          GO TO 999
      CALL UOPTC  (CHOPT, 'F', IOPFCA)
      CALL UOPTC  (CHOPT, 'I', IOPI)
      CALL UOPTC  (CHOPT, 'R', IOPR)
      LUFZCF = LUNFZ
      IF (LUFZCF.LE.0)       GO TO 999
      IF (IOPI.NE.0) THEN
        CALL CDPKTM (K1MIN, K1MAX, KY7MI, IRC)
        KYMIN  = 1
      ELSE IF (IOPR.NE.0) THEN
        CALL RZDATE (ICOPY, K1MIN, K1MAX, 3)
        KY7MI  = JBYT (ICOPY, 9, 24)
        KYMIN  = 1
      ELSE
        KY7MI  = 0
        IF (K1MIN.LT.1) THEN
          KYMIN  = 1
        ELSE
          KYMIN  = K1MIN
        ENDIF
        IF (K1MAX.LT.KYMIN) THEN
          KYMAX  = KYMIN + 100000
        ELSE
          KYMAX  = K1MAX
        ENDIF
      ENDIF
*
* *** Load top directory information; gets in PAT1CT complete path name
*
      ICOPY  = 0
      CALL CDLDUP (PATHI, 1, IRC)
      IF (IRC.NE.0)          GO TO 999
      IF (NKEYCK.LE.0)       GO TO 999
      PATHN  = PAT1CT
      NCHRI  = LENOCC (PATHN)
      KST    = NWKYCK + 1
      CALL CDKEYT
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
*
* *** Load IPREC/DELTA from dictionary; choose the transcript file
*
      CALL CDLDIC (PATHN, 1, IRC)
      IF (IRC.NE.0)          GO TO 999
*
* *** Partially fill up the header
*
      NDOP   = 1
      NWDP   = (NCHRI + 3) / 4
      NWHDCF = NWDP + NDOP + NWKYCK + 5
      IHEDCF(MACTCF) = 1
      IHEDCF(MNKYCF) = NWKYCK
      IHEDCF(MOPTCF) = NDOP
      IHEDCF(MPATCF) = NWDP
      CALL UCTOH (PATHN, IHEDCF(MPRECF+NWKYCK+NDOP+1), 4, 4*NWDP)
*
* *** Start reading in records from the input file
*
      NOBJCS = 0
      DO 10 I = 1, NPARCD
        ITIME(I)  = 1
   10 CONTINUE
      IF (IOPTP.EQ.0) THEN
        DO 20 IK = 1, NKEYCK
          IF (KY7MI.EQ.0) THEN
            KEY1   = IQ(KOFSCD+LCDRCD+IKDRCD+(IK-1)*KST+IDHKSN)
            IF (KEY1.LT.KYMIN.OR.KEY1.GT.KYMAX)   GO TO 20
          ELSE
            IF (IOPI.NE.0) THEN
              KEY7   = IQ(KOFSCD+LCDRCD+IKDRCD+(IK-1)*KST+IDHINS)
            ELSE
              LCYC   = IQ(KOFSCD+LCDRCD+IKDRCD+(IK-1)*KST)
              KEY7   = JBYT (IQ(KOFSCD+LCDRCD+LCYC+1), 9, 24)
            ENDIF
            IF (KEY7.LE.KY7MI)                    GO TO 20
          ENDIF
          IOPKCA = 0
          PACKCZ = .FALSE.
          NOBJCS = 1
          KEYSCS(IDHKSN,NOBJCS) = IK
          IOKYCA(IDHKSN) = 1
          CALL CDKXIN (ITIME, IDISCD, LOBJCS(NOBJCS), LOBJCS(NOBJCS),
     +                 JBIAS, NWKEY, KEYSCS(1,NOBJCS), IPREC, IRC)
          IOKYCA(IDHKSN) = 0
          IF (IRC.NE.0)                           GO TO 997
          IF (JBIT(KEYSCS(IDHFLG,NOBJCS),JRZUCD).NE.0) THEN
            IF (JBIT(KEYSCS(IDHFLG,NOBJCS),JASFCD).NE.0) THEN
              CHOPS  = 'HTY'
            ELSE
              CHOPS  = 'HY'
            ENDIF
          ELSE
            CALL CDRZIN (IDISCD, LSTRCL(2), 2, IK, ICYCL, PATHN, IRC)
            IF (IRC.NE.0) THEN
              CALL MZDROP (IDISCD, LSTRCL(2), 'L')
              GO TO 997
            ENDIF
            CALL UCOPY (Q(KOFUCD+LSTRCL(2)+3), IXX, 1)
            IF (IQ(KOFUCD+LSTRCL(2)+1).EQ.0) THEN
              IF (KEYSCS(IDHPTR,NOBJCS).EQ.0) THEN
                CHOPS  = 'H'
              ELSE
                CHOPS  = 'HD'
              ENDIF
            ELSE
              PACKCZ = (JBIT(IXX,32).EQ.0)
              IF (PACKCZ) THEN
                IDTY   = ICDTYP (LSTRCL(2))
                IF (IDTY.EQ.3) THEN
                  PRECCZ =  Q(KOFUCD+LSTRCL(2)+2)
                ELSE
                  PRECCZ = IQ(KOFUCD+LSTRCL(2)+2)
                ENDIF
                IF (KEYSCS(IDHPTR,NOBJCS).EQ.0) THEN
                  CHOPS  = 'HZ'
                ELSE
                  CHOPS  = 'HZD'
                ENDIF
              ELSE
                IF (KEYSCS(IDHPTR,NOBJCS).EQ.0) THEN
                  CHOPS  = 'HP'
                ELSE
                  CHOPS  = 'HPD'
                ENDIF
              ENDIF
            ENDIF
            CALL MZDROP (IDISCD, LSTRCL(2), 'L')
          ENDIF
          CALL CDFZWR (IPREC, CHOPS, IRC)
          CALL MZDROP (IDISCD, LOBJCS(NOBJCS), 'L')
          NOBJCS = 0
          IF (IRC.NE.0)                           GO TO 997
          ICOPY  = ICOPY + 1
   20   CONTINUE
*
      ELSE
*
*   **  Partitioned directory
*
        NKEYS  = NKEYCK
        DO 30 IKK = 1, NKEYS
          KPNT   = IUHUNT (IKK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                     NKEYS*KST, KST)
          IF (KPNT.GT.0) THEN
            KPNT   = KOFSCD + LCDRCD + IKDRCD + KPNT - MPSRCD
          ELSE
            KPNT   = KOFSCD + LCDRCD + IKDRCD + (IKK - 1) * KST
          ENDIF
          IF (KY7MI.EQ.0) THEN
            KYMP   = IQ(KPNT+MOBJCD)
            IF (KYMP.GT.KYMAX)                    GO TO 30
          ENDIF
          CALL CDPATH (TOP1CT, IKK)
          PAT2CT = PATHN(1:NCHRI)//'/'//TOP1CT
          CALL RZCDIR (PAT2CT, ' ')
          IF (IQUEST(1).NE.0) THEN
            IRC    = 71
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRTFZ : Ill'//
     +      'egal Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          DO 25 IK = 1, NKEYCK
            IF (KY7MI.EQ.0) THEN
              KEY1   = IQ(KOFSCD+LCDRCD+IKDRCD+(IK-1)*KST+IDHKSN)
              IF (KEY1.LT.KYMIN.OR.KEY1.GT.KYMAX) GO TO 25
            ELSE
              IF (IOPI.NE.0) THEN
                KEY7   = IQ(KOFSCD+LCDRCD+IKDRCD+(IK-1)*KST+IDHINS)
              ELSE
                LCYC   = IQ(KOFSCD+LCDRCD+IKDRCD+(IK-1)*KST)
                KEY7   = JBYT (IQ(KOFSCD+LCDRCD+LCYC+1), 9, 24)
              ENDIF
              IF (KEY7.LE.KY7MI)                  GO TO 25
            ENDIF
            IOPKCA = 0
            PACKCZ = .FALSE.
            NOBJCS = 1
            KEYSCS(IDHKSN,NOBJCS) = IK
            IOKYCA(IDHKSN) = 1
            CALL CDKXIN (ITIME, IDISCD, LOBJCS(NOBJCS),LOBJCS(NOBJCS),
     +                   JBIAS, NWKEY, KEYSCS(1,NOBJCS), IPREC, IRC)
            IOKYCA(IDHKSN) = 0
            IF (IRC.NE.0)                         GO TO 997
            IF (JBIT(KEYSCS(IDHFLG,NOBJCS),JRZUCD).NE.0) THEN
              IF (JBIT(KEYSCS(IDHFLG,NOBJCS),JASFCD).NE.0) THEN
                CHOPS  = 'HTY'
              ELSE
                CHOPS  = 'HY'
              ENDIF
            ELSE
              CALL CDRZIN (IDISCD, LSTRCL(2), 2,IK, ICYCL, PAT2CT,IRC)
              IF (IRC.NE.0) THEN
                CALL MZDROP (IDISCD, LSTRCL(2), 'L')
                GO TO 997
              ENDIF
              CALL UCOPY (Q(KOFUCD+LSTRCL(2)+3), IXX, 1)
              IF (IQ(KOFUCD+LSTRCL(2)+1).EQ.0) THEN
                IF (KEYSCS(IDHPTR,NOBJCS).EQ.0) THEN
                  CHOPS  = 'H'
                ELSE
                  CHOPS  = 'HD'
                ENDIF
              ELSE
                PACKCZ = (JBIT(IXX,32).EQ.0)
                IF (PACKCZ) THEN
                  IDTY   = ICDTYP (LSTRCL(2))
                  IF (IDTY.EQ.3) THEN
                    PRECCZ =  Q(KOFUCD+LSTRCL(2)+2)
                  ELSE
                    PRECCZ = IQ(KOFUCD+LSTRCL(2)+2)
                  ENDIF
                  IF (KEYSCS(IDHPTR,NOBJCS).EQ.0) THEN
                    CHOPS  = 'HZ'
                  ELSE
                    CHOPS  = 'HZD'
                  ENDIF
                ELSE
                  IF (KEYSCS(IDHPTR,NOBJCS).EQ.0) THEN
                    CHOPS  = 'HP'
                  ELSE
                    CHOPS  = 'HPD'
                  ENDIF
                ENDIF
              ENDIF
              CALL MZDROP (IDISCD, LSTRCL(2), 'L')
            ENDIF
            CALL CDFZWR (IPREC, CHOPS, IRC)
            CALL MZDROP (IDISCD, LOBJCS(NOBJCS), 'L')
            NOBJCS = 0
            IF (IRC.NE.0)                         GO TO 997
            ICOPY  = ICOPY + 1
   25     CONTINUE
*
          CALL RZCDIR (PATHN, ' ')
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
   30   CONTINUE
*
      ENDIF
*
* *** Drop the stored banks
*
  997 IF (NOBJCS.GT.0) THEN
        DO 998 IOBJ = 1, NOBJCS
          CALL MZDROP (IDISCD, LOBJCS(IOBJ), 'L')
  998   CONTINUE
        NOBJCS = 0
      ENDIF
  999 IQUEST(2) = ICOPY
*                                                             END CDRTFZ
      END
