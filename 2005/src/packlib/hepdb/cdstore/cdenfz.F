*
* $Id: cdenfz.F,v 1.1.1.1 1996/02/28 16:24:33 mclareni Exp $
*
* $Log: cdenfz.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:33  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if !defined(CERNLIB__P3CHILD)
      SUBROUTINE CDENFZ (PATHN, LUNFZ, IRC)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDENFZ (PATHN, LUNFZ, IRC*)                             *
*                                                                      *
*   Enters data objects using either data residing in a FZ file or     *
*   data hung at address LFIXCD                                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LUNFZ    Logical unit number of the FZ file                      *
*              or 0 if data does not reside on FZ file                 *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDFZUP                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 71 : Illegal path name in the transcript file        *
*               = 72 : Read error on the FZ file                       *
*               = 73 : Error in RZ for saving the data object          *
*               = 74 : Error in RZ for renaming the keys               *
*               = 77 : FZOUT fails to write on to the sequential file  *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/cinitl.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctpath.inc"
#include "hepdb/czpack.inc"
      PARAMETER       (JBIAS=2)
      CHARACTER       PATHN*(*), PATHX*16, CHFOR*100, CHOP*1
      CHARACTER       CFORM(6)*1, CHOPT*4
      DATA            CFORM / 'B', 'I', 'F', 'D', 'H', 'A' /
#include "zebra/q_jbit.inc"
* Ignoring t=pass
#include "zebra/q_sbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      NCH    = LENOCC (PATHN)
*
* *** Load IPREC/DELTA from dictionary; choose the transcript file
*
      CALL CDLDIC (PATHN, 1, IRC)
      IF (IRC.NE.0)                      GO TO 999
      IF (ICMPCD.EQ.0) THEN
        IOPYCA = 1
        PACKCZ = .FALSE.
      ELSE IF (ICMPCD.EQ.2.AND.IOPZCA.NE.0) THEN
        PACKCZ = .TRUE.
        PRECCZ = DELTCD
      ELSE
        PACKCZ = .FALSE.
      ENDIF
*
* *** Check if the object already exists in the data base
*
      IPNT   = KOFSCD + LCDRCD + IKDRCD
      ISTP   = NWKYCK + 1
      IF (NKEYCK.GT.0) THEN
        IOPTP  = JBIT (IQ(IPNT+IDHFLG), JPRTCD)
      ELSE
        IOPTP  = 0
      ENDIF
      KEY1   = IHEDCF(MPRECF+IDHKSN)
      IF (IOVRCD.NE.0) KEY1 = 0
      IF (IOPTP.EQ.0) THEN
        IF (KEY1.GT.0) THEN
          IFND   = IUHUNT (KEY1, IQ(IPNT+IDHKSN), NKEYCK*ISTP, ISTP)
          IF (IFND.NE.0)                 GO TO 998
        ENDIF
        KOBJ   = 0
        PAT2CT = PATHN
      ELSE
        MXKP   = IQ(IPNT+MXKPCD)
        NKEYS  = NKEYCK
        CALL CDKYTG
        DO 15 JK = 1, NKEYS
          IK     = NKEYS + 1 - JK
          KPNT   = IUHUNT (IK, IQ(IPNT+MPSRCD), NKEYS*ISTP, ISTP)
          IF (KPNT.NE.0) THEN
            KPNT   = IPNT + KPNT - MPSRCD
          ELSE
            KPNT   = IPNT + (IK - 1) * ISTP
          ENDIF
          IF (KEY1.GT.0) THEN
            IF (IQ(KPNT+MOBJCD).GE.KEY1) GO TO 15
          ENDIF
*
*  **     Get the appropriate sub-directory
*
          NK     = (KPNT - IPNT) / ISTP + 1
          CALL CDKEYR (NK, NWKYCK, KYP1CK)
          CALL CDPATH (PATHX, IK)
          PAT2CT = PATHN(1:NCH)//'/'//PATHX
          CALL RZCDIR (PAT2CT, ' ')
          IF (IQUEST(1).NE.0) THEN
            IRC    = 71
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDENFZ : Ill'//
     +      'egal path name '//PAT2CT//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          NKEYCK = IQUEST(7)
          NWKYCK = IQUEST(8)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          IPNT   = KOFSCD + LCDRCD + IKDRCD
          IF (KEY1.NE.0) THEN
            IFND   = IUHUNT (KEY1, IQ(IPNT+IDHKSN), NKEYCK*ISTP, ISTP)
            IF (IFND.NE.0) THEN
              PAT2CT = PATHN(1:NCH)
              CALL RZCDIR (PAT2CT, ' ')
              IF (IQUEST(1).NE.0) THEN
                IRC    = 71
                GO TO 999
              ENDIF
              NKEYCK = IQUEST(7)
              NWKYCK = IQUEST(8)
              LCDRCD = IQUEST(11)
              IKDRCD = IQUEST(13)
              GO TO 998
            ENDIF
          ENDIF
*
*  **     Create a new subdirectory if needed
*
          IF (IOPPCD.EQ.0.AND.NKEYCK.GE.MXKP) THEN
            IF (IK.EQ.NKEYS) THEN
              CHFOR  = ' '
              IF (KEY1.NE.0) THEN
                KEY7CK = IHEDCF(MPRECF+IDHINS)
              ELSE
                KEY7CK = 0
              ENDIF
              DO 10 I = 1, NWKYCK
                IF (I.EQ.1) THEN
                  CHFOR  = CFORM(IOTYCK(I))
                ELSE
                  CHFOR  = CHFOR(1:I-1)//CFORM(IOTYCK(I))
                ENDIF
   10         CONTINUE
              IF (ICMPCD.EQ.2) THEN
                CHOPT  = 'ZP'
              ELSE IF (ICMPCD.NE.0) THEN
                CHOPT = 'CP'
              ELSE
                CHOPT = 'P '
              ENDIF
              CALL CDMKDI (PATHN, NWKYCK, CHFOR, CTAGCK, MXKP, IPRECD,
     +                     DELTCD, CHOPT, IRC)
              IF (IRC.NE.0)              GO TO 999
              CALL RZCDIR (PATHN, ' ')
              IF (IQUEST(1).NE.0) THEN
                IRC    = 71
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.0) THEN
                  PAT2CT = PATHN
                  CALL CDPRNT (LPRTCD, '(/,'' CDENFZ : Illegal path '//
     +                 'name '//PAT2CT//''')', IARGCD, 0)
                ENDIF
#endif
                GO TO 999
              ENDIF
              NKEYCK = IQUEST(7)
              NWKYCK = IQUEST(8)
              LCDRCD = IQUEST(11)
              IKDRCD = IQUEST(13)
              IPNT   = KOFSCD + LCDRCD + IKDRCD
              NK     = IUHUNT (NKEYCK, IQ(IPNT+MPSRCD),NKEYCK*ISTP,ISTP)
              IF (NK.GT.0) THEN
                NK     = (NK - MPSRCD) / ISTP + 1
              ELSE
                NK     = NKEYCK
              ENDIF
              CALL CDKEYR (NK, NWKYCK, KYP1CK)
              CALL CDPATH (PATHX, NKEYCK)
              PAT2CT = PATHN(1:NCH)//'/'//PATHX
              CALL RZCDIR (PAT2CT, ' ')
              IF (IQUEST(1).NE.0) THEN
                IRC    = 71
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDENFZ :'//
     +          ' Illegal path name '//PAT2CT//''')', IARGCD, 0)
#endif
                GO TO 999
              ENDIF
              NKEYCK = IQUEST(7)
              NWKYCK = IQUEST(8)
              LCDRCD = IQUEST(11)
              IKDRCD = IQUEST(13)
            ENDIF
          ENDIF
          GO TO 20
   15   CONTINUE
   20   CALL UCOPY (KYP1CK, KYP2CK, NWKYCK)
        KOBJ   = KYP1CK(MOBJCD)
      ENDIF
*
      IF (KEY1.LE.0.AND.IOPPCD.EQ.0) THEN
        IF (NKEYCK.GT.0) THEN
          DO 25 IK = 1, NKEYCK
            IP     = KOFSCD + LCDRCD + IKDRCD + (IK-1)*ISTP + IDHKSN
            IF (IQ(IP).GT.KOBJ) KOBJ = IQ(IP)
   25     CONTINUE
        ENDIF
        KOBJ   = KOBJ + 1
        IHEDCF(MPRECF+IDHKSN) = KOBJ
      ENDIF
      IHEDCF(MPRECF+IDHPTR) = 0
      IHEDCF(MPRECF+IDHFLG) = MSBIT0 (IHEDCF(MPRECF+IDHFLG), JRZUCD)
      IHEDCF(MPRECF+IDHFLG) = MSBIT0 (IHEDCF(MPRECF+IDHFLG), JPRTCD)
      IHEDCF(MPRECF+IDHFLG) = MSBIT0 (IHEDCF(MPRECF+IDHFLG), JASFCD)
      IF (IOPICA.EQ.0) THEN
        IHEDCF(MPRECF+IDHFLG) = MSBIT0 (IHEDCF(MPRECF+IDHFLG), JIGNCD)
      ELSE
        IHEDCF(MPRECF+IDHFLG) = MSBIT1 (IHEDCF(MPRECF+IDHFLG), JIGNCD)
      ENDIF
      IF (IOPHCA.EQ.0.AND.IHEDCF(MPRECF+IDHINS).LE.0) THEN
        CALL DATIME (IDATE, ITIME)
        CALL CDPKTM (IDATE, ITIME, IHEDCF(MPRECF+IDHINS), IRC)
      ENDIF
*
* *** Now read the data part of the transcript file
*
      IF (LUNFZ.GT.0) THEN
        CALL FZIN (LUNFZ, IDISCD, LFIXCD, JBIAS, 'A', 0, 0)
        IF (IQUEST(1).GT.0) THEN
          IRC        = -1
          GO TO 999
        ELSE IF (IQUEST(1).NE.0) THEN
          IQUEST(11) = IQUEST(1)
          IRC        = 72
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDENFZ : FZ er'//
     +    'ror type '',I12)', IQUEST(11), 1)
#endif
          GO TO 999
        ENDIF
      ENDIF
*
      IDB    = ICDTYP (LFIXCD)
      IF (IDB.EQ.2.OR.IDB.EQ.3) THEN
        IOPTR  = 0
      ELSE
        IOPTR  = 1
      ENDIF
*
      IF (IOPYCA.NE.0 .OR. IOPTR.NE.0 .OR. IOPTCA.NE.0) THEN
        IF (IOPTCA.NE.0) THEN
          CHOP   = 'S'
        ELSE IF (IOPYCA.NE.0) THEN
          CHOP   = 'L'
        ELSE
          CHOP   = ' '
        ENDIF
      ELSE
        CHOP   = 'S'
      ENDIF
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
      IF (IOPPCD.NE.0) THEN
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(defined(CERNLIB__ONLINE))
        CALL CDWLOK (IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__ONLINE))
        CALL CDSTSV (TOPNCD, 0, IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        IF (IRC.NE.0) THEN
          CALL MZDROP (IDISCD, LFIXCD, 'L')
          GO TO 999
        ENDIF
      ENDIF
#endif
*
*  ** Write the sequential output if needed
*
      IF (LUFZCF.GT.0) THEN
        CALL FZOUT (LUFZCF, IDISCD, LFIXCD, 1, CHOP, IOFMCF, NWHDCF,
     +              IHEDCF)
        IF (IQUEST(1).NE.0) THEN
          CALL MZDROP (IDISCD, LFIXCD, 'L')
          IRC    = 77
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) THEN
            PAT2CT = PATHN
            CALL CDPRNT (LPRTCD, '(/,'' CDENFZ : Error in FZOUT for'//
     +           ' '//PAT2CT//''')', IARGCD, 0)
          ENDIF
#endif
          GO TO 999
        ENDIF
      ENDIF
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
      IF (IOPPCD.NE.0) THEN
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(defined(CERNLIB__ONLINE))
        ISAVW  = IQUEST(9)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        CALL MZDROP (IDISCD, LFIXCD, 'L')
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(defined(CERNLIB__ONLINE))
        IQUEST(9) = ISAVW
        CALL CDCWSV (IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        GO TO 999
      ENDIF
#endif
*
* *** Compress the data if needed
*
      IF (IOPYCA.NE.0 .OR. IOPTR.NE.0 .OR. IOPTCA.NE.0) THEN
        IHEDCF(MPRECF+IDHFLG) = MSBIT1 (IHEDCF(MPRECF+IDHFLG), JRZUCD)
        IF (IOPTCA.NE.0)
     +    IHEDCF(MPRECF+IDHFLG) = MSBIT1 (IHEDCF(MPRECF+IDHFLG), JASFCD)
      ELSE
        LBDACD = LFIXCD
        LFIXCD = 0
        IF (IQ(KOFUCD+LBDACD-1).EQ.0) THEN
          IRSET  = 1
          IOPPS  = IOPPCA
          IOPZS  = IOPZCA
          IOPPCA = 0
          IOPZCA = 0
        ELSE
          IRSET  = 0
        ENDIF
        CALL CDFRUS (LBDACD, LSTRCL(1), IPRECD, IRC)
        CALL MZDROP (IDISCD, LBDACD, 'L')
        IF (IRC.NE.0) THEN
          IF (IRSET.NE.0) THEN
            IOPPCA = IOPPS
            IOPZCA = IOPZS
          ENDIF
          GO TO 999
        ENDIF
        IF (IOPPCA.EQ.0.AND.IOPZCA.EQ.0) THEN
          LFIXCD = LSTRCL(1)
          IF (IRSET.NE.0) THEN
            IOPPCA = IOPPS
            IOPZCA = IOPZS
          ENDIF
        ELSE
          CALL CDCOMP (LSTRCL(1), LFIXCD, IHEDCF(MPRECF+1), IRC)
          IF (LSTRCL(1).NE.LFIXCD) CALL MZDROP (IDISCD, LSTRCL(1),'L')
          IF (IRC.NE.0)                  GO TO 999
        ENDIF
      ENDIF
*
* *** Save the record on data base
*
      IF (IOPSCD.NE.0) CALL RZLOCK ('CDENFZ')
      CALL RZOUT (IDISCD, LFIXCD, IHEDCF(MPRECF+1), ICYCL, CHOP)
      IER    = IQUEST(1)
      IF (IOPSCD.NE.0) CALL RZFREE ('CDENFZ')
      IKDRCD = IQ(KOFSCD+LCDRCD+KLKDCD)
      NKEYCK = IQ(KOFSCD+LCDRCD+KNKDCD)
      CALL MZDROP (IDISCD, LFIXCD, 'L')
      IF (IER.NE.0) THEN
        IRC    = 73
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          PAT2CT = PATHN
          CALL CDPRNT (LPRTCD, '(/,'' CDENFZ : RZOUT  error for path '//
     +         'name '//PAT2CT//''')', IARGCD, 0)
        ENDIF
#endif
        GO TO 999
      ELSE
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.1) THEN
          CALL UCOPY (IHEDCF(MPRECF+1), KEYNCK, NWKYCK)
          CALL CDUPTM (IARGCD(1), IARGCD(2), KEYNCK(IDHINS), IRC)
          CALL CDPRNT (LPRTCD, '(/,'' CDENFZ : Data was inserted into'//
     +         '   '//PAT2CT//''',/,10X,''on the '',I8,'' at '',I6'//
     +         ','' with Key-Vector '')', IARGCD, 2)
          CALL CDKEYT
          CALL CDPRKY (NWKYCK, KEYNCK, IOTYCK, IRC)
        ENDIF
#endif
        IF (IOPTP.NE.0) THEN
          CALL CDPVAL (IHEDCF(MPRECF+1))
          CALL RZCDIR (PATHN, ' ')
          IF (IQUEST(1).NE.0) THEN
            IRC    = 71
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) THEN
              PAT2CT = PATHN
              CALL CDPRNT (LPRTCD, '(/,'' CDENFZ : Illegal path name'//
     +             ' '//PAT2CT//''')', IARGCD, 0)
            ENDIF
#endif
            GO TO 999
          ENDIF
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          IF (IOPSCD.NE.0) CALL RZLOCK ('CDENFZ')
          CALL RZRENK (KYP1CK, KYP2CK)
          IERR   = IQUEST(1)
          IF (IOPSCD.NE.0) CALL RZFREE ('CDENFZ')
          IF (IERR.NE.0) THEN
            IQUEST(1) = 74
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) THEN
              PAT2CT = PATHN
              CALL CDPRNT (LPRTCD, '(/,'' CDENFZ : RZRENK error for '//
     +             'path name '//PAT2CT//''')', IARGCD, 0)
            ENDIF
#endif
            GO TO 999
          ENDIF
        ENDIF
      ENDIF
*
  998 IQUEST(1) = 0
*                                                             END CDENFZ
  999 END
#endif
