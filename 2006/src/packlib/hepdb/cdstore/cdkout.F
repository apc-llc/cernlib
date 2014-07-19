*
* $Id: cdkout.F,v 1.1.1.1 1996/02/28 16:24:33 mclareni Exp $
*
* $Log: cdkout.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:33  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDKOUT (PATHN, LADDR, IUDIV, KEYI, KEYO, IRC)
*     ========================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDKOUT (PATHN, LADDR, IUDIV, KEYI, KEYO*, IRC*)         *
*                                                                      *
*   Stores data from memory to disk                                    *
*   Data structure refered by LREFCD(2); set up by the calling routine *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IUDIV    Division index where the data reside                    *
*     KEYI     Input vector of keys (validity ranges, user and special *
*              user keys to be filled in by the user)                  *
*     KEYO     Output vector of keys (copy of KEYI +system keys filled *
*              in by the HEPDB package)                                *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDREPL, CDSTOR                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 71 : Illegal path name                               *
*               = 73 : RZOUT fails to write on disk                    *
*               = 74 : Error in RZRENK in updating key values for      *
*                      partitioned data set                            *
*               = 76 : Cannot form the IO descriptor for the FZ header *
*               = 77 : FZOUT fails to write on to the sequential file  *
*               = 78 : Illegal number of keys in data base             *
*                                                                      *
************************************************************************
*
#include "hepdb/caopti.inc"
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/cinitl.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctpath.inc"
#include "hepdb/czpack.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      PARAMETER       (NLEVM=20)
      CHARACTER       PATHN*(*), PATHY*80, PATHX*16, PATHL*80, CHOPT*28
      CHARACTER       CFORM(6)*1, CHCUR(NLEVM)*1, CHFOR*100, CHOP*1
      INTEGER         KEYI(9), KEYO(9), LADDR(9), NLCUR(NLEVM),IOPTS(26)
      EQUIVALENCE     (IOPACA, IOPTS(1))
      DATA            CFORM /'B', 'I', 'F', 'D', 'H', 'A'/
#include "zebra/q_jbit.inc"
* Ignoring t=pass
#include "zebra/q_sbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      LREFCD(2) = LADDR(1)
*
* *** Load top directory information; gets in PAT1CT complete path name
*
      CALL CDLDUP (PATHN, 1, IRC)
      IF (IRC.NE.0)         GO TO 999
*
* *** Set the current directory path name
*
      PATHL  = ' '
      PATHY  = PAT1CT
      CALL RZCDIR (PATHY, ' ')
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      KST    = NWKYCK + 1
      CALL CDKYTG
      IF (NKEYCK.NE.0)  THEN
        IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      ELSE
        IOPTP  = 0
      ENDIF
*
* *** Load IPREC/DELTA from dictionary; choose the transcript file
*
      CALL CDLDIC (PATHY, 1, IRC)
      IF (IRC.NE.0)         GO TO 999
      IF (ICMPCD.EQ.0) THEN
        IOPYCA = 1
        PACKCZ = .FALSE.
      ELSE IF (ICMPCD.EQ.2.AND.IOPZCA.NE.0) THEN
        PACKCZ = .TRUE.
        PRECCZ = DELTCD
      ELSE
        PACKCZ = .FALSE.
      ENDIF
      IF (IOPBCA.EQ.0) THEN
        LUFZCF = LUFZCD
      ELSE
        LUFZCF = LUBKCD
      ENDIF
*
* *** Fill up Key Serial number, pointer, insertion time
*
      CALL UCOPY (KEYI, KEYO, NWKYCK)
      KEYO(IDHKSN) = 0
      KEYO(IDHPTR) = 0
      IF (IOPHCA.EQ.0.OR.KEYO(IDHINS).LE.0) THEN
        CALL DATIME (IDATE, ITIME)
        CALL CDPKTM (IDATE, ITIME, IDATM, IRC)
        KEYO(IDHINS) = IDATM
      ENDIF
*
      IDB    = ICDTYP (LREFCD(2))
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
*
* *** Encode the character option, IO descriptor for the header
*
      IF (IOPPCD.NE.0.OR.LUFZCF.GT.0) THEN
        CHOPT  = 'H'
        NDOP   = 1
        DO 10 I = 1, 26
          IF (IOPTS(I).NE.0.AND.I.NE.8) THEN
            IF (NDOP.EQ.0) THEN
              CHOPT  = CALFCA(I)
            ELSE
              CHOPT  = CHOPT(1:NDOP)//CALFCA(I)
            ENDIF
            NDOP   = NDOP + 1
          ENDIF
   10   CONTINUE
        NDOP   = (NDOP + 3) / 4
*
        NLEV   = 1
        NCUR   = 5
        IFORO  = 2
        CHCUR(NLEV) = CFORM(IFORO)
        IF (PACKCZ) THEN
          NLCUR(NLEV) = 4
          IFORO  = 3
          NCUR   = 1
          NLEV   = NLEV + 1
          CHCUR(NLEV) = CFORM(IFORO)
          CALL UCOPY (PRECCZ, IHEDCF(MPRECF), 1)
        ELSE
          IHEDCF(MPRECF) = IPRECD
        ENDIF
        DO 15 I = 1, NWKYCK
          IFORM  = IOTYCK(I)
          IF (IFORM.EQ.6) IFORM = 5
          IF (IFORM.EQ.IFORO) THEN
            NCUR   = NCUR + 1
          ELSE
            NLCUR(NLEV) = NCUR
            IF (NLEV.GE.NLEVM) THEN
              IRC    = 76
#if defined(CERNLIB__DEBUG)
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKOUT : '//
     +        'Cannot get IO descriptor '//PATHY//''')', IARGCD, 0)
#endif
              GO TO 999
            ENDIF
            NLEV   = NLEV + 1
            CHCUR(NLEV) = CFORM(IFORM)
            NCUR   = 1
            IFORO  = IFORM
          ENDIF
   15   CONTINUE
        NLCUR(NLEV) = NCUR
*
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
        WRITE (CHFOR, 2001) (NLCUR(I), CHCUR(I), I = 1, NLEV)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
        CHFOR  = ' '
        II     = 1
        DO 20 I = 1, NLEV
          CALL UTWRIT (CHFOR(II:II+1), '(I2)', NLCUR(I))
          II     = II + 2
          CHFOR(II:II) = CHCUR(I)
          II     = II + 2
   20   CONTINUE
#endif
        II     = 4 *NLEV
        CHFOR = CHFOR(1:II)//' -H'
        CALL MZIOCH (IOFMCF, NWFMCF, CHFOR(1:II+3))
*
*  **   Complete the header
*
        NCHR   = LENOCC (PATHY)
        NWDP   = (NCHR + 3) / 4
        NWDH   = NWDP + NDOP + NWKYCK + 5
        IHEDCF(MACTCF) = 1
        IHEDCF(MNKYCF) = NWKYCK
        IHEDCF(MOPTCF) = NDOP
        IHEDCF(MPATCF) = NWDP
        IF (NDOP.GT.0)
     +    CALL UCTOH (CHOPT, IHEDCF(MPRECF+NWKYCK+1), 4, 4*NDOP)
        CALL UCTOH (PATHY, IHEDCF(MPRECF+NWKYCK+NDOP+1), 4, 4*NWDP)
        CALL UCOPY (KEYO, IHEDCF(MPRECF+1), NWKYCK)
      ENDIF
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
      IF (IOPPCD.NE.0) THEN
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
        CALL CDWLOK (IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
        CALL CDSTSV (TOPNCD, 0, IRC)
#endif
#if defined(CERNLIB__P3CHILD)
        LUFZCF = LODBP3
        RNDBP3 = 'CDKOUT '
        NWDBP3 = 2
        CALL UCTOH ('JOURNAL ', IWDBP3, 4, 8)
        CALL CDCHLD
        IRC    = IQDBP3
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        IF (IRC.NE.0)       GO TO 999
*
*  **   Copy the record to CLUSCOM or SPOOL
*
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(defined(CERNLIB__DEBUG))
          IF (IDEBCD.GE.1) CALL CDPRNT (LPRTCD, '(/,'' CDKOUT : Call'//
     +    ' FZOUT with option '//CHOP//''')', IARGCD, 0)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        CALL FZOUT (LUFZCF, IUDIV, LREFCD(2), 1, CHOP, IOFMCF, NWDH,
     +              IHEDCF)
        IF (IQUEST(1).NE.0) THEN
          IRC    = 77
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(defined(CERNLIB__DEBUG))
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKOUT : Error'//
     +    ' in FZOUT while writing Data for '//PATHY//''')', IARGCD, 0)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
          GO TO 999
        ENDIF
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
*
        CALL CDCWSV (IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        GO TO 999
      ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
*
* *** Take necessary action for partitioned and nonpartitioned datasets
*
      IF (IOPTP.EQ.0) THEN
        KOBJ   = 0
      ELSE
        KPNT   = IUHUNT (NKEYCK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                   NKEYCK*KST, KST)
        IF (KPNT.NE.0) THEN
          NK     = (KPNT - MPSRCD) / KST + 1
        ELSE
          NK     = NKEYCK
        ENDIF
        CALL CDKEYR (NK, NWKYCK, KYP1CK)
        KOBJ   = KYP1CK(MOBJCD)
        MXKP   = KYP1CK(MXKPCD)
        CALL CDPATH (PATHX, NKEYCK)
        CALL RZCDIR (PATHX, ' ')
        IF (IQUEST(1).NE.0) THEN
          IRC    = 71
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKOUT : '//
     +       'Illegal Path Name '//PATHY//PATHX(1:8)//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 999
        ENDIF
        NKEYCK = IQUEST(7)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
*
*  **   Make a different subdirectory if there are too many keys
*
        IF (NKEYCK.GE.MXKP) THEN
          NWKEY  = NWKYCK
          CHFOR = ' '
          DO 30 I = 1, NWKEY
            IF (I.EQ.1) THEN
              CHFOR  = CFORM(IOTYCK(I))
            ELSE
              CHFOR  = CHFOR(1:I-1)//CFORM(IOTYCK(I))
            ENDIF
   30     CONTINUE
          IF (IOPHCA.EQ.0.OR.KEYO(IDHINS).LE.0) THEN
            KEY7CK = 0
          ELSE
            KEY7CK = KEYO(IDHINS)
          ENDIF
          IF (ICMPCD.EQ.2) THEN
            CHOPT  = 'ZP'
          ELSE IF (ICMPCD.NE.0) THEN
            CHOPT = 'CP'
          ELSE
            CHOPT = 'P '
          ENDIF
          CALL CDMKDI (PATHY, NWKEY, CHFOR, CTAGCK, MXKP, IPRECD,
     +                 DELTCD, CHOPT, IRC)
          IF (IRC.NE.0)     GO TO 999
          CALL RZCDIR (PATHY, ' ')
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          KPNT   = IUHUNT (NKEYCK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                     NKEYCK*KST, KST)
          IF (KPNT.NE.0) THEN
            NK     = (KPNT - MPSRCD) / KST + 1
          ELSE
            NK     = NKEYCK
          ENDIF
          CALL CDKEYR (NK, NWKYCK, KYP1CK)
          KOBJ   = KYP1CK(MOBJCD)
          CALL CDPATH (PATHX, NKEYCK)
          CALL RZCDIR (PATHX, ' ')
          IF (IQUEST(1).NE.0) THEN
            IRC    = 71
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKOUT : '//
     +         'Illegal Path Name '//PATHY//PATHX(1:8)//''')', IARGCD,0)
#endif
#if !defined(CERNLIB__P3CHILD)
            GO TO 999
          ENDIF
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
        ENDIF
        CALL UCOPY (KYP1CK, KYP2CK, NWKYCK)
*
      ENDIF
*
* *** Find the unique serial number of the object
*
      LOBJ   = KOBJ
      IF (NKEYCK.GT.0) THEN
        ISTP   = NWKYCK + 1
        DO 40 IK = 1, NKEYCK
          IP     = KOFSCD + LCDRCD + IKDRCD + (IK-1)*ISTP + IDHKSN
          IF (IQ(IP).GT.LOBJ) LOBJ = IQ(IP)
   40   CONTINUE
      ENDIF
      LOBJ   = LOBJ + 1
*
*  ** Write the sequential output if needed
*
      IF (LUFZCF.GT.0) THEN
        IHEDCF(MPRECF+IDHKSN) = LOBJ
        CALL FZOUT (LUFZCF, IUDIV, LREFCD(2), 1, CHOP, IOFMCF, NWDH,
     +              IHEDCF)
        IF (IQUEST(1).NE.0) THEN
          IRC    = 77
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKOUT : Error'//
     +    ' in FZOUT while writing Data for '//PATHY//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 50
        ENDIF
      ENDIF
*
* *** Fill up Key vectors 1,2,6,7
*
      KEYO(IDHKSN) = LOBJ
      KEYO(IDHFLG) = MSBIT0 (KEYO(IDHFLG), JRZUCD)
      KEYO(IDHFLG) = MSBIT0 (KEYO(IDHFLG), JPRTCD)
      KEYO(IDHFLG) = MSBIT0 (KEYO(IDHFLG), JASFCD)
      IF (IOPICA.EQ.0) THEN
        KEYO(IDHFLG) = MSBIT0 (KEYO(IDHFLG), JIGNCD)
      ELSE
        KEYO(IDHFLG) = MSBIT1 (KEYO(IDHFLG), JIGNCD)
      ENDIF
*
* *** Lock the directory if necessary
*
      IF (IOPPCD.EQ.0.AND.IOPSCD.NE.0) THEN
        CALL RZCDIR (PATHY, ' ')
        NKEYCK = IQUEST(7)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        CALL RZLOCK ('CDKOUT')
        PATHL  = PATHY
        IF (IOPTP.NE.0) THEN
          CALL RZCDIR (PATHX, ' ')
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
        ENDIF
      ENDIF
*
      IF (IOPYCA.NE.0 .OR. IOPTR.NE.0 .OR. IOPTCA.NE.0) THEN
*
*  **   RZ mode output
*
        KEYO(IDHFLG) = MSBIT1 (KEYO(IDHFLG), JRZUCD)
        IF (IOPTCA.NE.0) KEYO(IDHFLG) = MSBIT1 (KEYO(IDHFLG), JASFCD)
*
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.2) CALL RZLDIR (' ', ' ')
#endif
#if !defined(CERNLIB__P3CHILD)
        CALL RZOUT (IUDIV, LREFCD(2), KEYO, ICYCLE, CHOP)
*
      ELSE
*
*  **   Copy data to DB internal store
*
*  **   0 Data word : do not pack
*
        IF (IQ(KOFUCD+LREFCD(2)-1).EQ.0) THEN
          IRSET  = 1
          IOPPS  = IOPPCA
          IOPZS  = IOPZCA
          IOPPCA = 0
          IOPZCA = 0
        ELSE
          IRSET  = 0
        ENDIF
        CALL CDFRUS (LREFCD(2), LSTRCL(1), IPRECD, IRC)
        IF (IRC.NE.0) THEN
          IF (IRSET.NE.0) THEN
            IOPPCA = IOPPS
            IOPZCA = IOPZS
          ENDIF
          GO TO 50
        ENDIF
*
*  **   Compress the data if requested
*
        IF (IOPPCA.EQ.0.AND.IOPZCA.EQ.0) THEN
          LREFCL(1) = LSTRCL(1)
          IF (IRSET.NE.0) THEN
            IOPPCA = IOPPS
            IOPZCA = IOPZS
          ENDIF
        ELSE
          CALL CDCOMP (LSTRCL(1), LREFCL(1), KEYO(1), IRC)
        ENDIF
        IF (IRC.NE.0)       GO TO 50
*
*  **   Drop the uncompressed data
*
        IF (LREFCL(1).NE.LSTRCL(1)) CALL MZDROP (IDISCD, LSTRCL(1), 'L')
*
*  **   Write on to disk
*
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.2) CALL RZLDIR (' ', ' ')
#endif
#if !defined(CERNLIB__P3CHILD)
        CALL RZOUT (IDISCD, LREFCL(1), KEYO, ICYCLE, 'S')
        IER       = IQUEST(1)
        CALL MZDROP (IDISCD, LREFCL(1), 'L')
        IQUEST(1) = IER
*
      ENDIF
      CALL UCOPY (KEYO, KEYNCK, NWKYCK)
      IKDRCD = IQ(KOFSCD+LCDRCD+KLKDCD)
      NKEYCK = IQ(KOFSCD+LCDRCD+KNKDCD)
*
      IF (IQUEST(1).NE.0) THEN
        IRC    = 73
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKOUT : Error '//
     +     'in RZOUT while writing Data for '//PATHY//PATHX(1:8)//''')',
     +     IRC, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
        GO TO 50
      ENDIF
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
      IF (IDEBCD.GT.1) THEN
        IARGCD(1) = IDATE
        IARGCD(2) = ITIME
        CALL CDPRNT (LPRTCD, '(/,'' CDKOUT : Data was inserted into'//
     +       '   '//PATHY//''',/,10X,''on the '',I8,'' at '',I6,'' '//
     +       'with Key-Vector '')', IARGCD, 2)
        CALL CDKEYT
        CALL CDPRKY (NWKYCK, KEYNCK, IOTYCK, IRC)
      ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
*
   50 IF (IOPTP.NE.0)  THEN
        CALL RZCDIR (PATHY, ' ')
        IF (IQUEST(1).NE.0) THEN
          IF (IRC.EQ.0) THEN
            IF (PATHL.NE.' ') THEN
              CALL RZCDIR (PATHL, ' ')
              CALL RZFREE ('CDKOUT')
            ENDIF
            IRC    = 71
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKOUT : '//
     +         'Illegal Path Name '//PATHY//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
            GO TO 999
          ENDIF
        ELSE
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          NKEYCK = IQUEST(7)
*
*  **     Rename Keys 3 and 4 of the latest subdirectory
*
          IF (IRC.EQ.0) THEN
            CALL CDPVAL (KEYO(1))
            CALL RZRENK (KYP1CK, KYP2CK)
            IF (IQUEST(1).NE.0) THEN
              IRC    = 74
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
              IF (IDEBCD.GT.0) THEN
                CALL UCOPY (KYP1CK, IARGCD(1),        NSYSCK)
                CALL UCOPY (KYP2CK, IARGCD(NSYSCK+1), NSYSCK)
                CALL CDPRNT (LPRTCD, '(/,'' CDKOUT : Error in RZRENK '//
     +               'while writing data for '//PATHY//''',/(10X,7I12))'
     +,              IARGCD, 2*NSYSCK)
              ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
*
      IF (PATHL.NE.' ') THEN
        CALL RZCDIR (PATHL, ' ')
        CALL RZFREE ('CDKOUT')
      ENDIF
#endif
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
*
 2001 FORMAT (20(I2,A1,1X))
#endif
*                                                             END CDKOUT
  999 END
