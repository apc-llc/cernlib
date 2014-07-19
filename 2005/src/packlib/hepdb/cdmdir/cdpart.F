*
* $Id: cdpart.F,v 1.1.1.1 1996/02/28 16:24:16 mclareni Exp $
*
* $Log: cdpart.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:16  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDPART (PATHI, PATHO, MXKP0, CHOPT, IRC)
*     ===================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPART (PATHI, PATHO, MXKP0, CHOPT, IRC*)               *
*                                                                      *
*   Transforms the contents of a non-partitioned directory to a        *
*   partitioned directory.                                             *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHI    Character string describing the input  pathname         *
*     PATHO    Character string describing the output pathname         *
*     MXKP0    Maximum number of objects in the partitioned directory  *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*          F   Updates with a fully matched data object (in user keys) *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 68 : Input directory is partitioned                  *
*               = 71 : Illegal path name                               *
*               = 73 : RZOUT fails to write on disk                    *
*               = 74 : Error in RZRENK in updating key values for      *
*                      partitioned data set                            *
*               = 76 : Cannot form the IO descriptor for the FZ header *
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
#include "hepdb/csavbk.inc"
#include "hepdb/ctpath.inc"
#include "hepdb/czpack.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      PARAMETER       (NLEVM=20, JBIAS=2)
      INTEGER         NLCUR(NLEVM), ITIME(MXPACD)
      CHARACTER       PATHY*255, PATHN*255, PATHX*16, CHFOR*100, CHOP0*4
      CHARACTER       CHCUR(NLEVM)*1, CHOPS(NSVMCS)*4, CFORM(6)*1
      CHARACTER       PATHI*(*), PATHO*(*), CHOPT*(*), CHOP*2, CHFRM*100
      DATA            CFORM /'B', 'I', 'F', 'D', 'H', 'A'/
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Decode character option etc.
*
      CALL CDOPTS (' ', IRC)
      CALL UOPTC  (CHOPT, 'B', IOPBCA)
      CALL CDSBLC (PATHO, PATHY, NCHAR)
*
* *** Load top directory information; gets in PAT1CT complete path name
*
      CALL CDLDUP (PATHI, 1, IRC)
      IF (IRC.NE.0) GO TO 999
      PATHN  = PAT1CT
      PATHX  = ' '
*
* *** Set the current directory for input path name
*
      CALL RZCDIR (PATHN, ' ')
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      KST    = NWKYCK + 1
      NWKEY  = NWKYCK
      CALL CDKYTG
      IF (NKEYCK.NE.0)  THEN
        IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      ELSE
        IOPTP  = 0
      ENDIF
      IF (IOPTP.NE.0) THEN
        IRC    = 68
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPART : Directo'//
     +  'ry '//PATHN//' is partitioned '')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Load IPREC/DELTA from dictionary; choose the transcript file
*
      CALL CDLDIC (PATHN, 1, IRC)
      IF (IRC.NE.0) GO TO 999
*
* *** Prepare the partitioned directory
*
      NKEYS  = NKEYCK
      CHFOR  = ' '
      DO 10 IK = 1, NWKEY
        IF (IOTYCK(IK).GT.0.AND.IOTYCK(IK).LT.7) THEN
          CHFOR(IK:IK) = CFORM(IOTYCK(IK))
        ELSE
          CHFOR(IK:IK) = CFORM(2)
        ENDIF
   10 CONTINUE
      IF (ICMPCD.EQ.2) THEN
        CHOP   = 'ZP'
      ELSE IF (ICMPCD.NE.0) THEN
        CHOP  = 'CP'
      ELSE
        CHOP  = 'P '
      ENDIF
      CALL CDMKDI (PATHY, NWKEY, CHFOR, CTAGCK, MXKP0, IPRECD,
     +             DELTCD, CHOP, IRC)
      IF (IRC.NE.0)                GO TO 999
      IF (NKEYS.LE.0)              GO TO 999
*
* *** Find the appropriate FZ file number
*
      IF (IOPBCA.EQ.0) THEN
        LUFZCF = LUFZCD
      ELSE
        LUFZCF = LUBKCD
      ENDIF
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
      IF (IOPPCD.NE.0) THEN
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
        LUFZCF = LUFMCD
#endif
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
        CALL CDSTSV (TOPNCD, 0, IRC)
        IF (IRC.NE.0)              GO TO 999
#endif
#if defined(CERNLIB__P3CHILD)
        LUFZCF = LODBP3
#endif
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
      ENDIF
#endif
*
* *** Partially fill up the header
*
      NDOP   = 1
      IF (LUFZCF.GT.0) THEN
        NCHR   = LENOCC (PATHY)
        NWDP   = (NCHR + 3) / 4
        NWDH   = NWDP + NDOP + NWKYCK + 5
        IHEDCF(MACTCF) = 1
        IHEDCF(MNKYCF) = NWKYCK
        IHEDCF(MOPTCF) = NDOP
        IHEDCF(MPATCF) = NWDP
        CALL UCTOH (PATHY, IHEDCF(NWKYCK+NDOP+MPRECF+1), 4, 4*NWDP)
      ENDIF
*
* *** Start reading in records from the input file
*
      ISTR   = 0
      DO 15 I = 1, NPARCD
        ITIME(I)  = 1
   15 CONTINUE
   20 NOBJCS = 0
      IOPKCA = 0
      PACKCZ = .FALSE.
      CALL RZCDIR (PATHN, ' ')
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      DO 25 IK = 1, NSVMCS
        ISTR   = ISTR + 1
        IF (ISTR.LE.NKEYS) THEN
          NOBJCS = NOBJCS + 1
          KEYSCS(IDHKSN,NOBJCS) = ISTR
          IOKYCA(IDHKSN) = 1
          CALL CDKXIN (ITIME, IDISCD, LOBJCS(NOBJCS), LOBJCS(NOBJCS),
     +                 JBIAS, NWKEY, KEYSCS(1,NOBJCS), IPREC, IRC)
          IOKYCA(IDHKSN) = 0
          IF (IRC.NE.0)            GO TO 997
          IF (JBIT(KEYSCS(IDHFLG,NOBJCS),JRZUCD).NE.0) THEN
            IF (JBIT(KEYSCS(IDHFLG,NOBJCS),JASFCD).NE.0) THEN
              CHOPS(NOBJCS) = 'HTY'
            ELSE
              CHOPS(NOBJCS) = 'HY'
            ENDIF
          ELSE
            CALL CDRZIN (IDISCD, LSTRCL(2), 2, ISTR, ICYCL, PATHN, IRC)
            IF (IRC.NE.0) THEN
              CALL MZDROP (IDISCD, LSTRCL(2), 'L')
              GO TO 997
            ENDIF
            CALL UCOPY (Q(KOFUCD+LSTRCL(2)+3), IXX, 1)
            IF (IQ(KOFUCD+LSTRCL(2)+1).EQ.0) THEN
              IF (KEYSCS(IDHPTR,NOBJCS).EQ.0) THEN
                CHOPS(NOBJCS) = 'H'
              ELSE
                CHOPS(NOBJCS) = 'HD'
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
                  CHOPS(NOBJCS) = 'HZ'
                ELSE
                  CHOPS(NOBJCS) = 'HZD'
                ENDIF
              ELSE
                IF (KEYSCS(IDHPTR,NOBJCS).EQ.0) THEN
                  CHOPS(NOBJCS) = 'HP'
                ELSE
                  CHOPS(NOBJCS) = 'HPD'
                ENDIF
              ENDIF
            ENDIF
            CALL MZDROP (IDISCD, LSTRCL(2), 'L')
          ENDIF
        ENDIF
   25 CONTINUE
*
* *** Set the current directory to the output directory
*
      CALL RZCDIR (PATHY, ' ')
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      KPNT   = IUHUNT (NKEYCK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                 NKEYCK*KST, KST)
      IF (KPNT.NE.0) THEN
        NK     = (KPNT - MPSRCD) / KST + 1
      ELSE
        NK     = NKEYCK
      ENDIF
      CALL CDKEYR (NK, NWKYCK, KYP1CK)
      KOBJ   = KYP1CK(MOBJCD)
      MXKP   = KYP1CK(MXKPCD)
      NWKYS  = NWKYCK
      CALL UCOPY (KYP1CK, KYP2CK, NWKYCK)
*
      CALL CDPATH (PATHX, NKEYCK)
      CALL RZCDIR (PATHX, ' ')
      IF (IQUEST(1).NE.0)          GO TO 991
      NKEYCK = IQUEST(7)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
*
* *** Get the Serial number of the last object inserted
*
      LOBJ   = KOBJ
      IF (NKEYCK.GT.0) THEN
        DO 30 IK = 1, NKEYCK
          IP     = KOFSCD + LCDRCD + IKDRCD + (IK-1)*KST + IDHKSN
          IF (IQ(IP).GT.LOBJ) LOBJ = IQ(IP)
   30   CONTINUE
      ENDIF
      NINS   = NKEYCK
*
* *** Loop over all the objects
*
      DO 50 IOBJ = 1, NOBJCS
        LOBJ   = LOBJ + 1
        NINS   = NINS + 1
        CHOP0  = CHOPS(IOBJ)
*
*  **   Fill up Key vectors 1,2
*
        KEYSCS(1,IOBJ) = LOBJ
        KEYSCS(IDHPTR,IOBJ) = 0
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
*
        IF (IOPPCD.NE.0) THEN
          CALL CDWLOK (IRC)
          IF (IRC.NE.0)            GO TO 999
        ENDIF
#endif
*
*  **   Write the sequential output if needed
*
        IF (LUFZCF.GT.0) THEN
          NLEV   = 1
          NCUR   = 5
          IFORO  = 2
          CHCUR(NLEV) = CFORM(IFORO)
          IF (INDEX(CHOP0,'Z').NE.0) THEN
            NLCUR(NLEV) = 4
            IFORO  = 3
            NCUR   = 1
            NLEV   = NLEV + 1
            CHCUR(NLEV) = CFORM(IFORO)
            CALL UCOPY (PRECCZ, IHEDCF(MPRECF), 1)
          ELSE
            IHEDCF(MPRECF) = IPREC
          ENDIF
          DO 40 I = 1, NWKYCK
            IFORM  = IOTYCK(I)
            IF (IFORM.EQ.6) IFORM = 5
            IF (IFORM.EQ.IFORO) THEN
              NCUR   = NCUR + 1
            ELSE
              NLCUR(NLEV) = NCUR
              IF (NLEV.GE.NLEVM) THEN
                IRC    = 76
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPART :'//
     +          ' Cannot get IO descriptor '//PATHY//''')', IARGCD, 0)
#endif
                GO TO 995
              ENDIF
              NLEV   = NLEV + 1
              CHCUR(NLEV) = CFORM(IFORM)
              NCUR   = 1
              IFORO  = IFORM
            ENDIF
   40     CONTINUE
          NLCUR(NLEV) = NCUR
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
          WRITE (CHFRM, 2001) (NLCUR(I), CHCUR(I), I = 1, NLEV)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
          CHFRM = ' '
          II    = 1
          DO 41 I = 1, NLEV
            CALL UTWRIT (CHFRM(II:II+1), '(I2)', NLCUR(I))
            II     = II + 2
            CHFRM(II:II) = CHCUR(I)
            II     = II + 2
   41     CONTINUE
#endif
          II     = 4 *NLEV
          CHFRM = CHFRM(1:II)//' -H'
          CALL MZIOCH (IOFMCF, NWFMCF, CHFRM(1:II+3))
          CALL UCTOH (CHOP0, IHEDCF(NWKYCK+MPRECF+1), 4, 4*NDOP)
          CALL UCOPY (KEYSCS(1,IOBJ), IHEDCF(MPRECF+1), NWKYCK)
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
          IF (IOPPCD.NE.0) IHEDCF(MPRECF+IDHKSN) = 0
#endif
#if defined(CERNLIB__P3CHILD)
          RNDBP3 = 'CDPART '
          NWDBP3 = 2
          CALL UCTOH ('JOURNAL ', IWDBP3, 4, 8)
          CALL CDCHLD
          IRC    = IQDBP3
          IF (IRC.NE.0)            GO TO 995
#endif
          CALL FZOUT (LUFZCF, IDISCD, LOBJCS(IOBJ), 1, 'L', IOFMCF,
     +                NWDH, IHEDCF)
          IF (IQUEST(1).NE.0) THEN
            IRC        = 77
            IQUEST(11) = IOBJ
            IQUEST(12) = NOBJCS
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPART : Err'//
     +      'or in FZOUT while writing Data for '',2I12)', IQUEST(11),2)
#endif
            GO TO 995
          ENDIF
        ENDIF
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
        IF (IOPPCD.NE.0) THEN
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
          CALL CDCWSV (IRC)
          IF (IRC.NE.0)            GO TO 997
#endif
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
          GO TO 50
        ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
*
*  **   Make a different subdirectory if there are too many keys
*
        IF (NINS.GT.MXKP) THEN
          CALL RZCDIR (PATHY, ' ')
          PATHX  = ' '
          IF (IQUEST(1).NE.0)      GO TO 991
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          NKEYCK = IQUEST(7)
*
*  **     Rename Keys 3 and 4 of the latest subdirectory
*
          IF (IOPSCD.NE.0) CALL RZLOCK ('CDPART')
          CALL RZRENK (KYP1CK, KYP2CK)
          IERR   = IQUEST(1)
          IF (IOPSCD.NE.0) CALL RZFREE ('CDPART')
          IF (IERR.NE.0) THEN
            IRC    = 74
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
            IF (IDEBCD.GT.0) THEN
              CALL UCOPY (KYP1CK, IARGCD(1),        NSYSCK)
              CALL UCOPY (KYP2CK, IARGCD(NSYSCK+1), NSYSCK)
              CALL CDPRNT (LPRTCD, '(/,'' CDPART : Error in RZRENK '//
     +             'while writing data for '//PATHY//''',/(10X,7I12))',
     +             IARGCD, 2*NSYSCK)
            ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
            GO TO 997
          ENDIF
*
*  **     Make a different subdirectory if there are too many keys
*
          KEY7CK = KEYSCS(IDHINS,IOBJ)
          IF (ICMPCD.EQ.2) THEN
            CHOP   = 'ZP'
          ELSE IF (ICMPCD.NE.0) THEN
            CHOP  = 'CP'
          ELSE
            CHOP  = 'P '
          ENDIF
          CALL CDMKDI (PATHY, NWKYS, CHFOR, CTAGCK, MXKP, IPRECD,
     +                 DELTCD, CHOP, IRC)
          IF (IRC.NE.0)            GO TO 997
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
          CALL CDPATH (PATHX, NKEYCK)
          CALL RZCDIR (PATHX, ' ')
          IF (IQUEST(1).NE.0)      GO TO 991
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          NINS   = NKEYCK + 1
          CALL UCOPY (KYP1CK, KYP2CK, NWKYCK)
        ENDIF
*
        IDB    = ICDTYP (LOBJCS(IOBJ))
        IF (IDB.EQ.2.OR.IDB.EQ.3) THEN
          IOPTR  = 0
        ELSE
          IOPTR  = 1
        ENDIF
*
        IF (IOPSCD.NE.0) CALL RZLOCK ('CDPART')
        IF (JBIT(KEYSCS(IDHFLG,IOBJ),JRZUCD).NE.0. OR. IOPTR.NE.0) THEN
*
*  **     RZ mode output
*
          IF (JBIT(KEYSCS(IDHFLG,IOBJ),JASFCD).NE.0) THEN
            CHOP   = 'S'
          ELSE IF (JBIT(KEYSCS(IDHFLG,IOBJ),JRZUCD).NE.0) THEN
            CHOP   = 'L'
          ELSE
            CHOP   = ' '
          ENDIF
*
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.2) CALL RZLDIR (' ', ' ')
#endif
#if !defined(CERNLIB__P3CHILD)
          CALL RZOUT (IDISCD, LOBJCS(IOBJ), KEYSCS(1,IOBJ), ICYCLE,CHOP)
*
        ELSE
*
*  **     Copy data to DB internal store
*
*  **     0 Data word : do not pack
*
          CALL CDFRUS (LOBJCS(IOBJ), LSTRCL(1), IPRECD, IRC)
          IF (IRC.NE.0)            GO TO 995
*
*  **     Compress the data if requested
*
          IF (IQ(KOFUCD+LOBJCS(IOBJ)-1).EQ.0 .OR.
     +       (INDEX(CHOP0,'P').EQ.0.AND.INDEX(CHOP0,'Z').EQ.0)) THEN
            LREFCL(1) = LSTRCL(1)
          ELSE
            IF (INDEX(CHOP0,'D').NE.0) THEN
              IOPDCA = 1
            ELSE
              IOPDCA = 0
            ENDIF
            IF (INDEX(CHOP0,'Z').NE.0) THEN
              IOPPCA = 0
              IOPZCA = 1
              PACKCZ = .TRUE.
            ELSE
              IOPPCA = 1
              IOPZCA = 0
              PACKCZ = .FALSE.
            ENDIF
            NOLD   = NKEYCK
            NKEYCK = NINS - 1
            CALL CDCOMP (LSTRCL(1), LREFCL(1), KEYSCS(1,IOBJ), IRC)
            IOPDCA = 0
            IOPPCA = 0
            IOPZCA = 0
            NKEYCK = NOLD
          ENDIF
          IF (IRC.NE.0)            GO TO 995
*
*  **     Drop the uncompressed data
*
          IF (LREFCL(1).NE.LSTRCL(1)) CALL MZDROP (IDISCD,LSTRCL(1),'L')
*
*  **     Write on to disk
*
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.2) CALL RZLDIR (' ', ' ')
#endif
#if !defined(CERNLIB__P3CHILD)
          CALL RZOUT (IDISCD, LREFCL(1), KEYSCS(1,IOBJ), ICYCLE, 'S')
          IER    = IQUEST(1)
          CALL MZDROP (IDISCD, LREFCL(1), 'L')
          IQUEST(1) = IER
        ENDIF
*
        IKDRCD = IQ(KOFSCD+LCDRCD+KLKDCD)
        NKEYCK = IQ(KOFSCD+LCDRCD+KNKDCD)
*
        IF (IOPSCD.NE.0) THEN
          IER    = IQUEST(1)
          CALL RZFREE ('CDPART')
          IQUEST(1) = IER
        ENDIF
*
        CALL CDPVAL (KEYSCS(1,IOBJ))
        IF (IQUEST(1).NE.0)        GO TO 993
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.1) THEN
          CALL UCOPY (KEYSCS(1,IOBJ), KEYNCK, NWKYCK)
          CALL CDUPTM (IARGCD(1), IARGCD(2), KEYNCK(IDHINS), IRC)
          CALL CDPRNT (LPRTCD, '(/,'' CDPART : Data was inserted into'//
     +         '   '//PATHY//''',/,10X,''on the '',I8,'' at '',I6,'' '//
     +         'with Key-Vector '')', IARGCD, 2)
          CALL CDKEYT
          CALL CDPRKY (NWKYCK, KEYNCK, IOTYCK, IRC)
        ENDIF
#endif
   50 CONTINUE
      IF (ISTR.GE.NKEYS)           GO TO 995
      DO 60 IOBJ = 1, NOBJCS
        IF (LOBJCS(IOBJ).NE.0) CALL MZDROP (IDISCD, LOBJCS(IOBJ), 'L')
   60 CONTINUE
*
*  ** Rename Keys 3 and 4 of the latest subdirectory
*
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))
      IF (IOPPCD.EQ.0) THEN
#endif
#if !defined(CERNLIB__P3CHILD)
        CALL RZCDIR (PATHY, ' ')
        PATHX  = ' '
        IF (IQUEST(1).NE.0)        GO TO 991
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        NKEYCK = IQUEST(7)
        IF (IOPSCD.NE.0) CALL RZLOCK ('CDPART')
        CALL RZRENK (KYP1CK, KYP2CK)
        IERR   = IQUEST(1)
        IF (IOPSCD.NE.0) CALL RZFREE ('CDPART')
        IF (IERR.NE.0) THEN
          IRC    = 74
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.0) THEN
            CALL UCOPY (KYP1CK, IARGCD(1),        NSYSCK)
            CALL UCOPY (KYP2CK, IARGCD(NSYSCK+1), NSYSCK)
            CALL CDPRNT (LPRTCD, '(/,'' CDPART : Error in RZRENK '//
     +           'while writing data for '//PATHY//''',/(10X,7I12))',
     +           IARGCD, 2*NSYSCK)
          ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 997
        ENDIF
#endif
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))
      ENDIF
#endif
      GO TO 20
*
* *** Error messages
*
  991 IRC    = 71
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPART : Illegal '//
     +   'Path Name '//PATHY//PATHX(1:8)//''')', IARGCD, 0)
#endif
      GO TO 997
#if !defined(CERNLIB__P3CHILD)
*
  993 IRC    = 73
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
      IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPART : Error in '//
     +   'RZOUT while writing Data for '//PATHY//PATHX(1:8)//''')',
     +   IARGCD, 0)
#endif
*
  995 CONTINUE
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
      IF (IOPPCD.NE.0) GO TO 997
#endif
#if !defined(CERNLIB__P3CHILD)
      IF (NINS.GT.0)  THEN
        CALL RZCDIR (PATHY, ' ')
        IF (IQUEST(1).NE.0) THEN
          IF (IRC.EQ.0) GO TO 991
        ELSE
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          NKEYCK = IQUEST(7)
*
*  **     Rename Keys 3 and 4 of the latest subdirectory
*
          IF (IRC.EQ.0) THEN
            IF (IOPSCD.NE.0) CALL RZLOCK ('CDPART')
            CALL RZRENK (KYP1CK, KYP2CK)
            IERR   = IQUEST(1)
            IF (IOPSCD.NE.0) CALL RZFREE ('CDPART')
            IF (IERR.NE.0) THEN
              IRC    = 74
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
              IF (IDEBCD.GT.0) THEN
                CALL UCOPY (KYP1CK, IARGCD(1),        NSYSCK)
                CALL UCOPY (KYP2CK, IARGCD(NSYSCK+1), NSYSCK)
                CALL CDPRNT (LPRTCD, '(/,'' CDPART : Error in RZRENK '//
     +               'while writing data for '//PATHY//''',/(10X,7I12))'
     +,              IARGCD, 2*NSYSCK)
              ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
#endif
*
* *** Drop the stored banks
*
  997 IF (NOBJCS.GT.0) THEN
        DO 998 IOBJ = 1, NOBJCS
          CALL MZDROP (IDISCD, LOBJCS(IOBJ), 'L')
  998   CONTINUE
        NOBJCS = 0
      ENDIF
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
*
 2001 FORMAT (20(I2,A1,1X))
#endif
*                                                             END CDPART
  999 END
