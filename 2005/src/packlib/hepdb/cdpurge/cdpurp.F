*
* $Id: cdpurp.F,v 1.1.1.1 1996/02/28 16:24:25 mclareni Exp $
*
* $Log: cdpurp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:25  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDPURP (PATHN, IKEEP, CHOPT, IRC)
*     ============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPURP (PATHN, IKEEP, CHOPT, IRC*)                      *
*                                                                      *
*   Deletes partitions in a partitioned directory except the last      *
*   (first) few ones.                                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IKEEP    Number of partitions to be preserved                    *
*              (If -ve the last -IKEEP partitions deleted)             *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by user,   CDFZUP                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 69 : Input directory is not partitioned              *
*               = 70 : Error in deleting a partition                   *
*               = 71 : Illegal path name                               *
*               = 73 : RZOUT fails to write on disk                    *
*               = 74 : Error in RZRENK in updating key values for      *
*                      partitioned data set                            *
*               = 77 : FZOUT fails to write on to the sequential file  *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/csavbk.inc"
#include "hepdb/ctpath.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      PARAMETER       (JBIAS=2)
      CHARACTER       PATHN*(*), CHOPT*(*), PATHY*80, PATHL*80
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Initialize options, insertion time cutoffs, etc.
*
      PATHL  = ' '
      CALL CDOPTS (' ', IRC)
      CALL UOPTC  (CHOPT, 'B', IOPBCA)
      NKEEP  = IKEEP
      KEY7   = KEY7CK
      KEY7CK = 0
      IF (NKEEP.EQ.0) NKEEP = 1
      IF (KEY7.LE.0) THEN
        CALL DATIME (IDATE, ITIME)
        CALL CDPKTM (IDATE, ITIME, KEY7, IRC)
      ENDIF
*
* *** Set the current directory for input path name
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)                       GO TO 999
      KST    = NWKYCK + 1
      PATHY  = PAT1CT
      NCHAR  = LENOCC (PATHY)
      CALL CDKEYT
      IF (NKEYCK.NE.0)  THEN
        IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      ELSE
        IOPTP  = 0
      ENDIF
      IF (IOPTP.EQ.0) THEN
        IRC    = 69
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURP : Directo'//
     +  'ry '//PATHY//' is not partitioned '')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      IF (NKEYCK.LE.NKEEP.AND.NKEEP.GT.0) GO TO 999
*
* *** Find the appropriate FZ file number
*
      IF (IOPBCA.EQ.0) THEN
        LUFZCF = LUFZCD
      ELSE
        LUFZCF = LUBKCD
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
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        IF (IRC.NE.0)                     GO TO 999
      ENDIF
#endif
*
* *** Partially fill up the header
*
      IF (LUFZCF.GT.0) THEN
        IF (IOPBCA.EQ.0) THEN
          NDOP   = 0
        ELSE
          NDOP   = 1
        ENDIF
        NWDP   = (NCHAR + 3) / 4
        NWDH   = NDOP + NWDP + 6
        IHEDCF(MACTCF) = 8
        IHEDCF(MNKYCF) = 0
        IHEDCF(MOPTCF) = NDOP
        IHEDCF(MPATCF) = NWDP
        IHEDCF(MDELCF) = KEY7
        IHEDCF(MKEPCF) = NKEEP
        IF (NDOP.EQ.1) CALL UCTOH ('B   ', IHEDCF(MKEPCF+1), 4, 4)
        CALL UCTOH (PATHY, IHEDCF(MKEPCF+NDOP+1), 4, 4*NWDP)
        CALL MZIOCH (IOFMCF, NWFMCF, '6I -H')
*
*  **   Now write on the sequential file
*
#if defined(CERNLIB__P3CHILD)
        RNDBP3 = 'CDPURP '
        NWDBP3 = 2
        CALL UCTOH ('JOURNAL ', IWDBP3, 4, 8)
        CALL CDCHLD
        IRC    = IQDBP3
        IF (IRC.NE.0)                     GO TO 999
#endif
        CALL FZOUT (LUFZCF, IDISCD, 0, 1, 'Z', IOFMCF, NWDH, IHEDCF)
        IF (IQUEST(1).NE.0) THEN
          IRC    = 77
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURP : Error'//
     +    ' in FZOUT while writing Data for '//PATHY//''')', IARGCD, 0)
#endif
          GO TO 999
        ENDIF
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
        IF (IOPPCD.NE.0) THEN
          IRC    = 0
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
          CALL CDCWSV (IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
          GO TO 999
        ENDIF
#endif
      ENDIF
#if !defined(CERNLIB__P3CHILD)
*
* *** Check if the directory updated after KEY7
*
      NKEYS  = NKEYCK
      CALL CDPATH (TOP1CT, NKEYS)
      PAT2CT = PATHY(1:NCHAR)//'/'//TOP1CT
      CALL RZCDIR (PAT2CT, ' ')
      IF (IQUEST(1).NE.0) THEN
        IRC    = 71
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURP : Illeg'//
     +  'al Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
        GO TO 999
      ENDIF
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      IF (IQ(KOFSCD+LCDRCD+IKDRCD+(NKEYCK-1)*KST+IDHINS).GT.KEY7)
     +                                    GO TO 999
*
* *** Lock the directory in shared mode
*
      IF (IOPSCD.NE.0) THEN
        PATHL  = PATHY(1:NCHAR)
        CALL RZCDIR (PATHL, ' ')
        CALL RZLOCK ('CDPURP')
      ENDIF
      IF (NKEEP.LT.0)                     GO TO 55
*
* *** Now delete all objects in the first few subdirectories
*
      DO 15 JKK = NKEEP+1, NKEYS
        IKK    = JKK - NKEEP
        CALL CDPATH (TOP1CT, IKK)
        PAT2CT = PATHY(1:NCHAR)//'/'//TOP1CT
        CALL RZCDIR (PAT2CT, ' ')
        IF (IQUEST(1).NE.0) THEN
          IRC    = 71
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURP : Illeg'//
     +    'al Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 998
        ENDIF
        NKEYCK = IQUEST(7)
        IF (NKEYCK.GT.0) CALL RZDELK (KDUM, ICDUM, 'K')
   15 CONTINUE
*
* *** Now transfer data from the last partitions to the first
* *** NKEEP partitions
*
      NOBJCS = 0
      DO 50 JKK = 1, NKEEP
        IKK    = NKEYS - NKEEP + JKK
        ISTR   = 0
*
*  **   Set the current directory to the input directory
*
   20   CALL CDPATH (TOP1CT, IKK)
        PAT2CT = PATHY(1:NCHAR)//'/'//TOP1CT
        CALL RZCDIR (PAT2CT, ' ')
        IF (IQUEST(1).NE.0) THEN
          IRC    = 71
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURP : Illeg'//
     +    'al Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 996
        ENDIF
        NKEYCK = IQUEST(7)
        NWKYCK = IQUEST(8)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        DO 25 IK = 1, NSVMCS
          ISTR   = ISTR + 1
          IF (ISTR.LE.NKEYCK) THEN
            NOBJCS = NOBJCS + 1
            KEYSCS(IDHKSN,NOBJCS) = ISTR
            ICYCL  = 9999
            CALL CDRZIN (IDISCD, LOBJCS(NOBJCS), JBIAS, ISTR, ICYCL,
     +                   PAT2CT, IRC)
            IF (IRC.NE.0)                 GO TO 996
            CALL CDKEYR (ISTR, NWKYCK, KEYSCS(1,NOBJCS))
          ENDIF
   25   CONTINUE
        IF (NOBJCS.EQ.0)                  GO TO 40
*
*  **   Set the current directory to the output directory
*
        CALL CDPATH (TOP1CT, JKK)
        PAT2CT = PATHY(1:NCHAR)//'/'//TOP1CT
        CALL RZCDIR (PAT2CT, ' ')
        IF (IQUEST(1).NE.0) THEN
          IRC    = 71
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURP : Illeg'//
     +    'al Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 996
        ENDIF
        NWKYCK = IQUEST(8)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        DO 30 IK = 1, NOBJCS
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.2) CALL RZLDIR (' ', ' ')
#endif
#if !defined(CERNLIB__P3CHILD)
          CALL RZOUT (IDISCD, LOBJCS(IK), KEYSCS(1,IK), ICYCLE, 'L')
          IF (IQUEST(1).NE.0) THEN
            IRC    = 73
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURP : Err'//
     +      'or in RZOUT while writing Data for '//PAT2CT//''')',
     +      IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
            GO TO 996
          ENDIF
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.1) THEN
            CALL UCOPY (KEYSCS(1,IK), KEYNCK, NWKYCK)
            CALL CDUPTM (IARGCD(1), IARGCD(2), KEYNCK(IDHINS), IRC)
            CALL CDPRNT (LPRTCD, '(/,'' CDPURP : Data was inserted in'//
     +           'to   '//PAT2CT//''',/,10X,''on the '',I8,'' at '','//
     +           'I6,'' with Key-Vector '')', IARGCD, 2)
            CALL CDPRKY (NWKYCK, KEYNCK, IOTYCK, IRC)
          ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
   30   CONTINUE
        DO 35 IK = 1, NOBJCS
          IF (LOBJCS(IK).NE.0) CALL MZDROP (IDISCD, LOBJCS(IK), 'L')
   35   CONTINUE
        NOBJCS = 0
        IF (ISTR.LT.NKEYCK)               GO TO 20
*
*  **   Now update the keys of the main directory
*
   40   CALL RZCDIR (PATHY, ' ')
        NKEYCK = IQUEST(7)
        NWKYCK = IQUEST(8)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        KPNT   = IUHUNT (JKK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                   NKEYCK*KST, KST)
        IF (KPNT.NE.0) THEN
          NK     = (KPNT - MPSRCD) / KST + 1
        ELSE
          NK     = JKK
        ENDIF
        CALL CDKEYR (NK, NWKYCK, KYP1CK)
        KPNT   = IUHUNT (IKK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                   NKEYCK*KST, KST)
        IF (KPNT.NE.0) THEN
          NK     = (KPNT - MPSRCD) / KST + 1
        ELSE
          NK     = IKK
        ENDIF
        CALL CDKEYR (NK, NWKYCK, KYP2CK)
        KYP2CK(MPSRCD) = KYP1CK(MPSRCD)
        CALL RZRENK (KYP1CK, KYP2CK)
        IF (IQUEST(1).NE.0) THEN
          IRC    = 74
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.0) THEN
            CALL UCOPY  (KYP1CK, IARGCD(1),        NSYSCK)
            CALL UCOPY  (KYP2CK, IARGCD(NSYSCK+1), NSYSCK)
            CALL CDPRNT (LPRTCD, '(/,'' CDPURP : Error in RZRENK whil'//
     +           'e writing data for '//PATHY//''',/(10X,7I12))',
     +           IARGCD, 2*NSYSCK)
          ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 996
        ENDIF
   50 CONTINUE
*
* *** Now delete the keys and subdirectories beyond NKEEP
*
   55 IF (NKEEP.GT.0) THEN
        NK1    = NKEEP + 1
      ELSE
        NK1    = NKEYS + NKEEP + 1
        IF (NK1.LT.2) NK1  = 2
        NOBJCS = 0
      ENDIF
      DO 60 JKK = NK1, NKEYS
        IKK    = NKEYS + NK1 - JKK
        CALL RZCDIR (PATHY, ' ')
        NKEYCK = IQUEST(7)
        NWKYCK = IQUEST(8)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        KPNT   = IUHUNT (IKK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                   NKEYCK*KST, KST)
        IF (KPNT.NE.0) THEN
          NK     = (KPNT - MPSRCD) / KST + 1
        ELSE
          NK     = IKK
        ENDIF
        CALL CDKEYR (NK, NWKYCK, KYP2CK)
        CALL RZDELK (KYP2CK, ICDUM, 'C')
        CALL CDPATH (TOP1CT, IKK)
        CALL RZDELT (TOP1CT)
        IF (IQUEST(1).NE.0) THEN
          IRC    = 70
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURP : Error'//
     +    ' in deleting partition '//TOP1CT(1:8)//' in '//PATHY//''')',
     +    IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 996
        ENDIF
   60 CONTINUE
#endif
*
      IRC    = 0
#if !defined(CERNLIB__P3CHILD)
*
* *** Drop the stored banks
*
  996 IF (NOBJCS.GT.0) THEN
        DO 997 IK = 1, NOBJCS
          IF (LOBJCS(IK).NE.0) CALL MZDROP (IDISCD, LOBJCS(IK), 'L')
  997   CONTINUE
      ENDIF
  998 IF (PATHL.NE.' ') THEN
        CALL RZCDIR (PATHL, ' ')
        CALL RZFREE ('CDPURP')
      ENDIF
#endif
*                                                             END CDPURP
  999 END
