*
* $Id: cdrenk.F,v 1.1.1.1 1996/02/28 16:24:34 mclareni Exp $
*
* $Log: cdrenk.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:34  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDRENK (PATHN, KEYO, KEYN, IRC)
*     ==========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDRENK (PATHN, KEYO, KEYN, IRC*)                        *
*                                                                      *
*   Changes the key elements of an existing object to new values       *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     KEYO     Array containing the old key elements                   *
*     KEYN     Array containing the new key elements                   *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user,   CDFZUP                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =191 : Illegal path name                               *
*               =192 : Illegal KEYO values (no matching object)        *
*               =194 : Error in getting the IO descriptor              *
*               =195 : Error in FZOUT in saving the journal file       *
*               =196 : Error in RZRENK in renaming key values          *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/cinitl.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      PARAMETER       (NLEVM=20)
      DIMENSION       KEYO(9), KEYN(9), NLCUR(NLEVM)
      CHARACTER       PATHN*(*), PATHY*80, CHFOR*100, CFORM(6)*1
      CHARACTER       CHCUR(NLEVM)*1
      DATA            CFORM /'B', 'I', 'F', 'D', 'H', 'A'/
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Set the current directory path name
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)   GO TO 999
      PATHY  = PAT1CT
      NCHAR  = LENOCC (PATHY)
      KST    = NWKYCK + 1
      CALL CDKEYT
      IF (NKEYCK.NE.0)  THEN
        IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      ELSE
        IRC    = 192
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRENK : Illegal'//
     +  ' key in Path '//PATHY(1:NCHAR)//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** See if the key matches with one existing
*
      IF (IOPTP.EQ.0) THEN
        KPNT   = IUHUNT (KEYO(IDHKSN), IQ(KOFSCD+LCDRCD+IKDRCD+IDHKSN),
     +                   NKEYCK*KST, KST)
        IF (KPNT.EQ.0) THEN
          IRC    = 192
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRENK : Illeg'//
     +    'al key in Path '//PATHY(1:NCHAR)//''')', IARGCD, 0)
#endif
          GO TO 999
        ELSE
          NK     = (KPNT - IDHKSN) / KST + 1
          CALL CDKEYR (NK, NWKYCK, KEYNCK)
          DO 10 IK = 1, NWKYCK
            IF (KEYNCK(IK).NE.KEYO(IK)) THEN
              IRC    = 192
#if defined(CERNLIB__DEBUG)
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRENK : I'//
     +        'llegal key in Path '//PATHY(1:NCHAR)//''')', IARGCD, 0)
#endif
              GO TO 999
            ENDIF
   10     CONTINUE
        ENDIF
*
      ELSE
        NKEYS   = NKEYCK
        DO 20 JK = 1, NKEYS
          IK     = NKEYS - JK + 1
          KPNT   = IUHUNT (IK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                     NKEYS*KST, KST)
          IF (KPNT.GT.0) THEN
            KPNT   = KOFSCD + LCDRCD + IKDRCD + KPNT - MPSRCD
          ELSE
            KPNT   = KOFSCD + LCDRCD + IKDRCD + (IK - 1) * KST
          ENDIF
          IF (KEYO(IDHKSN).LE.IQ(KPNT+MOBJCD)) GO TO 20
          NK     = (KPNT - KOFSCD - LCDRCD -IKDRCD) / KST + 1
          CALL CDKEYR (NK, NWKYCK, KYP1CK)
          CALL UCOPY (KYP1CK, KYP2CK, NWKYCK)
          CALL CDPATH (TOP1CT, IK)
          CALL RZCDIR (TOP1CT, ' ')
          IF (IQUEST(1).NE.0) THEN
            IRC    = 191
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRENK : Ill'//
     +      'egal Path Name '//PATHY(1:NCHAR)//TOP1CT(1:8)//''')', IQ,0)
#endif
            GO TO 999
          ENDIF
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          KPNT   = IUHUNT (KEYO(IDHKSN),IQ(KOFSCD+LCDRCD+IKDRCD+IDHKSN),
     +                     NKEYCK*KST, KST)
          IF (KPNT.EQ.0) THEN
            IRC    = 192
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRENK : Ill'//
     +      'egal key in Path '//PATHY(1:NCHAR)//TOP1CT(1:8)//''')',
     +      IARGCD, 0)
#endif
            GO TO 999
          ELSE
            NK     = (KPNT - IDHKSN) / KST + 1
            CALL CDKEYR (NK, NWKYCK, KEYNCK)
            DO 15 IK = 1, NWKYCK
              IF (KEYNCK(IK).NE.KEYO(IK)) THEN
                IRC    = 192
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRENK :'//
     +          ' Illegal key in Path '//PATHY(1:NCHAR)//TOP1CT(1:8)//
     +          ''')', IARGCD, 0)
#endif
                GO TO 999
              ENDIF
   15       CONTINUE
            GO TO 25
          ENDIF
   20   CONTINUE
        IRC    = 192
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRENK : Illegal'//
     +  ' key in Path '//PATHY(1:NCHAR)//TOP1CT(1:8)//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Find the appropriate FZ file number
*
   25 LUFZCF = LUFZCD
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
      IF (IOPPCD.NE.0) THEN
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
        LUFZCF = LUFMCD
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
        CALL CDSTSV (TOPNCD, 0, IRC)
        IF (IRC.NE.0)              GO TO 999
#endif
#if defined(CERNLIB__P3CHILD)
        LUFZCF = LODBP3
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
      ENDIF
#endif
*
* *** Get the IO descriptor for the header
*
      IF (LUFZCF.GT.0) THEN
        NLEV   = 1
        NCUR   = 5
        IFORO  = 2
        CHCUR(NLEV) = CFORM(IFORO)
        DO 40 J = 1, 2
          DO 30 I = 1, NWKYCK
            IFORM  = IOTYCK(I)
            IF (IFORM.EQ.6) IFORM = 5
            IF (IFORM.EQ.IFORO) THEN
              NCUR   = NCUR + 1
            ELSE
              NLCUR(NLEV) = NCUR
              IF (NLEV.GE.NLEVM) THEN
                IRC    = 194
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRENK :'//
     +          ' Cannot get IO descriptor '//PATHY//''')', IARGCD, 0)
#endif
                GO TO 999
              ENDIF
              NLEV   = NLEV + 1
              CHCUR(NLEV) = CFORM(IFORM)
              NCUR   = 1
              IFORO  = IFORM
            ENDIF
   30     CONTINUE
   40   CONTINUE
        NLCUR(NLEV) = NCUR
*
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
        WRITE (CHFOR, 2001) (NLCUR(I), CHCUR(I), I = 1, NLEV)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
        CHFOR  = ' '
        II     = 1
        DO 45 I = 1, NLEV
          CALL UTWRIT (CHFOR(II:II+1), '(I2)', NLCUR(I))
          II     = II + 2
          CHFOR(II:II) = CHCUR(I)
          II     = II + 2
   45   CONTINUE
#endif
        II     = 4 *NLEV
        CHFOR = CHFOR(1:II)//' -H'
        CALL MZIOCH (IOFMCF, NWFMCF, CHFOR(1:II+3))
*
*  **   Fill up the header
*
        NWDP   = (NCHAR + 3) / 4
        NWDH   = NWDP + 2 * NWKYCK + 5
        IHEDCF(MACTCF) = 5
        IHEDCF(MNKYCF) = NWKYCK
        IHEDCF(MOPTCF) = 0
        IHEDCF(MPATCF) = NWDP
        IHEDCF(MPRECF) = 0
        CALL UCOPY (KEYO,  IHEDCF(MPRECF+1),        NWKYCK)
        CALL UCOPY (KEYN,  IHEDCF(MPRECF+NWKYCK+1), NWKYCK)
        CALL UCTOH (PATHY, IHEDCF(MPRECF+2*NWKYCK+1), 4, 4*NWDP)
      ENDIF
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
*
      IF (IOPPCD.NE.0) THEN
        CALL CDWLOK (IRC)
        IF (IRC.NE.0)                    GO TO 999
      ENDIF
#endif
*
*  ** Write the sequential output if needed
*
      IF (LUFZCF.GT.0) THEN
#if defined(CERNLIB__P3CHILD)
        RNDBP3 = 'CDRENK '
        NWDBP3 = 2
        CALL UCTOH ('JOURNAL ', IWDBP3, 4, 8)
        CALL CDCHLD
        IRC    = IQDBP3
        IF (IRC.NE.0)                    GO TO 999
#endif
        CALL FZOUT (LUFZCF, IDISCD, 0, 1, 'Z', IOFMCF, NWDH, IHEDCF)
        IF (IQUEST(1).NE.0) THEN
          IRC    = 195
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRENK : Error'//
     +    ' in FZOUT while writing Data for '//PATHY(1:60)//''')',
     +    IARGCD, 0)
#endif
          GO TO 999
        ENDIF
      ENDIF
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
      IF (IOPPCD.NE.0)                           GO TO 998
#endif
#if !defined(CERNLIB__P3CHILD)
*
* *** Take necessary action for partitioned and nonpartitioned datasets
*
      IF (IOPSCD.NE.0) CALL RZLOCK ('CDRENK')
      CALL RZRENK (KEYO, KEYN)
      IERR   = IQUEST(1)
      IF (IOPSCD.NE.0) CALL RZFREE ('CDRENK')
      IF (IERR.NE.0) THEN
        IRC    = 196
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRENK : Error'//
     +  ' in RZRENK while writing data for '//PATHY(1:60)//''')',
     +  IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
        GO TO 999
      ENDIF
      IF (IOPTP.NE.0) THEN
        CALL CDPVAL (KEYN)
        CALL RZCDIR (PATHY, ' ')
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        NKEYCK = IQUEST(7)
*
*  **   Rename Keys 3 and 4 of the latest subdirectory
*
        IF (IOPSCD.NE.0) CALL RZLOCK ('CDRENK')
        CALL RZRENK (KYP1CK, KYP2CK)
        IERR   = IQUEST(1)
        IF (IOPSCD.NE.0) CALL RZFREE ('CDRENK')
        IF (IERR.NE.0) THEN
          IRC    = 196
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRENK : Error'//
     +    ' in RZRENK while writing data for '//PATHY(1:60)//''')',
     +    IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 999
        ENDIF
      ENDIF
#endif
*
  998 CONTINUE
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
      IF (IOPPCD.NE.0) THEN
        IOPBCA = 0
        CALL CDCWSV (IRC)
      ENDIF
#endif
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
*
 2001 FORMAT (20(I2,A1,1X))
#endif
*                                                             END CDRENK
  999 END
