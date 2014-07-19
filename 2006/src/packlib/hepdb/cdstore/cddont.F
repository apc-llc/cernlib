*
* $Id: cddont.F,v 1.1.1.1 1996/02/28 16:24:33 mclareni Exp $
*
* $Log: cddont.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:33  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDDONT (PATHN, IUDIV, LSUP, KEY, CHOPT, IRC)
*     =======================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDDONT (PATHN, IUDIV, LSUP, KEY, CHOPT, IRC*)           *
*                                                                      *
*   Stores data from memory to disk. If data exist with the same user  *
*   keys, the old object is physically replaced. This mode is not      *
*   recommended for all calibration data.                              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IUDIV    Division index of the user data bank                    *
*     LSUP     Address of bank in memory where data reside             *
*     KEY      Vector of keys                                          *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*          H   Insertion time as supplied by user to be honoured       *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user,   CDFZUP                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 71 : Illegal path name                               *
*               = 73 : RZ error during writing to disk                 *
*               = 76 : Cannot form the IO descriptor for the FZ header *
*               = 77 : FZOUT fails to write on to the sequential file  *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      PARAMETER       (NLEVM=20)
      DIMENSION       KEY(9), LSUP(9), KEYV(MXDMCK), NLCUR(NLEVM)
      CHARACTER       CHOPT*(*), PATHN*(*), CHOPF*4, PATHY*80, PATHL*80
      CHARACTER       CFORM(6)*1, CHFOR*100, CHCUR(NLEVM)*1
      DATA            CFORM /'B', 'I', 'F', 'D', 'H', 'A'/
#include "zebra/q_jbit.inc"
* Ignoring t=pass
#include "zebra/q_sbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Decode the character option (no compression of data)
*
      LREFCD(1) = LSUP(1)
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)             GO TO 999
      IOPYCA = 1
*
* *** Load top directory information; gets in PAT1CT complete path name
*
      CALL CDLDUP (PATHN, 1, IRC)
      IF (IRC.NE.0)             GO TO 999
      PATHY  = PAT1CT
      NCHAR  = LENOCC (PATHY)
      NWKEY  = NWKYCK
      PATHL  = ' '
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
      IF (IRC.NE.0)             GO TO 999
      IF (IOPBCA.EQ.0) THEN
        LUFZCF = LUFZCD
      ELSE
        LUFZCF = LUBKCD
      ENDIF
*
* *** Prepare the Key vector array
*
      CALL VZERO (KEYVCK, MXDMCK)
      DO 10 I = 1, NPARCD
        KEYVCK(NOF1CK+2*I-1) = KEY(NOF1CK+2*I-1)
        KEYVCK(NOF1CK+2*I)   = KEY(NOF1CK+2*I)
   10 CONTINUE
      KEYVCK(IDHUSI) = KEY(IDHUSI)
      IF (IOPHCA.NE.0) THEN
        KEYVCK(IDHINS) = KEY(IDHINS)
      ELSE
        CALL DATIME (IDATE, ITIME)
        CALL CDPKTM (IDATE, ITIME, KEYVCK(IDHINS), IRC)
      ENDIF
      IF (NWKYCK.GT.NSYSCK) THEN
        DO 15 NK = NSYSCK+1, NWKYCK
          IOKYCA(NK) = 1
          KEYVCK(NK) = KEY(NK)
   15   CONTINUE
      ENDIF
*
* *** Encode the character option, IO descriptor for the header
*
      IF (IOPPCD.NE.0.OR.LUFZCF.GT.0) THEN
        IF (IOPHCA.NE.0) THEN
          CHOPF  = 'H'//CHOPT
        ELSE
          CHOPF  = CHOPT
        ENDIF
        NDOP   = LENOCC (CHOPF)
        NDOP   = (NDOP + 3) / 4
*
        NLEV   = 1
        NCUR   = 5
        IFORO  = 2
        CHCUR(NLEV) = CFORM(IFORO)
        DO 20 I = 1, NWKYCK
          IFORM  = IOTYCK(I)
          IF (IFORM.EQ.6) IFORM = 5
          IF (IFORM.EQ.IFORO) THEN
            NCUR   = NCUR + 1
          ELSE
            NLCUR(NLEV) = NCUR
            IF (NLEV.GE.NLEVM) THEN
              IRC    = 76
#if defined(CERNLIB__DEBUG)
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDONT : '//
     +        'Cannot get IO descriptor '//PATHY//''')', IARGCD, 0)
#endif
              GO TO 999
            ENDIF
            NLEV   = NLEV + 1
            CHCUR(NLEV) = CFORM(IFORM)
            NCUR   = 1
            IFORO  = IFORM
          ENDIF
   20   CONTINUE
        NLCUR(NLEV) = NCUR
*
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
        WRITE (CHFOR, 2001) (NLCUR(I), CHCUR(I), I = 1, NLEV)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
        CHFOR  = ' '
        II     = 1
        DO 25 I = 1, NLEV
          CALL UTWRIT (CHFOR(II:II+1), '(I2)', NLCUR(I))
          II     = II + 2
          CHFOR(II:II) = CHCUR(I)
          II     = II + 2
   25   CONTINUE
#endif
        II     = 4 *NLEV
        CHFOR = CHFOR(1:II)//' -H'
        CALL MZIOCH (IOFMCF, NWFMCF, CHFOR(1:II+3))
*
*  **   Complete the header
*
        NWDP   = (NCHAR + 3) / 4
        NWDH   = NWDP + NDOP + NWKYCK + 5
        IHEDCF(MACTCF) = 9
        IHEDCF(MNKYCF) = NWKYCK
        IHEDCF(MOPTCF) = NDOP
        IHEDCF(MPATCF) = NWDP
        IHEDCF(MPRECF) = 0
        IF (NDOP.GT.0)
     +    CALL UCTOH (CHOPF, IHEDCF(MPRECF+NWKYCK+1), 4, 4*NDOP)
        CALL UCTOH (PATHY, IHEDCF(MPRECF+NWKYCK+NDOP+1), 4, 4*NWDP)
        CALL UCOPY (KEYVCK, IHEDCF(MPRECF+1), NWKYCK)
      ENDIF
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
      IF (IOPPCD.NE.0) THEN
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
        LUFZCF = LUFMCD
        CALL CDWLOK (IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
        CALL CDSTSV (TOPNCD, 0, IRC)
#endif
#if defined(CERNLIB__P3CHILD)
        LUFZCF = LODBP3
        RNDBP3 = 'CDDONT '
        NWDBP3 = 2
        CALL UCTOH ('JOURNAL ', IWDBP3, 4, 8)
        CALL CDCHLD
        IRC    = IQDBP3
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        IF (IRC.NE.0)           GO TO 999
      ENDIF
#endif
*
*  ** Write the sequential output if needed
*
      IF (LUFZCF.GT.0) THEN
        CALL FZOUT (LUFZCF, IUDIV, LREFCD(1), 1, 'L', IOFMCF, NWDH,
     +              IHEDCF)
        IF (IQUEST(1).NE.0) THEN
          IRC    = 77
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDONT : Error'//
     +    ' in FZOUT while writing Data for '//PATHY//''')', IARGCD, 0)
#endif
          GO TO 999
        ENDIF
      ENDIF
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
      IF (IOPPCD.NE.0) THEN
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
* *** Lock the directory if necessary
*
      IF (IOPPCD.EQ.0.AND.IOPSCD.NE.0) THEN
        CALL RZLOCK ('CDDONT')
        PATHL  = PATHY
      ENDIF
*
* *** Look if object already exists with similar user keys
*
      IF (IOPTP.EQ.0) THEN
        CALL CDHUNT (KEYV)
*
      ELSE
*
        NKEYS  = NKEYCK
        DO 30 JK = 1, NKEYS
          ICURCT = NKEYS + 1 - JK
          CALL CDPATH (TOP1CT, ICURCT)
          IF (JK.NE.1) THEN
            CALL RZCDIR (PATHY, ' ')
            IF (IQUEST(1).NE.0) GO TO 991
          ENDIF
*
          CALL RZCDIR (TOP1CT, ' ')
          IF (IQUEST(1).NE.0)   GO TO 991
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          CALL CDHUNT (KEYV)
          IF (KEYV(1).GT.0)     GO TO 35
*
   30   CONTINUE
*
      ENDIF
*
* *** Either update an old record
*
   35 IF (KEYV(1).GT.0) THEN
        CALL RZOUT (IUDIV, LREFCD(1), KEYV, ICYCLE, 'L')
        IER    = IQUEST(1)
        IKDRCD = IQ(KOFSCD+LCDRCD+KLKDCD)
        NKEYCK = IQ(KOFSCD+LCDRCD+KNKDCD)
        CALL RZPURG (0)
        IF (IER.NE.0) IQUEST(1) = IER
*
* *** Or enter a new one
*
      ELSE
*
*  **   Take necessary action for partitioned and nonpartitioned dataset
*
        IF (IOPTP.EQ.0) THEN
          KOBJ   = 0
        ELSE
          CALL RZCDIR (PATHY, ' ')
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          KPNT   = IUHUNT (NKEYS, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                     NKEYS*KST, KST)
          IF (KPNT.NE.0) THEN
            NK     = (KPNT - MPSRCD) / KST + 1
          ELSE
            NK     = NKEYS
          ENDIF
          CALL CDKEYR (NK, NWKYCK, KYP1CK)
          KOBJ   = KYP1CK(MOBJCD)
          MXKP   = KYP1CK(MXKPCD)
          CALL CDPATH (TOP1CT, NKEYS)
          CALL RZCDIR (TOP1CT, ' ')
          IF (IQUEST(1).NE.0)   GO TO 991
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
*
*  **     Make a different subdirectory if there are too many keys
*
          IF (NKEYCK.GE.MXKP) THEN
            CHFOR = ' '
            DO 40 I = 1, NWKYCK
              IF (I.EQ.1) THEN
                CHFOR  = CFORM(IOTYCK(I))
              ELSE
                CHFOR  = CHFOR(1:I-1)//CFORM(IOTYCK(I))
              ENDIF
   40       CONTINUE
            KEY7CK = KEYVCK(IDHINS)
            IF (ICMPCD.EQ.2) THEN
              CHOPF  = 'ZP'
            ELSE IF (ICMPCD.NE.0) THEN
              CHOPF = 'CP'
            ELSE
              CHOPF = 'P '
            ENDIF
            CALL CDMKDI (PATHY, NWKEY, CHFOR, CTAGCK, MXKP, IPRECD,
     +                   DELTCD, CHOPF, IRC)
            IF (IRC.NE.0)       GO TO 999
            CALL RZCDIR (PATHY, ' ')
            NKEYCK = IQUEST(7)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            KPNT   = IUHUNT (NKEYCK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                       NKEYCK*KST, KST)
            IF (KPNT.NE.0) THEN
              NK     = (KPNT - MPSRCD) / KST + 1
            ELSE
              NK     = NKEYCK
            ENDIF
            CALL CDKEYR (NK, NWKYCK, KYP1CK)
            KOBJ   = KYP1CK(MOBJCD)
            CALL CDPATH (TOP1CT, NKEYCK)
            CALL RZCDIR (TOP1CT, ' ')
            IF (IQUEST(1).NE.0) GO TO 991
            NKEYCK = IQUEST(7)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
          ENDIF
          CALL UCOPY (KYP1CK, KYP2CK, NWKYCK)
*
        ENDIF
*
*  **   Check the number of keys
*
        LOBJ   = KOBJ
        DO 45 IK = 1, NKEYCK
          IP     = KOFSCD + LCDRCD + IKDRCD + (IK-1)*KST + IDHKSN
          IF (IQ(IP).GT.LOBJ) LOBJ = IQ(IP)
   45   CONTINUE
        KEYVCK(IDHKSN) = LOBJ + 1
        KEYVCK(IDHFLG) = MSBIT1 (KEYVCK(IDHFLG), JRZUCD)
        CALL RZOUT (IUDIV, LREFCD(1), KEYVCK, ICYCLE, 'L')
        IKDRCD = IQ(KOFSCD+LCDRCD+KLKDCD)
        NKEYCK = IQ(KOFSCD+LCDRCD+KNKDCD)
        IER    = IQUEST(1)
        CALL CDPVAL (KEYVCK(1))
        CALL RZRENK (KYP1CK, KYP2CK)
        IF (IER.NE.0) IQUEST(1) = IER
      ENDIF
      IF (IQUEST(1).NE.0) THEN
        IRC    = 73
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDONT : Error i'//
     +    'n RZ while writing Data for '//PATHY//TOP1CT(1:8)//''')',I,0)
#endif
#if !defined(CERNLIB__P3CHILD)
      ENDIF
      GO TO 998
#endif
*
* *** Error messages
*
  991 IRC    = 71
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDONT : Illegal '//
     +  'Path Name '//PATHY//TOP1CT(1:8)//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
*
  998 IF (PATHL.NE.' ') THEN
        CALL RZCDIR (PATHL, ' ')
        CALL RZFREE ('CDDONT')
      ENDIF
#endif
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
*
 2001 FORMAT (20(I2,A1,1X))
#endif
*                                                             END CDDONT
  999 END
