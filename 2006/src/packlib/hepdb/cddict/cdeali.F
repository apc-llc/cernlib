*
* $Id: cdeali.F,v 1.1.1.1 1996/02/28 16:24:08 mclareni Exp $
*
* $Log: cdeali.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:08  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDEALI (PATHN, ALIAS, IFLAG, IRC)
*     ============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDEALI (PATHN, ALIAS, IFLAG, IRC*)                      *
*                                                                      *
*   Enter the alias name of a given directory either only for this     *
*   session or permanently in the data base.                           *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string specifying the directory path name     *
*     ALIAS    Character string specifying the alias name              *
*     IFLAG    Flag (0 if to be entered only for this session;         *
*                    1 if to be entered for this session and D.B.)     *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDALIA, CDFZUP                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =146 : Illegal path name                               *
*               =147 : Dictionary directory not found in memory        *
*               =148 : FZOUT fails to write on the sequential file     *
*               =149 : Error in RZ for writing to the R.A. file        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      CHARACTER       PATHN*(*), ALIAS*(*), CALI*8, PATHY*80, PATHL*80
*
*     ------------------------------------------------------------------
*
* *** Load the top directory information
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)       GO TO 999
      CALI   = ALIAS
      PATHL  = ' '
      PATHY  = PAT1CT
      NCHAR  = LENOCC (PATHY)
*
* *** Find the unique directory identifier from the pathname
*
      CALL CDGPID (PATHY, IDN)
      IF (IDN.LE.0) THEN
        IRC    = 146
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDEALI : Illegal'//
     +  ' pathname '//PATHY//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Modify in memory
*
      LUFZCF = LUFZCD
      LFIXCD = LQ(KOFUCD+LBUPCD-KLDICD)
      IF (LFIXCD.EQ.0) THEN
        IRC    = 147
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDEALI : DICTION'//
     +  'ARY not found for '//TOPNCD//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
      IPNT   = KOFUCD + LFIXCD + (IDN - 1) * NWITCD + 1
      CALL UCTOH (CALI, IQ(IPNT+MDCALI), 4, 8)
      IF (IFLAG.EQ.0)  THEN
        LFIXCD = 0
        GO TO 999
      ENDIF
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
      IF (IOPPCD.NE.0) THEN
        LFIXCD = 0
        IRC    = 0
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
        IF (IRC.NE.0) GO TO 999
      ENDIF
#endif
*
* *** Get the name of the correct DICTIONARY directory
*
      PAT2CT = '//'//TOPNCD(1:NCHRCD)//'/DICTIONARY'
      NCHRD  = NCHRCD + 13
*
      IF (LUFZCF.GT.0) THEN
*
*  **   Prepare the header containing all the information
*
        NCHD   = (NCHRD + 3) / 4
        NCHP   = (NCHAR + 3) / 4
        CALL MZIOCH (IOFMCF, NWFMCF, '6I -H')
        IHEDCF(MACTCF) = 7
        IHEDCF(MNKYCF) = 0
        IHEDCF(MOPTCF) = 0
        IHEDCF(MPATCF) = NCHD
        IHEDCF(MFLGCF) = IFLAG
        IHEDCF(MWDPCF) = NCHP
        NPNT1  = NCHD  + MWDPCF + 1
        NPNT2  = NPNT1 + 2
        CALL UCTOH (PAT2CT, IHEDCF(MWDPCF+1), 4, 4*NCHD)
        CALL UCTOH (CALI,   IHEDCF(NPNT1),    4, 8)
        CALL UCTOH (PATHY,  IHEDCF(NPNT2),    4, 4*NCHP)
        NWDH   = NPNT2 + NCHP - 1
*
*  **   Now write on the sequential output
*
#if defined(CERNLIB__P3CHILD)
        RNDBP3 = 'CDEALI '
        NWDBP3 = 2
        CALL UCTOH ('JOURNAL ',IWDBP3, 4, 8)
        CALL CDCHLD
        IRC    = IQDBP3
        IF (IRC.NE.0) GO TO 999
#endif
        CALL FZOUT (LUFZCF, IDIVCD, 0, 1, 'Z', IOFMCF, NWDH, IHEDCF)
        IF (IQUEST(1).NE.0) THEN
          LFIXCD = 0
          IRC    = 148
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDEALI : FZOUT'//
     +    ' error for path name '//PATHY//''')', IARGCD, 0)
#endif
          GO TO 999
        ENDIF
      ENDIF
*
      IRC    = 0
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
* *** Server environment, Public mode
*
      IF (IOPPCD.NE.0) THEN
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
        IOPBCA = 0
        CALL CDCWSV (IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        GO TO 999
      ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
*
* *** Now save this information in the data base
*
      CALL RZCDIR (PAT2CT, ' ')
      IF (IQUEST(1).NE.0) THEN
        LFIXCD = 0
        IRC    = 147
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDEALI : DICTION'//
     +  'ARY not found for '//TOPNCD//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
        GO TO 999
      ENDIF
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      ISTP   = NWKYCK + 1
*
* *** Lock the directory if necessary
*
      IF (IOPSCD.NE.0) THEN
        CALL RZCDIR (PAT2CT, ' ')
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        CALL RZLOCK ('CDEALI')
        PATHL  = PAT2CT
      ENDIF
*
      IF (NKEYCK.GT.0) THEN
        IPNT   = KOFSCD + LCDRCD + IKDRCD
        IMIN   = IUHUNT (-1, IQ(IPNT+IDHKSN), NKEYCK*ISTP, ISTP)
        IF (IMIN.GT.0) THEN
          IMIN   = (IMIN - IDHKSN) / ISTP + 1
          CALL CDKEYT
          CALL CDKEYR (IMIN, NWKYCK, KEYNCK)
          ISTEP  = 1
          CALL RZOUT (IDIVCD, LFIXCD, KEYNCK, ICYCLE, 'S')
          LFIXCD = 0
          IF (IQUEST(1).NE.0) THEN
            IRC        = 149
            IQUEST(11) = ISTEP
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDEALI : RZ '//
     +      'error in step '',I3,'' for path name '//PAT2CT//''')',
     +      IQUEST(11), 1)
#endif
#if !defined(CERNLIB__P3CHILD)
            GO TO 998
          ENDIF
          IKDRCD = IQ(KOFSCD+LCDRCD+KLKDCD)
          NKEYCK = IQ(KOFSCD+LCDRCD+KNKDCD)
          ISTEP  = 2
          CALL RZPURG (0)
          IF (IQUEST(1).NE.0) THEN
            IRC        = 149
            IQUEST(11) = ISTEP
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDEALI : RZ '//
     +      'error in step '',I3,'' for path name '//PAT2CT//''')',
     +      IQUEST(11), 1)
#endif
#if !defined(CERNLIB__P3CHILD)
            GO TO 998
          ENDIF
        ENDIF
      ELSE
        IMIN   = 0
      ENDIF
      LFIXCD = 0
      IF (IMIN.EQ.0) THEN
        IRC    = 147
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDEALI : DICTION'//
     +  'ARY not found for '//TOPNCD//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
      ELSE
        IRC    = 0
      ENDIF
*
* *** Free the locked directory
*
  998 IF (PATHL.NE.' ') THEN
        CALL RZCDIR (PATHL, ' ')
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        CALL RZFREE ('CDEALI')
      ENDIF
#endif
*                                                             END CDEALI
  999 END
