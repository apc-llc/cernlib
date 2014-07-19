*
* $Id: cdsnam.F,v 1.1.1.1 1996/02/28 16:24:10 mclareni Exp $
*
* $Log: cdsnam.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:10  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDSNAM (IFLG, KEYS, LBK, LUNFZ, IRC)
*     ===============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDSNAM (IFLG, KEYS, LBK, LUNFZ, IRC*)                   *
*                                                                      *
*   Saves the help file or name of the data objects in the journal     *
*   file or/and in the data base. The data part contains the encoded   *
*   data and the header contains Action code (6), number of keys,      *
*   number of characters for option (0) and pathname, followed by the  *
*   flag (1 for help file; 2 for data names), the keys and the path    *
*   name.                                                              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IFLG     Flag for help information(1) or name of data elements(2)*
*     KEYS     Vector of keys                                          *
*     LBK      Address of the data bank to be stored (or 0)            *
*     LUNFZ    Logical unit number of FZ file (or 0)                   *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDDINF, CDEHLP, CDENAM, CDFZUP                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =148 : FZOUT fails to write on the sequential file     *
*               =149 : Error in RZ for writing to the R.A. file        *
*               =152 : Illegal flag (IFLAG)                            *
*               =153 : FZIN error for reading the data structure       *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/ckkeys.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      CHARACTER       PATHN*32, PATHL*80, CHOP*4
      DIMENSION       KEYO(MXDMCK), LBK(9), KEYS(9)
#include "zebra/q_sbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Find the path name for storing the information
*
      LREFCD(2) = LBK(1)
      PATHL  = ' '
      IF (IFLG.EQ.1) THEN
        PATHN  = '//'//TOPNCD(1:NCHRCD)//'/HELP'
        NCHAR  = NCHRCD + 7
      ELSE IF (IFLG.EQ.2) THEN
        PATHN  = '//'//TOPNCD(1:NCHRCD)//'/DICTIONARY'
        NCHAR  = NCHRCD + 13
      ELSE
        IRC        = 152
        IQUEST(11) = IFLG
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDSNAM : Illegal'//
     +  ' flag '',I12)', IQUEST(11), 1)
#endif
        GO TO 999
      ENDIF
*
* *** Fill up the key vector
*
      IF (LUNFZ.GT.0) THEN
        CALL UCOPY (KEYS(1), KEYNCK, NSYSCK)
      ELSE
        CALL VZERO (KEYNCK, NSYSCK)
        KEYNCK(IDHKSN) = KEYS(1)
        CALL DATIME (IDATE, ITIME)
        KEYNCK(IDHFLG) = MSBIT1 (KEYNCK(IDHFLG), JRZUCD)
        IF (IFLG.EQ.1) KEYNCK(IDHFLG) = MSBIT1 (KEYNCK(IDHFLG), JASFCD)
        CALL CDPKTM (IDATE, ITIME, KEYNCK(IDHINS), IRC)
      ENDIF
*
      LUFZCF = LUFZCD
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
*
      IF (IOPPCD.NE.0) THEN
        IRC    = 0
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
        CALL CDWLOK (IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
        CALL CDSTSV (TOPNCD, 0, IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
        IRC    = IQUEST(1)
#endif
#if defined(CERNLIB__P3CHILD)
        LUFZCF = LODBP3
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        IF (IRC.NE.0)       GO TO 999
      ENDIF
#endif
*
* *** Now get the bank (either supplied by user or from FZ file)
*
      IF (IOPDCA.NE.0) THEN
        LBDACD = 0
      ELSE IF (LUNFZ.EQ.0) THEN
        LBDACD = LREFCD(2)
      ELSE
#if defined(CERNLIB__P3CHILD)
        RNDBP3 = 'CDSNAM '
        NWDBP3 = 1
        IWDBP3(1) = LUNFZ
        CALL CDCHLD
        IRC    = IQDBP3
        IF (IRC.NE.0)       GO TO 999
#endif
        CALL FZIN (LUNFZ, IDISCD, LBDACD, 2, 'A', 0, 0)
        IF (IQUEST(1).GT.0) THEN
          IRC    = -1
          GO TO 999
        ELSE IF (IQUEST(1).NE.0) THEN
          IQUEST(11) = IQUEST(1)
          IRC        = 153
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDSNAM : FZIN '//
     +    ' error type '',I12)', IQUEST(11), 1)
#endif
          GO TO 999
        ENDIF
      ENDIF
*
      IF (LUFZCF.GT.0) THEN
*
*  **   Prepare the header containing all the information
*
        NCHD   = (NCHAR + 3) / 4
        IF (IOPDCA.NE.0) THEN
          CHOP   = 'D'
          NDOP   = 1
        ELSE
          NDOP   = 0
          CHOP   = ' '
        ENDIF
        CALL MZIOCH (IOFMCF, NWFMCF, '12I -H')
        IHEDCF(MACTCF) = 6
        IHEDCF(MNKYCF) = NSYSCK
        IHEDCF(MOPTCF) = NDOP
        IHEDCF(MPATCF) = NCHD
        IHEDCF(MFLGCF) = IFLG
        CALL UCOPY (KEYNCK, IHEDCF(MFLGCF+1), NSYSCK)
        NPNT   = NSYSCK + MFLGCF + 1
        IF (NDOP.GT.0) THEN
          CALL UCTOH (CHOP, IHEDCF(NPNT), 4, 4*NDOP)
          NPNT   = NPNT + NDOP
        ENDIF
        CALL UCTOH (PATHN, IHEDCF(NPNT), 4, 4*NCHD)
        NWDH   = NPNT + NCHD - 1
*
*  **   Now write on the sequential output
*
#if defined(CERNLIB__P3CHILD)
        RNDBP3 = 'CDSNAM '
        NWDBP3 = 2
        CALL UCTOH ('JOURNAL ', IWDBP3, 4, 8)
        CALL CDCHLD
        IRC    = IQDBP3
        IF (IRC.NE.0)       GO TO 999
#endif
        IF (IOPDCA.NE.0) THEN
          CHOP   = 'Z'
        ELSE
          CHOP   = ' '
        ENDIF
        CALL FZOUT (LUFZCF, IDISCD, LBDACD, 1,CHOP, IOFMCF, NWDH,IHEDCF)
        IF (IQUEST(1).NE.0) THEN
          IF (LUNFZ.NE.0.AND.LBDACD.GT.0) CALL MZDROP(IDISCD,LBDACD,'L')
          IRC    = 148
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDSNAM : FZOUT'//
     +    ' error for path name '//PATHN//''')', IARGCD, 0)
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
        ISAVW  = IQUEST(9)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__ONLINE))
        IF (LUNFZ.NE.0.AND.LBDACD.GT.0) CALL MZDROP (IDISCD,LBDACD,'L')
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
        IQUEST(9) = ISAVW
        IOPBCA = 0
        CALL CDCWSV (IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        GO TO 999
      ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
*
* *** Enter the bank into data base
*
      ISTEP  = 1
      CALL RZCDIR (PATHN, ' ')
      IF (IQUEST(1).NE.0)   GO TO 900
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      IF (IOPSCD.NE.0) THEN
        CALL RZLOCK ('CDSNAM')
        PATHL  = PATHN
      ENDIF
      CALL CDKEYT
      ISTP   = NWKYCK + 1
      ISTEP  = 2
      KPNT   = IUHUNT (KEYNCK(IDHKSN), IQ(KOFSCD+LCDRCD+IKDRCD+IDHKSN),
     +                 NKEYCK*ISTP, ISTP)
      IF (KPNT.GT.0) THEN
        IK     = (KPNT - IDHKSN) / ISTP + 1
        CALL CDKEYR (IK, NWKYCK, KEYO)
        CALL RZDELK (KEYO, ICDUM, 'C')
        IF (IQUEST(1).NE.0) GO TO 800
      ENDIF
      ISTEP  = 3
      IF (IOPDCA.EQ.0) CALL RZOUT (IDISCD, LBDACD, KEYNCK, ICYCLE, 'S')
*
  800 IF (PATHL.NE.' ') THEN
        IERR   = IQUEST(1)
        CALL RZCDIR (PATHL, ' ')
        NKEYCK = IQUEST(7)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        CALL RZFREE ('CDSNAM')
        IQUEST(1) = IERR
      ELSE
        IKDRCD = IQ(KOFSCD+LCDRCD+KLKDCD)
        NKEYCK = IQ(KOFSCD+LCDRCD+KNKDCD)
      ENDIF
  900 IERR   = IQUEST(1)
      IF (LUNFZ.NE.0) CALL MZDROP (IDISCD, LBDACD, 'L')
      IF (IERR.NE.0) THEN
        IRC        = 149
        IQUEST(11) = ISTEP
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDSNAM : RZ erro'//
     +  'r in step '',I3,'' for path name '//PATHN//''')', IQUEST(11),1)
#endif
#if !defined(CERNLIB__P3CHILD)
      ENDIF
#endif
*                                                             END CDSNAM
  999 END
