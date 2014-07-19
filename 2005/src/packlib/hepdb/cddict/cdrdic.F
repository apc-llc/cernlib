*
* $Id: cdrdic.F,v 1.1.1.1 1996/02/28 16:24:10 mclareni Exp $
*
* $Log: cdrdic.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:10  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDRDIC (TOPN, IRC)
*     =============================
*
************************************************************************
*                                                                      *
*        SUBR. CDRDIC (TOPN, IRC*)                                     *
*                                                                      *
*   Recreates the dictionary information and stores it into database   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     TOPN     Name of the top directory                               *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user,   CDFZUP                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =141 : Error in creating the DICTIONARY directory      *
*               =142 : Error in RZ in writing the dictionary object    *
*               =143 : Error in RZ in purging the dictionary directory *
*               =148 : Error in FZOUT for saving the journal file      *
*                                                                      *
************************************************************************
*
#include "hepdb/caopti.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      CHARACTER       TOPN*(*), PATHN*80, CFOR*32
*     ------------------------------------------------------------------
*
* *** Load the directory information
*
      CALL CDOPTS (' ', IRC)
      PATHN  = '//'//TOPN
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)                   GO TO 999
      PATHN  = PAT1CT
      NCHR   = LENOCC (PAT1CT)
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
#if (defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__SERVER))
        LUFZCF = LODBP3
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        IF (IRC.NE.0)                 GO TO 999
      ENDIF
#endif
*
* *** Prepare the journal file if needed
*
      IF (LUFZCF.GT.0) THEN
        NWDP   = (NCHR + 3) / 4
        IHEDCF(MACTCF) = 10
        IHEDCF(MNKYCF) = 0
        IHEDCF(MOPTCF) = 0
        IHEDCF(MPATCF) = NWDP
        NWDH   = MPATCF
        CALL UCTOH (PATHN, IHEDCF(NWDH+1), 4, 4*NWDP)
        NWDH   = NWDH + NWDP
        CALL MZIOCH (IOFMCF, NWFMCF, '4I -H')
*
*  **   Now write on the sequential output
*
#if defined(CERNLIB__P3CHILD)
        RNDBP3 = 'CDRDIC  '
        NWDBP3 = 2
        CALL UCTOH ('JOURNAL ', IWDBP3, 4, 8)
        CALL CDCHLD
        IRC    = IQDBP3
        IF (IRC.NE.0)                 GO TO 999
#endif
        CALL FZOUT (LUFZCF, IDIVCD, 0, 1, 'Z', IOFMCF, NWDH, IHEDCF)
        IF (IQUEST(1).NE.0) THEN
          IRC    = 148
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDRDIC : FZOUT'//
     +    ' error for path name '//PATHN(1:NCHR)//''')', IARGCD, 0)
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
        CALL CDCWSV (IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        GO TO 999
      ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
      IF (IOUTCD.EQ.0)                GO TO 999
      PAT1CT = PATHN(1:NCHR)//'/DICTIONARY'
*
* *** Try to load the dictionary information if it exists
*
      CALL RZCDIR (PAT1CT, 'Q')
      IF (IQUEST(1).NE.0) THEN
        CFOR   = CHFTCK(1:NSYSCK)
        DO I = 1, NSYSCK
          CTAGCK(I) = CHTGCK(I)
        ENDDO
        DO I = 1, NPARCD
          CFOR(NOF1CK+2*I-1:NOF1CK+2*I-1) = 'I'
          CFOR(NOF1CK+2*I  :NOF1CK+2*I  ) = 'I'
          CTAGCK(NOF1CK+2*I-1) = 'STR_VAL'//CALFCA(27+I)
          CTAGCK(NOF1CK+2*I)   = 'END_VAL'//CALFCA(27+I)
        ENDDO
        CALL RZCDIR (PATHN, ' ')
        IF (IOPSCD.NE.0) CALL RZLOCK ('CDRDIC')
        CALL RZMDIR ('DICTIONARY', NSYSCK, CFOR, CTAGCK)
        IERR   = IQUEST(1)
        IF (IOPSCD.NE.0) THEN
          CALL RZCDIR (PATHN, ' ')
          CALL RZFREE ('CDRDIC')
        ENDIF
        IF (IERR.NE.0) THEN
          IRC    = 141
#endif
#if (!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__DEBUG))
          IF (IDEBCD.GT.0) THEN
            NCHD   = LENOCC (PAT1CT)
            CALL CDPRNT (LPRTCD, '(/,'' CDRDIC : RZMDIR error for pat'//
     +      'h name '//PAT1CT(1:NCHD)//''')', IARGCD, 0)
          ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 999
        ENDIF
        CALL RZCDIR (PAT1CT, 'Q')
      ENDIF
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
*
      IF (NKEYCK.GT.0) THEN
        IPNT   = KOFSCD + LCDRCD + IKDRCD
        ISTP   = NWKYCK + 1
        IMIN   = IUHUNT (-1, IQ(IPNT+IDHKSN), NKEYCK*ISTP, ISTP)
        IF (IMIN.GT.0) THEN
          IMIN   = (IMIN - IDHKSN) / ISTP + 1
          CALL CDKEYT
          CALL CDKEYR (IMIN, NWKYCK, KEYNCK)
        ENDIF
      ELSE
        IMIN   = 0
      ENDIF
      IF (IMIN.LE.0) THEN
        CALL VZERO (KEYNCK, NSYSCK)
        KEYNCK(IDHKSN) = -1
        KEYNCK(IDHFLG) = 1
        CALL DATIME (IDATE, ITIME)
        CALL CDPKTM (IDATE, ITIME, KEYNCK(IDHINS), IRC)
      ENDIF
      CALL CDMDIC (TOPN, LBUPCD, -KLDICD, IRC)
      IF (IRC.NE.0)                   GO TO 999
      LFIXCD = LQ(KOFUCD+LBUPCD-KLDICD)
*
* *** All subdirectories looked at; now store dictionary if permitted
*
      IF (IOUTCD.NE.0) THEN
        CALL RZCDIR (PAT1CT, 'Q')
        LCDRCD = IQUEST(11)
        IF (IOPSCD.NE.0) CALL RZLOCK ('CDRDIC')
        CALL RZOUT (IDIVCD, LFIXCD, KEYNCK, ICYCLE, 'S')
        IKDRCD = IQ(KOFSCD+LCDRCD+KLKDCD)
        NKEYCK = IQ(KOFSCD+LCDRCD+KNKDCD)
        IF (IQUEST(1).NE.0) THEN
          IF (IOPSCD.NE.0) CALL RZFREE ('CDRDIC')
          IRC    = 142
#endif
#if (!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__DEBUG))
          IF (IDEBCD.GT.0) THEN
            NCHD   = LENOCC (PAT1CT)
            CALL CDPRNT (LPRTCD, '(/,'' CDRDIC : RZOUT error for path'//
     +      ' name '//PAT1CT(1:NCHD)//''')', IARGCD, 0)
          ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 999
        ENDIF
        CALL RZPURG (0)
        IERR   = IQUEST(1)
        IF (IOPSCD.NE.0) CALL RZFREE ('CDRDIC')
        IF (IERR.NE.0) THEN
          IRC    = 143
#endif
#if (!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__DEBUG))
          IF (IDEBCD.GT.0) THEN
            NCHD   = LENOCC (PAT1CT)
            CALL CDPRNT (LPRTCD, '(/,'' CDRDIC : RZPURG error for pat'//
     +      'h name '//PAT1CT(1:NCHD)//''')', IARGCD, 0)
          ENDIF
#endif
#if !defined(CERNLIB__P3CHILD)
          GO TO 999
        ENDIF
      ENDIF
*
      IRC    = 0
#endif
*                                                             END CDRDIC
  999 END
