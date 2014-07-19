*
* $Id: cdspur.F,v 1.1.1.1 1996/02/28 16:24:25 mclareni Exp $
*
* $Log: cdspur.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:25  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDSPUR (PATHN, NWKEY, ITIME, KEYS, ICALL, IRC)
*     =========================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDSPUR (PATHN, NWKEY, ITIME, KEYS, ICALL, IRC*)         *
*                                                                      *
*   Saves the object purging information in the Journal file.          *
*   It creates a single header record (with zero data) for each entry. *
*   The header contains Action Code (3), Number of keys, numbers of    *
*   charcaters for option and pathname, ITIME, current time (like Key  *
*   7), followed by KEYS, Character option and the pathname.           *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     NWKEY    Number of key elements                                  *
*     ITIME    Flag (If > 0, ITIME value as indicated in CDPURK, or    *
*              < 0, a call from CDPURG)                                *
*     KEYS     Vector of keys, if called from CDPURK                   *
*              Vector containing KYDAT, KYTIM, if called from CDPURG   *
*     ICALL    Called from CDPURG (0) or from CDPURK (1)               *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDPURG, CDPURK                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =115 : Cannot form the IO descriptor for the FZ header *
*               =116 : FZOUT fails to write on the sequential file     *
*                                                                      *
************************************************************************
*
#include "hepdb/caopti.inc"
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/ckkeys.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      PARAMETER       (NLEVM=20)
      CHARACTER       CHCUR(NLEVM)*1, CHFOR*110, CFORM(6)*1
      CHARACTER       CHOPF*80, PATHN*(*)
      DIMENSION       ITIME(9), KEYS(9), NLCUR(NLEVM), IOPTS(26)
      EQUIVALENCE     (IOPACA, IOPTS(1))
      DATA            CFORM /'B', 'I', 'F', 'D', 'H', 'A'/
*
*     ------------------------------------------------------------------
*
* *** Find the logical unit number of the Journal file
*
      KEY7   = KEY7CK
      KEY7CK = 0
      NCH    = LENOCC (PATHN)
      IRC    = 0
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
        IF (IRC.NE.0)              GO TO 999
      ENDIF
#endif
*
      IF (LUFZCF.GT.0) THEN
*
*  **   Prepare the header containing all the information
*
        NCHD   = (NCH + 3) / 4
        IHEDCF(MACTCF) = 3
        IHEDCF(MNKYCF) = NWKEY
        IHEDCF(MPATCF) = NCHD
        IF (ICALL.GT.0) THEN
          IHEDCF(MPARCF) = NPARCD
          CALL UCOPY (ITIME, IHEDCF(MTIMCF), NPARCD)
          CALL UCOPY (IOKYCA(1), IHEDCF(MTIMCF+NPARCD), NWKEY)
          NPNT   = MTIMCF + 2*NWKEY + NPARCD
        ELSE
          NPNT   = MTIMCF + NWKEY
          IHEDCF(MPARCF) = -1
        ENDIF
        IF (KEY7.GT.0) THEN
          IHEDCF(MINSCF) = KEY7
        ELSE
          CALL DATIME (IDATX, ITIMX)
          CALL CDPKTM (IDATX, ITIMX, IHEDCF(MINSCF), IRC)
        ENDIF
        NCHOP  = 0
        DO 10 I = 1, 26
          IF (IOPTS(I).NE.0) THEN
            IF (NCHOP.EQ.0) THEN
              CHOPF = CALFCA(I)
            ELSE
              CHOPF = CHOPF(1:NCHOP)//CALFCA(I)
            ENDIF
            NCHOP  = NCHOP + 1
          ENDIF
   10   CONTINUE
        NWDOP  = (NCHOP + 3) / 4
        IHEDCF(MOPTCF) = NWDOP
        IF (NWDOP.GT.0) THEN
          CALL UCTOH (CHOPF, IHEDCF(NPNT), 4, 4*NWDOP)
          NPNT   = NPNT + NWDOP
        ENDIF
        CALL UCTOH (PATHN, IHEDCF(NPNT), 4, 4*NCHD)
        NWDH   = NPNT + NCHD - 1
*
*  **   Get the IO format for the header
*
        NLEV   = 1
        IF (ICALL.GT.0) THEN
          NCUR   = MTIMCF + NPARCD + NWKEY - 1
        ELSE
          NCUR   = MTIMCF - 1
        ENDIF
        IFORO  = 2
        CHCUR(NLEV) = CFORM(IFORO)
        DO 20 I = 1, NWKEY
          IFORM  = IOTYCK(I)
          IF (IFORM.EQ.6) IFORM = 5
          IF (IFORM.EQ.IFORO) THEN
            NCUR   = NCUR + 1
          ELSE
            NLCUR(NLEV) = NCUR
            IF (NLEV.GE.NLEVM) THEN
              IRC    = 115
#if defined(CERNLIB__DEBUG)
              IF (IDEBCD.GT.0) THEN
                CHFOR  = PATHN
                CALL CDPRNT (LPRTCD, '(/,'' CDSPUR : Cannot form IO '//
     +               'descriptor '//CHFOR(1:NCH)//''')', IARGCD, 0)
              ENDIF
#endif
              GO TO 999
            ENDIF
            NLEV   = NLEV + 1
            CHCUR(NLEV) = CFORM(IFORM)
            NCUR   = 1
            IFORO  = IFORM
          ENDIF
          IF (ICALL.NE.0) THEN
            IHEDCF(MTIMCF+NPARCD+NWKEY+I-1) = KEYS(I)
          ELSE
            IF (I.LE.NSYSCK) THEN
              IHEDCF(MTIMCF+I-1) = KEYS(I)
            ELSE IF (IFORM.EQ.5) THEN
              CALL UCTOH ('    ', IHEDCF(MTIMCF+I-1), 4, 4)
            ELSE
              IHEDCF(MTIMCF+I-1) = 0
            ENDIF
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
        DO 30 I = 1, NLEV
          CALL UTWRIT (CHFOR(II:II+1), '(I3)', NLCUR(I))
          II     = II + 3
          CHFOR(II:II) = CHCUR(I)
          II     = II + 2
   30   CONTINUE
#endif
        II     = 5 * NLEV
        CHFOR = CHFOR(1:II)//' -H'
        CALL MZIOCH (IOFMCF, NWFMCF, CHFOR(1:II+3))
*
*  **   Now write on the sequential output
*
#if defined(CERNLIB__P3CHILD)
        RNDBP3 = 'CDSPUR '
        NWDBP3 = 2
        CALL UCTOH ('JOURNAL ', IWDBP3, 4, 8)
        CALL CDCHLD
        IRC    = IQDBP3
        IF (IRC.NE.0)              GO TO 999
#endif
        CALL FZOUT (LUFZCF, IDIVCD, 0, 1, 'Z', IOFMCF, NWDH, IHEDCF)
        IF (IQUEST(1).NE.0) THEN
          IRC    = 116
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) THEN
            CHFOR  = PATHN
            CALL CDPRNT (LPRTCD, '(/,'' CDSPUR : FZOUT error for path'//
     +           ' name '//CHFOR(1:NCH)//''')', IARGCD, 0)
          ENDIF
#endif
          GO TO 999
        ENDIF
      ENDIF
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(defined(CERNLIB__ONLINE))
*
* *** Server environment, Public mode
*
      IF (IOPPCD.NE.0) THEN
        CALL CDCWSV (IRC)
        GO TO 999
      ENDIF
#endif
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
*
 2001 FORMAT (20(I3,A1,1X))
#endif
*                                                             END CDSPUR
  999 END
