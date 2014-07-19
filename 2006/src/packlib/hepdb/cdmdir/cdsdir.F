*
* $Id: cdsdir.F,v 1.1.1.1 1996/02/28 16:24:16 mclareni Exp $
*
* $Log: cdsdir.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:16  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDSDIR (PATHN, NWKEY, CHFOR, CHTAG, MXKP, IPREC, DELTA,
     +                   CHOPT, IRC)
*     ==================================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDSDIR (PATHN, NWKEY, CHFOR, CHTAG, MXKP, IPREC, DELTA, *
*                      CHOPT, IRC*)                                    *
*                                                                      *
*   Saves the directory creation information in the Journal file.      *
*   It creates a header record (with data bank containing IPREC and    *
*   DELTA) for each entry.                                             *
*   The header contains Action Code (2), Number of keys, numbers of    *
*   charcaters for option and pathname, MXKP, followed by Character    *
*   option, CHFOR, CHTAG and the pathname                              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     NWKEY    Number of key elements                                  *
*     CHFOR    Character variable describing each element of the key   *
*              vector                                                  *
*     CHTAG    Character array defined as CHARACTER*8 (NWKEY)          *
*     MXKP     Maximum number of objects in each partition             *
*     IPREC    Precision word sepcifying the number of significant     *
*              digits to be stored; (If IPREC > 0, data are stored     *
*              with IPREC significant digits right to the decimal      *
*              points; if IPREC < 0, data are stored with IPREC        *
*              insignificant digits left to the decimal point.)        *
*     DELTA    Variable specifying the absolute value below which data *
*              is treated as zero                                      *
*     CHOPT    Character string with any of the following characters   *
*          C   Data in the directory will be compressed by default     *
*          P   Create partitioned subdirectories for the pathname      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDMKDI                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 49 : FZOUT fails to write on the sequential file     *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctpath.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      PARAMETER       (JBIAS=2)
      CHARACTER       CTAG*8, PATHN*(*), CHFOR*(*), CHOPT*(*)
      CHARACTER*(*)   CHTAG(*)
*
*     ------------------------------------------------------------------
*
* *** Find the logical unit number of the Journal file
*
      KEY7   = KEY7CK
      KEY7CK = 0
      IRC    = 0
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
#if defined(CERNLIB__P3CHILD)
        LUFZCF = LODBP3
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))
        IF (IRC.NE.0) GO TO 999
      ENDIF
#endif
*
      IF (LUFZCF.GT.0) THEN
*
*  **   Prepare the header containing all the information
*
        NCH    = LENOCC (PATHN)
        NCHD   = (NCH  + 3) / 4
        NCHP   = LENOCC (CHOPT)
        NCHPD  = (NCHP + 3) / 4
        CALL MZIOCH (IOFMCF, NWFMCF, '7I -H')
        IHEDCF(MACTCF) = 2
        IHEDCF(MNKYCF) = NWKEY
        IHEDCF(MPATCF) = NCHD
        IHEDCF(MOPTCF) = NCHPD
        IHEDCF(MXKPCF) = MXKP
*       IF (NCHPD.GT.0) CALL UCTOH (CHOPT, IHEDCF(MRECCF+1), 4, 4*NCHPD)
        IF (NCHPD.GT.0) CALL UCTOH (CHOPT, IHEDCF(MRECCF+1), 4, NCHP)
        IF (KEY7.LE.0) THEN
          CALL DATIME (IDATE, ITIME)
          CALL CDPKTM (IDATE, ITIME, KEY7, IRC)
        ENDIF
        IHEDCF(MINSCF) = KEY7
        IHEDCF(MRECCF) = 0
        NCFO   = (NWKEY + 3) / 4
        NPNT   = IHEDCF(MOPTCF) + MRECCF + 1
        CALL UCTOH (CHFOR, IHEDCF(NPNT), 4, NWKEY)
        NPNT   = NPNT + NCFO
        DO 10 I = 1, NWKEY
          CTAG   = CHTAG(I)
          CALL UCTOH (CTAG, IHEDCF(NPNT), 4, 8)
          NPNT   = NPNT + 2
   10   CONTINUE
        CALL UCTOH (PATHN, IHEDCF(NPNT), 4, 4*NCHD)
        NWDH   = NPNT + NCHD - 1
*
*  **   Now write on the sequential output
*
#if defined(CERNLIB__P3CHILD)
        RNDBP3 = 'CDSDIR '
        NWDBP3 = 2
        CALL UCTOH ('JOURNAL ', IWDBP3, 4, 8)
        CALL CDCHLD
        IRC    = IQDBP3
        IF (IRC.NE.0) GO TO 999
#endif
        IF (LSTRCL(3).NE.0) CALL MZDROP (IDISCD, LSTRCL(3), ' ')
        CALL CDBANK (IDISCD, LSTRCL(3), LSTRCL(3), JBIAS, 'SAME', 0, 0,
     +               1, 3, 0, IRC)
        IF (IRC.NE.0) GO TO 999
        IQ(KOFUCD+LSTRCL(3)-5) = IPREC
         Q(KOFUCD+LSTRCL(3)+1) = DELTA
        CALL FZOUT (LUFZCF, IDISCD, LSTRCL(3), 1, 'S', IOFMCF, NWDH,
     +              IHEDCF)
        IERR   = IQUEST(1)
        CALL MZDROP (IDISCD, LSTRCL(3), ' ')
        IF (IERR.NE.0) THEN
          IRC    = 49
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) THEN
            PAT3CT = PATHN
            CALL CDPRNT (LPRTCD, '(/,'' CDSDIR : FZOUT error for path'//
     +           ' name '//PAT3CT//''')', IARGCD, 0)
          ENDIF
#endif
          GO TO 999
        ENDIF
      ENDIF
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
*
* *** Server environment, Public mode
*
      IF (IOPPCD.NE.0) THEN
        IOPBCA = 0
        CALL CDCWSV (IRC)
        GO TO 999
      ENDIF
#endif
*                                                             END CDSDIR
  999 END
