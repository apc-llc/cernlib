*
* $Id: cdatoi.F,v 1.1.1.1 1996/02/28 16:24:33 mclareni Exp $
*
* $Log: cdatoi.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:33  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDATOI (LUN, LBD, IRC)
*     =================================
*
************************************************************************
*                                                                      *
*        SUBR. CDATOI (LUN, LBD*, IRC*)                                *
*                                                                      *
*   Reads in from a file already opened and converts the ASCII file    *
*   to packed bits in a zebra bank                                     *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Logical unit number of the file with ASCII data         *
*     LBD      Address of the data bank                                *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDCHAR                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 66 : Illegal logical unit number                     *
*               = 67 : File too long; no space in buffer               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/clinks.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      PARAMETER       (JBIAS=2)
      DIMENSION       LBD(9)
      CHARACTER       CHID*4, KLINE*80
*
*     ------------------------------------------------------------------
*
      LBD(1) = 0
      IRC    = 0
*
* *** Check on logical unit number
*
      IF (LUN.LE.0) THEN
        IRC        = 66
        IQUEST(11) = LUN
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDATOI : Illegal'//
     +  ' unit number '',I10,'' for ASCII file'')', IQUEST(11), 1)
#endif
        GO TO 999
      ENDIF
*
* *** Read in the file and store the content in a bank
*
      NDATA  = NDMXCB
#if !defined(CERNLIB__P3CHILD)
      IF (LBFXCD.NE.0) THEN
        CALL UHTOC (IQ(KOFUCD+LBFXCD-4), 4, CHID, 4)
        IF (CHID.EQ.'USER') CALL MZDROP (IDIVCD, LBFXCD, ' ')
      ENDIF
      CALL CDBANK (IDIVCD, LBFXCD, LBFXCD, JBIAS, 'USER', 0, 0, NDATA,
     +             1, -1, IRC)
      IF (IRC.NE.0)     GO TO 999
*
*  ** Now read the file
*
      NDAT   = 0
      NREC   = 0
   10 READ (LUN, 1000, ERR=20, END=20) KLINE
      NREC   = NREC + 1
      LENG   = LENOCC (KLINE)
      CALL CDLINC (KLINE, LENG, IQ(KOFUCD+LBFXCD+1), NDAT)
      IF (NDAT.GT.NDATA-20) THEN
        IRC        = 67
        LBD(1)     = LBFXCD
        IQUEST(11) = NREC
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDATOI : Buffer '//
     +  'full after '',I8,'' records'')', IQUEST(11), 1)
#endif
#if !defined(CERNLIB__P3CHILD)
        GO TO 999
      ENDIF
      GO TO 10
*
   20 NDP    = NDAT - NDATA
      IF (NDP.LT.0) CALL MZPUSH (IDIVCD, LBFXCD, 0, NDP, 'I')
      LBD(1) = LBFXCD
#endif
#if defined(CERNLIB__P3CHILD)
      RNDBP3    = 'CDATOI'
      NWDBP3    = 1
      IWDBP3(1) = LUN
      CALL CDCHLD
      IF (IQDBP3.NE.0) THEN
        IRC     = IQDBP3
        GO TO 999
      ENDIF
      CALL APFZIN (LIDBP3, IDIVCD, LBFXCD, JBIAS, 'E', 0, IWDBP3)
      LBD(1)    = LBFXCD
#endif
#if !defined(CERNLIB__P3CHILD)
*
 1000 FORMAT (A80)
#endif
*                                                             END CDATOI
  999 END
