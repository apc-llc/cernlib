*
* $Id: cdctoi.F,v 1.1.1.1 1996/02/28 16:24:34 mclareni Exp $
*
* $Log: cdctoi.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:34  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCTOI (CHTEXT, NTEXT, LTEXT, LBD, IRC)
*     ==================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDCTOI (CHTEXT, NTEXT, LTEXT, LBD, IRC)                 *
*                                                                      *
*   Copy character data to packed bits in a zebra bank                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CHTEXT   Character array containing the data                     *
*     NTEXT    Number of elements to be stored                         *
*     LTEXT    Maximum length to take from each element (ignored)      *
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
      CHARACTER*(*)   CHTEXT(NTEXT)
      PARAMETER       (JBIAS=2)
      DIMENSION       LBD(9)
      CHARACTER       CHID*4, KLINE*80
*
*     ------------------------------------------------------------------
*
      LBD(1) = 0
      IRC    = 0
*
* *** Copy character data into a bank
*
      NDATA  = NDMXCB
      IF (LBFXCD.NE.0) THEN
        CALL UHTOC (IQ(KOFUCD+LBFXCD-4), 4, CHID, 4)
        IF (CHID.EQ.'USER') CALL MZDROP (IDIVCD, LBFXCD, ' ')
      ENDIF
      CALL CDBANK (IDIVCD, LBFXCD, LBFXCD, JBIAS, 'USER', 0, 0, NDATA,
     +             1, -1, IRC)
      IF (IRC.NE.0)     GO TO 999
*
*  ** Now copy the data
*
      NDAT   = 0
      NREC   = 0
      DO 10 I=1,NTEXT
        KLINE  = CHTEXT(I)
        NREC   = NREC + 1
        LENG   = LENOCC (KLINE)
        CALL CDLINC (KLINE, LENG, IQ(KOFUCD+LBFXCD+1), NDAT)
        IF (NDAT.GT.NDATA-20) THEN
          IRC        = 67
          LBD(1)     = LBFXCD
          IQUEST(11) = NREC
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCTOI : Buffe'//
     +    'r full after '',I8,'' records'')', IQUEST(11), 1)
#endif
          GO TO 999
        ENDIF
   10 CONTINUE
*
   20 NDP    = NDAT - NDATA
      IF (NDP.LT.0) CALL MZPUSH (IDIVCD, LBFXCD, 0, NDP, 'I')
      LBD(1) = LBFXCD
*                                                             END CDCTOI
  999 END
