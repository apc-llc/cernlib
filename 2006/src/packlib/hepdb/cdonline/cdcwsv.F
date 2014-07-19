*
* $Id: cdcwsv.F,v 1.1.1.1 1996/02/28 16:24:19 mclareni Exp $
*
* $Log: cdcwsv.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:19  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCWSV (IRC)
*     =======================
*
************************************************************************
*                                                                      *
*        SUBR. CDCWSV (IRC*)                                           *
*                                                                      *
*   Releases the CLUSCOM and communicates with the server              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDDDIR, CDDONT, CDEALI, CDENFZ, CDENTB, CDKOUT, CDPART,  *
*             CDPURP, CDRENK, CDSDIR, CDSNAM, CDSPUR                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =222 : Error in CC_RELEASE for releasing the CLUSCOM   *
*               =223 : Error in ICSIGNAL for signalling the VAX server *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/cinitl.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/cbsrvr.inc"
#include "hepdb/crccom.inc"
      INTEGER         ICACHE(4)
      BYTE            BCACHE(16)
      EQUIVALENCE     (BCACHE, ICACHE)
*
*     ------------------------------------------------------------------
*
      IWORCR(1) = IQUEST(9)
      CALL CC_RELEASE ('CRCCOM', ' ', IERR)
      IF (IERR.NE.0) THEN
        IRC        = 222
        IQUEST(11) = IERR
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCWSV : Error'//
     +  ' '',I10,'' in CC_RELEASE'')', IQUEST(11), 1)
#endif
        GO TO 999
      ENDIF
      TIMEOUT = 100
*
*  ** First 2 bytes of the cache contain identifier
*  ** of calling client; last word contains a flag
*  ** to be changed by the server.
*
      BCACHE(1) = ICHAR(FILECB(20:20))
      BCACHE(2) = ICHAR(FILECB(21:21))
      ICACHE(3) = IOPBCA
      ICACHE(4) = -1
      CALL IC_SIGNAL (CACHCB, ICACHE, TIMEOUT, ' ', IERR)
      IF (IERR.NE.0) THEN
        IRC        = 223
        IQUEST(11) = IERR
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCWSV : Error'//
     +  ' '',I10,'' in IC_SIGNAL'')', IQUEST(11), 1)
#endif
        GO TO 999
      ENDIF
      IRC       = ICACHE(3)
      IQUEST(2) = ICACHE(2)
*                                                             END CDCWSV
  999 END
