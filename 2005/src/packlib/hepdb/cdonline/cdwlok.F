*
* $Id: cdwlok.F,v 1.1.1.1 1996/02/28 16:24:18 mclareni Exp $
*
* $Log: cdwlok.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:18  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDWLOK (IRC)
*     =======================
*
************************************************************************
*                                                                      *
*        SUBR. CDWLOK (IRC*)                                           *
*                                                                      *
*   Locks the CLUSCOM before writing onto it and communicating to the  *
*   server                                                             *
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
*               =221 : Error in CC_WRITELOCK for locking the CLUSCOM   *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/cbsrvr.inc"
#include "hepdb/crccom.inc"
*
*     ------------------------------------------------------------------
*
      LUFZCF = LUFMCD
*
      CALL CC_WRITELOCK ('CRCCOM', ' ', IRC)
      IF (IRC.NE.0) THEN
        IQUEST(11) = IRC
        IRC        = 221
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDWLOK : Error'//
     +  ' '',I10,'' in CC_WRITELOCK '')', IQUEST(11), 1)
#endif
      ENDIF
*                                                             END CDWLOK
      END
