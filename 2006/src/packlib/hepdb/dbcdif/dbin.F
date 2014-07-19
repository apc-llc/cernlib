*
* $Id: dbin.F,v 1.1.1.1 1996/02/28 16:24:58 mclareni Exp $
*
* $Log: dbin.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:58  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBIN (PATHN, ITIME, IUDIV, LSUP, IPRVS, NWKEY, KEY,
     +                 IPREC, CHOPT)
*     ==============================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBIN (PATHN, ITIME, IUDIV, LSUP*, IPRVS, NWKEY*, KEY*,  *
*                    IPREC*, CHOPT)                                    *
*                                                                      *
*   Fetches from disk to memory data valid for a given time            *
*                                                                      *
*   Restrictions : No selection on user keys can be made               *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     ITIME    Time for which data are required to be valid            *
*     IUDIV    Division index where bank is expected to be returned    *
*     LSUP     Address of bank in memory where data have been stored   *
*     IPRVS    Version number of program used when data were created   *
*              (if = 0, accept any)                                    *
*     NWKEY    Length of the key vector                                *
*     KEY      Key vector elements                                     *
*     IPREC    Precision word; (If IPREC > 0, data are stored with     *
*              IPREC significant digits right to the decimal points; if*
*              IPREC < 0, data are stored with IPREC insignificant     *
*              digits left to the decimal point.)                      *
*     CHOPT    Character string with any of the following characters   *
*          N   If data do not exist for ITIME, take the nearest data   *
*              object in time                                          *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 36 : Data bank address zero on return from DBKXIN    *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "dxused.inc"
      PARAMETER       (JBIAS=2)
      DIMENSION       KEY(9), LSUP(9), ITIME(9)
      CHARACTER       CHOPT*(*), PATHN*(*), CHOPF*28
*
*     ------------------------------------------------------------------
*
* *** Reformat option
*
      CALL DBOPTS (CHOPT, IRC)
      IF (IRC.NE.0) THEN
        IQUEST(1) = IRC
        GO TO 999
      ENDIF
      IOPFDX = 1
      CALL DBOPTM (CHOPF)
*
* *** Setup a call to CDUSEM
*
      CALL VZERO (MASKDX, MXCMCK)
      CALL VZERO (KEYSDX, MXCMCK)
      IF (IPRVS.NE.0) THEN
        KEYSDX(IDHUSI) = IPRVS
        MASKDX(IDHUSI) = 1
      ENDIF
      LKEYDX = 0
      CALL CDUSEM (PATHN, LKEYDX, ITIME, MASKDX, KEYSDX, CHOPF, IRC)
      IF (IRC.NE.0) THEN
        IQUEST(1) = IRC
        GO TO 999
      ELSE IF (LKEYDX.EQ.0.OR.LQ(KOFUCD+LKEYDX-KLDACD).EQ.0) THEN
        IQUEST(1) = 36
        GO TO 999
      ENDIF
*
      LBN    = LQ(KOFUCD+LKEYDX-KLNOCD)
      NDK    = IQ(KOFUCD+LKEYDX-1)
      NWKEY  = IQ(KOFUCD+LBN+MNDNWK)
      IPREC  = IQ(KOFUCD+LKEYDX+NDK+MKYPRE)
      CALL UCOPY  (IQ(KOFUCD+LKEYDX+1), KEY, NWKEY)
      LDATDX = LQ(KOFUCD+LKEYDX-KLDACD)
      IF (IDIVCD.NE.IUDIV) THEN
        CALL MZCOPY (IDIVCD, LDATDX, IUDIV, LSUP(1), JBIAS, 'LP')
      ELSE
        CALL ZSHUNT (IDIVCD, LDATDX, LSUP(1), JBIAS, 2, 0)
        IQUEST(1) = 0
      ENDIF
*                                                               END DBIN
  999 END
