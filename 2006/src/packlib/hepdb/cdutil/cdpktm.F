*
* $Id: cdpktm.F,v 1.2 1999/08/17 12:42:41 mclareni Exp $
*
* $Log: cdpktm.F,v $
* Revision 1.2  1999/08/17 12:42:41  mclareni
* Y2k mods submitted by Rob Komar
*
* Revision 1.1.1.1  1996/02/28 16:24:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPKTM (IDATE, ITIME, IDATM, IRC)
*     ============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPKTM (IDATE, ITIME, IDATM*, IRC*)                     *
*                                                                      *
*   Packs date and time into one word                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATE    Date : 6 Decimal integer : YYMMDD                       *
*     ITIME    Time : 4 Decimal integer : HHMM                         *
*     IDATM    Packed date-time                                        *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Modified from original DBPKTM 19/03/90 JDS                         *
*                                                                      *
*   Called by user,   CDBFOR, CDCDIC, CDDDIR, CDDONT, CDENFZ, CDENTB,  *
*             CDINIT, CDKOUT, CDMKDI, CDPURP, CDSDIR, CDSNAM, CDSPUR,  *
*             CDUDIC                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 93 : Illegal time                                    *
*                                                                      *
************************************************************************
*
#include "hepdb/cdunit.inc"
#include "hepdb/ctmpak.inc"
*     ------------------------------------------------------------------
*
      IRC = 0
 
      IMIN = MOD(ITIME,100)
      IHOU = ITIME/100
*
      IDAY = MOD(IDATE,100)
      IYEA = IDATE/100
      IMON = MOD(IYEA,100)
      IYEA = IYEA/100
      IF(IYEA.LT.35) IYEA = IYEA + 100
*
      IDATM = IMIN
      MAXX = MXMIN
      IDATM = IDATM + MAXX*IHOU
      MAXX = MAXX*MXHOU
      IDATM = IDATM + MAXX*IDAY
      MAXX = MAXX*MXDAY
      IDATM = IDATM + MAXX*IMON
      MAXX = MAXX*MXMON
      IDATM = IDATM + MAXX*IYEA
 
      IF((IYEA.GT.MXYEA) .OR. (IMON.GE.MXMON) .OR. (IDAY.GE.MXDAY) .OR.
     +   (IHOU.GE.MXHOU) .OR. (IMIN.GE.MXMIN)) THEN
         IF(LLOGCD.GE.2)
     +   PRINT *,'CDPKTM. Invalid date/time - IPACK set to zero'
         IRC   = 93
         IDATM = 0
      ENDIF
*                                                             END CDPKTM
      END
