*
* $Id: cdpkts.F,v 1.2 1999/08/17 12:42:42 mclareni Exp $
*
* $Log: cdpkts.F,v $
* Revision 1.2  1999/08/17 12:42:42  mclareni
* Y2k mods submitted by Rob Komar
*
* Revision 1.1.1.1  1996/02/28 16:24:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPKTS (IDATE, ITIME, IDATM, IRC)
*     ============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPKTS (IDATE, ITIME, IDATM*, IRC*)                     *
*                                                                      *
*   Packs date and time into one word                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATE    Date : 6 Decimal integer : YYMMDD                       *
*              (Only Year > 80 allowed)                                *
*     ITIME    Time : 6 Decimal integer : HHMMSS                       *
*     IDATM    Packed date-time                                        *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by user                                                     *
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
      IRC  = 0
      ISEC = MOD(ITIME,100)
      IHOU = ITIME/100
      IMIN = MOD(IHOU,100)
      IHOU = IHOU/100
*
      IDAY = MOD(IDATE,100)
      IYEA = IDATE/100
      IMON = MOD(IYEA,100)
      IYEA = IYEA/100 - 80
      IF(IYEA.LT.0) IYEA = IYEA + 100
*
      IDATM = ISEC
      MAXX = MXSEC
      IDATM = IDATM + MAXX*IMIN
      MAXX = MAXX*MXMIN
      IDATM = IDATM + MAXX*IHOU
      MAXX = MAXX*MXHOU
      IDATM = IDATM + MAXX*IDAY
      MAXX = MAXX*MXDAY
      IDATM = IDATM + MAXX*IMON
      MAXX = MAXX*MXMON
      IDATM = IDATM + MAXX*IYEA
 
      IF((IYEA.GT.55) .OR. (IMON.GE.MXMON) .OR. (IDAY.GE.MXDAY) .OR.
     +   (IHOU.GE.MXHOU) .OR. (IMIN.GE.MXMIN)) THEN
         IF(LLOGCD.GE.2)
     +   PRINT *,'CDPKTS. Invalid date/time - IPACK set to zero'
         IRC   = 93
         IDATM = 0
      ENDIF
*                                                             END CDPKTS
      END
