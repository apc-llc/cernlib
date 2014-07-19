*
* $Id: cduptm.F,v 1.3 1999/08/17 12:42:42 mclareni Exp $
*
* $Log: cduptm.F,v $
* Revision 1.3  1999/08/17 12:42:42  mclareni
* Y2k mods submitted by Rob Komar
*
* Revision 1.2  1999/01/04 08:14:55  jamie
* y2k bug in cduptm
*
* Revision 1.1.1.1  1996/02/28 16:24:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUPTM (IDATE, ITIME, IDATM, IRC)
*     ============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDUPTM (IDATE*, ITIME*, IDATM, IRC*)                    *
*                                                                      *
*   Unpacks date and time from one word                                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATE    Date : 6 Decimal integer : YYMMDD                       *
*     ITIME    Time : 4 Decimal integer : HHMM                         *
*     IDATM    Packed date-time                                        *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by <USER>, CDENFZ, CDFZUP, CDLKEY, CDPART, CDPURP           *
*   Modified from original DBUPTM 19/03/90 JDS                         *
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
      III  = IDATM
      IMIN = MOD(III,MXMIN)
      III  = (III-IMIN)/MXMIN
      IHOU = MOD(III,MXHOU)
      III  = (III-IHOU)/MXHOU
      IDAY = MOD(III,MXDAY)
      III  = (III-IDAY)/MXDAY
      IMON = MOD(III,MXMON)
      IYEA = (III-IMON)/MXMON
 
      IF((IYEA.GT.MXYEA) .OR. (IMON.GE.MXMON) .OR. (IDAY.GE.MXDAY) .OR.
     +   (IHOU.GE.MXHOU) .OR. (IMIN.GE.MXMIN)) THEN
         IF(LLOGCD.GE.2)
     +   PRINT *,'CDUPTM. Invalid PACKed time - IDATE/ITIME set to zero'
         IRC   = 93
         IDATE = 0
         ITIME = 0
      ENDIF
*
      IDATE = IDAY + 100*IMON + 10000*IYEA
      IDATE = MOD(IDATE,1000000)
      ITIME = IMIN + 100*IHOU
*                                                             END CDUPTM
      END
