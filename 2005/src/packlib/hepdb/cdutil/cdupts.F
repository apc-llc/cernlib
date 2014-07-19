*
* $Id: cdupts.F,v 1.2 1999/08/17 12:42:43 mclareni Exp $
*
* $Log: cdupts.F,v $
* Revision 1.2  1999/08/17 12:42:43  mclareni
* Y2k mods submitted by Rob Komar
*
* Revision 1.1.1.1  1996/02/28 16:24:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUPTS (IDATE, ITIME, IDATM, IRC)
*     ============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDUPTS (IDATE*, ITIME*, IDATM)                          *
*                                                                      *
*   Unpacks date and time from one word                                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATE    Date : 6 Decimal integer : YYMMDD                       *
*     ITIME    Time : 6 Decimal integer : HHMMSS                       *
*     IDATM    Packed date-time                                        *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/ctmpak.inc"
*     ------------------------------------------------------------------
*
      IRC  = 0
      III  = IDATM
      ISEC = MOD(III,MXSEC)
      III  = (III-ISEC)/MXSEC
      IMIN = MOD(III,MXMIN)
      III  = (III-IMIN)/MXMIN
      IHOU = MOD(III,MXHOU)
      III  = (III-IHOU)/MXHOU
      IDAY = MOD(III,MXDAY)
      III  = (III-IDAY)/MXDAY
      IMON = MOD(III,MXMON)
      IYEA = (III-IMON)/MXMON
*
      IDATE = IDAY + 100*IMON + 10000*(IYEA + 80)
      IDATE = MOD(IDATE,1000000)
      ITIME = ISEC + 100*IMIN + 10000*IHOU
*                                                             END CDUPTS
      END
