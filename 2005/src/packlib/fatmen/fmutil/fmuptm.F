*
* $Id: fmuptm.F,v 1.3 1999/08/17 12:45:29 mclareni Exp $
*
* $Log: fmuptm.F,v $
* Revision 1.3  1999/08/17 12:45:29  mclareni
* Y2k mods submitted by Rob Komar
*
* Revision 1.2  1999/01/04 08:13:20  jamie
* y2k bug in fmuptm
*
* Revision 1.1.1.1  1996/03/07 15:18:16  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMUPTM (IDATE, ITIME, IDATM, IRC)
*     ============================================
*
************************************************************************
*                                                                      *
*        SUBR. FMUPTM (IDATE*, ITIME*, IDATM, IRC*)                    *
*                                                                      *
*   Unpacks date and time from one word                                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATE*   Date : 6 Decimal integer : YYMMDD                       *
*     ITIME*   Time : 4 Decimal integer : HHMM                         *
*     IDATM    Packed date-time                                        *
*                                                                      *
*   Called by <USER>                                                   *
*   Modified from original DBUPTM 19/03/90 JDS                         *
*                                                                      *
************************************************************************
*
#include "fatmen/fatbank.inc"
      DATA MXMIN/61/, MXHOU/25/, MXDAY/32/, MXMON/13/, MXYEA/134/
*
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
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMUPTM. Invalid PACKed time - IDATE/ITIME set to zero'
         IRC   = 1
         IDATE = 0
         ITIME = 0
      ENDIF
*
      IDATE = IDAY + 100*IMON + 10000*IYEA
      IDATE = MOD(IDATE, 1000000)
      ITIME = IMIN + 100*IHOU
 
      END
