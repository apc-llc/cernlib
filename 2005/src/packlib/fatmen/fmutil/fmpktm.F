*
* $Id: fmpktm.F,v 1.2 1999/08/17 12:45:28 mclareni Exp $
*
* $Log: fmpktm.F,v $
* Revision 1.2  1999/08/17 12:45:28  mclareni
* Y2k mods submitted by Rob Komar
*
* Revision 1.1.1.1  1996/03/07 15:18:16  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPKTM (IDATE, ITIME, IDATM,IRC)
*     ===========================================
*
************************************************************************
*                                                                      *
*        SUBR. FMPKTM (IDATE, ITIME, IDATM*, IRC*)                     *
*                                                                      *
*   Packs date and time into one word                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATE    Date : 6 Decimal integer : YYMMDD                       *
*     ITIME    Time : 4 Decimal integer : HHMM                         *
*     IDATM*   Packed date-time                                        *
*                                                                      *
*   Modified from original DBPKTM 19/03/90 JDS                         *
*                                                                      *
************************************************************************
*
#include "fatmen/fatbank.inc"
      DATA MXMIN/61/, MXHOU/25/, MXDAY/32/, MXMON/13/, MXYEA/134/
*
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
         IF(IDEBFA.GE.2)
     +   PRINT *,'FMPKTM. Invalid date/time - IPACK set to zero'
         IRC   = 1
         IDATM = 0
      ENDIF
 
*                                                             END FMPKTM
      END
 
