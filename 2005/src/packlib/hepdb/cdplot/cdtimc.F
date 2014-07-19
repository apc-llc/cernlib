*
* $Id: cdtimc.F,v 1.1.1.1 1996/02/28 16:24:23 mclareni Exp $
*
* $Log: cdtimc.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:23  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDTIMC (IDATX, ITIMX)
*     ================================
*
************************************************************************
*                                                                      *
*        SUBR. CDTIMC (*IDATX*, *ITIMX*)                               *
*                                                                      *
*   Checks the date and time for plotting purpose                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATX    Date : 6 Decimal integer : YYMMDD                       *
*     ITIMX    Time : 6 Decimal integer : HHMMSS                       *
*                                                                      *
*   Called by CDPLOB, CDPLOV, CDPLTI                                   *
*                                                                      *
************************************************************************
*
      DIMENSION       IDAYM(12)
      DATA            IDAYM /31, 28, 31, 30, 31, 30,
     +                       31, 31, 30, 31, 30, 31/
*
*     ------------------------------------------------------------------
*
      ISEC = MOD (ITIMX, 100)
      IMIN = MOD (ITIMX/100, 100)
      IHOU = MOD (ITIMX/10000, 100)
*
      IDAY = MOD (IDATX, 100)
      IMON = MOD (IDATX/100, 100)
      IYEA = MOD (IDATX/10000, 100)
*
      IF (ISEC.GT.59) THEN
        ISEC = 59
      ELSE IF (ISEC.LT.0) THEN
        ISEC = 0
      ENDIF
      IF (IMIN.GT.59) THEN
        IMIN = 59
      ELSE IF (IMIN.LT.0) THEN
        IMIN = 0
      ENDIF
      IF (IHOU.GT.23) THEN
        IHOU = 23
      ELSE IF (IHOU.LT.0) THEN
        IHOU = 0
      ENDIF
      IF (IMON.LT.1) THEN
        IMON = 1
      ELSE IF (IMON.GT.12) THEN
        IMON = 12
      ENDIF
      IF (IDAY.LT.1) THEN
        IDAY = 1
      ELSE IF (IDAY.GT.IDAYM(IMON)) THEN
        IDAY = IDAYM(IMON)
      ENDIF
      IDATX = IYEA*10000 + IMON*100 + IDAY
      ITIMX = IHOU*10000 + IMIN*100 + ISEC
*                                                             END CDTIMC
      END
