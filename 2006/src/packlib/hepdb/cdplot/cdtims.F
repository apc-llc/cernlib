*
* $Id: cdtims.F,v 1.1.1.1 1996/02/28 16:24:24 mclareni Exp $
*
* $Log: cdtims.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:24  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDTIMS (IDATM, ISECS)
*     ================================
*
************************************************************************
*                                                                      *
*        SUBR. CDTIMS (IDATM, ISECS*)                                  *
*                                                                      *
*   Computes number of seconds passed since midnight of January 1,1980 *
*   from the packed date and time (ala CDPKTS)                         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATM    Packed date-time                                        *
*     IMINS*   Number of seconds passed from 00.00 on Jan 1, 1980      *
*                                                                      *
*   Called by CDNTPL                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/ctmpak.inc"
*     ------------------------------------------------------------------
*
      III  = IDATM
      ISEC = MOD (III, MXSEC)
      III  = (III-ISEC)/MXSEC
      IMIN = MOD (III, MXMIN)
      III  = (III-IMIN)/MXMIN
      IHOU = MOD (III, MXHOU)
      III  = (III-IHOU)/MXHOU
      IDAY = MOD (III, MXDAY)
      III  = (III-IDAY)/MXDAY
      IMON = MOD (III, MXMON)
      IYEA = (III-IMON)/MXMON
      IF (IYEA.LT.1) THEN
        IADD = 0
        ITYP = 2
      ELSE IF (IYEA.GT.20) THEN
        IADD = IDAYY(20)
        ITYP = 2
      ELSE
        IADD = IDAYY(IYEA)
        ITYP = MOD (IYEA, 4)
        IF (ITYP.EQ.0) THEN
          ITYP = 2
        ELSE
          ITYP = 1
        ENDIF
      ENDIF
*
      IDAYS = IDAY + IADD + IDAYM(IMON,ITYP)
      ISECS = ISEC + 60*(IMIN + 60*(IHOU + 24*IDAYS))
*                                                             END CDTIMS
      END
