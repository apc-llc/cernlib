*
* $Id: cdtimm.F,v 1.1.1.1 1996/02/28 16:24:23 mclareni Exp $
*
* $Log: cdtimm.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:23  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDTIMM (IDATM, IMINS)
*     ================================
*
************************************************************************
*                                                                      *
*        SUBR. CDTIMM (IDATM, IMINS*)                                  *
*                                                                      *
*   Computes number of minutes passed since midnight of January 1,1980 *
*   from the packed date and time (ala CDPKTM)                         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATM    Packed date-time                                        *
*     IMINS*   Number of minutes passed from 00.00 on Jan 1, 1980      *
*                                                                      *
*   Called by CDNTPL                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/ctmpak.inc"
*     ------------------------------------------------------------------
*
      III  = IDATM
      IMIN = MOD (III, MXMIN)
      III  = (III-IMIN)/MXMIN
      IHOU = MOD (III, MXHOU)
      III  = (III-IHOU)/MXHOU
      IDAY = MOD (III, MXDAY)
      III  = (III-IDAY)/MXDAY
      IMON = MOD (III, MXMON)
      IYEA = (III-IMON)/MXMON - 80
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
      IMINS = IMIN + 60*(IHOU + 24*IDAYS)
*                                                             END CDTIMM
      END
