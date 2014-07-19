*
* $Id: dbupts.F,v 1.2 1998/03/05 14:53:41 mclareni Exp $
*
* $Log: dbupts.F,v $
* Revision 1.2  1998/03/05 14:53:41  mclareni
* Modifications supplied by L3 to allow dates up to 2010
*
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE DBUPTS (IDATE, ITIME, IDATM)
*     =======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBUPTS (IDATE*, ITIME*, IDATM)                          *
*                                                                      *
*   Unpacks date and time from one word                                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATE*   Date : 6 Decimal integer : YYMMDD                       *
*     ITIME*   Time : 6 Decimal integer : HHMMSS                       *
*     IDATM    Packed date-time                                        *
*                                                                      *
*   Called by user,   DBPLOB, DBPLOV, DBPLTI, DBAUXI, DBDISD, DBDKYH,  *
*             DBDKYV                                                   *
*                                                                      *
************************************************************************
*
#include "fatmen/dtmpak.inc"
*     ------------------------------------------------------------------
*
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
      IYEA = (III-IMON)/MXMON + 80
      IYEA = MOD(IYEA,100)
*
      IDATE = IDAY + 100*IMON + 10000*IYEA
      ITIME = ISEC + 100*IMIN + 10000*IHOU
*                                                             END DBUPTS
      END
