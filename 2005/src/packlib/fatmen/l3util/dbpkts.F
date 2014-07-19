*
* $Id: dbpkts.F,v 1.2 1998/03/05 14:53:40 mclareni Exp $
*
* $Log: dbpkts.F,v $
* Revision 1.2  1998/03/05 14:53:40  mclareni
* Modifications supplied by L3 to allow dates up to 2010
*
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE DBPKTS (IDATE, ITIME, IDATM)
*     =======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBPKTS (IDATE, ITIME, IDATM*)                           *
*                                                                      *
*   Packs date and time into one word                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATE    Date : 6 Decimal integer : YYMMDD                       *
*              (Only Year > 80 allowed)                                *
*     ITIME    Time : 6 Decimal integer : HHMMSS                       *
*     IDATM*   Packed date-time                                        *
*                                                                      *
*   Called by user,   DBCDIC, DBFRST, DBRDIC, DBSNAM, DBUDIC, DBPLNT,  *
*             DBPLOB, DBPLOV, DBPLTI, DBAUXI, DBDCKH, DBDCKV, DVRVNT,  *
*             DBRVPL, DBVIEW                                           *
*                                                                      *
************************************************************************
*
#include "fatmen/dtmpak.inc"
*     ------------------------------------------------------------------
*
      ISEC = MOD(ITIME,100)
      IHOU = ITIME/100
      IMIN = MOD(IHOU,100)
      IHOU = IHOU/100
*
      IDAY = MOD(IDATE,100)
      IYEA = IDATE/100
      IMON = MOD(IYEA,100)
      IYEA = IYEA/100 - 80
      IF (IYEA.LT.0)     IYEA = IYEA + 100
      IF (IYEA.GT.MXYER) IYEA = MXYER
*
      IDATM = ISEC
      MAXX  = MXSEC
      IDATM = IDATM + MAXX*IMIN
      MAXX  = MAXX*MXMIN
      IDATM = IDATM + MAXX*IHOU
      MAXX  = MAXX*MXHOU
      IDATM = IDATM + MAXX*IDAY
      MAXX  = MAXX*MXDAY
      IDATM = IDATM + MAXX*IMON
      MAXX  = MAXX*MXMON
      IDATM = IDATM + MAXX*IYEA
*                                                             END DBPKTS
      END
