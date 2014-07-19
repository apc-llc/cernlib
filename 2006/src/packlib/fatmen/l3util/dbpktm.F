*
* $Id: dbpktm.F,v 1.2 1998/03/05 14:53:39 mclareni Exp $
*
* $Log: dbpktm.F,v $
* Revision 1.2  1998/03/05 14:53:39  mclareni
* Modifications supplied by L3 to allow dates up to 2010
*
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE DBPKTM (IDATE, ITIME, IDATM)
*     =======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBPKTM (IDATE, ITIME, IDATM*)                           *
*                                                                      *
*   Packs date and time into one word                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATE    Date : 6 Decimal integer : YYMMDD                       *
*     ITIME    Time : 4 Decimal integer : HHMM                         *
*     IDATM*   Packed date-time                                        *
*                                                                      *
*   Called by user,   DBDELT, DBDONT, DBEFOR, DBENTB, DBINIT, DBMDIP,  *
*             DBPRGD, DBRDIC, DBRTFZ, DBCDIC, DBENFZ, DBKOUT, DBSDIR,  *
*             DBSNAM, DBSPUR, DBUDIC, DBPLOB, DBAUXI, DBDCKH, DBDCKV,  *
*             DBRVNT, DBRVPL                                           *
*                                                                      *
************************************************************************
*
#include "fatmen/dtmpak.inc"
*     ------------------------------------------------------------------
*
      IMIN = MOD(ITIME,100)
      IHOU = ITIME/100
*
      IDAY = MOD(IDATE,100)
      IYEA = IDATE/100
      IMON = MOD(IYEA,100)
      IYEA = MOD(IYEA/100,1000)
      IF (IYEA.LT.80) IYEA = IYEA + 100
*
      IDATM = IMIN
      MAXX  = MXMIN
      IDATM = IDATM + MAXX*IHOU
      MAXX  = MAXX*MXHOU
      IDATM = IDATM + MAXX*IDAY
      MAXX  = MAXX*MXDAY
      IDATM = IDATM + MAXX*IMON
      MAXX  = MAXX*MXMON
      IDATM = IDATM + MAXX*IYEA
*                                                             END DBPKTM
      END
