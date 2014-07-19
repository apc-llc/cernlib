*
* $Id: dbuptm.F,v 1.2 1998/03/05 14:53:41 mclareni Exp $
*
* $Log: dbuptm.F,v $
* Revision 1.2  1998/03/05 14:53:41  mclareni
* Modifications supplied by L3 to allow dates up to 2010
*
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE DBUPTM (IDATE, ITIME, IDATM)
*     =======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBUPTM (IDATE*, ITIME*, IDATM)                          *
*                                                                      *
*   Unpacks date and time from one word                                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATE*   Date : 6 Decimal integer : YYMMDD                       *
*     ITIME*   Time : 4 Decimal integer : HHMM                         *
*     IDATM    Packed date-time                                        *
*                                                                      *
*   Called by user,   DBFZUP, DBLKEY, DBNTOP, DBPRGD, DBSRTM, DBENFZ,  *
*             DBPLOB, DBDKYH, DBDKYV                                   *
*                                                                      *
************************************************************************
*
#include "fatmen/dtmpak.inc"
*     ------------------------------------------------------------------
*
      III  = IDATM
      IMIN = MOD(III,MXMIN)
      III  = (III-IMIN)/MXMIN
      IHOU = MOD(III,MXHOU)
      III  = (III-IHOU)/MXHOU
      IDAY = MOD(III,MXDAY)
      III  = (III-IDAY)/MXDAY
      IMON = MOD(III,MXMON)
      IYEA = (III-IMON)/MXMON
      IYEA = MOD(IYEA,100)
*
      IDATE = IDAY + 100*IMON + 10000*IYEA
      ITIME = IMIN + 100*IHOU
*                                                             END DBUPTM
      END
