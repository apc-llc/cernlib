*
* $Id: cdcrmd.F,v 1.1.1.1 1996/02/28 16:23:44 mclareni Exp $
*
* $Log: cdcrmd.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:44  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCRMD
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDCRMD                                                  *
*                                                                      *
*   Creates the directory structure of the immitation data base        *
*                                                                      *
*   Called by CEXAM01                                                  *
*                                                                      *
************************************************************************
*
#include "cuser1.inc"
      CHARACTER       CHTAG(2)*8
      DATA            CHTAG /'Us_key_1', 'Us_key_2'/
*
*     ------------------------------------------------------------------
*
      CALL TIMEX (TIMINT)
      CALL CDUVTX (IDAY, ITIM1)
*
      IPREC  = 3
      DELTA  = 0.
      CALL CDMDIR ('//DBL3/MUCH/MCALB/UVDS',       1, 'I',  CHTAG, 0,
     +             IPREC, DELTA, 'C', IRC)
      IPREC  = 0
      CALL CDMDIR ('//DBL3/MUCH/MCALB/ALIG/HRAS',  2, 'II', CHTAG, 0,
     +             IPREC, DELTA, 'C', IRC)
      CALL CDMDIR ('//DBL3/MUCH/MCALB/ALIG/BEAC',  1, 'I',  CHTAG, 0,
     +             IPREC, DELTA, 'C', IRC)
      CALL CDMDIR ('//DBL3/MUCH/MPARS/BWIR',       0, 'I',  CHTAG, 0,
     +             IPREC, DELTA, 'C', IRC)
      CALL CDMDIR ('//DBL3/MUCH/MPARS/MGEOM/XBRI', 2, 'II', CHTAG, 0,
     +             IPREC, DELTA, 'C', IRC)
      CALL CDMDIR ('//DBL3/MUCH/MPARS/MGEOM/YPCH/YAPL', 1, 'I', CHTAG,
     +             0, IPREC, DELTA, 'C', IRC)
      CALL CDMDIR ('//DBL3/MUCH/MPARS/MGEOM/YPCH/YBRI', 1, 'I', CHTAG,
     +             0, IPREC, DELTA, 'C', IRC)
*
      CALL TIMEX (TIMNOW)
      TIME  = TIMNOW - TIMINT
      CALL CDUVTX (IDAY, ITIM2)
      ITIME = ITIM2 - ITIM1
      WRITE (LPRTCU, 1001) IDAY, ITIM1, ITIM2, ITIME
      WRITE (LPRTCU, 1002) TIMINT, TIMNOW, TIME
*
 1001 FORMAT (/,' CDCRMD : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CDCRMD : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                             END CDCRMD
      END
