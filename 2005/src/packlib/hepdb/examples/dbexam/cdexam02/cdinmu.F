*
* $Id: cdinmu.F,v 1.1.1.1 1996/02/28 16:23:45 mclareni Exp $
*
* $Log: cdinmu.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:45  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDINMU
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDINMU                                                  *
*                                                                      *
*   Initializes DBUSE on the imitation of the Muon Data Base           *
*                                                                      *
*   Called by CEXAM02                                                  *
*                                                                      *
************************************************************************
*
#include "cstor2.inc"
#include "cuser2.inc"
#include "cvser2.inc"
      PARAMETER       (NOFF2=12, NOFFP=6)
      DIMENSION       KEYS(100), MASK(100)
*
*     ------------------------------------------------------------------
*
      CALL TIMEX (TIMINT)
      CALL CDUVTX (IDAY, ITIM1)
      ITIME = 0
*
* *** -------------- UVDS ---------------------
*
      CALL VZERO  (MASK, 100)
      CALL VZERO  (KEYS, 100)
      MASK(NOFF2+1) = 1
      KEYS(NOFF2+1) = 16
      DO 5 I = 1, KEYS(NOFF2+1)
        KEYS(NOFF2+1+I) = I
    5 CONTINUE
      CALL CDUSEDB ('//DBL3/MUCH/MCALB/UVDS', LKUVCU(1), ITIME,
     +              MASK, KEYS, 'M', IRC)
      CALL CDUSEDB ('//DVL3/MUCH/MCALB/UVDS', LKUVCV(1), ITIME,
     +              MASK, KEYS, 'M', IRC)
#if defined(CERNLIB__DEBUG)
      IF (LKUVCU(1).NE.0)
     +  CALL DZSHOW ('UVDS    ', IDIVCU, LKUVCU(1), 'L', 0, 0, 0, 0)
      IF (LKUVCV(1).NE.0)
     +  CALL DZSHOW ('UVDS    ', IDIVCU, LKUVCV(1), 'L', 0, 0, 0, 0)
#endif
*
* *** -------------- HRAS ---------------------
*
      CALL VZERO  (MASK, 100)
      CALL VZERO  (KEYS, 100)
      MASK(NOFF2+1) = 1
      MASK(NOFF2+2) = 1
      KEYS(NOFF2+1) = 16
      KEYS(NOFF2+2) = 5
      DO 10 I = 1, KEYS(NOFF2+1)
        KEYS(NOFF2+2+I) = I
   10 CONTINUE
      IS    = NOFF2 + 2 + KEYS(NOFF2+1)
      DO 20 I = 1, KEYS(NOFF2+2)
        KEYS(IS+I) = I
   20 CONTINUE
      CALL CDUSEDB ('//DBL3/MUCH/MCALB/ALIG/HRAS', LKHRCU(1,1),
     +              ITIME, MASK, KEYS, 'M', IRC)
      CALL CDUSEDB ('//DVL3/MUCH/MCALB/ALIG/HRAS', LKHRCV(1,1),
     +              ITIME, MASK, KEYS, 'M', IRC)
#if defined(CERNLIB__DEBUG)
      IF (LKHRCU(1,1).NE.0)
     +  CALL DZSHOW ('HRAS    ', IDIVCU, LKHRCU(1,1), 'L', 0, 0, 0, 0)
      IF (LKHRCV(1,1).NE.0)
     +  CALL DZSHOW ('HRAS    ', IDIVCU, LKHRCV(1,1), 'L', 0, 0, 0, 0)
#endif
*
* *** -------------- BWIR ---------------------
*
      CALL CDUSE ('//DBL3/MUCH/MPARS/BWIR', LKBWCU, ITIME, ' ', IRC)
      CALL CDUSE ('//DVL3/MUCH/MPARS/BWIR', LKBWCV, ITIME, ' ', IRC)
#if defined(CERNLIB__DEBUG)
      IF (LKBWCU.NE.0)
     +  CALL DZSHOW ('BWIR    ', IDIVCU, LKBWCU, 'B', 0, 0, 0, 0)
      IF (LKBWCV.NE.0)
     +  CALL DZSHOW ('BWIR    ', IDIVCU, LKBWCV, 'B', 0, 0, 0, 0)
#endif
*
* *** -------------- YBRI ---------------------
*
      CALL VZERO  (MASK, 100)
      CALL VZERO  (KEYS, 100)
      MASK(NOFF2+1) = 1
      MASK(NOFFP)   = 1
      KEYS(NOFFP)   = 1
      KEYS(NOFF2+1) = 17
      DO 30 I = 1, KEYS(NOFF2+1)
        KEYS(NOFF2+1+I) = I
   30 CONTINUE
      CALL CDUSEDB ('//DBL3/MUCH/MPARS/MGEOM/YPCH/YBRI', LKYBCU(1),
     +              ITIME, MASK, KEYS, 'M', IRC)
      CALL CDUSEDB ('//DVL3/MUCH/MPARS/MGEOM/YPCH/YBRI', LKYBCV(1),
     +              ITIME, MASK, KEYS, 'M', IRC)
#if defined(CERNLIB__DEBUG)
      IF (LKYBCU(1).NE.0)
     +  CALL DZSHOW ('YBRI    ', IDIVCU, LKYBCU(1), 'L', 0, 0, 0, 0)
      IF (LKYBCV(1).NE.0)
     +  CALL DZSHOW ('YBRI    ', IDIVCU, LKYBCV(1), 'L', 0, 0, 0, 0)
#endif
*
      CALL TIMEX (TIMNOW)
      TIME  = TIMNOW - TIMINT
      CALL CDUVTX (IDAY, ITIM2)
      ITIME = ITIM2 - ITIM1
      WRITE (LPRTCU, 1001) IDAY, ITIM1, ITIM2, ITIME
      WRITE (LPRTCU, 1002) TIMINT, TIMNOW, TIME
*
 1001 FORMAT (/,' CDINMU : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CDINMU : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                             END CDINMU
      END
