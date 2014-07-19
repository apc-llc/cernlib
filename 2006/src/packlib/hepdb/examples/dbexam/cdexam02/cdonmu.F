*
* $Id: cdonmu.F,v 1.1.1.1 1996/02/28 16:23:45 mclareni Exp $
*
* $Log: cdonmu.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:45  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDONMU (ITIME)
*     =========================
*
************************************************************************
*                                                                      *
*        SUBR. CDONMU (ITIME)                                          *
*                                                                      *
*   Tests CDUSEDB on the imitation of the Muon Data Base               *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITIME    Validity time                                           *
*                                                                      *
*   Called by CEXAM02                                                   *
*                                                                      *
************************************************************************
*
#include "cstor2.inc"
#include "cuser2.inc"
#include "cvser2.inc"
      PARAMETER       (NOFF2=12, NOFFP=6)
      DIMENSION       KEYS(100), MASK(100)
      CHARACTER       CHOPT*4
*
*     ------------------------------------------------------------------
*
      CALL TIMEX (TIMINT)
      CALL CDUVTX (IDAY, ITIM1)
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
      CHOPT = 'AM'
      CALL CDUSEDB ('//DBL3/MUCH/MCALB/UVDS', LKUVCU(1), ITIME, MASK,
     +              KEYS, CHOPT, IRC)
      CALL CDUSEDB ('//DVL3/MUCH/MCALB/UVDS', LKUVCV(1), ITIME, MASK,
     +              KEYS, CHOPT, IRC)
      DO 10 I = 1, KEYS(NOFF2+1)
        IF (LKUVCU(I).GT.0) THEN
          LDUVCU(I) = LQ(LKUVCU(I)-1)
        ELSE
          LDUVCU(I) = 0
        ENDIF
#if !defined(CERNLIB__DEBUG)
        IF (LDUVCU(I).NE.0.AND.I.EQ.KEYS(NOFF2+1))
#endif
#if defined(CERNLIB__DEBUG)
        IF (LDUVCU(I).NE.0)
#endif
     +    CALL DZSHOW ('UVDS    ', IDIVCU, LDUVCU(I), 'V', 0, 0, 0, 0)
        IF (LKUVCV(I).GT.0) THEN
          LDUVCV(I) = LQ(LKUVCV(I)-1)
        ELSE
          LDUVCV(I) = 0
        ENDIF
#if !defined(CERNLIB__DEBUG)
        IF (LDUVCV(I).NE.0.AND.I.EQ.KEYS(NOFF2+1))
#endif
#if defined(CERNLIB__DEBUG)
        IF (LDUVCV(I).NE.0)
#endif
     +    CALL DZSHOW ('UVDS    ', IDIVCU, LDUVCV(I), 'V', 0, 0, 0, 0)
   10 CONTINUE
      CALL CDFREE ('//DBL3/MUCH/MCALB/UVDS', LKUVCU, MASK, KEYS, CHOPT,
     +             IRC)
      CALL CDFREE ('//DVL3/MUCH/MCALB/UVDS', LKUVCV, MASK, KEYS, CHOPT,
     +             IRC)
*
* *** -------------- HRAS ---------------------
*
      CALL VZERO  (MASK, 100)
      CALL VZERO  (KEYS, 100)
      MASK(NOFF2+1) = 1
      MASK(NOFF2+2) = 1
      KEYS(NOFF2+1) = 16
      KEYS(NOFF2+2) = 5
      DO 15 I = 1, KEYS(NOFF2+1)
        KEYS(NOFF2+2+I) = I
   15 CONTINUE
      IS    = NOFF2 + 2 + KEYS(NOFF2+1)
      DO 20 I = 1, KEYS(NOFF2+2)
        KEYS(IS+I) = I
   20 CONTINUE
      CHOPT = 'AM'
      CALL CDUSEDB ('//DBL3/MUCH/MCALB/ALIG/HRAS', LKHRCU(1,1), ITIME,
     +              MASK, KEYS, CHOPT, IRC)
      CALL CDUSEDB ('//DVL3/MUCH/MCALB/ALIG/HRAS', LKHRCV(1,1), ITIME,
     +              MASK, KEYS, CHOPT, IRC)
      DO 30 J = 1, KEYS(NOFF2+2)
        DO 25 I = 1, KEYS(NOFF2+1)
          IF (LKHRCU(I,J).GT.0) THEN
            LDHRCU(I,J) = LQ(LKHRCU(I,J)-1)
          ELSE
            LDHRCU(I,J) = 0
          ENDIF
#if !defined(CERNLIB__DEBUG)
          IF (LDHRCU(I,J).NE.0.AND.I.EQ.KEYS(NOFF2+1))
#endif
#if defined(CERNLIB__DEBUG)
          IF (LDHRCU(I,J).NE.0)
#endif
     +      CALL DZSHOW ('HRAS    ', IDIVCU, LDHRCU(I,J), 'V', 0,0, 0,0)
          IF (LKHRCV(I,J).GT.0) THEN
            LDHRCV(I,J) = LQ(LKHRCV(I,J)-1)
          ELSE
            LDHRCV(I,J) = 0
          ENDIF
#if !defined(CERNLIB__DEBUG)
          IF (LDHRCV(I,J).NE.0.AND.I.EQ.KEYS(NOFF2+1))
#endif
#if defined(CERNLIB__DEBUG)
          IF (LDHRCV(I,J).NE.0)
#endif
     +      CALL DZSHOW ('HRAS    ', IDIVCU, LDHRCV(I,J), 'V', 0,0, 0,0)
   25   CONTINUE
   30 CONTINUE
      CALL CDFREE ('//DBL3/MUCH/MCALB/ALIG/HRAS', LKHRCU, MASK, KEYS,
     +             CHOPT, IRC)
      CALL CDFREE ('//DVL3/MUCH/MCALB/ALIG/HRAS', LKHRCV, MASK, KEYS,
     +             CHOPT, IRC)
*
* *** -------------- BWIR ---------------------
*
      CHOPT = 'AV'
      CALL VZERO  (MASK, 100)
      CALL CDUSE ('//DBL3/MUCH/MPARS/BWIR', LKBWCU, ITIME, CHOPT, IRC)
      CALL CDUSE ('//DVL3/MUCH/MPARS/BWIR', LKBWCV, ITIME, CHOPT, IRC)
      IF (LKBWCU.GT.0) THEN
        LDBWCU = LQ(LKBWCU-1)
        CALL DZSHOW ('BWIR    ', IDIVCU, LDBWCU, 'V', 0, 0, 0, 0)
      ENDIF
      IF (LKBWCV.GT.0) THEN
        LDBWCV = LQ(LKBWCV-1)
        CALL DZSHOW ('BWIR    ', IDIVCU, LDBWCV, 'V', 0, 0, 0, 0)
      ENDIF
      CALL CDFREE ('//DBL3/MUCH/MPARS/BWIR', LKBWCU, MASK, KEYS,
     +             CHOPT, IRC)
      CALL CDFREE ('//DVL3/MUCH/MPARS/BWIR', LKBWCV, MASK, KEYS,
     +             CHOPT, IRC)
*
* *** -------------- YBRI ---------------------
*
      CALL VZERO  (MASK, 100)
      CALL VZERO  (KEYS, 100)
      MASK(NOFF2+1) = 1
      MASK(NOFFP)   = 1
      KEYS(NOFFP)   = 1
      KEYS(NOFF2+1) = 17
      DO 35 I = 1, KEYS(NOFF2+1)
        KEYS(NOFF2+1+I) = I
   35 CONTINUE
      CHOPT = 'AM'
      CALL CDUSEDB ('//DBL3/MUCH/MPARS/MGEOM/YPCH/YBRI', LKYBCU(1),
     +              ITIME, MASK, KEYS, CHOPT, IRC)
      CALL CDUSEDB ('//DVL3/MUCH/MPARS/MGEOM/YPCH/YBRI', LKYBCV(1),
     +              ITIME, MASK, KEYS, CHOPT, IRC)
      DO 40 I = 1, KEYS(NOFF2+1)
        IF (LKYBCU(I).GT.0) THEN
          LDYBCU(I) = LQ(LKYBCU(I)-1)
        ELSE
          LDYBCU(I) = 0
        ENDIF
#if !defined(CERNLIB__DEBUG)
        IF (LDYBCU(I).NE.0.AND.I.EQ.KEYS(NOFF2+1))
#endif
#if defined(CERNLIB__DEBUG)
        IF (LDYBCU(I).NE.0)
#endif
     +    CALL DZSHOW ('YBRI    ', IDIVCU, LDYBCU(I), 'V', 0, 0, 0, 0)
        IF (LKYBCV(I).GT.0) THEN
          LDYBCV(I) = LQ(LKYBCV(I)-1)
        ELSE
          LDYBCV(I) = 0
        ENDIF
#if !defined(CERNLIB__DEBUG)
        IF (LDYBCV(I).NE.0.AND.I.EQ.KEYS(NOFF2+1))
#endif
#if defined(CERNLIB__DEBUG)
        IF (LDYBCV(I).NE.0)
#endif
     +    CALL DZSHOW ('YBRI    ', IDIVCU, LDYBCV(I), 'V', 0, 0, 0, 0)
   40 CONTINUE
      CALL CDFREE ('//DBL3/MUCH/MPARS/MGEOM/YPCH/YBRI', LKYBCU, MASK,
     +             KEYS, CHOPT, IRC)
      CALL CDFREE ('//DVL3/MUCH/MPARS/MGEOM/YPCH/YBRI', LKYBCV, MASK,
     +             KEYS, CHOPT, IRC)
#if defined(CERNLIB__DEBUG)
*
      CALL DZSNAP ('MEMORY MAP OF MUON DATABASE', IDIVCU, 'LM')
#endif
*
      CALL TIMEX (TIMNOW)
      TIME  = TIMNOW - TIMINT
      CALL CDUVTX (IDAY, ITIM2)
      ITIME = ITIM2 - ITIM1
      WRITE (LPRTCU, 1001) IDAY, ITIM1, ITIM2, ITIME
      WRITE (LPRTCU, 1002) TIMINT, TIMNOW, TIME
*
 1001 FORMAT (/,' CDONMU : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CDONMU : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                             END CDONMU
      END
