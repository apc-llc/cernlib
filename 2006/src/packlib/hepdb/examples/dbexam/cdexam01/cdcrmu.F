*
* $Id: cdcrmu.F,v 1.1.1.1 1996/02/28 16:23:44 mclareni Exp $
*
* $Log: cdcrmu.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:44  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCRMU (ITIMB)
*     =========================
*
************************************************************************
*                                                                      *
*        SUBR. CDCRMU (ITIMB)                                          *
*                                                                      *
*   Creates the imitation of the Muon Chamber Data Base                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITIMB    Start time of validity                                  *
*                                                                      *
*   Called by CEXAM01                                                  *
*                                                                      *
************************************************************************
*
#include "cstor1.inc"
#include "cuser1.inc"
      PARAMETER       (NOFF1=10, NOFF2=12, NOFFP=6)
      DIMENSION       KEYS(20)
      DATA            IPROG /1/, XRANG / 1000.0/, XCENT / 500.0/
*     ------------------------------------------------------------------
*
      CALL TIMEX (TIMINT)
      CALL CDUVTX (IDAY, ITIM1)
      CALL CDPKTS (991231, 235959, INFIN, IRC)
*
* *** --------  UVDS  ----------------------
*
      NKEY8 = 16
      NDAT  = 48
      NKEYS = NOFF2 + 1
      CALL VZERO (KEYS, NKEYS)
      KEYS(NOFF1+1) = ITIMB
      KEYS(NOFF1+2) = INFIN
      KEYS(NOFFP)   = IPROG
      DO 10 K = 1, NKEY8
        CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'UVDS', 0, 0, NDAT, 3,0)
        KEYS(NOFF2+1) = K
        DO 5 I = 1, NDAT
          Q(LUSECU+I) = XRANG*RNDM(Q) - XCENT
    5   CONTINUE
        CALL CDSTOR ('//DBL3/MUCH/MCALB/UVDS', LUSECU, LKUVCU(K),
     +               IDIVCU, KEYS, 'CDP', IRC)
#if !defined(CERNLIB__DEBUG)
        IF (LUSECU.NE.0 .AND. K.EQ.NKEY8)
#endif
#if defined(CERNLIB__DEBUG)
        IF (LUSECU.NE.0)
#endif
     +    CALL DZSHOW ('UVDS    ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
        CALL MZDROP (IDIVCU, LUSECU, ' ')
   10 CONTINUE
#if defined(CERNLIB__DEBUG)
      K     = 1
#endif
#if !defined(CERNLIB__DEBUG)
      K     = NKEY8
#endif
      CALL DZSHOW ('LKUVCU ', IDIVCU, LKUVCU(K), 'BLV', 0, 0, 0, 0)
      CALL MZDROP (IDIVCU, LKUVCU(1), 'L')
      LKUVCU(1) = 0
*
* *** --------  HRAS  ----------------------
*
      NKEY8 = 16
      NKEY9 = 5
      NDAT  = 21
      NKEYS = NOFF2 + 2
      CALL VZERO (KEYS, NKEYS)
      KEYS(NOFF1+1) = ITIMB
      KEYS(NOFF1+2) = INFIN
      KEYS(NOFFP)   = IPROG
      DO 25 L = 1, NKEY9
        DO 20 K = 1, NKEY8
          CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'HRAS', 0,0, NDAT,3,0)
          KEYS(NOFF2+1) = K
          KEYS(NOFF2+2) = L
          DO 15 I = 1, NDAT
            Q(LUSECU+I) = XRANG*RNDM(Q) - XCENT
   15     CONTINUE
          CALL CDSTOR ('//DBL3/MUCH/MCALB/ALIG/HRAS', LUSECU, LKUVCU(1),
     +                 IDIVCU, KEYS, 'DP', IRC)
#if !defined(CERNLIB__DEBUG)
          IF (LUSECU.NE.0 .AND. K.EQ.NKEY8)
#endif
#if defined(CERNLIB__DEBUG)
          IF (LUSECU.NE.0)
#endif
     +      CALL DZSHOW ('HRAS    ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
          CALL MZDROP (IDIVCU, LUSECU, ' ')
   20   CONTINUE
   25 CONTINUE
      IF (LKUVCU(1).NE.0) CALL MZDROP (IDIVCU, LKUVCU(1), 'L')
      LKUVCU(1) = 0
*
* *** --------  BEAC  ----------------------
*
      NKEY8 = 16
      NDAT  = 67
      NKEYS = NOFF2 + 1
      CALL VZERO (KEYS, NKEYS)
      KEYS(NOFF1+1) = ITIMB
      KEYS(NOFF1+2) = INFIN
      KEYS(NOFFP)   = IPROG
      DO 35 K = 1, NKEY8
        CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'BEAC', 0, 0, NDAT, 3,0)
        KEYS(NOFF2+1) = K
        DO 30 I = 1, NDAT
          Q(LUSECU+I) = XRANG*RNDM(Q) - XCENT
   30   CONTINUE
        CALL CDSTOR ('//DBL3/MUCH/MCALB/ALIG/BEAC', LUSECU, LKUVCU(1),
     +               IDIVCU, KEYS, 'DP', IRC)
#if !defined(CERNLIB__DEBUG)
        IF (LUSECU.NE.0 .AND. K.EQ.NKEY8)
#endif
#if defined(CERNLIB__DEBUG)
        IF (LUSECU.NE.0)
#endif
     +    CALL DZSHOW ('BEAC    ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
        CALL MZDROP (IDIVCU, LUSECU, ' ')
   35 CONTINUE
      IF (LKUVCU(1).NE.0) CALL MZDROP (IDIVCU, LKUVCU(1), 'L')
      LKUVCU(1) = 0
*
* *** --------  BWIR  ----------------------
*
      NDAT  = 100
      NKEYS = NOFF2
      CALL VZERO (KEYS, NKEYS)
      KEYS(NOFF1+1) = ITIMB
      KEYS(NOFF1+2) = INFIN
      KEYS(NOFFP)   = IPROG
      CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'BWIR', 0, 0, NDAT, 2, 0)
      DO 40 I = 1, NDAT/4
        J     = 4*(I-1)
        IQ(LUSECU+J+1) = 12
        IQ(LUSECU+J+2) = 5
        IQ(LUSECU+J+3) = -1
        IQ(LUSECU+J+4) = 1000*RNDM(Q)
   40 CONTINUE
      CALL CDSTOR ('//DBL3/MUCH/MPARS/BWIR', LUSECU, LKUVCU(1),
     +             IDIVCU, KEYS, 'DP', IRC)
      CALL DZSHOW ('BWIR    ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
      CALL MZDROP (IDIVCU, LUSECU, ' ')
      IF (LKUVCU(1).NE.0) CALL MZDROP (IDIVCU, LKUVCU(1), 'L')
      LKUVCU(1) = 0
*
* *** --------  XBRI  ----------------------
*
      NKEY8 = 17
      NKEY9 = 5
      NDAT  = 30
      NKEYS = NOFF2 + 2
      CALL VZERO (KEYS, NKEYS)
      KEYS(NOFF1+1) = ITIMB
      KEYS(NOFF1+2) = INFIN
      KEYS(NOFFP)   = IPROG
      DO 55 L = 1, NKEY9
        DO 50 K = 1, NKEY8
          CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'XBRI', 0,0, NDAT,3,0)
          KEYS(NOFF2+1) = K
          KEYS(NOFF2+2) = L
          DO 45 I = 1, NDAT
            Q(LUSECU+I) = XRANG*RNDM(Q) - XCENT
   45     CONTINUE
          CALL CDSTOR ('//DBL3/MUCH/MPARS/MGEOM/XBRI', LUSECU,
     +                 LKUVCU(1), IDIVCU, KEYS, 'DP', IRC)
#if !defined(CERNLIB__DEBUG)
          IF (LUSECU.NE.0 .AND. K.EQ.NKEY8)
#endif
#if defined(CERNLIB__DEBUG)
          IF (LUSECU.NE.0)
#endif
     +      CALL DZSHOW ('XBRI    ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
          CALL MZDROP (IDIVCU, LUSECU, ' ')
   50  CONTINUE
   55 CONTINUE
      IF (LKUVCU(1).NE.0) CALL MZDROP (IDIVCU, LKUVCU(1), 'L')
      LKUVCU(1) = 0
*
* *** --------  YAPL  ----------------------
*
      NKEY8 = 17
      NDAT  = 67
      NKEYS = NOFF2 + 1
      CALL VZERO (KEYS, NKEYS)
      KEYS(NOFF1+1) = ITIMB
      KEYS(NOFF1+2) = INFIN
      KEYS(NOFFP)   = IPROG
      DO 65 K = 1, NKEY8
        CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'YAPL', 0, 0, NDAT, 3,0)
        KEYS(NOFF2+1) = K
        DO 60 I = 1, NDAT
          Q(LUSECU+I) = XRANG*RNDM(Q) - XCENT
   60   CONTINUE
        CALL CDSTOR ('//DBL3/MUCH/MPARS/MGEOM/YPCH/YAPL', LUSECU,
     +               LKUVCU(1), IDIVCU, KEYS, 'DP', IRC)
#if !defined(CERNLIB__DEBUG)
        IF (LUSECU.NE.0 .AND. K.EQ.NKEY8)
#endif
#if defined(CERNLIB__DEBUG)
        IF (LUSECU.NE.0)
#endif
     +    CALL DZSHOW ('YAPL    ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
        CALL MZDROP (IDIVCU, LUSECU, ' ')
   65 CONTINUE
      IF (LKUVCU(1).NE.0) CALL MZDROP (IDIVCU, LKUVCU(1), 'L')
      LKUVCU(1) = 0
*
* *** --------  YBRI  ----------------------
*
      NKEY8 = 17
      NDAT  = 43
      NKEYS = NOFF2 + 1
      CALL VZERO (KEYS, NKEYS)
      KEYS(NOFF1+1) = ITIMB
      KEYS(NOFF1+2) = INFIN
      KEYS(NOFFP)   = IPROG
      DO 75 K = 1, NKEY8
        CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'YBRI', 0, 0, NDAT, 3,0)
        KEYS(NOFF2+1) = K
        DO 70 I = 1, NDAT
          Q(LUSECU+I) = XRANG*RNDM(Q) - XCENT
   70   CONTINUE
        CALL CDSTOR ('//DBL3/MUCH/MPARS/MGEOM/YPCH/YBRI', LUSECU,
     +               LKUVCU(1), IDIVCU, KEYS, 'DP', IRC)
#if !defined(CERNLIB__DEBUG)
        IF (LUSECU.NE.0 .AND. K.EQ.NKEY8)
#endif
#if defined(CERNLIB__DEBUG)
        IF (LUSECU.NE.0)
#endif
     +    CALL DZSHOW ('YBRI    ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
        CALL MZDROP (IDIVCU, LUSECU, ' ')
   75 CONTINUE
      IF (LKUVCU(1).NE.0) CALL MZDROP (IDIVCU, LKUVCU(1), 'L')
      LKUVCU(1) = 0
*
      CALL TIMEX (TIMNOW)
      TIME  = TIMNOW - TIMINT
      CALL CDUVTX (IDAY, ITIM2)
      ITIME = ITIM2 - ITIM1
      WRITE (LPRTCU, 1001) IDAY, ITIM1, ITIM2, ITIME
      WRITE (LPRTCU, 1002) TIMINT, TIMNOW, TIME
*
 1001 FORMAT (/,' CDCRMU : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CDCRMU : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                             END CDCRMU
      END
