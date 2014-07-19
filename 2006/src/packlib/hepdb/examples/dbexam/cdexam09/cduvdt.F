*
* $Id: cduvdt.F,v 1.1.1.1 1996/02/28 16:23:53 mclareni Exp $
*
* $Log: cduvdt.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:53  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUVDT
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDUVDT                                                  *
*                                                                      *
*   Tests the objects stored in the directories                        *
*                                                                      *
*   Called by CEXAM09                                                  *
*                                                                      *
************************************************************************
*
#include "ckkeys.inc"
#include "cstor9.inc"
#include "cuser9.inc"
      CHARACTER       PATHN*30
      PARAMETER       (NOFFP=6)
*
*     ------------------------------------------------------------------
*
* *** Get Real time at the beginning of the loop
*
      CALL TIMEX (TIMINT)
      CALL CDUVTX (IDAY, ITIM1)
*
* *** Now test the first directory
*
      PATHN = '//DBL3/DBMU/MCALB/UVD1'
      WRITE (LPRTCU, 1003) PATHN
      NERR   = 0
      DO 20 IOBJ = 1, NOBJCU
        IERR   = 0
        KEY1S  = IOBJ
        NDAT   = IQ(LUSECU(IOBJ)-1)
        CALL CDBKIN (PATHN, KEY1S, IDIVCU, LDUVCU, LKUVCU, 2, IRC)
        NDATD  = IQ(LDUVCU-1)
        DO 10 K = 1, NKEYCU
          IF (K.GE.NOFFP.AND.KEYVCK(K).NE.KEYSCU(K,IOBJ)) IERR = IERR +1
   10   CONTINUE
        IF (NDAT.NE.NDATD) THEN
          IERR   = IERR + 1
        ELSE
          DO 15 I = 1, NDAT
            DIFF   = (Q(LUSECU(IOBJ)+I) - Q(LDUVCU+I)) / AMAX1
     +               (ABS(Q(LDUVCU+I)), ABS(Q(LUSECU(IOBJ)+I)), 1.0E-12)
            IF (ABS(DIFF).GT.0.01) IERR = IERR + 1
   15     CONTINUE
        ENDIF
        IF (IERR.GT.0) THEN
          NERR   = NERR + 1
#if defined(CERNLIB__DEBUG)
          WRITE (LPRTCU, 1004) (KEYSCU(K,IOBJ), K = 1, NKEYCU)
          WRITE (LPRTCU, 1005) (KEYVDK(K), K = 1, NKEYCU)
          WRITE (LPRTCU, 1006) NDAT, (Q(LUSECU(IOBJ)+I), I = 1, NDAT)
          WRITE (LPRTCU, 1007) NDATD, (Q(LDUVCU+I), I = 1, NDATD)
#endif
        ENDIF
        IF (LDUVCU.GT.0) CALL MZDROP (ISTOCU, LDUVCU, 'L')
   20 CONTINUE
      WRITE (LPRTCU, 1008) NERR, NOBJCU
*
* *** Now test the second directory
*
      PATHN = '//DBL3/DBMU/MCALB/UVD2'
      WRITE (LPRTCU, 1003) PATHN
      NERR   = 0
      DO 40 IOBJ = 1, NOBJCU
        IERR   = 0
        KEY1S  = IOBJ
        NDAT   = IQ(LUSECU(IOBJ)-1)
        CALL CDBKIN (PATHN, KEY1S, IDIVCU, LDUVCU, LKUVCU, 2, IRC)
        NDATD  = IQ(LDUVCU-1)
        DO 30 K = 1, NKEYCU
          IF (K.GE.NOFFP.AND.KEYVCK(K).NE.KEYSCU(K,IOBJ)) IERR = IERR +1
   30   CONTINUE
        IF (NDAT.NE.NDATD) THEN
          IERR   = IERR + 1
        ELSE
          DO 35 I = 1, NDAT
            DIFF   = (Q(LUSECU(IOBJ)+I) - Q(LDUVCU+I)) / AMAX1
     +               (ABS(Q(LDUVCU+I)), ABS(Q(LUSECU(IOBJ)+I)), 1.0E-12)
            IF (ABS(DIFF).GT.0.01) IERR = IERR + 1
   35     CONTINUE
        ENDIF
        IF (IERR.GT.0) THEN
          NERR   = NERR + 1
#if defined(CERNLIB__DEBUG)
          WRITE (LPRTCU, 1004) (KEYSCU(K,IOBJ), K = 1, NKEYCU)
          WRITE (LPRTCU, 1005) (KEYVDK(K), K = 1, NKEYCU)
          WRITE (LPRTCU, 1006) NDAT, (Q(LUSECU(IOBJ)+I), I = 1, NDAT)
          WRITE (LPRTCU, 1007) NDATD, (Q(LDUVCU+I), I = 1, NDATD)
#endif
        ENDIF
        IF (LDUVCU.GT.0) CALL MZDROP (ISTOCU, LDUVCU, 'L')
   40 CONTINUE
      WRITE (LPRTCU, 1008) NERR, NOBJCU
*
* *** Now test the third directory
*
      PATHN = '//DBL3/DBMU/MCALB/UVD3'
      WRITE (LPRTCU, 1003) PATHN
      NERR   = 0
      DO 60 IOBJ = 1, NOBJCU
        IERR   = 0
        KEY1S  = IOBJ
        NDAT   = IQ(LUSECU(IOBJ)-1)
        CALL CDBKIN (PATHN, KEY1S, IDIVCU, LDUVCU, LKUVCU, 2, IRC)
        NDATD  = IQ(LDUVCU-1)
        DO 50 K = 1, NKEYCU
          IF (K.GE.NOFFP.AND.KEYVCK(K).NE.KEYSCU(K,IOBJ)) IERR = IERR +1
   50   CONTINUE
        IF (NDAT.NE.NDATD) THEN
          IERR   = IERR + 1
        ELSE
          DO 55 I = 1, NDAT
            DIFF   = (Q(LUSECU(IOBJ)+I) - Q(LDUVCU+I)) / AMAX1
     +               (ABS(Q(LDUVCU+I)), ABS(Q(LUSECU(IOBJ)+I)), 1.0E-12)
            IF (ABS(DIFF).GT.0.01) IERR = IERR + 1
   55     CONTINUE
        ENDIF
        IF (IERR.GT.0) THEN
          NERR   = NERR + 1
#if defined(CERNLIB__DEBUG)
          WRITE (LPRTCU, 1004) (KEYSCU(K,IOBJ), K = 1, NKEYCU)
          WRITE (LPRTCU, 1005) (KEYVDK(K), K = 1, NKEYCU)
          WRITE (LPRTCU, 1006) NDAT, (Q(LUSECU(IOBJ)+I), I = 1, NDAT)
          WRITE (LPRTCU, 1007) NDATD, (Q(LDUVCU+I), I = 1, NDATD)
#endif
        ENDIF
        IF (LDUVCU.GT.0) CALL MZDROP (ISTOCU, LDUVCU, 'L')
   60 CONTINUE
      WRITE (LPRTCU, 1008) NERR, NOBJCU
*
* *** Now test the fourth directory
*
      PATHN = '//DBL3/DBMU/MCALB/UVD4'
      WRITE (LPRTCU, 1003) PATHN
      NERR   = 0
      DO 80 IOBJ = 1, NOBJCU
        IERR   = 0
        KEY1S  = IOBJ
        NDAT   = IQ(LUSECU(IOBJ)-1)
        CALL CDBKIN (PATHN, KEY1S, IDIVCU, LDUVCU, LKUVCU, 2, IRC)
        NDATD  = IQ(LDUVCU-1)
        DO 70 K = 1, NKEYCU
          IF (K.GE.NOFFP.AND.KEYVCK(K).NE.KEYSCU(K,IOBJ)) IERR = IERR +1
   70   CONTINUE
        IF (NDAT.NE.NDATD) THEN
          IERR   = IERR + 1
        ELSE
          DO 75 I = 1, NDAT
            DIFF   = (Q(LUSECU(IOBJ)+I) - Q(LDUVCU+I)) / AMAX1
     +               (ABS(Q(LDUVCU+I)), ABS(Q(LUSECU(IOBJ)+I)), 1.0E-12)
            IF (ABS(DIFF).GT.0.01) IERR = IERR + 1
   75     CONTINUE
        ENDIF
        IF (IERR.GT.0) THEN
          NERR   = NERR + 1
#if defined(CERNLIB__DEBUG)
          WRITE (LPRTCU, 1004) (KEYSCU(K,IOBJ), K = 1, NKEYCU)
          WRITE (LPRTCU, 1005) (KEYVDK(K), K = 1, NKEYCU)
          WRITE (LPRTCU, 1006) NDAT, (Q(LUSECU(IOBJ)+I), I = 1, NDAT)
          WRITE (LPRTCU, 1007) NDATD, (Q(LDUVCU+I), I = 1, NDATD)
#endif
        ENDIF
        IF (LDUVCU.GT.0) CALL MZDROP (ISTOCU, LDUVCU, 'L')
   80 CONTINUE
      WRITE (LPRTCU, 1008) NERR, NOBJCU
*
* *** Get Real time at the end of the loop
*
      CALL TIMEX (TIMNOW)
      TIME  = TIMNOW - TIMINT
      CALL CDUVTX (IDAY, ITIM2)
      ITIME = ITIM2 - ITIM1
      WRITE (LPRTCU, 1001) IDAY, ITIM1, ITIM2, ITIME
      WRITE (LPRTCU, 1002) TIMINT, TIMNOW, TIME
*
 1001 FORMAT (/,' CDUVDT : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CDUVDT : TIMINT/TIMNOW/TIME ',4G12.4)
 1003 FORMAT (//,' CDUVDT : Test object stored in directory ',A)
#if defined(CERNLIB__DEBUG)
 1004 FORMAT (' Input  Keys',10I12)
 1005 FORMAT (' Output Keys',10I12)
 1006 FORMAT (' Input  Data',I12,9G12.4/(12X,10G12.4))
 1007 FORMAT (' Output Data',I12,9G12.4/(12X,10G12.4))
#endif
 1008 FORMAT (/,' CDUVDT : Number of errors ',I6,' in ',I6,' objects')
*                                                             END CDUVDT
      END
