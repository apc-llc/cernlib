*
* $Id: ihrfil.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihrfil.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/05 26/05/93  17.59.17  by  O.Couet
*-- Author :
      SUBROUTINE IHRFIL(NN,XY)
************************************************************************
*                                                                      *
*     IHRFIL                                     Date:    14.05.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Fill a polygon including border ("RASTER SCREEN")      *
*                                                                      *
*     Input: NN      - number of polygon nodes                         *
*            XY(2,*) - polygon nodes                                   *
*                                                                      *
************************************************************************
#include "higz/hcrast.inc"
      PARAMETER         (LMAX=12)
      REAL              XY(2,*)
      INTEGER           YMIN,YMAX,X,Y,XX,YY,DX,DY,SIGNDX,T,STEP,YSCAN
      INTEGER           XX1,XX2
      INTEGER           X1(LMAX+2),Y1(LMAX+2),X2(LMAX),Y2(LMAX)
      INTEGER           XCUR(LMAX),XNEX(LMAX),TEST(LMAX),XSCAN(2,LMAX)
*-
**          T R A N S F E R   T O   S C R E E N   C O O R D I N A T E S
*
      IF (IFRAST .NE. 1)                GOTO 999
      N      = NN
      DO 100 I=1,N
        X1(I)  = NXRAST * ((XY(1,I)-XRAST)/DXRAST) - 0.01
        Y1(I)  = NYRAST * ((XY(2,I)-YRAST)/DYRAST) - 0.01
  100   CONTINUE
      X1(N+1) = X1(1)
      Y1(N+1) = Y1(1)
*
**          F I N D   Y - M I N   A N D   Y - M A X
**          S E T   R I G H T   E D G E   O R I E N T A T I O N
*
      YMIN   = Y1(1)
      YMAX   = Y1(1)
      DO 200 I=1,N
        IF (YMIN  .GT. Y1(I))           YMIN  = Y1(I)
        IF (YMAX  .LT. Y1(I))           YMAX  = Y1(I)
        IF (Y1(I) .LE. Y1(I+1)) THEN
          X2(I)  = X1(I+1)
          Y2(I)  = Y1(I+1)
        ELSE
          X2(I)  = X1(I)
          Y2(I)  = Y1(I)
          X1(I)  = X1(I+1)
          Y1(I)  = Y1(I+1)
        END IF
  200   CONTINUE
      IF (YMIN .GE. NYRAST)             GOTO 999
      IF (YMAX .LT. 0)                  GOTO 999
      IF (YMAX .GE. NYRAST)             YMAX = NYRAST - 1
*
**          S O R T   L I N E S
*
      DO 250 I=1,N-1
        IF (Y1(I+1) .GE. Y1(I))         GOTO 250
        Y      = Y1(I+1)
        K      = 1
        DO 210 J=I-1,1,-1
          IF (Y .LT. Y1(J))             GOTO 210
          K      = J + 1
          GOTO 220
  210     CONTINUE
  220   X      = X1(I+1)
        XX     = X2(I+1)
        YY     = Y2(I+1)
        DO 230 J=I,K,-1
          X1(J+1) = X1(J)
          Y1(J+1) = Y1(J)
          X2(J+1) = X2(J)
          Y2(J+1) = Y2(J)
  230     CONTINUE
        X1(K) = X
        Y1(K) = Y
        X2(K) = XX
        Y2(K) = YY
  250   CONTINUE
*
**          S E T   I N I T I A L   V A L U E S
*
      DO 300 I=1,N
        XCUR(I) = X1(I)
        DY      = Y2(I) - Y1(I)
        DX      = X2(I) - X1(I)
        SIGNDX  = 1
        IF (DX .LT. 0)                  SIGNDX =-1
        IF (DX .LT. 0)                  DX =-DX
        IF (DX .LE. DY) THEN
          T       =-(DY+1)/2 + DX
          IF (T .LT. 0) THEN
            TEST(I) = T
            XNEX(I) = XCUR(I)
          ELSE
            TEST(I) = T - DY
            XNEX(I) = XCUR(I) + SIGNDX
          END IF
        ELSE IF (DY .NE. 0) THEN
          STEP    = (DX-1)/(DY+DY) + 1
          TEST(I) = STEP*DY - (DX+1)/2 - DX
          XNEX(I) = XCUR(I) + SIGNDX*STEP
        END IF
  300   CONTINUE
*
**          L O O P   O N   S C A N   L I N E S
*
      NSTART   = 1
      DO 700 YSCAN=YMIN,YMAX
        NX     = 0
        NXA    = 0
        NXB    = LMAX + 1
        DO 440 I=NSTART,N
          IF (Y1(I) .GT. YSCAN)         GOTO 500
          IF (Y2(I) .GT. YSCAN)         GOTO 410
          IF (I .EQ. NSTART)            NSTART = NSTART + 1
          IF (Y2(I) .NE. YSCAN)         GOTO 440
          NXB     = NXB - 1
          IF (X2(I) .GE. XCUR(I)) THEN
           XSCAN(1,NXB) = XCUR(I)
           XSCAN(2,NXB) = X2(I)
          ELSE
           XSCAN(1,NXB) = X2(I)
           XSCAN(2,NXB) = XCUR(I)
          END IF
          GOTO 440
*
**          S T O R E   C U R R E N T  X
**          P R E P A R E   X   F O R   N E X T   S C A N - L I N E
*
  410     NXA    = NXA + 1
          DY     = Y2(I) - Y1(I)
          DX     = X2(I) - X1(I)
          IF (DX .GE. 0) THEN
            SIGNDX = 1
            XSCAN(1,NXA) = XCUR(I)
            XSCAN(2,NXA) = XNEX(I)
            IF(XSCAN(1,NXA).NE.XSCAN(2,NXA)) XSCAN(2,NXA)=XSCAN(2,NXA)-1
          ELSE
            DX     =-DX
            SIGNDX =-1
            XSCAN(1,NXA) = XNEX(I)
            XSCAN(2,NXA) = XCUR(I)
            IF(XSCAN(1,NXA).NE.XSCAN(2,NXA)) XSCAN(1,NXA)=XSCAN(1,NXA)+1
          END IF
          XCUR(I) = XNEX(I)
          IF (DX .GT. DY)               GOTO 430
          TEST(I) = TEST(I) + DX
          IF (TEST(I) .LT. 0)           GOTO 440
          TEST(I) = TEST(I) - DY
          XNEX(I) = XNEX(I) + SIGNDX
          GOTO 440
  430     STEP   = DX/DY
          T      = TEST(I) + STEP*DY
          IF (T .GE. 0) THEN
            TEST(I) = T - DX
            XNEX(I) = XNEX(I) + SIGNDX*STEP
          ELSE
            TEST(I) = T + DY - DX
            XNEX(I) = XNEX(I) + SIGNDX*(STEP+1)
          END IF
  440     CONTINUE
*
**          S O R T   P O I N T S   A L O N G   X
*
  500   IF (YSCAN .LT. 0)                       GOTO 700
        IBASE  = YSCAN*NXRAST
        IF (NXA .LT. 2)                         GOTO 540
        DO 520 I=1,NXA-1
          DO 510 J=I,1,-1
            IF (XSCAN(1,J+1) .GE. XSCAN(1,J))   GOTO 520
            X            = XSCAN(1,J+1)
            XSCAN(1,J+1) = XSCAN(1,J)
            XSCAN(1,J)   = X
            X            = XSCAN(2,J+1)
            XSCAN(2,J+1) = XSCAN(2,J)
            XSCAN(2,J)   = X
  510       CONTINUE
  520     CONTINUE
        DO 530 I=1,NXA,2
          NX     = NX  + 1
          XSCAN(1,NX) = XSCAN(1,I)
          X           = XSCAN(2,I+1)
          IF (XSCAN(2,I) .GT. X)                X = XSCAN(2,I)
          XSCAN(2,NX) = X
  530     CONTINUE
*
  540   IF (NXB .GT. LMAX)                      GOTO 600
        DO 550 I=NXB,LMAX,1
          NX     = NX  + 1
          XSCAN(1,NX) = XSCAN(1,I)
          XSCAN(2,NX) = XSCAN(2,I)
  550     CONTINUE
*
**          C O N C A N T I N A T E   A N D   F I L L
*
  600   IF (NX .EQ. 0)                          GOTO 700
        XX1    = XSCAN(1,NX)
        XX2    = XSCAN(2,NX)
        NX     = NX - 1
        K      = 1
  610   IF (K .GT. NX)                          GOTO 630
        IF (XSCAN(1,K) .GT. XX2+1)              GOTO 620
        IF (XSCAN(2,K) .LT. XX1-1)              GOTO 620
        IF (XSCAN(1,K) .LT. XX1)                XX1 = XSCAN(1,K)
        IF (XSCAN(2,K) .GT. XX2)                XX2 = XSCAN(2,K)
        XSCAN(1,K) = XSCAN(1,NX)
        XSCAN(2,K) = XSCAN(2,NX)
        NX     = NX - 1
        GOTO 610
  620   K      = K + 1
        GOTO 610
  630   IF (XX1 .LT. 0)                         XX1 = 0
        IF (XX2 .GE. NXRAST)                    XX2 = NXRAST - 1
        NBIT   = XX2 - XX1 + 1
        KBIT   = IBASE + XX1
        IW     = KBIT / NBW
        IB     = KBIT - IW*NBW + 1
        IW     = IW + JRAST + 1
        NB     = NBW - IB + 1
        IF (NB .GT. NBIT)                       NB = NBIT
        IRAST(IW) = IOR(IRAST(IW),MASK(JMASK(NB)+IB))
        NBIT   = NBIT - NB
        IF (NBIT .EQ. 0)                        GOTO 600
        DO 640 WHILE (NBIT .GT. NBW)
          IW        = IW + 1
          IRAST(IW) = MASK(LMASK)
          NBIT      = NBIT - NBW
  640   CONTINUE
        IW        = IW + 1
        IRAST(IW) = IOR(IRAST(IW),MASK(JMASK(NBIT)+1))
        GOTO 600
  700   CONTINUE
*
  999 RETURN
      END
