*
* $Id: hp1dim.F,v 1.1.1.1 1996/01/16 17:07:44 mclareni Exp $
*
* $Log: hp1dim.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:44  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 23/08/89  18.05.35  by  Rene Brun
*-- Author :
      SUBROUTINE HP1DIM(C,E,F,N,XMIN,XMAX,NLINE)
*.==========>
*.           PRINTS HIST VERTICALLY
*..=========> ( R.Brun )
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
      COMMON/HCGARB/YUP,YLOW,ETOP,EDOWN,IZERO,YL,YR,K,Y0,Y1,
     +          BH,BL,BWID,NBB,Y,XS,GA
      COMMON/HFORM/IFORM(128)
      DIMENSION C(1),E(1),F(1),KY(9),ITAB(110)
      EQUIVALENCE (KY(1),IFORM(4)),(ITAB(1),IFORM(16))
      SAVE AK,IBIG
      DATA AK/10000./
      DATA IBIG/1000000/
*.___________________________________________
*
*             GET SCALE
*
      IF(XMIN.EQ.0..AND.XMAX.EQ.0.)RETURN
      IF(XMIN.GT.XMAX)RETURN
      IF(I34.NE.0)THEN
         ETOP=-BIGP
         EDOWN=BIGP
         DO 5 I=1,N
            BH=C(I)+E(I)
            BL=C(I)-E(I)
            IF(BH.GT.ETOP)ETOP=BH
            IF(BL.LT.EDOWN)EDOWN=BL
   5     CONTINUE
      ENDIF
*
      IF(I20.EQ.0)THEN
         IF(I34.NE.0)XMAX=ETOP
         IF(I12.NE.0)THEN
            YUP=VMAX(F,N)
            XMAX=MAX(YUP,XMAX)
         ENDIF
      ENDIF
      IF(I21.EQ.0)THEN
         IF(I34.NE.0)XMIN=EDOWN
         IF(I12.NE.0)THEN
            YLOW=VMIN(F,N)
            XMIN=MIN(YLOW,XMIN)
         ENDIF
      ENDIF
      Y0=XMIN
*-*      IF(XMAX.EQ.0.)XMAX=-0.000001
      Y1=XMAX
      IF(I26.NE.0)THEN
         IF(XMAX.LE.0)RETURN
         Y1=LOG10(XMAX)
         AL10=LOG(10.)
         IF(XMIN.LE.0.)THEN
            XMIN=BIGP
            DO 40 J=1,N
               IF(C(J).LT.XMIN.AND.C(J).GT.0.)XMIN=C(J)
  40        CONTINUE
         ENDIF
         Y0=LOG10(0.9999999*XMIN)
      ENDIF
*
      CALL HBIN(Y0,Y1,NLINE,BL,BH,NBB,BWID)
*
      IF(BWID.GE.5.)GO TO 70
      IF(I24.EQ.0)GO TO 70
      IF(I26.NE.0)GO TO 70
*
*             H1EVLI CALLED  STEP MUST BE INTEGER
*
      XS=1.
      IF(BWID.EQ.2.5)XS=.625
      IF(BWID.EQ.1.5)XS=.75
      IF(BWID.LT.1.) XS=BWID
      BWID=BWID/XS
      BL=BL-MOD(BL,BWID)
      IF(BL.GT.XMIN)BL=BL-BWID
      BH=BH-MOD(BH,BWID)
      IF(BH.LT.XMAX)BH=BH+BWID
      NBB=(BH-BL)/BWID +0.00001
  70  CALL VBLANK(IFORM,128)
      IF(BL*BH.LT.0.)NBB=NBB+1
      IF(BH.LT.0.)BH=BH-BWID
*
      NL=0
 100  NL=NL+1
      IF(NL.GT.NBB)GO TO 99
*
      CALL VBLANK(ITAB,N)
*
      YUP=BH+FLOAT(1-NL)*BWID
      YLOW=YUP-BWID
      IF(I26.NE.0)THEN
         YUP=EXP(YUP*AL10)
         IF(YUP.LT.XMIN)GO TO 99
         YLOW=EXP(YLOW*AL10)
      ENDIF
      IF(YUP.LT.0.)GO TO 300
      IF(XMAX.LT.0.)NBB=NBB+1
      IF(XMAX.LT.0.)GO TO 100
      IF(YLOW.LE.-0.000001)THEN
         CALL VFILL(ITAB,N,IDG(39))
         GO TO 420
      ENDIF
      IUP=AK*YUP+0.5
      ILOW=AK*YLOW+0.5
      XS=YUP-YLOW
*
      IF(I34.NE.0)GO TO 180
      IF(I27.NE.0)GO TO 130
      IF(I28.NE.0)GO TO 160
*
*             NORMAL CONTOUR HISTOGRAM
*
      DO 120 J=1,N
         IF(C(J).LE.0.)GO TO 120
         I=AK*C(J)+0.5
         IF(I.LE.ILOW)GO TO 120
         IF(I.LE.IUP)THEN
            ITAB(J)=IDG(39)
         ELSE
            ITAB(J)=IDG(19)
            IL=-IBIG
            IF(J.NE.1)IL=AK*(C(J-1)-XS)+0.5
            IR=-IBIG
            IF(J.NE.N)IR=AK*(C(J+1)-XS)+0.5
            IF(ILOW.LT.IL.AND.ILOW.LT.IR)ITAB(J)=IDG(41)
         ENDIF
 120  CONTINUE
      GO TO 200
*
*             BLACK HISTOGRAM
*
 130  CONTINUE
      DO 150 J=1,N
         IF(C(J).LE.0.)GO TO 150
         I=AK*C(J)+0.5
         IF(I.LE.ILOW)GO TO 150
         IF(I.GT.IUP)GO TO 140
*
*             COMPUTES PERCENTAGE CHARACTER
*
         K=10.*(C(J)-YLOW)/XS+1.001
         IF(K.GT.10)GO TO 140
         ITAB(J)=IDG(K)
         GO TO 150
 140     ITAB(J)=ICBLAC
 150  CONTINUE
      GO TO 200
*
*             STAR HISTOGRAM
*
 160  CONTINUE
      DO 170 J=1,N
         IF(C(J).LE.0.)GO TO 170
         I=AK*C(J)+0.5
         IF(I.LE.ILOW)GO TO 170
         IF(I.GT.IUP)GO TO 170
         ITAB(J)=ICSTAR
 170  CONTINUE
      GO TO 200
*
*             ERROR BARS
*
 180  CONTINUE
      DO 190 J=1,N
         Y=C(J)
         IF(Y.EQ.0..AND.E(J).EQ.0.)GO TO 190
         ETOP=Y+E(J)
         EDOWN=Y-E(J)
         IF(EDOWN.GT.YUP)GO TO 190
         IF(ETOP.LE.YLOW)GO TO 190
         IF(ETOP.EQ.0.)GO TO 190
         IF(E(J).NE.0.)ITAB(J)=IDG(19)
         IF(Y.GT.YLOW.AND.Y.LE.YUP)ITAB(J)=IDG(1)
 190  CONTINUE
*
*             FUNCTION SUPERIMPOSED
*
 200  IF(I12.EQ.0)GO TO 420
      DO 210 J=1,N
         Y=F(J)
         IF(Y.LE.0.)GO TO 210
         IF(Y.LE.YLOW)GO TO 210
         IF(Y.GT.YUP)GO TO 210
         ITAB(J)=ICFUNC
 210  CONTINUE
      GO TO 420
*
*
*             CONTENTS ARE NEGATIVE
*
 300  IUP=AK*YUP-0.5
      ILOW=AK*YLOW-0.5
      IF(I34.NE.0)GO TO 380
      IF(I27.NE.0)GO TO 330
      IF(I28.NE.0)GO TO 360
*
*             NORMAL CONTOUR HISTOGRAM
*
      DO 320 J=1,N
         IF(C(J).GE.0.)GO TO 320
         I=AK*(C(J)-BWID)-0.5
         IF(I.GE.IUP)GO TO 320
         IF(I.GE.ILOW)THEN
            ITAB(J)=IDG(39)
         ELSE
            ITAB(J)=IDG(19)
            IL=IBIG
            IF(J.NE.1)IL=AK*C(J-1)-0.5
            IR=IBIG
            IF(J.NE.N)IR=AK*C(J+1)-0.5
            IF(IUP.GT.IL.AND.IUP.GT.IR)ITAB(J)=IDG(41)
         ENDIF
 320  CONTINUE
      GO TO 400
*
*             BLACK HISTOGRAM
*
 330  CONTINUE
      DO 350 J=1,N
         IF(C(J).GE.0.)GO TO 350
         I=AK*(C(J)-BWID)-0.5
         IF(I.LE.ILOW)GO TO 340
         IF(I.GE.IUP)GO TO 350
*
         K=10.*(YUP-C(J)+BWID)/BWID+1.001
         IF(K.GT.10)GO TO 340
         ITAB(J)=IDG(K)
         GO TO 350
 340     ITAB(J)=ICBLAC
 350  CONTINUE
      GO TO 400
*
*             STAR HISTOGRAM
*
 360  CONTINUE
      DO 370 J=1,N
         IF(C(J).GE.0.)GO TO 370
         I=AK*(C(J)-BWID)-0.5
         IF(I.LT.ILOW)GO TO 370
         IF(I.GE.IUP)GO TO 370
         ITAB(J)=ICSTAR
 370  CONTINUE
      GO TO 400
*
*             ERROR BARS
*
 380  CONTINUE
      DO 390 J=1,N
         IF(C(J).EQ.0..AND.E(J).EQ.0.)GO TO 390
         Y=C(J)-BWID
         ETOP=Y+E(J)
         EDOWN=Y-E(J)
         IF(EDOWN.GE.YUP)GO TO 390
         IF(ETOP.LT.YLOW)GO TO 390
         IF(EDOWN.EQ.-BWID)GO TO 390
         IF(E(J).NE.0.)ITAB(J)=IDG(19)
         IF(Y.GE.YLOW.AND.Y.LT.YUP)ITAB(J)=IDG(1)
 390  CONTINUE
*
*             FUNCTION SUPERIMPOSED
*
 400  IF(I12.EQ.0)GO TO 420
      DO 410 J=1,N
         IF(F(J).GE.0.)GO TO 410
         Y=F(J)-BWID
         IF(Y.LT.YLOW)GO TO 410
         IF(Y.GE.YUP)GO TO 410
         ITAB(J)=ICFUNC
 410  CONTINUE
*
 420  CALL HBCDF(YUP,9,KY)
      CALL HFORMA(1)
      GO TO 100
*
  99  RETURN
      END
