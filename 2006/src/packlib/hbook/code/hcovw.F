*
* $Id: hcovw.F,v 1.1.1.1 1996/01/16 17:07:34 mclareni Exp $
*
* $Log: hcovw.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:34  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 31/08/90  18.52.54  by  Rene Brun
*-- Author :
      SUBROUTINE HCOVW (X,EY,W,WT,B,BT,DD,FF,IBASFT,JBF)
*.==========>
*.      FORM VARIANCE-COVARIANCE MATRIX OF W WITH
*.      ONE ADDITIONAL BASIC FUNCTION IBASFT(*,JBF)
*..=========> ( D.Lienart )
#include "hbook/hcpar1.inc"
      DIMENSION X(NPMAX,ND),EY(1),W(NPMAX,NCOMAX),WT(1),
     +          B(NCOMAX,NCOMAX),BT(NCOMAX,NCOMAX),DD(1),FF(1),
     +          IBASFT(ND,NBFMAX),XV(10)
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION W,WT,WNT,B,BT,DD,FF,E,HSTELF,HELEFT,HBASFT,DP
#endif
*
*  DETERMINE IF THE GIVEN BASIC FUNCTION IS THE CONSTANT TERM
*
      ICONST=1
      DO 5 I=1,ND
         IF (IBASFT(I,JBF).NE.0) ICONST=0
    5 CONTINUE
      DO 10 K=1,NP
         WT(K)=1.
   10 CONTINUE
*
*  COMPUTE THE NP VALUES OF THE BASIC FUNCTION IN VECTOR WT
*
      IF (ICONST.EQ.0) THEN
         DO 35 I=1,ND
            NUM=IBASFT(I,JBF)/10
            ITYP=IBASFT(I,JBF)-NUM*10
            IF (NUM.NE.0) THEN
               IF (ITYP.EQ.0) THEN
                  DO 15 K=1,NP
                     WT(K)=WT(K)*HSTELF(IOPT(4),NUM,X(K,I))
   15             CONTINUE
               ELSE IF (ITYP.EQ.1) THEN
                  DO 20 K=1,NP
                     WT(K)=WT(K)*HELEFT(NUM,X(K,I))
   20             CONTINUE
               ELSE IF (ITYP.EQ.2) THEN
                  DO 30 K=1,NP
                     DO 25 J=1,ND
                        XV(J)=X(K,J)
   25                CONTINUE
                     WT(K)=HBASFT(NUM,XV)
   30             CONTINUE
                  GOTO 40
               ENDIF
            ENDIF
   35    CONTINUE
      ENDIF
*
*  APPLY WEIGHT CORRECTION TO WT
*
   40 IF (IOPT(3).EQ.0) THEN
         DO 45 K=1,NP
            WT(K)=WT(K)/EY(K)
   45    CONTINUE
      ENDIF
*
*  UPDATE COVARIANCE MATRIX B: FIRST PARTITION W'W, THEN
*  INVERT THIS PARTITIONED MATRIX
*
      WNT=0.
      DO 47 I=1,NP
         WNT=WNT+WT(I)*WT(I)
   47 CONTINUE
      DO 50 I=1,NCO-1
         DP=0.
         DO 49 K=1,NP
            DP=DP+WT(K)*W(K,I)
   49    CONTINUE
         DD(I)=DP
   50 CONTINUE
      E=0.
      DO 55 I=1,NCO-1
         DP=0.
         DO 52 K=1,NCO-1
            DP=DP+DD(K)*B(K,I)
   52    CONTINUE
         FF(I)=DP
         E=E+DP*DD(I)
   55 CONTINUE
      E=1./(WNT-E)
      DO 65 I=1,NCO-1
         DO 60 J=1,NCO-1
            BT(I,J)=B(I,J)+E*FF(I)*FF(J)
   60    CONTINUE
         BT(I,NCO)=-FF(I)*E
         BT(NCO,I)=BT(I,NCO)
   65 CONTINUE
      BT(NCO,NCO)=E
      END
