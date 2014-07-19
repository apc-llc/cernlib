*
* $Id: hmufit.F,v 1.1.1.1 1996/01/16 17:07:43 mclareni Exp $
*
* $Log: hmufit.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:43  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 31/08/90  18.34.26  by  Rene Brun
*-- Author :
      SUBROUTINE HMUFIT (X,Y,EY,IBASFT,W,WT,WY,B,BT,BTT,DD,FF,COEFF,
     +                   CT,MARKBF,MAXBF,R2MIN,FLEVEL)
*.==========>
*.      PERFORM MULTIDIMENSIONAL FIT
*.      W      = MATRIX WHOSE COLUMNS ARE THE BASIC
*.               FUNCTIONS OR REGRESSORS
*.      WT     = ONE BASIC FUNCTION CORRESPONDING TO A COLUMN
*.               OF MATRIX W
*.      WY     = MATRIX PRODUCT W.Y
*.      B      = COVARIANCE MATRIX = (W'.W)-1
*.      BT,BTT = TEMPORARY COPIES OF B
*.      DD,FF  = WORKING STORAGE FOR PARTITIONED INVERSION OF W'.W
*.      MARKBF = BASIC FUNCTIONS MARKER
*.               0 : AVAILABLE
*.              >0 : ALREADY INCLUDED (=REGRESSOR NUMBER)
*.              -1 : UNAVAILABLE FOR ONE ROUND (REJECTED BY THE
*.                   STEPWISE PROCEDURE)
*.      MAXBF  = GIVES NUMBER OF BASIC FUNCTION CORRESPONDING TO A
*.               REGRESSOR NUMBER
*.      SYY    = SUM OF SQUARES OF Y-VALUES
*.      RSS    = RESIDUAL SUM OF SQUARES
*.      VARIAN = ESTIMATED VARIANCE OF RESIDUALS OR MULTIPLICATION
*.               FACTOR OF THE COVARIANCE MATRIX B
*.      DROP   = DROP IN SYY CORRESPONDING TO THE INCLUSION
*.               OF ONE ADDITIONAL REGRESSOR
*.      R2P    = PARTIAL CORRELATION COEFFICIENT BETWEEN Y AND THE
*.               NEW REGRESSOR, ALL OTHERS ALREADY INCLUDED
*.      FR     = F-RATIO FOR THE INCLUSION OF A NEW REGRESSOR
*.      F      = F-RATIO FOR THE REJECTANCE TEST OF ALREADY INCLUDED
*.               REGRESSORS ; A REGRESSOR WILL BE REJECTED IF ITS
*.               F-RATIO < FLEVEL
*.      R2     = MULTIPLE CORRELATION COEFFICIENT
*.      R2ADJ  =    "         "           "       ADJUSTED FOR THE
*.               DEGREES OF FREEDOM
*.      MINVAR = REGRESSOR NUMBER CORR. TO MIN VARIAN OR MAX R2ADJ
*..=========> ( D.Lienart )
#include "hbook/hcpar1.inc"
#include "hbook/hcpout.inc"
#include "hbook/hcunit.inc"
      DIMENSION X(NPMAX,ND),Y(1),EY(1),IBASFT(ND,NBFMAX),
     +          W(NPMAX,NCOMAX),WT(1),WY(1),B(NCOMAX,NCOMAX),
     +          BT(NCOMAX,NCOMAX),BTT(NCOMAX,NCOMAX),DD(1),FF(1),
     +          COEFF(1),CT(1),MARKBF(1),MAXBF(1),IAUX(10)
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION W,WT,WY,B,BT,BTT,DD,FF,COEFF,CT,SYY,RSS,VARIAN,
     +                 WYT,DROP,DROPMX,DP,R2,R2ADJ,YD,BII,
     +                 FR,R2P,R2PMX,EPSMAC
      DOUBLE PRECISION ONE, HALF, EPSTST
#endif
*
*  DETERMINE MACHINE ACCURACY EPSMAC
*
      EPSMAC=0.0625
      ONE = 1.
      HALF = 0.5
   1  CONTINUE
      EPSMAC = EPSMAC * HALF
      EPSTST = ONE + EPSMAC * HALF
      IF (EPSTST .NE. ONE) GO TO 1
      SYY=0.
      DO 3 I=1,NP
#if !defined(CERNLIB_DOUBLE)
         YD=Y(I)
#endif
#if defined(CERNLIB_DOUBLE)
         YD=DBLE(Y(I))
#endif
         SYY=SYY+YD*YD
    3 CONTINUE
      RSS=SYY
      VARIAN=SYY/NP
      IF (IOPT(7).NE.2) CALL VZERO (MARKBF,NBF)
      NCO=0
      ITER=0
*
      IF (IOPT(2).GE.1) WRITE (LOUT,290) NBF,NP,SYY,EPSMAC
*
*
*  SET UP MARKBF FOR SIMPLE REGRESSION (IOPT(7)=2): ALL BASIC
*  FUNCTIONS WILL BE INCLUDED
*
    5 NCO=NCO+1
      ITER=ITER+1
      IFLAG=1
      R2PMX=0.
      DROPMX=0.
      IF (IOPT(7).EQ.2) THEN
         MARKBF(NCO)=0
         CALL VFILL (MARKBF(NCO+1),NBF-NCO,-1)
      ENDIF
*
*  FOR EACH AVAILABLE BASIC FUNCTION, FORM COVARIANCE MATRIX,
*  COMPUTE COEFF. CT, DROP AND FR; IF THIS RATIO IS NEGATIVE, RSS CANNOT
*  BE REDUCED ANY MORE DUE TO PRECISION PROBLEMS (IFLAG=2); OTHERWISE
*  R2P IS COMPUTED AND POSSIBLY SAVED IF > R2PMX, ALONG WITH DROP,WT,BT,
*  WYT AND THE REGRESSOR NUMBER
*
      DO 30 I=1,NBF
         IF (MARKBF(I).EQ.0.AND.IFLAG.NE.2) THEN
            CALL HCOVW (X,EY,W,WT,B,BT,DD,FF,IBASFT,I)
            DP=0.
            DO 7 K=1,NP
               DP=DP+WT(K)*Y(K)
    7       CONTINUE
            WY(NCO)=DP
            DROP=0.
            DO 10 J=1,NCO
               DP=0.
               DO 8 K=1,NCO
                  DP=DP+WY(K)*BT(K,J)
    8          CONTINUE
               CT(J)=DP
               DROP=DROP+DP*WY(J)
   10       CONTINUE
            FR=CT(NCO)**2*(NP-NCO)/(BT(NCO,NCO)*(SYY-DROP))
            IF (FR.LT.0.) THEN
*
*  NORMALLY FR IS ALWAYS > 0; DUE TO MACHINE PRECISION LIMITATIONS IT
*  CAN HAPPEN THAT RSS GETS < 0; A SLIGHT PERTURBATION OF THE VARIANCE
*  OF THE LAST REGRESSOR WILL NORMALLY SETTLE THIS PROBLEM
*
               BII=BT(NCO,NCO)
               DP=CT(NCO)
               BT(NCO,NCO)=BII*(1.-EPSMAC)
               CT(NCO)=DP+(BT(NCO,NCO)-BII)*WY(NCO)
               DROP=DROP+(DP-CT(NCO))*WY(NCO)
               FR=CT(NCO)**2*(NP-NCO)/(BT(NCO,NCO)*(SYY-DROP))
               IF (FR.LT.0.) IFLAG=2
            ENDIF
            IF (FR.GE.0.) THEN
               IF (R2MIN.NE.2..OR.(FR.GT.FLEVEL.AND.
     +         (SYY-DROP)/(NP-NCO).LT.VARIAN)) THEN
*
*  IF R2MIN=2, THE VARIANCE SHOULD DECREASE FOR A REGRESSOR TO BE
*  CONSIDERED FOR INCLUSION: 2 EQUIVALENT TESTS ARE USED TO INSURE
*  GREATER STABILITY
*
                  IFLAG=-1
                  R2P=FR/(FR+NP-NCO)
                  IF (R2P.GT.R2PMX) THEN
                     R2PMX=R2P
                     DROPMX=DROP
                     DO 15 K=1,NP
                        W(K,NCO)=WT(K)
   15                CONTINUE
                     DO 25 K=1,NCO
                        DO 20 L=1,NCO
                           BTT(K,L)=BT(K,L)
   20                   CONTINUE
   25                CONTINUE
                     WYT=WY(NCO)
                     MAXBF(NCO)=I
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
   30 CONTINUE
*
*  IF THE MINIMUM VARIANCE CRITERION IS ACTIVE, AND THE VARIANCE
*  DOES NOT DECREASE ANY MORE, THEN THE PROCESS WILL BE STOPPED
*  IT WILL ALSO BE STOPPED IF THE VARIANCE GETS NEGATIVE
*
      IF (IFLAG.NE.-1) THEN
         NCO=NCO-1
      ELSE
*
*  VALIDATE NEW MODEL: SET B, WY AND RSS
*  REENABLE DISABLED CANDIDATE BASIC FUNCTIONS
*
         RSS=SYY-DROPMX
         WY(NCO)=WYT
         DO 33 J=1,NCO
            DP=0.
            DO 32 I=1,NCO
               B(I,J)=BTT(I,J)
               DP=DP+WY(I)*B(I,J)
   32       CONTINUE
            COEFF(J)=DP
   33    CONTINUE
         MARKBF(MAXBF(NCO))=NCO
         DO 35 I=1,NBF
            IF (MARKBF(I).EQ.-1) MARKBF(I)=0
   35    CONTINUE
*
*  BACKWARD STAGE OF THE STEPWISE REGRESSION
*  REJECT REGRESSOR WITH MINIMUM F-RATIO UNLESS > FLEVEL
*  UPDATE RSS, W, WY, B, COEFF, MAXBF AND MARK THE CORR. BASIC
*  FUNCTION WITH VALUE -1
*  REPEAT UNTIL NO MORE REGRESSORS CAN BE ELIMINATED
*
         IF (IOPT(7).EQ.0) THEN
   40       IFMIN=0
            FMIN=FLEVEL
            DO 45 I=1,NCO-1
               F=COEFF(I)**2*(NP-NCO)/(B(I,I)*RSS)
               IF (F.LT.FLEVEL.AND.F.LT.FMIN) THEN
                  FMIN=F
                  IFMIN=I
               ENDIF
   45       CONTINUE
            IF (IFMIN.NE.0) THEN
               J=MAXBF(IFMIN)
               MARKBF(J)=-1
               BII=B(IFMIN,IFMIN)
               DO 50 K=1,NCO
                  DD(K)=B(K,IFMIN)
   50          CONTINUE
               DO 70 K=IFMIN+1,NCO
                  DO 55 L=1,NP
                     W(L,K-1)=W(L,K)
   55             CONTINUE
                  DO 60 L=1,NCO
                     B(L,K-1)=B(L,K)
   60             CONTINUE
                  DO 65 L=1,NCO
                     B(K-1,L)=B(K,L)
   65             CONTINUE
                  MAXBF(K-1)=MAXBF(K)
                  WY(K-1)=WY(K)
                  DD(K-1)=DD(K)
   70          CONTINUE
               NCO=NCO-1
               DO 75 I=1,NCO
                  DO 75 J=1,NCO
                     B(I,J)=B(I,J)-DD(I)*DD(J)/BII
   75          CONTINUE
               RSS=SYY
               DO 80 K=1,NCO
                  DP=0.
                  DO 77 I=1,NCO
                     DP=DP+WY(I)*B(I,K)
   77             CONTINUE
                  COEFF(K)=DP
                  RSS=RSS-DP*WY(K)
   80          CONTINUE
               GOTO 40
            ENDIF
         ENDIF
         IF (RSS/(NP-NCO).LT.VARIAN) THEN
            VARIAN=RSS/(NP-NCO)
            MINVAR=-1
         ELSE
            IF (MINVAR.EQ.-1) MINVAR=NCO-1
         ENDIF
      ENDIF
*
*
*
      R2=1-RSS/SYY
      R2ADJ=1-(NP-1)*(1-R2)/(NP-NCO)
*
*  TEST IF FITTING IS TO BE STOPPED OR NOT
*
      IF (IFLAG.EQ.2) THEN
         WRITE (LOUT,190) R2,NCO
      ELSE
         IF (IFLAG.EQ.1) THEN
            WRITE (LOUT,200) R2,NCO
         ELSE
            IF (R2.GE.R2MIN) THEN
               WRITE (LOUT,210) R2,NCO
               IFLAG=0
            ELSE
               IF (NCO.LT.NCOMAX) THEN
                  DO 85 I=1,NBF
                     IF (MARKBF(I).EQ.0) GOTO 87
   85             CONTINUE
                  WRITE (LOUT,220) R2,NCO
                  IFLAG=4
               ELSE
                  WRITE (LOUT,230) NCOMAX,R2
                  IFLAG=3
               ENDIF
            ENDIF
         ENDIF
      ENDIF
*
*  PRINT OUT RESULTS OF EACH ITERATION AND/OR FINAL RESULTS
*
   87 IF (IFLAG.NE.-1) THEN
         IF (IFLAG.NE.1.AND.IFLAG.NE.2.AND.IOPT(2).GE.1) THEN
            IF (MINVAR.EQ.-1) THEN
               WRITE (LOUT,240)
            ELSE
               WRITE (LOUT,250) MINVAR
            ENDIF
         ENDIF
         WRITE (LOUT,300)
      ENDIF
      IF (IOPT(2).EQ.2.OR.IFLAG.NE.-1) THEN
         WRITE (LOUT,260)
         WRITE (LOUT,270) ITER,RSS,R2ADJ,COEFF(1),(IBASFT(K,MAXBF(1))
     +   ,K=1,ND)
         DO 88 I=2,NCO
            WRITE (LOUT,280) I,COEFF(I),(IBASFT(K,MAXBF(I)),K=1,ND)
   88    CONTINUE
      ENDIF
      IF (IFLAG.EQ.-1) GOTO 5
*
*  REORDER IBASFT SO AS TO REFLECT THE ORDER OF INCLUSION
*  OF THE CORR. REGRESSORS IN THE FINAL EQUATION
*
      DO 105 I=1,NCO
         J=MAXBF(I)
         DO 95 L=I+1,NCO
            IF (MAXBF(L).EQ.I) THEN
               MAXBF(L)=MAXBF(I)
               DO 90 K=1,ND
                  IAUX(K)=IBASFT(K,I)
                  IBASFT(K,I)=IBASFT(K,J)
                  IBASFT(K,J)=IAUX(K)
   90          CONTINUE
               GOTO 105
            ENDIF
   95    CONTINUE
         DO 100 K=1,ND
            IBASFT(K,I)=IBASFT(K,J)
  100    CONTINUE
  105 CONTINUE
*
*  COMPUTE THE STANDARD DEVIATIONS OF THE COEFFICIENTS
*
      VARIAN=RSS/(NP-NCO)
      DO 110 K=1,NCO
         SECO(K)=SQRT(B(K,K)*VARIAN)
  110 CONTINUE
*
*  COMPUTE CONFIDENCE INTERVALS FOR COEFFICIENTS
*
      T=1.56/(NP-NCO)+1.645
      DO 115 I=1,NCO
         COMIN(I)=COEFF(I)-T*SECO(I)
         COMAX(I)=COEFF(I)+T*SECO(I)
  115 CONTINUE
*
*
      IF (IOPT(2).GE.1) THEN
         WRITE (LOUT,310)
         DO 120 I=1,NCO
            WRITE (LOUT,320) I,SECO(I),COMIN(I),COMAX(I)
  120    CONTINUE
      ENDIF
      RSSS=RSS
      R2S=R2
*
  190 FORMAT (//' FITTING PROCESS STOPPED AS RESIDUAL SUM OF SQUARES',
     +        ' CANNOT BE REDUCED ANY MORE'/,' R2 = ',F8.5,5X,
     +        I3,' REGRESSORS INCLUDED')
  200 FORMAT (//' FITTING PROCESS STOPPED AS RESIDUAL VARIANCE HITS ',
     +        'MINIMUM'/,' R2 = ',F8.5,5X,I3,' REGRESSORS INCLUDED')
  210 FORMAT (//' FITTING PROCESS STOPPED AS R2 = ',F8.5,/,
     +        I3,' REGRESSORS INCLUDED')
  220 FORMAT (//' FITTING PROCESS STOPPED AS NO MORE REGRESSORS ',
     +        'AVAILABLE'/,' R2 = ',F7.5,5X,I3,' REGRESSORS INCLUDED')
  230 FORMAT (//' FITTING PROCESS STOPPED AS THE MAXIMUM NUMBER OF ',
     +        'REGRESSORS (',I2,') WAS REACHED'/,' R2 = ',F8.5)
  240 FORMAT (' RESIDUAL VARIANCE STILL DECREASES')
  250 FORMAT (' MIMIMUM RESIDUAL VARIANCE REACHED AFTER REGRESSOR',
     +        ' NO ',I2,' WAS INCLUDED')
  260 FORMAT (//' ITERATION',7X,'RSS',7X,'R2ADJ',5X,'REGRESSOR',2X,
     +        'COEFF. VALUE',3X,'TERM OF PARAMETRIZATION'/)
  270 FORMAT (4X,I2,6X,G12.5,3X,F8.5,6X,' 1',6X,G12.5,3X,10(I3,1X))
  280 FORMAT (40X,I2,6X,G12.5,3X,10(I3,1X))
  290 FORMAT (/,1X,I3,' CANDIDATE BASIC FUNCTIONS WERE RETAINED FOR',
     +        ' THE FIT',/,' NUMBER OF POINTS TO FIT = ',I5,/,
     +        ' SUM OF SQUARES OF Y-VALUES = ',G12.5,/,' MACHINE ',
     +        'PRECISION = ',E9.2)
  300 FORMAT (///' FINAL RESULTS OF THE FIT'/,1X,24('*'))
  310 FORMAT (//' REGRESSOR',2X,'STANDARD DEVIATION',5X,'CONFIDENCE',
     +        ' INTERVAL')
  320 FORMAT (4X,I2,9X,G12.5,5X,'[',G12.5,',',G12.5,']')
      END
