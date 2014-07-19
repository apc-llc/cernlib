*
* $Id: mnimpr.F,v 1.1.1.1 1996/03/07 14:31:30 mclareni Exp $
*
* $Log: mnimpr.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:30  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNIMPR(FCN,FUTIL)
#include "minuit/d506dp.inc"
CC        Attempts to improve on a good local minimum by finding a
CC        better one.   The quadratic part of FCN is removed by MNCALF
CC        and this transformed function is minimized using the simplex
CC        method from several random starting points.
CC        ref. -- Goldstein and Price, Math.Comp. 25, 569 (1971)
CC
#include "minuit/d506cm.inc"
      EXTERNAL FCN,FUTIL
      DIMENSION DSAV(MNI), Y(MNI+1)
      PARAMETER (ALPHA=1.,BETA=0.5,GAMMA=2.0)
      DATA RNUM/0./
      IF (NPAR .LE. 0)  RETURN
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN(FCN,FUTIL)
      CSTATU = 'UNCHANGED '
      ITAUR = 1
      EPSI = 0.1*UP
      NPFN=NFCN
      NLOOP = WORD7(2)
      IF (NLOOP .LE. 0)  NLOOP = NPAR + 4
      NPARX = NPAR
      NPARP1=NPAR+1
      WG = 1.0/FLOAT(NPAR)
      SIGSAV = EDM
      APSI = AMIN
         DO 2 I= 1, NPAR
         XT(I) = X(I)
         DSAV(I) = WERR(I)
           DO 2 J = 1, I
           NDEX = I*(I-1)/2 + J
           P(I,J) = VHMAT(NDEX)
    2      P(J,I) = P(I,J)
      CALL MNVERT(P,MAXINT,MAXINT,NPAR,IFAIL)
      IF (IFAIL .GE. 1)  GO TO 280
C               Save inverted matrix in VT
         DO 12 I= 1, NPAR
         NDEX = I*(I-1)/2
           DO 12 J= 1, I
           NDEX = NDEX + 1
   12      VTHMAT(NDEX) = P(I,J)
      LOOP = 0
C
   20 CONTINUE
         DO 25 I= 1, NPAR
         DIRIN(I) = 2.0*DSAV(I)
         CALL MNRN15(RNUM,ISEED)
   25    X(I) = XT(I) + 2.0*DIRIN(I)*(RNUM-0.5)
      LOOP = LOOP + 1
      REG = 2.0
      IF (ISW(5) .GE. 0)   WRITE (ISYSWR, 1040) LOOP
   30 CALL  MNCALF(FCN,X,YCALF,FUTIL)
      AMIN = YCALF
C                                        . . . . set up  random simplex
      JL = NPARP1
      JH = NPARP1
      Y(NPARP1) = AMIN
      AMAX = AMIN
         DO 45 I= 1, NPAR
         XI = X(I)
         CALL MNRN15(RNUM,ISEED)
         X(I) = XI - DIRIN(I) *(RNUM-0.5)
         CALL MNCALF(FCN,X,YCALF,FUTIL)
         Y(I) = YCALF
         IF (Y(I) .LT. AMIN)  THEN
            AMIN = Y(I)
            JL = I
         ELSE IF (Y(I) .GT. AMAX)  THEN
            AMAX = Y(I)
            JH = I
         ENDIF
            DO 40 J= 1, NPAR
   40       P(J,I) = X(J)
         P(I,NPARP1) = XI
         X(I) = XI
   45    CONTINUE
C
      EDM = AMIN
      SIG2 = EDM
C                                        . . . . . . .  start main loop
   50 CONTINUE
      IF (AMIN .LT. ZERO)  GO TO 95
      IF (ISW(2) .LE. 2)  GO TO 280
      EP = 0.1*AMIN
      IF (SIG2 .LT. EP   .AND. EDM.LT.EP  )     GO TO 100
      SIG2 = EDM
      IF ((NFCN-NPFN) .GT. NFCNMX)  GO TO 300
C         calculate new point * by reflection
      DO 60 I= 1, NPAR
      PB = 0.
      DO 59 J= 1, NPARP1
   59 PB = PB + WG * P(I,J)
      PBAR(I) = PB - WG * P(I,JH)
   60 PSTAR(I)=(1.+ALPHA)*PBAR(I)-ALPHA*P(I,JH)
      CALL MNCALF(FCN,PSTAR,YCALF,FUTIL)
      YSTAR = YCALF
      IF(YSTAR.GE.AMIN) GO TO 70
C         point * better than jl, calculate new point **
      DO 61 I=1,NPAR
   61 PSTST(I)=GAMMA*PSTAR(I)+(1.-GAMMA)*PBAR(I)
      CALL MNCALF(FCN,PSTST,YCALF,FUTIL)
      YSTST = YCALF
   66 IF (YSTST .LT. Y(JL))  GO TO 67
      CALL MNRAZZ(YSTAR,PSTAR,Y,JH,JL)
      GO TO 50
   67 CALL MNRAZZ(YSTST,PSTST,Y,JH,JL)
      GO TO 50
C         point * is not as good as jl
   70 IF (YSTAR .GE. Y(JH))  GO TO 73
      JHOLD = JH
      CALL MNRAZZ(YSTAR,PSTAR,Y,JH,JL)
      IF (JHOLD .NE. JH)  GO TO 50
C         calculate new point **
   73 DO 74 I=1,NPAR
   74 PSTST(I)=BETA*P(I,JH)+(1.-BETA)*PBAR(I)
      CALL MNCALF(FCN,PSTST,YCALF,FUTIL)
      YSTST = YCALF
      IF(YSTST.GT.Y(JH)) GO TO 30
C     point ** is better than jh
      IF (YSTST .LT. AMIN)  GO TO 67
      CALL MNRAZZ(YSTST,PSTST,Y,JH,JL)
      GO TO 50
C                                        . . . . . .  end main loop
   95 IF (ISW(5) .GE. 0)  WRITE (ISYSWR,1000)
      REG = 0.1
C                                        . . . . . ask if point is new
  100 CALL MNINEX(X)
      CALL FCN(NPARX,GIN,AMIN,U,4,FUTIL)
      NFCN = NFCN + 1
      DO 120 I= 1, NPAR
      DIRIN(I) = REG*DSAV(I)
      IF (ABS(X(I)-XT(I)) .GT. DIRIN(I)) GO TO 150
  120 CONTINUE
      GO TO 230
  150 NFCNMX = NFCNMX + NPFN - NFCN
      NPFN = NFCN
      CALL MNSIMP(FCN,FUTIL)
      IF (AMIN .GE. APSI)  GO TO 325
      DO 220 I= 1, NPAR
      DIRIN(I) = 0.1 *DSAV(I)
      IF (ABS(X(I)-XT(I)) .GT. DIRIN(I)) GO TO 250
  220 CONTINUE
  230 IF (AMIN .LT. APSI)  GO TO 350
      GO TO 325
C                                        . . . . . . truly new minimum
  250 LNEWMN = .TRUE.
      IF (ISW(2) .GE. 1) THEN
          ISW(2) = 1
          DCOVAR = MAX(DCOVAR,HALF)
      ELSE
          DCOVAR = 1.
      ENDIF
      ITAUR = 0
      NFCNMX = NFCNMX + NPFN - NFCN
      CSTATU = 'NEW MINIMU'
      IF (ISW(5) .GE. 0)      WRITE (ISYSWR,1030)
      RETURN
C                                        . . . return to previous region
  280 IF (ISW(5) .GT. 0) WRITE (ISYSWR,1020)
      GO TO 325
  300 ISW(1) = 1
  325 DO 330 I= 1, NPAR
      DIRIN(I) = 0.01*DSAV(I)
  330 X(I) = XT(I)
      AMIN = APSI
      EDM = SIGSAV
  350 CALL MNINEX(X)
      IF (ISW(5) .GT. 0)    WRITE (ISYSWR,1010)
      CSTATU= 'UNCHANGED '
      CALL MNRSET(0)
      IF (ISW(2) .LT. 2)  GO TO 380
      IF (LOOP .LT. NLOOP .AND. ISW(1) .LT. 1)  GO TO 20
  380 CALL MNPRIN (5,AMIN)
      ITAUR = 0
      RETURN
 1000 FORMAT (54H AN IMPROVEMENT ON THE PREVIOUS MINIMUM HAS BEEN FOUND)
 1010 FORMAT (51H IMPROVE HAS RETURNED TO REGION OF ORIGINAL MINIMUM)
 1020 FORMAT (/44H COVARIANCE MATRIX WAS NOT POSITIVE-DEFINITE)
 1030 FORMAT (/38H IMPROVE HAS FOUND A TRULY NEW MINIMUM/1H ,37(1H*)/)
 1040 FORMAT (/18H START ATTEMPT NO.,I2,  20H TO FIND NEW MINIMUM)
      END
