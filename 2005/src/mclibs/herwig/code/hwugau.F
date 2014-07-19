CDECK  ID>, HWUGAU.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Adapted by Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWUGAU(F,A,B,EPS)
C-----------------------------------------------------------------------
C     ADAPTIVE GAUSSIAN INTEGRATION OF FUNCTION F
C     IN INTERVAL (A,B) WITH PRECISION EPS
C     (MODIFIED CERN LIBRARY ROUTINE GAUSS)
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWUGAU,F,A,B,EPS,CONST,AA,BB,C1,C2,S8,U,S16,
     & W(12),X(12)
      INTEGER I
      EXTERNAL F
      DATA W/.1012285363D0,.2223810345D0,.3137066459D0,
     &       .3626837834D0,.0271524594D0,.0622535239D0,
     &       .0951585117D0,.1246289713D0,.1495959888D0,
     &       .1691565194D0,.1826034150D0,.1894506105D0/
      DATA X/.9602898565D0,.7966664774D0,.5255324099D0,
     &       .1834346425D0,.9894009350D0,.9445750231D0,
     &       .8656312024D0,.7554044084D0,.6178762444D0,
     &       .4580167777D0,.2816035508D0,.0950125098D0/
      HWUGAU=0.
      IF (A.EQ.B) RETURN
      CONST=.005/ABS(B-A)
      BB=A
    1 AA=BB
      BB=B
    2    C1=0.5*(BB+AA)
         C2=0.5*(BB-AA)
         S8=0.
         DO 3 I=1,4
            U=C2*X(I)
            S8=S8+W(I)*(F(C1+U)+F(C1-U))
    3    CONTINUE
         S8=C2*S8
         S16=0.
         DO 4 I=5,12
            U=C2*X(I)
            S16=S16+W(I)*(F(C1+U)+F(C1-U))
    4    CONTINUE
         S16=C2*S16
         IF (ABS(S16-S8).LE.EPS*(1.+ABS(S16))) GOTO 5
         BB=C1
         IF (1.+CONST*ABS(C2).NE.1.) GOTO 2
C---TOO HIGH ACCURACY REQUESTED
         CALL HWWARN('HWUGAU',500,*999)
    5 HWUGAU=HWUGAU+S16
      IF (BB.NE.B) GOTO 1
  999 END
