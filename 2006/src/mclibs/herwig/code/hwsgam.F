CDECK  ID>, HWSGAM.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Adapted by Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWSGAM(ZINPUT)
C-----------------------------------------------------------------------
C   Gamma function computed by eq. 6.1.40, Abramowitz.
C   B(M) = B2m/(2m *(2m-1)) where B2m is the 2m'th Bernoulli number.
C   HLNTPI = .5*LOG(2.*PI)
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWSGAM,ZINPUT,B(10),HLNTPI,Z,SHIFT,G,T,RECZSQ
      INTEGER I
      DATA B/
     1       0.83333333333333333333D-01,   -0.27777777777777777778D-02,
     1       0.79365079365079365079D-03,   -0.59523809523809523810D-03,
     1       0.84175084175084175084D-03,   -0.19175269175269175269D-02,
     1       0.64102564102564102564D-02,   -0.29550653594771241830D-01,
     1       0.17964437236883057316D0  ,    -1.3924322169059011164D0  /
      DATA HLNTPI/0.91893853320467274178D0/
C
C   Shift argument to large value ( > 20 )
C
      Z=ZINPUT
      SHIFT=1.
   10 IF (Z.LT.20.D0) THEN
         SHIFT = SHIFT*Z
         Z = Z + 1.D0
         GOTO 10
      ENDIF
C
C   Compute asymptotic formula
C
      G = (Z-.5D0)*LOG(Z) - Z + HLNTPI
      T = 1.D0/Z
      RECZSQ = T**2
      DO 20 I = 1,10
         G = G + B(I)*T
         T = T*RECZSQ
   20 CONTINUE
      HWSGAM = EXP(G)/SHIFT
      END
