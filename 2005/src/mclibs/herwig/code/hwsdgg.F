CDECK  ID>, HWSDGG.
*CMZ :=        =26/04/91  12.47.48  by  Federico Carminati
*-- Author :    Drees, Grassie, Charchula, modified by Bryan Webber
C ===============================================================
C  DREES & GRASSIE PARAMETRIZATION OF PHOTON STRUCTURE FUNCTION
C
C    HWSDGQ(X,Q2,NFL,NCH) - X*QUARK_IN_PHOTON/ALPHA  (!)
C    HWSDGG(X,Q2,NFL)     - X*GLUON_IN_PHOTON/ALPHA  (!)
C WHERE:
C        (INTEGER) NCH - QUARK CHARGE: 1 FOR 1/3
C                                      2 FOR 2/3
C        (INTEGER) NFL - NUMBER OF QUARK FLAVOURS /3 OR 4/
C                   Q2 - SQUARE OF MOMENTUM Q /IN GEV2/
C                   X  - LONGITUDINAL FRACTION
C  LAMBDA=0.4 GEV
C
C       NFL=3:     1 < Q2 < 50   GEV^2
C       NFL=4:    20 < Q2 < 500  GEV^2
C       NFL=5:   200 < Q2 < 10^4 GEV^2
C
C
C  KRZYSZTOF CHARCHULA  /14.02.1989/
C================================================================
C
C PS. Note that for the case of three flavors, one has to add
C the QPM charm contribution for getting F2.
C
C================================================================
C MODIFIED FOR HERWIG BY BRW 19/4/91
C--- -----------------------------------------------
C        GLUON PART OF THE PHOTON SF
C--- -----------------------------------------------
      FUNCTION HWSDGG(X,Q2,NFL)
      IMPLICIT REAL (A-H,P-Z)
      INTEGER NFL
      DIMENSION A(3,4,3),AT(3)
      ALAM2=0.160
      T=LOG(Q2/ALAM2)
C- ---  CHECK WHETHER NFL  HAVE RIGHT VALUES -----
      IF (.NOT.((NFL.EQ.3).OR.(NFL.EQ.4).OR.(NFL.EQ.5)))THEN
 130   WRITE(6,131)
 131   FORMAT(' NUMBER OF FLAVOURS(NFL) HAS NOT BEEN SET TO: 3,4 OR 5;'/
     *'          NFL=3 IS ASSUMED')
       NFL=3
      ELSEIF (T.LE.0) THEN
       WRITE(6,132)
 132   FORMAT(' HWSDGG CALLED WITH SCALE < LAMBDA. RETURNING ZERO.')
       HWSDGG=0
       RETURN
      ENDIF
C ------ INITIALIZATION OF PARAMETERS ARRAY -----
      DATA(((A(I,J,K),I=1,3),J=1,4),K=1,3)/
     + -0.20700,-0.19870, 5.11900,
     +  0.61580, 0.62570,-0.27520,
     +  1.07400, 8.35200,-6.99300,
     +  0.00000, 5.02400, 2.29800,
     +    0.8926E-2, 0.05090,-0.23130,
     +    0.659400, 0.27740, 0.13820,
     +    0.476600,-0.39060, 6.54200,
     +    0.019750,-0.32120, 0.51620,
     +  0.031970, -0.618E-2, -0.1216,
     +  1.0180,    0.94760,  0.90470,
     +  0.24610,  -0.60940,  2.6530,
     +  0.027070, -0.010670, 0.2003E-2/
C ------ Q2 DEPENDENCE -----------
      LF=NFL-2
      DO 20 I=1,3
        AT(I)=A(I,1,LF)*T**A(I,2,LF)+A(I,3,LF)*T**(-A(I,4,LF))
 20   CONTINUE
C ------ GLUON DISTRIBUTION -------------
      HWSDGG=AT(1)*X**AT(2)*(1.0-X)**AT(3)/137.
      RETURN
      END
